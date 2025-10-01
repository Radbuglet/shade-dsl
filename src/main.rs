use std::{fs, path::Path, rc::Rc};

use shade_dsl::{
    base::{
        Session,
        syntax::{NaiveSegmenter, SourceFileOrigin},
    },
    parse::{ast::parse_file, lower::lower_file, token::tokenize},
    typeck::{
        analysis::{IntrinsicResolver, TyCtxt, WfRequirement},
        syntax::{AnyFuncValue, FuncIntrinsic, ScalarKind, Ty, Value, ValueKind},
    },
};

fn main() {
    let session = Session::new();
    let _guard = session.bind();

    let path = Path::new("samples/app.sdl");

    let span = Session::fetch().source_map.create(
        &mut NaiveSegmenter,
        SourceFileOrigin::Fs(path.to_path_buf()),
        Rc::new(String::from_utf8(fs::read(path).unwrap()).unwrap()),
    );

    let tokens = tokenize(span);
    let ast = parse_file(&tokens);
    let ir = lower_file(&ast);

    let tcx = TyCtxt::new(
        Session::fetch(),
        IntrinsicResolver::new_map([(
            "core",
            IntrinsicResolver::new_map([(
                "get_builtin_type",
                IntrinsicResolver::new_terminal(|tcx| {
                    let s = &tcx.session;

                    tcx.intern_from_scratch_arena(|arena| {
                        arena.alloc(Value {
                            ty: tcx.intern_ty(Ty::Func(
                                tcx.intern_tys(&[tcx.intern_ty(Ty::MetaString)]),
                                tcx.intern_ty(Ty::MetaTy),
                            )),
                            kind: ValueKind::Func(AnyFuncValue::Intrinsic(FuncIntrinsic::new(
                                move |tcx, arena, args| {
                                    let ValueKind::MetaString(str) = arena.read(args[0]).kind
                                    else {
                                        unreachable!()
                                    };
                                    let str = str.as_str(&tcx.session);

                                    let ty = match str {
                                        "bool" => tcx.intern_ty(Ty::Scalar(ScalarKind::Bool)),
                                        "u32" => tcx.intern_ty(Ty::Scalar(ScalarKind::U32)),
                                        "type" => tcx.intern_ty(Ty::MetaTy),
                                        _ => todo!(),
                                    };

                                    Ok(arena.alloc(Value {
                                        ty: tcx.intern_ty(Ty::MetaTy),
                                        kind: ValueKind::MetaType(ty),
                                    }))
                                },
                                s,
                            ))),
                        })
                    })
                }),
            )]),
        )]),
    );

    tcx.queue_wf(WfRequirement::EvaluateType(
        tcx.intern_fn_instance(ir, None),
    ));

    tcx.flush_wf();
}

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
                            ty: tcx.intern_ty(Ty::Func(vec![], tcx.intern_ty(Ty::MetaTy))),
                            kind: ValueKind::Func(AnyFuncValue::Intrinsic(FuncIntrinsic::new(
                                move |tcx, arena, _args| {
                                    Ok(arena.alloc(Value {
                                        ty: tcx.intern_ty(Ty::MetaTy),
                                        kind: ValueKind::MetaType(
                                            tcx.intern_ty(Ty::Scalar(ScalarKind::Bool)),
                                        ),
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

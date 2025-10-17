use std::{fs, path::Path, rc::Rc};

use shade_dsl::{
    base::{
        Session,
        syntax::{NaiveSegmenter, SourceFileOrigin},
    },
    parse::{ast::parse_file, lower::lower_file, token::tokenize},
    typeck::{
        analysis::{IntrinsicResolver, TyCtxt, WfRequirement},
        syntax::{FuncIntrinsic, ScalarKind, Ty, ValueKind},
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
                        arena.alloc_intrinsic_fn(FuncIntrinsic::new(
                            tcx.intern_tys(&[tcx.intern_ty(Ty::MetaString)]),
                            tcx.intern_ty(Ty::DynMetaTy),
                            move |tcx, arena, args| {
                                let ValueKind::MetaString(Some(str)) = arena.read(args[0]).kind
                                else {
                                    unreachable!()
                                };
                                let str = str.as_str(&tcx.session);

                                let ty = match str {
                                    "bool" => tcx.intern_ty(Ty::Scalar(ScalarKind::Bool)),
                                    "u32" => tcx.intern_ty(Ty::Scalar(ScalarKind::U32)),
                                    "type" => tcx.intern_ty(Ty::DynMetaTy),
                                    "any" => tcx.intern_ty(Ty::MetaAny),
                                    _ => todo!(),
                                };

                                Ok(arena.alloc_terminal(
                                    tcx.intern_ty(Ty::DynMetaTy),
                                    ValueKind::DynMetaType(Some(ty)),
                                ))
                            },
                            s,
                        ))
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

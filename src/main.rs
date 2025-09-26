use std::{fs, path::Path, rc::Rc};

use shade_dsl::{
    base::{
        Session,
        syntax::{NaiveSegmenter, SourceFileOrigin},
    },
    parse::{ast::parse_file, lower::lower_file, token::tokenize},
    typeck::{analysis::TyCtxt, syntax::ValueKind},
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

    let tcx = TyCtxt::new(Session::fetch());
    let value = tcx
        .eval_paramless(tcx.intern_fn_instance(ir, None))
        .unwrap();

    dbg!(value.debug(tcx.value_interner.arena()));
}

use std::{fs, path::Path, sync::Arc};

use shade_dsl::{
    base::{
        Session, World,
        mem::Handle,
        syntax::{NaiveSegmenter, SourceFileOrigin, SourceMap},
    },
    parse::{ast::parse_file, lower::lower_file, token::tokenize},
};

fn main() {
    let session = Session::new();
    let mut world = World::new();
    let _guard = session.bind();

    let path = Path::new("samples/app.sdl");

    let span = Session::fetch().get::<SourceMap>().create(
        &mut NaiveSegmenter,
        SourceFileOrigin::Fs(path.to_path_buf()),
        Arc::new(String::from_utf8(fs::read(path).unwrap()).unwrap()),
    );

    let tokens = tokenize(span);
    let ast = parse_file(&tokens);

    dbg!(lower_file(&ast, &mut world).debug(&world));
}

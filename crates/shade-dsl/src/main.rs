use std::{fs, path::Path, sync::Arc};

use shade_dsl::{
    base::{
        Session,
        syntax::{NaiveSegmenter, SourceFileOrigin},
    },
    parse::{ast::parse_file, lower::lower_file, token::tokenize},
};

fn main() {
    let session = Session::default();
    let _guard = session.bind();

    let path = Path::new("samples/app.sdl");

    let span = Session::fetch().source_map.create(
        &mut NaiveSegmenter,
        SourceFileOrigin::Fs(path.to_path_buf()),
        Arc::new(String::from_utf8(fs::read(path).unwrap()).unwrap()),
    );

    let tokens = tokenize(span);
    let ast = parse_file(&tokens);

    dbg!(lower_file(&ast));
}

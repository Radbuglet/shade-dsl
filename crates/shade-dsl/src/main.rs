use std::{fs, path::Path, sync::Arc};

use shade_dsl::{
    base::{GcxOwned, NaiveSegmenter, SourceFileOrigin},
    parse::{ast::parse_file, token::tokenize},
};

fn main() {
    GcxOwned::init(|gcx| {
        let path = Path::new("samples/app.sdl");

        let span = gcx.source_map.create(
            &mut NaiveSegmenter,
            SourceFileOrigin::Fs(path.to_path_buf()),
            Arc::new(String::from_utf8(fs::read(path).unwrap()).unwrap()),
        );

        let tokens = tokenize(gcx, span);
        let ast = parse_file(gcx, &tokens);

        dbg!(&ast);
    });
}

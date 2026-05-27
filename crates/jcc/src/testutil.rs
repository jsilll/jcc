use jcc_backend::codemap::{byte::BytePos, file::SourceFile};

pub fn file(src: &str) -> SourceFile {
    SourceFile::from_source(BytePos::ZERO, "<test>", format!("{src}\n"))
}

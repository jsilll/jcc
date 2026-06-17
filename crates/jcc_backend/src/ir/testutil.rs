use crate::{
    ir::parser::parse::{Parser, ParserResult},
    IdentInterner,
};

use jcc_codemap::{simple::SimpleFiles, Diagnostic, Files};

pub fn parse_ir(
    files: &mut SimpleFiles,
    interner: &mut IdentInterner,
    source: &str,
) -> ParserResult {
    let file = files.add("<test>", source.to_owned());
    files
        .get(file)
        .map(|file| Parser::new(file, interner).parse())
        .unwrap_or_default()
}

pub fn check_parse(files: &mut impl Files, res: &ParserResult) -> Result<(), String> {
    let mut report = String::new();
    for issue in &res.lexer_issues {
        let diag: Diagnostic = issue.clone().into();
        report.push_str(&diag.emit_to_string(files).map_err(|e| e.to_string())?);
    }
    for issue in &res.parser_issues {
        let diag: Diagnostic = issue.clone().into();
        report.push_str(&diag.emit_to_string(files).map_err(|e| e.to_string())?);
    }
    if report.is_empty() {
        Ok(())
    } else {
        Err(report)
    }
}

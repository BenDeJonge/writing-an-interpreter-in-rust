use std::io::{self, Write};

use crate::{
    evaluating::evaluator::{eval, format_evaluation},
    parsing::parser::parse,
};

const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    let stdin_handle = io::stdin();
    let mut stdout_handle = io::stdout();
    let mut buffer = String::new();
    loop {
        stdout_handle.write_all(PROMPT.as_bytes())?;
        stdout_handle.flush()?;
        stdin_handle
            .read_line(&mut buffer)
            .expect("could not read input");
        let _ = match parse(&buffer) {
            Ok(node) => {
                // TODO: handle errors.
                let evaluation = eval(node);
                writeln!(stdout_handle, "{}", format_evaluation(&evaluation))
            }
            Err(e) => writeln!(stdout_handle, "{e}"),
        };
        stdout_handle.flush()?;
        buffer.clear();
    }
}

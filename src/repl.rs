use crate::lexer::Lexer;
use crate::token::Token;
use std::io::{self, Write};

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
        let mut lexer = Lexer::new(&buffer);
        loop {
            let token = lexer.next();
            stdout_handle.write_all(format!("{:?}\n", token).as_bytes())?;
            if token == Token::Eof {
                break;
            }
        }
        stdout_handle.flush()?;
        buffer.clear();
    }
}

use std::io;
use writing_an_interpreter_in_rust::repl;
fn main() -> io::Result<()> {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to start typing commands.");
    repl::start()?;
    Ok(())
}

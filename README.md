# writing-an-interpreter-in-rust

A Rust implementation of the book
[Writing an Interpreter in Go](https://interpreterbook.com/). See also:

- the [monkey-wasm repository](https://github.com/shioyama18/monkey-wasm/) for
  an implementation that closely follows the book.
- the [monkey-rust repository](https://github.com/Rydgel/monkey-rust) for a more
  advanced implementation.

This repo documents my educational read-through of the book, in order to learn
more about Rust and what the hell is actually going on when I execute code.

# Current chapters

- [x] **1. Lexing**

  - A basic [lexer](https://en.wikipedia.org/wiki/Lexical_analysis), capable of
    tokenizing the following subset of the Monkey language.

    ```
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
        x + y
    };

    let result = add(five, ten);

    if (5 < 10 > 5) {
        return true;
    } else {
        return false;
    }

    10 == 10;
    10 != 9;
    ```

  - A simple
    [Read-Eval-Print Loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
    application.

- [x] **2. Parsing**

  - A [Pratt parser](https://en.wikipedia.org/wiki/Operator-precedence_parser)
    that can parse the same Monkey language subset.
  - Inclusion of the parser in the REPL.

- [ ] **3. Evaluation**
- [ ] **4. Extending the interpreter**

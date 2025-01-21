# writing-an-interpreter-in-rust

A Rust implementation of the book
[Writing an Interpreter in Go](https://interpreterbook.com/). See also:

- the [monkey-wasm repository](https://github.com/shioyama18/monkey-wasm/) for
  an implementation that closely follows the book.
- the [monkey-rust repository](https://github.com/Rydgel/monkey-rust) for a more
  advanced implementation.

This repo documents my educational read-through of the book, in order to learn
more about Rust and what the hell is actually going on when I execute code.

A full-fledged programming, extendable language has been implemented, as shown
here:

```
let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
        }
    };
    iter(arr, []);
};

let reduce = fn(arr, initial, f) {
    let iter = fn(arr, result) {
        if (len(arr) == 0) {
            result
        } else {
            iter(rest(arr), f(result, first(arr)));
        }
    };
    iter(arr, initial);
};

let sum = fn(arr) {
    reduce(arr, 0, fn(initial, el) { initial + el });
};

>> sum([1, 2, 3, 4]);
10
```

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

- [x] **3. Evaluation**

  - An evaluator that can evaluate the same Monkey language subset.
  - Inclusion of the evaluator in the REPL.

- [x] **4. Extending the interpreter**

  - Extending the Monkey language with:

    - Strings

    ```
    >> "foo" + "bar"
    "foobar"
    ```

    - Arrays

    ```
    >> [0, 1, true, false, null, [2, 3]][0]
    0
    >> [0, 1][-1]
    1
    ```

    - Hashmaps

    ```
    >> {"foo": 1, "bar": null, true: false}["foo"]
    1
    >> {}["baz"]
    null
    ```

    - Builtin functions

    ```
    >> len([0, 1, 2])
    3
    >> len("foobar")
    6
    >> len({"foo": 1, "bar": null, true: false})
    3

    >> first([0, 1, 2])
    0
    >> first("foobar")
    "f"
    >> first({"foo": 1, "bar": null, true: false})
    true

    >> last([0, 1, 2])
    2
    >> last("foobar")
    "r"
    >> last({"foo": 1, "bar": null, true: false})
    "foo"

    >> push([0, 1, 2], 3)
    [0, 1, 2, 3]
    >> push("foobar", "baz")
    "foobarbaz"
    >> push({"foo": 1, "bar": null, true: false}, ["baz", 0])
    {true: false, "bar": null, "baz": 0, "foo": 1}

    >> puts("foobar")
    "foobar"
    ```

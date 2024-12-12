# Diamond

`Diamond` is a lightweight, interpreted programming language designed for simplicity and readability. Built with a Rust-based interpreter, `Diamond` offers fast execution and a clean syntax, perfect for scripting and exploring programming concepts.

## Features

- **First-Class Functions**: Easily define and use functions.
- **Dynamic Typing**: No need for type declarations.
- **Control Structures**: Includes `if`, `while`, and more.
- **Built-in Functions**: Perform common operations like getting array lengths, manipulating collections, and printing output.

## Quick Syntax Overview

### Keywords

`diamond` supports:

- `fn` to define functions
- `let` for variable declarations
- `if`, `else`, and `while` for control flow
- `return` for function returns

### Built-in Functions

- `len(arg)`: Get the length of a string or array.
- `first(arg)`, `last(arg)`: Access the first or last element of an array.
- `rest(arg)`: Get all but the first element of an array.
- `push(array, value)`: Append an element to an array.
- `say(arg)`: Print values to the console.

## Getting Started

1. Clone the repository:

```
git clone https://github.com/yourusername/diamond.git
cd diamond
```

2. Build the interpreter and the REPL binaries (requires Rust):

```
cargo build --release
```

3. Play with the language using the REPL or run a `.dd` file using the interpreter:

```
./target/release/repl

OR

./target/release/diamond examples/fibonacci.dd
```

## License

`Diamond` is licensed under the MIT License.

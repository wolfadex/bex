# Bex

A simple, [concatenative](https://en.wikipedia.org/wiki/Concatenative_programming_language) programming language for learning and fun.

## REPL

Once the repl is up and running, using the instructions below in Development, you can do some basic math.

### Examples:

```
> 1 2 +
3
```

or

```
> 3 1 -
-2
```

### Syntax

The syntax is:

| Syntax                    | Description                                                                                                                                                                                                                                              |
| ------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| positive integers         | 0, 1, 2 ... but uses JS's BigInt.                                                                                                                                                                                                                        |
| +                         | Sum of 2 numbers                                                                                                                                                                                                                                         |
| -                         | Difference of the top 2 numbers                                                                                                                                                                                                                          |
| \*                        | Product of the top 2 numbers                                                                                                                                                                                                                             |
| /                         | Quotient of the top 2 numbers                                                                                                                                                                                                                            |
| =                         | Equality of the top 2 items. If equal it pushes replaces them with a 1, otherwise a 2. Ueses JS's `===`                                                                                                                                                  |
| drop                      | Remove top item on stack                                                                                                                                                                                                                                 |
| swap                      | Swap the place of the top 2 items on the stack                                                                                                                                                                                                           |
| dup                       | Duplicate the top item on the stack                                                                                                                                                                                                                      |
| `**func**                 | Quote a function. This puts the function on the top of the stack instead of applying it. Will be useful for partially apply functions as well as conditionals.                                                                                           |
| apply                     | Pull the type item off the stack and apply it. That top item must be a function or it'll cause a runtime error.                                                                                                                                          |
| identity                  | Keeps the stack as is.                                                                                                                                                                                                                                   |
| then                      | Used like `if` in most contemporary languages. Because Bex compiles to JS this uses JS's truthiness to determine truthiness.                                                                                                                             |
| else                      | Used like `if` in most contemporary languages, but with the truthiness reversed. Use when you want the opposite behavior of `then`.                                                                                                                      |
| def **_name_** **_body_** | Define your own functions. The **_name_** must be an alpha character followed by any number of alphanumeric characters. The **_body_** is a space separated list of other functions. The definition ends at a newline character or the end of the input. |
| emit                      | Prints, using JS's `console.log`, the top value of the stack without modifying the stack.                                                                                                                                                                |

#### Examples of using `def`

- n ^ 2 = `def square dup *`
- increment = `def inc 1 +`
- decrement = `def dec 1 swap -`

#### Example of using `then` with quotes and `apply`

This will push `1` onto the stack, then `2`, then `0`. The `then` function will pop the `0` off the stack and check its truthiness. Being falsey, it will discard the `1`.

```
1 2 0 then
```

```
0 `inc `dec 5 else apply
```

This will push `0` onto the stack, then `inc`, `dec`, and `5`. The `else` function will pop the `5` off the stack and check its truthiness. Being truthy, it will discard the `inc`. Finally `apply` will pop `dec` off the stack and run it which pops `0` off the stack and decrements it `-1` and puts it back on the stack.

### Intro to concatenative programming

For those new to concatenative programming, the basic idea that your code is composing (aka combining) functions and not moving values. Currently Bex uses a stack for working with functions and values. How this roughly works is that a number, such as `1`, is actually a function that pushes the value `1` onto the stack. `+` is a function that pops the top 2 items off the stack and pushed the sum of those numbers onto the stack. `drop` removes the top of the stack. Finally, `swap` switches the top and second from the top items on the stack.

A basic visual example:

<table>
  <thead>
    <tr>
      <th>
        Step
      </th>
      <th>
        Program
      </th>
      <th>
        Stack
      </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>
        Enter program
      </td>
      <td>
        <pre>1 3 5 swap drop +</pre>
      </td>
      <td>
        <pre>┌─┐
└─┘</pre>
      </td>
    </tr>
    <tr>
      <td>
        push 1 onto the stack
      </td>
      <td>
        <pre>3 5 swap drop +</pre>
      </td>
      <td>
        <pre>┌─┐
│1│
└─┘</pre>
      </td>
    </tr>
    <tr>
      <td>
        push 3 onto the stack
      </td>
      <td>
        <pre>5 swap drop +</pre>
      </td>
      <td>
        <pre>┌─┐
│3│
│1│
└─┘</pre>
      </td>
    </tr>
    <tr>
      <td>
        push 5 onto the stack
      </td>
      <td>
        <pre>swap drop +</pre>
      </td>
      <td>
        <pre>┌─┐
│5│
│3│
│1│
└─┘</pre>
      </td>
    </tr>
    <tr>
      <td>
        swap the top 2 items
      </td>
      <td>
        <pre>drop +</pre>
      </td>
      <td>
        <pre>┌─┐
│3│
│5│
│1│
└─┘</pre>
      </td>
    </tr>
    <tr>
      <td>
        remove the top item
      </td>
      <td>
        <pre>+</pre>
      </td>
      <td>
        <pre>┌─┐
│5│
│1│
└─┘</pre>
      </td>
    </tr>
    <tr>
      <td>
        sum the top 2 items on the stack
      </td>
      <td>
        <pre></pre>
      </td>
      <td>
        <pre>┌─┐
│6│
└─┘</pre>
      </td>
    </tr>
  </tbody>
</table>

## Compile to JS

A basic `.bex` file has the shape

```
MakeEven exposing
	main

import Basics

main
	5 makeEven square emit

makeEven
	dup `identity swap `Basics.decrement swap Basics.isEven then apply

square
	dup *
```

The first line must be the module name followed by the word exposing. The second+ lines must be tab indented words that are exposed. The entry file is only concerned with the word `main` being exposed.

Defining new words requires less syntax than in the repl. This example contains 3 defined words: `main`, `dec`, and `square`. Defining a new word in Bex must follow exactly:

- a lower case alpha character followed by any number of alphanumeric characters
- a newline character
- 1 or more lines of a single tab and space separated words, literals, and operators
- at least 1 empty line (only a newline character) between each definition

Using functions from other modules requires importing the module and then using it's exported functions fully qualified. While there's no error to catch it, cyclical imports with not work.

### Bex Projects

For an example Bex project, see [./fixtures/hello_project](./fixtures/hello_project). Versions listed in th `bex.json` config as well as in the path names are posix timestamps (unix time). Not sure how well this will work but it's something I've always wanted to experiment with as a versioning system.

### Running compiled Bex

Once compiled to JS, Bex code can be either run from the browser or from a Deno or Node like environment. If you compile the above example running it in the browser then do

```html
<script src="./path/to/bex.js"></script>
<script>
  const resultingStack = window.Bex.Main.run();
</script>
```

When you do this, `resultingStack` will be the values on the stack after the code has run. In the future Bex will have built in ways to print to the console as well as some form of FFI.

## Development

[Elm](https://elm-lang.org/) is used for parsing and evaluating the language. [Deno](https://deno.land/) is used as a minimal clue between Elm and the terminal.

Make sure both Elm and Deno are installed, as well as [Velociraptor](https://deno.land/x/velociraptor) which is usd for simplifying running scripts. In your terminal run `vr elm-repl-dev` to compile the Elm code on change, and run `vr main-repl-dev` to compile the Deno glue and start the repl.

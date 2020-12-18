# Bex

A simple, repl only, [concatenative](https://en.wikipedia.org/wiki/Concatenative_programming_language) programming language for learning and fun.

## Use

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

The supported syntax is currently positive integers, the operators `+` and `-` as well as the words `drop` and `swap`.

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

## Development

[Elm](https://elm-lang.org/) is used for parsing and evaluating the language in the repl. [Deno](https://deno.land/) is used as a minimal clue between Elm and the terminal in the repl.

Make sure both Elm and Deno are installed, as well as [Velociraptor](https://deno.land/x/velociraptor) which is usd for simplifying running scripts during development. In your terminal run `vr elm-dev` to compile the Elm code on change, and run `vr main-dev` to compile the Deno glue and start the repl.

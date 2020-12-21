// Stack manipulation
function __kernel__swap(stack) {
  const [a, b, ...rest] = stack;
  return [b, a, ...rest];
}

function __kernel__drop(stack) {
  const [a, ...rest] = stack;
  return rest;
}

function __kernel__dup(stack) {
  const [a, ...rest] = stack;
  return [a, a, ...rest];
}

function __kernel__rotate(stack) {
  const [a, b, c, ...rest] = stack;
  return [c, a, b, ...rest];
}

function __kernel__over(stack) {
  const [a, b, ...rest] = stack;
  return [b, a, b, ...rest];
}

/*
  others to possibly incorporate
  - 2swap [a, b, c, d, ...rest] => [c, d, a, b, ...rest]
  - 2dup [a, b, ...rest] => [a, b, a, b, ...rest]
  - 2over [a, b, c, d, ...rest] => [c, d, a, b, c, d, ...rest]
  - 2drop [a, b, ...rest] => rest
*/

function __kernel__quote(quotedFn) {
  return function (stack) {
    return [quotedFn, ...stack];
  };
}

function __kernel__apply(stack) {
  const [f, ...rest] = stack;
  return f(rest);
}

function __kernel__identity(stack) {
  return stack;
}

function __kernel__then(stack) {
  const [condition, trueCase, falseCase, ...rest] = stack;
  return [condition ? trueCase : falseCase, ...rest];
}

// function __kernel__else(stack) {
//   const [condition, trueCase, falseCase, ...rest] = stack;
//   return [condition ? falseCase : trueCase, ...rest];
// }

function __kernel__identity(stack) {
  return stack;
}

//  Math
function __kernel__operator_add(stack) {
  const [right, left, ...rest] = stack;
  return [left + right, ...rest];
}

function __kernel__operator_subtract(stack) {
  const [right, left, ...rest] = stack;
  return [left - right, ...rest];
}

function __kernel__operator_times(stack) {
  const [right, left, ...rest] = stack;
  return [left * right, ...rest];
}

function __kernel__operator_divide(stack) {
  const [right, left, ...rest] = stack;
  if (right === 0) {
    return [BigInt(0), ...rest];
  } else {
    return [left / right, ...rest];
  }
}

function __kernel__operator_mod(stack) {
  const [right, left, ...rest] = stack;
  return [left % right, ...rest];
}

function __kernel__operator_rem(stack) {
  const [right, left, ...rest] = stack;
  return [left - (left % right) * right, ...rest];
}

// Math-like
function __kernel__operator_equal(stack) {
  const [a, b, ...rest] = stack;
  // Use 0 and 1 because we only work with Ints and Funcs right now
  return [a === b ? BigInt(1) : BigInt(0), ...rest];
}

function __kernel__operator_less_than(stack) {
  const [a, b, ...rest] = stack;
  // Use 0 and 1 because we only work with Ints and Funcs right now
  return [b < a ? BigInt(1) : BigInt(0), ...rest];
}

function __kernel__operator_less_than_or_equal(stack) {
  const [a, b, ...rest] = stack;
  // Use 0 and 1 because we only work with Ints and Funcs right now
  return [b <= a ? BigInt(1) : BigInt(0), ...rest];
}

function __kernel__operator_greater_than(stack) {
  const [a, b, ...rest] = stack;
  // Use 0 and 1 because we only work with Ints and Funcs right now
  return [b > a ? BigInt(1) : BigInt(0), ...rest];
}

function __kernel__operator_greater_than_or_equal(stack) {
  const [a, b, ...rest] = stack;
  // Use 0 and 1 because we only work with Ints and Funcs right now
  return [b >= a ? BigInt(1) : BigInt(0), ...rest];
}

// IO

function __kernel__emit(stack) {
  const [a, ...rest] = stack;
  console.log(a);
  return stack;
}

// Literals

function __kernel__literal_int(i) {
  return function (stack) {
    return [BigInt(i), ...stack];
  };
}

const bexFile = await Deno.readFile("./bex.js");
const decoder = new TextDecoder("utf8");
const encoder = new TextEncoder();
const bexJS = decoder.decode(bexFile);

const globalEval = eval;
globalEval(bexJS);

const [entryModule] = Deno.args;
console.log(entryModule);
console.log(globalThis.Bex[entryModule].run());

const elmFile = await Deno.readFile("./src/elm-compiler.js");
const decoder = new TextDecoder("utf8");
const encoder = new TextEncoder();
const elmJS = decoder.decode(elmFile);

const globalEval = eval;
globalEval(elmJS);

// @ts-ignore
const app = globalThis.Elm.Compiler.init({
  flags: Deno.args,
});

app.ports.loadFile.subscribe(async function (filePath: string) {
  const file = await Deno.readFile(filePath);
  app.ports.fileLoaded.send({
    path: filePath,
    content: decoder.decode(file),
  });
});

interface WriteMsg {
  filePath: string;
  content: string;
}

app.ports.writeFile.subscribe(async function ({ filePath, content }: WriteMsg) {
  await Deno.writeFile(filePath, encoder.encode(content));
});

app.ports.status.subscribe(function (status: string) {
  console.log(status);
});

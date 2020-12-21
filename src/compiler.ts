const decoder = new TextDecoder("utf8");
const encoder = new TextEncoder();

const elmFile = await Deno.readFile("./src/elm-compiler.js");
const elmJS = decoder.decode(elmFile);

const globalEval = eval;
globalEval(elmJS);

// const kernelFile = await Deno.readFile("./core/Bex/Kernel.js");
// const kernelCode = decoder.decode(kernelFile);

// @ts-ignore
const app = globalThis.Elm.Compiler.init({
  flags: Deno.args,
});

app.ports.loadFile.subscribe(async function (filePath: string) {
  try {
    const file = await Deno.readFile(filePath);
    app.ports.fileLoaded.send({
      path: filePath,
      content: decoder.decode(file),
    });
  } catch (err) {
    console.log("loadFile", filePath, err);
  }
});

app.ports.loadConfig.subscribe(async function (filePath: string) {
  try {
    const config = await Deno.readFile(filePath);
    app.ports.configLoaded.send(JSON.parse(decoder.decode(config)));
  } catch (err) {
    console.log("loadConfig", filePath, err);
  }
});

app.ports.loadKernel.subscribe(async function (filePath: string) {
  try {
    const file = await Deno.readFile(filePath);
    app.ports.kernelLoaded.send(decoder.decode(file));
  } catch (err) {
    console.log("loadKernel", filePath, err);
  }
});

interface WriteMsg {
  filePath: string;
  content: string;
}

app.ports.writeFile.subscribe(async function ({ filePath, content }: WriteMsg) {
  try {
    await Deno.writeFile(filePath, encoder.encode(content));
  } catch (err) {
    console.log("writeFile", filePath, err);
  }
});

app.ports.status.subscribe(function (status: string) {
  console.log(status);
});

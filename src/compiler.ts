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

app.ports.status.subscribe(function (status: string) {
  console.log(status);
});

// app.ports.toTS.subscribe(async function ({ action, payload }: ElmMsg) {
//   switch (action) {
//     case "LOAD_FILE":
//       const file = await Deno.readFile(payload);
//       app.ports.fromTS.send({
//         action: "FILE_LOADED",
//         payload: {
//           path: payload,
//           content: decoder.decode(file),
//         },
//       });
//       break;
//     case "WRITE_FILE":
//       await Deno.writeFile(payload.path, encoder.encode(payload.content));
//       break;
//     case "STATUS":
//       console.log(payload);
//       break;
//   }
// });

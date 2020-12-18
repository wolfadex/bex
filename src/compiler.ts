const elmFile = await Deno.readFile("./src/elm-compiler.js");
const decoder = new TextDecoder("utf8");
const encoder = new TextEncoder();
const elmJS = decoder.decode(elmFile);

const globalEval = eval;
globalEval(elmJS);

const buffer = new Uint8Array(1024);
// @ts-ignore
const app = globalThis.Elm.Compiler.init();

interface ElmMsg {
  action: string;
  payload: any;
}

app.ports.toTS.subscribe(async function ({ action, payload }: ElmMsg) {
  switch (action) {
    case "LOAD_FILE":
      const file = await Deno.readFile(payload);
      app.ports.fromTS.send({
        action: "FILE_LOADED",
        payload: {
          path: payload,
          content: decoder.decode(file),
        },
      });
      break;
    case "WRITE_FILE":
      await Deno.writeFile(payload.path, encoder.encode(payload.content));
      break;
  }
});

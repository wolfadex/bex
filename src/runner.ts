const elmFile = await Deno.readFile("./src/elm.js");
const decoder = new TextDecoder("utf8");
const encoder = new TextEncoder();
const elmJS = decoder.decode(elmFile);

const globalEval = eval;
globalEval(elmJS);

const buffer = new Uint8Array(1024);
// @ts-ignore
const app = globalThis.Elm.Main.init();

interface ElmMsg {
  action: string;
  payload: any;
}

app.ports.toTS.subscribe(async function ({ action, payload }: ElmMsg) {
  switch (action) {
    case "PROMPT":
      await print(payload);
      const input = await Deno.stdin.read(buffer);
      // @ts-ignore
      const decodedInput = decoder.decode(buffer.subarray(0, input));
      app.ports.fromTS.send({
        action: "USER_INPUT",
        payload: decodedInput.trim(),
      });
      break;
    case "PRINT":
      await print(payload);
      break;
  }
});

async function print(str: string) {
  const toPrint = encoder.encode(str);
  await Deno.stdout.write(Uint8Array.from(toPrint));
}

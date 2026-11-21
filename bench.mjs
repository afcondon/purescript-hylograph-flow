// The benchmark of hylograph-flow: every measure of Test.Flow.Benchmark on
// every shape of Test.Flow.Shapes at about 1k, 10k and 50k wires, for the
// library as it is and for the 0.1.0 functions kept in the tests
// (Test.Flow.Reference). One measurement to a node process, one process at
// a time, each with a time limit, since the 0.1.0 functions do not finish
// at the larger sizes.
//
//   spago build && node bench.mjs [--limit seconds] [--sizes 1000,10000,50000]
//                                 [--only library|reference] [--measure name]
//
// Prints a table in Markdown; times in milliseconds.
import { spawnSync } from "node:child_process";

const argument = (name, otherwise) => {
  const at = process.argv.indexOf("--" + name);
  return at < 0 ? otherwise : process.argv[at + 1];
};
const limit = Number(argument("limit", "60"));
const sizes = argument("sizes", "1000,10000,50000").split(",").map(Number);
const only = argument("only", null);
const onlyMeasure = argument("measure", null);

const measures = ["problems", "project-Holders", "project-Steps", "project-Both", "project-Chosen", "layers-Steps", "faults-Holders", "faults-Steps", "faults-Both", "collapse", "collapse-0.1.1", "collapse-holding"]
  .filter((name) => !onlyMeasure || name === onlyMeasure);
const shapes = ["chain", "fan-outs", "modules"];
const implementations = ["reference", "library"].filter((name) => !only || name === only);

const run = (implementation, measure, shape, size) => {
  const result = spawnSync(
    process.execPath,
    ["--stack-size=8000", "-e", "import('./output/Test.Flow.Benchmark/index.js').then((m) => m.main())", implementation, measure, shape, String(size)],
    { encoding: "utf8", timeout: limit * 1000, killSignal: "SIGKILL" }
  );
  if (result.error && result.error.code === "ETIMEDOUT") return { text: "> " + limit + " s" };
  if (result.status !== 0) return { text: "failed" };
  const line = JSON.parse(result.stdout.trim().split("\n").pop());
  return { text: line.milliseconds < 10 ? line.milliseconds.toFixed(1) : Math.round(line.milliseconds).toLocaleString("en"), line };
};

// once a size is over the limit, the larger sizes of the same case are not tried
console.log("| function | flow | wires | steps | " + implementations.map((name) => (name === "reference" ? "0.1.0 (ms)" : "now (ms)")).join(" | ") + " |");
console.log("|---|---|---:|---:|" + implementations.map(() => "---:").join("|") + "|");
for (const measure of measures) {
  for (const shape of shapes) {
    const over = {};
    for (const size of sizes) {
      const cells = [];
      let seen = null;
      for (const implementation of implementations) {
        if (over[implementation]) { cells.push("(not run)"); continue; }
        const result = run(implementation, measure, shape, size);
        if (result.text.startsWith(">")) over[implementation] = true;
        if (result.line) seen = result.line;
        cells.push(result.text);
      }
      console.log(`| ${measure} | ${shape} | ${seen ? seen.wires.toLocaleString("en") : "~" + size} | ${seen ? seen.steps.toLocaleString("en") : ""} | ${cells.join(" | ")} |`);
    }
  }
}

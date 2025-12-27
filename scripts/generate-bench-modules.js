const fs = require("fs");
const path = require("path");

// Format a number as a three-digit string (e.g., 1 -> "001", 23 -> "023", 912 -> "912")
function formatNum(n) {
  if (n < 10) return `00${n}`;
  if (n < 100) return `0${n}`;
  return `${n}`;
}

// Generate state name (e.g., 1 -> "State001", 23 -> "State023", 912 -> "State912")
function stateName(n) {
  return `State${formatNum(n)}`;
}

// Generate message name (e.g., 1 -> "Msg001", 23 -> "Msg023", 912 -> "Msg912")
function msgName(n) {
  return `Msg${formatNum(n)}`;
}

// Generate TransitSize module
function generateTransitSizeModule(size, baseNamespace) {
  const states = Array.from({ length: size }, (_, i) => i + 1);
  const msgs = Array.from({ length: size }, (_, i) => i + 1);

  const stateRows = states
    .map((n) => `  , "${stateName(n)}" :: {}`)
    .join("\n")
    .replace(/^  ,/, "  (");
  const msgRows = msgs
    .map((n) => `  , "${msgName(n)}" :: {}`)
    .join("\n")
    .replace(/^  ,/, "  (");

  const transitions = states
    .map((n) => {
      const from = stateName(n);
      const to = stateName(n === size ? 1 : n + 1);
      const msg = msgName(n);
      return `    :* ("${from}" :@ "${msg}" >| "${to}")`;
    })
    .join("\n");

  const matches = states
    .map((n) => {
      const from = stateName(n);
      const to = stateName(n === size ? 1 : n + 1);
      const msg = msgName(n);
      return `  (match @"${from}" @"${msg}" \\_ _ -> return @"${to}")`;
    })
    .join("\n");

  const walkItems = states.map((n) => {
    const msg = msgName(n);
    const to = stateName(n === size ? 1 : n + 1);
    return `v @"${msg}" /\\ v @"${to}"`;
  });
  const walk =
    "  [ " +
    walkItems[0] +
    "\n" +
    walkItems
      .slice(1)
      .map((item) => `  , ${item}`)
      .join("\n") +
    "\n  ]";

  return `module ${baseNamespace}.Transit.Size${formatNum(size)} where

import Prelude

import Data.Tuple.Nested (type (/\\), (/\\))
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Transit (type (:*), type (:@), type (>|), Transit, match, mkUpdate, return)
import Transit.VariantUtils (v)
import Unsafe.Coerce (unsafeCoerce)

type State = Variant
${stateRows}
  )

printState :: State -> String
printState v = t
  where
  VariantRep { type: t } = unsafeCoerce v

init :: State
init = v @"State001"

type Msg = Variant
${msgRows}
  )

printMsg :: Msg -> String
printMsg v = t
  where
  VariantRep { type: t } = unsafeCoerce v

type BenchTransit =
  Transit
${transitions}

update :: State -> Msg -> State
update = mkUpdate @BenchTransit
${matches}

walk :: Array (Msg /\\ State)
walk =
${walk}
`;
}

// Generate ClassicSize module
function generateClassicSizeModule(size, baseNamespace) {
  const states = Array.from({ length: size }, (_, i) => i + 1);
  const msgs = Array.from({ length: size }, (_, i) => i + 1);

  const stateConstructors = states
    .slice(1)
    .map((n) => `  | ${stateName(n)} {}`)
    .join("\n");
  const msgConstructors = msgs
    .slice(1)
    .map((n) => `  | ${msgName(n)} {}`)
    .join("\n");

  const printStateCases = states
    .map((n) => {
      const name = stateName(n);
      return `  ${name} {} -> "${name}"`;
    })
    .join("\n");

  const printMsgCases = msgs
    .map((n) => {
      const name = msgName(n);
      return `  ${name} {} -> "${name}"`;
    })
    .join("\n");

  const updateCases = [
    ...states.map((n) => {
      const from = stateName(n);
      const to = stateName(n === size ? 1 : n + 1);
      const msg = msgName(n);
      return `  ${from} {}, ${msg} {} -> ${to} {}`;
    }),
    `  _, _ -> state`,
  ].join("\n");

  const walkDItems = states.map((n) => {
    const msg = msgName(n);
    const to = stateName(n === size ? 1 : n + 1);
    return `${msg} {} /\\ ${to} {}`;
  });
  const walkDArray =
    "  [ " +
    walkDItems[0] +
    "\n" +
    walkDItems
      .slice(1)
      .map((item) => `  , ${item}`)
      .join("\n") +
    "\n  ]";

  return `module ${baseNamespace}.Classic.Size${formatNum(size)} where

import Prelude

import Data.Tuple.Nested (type (/\\), (/\\))

data StateD
  = State001 {}
${stateConstructors.replace(/^  \| State001/, "")}

derive instance Eq StateD

printStateClassic :: StateD -> String
printStateClassic = case _ of
${printStateCases}

initClassic :: StateD
initClassic = State001 {}

data MsgD
  = Msg001 {}
${msgConstructors.replace(/^  \| Msg001/, "")}

derive instance Eq MsgD

printMsgClassic :: MsgD -> String
printMsgClassic = case _ of
${printMsgCases}

updateClassic :: StateD -> MsgD -> StateD
updateClassic state msg = case state, msg of
${updateCases}

walkClassic :: Array (MsgD /\\ StateD)
walkClassic =
${walkDArray}
`;
}

// Parse command-line arguments or use defaults
function parseArgs() {
  const args = process.argv.slice(2);
  const config = {
    min: 20,
    max: 300,
    step: 20,
    targetFolder: path.join(__dirname, "..", "test", "Test", "Bench"),
    baseNamespace: "Test.Bench",
    generateRunner: false,
    runnerModuleName: null,
    runnerFilePath: null,
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "--min" && i + 1 < args.length) {
      config.min = parseInt(args[++i], 10);
    } else if (arg === "--max" && i + 1 < args.length) {
      config.max = parseInt(args[++i], 10);
    } else if (arg === "--step" && i + 1 < args.length) {
      config.step = parseInt(args[++i], 10);
    } else if (arg === "--target-folder" && i + 1 < args.length) {
      config.targetFolder = args[++i];
    } else if (arg === "--base-namespace" && i + 1 < args.length) {
      config.baseNamespace = args[++i];
    } else if (arg === "--generate-runner" && i + 2 < args.length) {
      config.generateRunner = true;
      config.runnerModuleName = args[++i];
      config.runnerFilePath = args[++i];
    } else if (arg === "--help" || arg === "-h") {
      console.log(`
Usage: node generate-bench-modules.js [options]

Options:
  --min <number>           Minimum size (default: 20)
  --max <number>           Maximum size (default: 300)
  --step <number>          Step size (default: 20)
  --target-folder <path>   Target folder for output (default: test/Test/Bench)
  --base-namespace <ns>     Base namespace for modules (default: Test.Bench)
  --generate-runner <module> <file>  Generate benchmark runner file
                                      module: full module name (e.g., Test.BenchSmall)
                                      file: output file path (e.g., test/Test/BenchSmall.purs)
  --help, -h                Show this help message

Example:
  node generate-bench-modules.js --min 10 --max 200 --step 10 --target-folder test/bench --base-namespace Test.Bench
      `);
      process.exit(0);
    }
  }

  return config;
}

// Generate sizes array from min, max, step
function generateSizes(min, max, step) {
  const sizes = [];
  for (let size = min; size <= max; size += step) {
    sizes.push(size);
  }
  return sizes;
}

// Generate benchmark runner file
function generateBenchRunner(
  sizes,
  baseNamespace,
  runnerModuleName,
  runnerFilePath,
) {
  const classicImports = sizes
    .map((size) => {
      const alias = `ClassicSize${size}`;
      return `import ${baseNamespace}.Classic.Size${formatNum(size)} as ${alias}`;
    })
    .join("\n");

  const transitImports = sizes
    .map((size) => {
      const alias = `TransitSize${size}`;
      return `import ${baseNamespace}.Transit.Size${formatNum(size)} as ${alias}`;
    })
    .join("\n");

  const classicInputs = sizes
    .map((size) => {
      const alias = `ClassicSize${size}`;
      return `  , ${size} /\\ mkInput ${alias}.updateClassic ${alias}.initClassic (map fst ${alias}.walkClassic) ${alias}.printStateClassic`;
    })
    .join("\n")
    .replace(/^  ,/, "  [");

  const transitInputs = sizes
    .map((size) => {
      const alias = `TransitSize${size}`;
      return `  , ${size} /\\ mkInput ${alias}.update ${alias}.init (map fst ${alias}.walk) ${alias}.printState`;
    })
    .join("\n")
    .replace(/^  ,/, "  [");

  const sizesArray = sizes.join(", ");

  return `module ${runnerModuleName} (main) where

import Prelude

import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\\), (/\\))
import Effect (Effect)
import Bench.Common (Input, getConfigFromEnv, mkInput, runBench)
${classicImports}
${transitImports}

inputsClassic :: Array (Int /\\ Input)
inputsClassic =
${classicInputs}
  ]

inputs :: Array (Int /\\ Input)
inputs =
${transitInputs}
  ]

main :: Effect Unit
main = do
  config <- getConfigFromEnv

  runBench config { inputs, inputsClassic }
`;
}

// Main function
function main() {
  const config = parseArgs();
  const sizes = generateSizes(config.min, config.max, config.step);

  const transitOutputDir = path.join(config.targetFolder, "Transit");
  const classicOutputDir = path.join(config.targetFolder, "Classic");

  // Ensure output directories exist and clean them
  for (const outputDir of [transitOutputDir, classicOutputDir]) {
    if (!fs.existsSync(outputDir)) {
      fs.mkdirSync(outputDir, { recursive: true });
    } else {
      // Delete all existing files in the directory to avoid stale generations
      console.log(`Cleaning existing files from ${outputDir}...`);
      const existingFiles = fs.readdirSync(outputDir);
      for (const file of existingFiles) {
        const filePath = path.join(outputDir, file);
        const stat = fs.statSync(filePath);
        if (stat.isFile()) {
          fs.unlinkSync(filePath);
          console.log(`Deleted ${file}`);
        }
      }
    }
  }

  console.log(`Generating modules with sizes: ${sizes.join(", ")}`);
  console.log(`Target folder: ${config.targetFolder}`);
  console.log(`Base namespace: ${config.baseNamespace}`);
  console.log("");

  console.log("Generating TransitSize modules...");
  for (const size of sizes) {
    const content = generateTransitSizeModule(size, config.baseNamespace);
    const filename = path.join(transitOutputDir, `Size${formatNum(size)}.purs`);
    fs.writeFileSync(filename, content, "utf8");
    console.log(`Generated ${filename}`);
  }

  console.log("Generating ClassicSize modules...");
  for (const size of sizes) {
    const content = generateClassicSizeModule(size, config.baseNamespace);
    const filename = path.join(classicOutputDir, `Size${formatNum(size)}.purs`);
    fs.writeFileSync(filename, content, "utf8");
    console.log(`Generated ${filename}`);
  }

  if (config.generateRunner) {
    console.log("Generating benchmark runner...");
    const runnerContent = generateBenchRunner(
      sizes,
      config.baseNamespace,
      config.runnerModuleName,
      config.runnerFilePath,
    );
    fs.writeFileSync(config.runnerFilePath, runnerContent, "utf8");
    console.log(`Generated ${config.runnerFilePath}`);
  }

  console.log("Done!");
}

main();

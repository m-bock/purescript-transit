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
function generateTransitSizeModule(size) {
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

  return `module Test.Bench.Transit.Size${formatNum(size)} where

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
function generateClassicSizeModule(size) {
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

  return `module Test.Bench.Classic.Size${formatNum(size)} where

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

// Main function
function main() {
  const sizes = [
    10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170,
    180, 190, 200,
  ];
  const transitOutputDir = path.join(
    __dirname,
    "..",
    "test",
    "Test",
    "Bench",
    "Transit",
  );
  const classicOutputDir = path.join(
    __dirname,
    "..",
    "test",
    "Test",
    "Bench",
    "Classic",
  );

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

  console.log("Generating TransitSize modules...");
  for (const size of sizes) {
    const content = generateTransitSizeModule(size);
    const filename = path.join(transitOutputDir, `Size${formatNum(size)}.purs`);
    fs.writeFileSync(filename, content, "utf8");
    console.log(`Generated ${filename}`);
  }

  console.log("Generating ClassicSize modules...");
  for (const size of sizes) {
    const content = generateClassicSizeModule(size);
    const filename = path.join(classicOutputDir, `Size${formatNum(size)}.purs`);
    fs.writeFileSync(filename, content, "utf8");
    console.log(`Generated ${filename}`);
  }

  console.log("Done!");
}

main();

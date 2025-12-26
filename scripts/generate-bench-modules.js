#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

// Format a number as a two-digit string (e.g., 1 -> "01", 10 -> "10")
function formatNum(n) {
  return n < 10 ? `0${n}` : `${n}`;
}

// Generate state name (e.g., 1 -> "State01")
function stateName(n) {
  return `State${formatNum(n)}`;
}

// Generate message name (e.g., 1 -> "Msg01")
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

  return `module Test.BenchDef.TransitSize${size} where

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
init = v @"State01"

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

  return `module Test.BenchDef.ClassicSize${size} where

import Prelude

import Data.Tuple.Nested (type (/\\), (/\\))

data StateD
  = State01 {}
${stateConstructors.replace(/^  \| State01/, "")}

derive instance Eq StateD

printStateClassic :: StateD -> String
printStateClassic = case _ of
${printStateCases}

initClassic :: StateD
initClassic = State01 {}

data MsgD
  = Msg01 {}
${msgConstructors.replace(/^  \| Msg01/, "")}

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
  const sizes = [10, 20, 30, 40, 50, 60, 70, 80, 90];
  const outputDir = path.join(__dirname, "..", "test", "Test", "BenchDef");

  // Ensure output directory exists
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
  }

  console.log("Generating TransitSize modules...");
  for (const size of sizes) {
    const content = generateTransitSizeModule(size);
    const filename = path.join(outputDir, `TransitSize${size}.purs`);
    fs.writeFileSync(filename, content, "utf8");
    console.log(`Generated ${filename}`);
  }

  console.log("Generating ClassicSize modules...");
  for (const size of sizes) {
    const content = generateClassicSizeModule(size);
    const filename = path.join(outputDir, `ClassicSize${size}.purs`);
    fs.writeFileSync(filename, content, "utf8");
    console.log(`Generated ${filename}`);
  }

  console.log("Done!");
}

main();

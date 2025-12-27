#!/usr/bin/env node

/**
 * Compile-time benchmark script
 *
 * Benchmarks compilation time for Transit and Classic modules
 * by removing cached output and timing recompilation.
 *
 * Usage: node scripts/compile-time-bench.js [small|large]
 *   small: Benchmarks from test/BenchSmall/ (default)
 *   large: Benchmarks from test/BenchLarge/
 */

const { execSync } = require("child_process");
const fs = require("fs");
const path = require("path");

// Parse arguments
const SIZE = process.argv[2] || "small";

if (SIZE !== "small" && SIZE !== "large") {
  console.error("Error: Size must be 'small' or 'large'");
  console.error("Usage: node scripts/compile-time-bench.js [small|large]");
  console.error("  small: Benchmarks from test/BenchSmall/ (default)");
  console.error("  large: Benchmarks from test/BenchLarge/");
  process.exit(1);
}

// Set paths based on size
const BASE_NAMESPACE = SIZE === "small" ? "BenchSmall" : "BenchLarge";
const BENCH_FOLDER = SIZE === "small" ? "test/BenchSmall" : "test/BenchLarge";

// Results file
const PROJECT_ROOT = path.join(__dirname, "..");
const RESULTS_FILE = path.join(
  PROJECT_ROOT,
  "bench",
  "compile-time",
  "results.txt",
);

// Clear results file
fs.writeFileSync(RESULTS_FILE, "");

// Helper to run command and capture output
function runCommand(command, options = {}) {
  try {
    return execSync(command, {
      encoding: "utf-8",
      stdio: options.silent ? "pipe" : "inherit",
      ...options,
    });
  } catch (error) {
    if (!options.silent) {
      console.error(`Error running command: ${command}`);
      console.error(error.message);
    }
    throw error;
  }
}

// Helper to run command with timing
function timeCommand(command) {
  // Use bash time command - time writes to stderr, so we capture both
  const timeCommand = `{ time ${command}; } 2>&1 | grep -E "^real"`;
  try {
    const output = execSync(timeCommand, {
      encoding: "utf-8",
      shell: "/bin/bash",
      stdio: "pipe",
    });
    return output.trim();
  } catch (error) {
    // grep might exit with non-zero if no match, but stderr might have the output
    const stderr = error.stderr ? error.stderr.toString() : "";
    const stdout = error.stdout ? error.stdout.toString() : "";
    const combined = stdout + stderr;
    const match = combined.match(/^real\s+.*/m);
    if (match) {
      return match[0].trim();
    }
    throw error;
  }
}

// Convert file path to module name
// e.g., test/BenchSmall/Classic/Size020.purs -> BenchSmall.Classic.Size020
function fileToModule(file) {
  // Make path relative to PROJECT_ROOT if it's absolute
  let relativePath = path.relative(PROJECT_ROOT, file);
  // Remove test/ prefix, .purs extension, and convert slashes to dots
  return relativePath
    .replace(/^test\//, "")
    .replace(/\.purs$/, "")
    .replace(/\//g, ".");
}

// Find all Size*.purs files in Classic and Transit folders
function findClassicModules() {
  const classicDir = path.join(PROJECT_ROOT, BENCH_FOLDER, "Classic");
  if (!fs.existsSync(classicDir)) {
    return [];
  }
  return fs
    .readdirSync(classicDir)
    .filter((file) => file.startsWith("Size") && file.endsWith(".purs"))
    .map((file) => path.join(classicDir, file))
    .sort();
}

function findTransitModules() {
  const transitDir = path.join(PROJECT_ROOT, BENCH_FOLDER, "Transit");
  if (!fs.existsSync(transitDir)) {
    return [];
  }
  return fs
    .readdirSync(transitDir)
    .filter((file) => file.startsWith("Size") && file.endsWith(".purs"))
    .map((file) => path.join(transitDir, file))
    .sort();
}

// Benchmark a single module
function benchModule(moduleName) {
  const outputPath = path.join(
    PROJECT_ROOT,
    "output",
    ...moduleName.split("."),
  );
  const relativeOutputPath = path.relative(PROJECT_ROOT, outputPath);

  console.log("");
  console.log("==========================================");
  console.log(`Benchmarking: ${moduleName}`);
  console.log(`Removing cached output: ${relativeOutputPath}`);

  // Remove cached output
  if (fs.existsSync(outputPath)) {
    fs.rmSync(outputPath, { recursive: true, force: true });
  }

  console.log("Compiling...");

  // Time the compilation
  const timeOutput = timeCommand("npx spago build");
  console.log(`Time: ${timeOutput}`);

  // Extract time in seconds (format: real    0m2.235s -> 2.235)
  const timeMatch = timeOutput.match(/real\s+0m([0-9]+\.[0-9]+)s/);
  const timeStr = timeMatch ? timeMatch[1] : "0";

  // Extract size from module name (e.g., BenchSmall.Transit.Size100 -> 100)
  const sizeMatch = moduleName.match(/Size(\d+)/);
  const size = sizeMatch ? sizeMatch[1] : "0";

  // Determine series name
  const series = moduleName.includes("Classic") ? "updateClassic" : "update";

  // Write to results file: moduleName|size|series|time_seconds
  const resultLine = `${moduleName}|${size}|${series}|${timeStr}\n`;
  fs.appendFileSync(RESULTS_FILE, resultLine);
}

// Main execution
function main() {
  console.log("Building project (initial build)...");
  runCommand("npx spago build");

  console.log("");
  console.log("==========================================");
  console.log(`Benchmarking ${SIZE} modules from ${BENCH_FOLDER}`);
  console.log("==========================================");
  console.log("");

  // Find modules
  const transitModules = findTransitModules();
  const classicModules = findClassicModules();
  const totalCount = transitModules.length + classicModules.length;

  console.log(
    `Found ${transitModules.length} Transit modules and ${classicModules.length} Classic modules (total: ${totalCount})`,
  );
  console.log("");

  // Benchmark Transit modules
  console.log("Transit modules:");
  transitModules.forEach((file, index) => {
    const moduleName = fileToModule(file);
    console.log(`[${index + 1}/${transitModules.length}]`);
    benchModule(moduleName);
  });

  // Benchmark Classic modules
  console.log("");
  console.log("Classic modules:");
  classicModules.forEach((file, index) => {
    const moduleName = fileToModule(file);
    console.log(`[${index + 1}/${classicModules.length}]`);
    benchModule(moduleName);
  });

  console.log("");
  console.log(
    `Results written to ${path.relative(PROJECT_ROOT, RESULTS_FILE)}`,
  );
  console.log("Run: node scripts/generate-vega-lite.js");
}

main();

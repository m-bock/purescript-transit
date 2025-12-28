#!/usr/bin/env node

/**
 * Compile-time benchmark script
 *
 * Benchmarks compilation time for Transit and Classic modules
 * by removing cached output and timing recompilation.
 *
 * Usage: node scripts/compile-time-bench.js
 *   Benchmarks from test/Bench/Generated/
 */

const { execSync } = require("child_process");
const fs = require("fs");
const path = require("path");

// Set paths
const BASE_NAMESPACE = "Bench.Generated";
const BENCH_FOLDER = "test/Bench/Generated";

// Results file
const PROJECT_ROOT = path.join(__dirname, "..");
const RESULTS_FILE = path.join(
  PROJECT_ROOT,
  "bench",
  "compile-time",
  "results.vl.json",
);

// Initialize results data array
const resultsData = [];

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

// Helper to run command with timing and progress indication
function timeCommand(command) {
  // Use bash time command - time writes to stderr, so we capture both
  const timeCommand = `{ time ${command}; } 2>&1 | grep -E "^real"`;

  // Start progress indicator
  const startTime = Date.now();
  const progressInterval = setInterval(() => {
    const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
    process.stderr.write(`\r⏳ Compiling... (${elapsed}s)`);
  }, 5000); // Update every 5 seconds

  try {
    const output = execSync(timeCommand, {
      encoding: "utf-8",
      shell: "/bin/bash",
      stdio: "pipe",
    });
    clearInterval(progressInterval);
    process.stderr.write("\r"); // Clear progress line
    return output.trim();
  } catch (error) {
    clearInterval(progressInterval);
    process.stderr.write("\r"); // Clear progress line

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
// e.g., test/Bench/Generated/Classic/Size020.purs -> Bench.Generated.Classic.Size020
function fileToModule(file) {
  // Make path relative to PROJECT_ROOT if it's absolute
  let relativePath = path.relative(PROJECT_ROOT, file);
  // Remove test/ prefix, .purs extension, and convert slashes to dots
  return relativePath
    .replace(/^test\//, "")
    .replace(/\.purs$/, "")
    .replace(/\//g, ".");
}

// Find all Size*.purs files in a subdirectory
function findModules(subdir) {
  const dir = path.join(PROJECT_ROOT, BENCH_FOLDER, subdir);
  if (!fs.existsSync(dir)) {
    return [];
  }
  return fs
    .readdirSync(dir)
    .filter((file) => file.startsWith("Size") && file.endsWith(".purs"))
    .map((file) => path.join(dir, file))
    .sort();
}

// Find all Size*.purs files in Classic and Transit folders
function findClassicModules() {
  return findModules("Classic");
}

function findTransitModules() {
  return findModules("Transit");
}

// Add benchmark result to the results data
function addBenchResult(series, size, timeMs) {
  resultsData.push({
    ms: timeMs,
    series,
    size,
  });
}

// Benchmark a single module
function benchModule(moduleName) {
  // Output path uses dots: output/Foo.Bar.Baz
  const outputPath = path.join(PROJECT_ROOT, "output", moduleName);
  const relativeOutputPath = path.relative(PROJECT_ROOT, outputPath);

  console.log("");
  console.log("==========================================");
  console.log(`Benchmarking: ${moduleName}`);
  console.log(`Removing cached output: ${relativeOutputPath}`);

  // Remove cached output
  if (fs.existsSync(outputPath)) {
    fs.rmSync(outputPath, { recursive: true, force: true });
  }

  // Time the compilation
  const timeOutput = timeCommand("npx spago build");

  // Extract time in seconds (format: real    0m2.235s or real    1m2.235s)
  const timeMatch = timeOutput.match(/real\s+(?:(\d+)m)?([0-9]+\.[0-9]+)s/);
  let timeSeconds = 0;
  if (timeMatch) {
    const minutes = timeMatch[1] ? parseInt(timeMatch[1], 10) : 0;
    const seconds = parseFloat(timeMatch[2]);
    timeSeconds = minutes * 60 + seconds;

    // Display time
    if (minutes > 0) {
      console.log(`Time: ${minutes}m${seconds.toFixed(2)}s`);
    } else {
      console.log(`Time: ${seconds.toFixed(2)}s`);
    }
  } else {
    console.log(`Time: ${timeOutput}`);
  }

  const timeMs = timeSeconds * 1000;

  // Extract size from module name (e.g., BenchSmall.Transit.Size100 -> 100)
  const sizeMatch = moduleName.match(/Size(\d+)/);
  const size = sizeMatch ? parseInt(sizeMatch[1], 10) : 0;

  // Determine series name
  const series = moduleName.includes("Classic") ? "updateClassic" : "update";

  // Add to results
  addBenchResult(series, size, timeMs);
}

// Main execution
function main() {
  console.log("Building project (initial build)...");
  runCommand("npx spago build");

  console.log("");
  console.log("==========================================");
  console.log(`Benchmarking modules from ${BENCH_FOLDER}`);
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

  // Benchmark modules
  function benchmarkModules(modules, label) {
    console.log(`${label} modules:`);
    modules.forEach((file, index) => {
      const moduleName = fileToModule(file);
      console.log(`[${index + 1}/${modules.length}]`);
      benchModule(moduleName);
    });
  }

  benchmarkModules(transitModules, "Transit");
  console.log("");

  benchmarkModules(classicModules, "Classic");

  // Sort data by series, then by size
  resultsData.sort((a, b) => {
    if (a.series !== b.series) {
      return a.series.localeCompare(b.series);
    }
    return a.size - b.size;
  });

  // Generate Vega-Lite JSON
  const vegaLite = {
    $schema: "https://vega.github.io/schema/vega-lite/v6.json",
    data: {
      values: resultsData,
    },
    encoding: {
      color: {
        field: "series",
        legend: {
          orient: "right",
          symbolLimit: 30,
          title: "Implementation",
        },
        type: "nominal",
      },
      x: {
        field: "size",
        title: "Size",
        type: "quantitative",
      },
      y: {
        field: "ms",
        title: "Time (ms)",
        type: "quantitative",
      },
    },
    mark: {
      point: true,
      type: "line",
    },
  };

  // Write results to JSON file
  fs.writeFileSync(RESULTS_FILE, JSON.stringify(vegaLite, null, 2), "utf-8");

  console.log("");
  console.log(
    `Results written to ${path.relative(PROJECT_ROOT, RESULTS_FILE)}`,
  );
}

main();

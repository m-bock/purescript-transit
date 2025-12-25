#!/usr/bin/env node

/**
 * Generates Vega-Lite JSON file from compile-time benchmark results
 *
 * Reads from: bench/compile-time-results.txt
 * Writes to: bench/backend-ES/Update-Functions.vl.json
 *
 * Format of input file (pipe-separated):
 *   moduleName|size|series|time_seconds
 *
 * Example:
 *   Test.BenchDef.TransitSize10|10|update|2.235
 *   Test.BenchDef.ClassicSize10|10|updateClassic|1.623
 */

const fs = require("fs");
const path = require("path");

const INPUT_FILE = path.join(__dirname, "compile-time-results.txt");
const OUTPUT_FILE = path.join(__dirname, "compile-time-results.vl.json");

// Read and parse results
function readResults() {
  if (!fs.existsSync(INPUT_FILE)) {
    console.error(`Error: Results file not found: ${INPUT_FILE}`);
    console.error("Please run compile-time-bench.sh first");
    process.exit(1);
  }

  const content = fs.readFileSync(INPUT_FILE, "utf-8");
  const lines = content
    .trim()
    .split("\n")
    .filter((line) => line.trim());

  return lines.map((line) => {
    const [moduleName, size, series, timeSeconds] = line.split("|");
    return {
      moduleName: moduleName.trim(),
      size: parseInt(size.trim(), 10),
      series: series.trim(),
      timeMs: parseFloat(timeSeconds.trim()) * 1000, // Convert seconds to milliseconds
    };
  });
}

// Generate Vega-Lite JSON
function generateVegaLite(data) {
  return {
    $schema: "https://vega.github.io/schema/vega-lite/v6.json",
    data: {
      values: data.map((d) => ({
        ms: d.timeMs,
        series: d.series,
        size: d.size,
      })),
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
      point: false,
      type: "line",
    },
  };
}

// Main execution
function main() {
  console.log("Reading results from:", INPUT_FILE);
  const results = readResults();
  console.log(`Found ${results.length} benchmark results`);

  console.log("Generating Vega-Lite JSON...");
  const vegaLite = generateVegaLite(results);

  // Ensure output directory exists
  const outputDir = path.dirname(OUTPUT_FILE);
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
  }

  // Write output file
  fs.writeFileSync(OUTPUT_FILE, JSON.stringify(vegaLite, null, 2), "utf-8");
  console.log("Vega-Lite file generated:", OUTPUT_FILE);
}

main();

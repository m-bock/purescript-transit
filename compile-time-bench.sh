npx spago build

# Results file for JS script to process
RESULTS_FILE="bench/compile-time-results.txt"
> "$RESULTS_FILE"  # Clear the file

bench_module() {
  local moduleName=$1
  local outputPath="output/${moduleName}"
  rm -rf "$outputPath"
  echo "$moduleName:"
  local time_output=$( { time npx spago build >/dev/null 2>&1; } 2>&1 | grep -E "^(real)" )
  echo "$time_output"
  
  # Extract time in seconds (format: real    0m2.235s -> 2.235)
  local time_str=$(echo "$time_output" | sed -E 's/real[[:space:]]+0m([0-9]+\.[0-9]+)s/\1/')
  
  # Extract size from module name (e.g., TransitSize10 -> 10)
  local size=$(echo "$moduleName" | sed -E 's/.*Size([0-9]+)/\1/')
  
  # Determine series name
  local series="update"
  if [[ "$moduleName" == *"Classic"* ]]; then
    series="updateClassic"
  fi
  
  # Write to results file: moduleName|size|series|time_seconds
  echo "$moduleName|$size|$series|$time_str" >> "$RESULTS_FILE"
}


echo "TransitSizeXX:"

bench_module "Test.BenchDef.TransitSize10"
bench_module "Test.BenchDef.TransitSize20"
bench_module "Test.BenchDef.TransitSize30"
bench_module "Test.BenchDef.TransitSize40"
bench_module "Test.BenchDef.TransitSize50"
bench_module "Test.BenchDef.TransitSize60"
bench_module "Test.BenchDef.TransitSize70"
bench_module "Test.BenchDef.TransitSize80"
bench_module "Test.BenchDef.TransitSize90"

echo "ClassicSizeXX:"

bench_module "Test.BenchDef.ClassicSize10"
bench_module "Test.BenchDef.ClassicSize20"
bench_module "Test.BenchDef.ClassicSize30"
bench_module "Test.BenchDef.ClassicSize40"
bench_module "Test.BenchDef.ClassicSize50"
bench_module "Test.BenchDef.ClassicSize60"
bench_module "Test.BenchDef.ClassicSize70"
bench_module "Test.BenchDef.ClassicSize80"
bench_module "Test.BenchDef.ClassicSize90"

echo ""
echo "Results written to $RESULTS_FILE"
echo "Run: node bench/generate-vega-lite.js"


# echo ""
# echo "MkLookup in isolation:"
# bench_module "Test.BenchDef.MkLookup.MkLookupSize10"
# bench_module "Test.BenchDef.MkLookup.MkLookupSize20"
# bench_module "Test.BenchDef.MkLookup.MkLookupSize40"
# bench_module "Test.BenchDef.MkLookup.MkLookupSize80"

# echo ""
# echo "MkUpdateV2 in isolation:"
# bench_module "Test.BenchDef.MkUpdateV2.MkUpdateV2Size10"
# bench_module "Test.BenchDef.MkUpdateV2.MkUpdateV2Size20"
# bench_module "Test.BenchDef.MkUpdateV2.MkUpdateV2Size40"
# bench_module "Test.BenchDef.MkUpdateV2.MkUpdateV2Size80"

# bench_module "Transit.Try.Size010"
# bench_module "Transit.Try.Size100"
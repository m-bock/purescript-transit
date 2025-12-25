npx spago build # fail script if this fails

# for the next steps i only want to see the time result in cli

bench_module() {
  local moduleName=$1
  local outputPath="output/${moduleName}"
  rm -rf "$outputPath"
  echo "$moduleName:"
  { time npx spago build >/dev/null 2>&1; } 2>&1 | grep -E "^(real|user|sys)"
}

bench_module "Test.BenchDef.Size30"
bench_module "Test.BenchDef.Size60"


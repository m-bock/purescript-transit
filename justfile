gen-examples:
    npx spago run -m Docs.Main

gen-patchdown:
    PATCHDOWN_FILE_PATH=README.md npx spago run -m Patchdown

gen-svgs:
    find renders assets -name "*.dot" -exec sh -c 'dot -Tsvg "$1" -o "${1%.dot}.svg"' _ {} \;

gen-html-prettier:
    npx prettier --write "renders/*.html"

gen-doctoc:
    npx doctoc --maxlevel 3 README.md

gen-preview:
    pandoc README.md \
        --highlight-style=zenburn \
        --template=assets/gh-template.html \
        -o gh-preview.html

build:
    npx spago build

build-es:
    rm -rf output-es
    npx spago build && npx purs-backend-es build

bench-run ITERATIONS='1000':
    export ITERATIONS={{ITERATIONS}} && \
    BACKEND=ES \
    just node-bench output-es Bench.Generated.Main && \
    BACKEND=JS \
    just node-bench output Bench.Generated.Main

node-bench OUTPUT_DIR MODULE:
    node --no-lazy --predictable --expose-gc --max-old-space-size=4096 --jitless --single-threaded-gc --no-opt -e "import { main } from './{{OUTPUT_DIR}}/{{MODULE}}/index.js'; main();"

test:
    npx spago test

nix:
    nix --extra-experimental-features "nix-command flakes" develop

gen-vega:
    find bench -name "*vl.json" -type f -exec sh -c 'vl2vg "$1" > "${1%.vl.json}.vg.json"' _ {} \; && \
    find bench -name "*.vg.json" -type f -exec sh -c 'vg2svg "$1" > "${1%.vg.json}.svg"' _ {} \;

format:
    npx purs-tidy format-in-place 'src/**/*.purs'
    npx purs-tidy format-in-place 'test/**/*.purs'

gen-bench-modules:
    rm -rf test/Bench/Generated
    rm -rf output/Bench.Generated.*
    node scripts/generate-bench-modules.js \
      --min 20 --max 200 --step 20 \
      --target-folder test/Bench/Generated --base-namespace Bench.Generated \
      --generate-runner Bench.Generated.Main test/Bench/Generated/Main.purs

clean:
    rm -rf output

compile-time-bench:
    node scripts/compile-time-bench.js

gen-full:
    just clean && \
    \
    just test && \
    \
    just gen-bench-modules && \
    \
    just compile-time-bench && \
    \
    just build-es && \
    just bench-run && \
    \
    just gen

gen:
    just gen-examples && \
    just gen-html-prettier && \
    just gen-vega && \
    just gen-svgs && \
    just gen-patchdown && \
    just gen-doctoc && \
    \
    just gen-preview

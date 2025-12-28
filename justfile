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

bench-run-small:
    ITERATIONS=1000 \
    BACKEND=JS \
    just node-bench BenchSmall.Main \
    node --no-lazy --predictable --expose-gc --max-old-space-size=4096 --jitless --single-threaded-gc --no-opt -e "import { main } from './output-es/BenchSmall.Main/index.js'; main();" \
    BACKEND=ES \
    just node-bench BenchSmall.Main


bench-run-large:
    export ITERATIONS=100 && \
    BACKEND=ES \
    just node-bench output-es BenchLarge.Main && \
    BACKEND=JS \
    just node-bench output BenchLarge.Main

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

gen-bench-modules-small:
    node scripts/generate-bench-modules.js \
      --min 20 --max 100 --step 20 \
      --target-folder test/BenchSmall --base-namespace BenchSmall \
      --generate-runner BenchSmall.Main test/BenchSmall/Main.purs \

gen-bench-modules-large:
    rm -rf test/BenchLarge
    rm -rf output/BenchLarge.*
    node scripts/generate-bench-modules.js \
      --min 20 --max 200 --step 20 \
      --target-folder test/BenchLarge --base-namespace BenchLarge \
      --generate-runner BenchLarge.Main test/BenchLarge/Main.purs \

clean:
    rm -rf output

compile-time-bench-small:
    node scripts/compile-time-bench.js small

compile-time-bench-large:
    node scripts/compile-time-bench.js large

gen-full:
    just clean && \
    \
    just test && \
    \
    just gen-bench-modules-small && \
    just gen-bench-modules-large && \
    \
    just compile-time-bench-small && \
    \
    just build-es && \
    just bench-run-large && \
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

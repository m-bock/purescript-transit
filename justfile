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
    node --no-lazy --predictable -e "import { main } from './output-es/BenchSmall.Main/index.js'; main();" \
    BACKEND=ES \
    node --no-lazy --predictable -e "import { main } from './output/BenchSmall.Main/index.js'; main();"

bench-run-large:
    ITERATIONS=10000 \
    BACKEND=JS \
    node --no-lazy --predictable -e "import { main } from './output-es/BenchLarge.Main/index.js'; main();" \
    BACKEND=ES \
    node --no-lazy --predictable -e "import { main } from './output/BenchLarge.Main/index.js'; main();"

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
    node scripts/generate-bench-modules.js \
      --min 20 --max 300 --step 20 \
      --target-folder test/BenchLarge --base-namespace BenchLarge \
      --generate-runner BenchLarge.Main test/BenchLarge/Main.purs \

clean-bench-modules-large:
    rm -rf test/BenchLarge
    rm -rf output/BenchLarge.*

clean:
    rm -rf output

compile-time-bench-small:
    node scripts/compile-time-bench.js small

compile-time-bench-large:
    node scripts/compile-time-bench.js large

gen-vega-lite:
    node scripts/generate-vega-lite.js

    # just clean && \
    # just clean-bench-modules-large && \
    # \
    # just test && \
    # \
    # just gen-bench-modules-small && \
    # just gen-bench-modules-large && \
    # \
    # just compile-time-bench-small && \
    # just gen-vega-lite && \
    # \

gen-full:
    just build-es && \
    just bench-run-large && \
    \
    just gen

gen:
    just gen-examples && \
    just gen-html-prettier && \
    just gen-svgs && \
    just gen-vega && \
    just gen-patchdown && \
    just gen-doctoc && \
    \
    just gen-preview

gen-examples:
    npx spago run -m Docs.MainExamples

gen-patchdown:
    PATCHDOWN_FILE_PATH=README.md npx spago run -m Patchdown

gen-svgs:
    find graphs assets -name "*.dot" -exec sh -c 'dot -Tsvg "$1" -o "${1%.dot}.svg"' _ {} \;

gen-html-prettier:
    npx prettier --write "graphs/*.html"

gen-doctoc:
    npx doctoc --maxlevel 3 README.md

gen-preview:
    pandoc README.md \
        --highlight-style=zenburn \
        --template=assets/gh-template.html \
        -o gh-preview.html

gen-book:
    rm -rf site
    pandoc README.md -t chunkedhtml --split-level=3 --toc --toc-depth=3 -o site --highlight-style=zenburn  --template=assets/gh-template.html
    cp -r assets site/assets
    cp -r graphs site/graphs
    cp -r bench site/bench

build:
    npx spago build

build-es:
    rm -rf output-es
    npx spago build && npx purs-backend-es build

bench-quick:
    just build && \
    just build-es && \
    export ITERATIONS=1000 && just bench-es && just bench-js

bench:
    just build && \
    just build-es && \
    export ITERATIONS=1000 && just bench-es && just bench-js

bench-js:
    BACKEND=JS node -e 'import { main } from "./output/Test.Bench/index.js"; main();'

bench-es:
    BACKEND=ES node -e 'import { main } from "./output-es/Test.Bench/index.js"; main();'

gen: 
    just gen-examples && \
    just gen-html-prettier && \
    just gen-svgs && \
    just gen-vega && \
    just gen-patchdown && \
    just gen-doctoc && \
    just gen-preview && \
    just gen-book

test:
    npx spago test

nix:
    nix --extra-experimental-features "nix-command flakes" develop

gen-vega:
    find bench -name "*vl.json" -type f -exec sh -c 'vl2vg "$1" > "${1%.vl.json}.vg.json"' _ {} \; && \
    find bench -name "*.vg.json" -type f -exec sh -c 'vg2svg "$1" > "${1%.vg.json}.svg"' _ {} \;

watch:
    echo README.md | entr -c just gen

format:
    npx purs-tidy format-in-place 'src/**/*.purs'
    npx purs-tidy format-in-place 'test/**/*.purs'

gen-bench-modules:
    node scripts/generate-bench-modules.js
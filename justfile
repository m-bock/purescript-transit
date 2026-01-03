gen-examples:
    just node-run Docs.Main

node-run MODULE:
    node -e "import { main } from './output/{{MODULE}}/index.js'; main();"

gen-patchdown:
    PATCHDOWN_FILE_PATH=docs/tutorial.md \
    PATCHDOWN_BASE_URL=https://github.com/m-bock/purescript-transit/blob/main \
    just node-run Md.Main \
    
    PATCHDOWN_FILE_PATH=README.md \
    just node-run Md.Main

gen-svgs:
    find renders assets -name "*.dot" -exec sh -c 'dot -Tsvg "$1" -o "${1%.dot}.svg"' _ {} \;

gen-md-prettier:
    npx prettier --write "renders/*.md"

gen-doctoc:
    npx doctoc --maxlevel 3 README.md
    npx doctoc --maxlevel 3 docs/tutorial.md

gen-book BASEURL='':
    rm -rf site
    pandoc docs/tutorial.md -t chunkedhtml \
      --split-level=2 \
      --toc --toc-depth=2 \
      -o site \
      --highlight-style=zenburn \
      --template=assets/gh-template.html \
      --variable=baseurl:{{BASEURL}}
    cp -r assets site/assets
    cp -r renders site/renders
    cp -r bench site/bench

build:
    npx spago build

build-es:
    rm -rf output-es
    npx spago build && npx purs-backend-es build

bench-run ITERATIONS='1000':
    export ITERATIONS={{ITERATIONS}} && \
    BACKEND=ES just node-bench output-es Bench.Generated.Main && \
    BACKEND=JS just node-bench output Bench.Generated.Main

node-bench OUTPUT_DIR MODULE:
    node \
      --no-lazy --predictable --expose-gc \
      --max-old-space-size=4096 --jitless \
      --single-threaded-gc --no-opt \
      -e "import { main } from './{{OUTPUT_DIR}}/{{MODULE}}/index.js'; main();"

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

gen-bench:
    just clean && \
    \
    just gen-bench-modules && \
    \
    just compile-time-bench && \
    \
    just build-es && \
    just bench-run && \
    \
    just gen-vega

gen:
    just build && \
    just gen-examples && \
    just gen-md-prettier && \
    just gen-svgs && \
    just gen-patchdown

watch:
    just gen && \
    npx concurrently "npx browser-sync start --server --files 'renders/**/*.md' 'renders/**/*.html' --port 5000 --no-open --reload-delay 100" "while true; do sleep 30; just gen; done"

deploy:
    cp -r assets renders -t site && \
    just test && \
    just gen && \
    just gen-book 'https://m-bock.github.io/purescript-transit/' && \
    npx gh-pages -d site
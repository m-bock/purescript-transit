node-run MODULE:
    node -e "import { main } from './output/{{MODULE}}/index.js'; main();"

gen-examples:
    rm -rf renders
    mkdir -p renders
    mkdir -p renders/themes
    just node-run Docs.Main

t:
    echo 1

    echo 2

gen-patchdown:
    for file in docs/tutorial/*.md; do \
        PATCHDOWN_FILE_PATH="$file" \
        PATCHDOWN_BASE_URL=https://github.com/m-bock/purescript-transit/blob/main \
        just node-run Md.Main; \
    done
    
    PATCHDOWN_FILE_PATH=README.md \
    just node-run Md.Main

gen-svgs:
    find renders assets -name "*.dot" -exec sh -c 'dot -Tsvg "$1" -o "${1%.dot}.svg"' _ {} \;

gen-md-prettier:
    npx prettier --write "renders/*.md"

gen-book BASEURL='':
    rm -rf site
    pandoc docs/tutorial/*.md -t chunkedhtml \
      --split-level=2 \
      --toc --toc-depth=2 \
      -o site \
      --highlight-style=zenburn \
      --template=assets/gh-template.html \
      --variable=baseurl:{{BASEURL}}
    
    cp -r assets renders bench -t site

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

clean-bench-modules:
    rm -rf test/Bench/Generated
    rm -rf output/Bench.Generated.*

gen-bench-modules:
    just clean-bench-modules
    node scripts/generate-bench-modules.js \
      --min 20 --max 200 --step 20 \
      --target-folder test/Bench/Generated --base-namespace Bench.Generated \
      --generate-runner Bench.Generated.Main test/Bench/Generated/Main.purs

clean:
    rm -rf output

compile-time-bench:
    node scripts/compile-time-bench.js

bench:
    just clean
    just gen-bench-modules
    just compile-time-bench
    just build-es
    just bench-run
    just gen-vega

gen:
    just build
    just gen-examples
    just gen-md-prettier
    just gen-svgs
    just gen-patchdown

check-git-clean:
    if ! git diff --quiet; then \
        echo "Git is not clean. The following files have changes:"; \
        git status --short; \
        git diff; \
        exit 1; \
    fi

deploy:
    just clean
    just test
    just gen
    just gen-book 'https://m-bock.github.io/purescript-transit/'
    just format
    just check-git-clean
    npx gh-pages -d site

bench-compare:
    git archive HEAD:bench | tar -x -C tmp/bench-old

    firefox \
      tmp/bench-old/backend-JS/Update-Functions.svg \
              bench/backend-JS/Update-Functions.svg \
      tmp/bench-old/backend-ES/Update-Functions.svg \
              bench/backend-ES/Update-Functions.svg \
      tmp/bench-old/compile-time/results.svg \
              bench/compile-time/results.svg

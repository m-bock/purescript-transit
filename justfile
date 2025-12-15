readme-split:
    rm -rf readme
    scripts/split-markdown.js split README.md readme 3

readme-merge:
    scripts/split-markdown.js merge readme README.md

gen-docs:
    PATCHDOWN_FILE_PATH=README.md npx spago run -m Docs.Main

gen-svgs:
    find graphs assets -name "*.dot" -exec sh -c 'dot -Tsvg "$1" -o "${1%.dot}.svg"' _ {} \;

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

build:
    npx spago build

build-es:
    scripts/build-es.sh

build-all:
    just build && just build-es

bench:
    npx spago run -m Test.Bench

gen: 
    just readme-merge && \
    just gen-docs && \
    just gen-svgs && \
    just gen-doctoc && \
    just gen-preview && \
    just gen-book
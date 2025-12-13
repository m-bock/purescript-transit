gen-svgs:
    find graphs assets -name "*.dot" -exec sh -c 'dot -Tsvg "$1" -o "${1%.dot}.svg"' _ {} \;


gen-docs:
    PATCHDOWN_FILE_PATH="./README.md" npx spago run -m Docs.Main

gen-doctoc:
    npx doctoc --maxlevel 3 README.md

gen: 
    just gen-docs && just gen-svgs && just gen-doctoc && just gen-preview && just gen-book

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

dev:
    ls README.md | entr just gen

gen-svgs:
    find graphs assets -name "*.dot" -exec sh -c 'dot -Tsvg "$1" -o "${1%.dot}.svg"' _ {} \;


gen-docs:
    PATCHDOWN_FILE_PATH="./README.md" npx spago run -m Docs.Main

gen-doctoc:
    npx doctoc --maxlevel 3 README.md

gen: 
    just gen-docs && just gen-svgs && just gen-doctoc && just gen-preview

gen-preview:
    pandoc README.md \
        -f gfm \
        -t html5 \
        --template=gh-template.html \
        -o preview.html

dev:
    ls README.md | entr just gen

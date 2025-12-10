gen-svgs:
    find graphs assets -name "*.dot" -exec sh -c 'dot -Tsvg "$1" -o "${1%.dot}.svg"' _ {} \;


gen-docs:
    PATCHDOWN_FILE_PATH="./README.md" npx spago run -m Docs.Main
    npx doctoc --maxlevel 3 README.md

gen: gen-docs gen-svgs 
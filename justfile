gen-svgs:
    cd graphs && for file in *.dot; do dot -Tsvg "$file" -o "${file%.dot}.svg"; done

run-example:
    npx spago run -m Test.Example

gen-docs:
    PATCHDOWN_FILE_PATH="./README.md" npx spago run -m Patchdown
    npx doctoc --maxlevel 3 README.md

gen: run-example gen-svgs gen-docs
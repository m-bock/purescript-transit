gen-svgs:
    cd graphs && for file in *.dot; do dot -Tsvg "$file" -o "${file%.dot}.svg"; done

gen-docs:
    PATCHDOWN_FILE_PATH="./README.md" npx spago run -m Test.Main
    npx doctoc --maxlevel 3 README.md

gen: gen-docs gen-svgs 
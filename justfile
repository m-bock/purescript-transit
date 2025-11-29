gen-docs:
    PATCHDOWN_FILE_PATH="./README.md" npx spago run -m Patchdown
    npx doctoc --maxlevel 3 README.md
General AI guideline:

- always run `npx spago build` to check if changes compile, edit until it compiles.
- always run `npx spago test` to check if tests pass, edit until they pass.
- Each src file must have a test file
- the module name of each PureScript file must match the file name
- Each test file must be referenced in Test.Main

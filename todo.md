- [ ] cleanup Transit module
- [ ] make tl vars consistent (spec/impl)
- [ ] Find better name for FilterRow
- [ ] Find better name for Empty/Wrap

---

- [ ] implement dark/light theme chart generation
- [ ] Add haus/nikolaus example
- [ ] implement classes in core module
- [ ] Add variant usage

---

- [ ] cleanup imports
- [ ] publish to pursuit

---

General AI guideline:

- always run `npx spago build` to check if changes compile, edit until it compiles.
- always run `npx spago test` to check if tests pass, edit until they pass.
- Each src file must have a test file
- the module name of each PureScript file must match the file name
- Each test file must be referenced in Test.Main

<table>
  <tbody>
    <tr><td>A</td><td>B</td></tr>
  </tbody>
  <tbody>
    <tr><td>C</td><td>D</td></tr>
  </tbody>
  <tbody>
    <tr><td>E</td><td>F</td></tr>
  </tbody>
</table>

# Error handling

With **Transit**'s `mkUpdate` function, you define valid transitions in your type-level specification, and the compiler ensures you implement handlers for each one. When an invalid state-message combination (one not in your specification) is encountered at runtime, `mkUpdate` silently returns the unchanged state. This is sometimes exactly what you want - the state machine simply ignores invalid messages and stays in its current state.

However, sometimes you need to explicitly know whether a transition was valid or not. For these cases, **Transit** provides `mkUpdateMaybe`, which wraps the result in a `Maybe` type. Valid transitions return `Just state`, while invalid transitions return `Nothing`, allowing you to handle the error explicitly.

Here's an update function that only handles valid transitions:

<!-- PD_START:purs
filePath: test/Examples/ErrorHandling.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> Maybe State
update = mkUpdateMaybe @DoorTransit
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandling.purs#L13-L20">test/Examples/ErrorHandling.purs L13-L20</a>
  </sup>
</p>

<!-- PD_END -->

The key difference is the return type: `State -> Msg -> Maybe State` instead of `State -> Msg -> State`. The function uses `mkUpdateMaybe` instead of `mkUpdate`, but the match handlers remain the same.

When you call this update function with a valid state-message combination, it returns `Just` with the new state:

<!-- PD_START:purs
filePath: test/Examples/ErrorHandling.purs
pick:
  - specSuccess
-->

```purescript
specSuccess :: Spec Unit
specSuccess = do
  it "should return the correct state" do
    update (v @"DoorOpen") (v @"Close") `shouldEqual` Just (v @"DoorClosed")
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandling.purs#L22-L25">test/Examples/ErrorHandling.purs L22-L25</a>
  </sup>
</p>

<!-- PD_END -->

But when you call it with an invalid combination (like trying to open a door that's already open), it returns `Nothing`:

<!-- PD_START:purs
filePath: test/Examples/ErrorHandling.purs
pick:
  - specFailure
-->

```purescript
specFailure :: Spec Unit
specFailure = do
  it "should return the correct state" do
    update (v @"DoorOpen") (v @"Open") `shouldEqual` Nothing
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandling.purs#L27-L30">test/Examples/ErrorHandling.purs L27-L30</a>
  </sup>
</p>

<!-- PD_END -->

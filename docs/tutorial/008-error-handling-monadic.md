# Monadic Error handling

For more advanced scenarios, you can combine error handling with monadic effects using `mkUpdateMaybeM`.

Here's an update function that logs transitions AND returns `Maybe`. Again we use the `Writer` as example, but you can use any monad you want.

<!-- PD_START:purs
filePath: test/Examples/ErrorHandlingMonadic.purs
pick:
  - Accum
  - update
-->

```purescript
type Accum = Array String

update :: forall m. MonadWriter Accum m => State -> Msg -> m (Maybe State)
update = mkUpdateMaybeM @DoorTransit
  ( matchM @"DoorOpen" @"Close" \_ _ -> do
      tell [ "Closing door" ]
      pure $ return @"DoorClosed"
  )
  ( matchM @"DoorClosed" @"Open" \_ _ -> do
      tell [ "Opening door" ]
      pure $ return @"DoorOpen"
  )
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandlingMonadic.purs#L15-L26">test/Examples/ErrorHandlingMonadic.purs L15-L26</a>
  </sup>
</p>

<!-- PD_END -->

The return type is `m (Maybe State)` - combining the monad `m` (for effects) with `Maybe` (for error handling). Inside each handler, we can perform monadic operations like `tell` to log transitions.

Now let's use this in a scenario where we attempt an invalid transition in the middle of a sequence:

<!-- PD_START:purs
filePath: test/Examples/ErrorHandlingMonadic.purs
pick:
  - walk
-->

```purescript
walk :: MaybeT (Writer Accum) State
walk = do
  let s0 = v @"DoorOpen"
  s1 <- MaybeT $ update s0 (v @"Close")
  s2 <- MaybeT $ update s1 (v @"Open")
  s3 <- MaybeT $ update s2 (v @"Close")
  s4 <- MaybeT $ update s3 (v @"Close") -- here we request illegal transition
  s5 <- MaybeT $ update s4 (v @"Open")
  pure s5
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandlingMonadic.purs#L28-L36">test/Examples/ErrorHandlingMonadic.purs L28-L36</a>
  </sup>
</p>

<!-- PD_END -->

Here we use the `MaybeT` transformer to work with the `Writer` monad while handling potential failures. Notice that somewhere along the way, we attempt an invalid transition (closing an already closed door). This will cause the computation to short-circuit - the subsequent transitions won't be executed.

Let's verify that the logs only contain the transitions that actually occurred:

<!-- PD_START:purs
filePath: test/Examples/ErrorHandlingMonadic.purs
pick:
  - spec
-->

```purescript
spec :: Spec Unit
spec = do
  describe "ErrorHandlingMonadic" do
    it "should return the correct state" do
      let
        logs :: Array String
        logs = execWriter (runMaybeT walk)
      logs `shouldEqual`
        [ "Closing door"
        , "Opening door"
        , "Closing door"
        ]
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandlingMonadic.purs#L38-L49">test/Examples/ErrorHandlingMonadic.purs L38-L49</a>
  </sup>
</p>

<!-- PD_END -->

Indeed, the logs stop after the invalid transition.

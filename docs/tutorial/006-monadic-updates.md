# Monadic updates

In some cases, you may want your update function to perform effects or collect information during state transitions. **Transit** supports monadic update functions through `mkUpdateM` and `matchM`, which allow you to work within any monad context.

This is useful for scenarios like:

- Logging state transitions
- Accumulating data during updates
- Performing other side effects

Let's see how to create an update function that logs each transition using the `Writer` monad. This is just an example, you can use any monad.

<!-- PD_START:purs
filePath: test/Examples/Monadic.purs
pick:
  - Accum
  - update
-->

```purescript
type Accum = Array String

update :: forall m. MonadWriter Accum m => State -> Msg -> m State
update = mkUpdateM @DoorTransit
  ( matchM @"DoorOpen" @"Close" \_ _ -> do
      tell [ "You just closed the door" ]
      pure $ return @"DoorClosed"
  )
  ( matchM @"DoorClosed" @"Open" \_ _ -> do
      tell [ "You just opened the door" ]
      pure $ return @"DoorOpen"
  )
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Monadic.purs#L13-L24">test/Examples/Monadic.purs L13-L24</a>
  </sup>
</p>

<!-- PD_END -->

The key differences from the non-monadic version are:

- We use `mkUpdateM` instead of `mkUpdate`
- We use `matchM` instead of `match`
- The update function returns `m State` instead of just `State`
- Inside each match handler, we can perform monadic operations (like `tell` for the Writer monad)
- We wrap the return value with `pure $ return @"..."` to lift it into the monad

Now we can use this monadic update function to process a sequence of messages while collecting logs:

<!-- PD_START:purs
filePath: test/Examples/Monadic.purs
pick:
  - walk
-->

```purescript
walk :: Writer Accum State
walk = do
  let s0 = v @"DoorOpen"
  s1 <- update s0 (v @"Close")
  s2 <- update s1 (v @"Open")
  s3 <- update s2 (v @"Close")
  pure s3
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Monadic.purs#L26-L32">test/Examples/Monadic.purs L26-L32</a>
  </sup>
</p>

<!-- PD_END -->

The `monadicWalk` function chains multiple state updates in a do-notation, just like any other monadic computation. Each call to `update` not only returns the next state, but also accumulates logs in the Writer monad.

Let's verify that the logs are collected correctly:

<!-- PD_START:purs
filePath: test/Examples/Monadic.purs
pick:
  - specLogs
-->

```purescript
specLogs :: Spec Unit
specLogs = do
  it "should return the correct state" do
    let
      logs :: Accum
      logs = execWriter walk

    logs `shouldEqual`
      [ "You just closed the door"
      , "You just opened the door"
      , "You just closed the door"
      ]
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Monadic.purs#L34-L45">test/Examples/Monadic.purs L34-L45</a>
  </sup>
</p>

<!-- PD_END -->

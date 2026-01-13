# Advanced Update Patterns

So far we've used `mkUpdate` to create pure update functions that simply transform state based on messages. However, real-world applications often need more sophisticated behavior: logging transitions, performing side effects, or explicitly handling invalid transitions. **Transit** provides three advanced patterns to cover these scenarios:

- **Monadic Updates** (`mkUpdateM`) - perform effects during transitions
- **Error Handling** (`mkUpdateMaybe`) - explicitly track valid vs invalid transitions
- **Combining Both** (`mkUpdateMaybeM`) - effects with error handling

Let's explore each pattern in detail.

## Monadic Updates

Full source code: _[test/Examples/Monadic.purs](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Monadic.purs)_

In some cases, you may want your update function to perform effects or collect information during state transitions. **Transit** supports monadic update functions through `mkUpdateM` and `matchM`, which allow you to work within any monad context.

This is useful for scenarios like:

- Logging state transitions
- Accumulating data during updates
- Performing other side effects

Let's see how to create an update function that logs each transition using the `Writer` monad. This is just an example, you can use any monad.

<!-- PD_START:purs { filePath: test/Examples/Monadic.purs, pick: [Accum, update] } -->

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

[test/Examples/Monadic.purs (lines 13-24)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Monadic.purs#L13-L24){.fileLink}

<!-- PD_END -->

The key differences from the non-monadic version are:

- We use `mkUpdateM` instead of `mkUpdate`
- We use `matchM` instead of `match`
- The update function returns `m State` instead of just `State`
- Inside each match handler, we can perform monadic operations (like `tell` for the Writer monad)
- We wrap the return value with `pure $ return @"..."` to lift it into the monad

Now we can use this monadic update function to process a sequence of messages while collecting logs:

<!-- PD_START:purs { filePath: test/Examples/Monadic.purs, pick: [walk] } -->

```purescript
walk :: Writer Accum State
walk = do
  let s0 = v @"DoorOpen"
  s1 <- update s0 (v @"Close")
  s2 <- update s1 (v @"Open")
  s3 <- update s2 (v @"Close")
  pure s3
```

[test/Examples/Monadic.purs (lines 26-32)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Monadic.purs#L26-L32){.fileLink}

<!-- PD_END -->

Here we chain multiple state updates in a do-notation, just like any other monadic computation. Each call to `update` not only returns the next state, but also accumulates logs in the Writer monad.

Let's verify that the logs are collected correctly:

<!-- PD_START:purs { filePath: test/Examples/Monadic.purs, pick: [specLogs] } -->

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

[test/Examples/Monadic.purs (lines 34-45)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Monadic.purs#L34-L45){.fileLink}

<!-- PD_END -->

## Error Handling

Full source code: _[test/Examples/ErrorHandling.purs](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandling.purs)_

With **Transit**'s `mkUpdate` function, you define valid transitions in your type-level specification, and the compiler ensures you implement handlers for each one. When an invalid state-message combination (one not in your specification) is encountered at runtime, `mkUpdate` silently returns the unchanged state. This is sometimes exactly what you want - the state machine simply ignores invalid messages and stays in its current state.

However, sometimes you need to explicitly know whether a transition was valid or not. For these cases, **Transit** provides `mkUpdateMaybe`, which wraps the result in a `Maybe` type. Valid transitions return `Just state`, while invalid transitions return `Nothing`, allowing you to handle the error explicitly.

Here's an update function that only handles valid transitions:

<!-- PD_START:purs { filePath: test/Examples/ErrorHandling.purs, pick: [update] } -->

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

[test/Examples/ErrorHandling.purs (lines 13-20)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandling.purs#L13-L20){.fileLink}

<!-- PD_END -->

The key difference is the return type: `State -> Msg -> Maybe State` instead of `State -> Msg -> State`. The function uses `mkUpdateMaybe` instead of `mkUpdate`, but the match handlers remain the same.

When you call this update function with a valid state-message combination, it returns `Just` with the new state:

<!-- PD_START:purs { filePath: test/Examples/ErrorHandling.purs, pick: [specSuccess] } -->

```purescript
specSuccess :: Spec Unit
specSuccess = do
  it "should return the correct state" do
    update (v @"DoorOpen") (v @"Close") `shouldEqual` Just (v @"DoorClosed")
```

[test/Examples/ErrorHandling.purs (lines 22-25)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandling.purs#L22-L25){.fileLink}

<!-- PD_END -->

But when you call it with an invalid combination (like trying to open a door that's already open), it returns `Nothing`:

<!-- PD_START:purs { filePath: test/Examples/ErrorHandling.purs, pick: [specFailure] } -->

```purescript
specFailure :: Spec Unit
specFailure = do
  it "should return the correct state" do
    update (v @"DoorOpen") (v @"Open") `shouldEqual` Nothing
```

[test/Examples/ErrorHandling.purs (lines 27-30)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandling.purs#L27-L30){.fileLink}

<!-- PD_END -->

## Combining Monads and Error Handling

Full source code: _[test/Examples/ErrorHandlingMonadic.purs](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandlingMonadic.purs)_

For more advanced scenarios, you can combine error handling with monadic effects using `mkUpdateMaybeM`.

Here's an update function that logs transitions AND returns `Maybe`. Again we use the `Writer` as example, but you can use any monad you want.

<!-- PD_START:purs { filePath: test/Examples/ErrorHandlingMonadic.purs, pick: [Accum, update] } -->

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

[test/Examples/ErrorHandlingMonadic.purs (lines 15-26)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandlingMonadic.purs#L15-L26){.fileLink}

<!-- PD_END -->

The return type is `m (Maybe State)` - combining the monad `m` (e.g. for effects) with `Maybe` (for error handling). Inside each handler, we can perform monadic operations like `tell` to log transitions.

Now let's use this in a scenario where we attempt an invalid transition in the middle of a sequence:

<!-- PD_START:purs { filePath: test/Examples/ErrorHandlingMonadic.purs, pick: [walk] } -->

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

[test/Examples/ErrorHandlingMonadic.purs (lines 28-36)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandlingMonadic.purs#L28-L36){.fileLink}

<!-- PD_END -->

Here we use the `MaybeT` transformer to work with the `Writer` monad while handling potential failures. Notice that somewhere along the way, we attempt an invalid transition (closing an already closed door). This will cause the computation to short-circuit - the subsequent transitions won't be executed.

Let's verify that the logs only contain the transitions that actually occurred:

<!-- PD_START:purs { filePath: test/Examples/ErrorHandlingMonadic.purs, pick: [spec] } -->

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

[test/Examples/ErrorHandlingMonadic.purs (lines 38-49)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/ErrorHandlingMonadic.purs#L38-L49){.fileLink}

<!-- PD_END -->

Indeed, the logs stop after the invalid transition.

module Transit.Tmp where

import Prelude

-- x =
--   [ { edges: (fromFoldable []), fromNode: "Alarm" }
--   , { edges: (fromFoldable [ { edge: { guard: Nothing, msg: "Close" }, toNodes: (fromFoldable [ "DoorClosed" ]) } ]), fromNode: "DoorOpen" }
--   , { edges: (fromFoldable [ { edge: { guard: Nothing, msg: "Lock" }, toNodes: (fromFoldable [ "DoorLocked" ]) }, { edge: { guard: Nothing, msg: "Open" }, toNodes: (fromFoldable [ "DoorOpen" ]) } ]), fromNode: "DoorClosed" }
--   , { edges:
--         ( fromFoldable
--             [ { edge: { guard: (Just "PinCorrect"), msg: "Unlock" }, toNodes: (fromFoldable [ "DoorClosed" ]) }
--             , { edge: { guard: (Just "PinIncorrect"), msg: "Unlock" }, toNodes: (fromFoldable [ "DoorLocked" ]) }
--             , { edge: { guard: (Just "TooManyAttempts"), msg: "Unlock" }
--               , toNodes: (fromFoldable [ "Alarm" ])
--               }
--             ]
--         )
--     , fromNode: "DoorLocked"
--     }
--   ]
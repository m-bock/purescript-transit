module Md.Main where

import Prelude

import Data.String as Str
import Effect (Effect)
import Patchdown as Patchdown
import Patchdown.Converters.Purs (FileLinkParams, defaultPursConfig, mkConverterPurs)
import Patchdown.Converters.Raw (converterRaw)

main :: Effect Unit
main = do
  Patchdown.mainWithConfig Patchdown.defaultConfig
    { mkConverters =
        [ mkConverterPurs defaultPursConfig
            { renderFileLink = renderFileLink
            }
        , pure converterRaw
        ]
    }

renderFileLink :: FileLinkParams -> String
renderFileLink { url, label } = Str.joinWith "\n"
  [ "[" <> label <> "](" <> url <> "){.fileLink}"
  , "\n"
  ]


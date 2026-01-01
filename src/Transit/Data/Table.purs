module Transit.Data.Table where

import Prelude

import Data.Array as Array
import Data.String as Str
import Transit.Data.Html as Html

newtype Table = Table { headers :: Array TableHeader, rows :: Array TableRow }

newtype TableRow = TableRow (Array TableCell)

data TableCell = TableCell { bold :: Boolean } String

newtype TableHeader = TableHeader String

newtype TableData = TableData String

toHtml :: Table -> Html.Node
toHtml (Table { headers, rows }) = Html.table [] $ join
  [ pure $ Html.thead [] $ map
      ( \(TableHeader header) ->
          Html.th [] [ Html.text header ]
      )
      headers
  , map
      ( \(TableRow cells) ->
          Html.tbody [] $ pure $ Html.tr [] $ map
            ( \(TableCell { bold } content) -> Html.td []
                [ if bold then Html.b [] [ Html.text content ] else Html.text content ]
            )
            cells
      )
      rows
  ]

toMarkdown :: Table -> String
toMarkdown (Table { headers, rows }) =
  let
    headerRow = Str.joinWith " | " $ map (\(TableHeader header) -> header) headers
    separatorRow = Str.joinWith " | " $ map (const "---") headers
    dataRows = map
      ( \(TableRow cells) ->
          Str.joinWith " | " $ map
            ( \(TableCell { bold } content) ->
                if bold then "**" <> content <> "**" else content
            )
            cells
      )
      rows
  in
    Str.joinWith "\n" $ [ headerRow, separatorRow ] <> dataRows
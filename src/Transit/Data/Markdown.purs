module Transit.Data.Markdown where

import Prelude

newtype Table = Table { headers :: Array TableHeader, rows :: Array TableRow }

newtype TableRow = TableRow (Array TableCell)

data TableCell = TableCell { bold :: Boolean } String

newtype TableHeader = TableHeader String

newtype TableData = TableData String

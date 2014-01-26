import Prelude

import DOM hiding (setValue, getValue)
import FFI

import Anvil
import Anvil.Data
import Anvil.Show
import Anvil.UI.Base
import Anvil.UI.Editor

main :: Fay ()
main = do
  initEditor
  item <- editorItem
  print item
  putStrLn $ showItem item
  {-putStrLn $ showItemType maybeItemType-}

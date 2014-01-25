import Prelude

import DOM
import FFI

import Anvil
import Anvil.Data
import Anvil.Show

getElementsByClass :: String -> Fay [Element]
getElementsByClass = ffi "document.getElementsByClassName(%1)"

focusElement :: Element -> Fay ()
focusElement = ffi "%1.focus()"

addEventListener :: String -> (Event -> Fay Bool) -> Element -> Fay ()
addEventListener = ffi "%3.addEventListener(%1,%2,false)"

getEventMouseButton :: Event -> Fay Int
getEventMouseButton = ffi "%1.button"

getEventElement :: Event -> Fay Element
getEventElement = ffi "%1.target"

getItemTypeElements :: Fay [Element]
getItemTypeElements = ffi "document.querySelectorAll('#editor input[name=itemType]')"


initItemTypeElements :: Fay ()
initItemTypeElements = do
    elements <- getItemTypeElements
    print elements
    forM_ (zip elements itemTypes) $ \(element, itemType) -> do
        setItemTypeValue element itemType
        setTextAfter element (showItemType itemType)
  where
    itemTypes = [Sword, Pickaxe, Shovel, Axe, Helmet, Chestplate, Leggings, Boots, Bow, FishingRod]

setTextAfter :: Element -> String -> Fay ()
setTextAfter e s = do
    p <- parentNode e
    createTextNode s >>= appendChild p

setItemTypeValue :: Element -> ItemType -> Fay ()
setItemTypeValue = ffi "%1.value = %2"

initEditor :: Fay ()
initEditor = do
    initItemTypeElements

editorItemType :: Fay ItemType
editorItemType = ffi "document.querySelector('input[name=itemType]:checked').value"

setBackgroundBlack :: Element -> Fay ()
setBackgroundBlack = ffi "%1.style.backgroundColor = '#000'"

slotClick :: Event -> Fay Bool
slotClick event = do
    elem <- getEventElement event
    setBackgroundBlack elem
    return False

setItem :: Element -> Item -> Fay ()
setItem = ffi "%1.dataset.item = %2"

getItem :: Element -> Fay (Maybe Item)
getItem = ffi "%1.dataset.item"

data Test = Test

main :: Fay ()
main = do
  initEditor
  print Test
  slots <- getElementsByClass "slot"
  forM_ slots $ addEventListener "mousedown" slotClick
  maybeItemType <- editorItemType
  print maybeItemType
  {-putStrLn $ showItemType maybeItemType-}

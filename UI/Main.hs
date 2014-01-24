import Prelude

import FFI

import Anvil
import AnvilData
import AnvilShow

data Element

getElementById :: String -> Fay Element
getElementById = ffi "document.getElementById(%1)"

getElementsByClass :: String -> Fay [Element]
getElementsByClass = ffi "document.getElementsByClassName(%1)"

focusElement :: Element -> Fay ()
focusElement = ffi "%1.focus()"

data Event

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
        setText element (showItemType itemType)
  where
    itemTypes = [Sword, Pickaxe, Shovel, Axe]

setText :: Element -> String -> Fay ()
setText = ffi "%1.parentNode.appendChild(document.createTextNode(%2))"

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

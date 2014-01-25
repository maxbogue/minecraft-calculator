import Prelude

import DOM hiding (setValue, getValue)
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

querySelector :: String -> Fay Element
querySelector = ffi "document.querySelector(%1)"

querySelectorAll :: String -> Fay [Element]
querySelectorAll = ffi "document.querySelectorAll(%1)"

bindSelectableEventListener :: Element -> [Element] -> Fay ()
bindSelectableEventListener e es = do
    addEventListener "click" (\ev -> do
        forM_ es $ \e' -> removeClass e' "selected"
        addClass e "selected"
        return True) e

initItemTypeElements :: Fay ()
initItemTypeElements = do
    elements <- querySelectorAll "#editor .itemType"
    forM_ (zip elements itemTypes) $ \(element, itemType) -> do
        setValue element itemType
        setText element (showItemType itemType)
        bindSelectableEventListener element elements
  where
    itemTypes = [Sword, Helmet, Pickaxe, Chestplate, Shovel, Leggings, Axe, Boots, Bow, FishingRod]

initMaterialElements :: Fay ()
initMaterialElements = do
    elements <- querySelectorAll "#editor .material"
    forM_ (zip elements materials) $ \(element, material) -> do
        setValue element material
        setText element (showMaterial material)
        bindSelectableEventListener element elements
  where
    materials = [Diamond, Iron, Gold, Chain, Leather, Stone, Wood]

setText :: Element -> String -> Fay ()
setText = ffi "%1.innerText = %2"

getValue :: Element -> Fay a
getValue = ffi "%1.value"

setValue :: Element -> a -> Fay ()
setValue = ffi "%1.value = %2"

initEditor :: Fay ()
initEditor = do
    initItemTypeElements
    initMaterialElements

editorItemType :: Fay ItemType
editorItemType = querySelector ".itemType.selected" >>= getValue

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

main :: Fay ()
main = do
  initEditor
  slots <- getElementsByClass "slot"
  forM_ slots $ addEventListener "mousedown" slotClick
  iT <- editorItemType
  print iT
  print $ showItemType iT
  {-putStrLn $ showItemType maybeItemType-}

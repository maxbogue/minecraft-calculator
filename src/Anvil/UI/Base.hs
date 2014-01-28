module Anvil.UI.Base where

import Prelude

import DOM
import FFI

-- Getting DOM elements

getElementsByClass :: String -> Fay [Element]
getElementsByClass = ffi "document.getElementsByClassName(%1)"

querySelector :: String -> Fay Element
querySelector = ffi "document.querySelector(%1)"

querySelectorAll :: String -> Fay [Element]
querySelectorAll = ffi "document.querySelectorAll(%1)"

-- Events

addEventListener :: String -> (Event -> Fay Bool) -> Element -> Fay ()
addEventListener = ffi "%3.addEventListener(%1,%2,false)"

getEventMouseButton :: Event -> Fay Int
getEventMouseButton = ffi "%1.button"

getEventElement :: Event -> Fay Element
getEventElement = ffi "%1.target"

bindSelectableEventListener :: Element -> [Element] -> Fay ()
bindSelectableEventListener e es = do
    addEventListener "click" (\ev -> do
        forM_ es $ \e' -> removeClass e' "selected"
        addClass e "selected"
        return True) e

-- Modifying DOM elements

setText :: Element -> String -> Fay ()
setText = ffi "%1.innerText = %2"

getAnvilValue :: Element -> Fay a
getAnvilValue = ffi "%1.anvilValue"

setAnvilValue :: Element -> a -> Fay ()
setAnvilValue = ffi "%1.anvilValue = %2"

getStringValue :: Element -> Fay String
getStringValue = ffi "%1.value"

hideElement :: Element -> Fay ()
hideElement = ffi "%1.style.display = 'none'"

showBlock :: Element -> Fay ()
showBlock = ffi "%1.style.display = 'block'"

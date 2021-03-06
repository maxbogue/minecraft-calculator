module Anvil.UI.Base where

import Prelude

import DOM
import FFI

forMaybeM :: [a] -> (a -> Fay (Maybe b)) -> Fay [b]
forMaybeM ls m = foldr forMaybeM' (return []) ls where
    forMaybeM' x ms = do
        y' <- m x
        ys <- ms
        case y' of
            Just y -> return (y:ys)
            Nothing -> return ys

replicateM_ :: Int -> Fay a -> Fay ()
replicateM_ n x = sequence_ $ replicate n x

-- Getting DOM elements

getElementsByClass :: String -> Fay [Element]
getElementsByClass = ffi "document.getElementsByClassName(%1)"

querySelector :: String -> Fay Element
querySelector = ffi "document.querySelector(%1)"

querySelectorAll :: String -> Fay [Element]
querySelectorAll = ffi "document.querySelectorAll(%1)"

firstChild :: Element -> Fay Element
firstChild = ffi "%1.firstChild"

copyElement :: Element -> Fay Element
copyElement = ffi "%1.copyNode(true)"

-- Events

addEventListener :: String -> Element -> (Event -> Fay Bool) -> Fay ()
addEventListener = ffi "%2.addEventListener(%1,%3,false)"

onClick :: Element -> (Event -> Fay Bool) -> Fay ()
onClick = addEventListener "click"

dispatchEvent :: Element -> Event -> Fay ()
dispatchEvent = ffi "%1.dispatchEvent(%2)"

eventWrapper :: Fay a -> Event -> Fay Bool
eventWrapper f ev = f >> return True

getEventMouseButton :: Event -> Fay Int
getEventMouseButton = ffi "%1.button"

getEventElement :: Event -> Fay Element
getEventElement = ffi "%1.target"

-- Modifying DOM elements

setText :: Element -> String -> Fay ()
setText = ffi "%1.innerText = %2"

getAnvilValue :: Element -> Fay a
getAnvilValue = ffi "%1.anvilValue"

setAnvilValue :: Element -> a -> Fay ()
setAnvilValue = ffi "%1.anvilValue = %2"

hideElement :: Element -> Fay ()
hideElement = ffi "%1.style.display = 'none'"

showBlock :: Element -> Fay ()
showBlock = ffi "%1.style.display = 'block'"

showTableRow :: Element -> Fay ()
showTableRow = ffi "%1.style.display = 'table-row'"

setBackgroundColor :: Element -> String -> Fay ()
setBackgroundColor = ffi "%1.style.backgroundColor = %2"

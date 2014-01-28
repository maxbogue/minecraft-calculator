module Anvil.UI.Editor (
    editorItem,
    {-hideEditor,-}
    initEditor,
    {-showEditor-}
    ) where

import Prelude

import DOM
import FFI

import Anvil
import Anvil.Data
import Anvil.Show
import Anvil.UI.Base

{-showEditor :: Element -> Fay ()-}
{-showEditor slot = do-}
    {-editor <- getElementById("editor")-}
    {-ffi "%1.style.display = 'block'" editor-}
    {-ffi "%1.slot = %2" editor slot-}

{-hideEditor :: Fay ()-}
{-hideEditor = do-}
    {-editor <- getElementById("editor")-}
    {-ffi "%1.style.display = 'none'" editor-}
    {-ffi "%1.slot = undefined" editor-}

initEditor :: Fay ()
initEditor = do
    editor <- getElementById "editor"
    initMaterialElements
    initItemTypeElements
    showBlock editor

initItemTypeElements :: Fay ()
initItemTypeElements = do
    elements <- querySelectorAll "#editor .itemType"
    forM_ (zip elements itemTypes) $ \(element, itemType) -> do
        selected <- hasClass element "selected"
        when selected $ filterMaterials itemType
        setAnvilValue element itemType
        setText element (showShortItemType itemType)
        bindSelectableEventListener element elements
        addEventListener "click" itemTypeClicked element
  where
    itemTypes = [Sword, Pickaxe, Shovel, Axe, Bow, Helmet, Chestplate, Leggings, Boots, FishingRod]

initMaterialElements :: Fay ()
initMaterialElements = do
    elements <- querySelectorAll "#editor .material"
    forM_ (zip elements materials) $ \(element, material) -> do
        setAnvilValue element material
        setText element (showShortMaterial material)
        bindSelectableEventListener element elements
  where
    materials = [Diamond, Iron, Gold, Chain, Leather, Stone, Wood]

editorItem :: Fay Item
editorItem = do
    itemType <- querySelector ".itemType.selected" >>= getAnvilValue
    material <- querySelector ".material.selected" >>= getAnvilValue
    name <- querySelector "#name" >>= getStringValue
    let nnj = if name == "" then Right 0 else Left name
    return $ makeItem itemType material [] nnj

itemTypeClicked :: Event -> Fay Bool
itemTypeClicked ev = do
    getEventElement ev >>= getAnvilValue >>= filterMaterials
    return True

filterMaterials :: ItemType -> Fay ()
filterMaterials itemType = do
    let vMats = validMaterials itemType
    matElements <- querySelectorAll "#editor .material"
    forM_ matElements $ \matElem -> do
        mat <- getAnvilValue matElem
        if mat `elem` vMats then showBlock matElem else hideElement matElem


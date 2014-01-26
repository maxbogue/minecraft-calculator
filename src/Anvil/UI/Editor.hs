module Anvil.UI.Editor (
    editorItem,
    {-hideEditor,-}
    initEditor,
    {-showEditor-}
    ) where

import Prelude

import DOM hiding (getValue, setValue)
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
    initItemTypeElements
    initMaterialElements

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

editorItem :: Fay Item
editorItem = do
    itemType <- querySelector ".itemType.selected" >>= getValue
    material <- querySelector ".material.selected" >>= getValue
    name <- querySelector "#name" >>= getStringValue
    let nnj = if name == "" then Right 0 else Left name
    return $ makeItem itemType material [] nnj

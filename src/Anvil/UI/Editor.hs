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

showEditorForSlot :: Element -> Fay ()
showEditorForSlot slot = do
    popover <- getElementById("popover")
    editor <- getElementById("editor")
    showBlock popover
    setAnvilValue editor slot

hideEditor :: Fay ()
hideEditor = do
    popover <- getElementById("popover")
    editor <- getElementById("editor")
    hideElement popover
    setAnvilValue editor Undefined

initEditor :: Fay ()
initEditor = do
    editor <- getElementById "editor"
    anvil <- getElementById "anvil"
    popover <- getElementById "popover"
    onClick anvil $ eventWrapper $ showEditorForSlot anvil
    onClick editor $ \ev -> stopProp ev >> return True
    onClick popover $ eventWrapper $ hideEditor
    initEnchantElements
    initMaterialElements
    initItemTypeElements

initItemTypeElements :: Fay ()
initItemTypeElements = do
    elements <- getElementsByClass "itemType"
    forM_ (zip elements itemTypes) $ \(element, itemType) -> do
        selected <- hasClass element "selected"
        when selected $ filterMaterials itemType >> filterEnchants itemType
        setAnvilValue element itemType
        setText element (showShortItemType itemType)
        bindSelectableEventListener element elements
        onClick element itemTypeClicked
  where
    itemTypes = [Sword, Pickaxe, Shovel, Axe, Bow, Helmet, Chestplate, Leggings, Boots, FishingRod]

initMaterialElements :: Fay ()
initMaterialElements = do
    elements <- getElementsByClass "material"
    forM_ (zip elements materials) $ \(element, material) -> do
        setAnvilValue element material
        setText element (showShortMaterial material)
        bindSelectableEventListener element elements
  where
    materials = [Diamond, Iron, Gold, Chain, Leather, Stone, Wood]

initEnchantElements :: Fay ()
initEnchantElements = do
    elements <- getElementsByClass "enchant"
    forM_ (zip elements enchantmentTs) $ \(element, enchantmentT) -> do
        nodes <- children element
        case nodeListToArray nodes of
            (name : levels) -> do
                setAnvilValue element enchantmentT
                setText name $ showEnchantmentT enchantmentT
                forM_ (zip levels [1..5]) $ \(level, i) -> do
                    setAnvilValue level i
                    onClick level (levelClicked levels)
            [] -> return ()
  where
    enchantmentTs :: [EnchantmentT]
    enchantmentTs = [
        Protection, FireProtection, FeatherFalling, BlastProtection,
        ProjectileProtection, Respiration, AquaAffinity, Thorns,
        Sharpness, Smite, BaneOfArthropods, Knockback, FireAspect, Looting,
        Efficiency, SilkTouch, Unbreaking, Fortune,
        Power, Punch, Flame, Infinity, LuckOfTheSea, Lure]
    levelClicked :: [Element] -> Event -> Fay Bool
    levelClicked levels ev = do
        level <- getEventElement ev
        selected <- hasClass level "selected"
        if selected then removeClass level "selected"
        else do
            forM_ levels (flip removeClass "selected")
            addClass level "selected"
        return True

editorItem :: Fay Item
editorItem = do
    itemType <- querySelector ".itemType.selected" >>= getAnvilValue
    material <- querySelector ".material.selected" >>= getAnvilValue
    name <- querySelector "#name" >>= getStringValue
    let nnj = if name == "" then Right 0 else Left name
    return $ makeItem itemType material [] nnj

itemTypeClicked :: Event -> Fay Bool
itemTypeClicked ev = do
    iT <- getEventElement ev >>= getAnvilValue 
    filterMaterials iT
    filterEnchants iT
    return True

filterMaterials :: ItemType -> Fay ()
filterMaterials itemType = do
    let vMats = validMaterials itemType
    matElements <- getElementsByClass "material"
    forM_ matElements $ \matElem -> do
        mat <- getAnvilValue matElem
        if mat `elem` vMats then showBlock matElem else hideElement matElem

filterEnchants :: ItemType -> Fay ()
filterEnchants iT = do
    enchantElements <- getElementsByClass "enchant"
    forM_ enchantElements $ \enchantElement -> do
        eT <- getAnvilValue enchantElement
        if iT `elem` validItemTypes eT
          then showTableRow enchantElement
          else do
            hideElement enchantElement
            removeClass enchantElement "selected"

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

select :: Element -> Fay ()
select = flip addClass "selected"

deselect :: Element -> Fay ()
deselect = flip removeClass "selected"

isSelected :: Element -> Fay Bool
isSelected = flip hasClass "selected"

bindSelectableEventListener :: Element -> [Element] -> Fay ()
bindSelectableEventListener e es = do
    onClick e $ \ev -> do
        forM_ es (flip removeClass "selected")
        addClass e "selected"
        return True

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

editorItem :: Fay Item
editorItem = do
    itemType <- querySelector ".itemType.selected" >>= getAnvilValue
    material <- querySelector ".material.selected" >>= getAnvilValue
    name <- getElementById "name" >>= getStringValue
    let nnj = if name == "" then Right 0 else Left name
    enchants <- querySelectorAll ".enchant.selected" >>= mapM extractEnchant
    return $ makeItem itemType material enchants nnj
  where
    extractEnchant :: Element -> Fay Enchantment
    extractEnchant e = do
        level <- getAnvilValue e
        eT <- parentNode e >>= getAnvilValue
        return $ Enchantment eT level

-- ItemType selection logic.

initItemTypeElements :: Fay ()
initItemTypeElements = do
    elements <- getElementsByClass "itemType"
    forM_ (zip elements itemTypes) $ \(element, itemType) -> do
        selected <- isSelected element
        when selected $ filterMaterials itemType >> filterEnchants itemType
        setAnvilValue element itemType
        setText element (showShortItemType itemType)
        bindSelectableEventListener element elements
        onClick element itemTypeClicked
  where
    itemTypes = [Sword, Pickaxe, Shovel, Axe, Bow, Helmet, Chestplate, Leggings, Boots, FishingRod]

itemTypeClicked :: Event -> Fay Bool
itemTypeClicked ev = do
    iT <- getEventElement ev >>= getAnvilValue 
    filterMaterials iT
    filterEnchants iT
    return True

-- Material selection logic.

initMaterialElements :: Fay ()
initMaterialElements = do
    elements <- getElementsByClass "material"
    forM_ (zip elements materials) $ \(element, material) -> do
        setAnvilValue element material
        setText element (showShortMaterial material)
        bindSelectableEventListener element elements
  where
    materials = [Diamond, Iron, Gold, Chain, Leather, Stone, Wood]

filterMaterials :: ItemType -> Fay ()
filterMaterials itemType = do
    let vMats = validMaterials itemType
    matElements <- getElementsByClass "material"
    shown <- forMaybeM matElements $ \matElem -> do
        mat <- getAnvilValue matElem
        if mat `elem` vMats
            then showBlock matElem >> return (Just matElem)
            else hideElement matElem >> return Nothing
    selectedInShown <- forM shown isSelected
    when (not $ any id selectedInShown) $ do
        selected <- querySelectorAll ".material.selected"
        mapM_ deselect selected
        select $ head shown

-- Enchantment selection logic.

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
                    if i <= maxLevelT enchantmentT then do
                        setAnvilValue level i
                        onClick level (levelClicked elements levels)
                    else addClass level "disabled"
            [] -> return ()
  where
    enchantmentTs :: [EnchantmentT]
    enchantmentTs = [
        Protection, FireProtection, FeatherFalling, BlastProtection,
        ProjectileProtection, Respiration, AquaAffinity, Thorns,
        Sharpness, Smite, BaneOfArthropods, Knockback, FireAspect, Looting,
        Efficiency, SilkTouch, Unbreaking, Fortune,
        Power, Punch, Flame, Infinity, LuckOfTheSea, Lure]

levelClicked :: [Element] -> [Element] -> Event -> Fay Bool
levelClicked enchantElements levels ev = do
    level <- getEventElement ev
    selected <- isSelected level
    if selected then deselect level
    else do
        eT <- parentNode level >>= getAnvilValue
        levels' <- getExclusiveEnchantLevels enchantElements levels eT 
        forM_ levels' deselect
        select level
    return True
  where
    getExclusiveEnchantLevels :: [Element] -> [Element] -> EnchantmentT -> Fay [Element]
    getExclusiveEnchantLevels enchantElements levels eT = do
        case exclusivityTagT eT of
            Just tag -> do
                eTs <- mapM getAnvilValue enchantElements
                levelss <- forMaybeM enchantElements (getLevelsIfConflicting tag)
                return $ concat levelss
            Nothing -> return levels
    getLevelsIfConflicting :: String -> Element -> Fay (Maybe [Element])
    getLevelsIfConflicting tag e = do
        eT <- getAnvilValue e
        case exclusivityTagT eT of
            Just tag' -> do
                if tag == tag'
                    then return . Just . tail . nodeListToArray =<< children e
                    else return Nothing
            Nothing -> return Nothing

filterEnchants :: ItemType -> Fay ()
filterEnchants iT = do
    enchantElements <- getElementsByClass "enchant"
    forM_ enchantElements $ \enchantElement -> do
        eT <- getAnvilValue enchantElement
        if iT `elem` validItemTypes eT
          then showTableRow enchantElement
          else do
            hideElement enchantElement
            deselect enchantElement

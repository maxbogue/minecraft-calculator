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
    updateEditorTarget

editorItemType :: Fay ItemType
editorItemType = querySelector ".itemType.selected" >>= getAnvilValue

editorMaterial :: Fay Material
editorMaterial = querySelector ".material.selected" >>= getAnvilValue

editorName :: Fay String
editorName = getElementById "name" >>= getStringValue

editorItem :: Fay Item
editorItem = do
    itemType <- editorItemType
    material <- editorMaterial
    name <- editorName
    let nnj = if name == "" then Right 0 else Left name
    enchants <- querySelectorAll ".level.selected" >>= mapM extractEnchant
    return $ makeItem itemType material enchants nnj
  where
    extractEnchant :: Element -> Fay Enchantment
    extractEnchant e = do
        level <- getAnvilValue e
        eT <- parentNode e >>= getAnvilValue
        return $ Enchantment eT level

updateEditorTarget :: Fay ()
updateEditorTarget = do
    item <- editorItem
    putStrLn $ showItem item
    let hurtItem = item {durability = 1, nameOrNumJobs = Left "test"}
    let plainItem = makePlain item
    let unitCost = if isUnitRepairable item
        then getCost (fst (unitRepair hurtItem 1))
        else 99
    let plainCost1 = getCost $ fst $ combineItems hurtItem plainItem
    let plainCost2 = getCost $ fst $ combineItems plainItem hurtItem
    let plainCost = min plainCost1 plainCost2
    editor <- getElementById "editor"
    setBackgroundColor editor $ if plainCost < 40 then "#DF9"
        else if unitCost < 40 then "#9DF" else "#F88"
    let s = show unitCost ++ "/" ++ show plainCost1 ++ "/" ++ show plainCost2
    numbers <- getElementById "numbers"
    setText numbers s

-- ItemType selection logic.

initItemTypeElements :: Fay ()
initItemTypeElements = do
    elements <- getElementsByClass "itemType"
    forM_ (zip elements itemTypes) $ \(element, itemType) -> do
        setAnvilValue element itemType
        setText element (showShortItemType itemType)
        selected <- isSelected element
        when selected $ filterMaterials itemType >> filterEnchants itemType
        bindSelectableEventListener element elements
        onClick element itemTypeClicked
  where
    itemTypes = [Sword, Pickaxe, Shovel, Axe, Bow, Helmet, Chestplate, Leggings, Boots, FishingRod]

itemTypeClicked :: Event -> Fay Bool
itemTypeClicked ev = do
    iT <- getEventElement ev >>= getAnvilValue 
    filterMaterials iT
    filterEnchants iT
    updateEditorTarget
    return True

-- Material selection logic.

initMaterialElements :: Fay ()
initMaterialElements = do
    elements <- getElementsByClass "material"
    forM_ (zip elements materials) $ \(element, material) -> do
        setAnvilValue element material
        setText element (showShortMaterial material)
        bindSelectableEventListener element elements
        onClick element $ \_ -> do
            updateMaxDurability
            updateEditorTarget
            return True
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
    updateMaxDurability

-- Enchantment selection logic.

-- Create a row of the enchantment table.
makeEnchantElement = do
    tr <- createElement "tr"
    addClass tr "enchant"
    -- The enchant name cell.
    createElement "td" >>= appendChild tr
    -- The level cells.
    replicateM_ 5 $ makeCell >>= appendChild tr
    return tr
  where
    makeCell = do
        td <- createElement "td"
        addClass td "level"
        return td

initEnchantElements :: Fay ()
initEnchantElements = do
    enchantsTable <- querySelector "#enchants tbody"
    let numEnchantTs = length enchantmentTs
    replicateM_ numEnchantTs $ makeEnchantElement >>= appendChild enchantsTable
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
        Sharpness, Smite, BaneOfArthropods, Efficiency, Power,
        Protection, FireProtection, BlastProtection, ProjectileProtection,
        FeatherFalling, Unbreaking, Respiration, Thorns, AquaAffinity,
        Fortune, SilkTouch, Looting, FireAspect, Knockback,
        Punch, Flame, Infinity, LuckOfTheSea, Lure]

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
    updateEditorTarget
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
            levels <- children enchantElement
            mapM_ deselect $ nodeListToArray levels

-- Other

updateMaxDurability :: Fay ()
updateMaxDurability = do
    iT <- editorItemType
    mat <- editorMaterial
    maxDurElem <- getElementById "maxDurability"
    setText maxDurElem $ show $ maxDurability' mat iT

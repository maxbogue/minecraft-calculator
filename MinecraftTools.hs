module MinecraftTools where

import Data.List

data ItemType = Pickaxe | Shovel | Axe | Sword | Helmet | Chestplate | Leggings | Boots | Bow | FishingRod deriving (Eq, Show)

data EnchantmentCategory = ToolE | SwordE | ArmorE | BowE | FishingRodE deriving Eq

data Material = Wood | Leather | Stone | Chain | Iron | Gold | Diamond deriving (Eq, Show)

validMaterials :: ItemType -> [Material]
validMaterials itemType = case enchantmentCategory itemType of
    ToolE -> [Wood, Stone, Iron, Gold, Diamond]
    SwordE -> [Wood, Stone, Iron, Gold, Diamond]
    ArmorE -> [Leather, Chain, Iron, Gold, Diamond]
    BowE -> [Wood]
    FishingRodE -> [Wood]

data Item = Item {
    itemType :: ItemType,
    material :: Material,
    durability :: Int,
    enchantments :: [Enchantment],
    priorWorkPenalty :: Int } deriving (Eq, Show)

makeItem :: ItemType -> Material -> [Enchantment] -> Int -> Item
makeItem it m es pwp = Item it m (maxDurability' m it) es pwp

data EnchantmentT =
    Protection |
    FireProtection |
    FeatherFalling |
    BlastProtection |
    ProjectileProtection |
    Respiration |
    AquaAffinity |
    Thorns |
    Sharpness |
    Smite |
    BaneOfArthropods |
    Knockback |
    FireAspect |
    Looting |
    Efficiency |
    SilkTouch |
    Unbreaking |
    Fortune |
    Power |
    Punch |
    Flame |
    Infinity |
    LuckOfTheSea |
    Lure deriving (Eq, Show)
    
data Enchantment = Enchantment {
    enchantmentT :: EnchantmentT,
    level :: Int } deriving (Eq, Show)

validEnchantment :: Item -> Enchantment -> Bool
validEnchantment item e = itemTypeIsIn (primaryItems e) || itemTypeIsIn (secondaryItems e) where
    itemTypeIsIn = elem $ itemType item

baseValue :: Item -> Int
baseValue item = multipleEnchantPenalty (length $ enchantments item) + totalEnchantCost where
    enchantmentCost e = level e * costPerLevel e
    totalEnchantCost = sum $ map enchantmentCost $ enchantments item

durabilityCost :: Item -> Item -> Int
durabilityCost target sacrifice
    | atMaxDurability target = 0
    | otherwise = sacrificeCost sacrifice
  where
    atMaxDurability :: Item -> Bool
    atMaxDurability i = durability i == maxDurability i
    sacrificeCost :: Item -> Int
    sacrificeCost item = 1 + (floor ((fromIntegral $ (durability item) - offset item) / 100.0))
      where
        offset :: Item -> Int
        offset item = 100 - floor (0.12 * (fromIntegral $ maxDurability item))

updateEnchant :: [Enchantment] -> Enchantment -> [Enchantment]
updateEnchant (e:es) e' = if enchantmentT e == enchantmentT e' then (e' : es) else (e : updateEnchant es e')
updateEnchant [] _ = []

combineEnchantments :: Item -> Item -> (Int, [Enchantment])
combineEnchantments target sacrifice = (enchantCost + newEnchantCost, finalEnchants) where
    sacrificeEnchants = enchantments sacrifice
    targetEnchants = enchantments target
    combineEnchantments' (e:es) = if incompatibleEnchant
        then (cost + level e, enchants)
        else case find ((enchantmentT e ==) . enchantmentT) enchants of
            Just m -> case compare (level e) (level m) of
                LT -> (cost, enchants)
                EQ -> if level e == maxLevel e
                    then (cost + costPerLevel e, enchants)
                    else (cost + costPerLevel e * 2, updateEnchant enchants (Enchantment (enchantmentT e) (level e + 1)))
                GT -> (cost + (level e - level m) * costPerLevel e * 2, updateEnchant enchants e)
            Nothing -> (cost + level e * costPerLevel e * 2, e : enchants)
      where
        eT = enchantmentT e
        (cost, enchants) = combineEnchantments' es
        eTs = map enchantmentT enchants
        incompatibleEnchant = any (exclusivityTag e ==) (map exclusivityTag es)
    combineEnchantments' [] = (0, targetEnchants)
    (enchantCost, finalEnchants) = combineEnchantments' sacrificeEnchants
    numFinalEnchants = length finalEnchants
    numNewEnchants = numFinalEnchants - length targetEnchants
    newEnchantCost = if numNewEnchants == numFinalEnchants then 0 else numNewEnchants * (numFinalEnchants - 1) + 1

combineItems :: Item -> Item -> (Int, [Enchantment])
combineItems target sacrifice = (baseValue target +
    priorWorkPenalty target + priorWorkPenalty sacrifice +
    durabilityCost target sacrifice +
    enchantCost, finalEnchants)
  where
    (enchantCost, finalEnchants) = combineEnchantments target sacrifice

enchantmentCategory :: ItemType -> EnchantmentCategory
enchantmentCategory Pickaxe = ToolE
enchantmentCategory Shovel = ToolE
enchantmentCategory Axe = ToolE
enchantmentCategory Sword = SwordE
enchantmentCategory Helmet = ArmorE
enchantmentCategory Chestplate = ArmorE
enchantmentCategory Leggings = ArmorE
enchantmentCategory Boots = ArmorE
enchantmentCategory Bow = BowE
enchantmentCategory FishingRod = FishingRodE

multipleEnchantPenalty :: Int -> Int
multipleEnchantPenalty 1 = 1
multipleEnchantPenalty 2 = 3
multipleEnchantPenalty 3 = 6
multipleEnchantPenalty 4 = 10
multipleEnchantPenalty 5 = 15
multipleEnchantPenalty _ = error "You can't have more than 5 enchantments."

-- (maxLevel, costPerLevel, primaryItems, secondaryItems, exclusivityTag)
enchantmentData :: Enchantment -> (Int, Int, [ItemType], [ItemType], Maybe String)
enchantmentData enchantment = case enchantmentT enchantment of
    Protection -> (4, 1, armor, [], Just "protection")
    FireProtection -> (4, 2, armor, [], Just "protection")
    FeatherFalling -> (4, 2, [Boots], [], Nothing)
    BlastProtection -> (4, 4, armor, [], Just "protection")
    ProjectileProtection -> (4, 2, armor, [], Just "protection")
    Respiration -> (3, 4, [Helmet], [], Nothing)
    AquaAffinity -> (1, 4, [Helmet], [], Nothing)
    Thorns -> (3, 8, [Chestplate], [Helmet, Leggings, Boots], Nothing)
    Sharpness -> (5, 1, [Sword], [Axe], Just "sword_damage")
    Smite -> (5, 2, [Sword], [Axe], Just "sword_damage")
    BaneOfArthropods -> (5, 2, [Sword], [Axe], Just "sword_damage")
    Knockback -> (2, 2, [Sword], [], Nothing)
    FireAspect -> (2, 4, [Sword], [], Nothing)
    Looting -> (3, 4, [Sword], [], Nothing)
    Efficiency -> (5, 1, tools, [], Nothing)
    SilkTouch -> (1, 8, tools, [], Just "tool_special")
    Unbreaking -> (3, 2, armor ++ tools ++ [Sword, FishingRod, Bow], [], Nothing)
    Fortune -> (3, 4, tools, [], Just "tool_special")
    Power -> (5, 1, [Bow], [], Nothing)
    Punch -> (2, 4, [Bow], [], Nothing)
    Flame -> (1, 4, [Bow], [], Nothing)
    Infinity -> (1, 8, [Bow], [], Nothing)
    LuckOfTheSea -> (3, 4, [FishingRod], [], Nothing)
    Lure -> (3, 4, [FishingRod], [], Nothing)
  where
    armor = [Helmet, Chestplate, Leggings, Boots]
    tools = [Pickaxe, Shovel, Axe]

maxLevel :: Enchantment -> Int
maxLevel enchantment = maxLevel' $ enchantmentData enchantment where
    maxLevel' (x, _, _, _, _) = x

costPerLevel :: Enchantment -> Int
costPerLevel enchantment = costPerLevel' $ enchantmentData enchantment where
    costPerLevel' (_, x, _, _, _) = x

primaryItems :: Enchantment -> [ItemType]
primaryItems enchantment = primaryItems' $ enchantmentData enchantment where
    primaryItems' (_, _, x, _, _) = x

secondaryItems :: Enchantment -> [ItemType]
secondaryItems enchantment = secondaryItems' $ enchantmentData enchantment where
    secondaryItems' (_, _, _, x, _) = x

exclusivityTag :: Enchantment -> Maybe String
exclusivityTag enchantment = exclusivityTag' $ enchantmentData enchantment where
    exclusivityTag' (_, _, _, _, x) = x

maxDurability :: Item -> Int
maxDurability item = maxDurability' (material item) (itemType item)

maxDurability' material itemType
    | elem itemType [Sword, Pickaxe, Axe, Shovel] = case material of
        Wood -> 60
        Stone -> 132
        Iron -> 251
        Gold -> 33
        Diamond -> 1562
    | otherwise = case (material, itemType) of
        (Leather, Helmet) -> 56
        (Gold, Helmet) -> 78
        (Iron, Helmet) -> 166
        (Diamond, Helmet) -> 364
        (Leather, Chestplate) -> 81
        (Gold, Chestplate) -> 113
        (Iron, Chestplate) -> 241
        (Diamond, Chestplate) -> 529
        (Leather, Leggings) -> 76
        (Gold, Leggings) -> 106
        (Iron, Leggings) -> 226
        (Diamond, Leggings) -> 496
        (Leather, Boots) -> 66
        (Gold, Boots) -> 92
        (Iron, Boots) -> 196
        (Diamond, Boots) -> 430
        (Wood, Bow) -> 385
        (Wood, FishingRod) -> 1
        (x, y) -> error $ "Can't have a " ++ show y ++ " of material " ++ show x 

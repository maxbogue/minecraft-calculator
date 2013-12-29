module MinecraftTools where

import Prelude

-- My own implementation of find so that this is Fay and GHCi compatible.
find' p (e:es) = if p e then Just e else find' p es
find' p [] = Nothing

join :: String -> [String] -> String
join sep [] =  ""
join sep [s] = s
join sep (s:ss) = s ++ sep ++ join sep ss

data AnnotatedCost = CostNode String [AnnotatedCost] | CostLeaf String Int

showCost :: AnnotatedCost -> String
showCost ac = showCost' "" ac where
    showCost' prefix ac@(CostNode s subNodes) =
        prefix ++ s ++ ": " ++ (show $ getCost ac) ++ "\n" ++ showCostSubNodes
      where
        showCostSubNodes = concatMap (showCost' $ "  " ++ prefix) subNodes
    showCost' prefix (CostLeaf s i) = prefix ++ s ++ ": " ++ show i ++ "\n"

getCost :: AnnotatedCost -> Int
getCost (CostNode _ cs) = sum $ map getCost cs
getCost (CostLeaf _ c) = c

data ItemType = Pickaxe | Shovel | Axe | Sword | Helmet | Chestplate | Leggings | Boots | Bow | FishingRod deriving (Eq, Show)

showItemType itemType = case itemType of
    Pickaxe -> "Pickaxe"
    Shovel -> "Shovel"
    Axe -> "Axe"
    Sword -> "Sword"
    Helmet -> "Helmet"
    Chestplate -> "Chestplate"
    Leggings -> "Leggings"
    Boots -> "Boots"
    Bow -> "Bow"
    FishingRod -> "Fishing Rod"

data EnchantmentCategory = ToolE | SwordE | ArmorE | BowE | FishingRodE deriving Eq

data Material = Wood | Leather | Stone | Chain | Iron | Gold | Diamond deriving (Eq, Show)

showMaterial Wood = "Wood"
showMaterial Leather = "Leather"
showMaterial Stone = "Stone"
showMaterial Chain = "Chain"
showMaterial Iron = "Iron"
showMaterial Gold = "Gold"
showMaterial Diamond = "Diamond"

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
    nameOrNumJobs :: Either String Int }

showItem i@(Item iT mat dur es nnj) = "[" ++ matType ++ nnj' ++ dur' ++ es'
  where
    matType = (showMaterial mat) ++ " " ++ (showItemType iT)
    nnj' = case nnj of
        Left name -> " \"" ++ name ++ "\""
        Right numJobs -> " (" ++ show numJobs ++ " jobs)"
    dur' = " " ++ show dur ++ "/" ++ (show $ maxDurability i)
    es' = " " ++ join ", " (map showEnchantment es)

priorWorkPenalty :: Item -> Int
priorWorkPenalty i = case nameOrNumJobs i of
    Left name -> 2
    Right numJobs -> 2 * numJobs

makeItem :: ItemType -> Material -> [Enchantment] -> Either String Int -> Item
makeItem it m es nnj = Item it m (maxDurability' m it) es nnj

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

showEnchantmentT e = case e of
    Protection -> "Protection"
    FireProtection -> "Fire Protection"
    FeatherFalling -> "Feather Falling"
    BlastProtection -> "Blast Protection"
    ProjectileProtection -> "Projectile Protection"
    Respiration -> "Respiration"
    AquaAffinity -> "Aqua Affinity"
    Thorns -> "Thorns"
    Sharpness -> "Sharpness"
    Smite -> "Smite"
    BaneOfArthropods -> "Bane Of Arthropods"
    Knockback -> "Knockback"
    FireAspect -> "Fire Aspect"
    Looting -> "Looting"
    Efficiency -> "Efficiency"
    SilkTouch -> "Silk Touch"
    Unbreaking -> "Unbreaking"
    Fortune -> "Fortune"
    Power -> "Power"
    Punch -> "Punch"
    Flame -> "Flame"
    Infinity -> "Infinity"
    LuckOfTheSea -> "Luck Of The Sea"
    Lure -> "Lure"

data Enchantment = Enchantment {
    enchantmentT :: EnchantmentT,
    level :: Int } deriving (Eq, Show)

showEnchantment (Enchantment eT level) = showEnchantmentT eT ++ " " ++ show level

validEnchantment :: Item -> Enchantment -> Bool
validEnchantment item e = itemTypeIsIn (primaryItems e) || itemTypeIsIn (secondaryItems e) where
    itemTypeIsIn = elem $ itemType item

baseValue :: Item -> AnnotatedCost
baseValue item = CostNode "Target base value" [
    enchantBaseCost,
    CostLeaf "Multiple enchant penalty" mep]
  where
    mep = multipleEnchantPenalty (length $ enchantments item)
    enchantmentCost e = CostLeaf ((show $ enchantmentT e) ++ " " ++ (show $ level e) ++ " x " ++ (show $ costPerLevel e)) $ level e * costPerLevel e
    enchantBaseCost = CostNode "Enchantment base costs" $ map enchantmentCost $ enchantments item

durabilityCost :: Item -> Item -> AnnotatedCost
durabilityCost target sacrifice
    | atMaxDurability target = CostLeaf "Durability cost" 0
    | otherwise = CostLeaf "Durability cost" $ sacrificeCost sacrifice
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

combineEnchantments :: Item -> Item -> (AnnotatedCost, [Enchantment])
combineEnchantments target sacrifice = (annotatedCost, finalEnchants) where
    sacrificeEnchants = enchantments sacrifice
    targetEnchants = enchantments target
    combineEnchantments' (e:es) = if incompatibleEnchant
        then (CostLeaf (enchName ++ ", incompatible") (level e) : costs, enchants)
        else case find' ((enchantmentT e ==) . enchantmentT) enchants of
            Just m -> case compare (level e) (level m) of
                LT -> (costs, enchants)
                EQ -> if level e == maxLevel e
                    then (CostLeaf (enchName ++ ", max level") (costPerLevel e) : costs, enchants)
                    else (CostLeaf (enchName ++ ", same level") (costPerLevel e * 2) : costs,
                        updateEnchant enchants (Enchantment (enchantmentT e) (level e + 1)))
                GT -> (CostLeaf (enchName ++ ", upgrade") ((level e - level m) * costPerLevel e * 2) : costs,
                    updateEnchant enchants e)
            Nothing -> (CostLeaf (enchName ++ ", new") (level e * costPerLevel e * 2) : costs, e : enchants)
      where
        enchName = show $ enchantmentT e
        (costs, enchants) = combineEnchantments' es
        incompatibleEnchant = any (exclusive e) enchants
    combineEnchantments' [] = ([], targetEnchants)
    (enchantCosts, finalEnchants) = combineEnchantments' sacrificeEnchants
    numFinalEnchants = length finalEnchants
    numNewEnchants = numFinalEnchants - length targetEnchants
    newEnchantCost = if numNewEnchants == 0 then 0 else numNewEnchants * (numFinalEnchants - 1) + 1
    annotatedCost = CostNode "Changed enchantment cost" [CostNode "Base enchantment costs" enchantCosts, CostLeaf "New enchant penalty" newEnchantCost]

combineItems :: Item -> Item -> (AnnotatedCost, [Enchantment])
combineItems target sacrifice = (
    CostNode "Total cost" [
        baseValue target,
        CostNode "Prior work penalty" [
            CostLeaf "Target" (priorWorkPenalty target),
            CostLeaf "Sacrifice" (priorWorkPenalty sacrifice)],
        durabilityCost target sacrifice,
        enchantCost],
    finalEnchants)
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

exclusive :: Enchantment -> Enchantment -> Bool
exclusive e1 e2 = case (exclusivityTag e1, exclusivityTag e2) of
    (Just t1, Just t2) -> enchantmentT e1 /= enchantmentT e2
    _ -> False

pixie = makeItem Pickaxe Diamond [Enchantment Fortune 2, Enchantment Efficiency 3, Enchantment Unbreaking 3] (Left "Pixie")
pointy = makeItem Sword Diamond [Enchantment Sharpness 3, Enchantment Knockback 2, Enchantment Looting 3] (Left "Pointy")
i1 = makeItem Sword Diamond [Enchantment Sharpness 3, Enchantment Looting 3] (Right 2)

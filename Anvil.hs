module Anvil where

import Prelude

import AnvilData
import AnvilShow

-- My own implementation of find so that this is Fay and GHCi compatible.
find' p (e:es) = if p e then Just e else find' p es
find' p [] = Nothing

priorWorkPenalty :: Item -> Int
priorWorkPenalty i = case nameOrNumJobs i of
    Left name -> 2
    Right numJobs -> 2 * numJobs

makeItem :: ItemType -> Material -> [Enchantment] -> Either String Int -> Item
makeItem it m es nnj = Item it m (maxDurability' m it) es nnj

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

pixie = makeItem Pickaxe Diamond [Enchantment Fortune 2, Enchantment Efficiency 3, Enchantment Unbreaking 3] (Left "Pixie")
pointy = makeItem Sword Diamond [Enchantment Sharpness 3, Enchantment Knockback 2, Enchantment Looting 3] (Left "Pointy")
i1 = makeItem Sword Diamond [Enchantment Sharpness 3, Enchantment Looting 3] (Right 2)

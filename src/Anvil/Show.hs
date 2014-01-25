module Anvil.Show where

import Prelude
import Anvil.Data

join :: String -> [String] -> String
join sep [] =  ""
join sep [s] = s
join sep (s:ss) = s ++ sep ++ join sep ss

data AnnotatedCost = CostNode String [AnnotatedCost] | CostLeaf String Int

getCost :: AnnotatedCost -> Int
getCost (CostNode _ cs) = sum $ map getCost cs
getCost (CostLeaf _ c) = c

showCost :: AnnotatedCost -> String
showCost ac = showCost' "" ac where
    showCost' prefix ac@(CostNode s subNodes) =
        prefix ++ s ++ ": " ++ (show $ getCost ac) ++ "\n" ++ showCostSubNodes
      where
        showCostSubNodes = concatMap (showCost' $ "  " ++ prefix) subNodes
    showCost' prefix (CostLeaf s i) = prefix ++ s ++ ": " ++ show i ++ "\n"

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

showMaterial Wood = "Wood"
showMaterial Leather = "Leather"
showMaterial Stone = "Stone"
showMaterial Chain = "Chain"
showMaterial Iron = "Iron"
showMaterial Gold = "Gold"
showMaterial Diamond = "Diamond"

showItem i@(Item iT mat dur es nnj) = "[" ++ matType ++ nnj' ++ dur' ++ es'
  where
    matType = (showMaterial mat) ++ " " ++ (showItemType iT)
    nnj' = case nnj of
        Left name -> " \"" ++ name ++ "\""
        Right numJobs -> " (" ++ show numJobs ++ " jobs)"
    dur' = " " ++ show dur ++ "/" ++ (show $ maxDurability i)
    es' = (if null es then "" else " ") ++ join ", " (map showEnchantment es)

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

showEnchantment (Enchantment eT level) = showEnchantmentT eT ++ " " ++ show level

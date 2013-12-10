module MinecraftTools where

data ItemType = Pickaxe | Shovel | Axe | Sword | Helmet | Chestplate | Leggings | Boots | Bow | FishingRod deriving (Eq, Show)

data EnchantmentCategory = ToolE | SwordE | ArmorE | BowE | FishingRodE deriving Eq

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

data Material = Wood | Leather | Stone | Chain | Iron | Gold | Diamond deriving (Eq, Show)

validMaterials :: ItemType -> [Material]
validMaterials itemType = case enchantmentCategory itemType of
    ToolE -> [Wood, Stone, Iron, Gold, Diamond]
    SwordE -> [Wood, Stone, Iron, Gold, Diamond]
    ArmorE -> [Leather, Chain, Iron, Gold, Diamond]
    BowE -> [Wood]
    FishingRodE -> [Wood]
    
data ItemT = ItemT {
     itemType :: ItemType,
     material :: Material } deriving Eq

data Item = Item {
    itemT :: ItemT,
    durability :: Int,
    enchantments :: [Enchantment],
    priorWorkPenalty :: Int } deriving Eq

maxDurability :: Item -> Int
maxDurability item = error "wat" -- case enchantCategory (itemType item) of 

data EnchantmentT = EnchantmentT {
    name :: String,
    maxLevel :: Int,
    costPerLevel :: Int,
    primaryItems :: [ItemType],
    secondaryItems :: [ItemType] } deriving Eq

data Enchantment = Enchantment {
    enchantmentT :: EnchantmentT,
    level :: Int } deriving Eq

validEnchantment :: Item -> Enchantment -> Bool
validEnchantment item enchantment = itemTypeIsIn (primaryItems eT) || itemTypeIsIn (secondaryItems eT) where
    itemTypeIsIn = elem $ itemType $ itemT item
    eT = enchantmentT enchantment

sacrificeRepairCost :: Item -> Int
sacrificeRepairCost item = 1 + (floor ((fromIntegral $ 1513 - offset item) / 100.0)) where
    offset :: Item -> Int
    offset item = 100 - floor (0.12 * (fromIntegral $ maxDurability item))

multipleEnchantPenalty :: Int -> Int
multipleEnchantPenalty 1 = 1
multipleEnchantPenalty 2 = 3
multipleEnchantPenalty 3 = 6
multipleEnchantPenalty 4 = 10
multipleEnchantPenalty 5 = 15
multipleEnchantPenalty _ = error "You can't have more than 5 enchantments."

baseValue :: Item -> Int
baseValue item = multipleEnchantPenalty (length $ enchantments item) + (sum $ map enchantmentCost $ enchantments item) where
    enchantmentCost :: Enchantment -> Int
    enchantmentCost (Enchantment eT level) = level * maxLevel eT

durabilityCost :: Item -> Item -> Int
durabilityCost target sacrifice = error "Undefined"

enchantmentCost :: Item -> Item -> Int
enchantmentCost target sacrifice = error "Undefined"

combiningItemsCost :: Item -> Item -> Int
combiningItemsCost target sacrifice = baseValue target + priorWorkPenalty target + priorWorkPenalty sacrifice + durabilityCost target sacrifice + enchantmentCost target sacrifice

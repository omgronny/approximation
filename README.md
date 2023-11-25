# Лабораторная работа 2

## Реализация

```haskell

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq)

--------------------------------------------------------------------------------

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
  | x < y = Node y (insert x left) right
  | x > y = Node y left (insert x right)
  | otherwise = Node y left right

--------------------------------------------------------------------------------

-- delete
delete :: (Ord a) => a -> Tree a -> Tree a
delete x Leaf = Node x Leaf Leaf
delete x (Node y left right)
  | x < y = Node y (delete x left) right
  | x > y = Node y left (delete x right)
  | otherwise = deleteRoot (Node y left right)

deleteRoot :: (Ord a) => Tree a -> Tree a
deleteRoot Leaf = Leaf
deleteRoot (Node _ left Leaf) = left
deleteRoot (Node _ Leaf right) = right
deleteRoot (Node _ left right) = Node (findMinElement right) (extractMinElement left) right

findMinElement :: (Ord a) => Tree a -> a
findMinElement (Node y Leaf _) = y
findMinElement (Node _ left _) = findMinElement left

extractMinElement :: (Ord a) => Tree a -> Tree a
extractMinElement (Node _ Leaf right) = right
extractMinElement (Node x left right) = Node x (extractMinElement left) right

--------------------------------------------------------------------------------

contains :: (Ord a) => Tree a -> a -> Bool
contains Leaf _ = False
contains (Node y left right) x
  | x < y = contains left x
  | x > y = contains right x
  | otherwise = True

--------------------------------------------------------------------------------

mapSet :: (Ord a) => (a -> a) -> Tree a -> Tree a
mapSet mapper tree = mapSetRecursive tree Leaf
  where
    mapSetRecursive Leaf mapped = mapped
    mapSetRecursive (Node x left right) mapped =
      let leftResult = mapSetRecursive left (insert (mapper x) mapped)
       in mapSetRecursive right leftResult

filterSet :: (Ord a) => (a -> Bool) -> Tree a -> Tree a
filterSet predicate tree = filterSetRecursive tree Leaf
  where
    filterSetRecursive Leaf filtered = filtered
    filterSetRecursive (Node x left right) filtered =
      let leftResult = filterSetRecursive left (maybeInsert x filtered)
       in filterSetRecursive right leftResult

    maybeInsert x tree' = if predicate x then insert x tree' else tree'

--------------------------------------------------------------------------------

foldrSet :: (a -> a1 -> a1) -> a1 -> Tree a -> a1
foldrSet foldFun begin tree = foldrSetRecursive tree begin
  where
    foldrSetRecursive Leaf result = result
    foldrSetRecursive (Node x left right) result =
      let rightResult = foldrSetRecursive right result
       in foldrSetRecursive left (foldFun x rightResult)

foldlSet :: (a1 -> a -> a1) -> a1 -> Tree a -> a1
foldlSet foldFun begin tree = foldlSetRecursive tree begin
  where
    foldlSetRecursive Leaf result = result
    foldlSetRecursive (Node x left right) result =
      let leftResult = foldlSetRecursive left result
       in foldlSetRecursive right (foldFun leftResult x)

--------------------------------------------------------------------------------

mergeSets :: (Ord a) => Tree a -> Tree a -> Tree a
mergeSets Leaf rhs = rhs
mergeSets lhs Leaf = lhs
mergeSets lhs (Node x left right) =
  let leftResult = mergeSets (insert x lhs) left
   in mergeSets leftResult right

```

### Тесты

``` haskell

prop_insert_delete :: [Int] -> Int -> Bool
prop_insert_delete list elementToInsert = do
    let tree = listToSet list
    let inserted = insert elementToInsert tree

    if contains tree elementToInsert then
        tree == inserted
    else
        tree == delete elementToInsert inserted

prop_filter :: [Int] -> Int -> Bool
prop_filter list threshold = do
    let tree = listToSet list
    let filtered = filterSet (> threshold) tree

    all (> threshold) (setToList filtered) &&
        all (contains filtered) (filter (> threshold) list)

prop_merge :: [Int] -> [Int] -> Bool
prop_merge list1 list2 = do
    let tree1 = listToSet list1
    let tree2 = listToSet list2

    let merged = mergeSets tree1 tree2

    all (contains merged) (setToList tree1) &&
        all (contains merged) (setToList tree2)

prop_merge_assoc :: [Int] -> [Int] -> [Int] -> Bool
prop_merge_assoc list1 list2 list3 = do
    let tree1 = listToSet list1
    let tree2 = listToSet list2
    let tree3 = listToSet list3

    let merged1 = mergeSets (mergeSets tree1 tree2) tree3
    let merged2 = mergeSets tree1 (mergeSets tree2 tree3)

    all (contains merged1) (setToList merged2) &&
        all (contains merged2) (setToList merged1)
```




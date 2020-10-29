-- Insertion Sort
insertionSort :: (Ord a) => [a] -> [a]
insert :: (Ord a) => a -> [a] -> [a]

insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)

insert x [] = [x]
insert x (y:ys) =
  if x <= y
     then x : y : ys
     else y : insert x ys

-- Merge Sort
mergeSort :: (Ord a) => [a] -> [a]
merge :: (Ord a) => [a] -> [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l =
  merge (mergeSort right) (mergeSort left)
    where
      right = take (length l `div` 2) l
      left = drop (length l `div` 2) l

merge l [] = l 
merge [] r = r
merge (x:xs) (y:ys) = 
  if x <= y
     then x : y : merge xs ys
     else y : merge (x:xs) ys

-- In mergeSort, we calculate the value of the expression length' l `div` 2
-- twice. Modify the mergeSort function to remove this inefficiency  
mergeSortEff :: (Ord a) => [a] -> [a]
mergeEff  :: (Ord a) => [a] -> [a] -> [a]

mergeSortEff  [] = []
mergeSortEff  [x] = [x]
mergeSortEff  l =
  mergeEff  (mergeSortEff  right) (mergeSortEff  left)
    where
      center = length l `div` 2
      right = take center l
      left = drop center l

mergeEff  l [] = l 
mergeEff  [] r = r
mergeEff  (x:xs) (y:ys) = 
  if x <= y
     then x : y : mergeEff  xs ys
     else y : mergeEff  (x:xs) ys

-- Write a version of insertion sort which sorts the argument list into reverse
-- order
insertionSortRev :: (Ord a) => [a] -> [a]
insertRev :: (Ord a) => a -> [a] -> [a]

insertionSortRev [] = []
insertionSortRev  [x] = [x]
insertionSortRev  (x:xs) = insertRev x (insertionSortRev xs)

insertRev  x [] = [x]
insertRev  x (y:ys) =
  if x >= y
     then x : y : ys
     else y : insertRev  x ys

-- Combine sort and insert functions into a single sortComplete function, first
-- using the where construct then using the let construct instead. 
insertionSortComplete :: (Ord a) => [a] -> [a]

insertionSortComplete [] = []
insertionSortComplete [x] = [x]
insertionSortComplete (x:xs) = insertComplete x (insertionSortComplete xs)
  where
    insertComplete a [] = [a]
    insertComplete a (b:bs) =
      if a <= b
        then a : b : bs
        else b : insertComplete a bs

mergeSortComplete :: (Ord a) => [a] -> [a]

mergeSortComplete  [] = []
mergeSortComplete  [x] = [x]
mergeSortComplete  l =
  mergeComplete (mergeSortComplete right) (mergeSortComplete left)
    where
      right = take (length l `div` 2) l
      left = drop (length l `div` 2) l

      mergeComplete l' [] = l'
      mergeComplete [] r = r
      mergeComplete (x:xs) (y:ys) = 
        if x <= y
          then x : y : merge xs ys
          else y : merge (x:xs) ys
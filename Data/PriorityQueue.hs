{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Priority Queues Implement Via Binomial Heaps.
module Data.PriorityQueue (
    PriorityQueue,
    empty,
    insert,
    findMin,
    deleteMin,
    findDeleteMin,
) where


newtype PriorityQueue k p = PriorityQueue
    { fromPriorityQueue :: [BinTree k p]
    }
    deriving (Show, Read, Eq, Ord)


data BinTree k p = BinNode
    { bnRank :: !Int
    , bnElement :: k
    , bnPriority :: !p
    , bnChildren :: ![BinTree k p]
    }
    deriving (Show, Read, Eq, Ord)


empty :: PriorityQueue k p
empty = PriorityQueue []


-- | Link trees with larger roots under trees with smaller roots, resulting
-- in a tree with an increased rank.
link :: (Ord p) => BinTree k p -> BinTree k p -> BinTree k p
link bt1 bt2 =
    if bnPriority bt1 <= bnPriority bt2
        then
            bt1
                { bnRank = succ $ bnRank bt1
                , bnChildren = bt2 : bnChildren bt1
                }
        else
            bt2
                { bnRank = succ $ bnRank bt2
                , bnChildren = bt1 : bnChildren bt2
                }


-- | Insert a new value & priority into the queue.
insert :: (Ord p) => k -> p -> PriorityQueue k p -> PriorityQueue k p
insert k p =
    let
        newTree = BinNode {bnRank = 0, bnElement = k, bnPriority = p, bnChildren = []}
     in
        PriorityQueue . insert_ newTree . fromPriorityQueue


-- | Insert the tree into a missing rank, linking trees & pushing them
-- further right if lower rank.
insert_ :: (Ord p) => BinTree k p -> [BinTree k p] -> [BinTree k p]
insert_ newTree = \case
    [] -> [newTree]
    oldTrees@(t : ts) ->
        if bnRank newTree < bnRank t
            then newTree : oldTrees
            else insert_ (link newTree t) ts


merge :: forall k p. (Ord p) => PriorityQueue k p -> PriorityQueue k p -> PriorityQueue k p
merge (PriorityQueue pq1) (PriorityQueue pq2) = PriorityQueue $ go (pq1, pq2)
  where
    go :: ([BinTree k p], [BinTree k p]) -> [BinTree k p]
    go = \case
        (p1ts, []) ->
            p1ts
        ([], p2ts) ->
            p2ts
        (p1t : p1ts, p2t : p2ts) ->
            if
                | bnRank p1t < bnRank p2t ->
                    p1t : go (p1ts, pq2)
                | bnRank p2t < bnRank p1t ->
                    p2t : go (pq1, p2ts)
                | otherwise ->
                    insert_ (link p1t p2t) (go (p1ts, p2ts))


removeMinTree :: (Ord p) => [BinTree k p] -> Maybe (BinTree k p, [BinTree k p])
removeMinTree = \case
    [] -> Nothing
    [t] -> Just (t, [])
    t : rest ->
        case removeMinTree rest of
            Nothing ->
                Just (t, [])
            Just (t2, rest2) ->
                if bnPriority t < bnPriority t2
                    then Just (t, rest)
                    else Just (t2, t : rest2)


findMin :: (Ord p) => PriorityQueue k p -> Maybe k
findMin (PriorityQueue pq) =
    bnElement . fst <$> removeMinTree pq


deleteMin :: (Ord p) => PriorityQueue k p -> PriorityQueue k p
deleteMin pq =
    maybe pq snd $ findDeleteMin pq


findDeleteMin :: (Ord p) => PriorityQueue k p -> Maybe (k, PriorityQueue k p)
findDeleteMin (PriorityQueue pq) = case removeMinTree pq of
    Nothing -> Nothing
    Just (removed, remaining) ->
        Just
            ( bnElement removed
            , merge
                (PriorityQueue . reverse $ bnChildren removed)
                (PriorityQueue remaining)
            )

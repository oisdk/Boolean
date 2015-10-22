import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe

data Expr = Const Bool
          | Var Char
          | NOT Expr
          | AND Expr Expr
          | OR  Expr Expr deriving (Eq, Ord)
          
prec :: Expr -> Int
prec (Const _) = 8
prec (Var   _) = 8
prec (NOT   _) = 8
prec (AND _ _) = 5
prec (OR  _ _) = 3

condBrack :: Int -> Expr -> String
condBrack n e | prec e < n = "(" ++ show e ++ ")"
              | otherwise  = show e

xor :: Expr -> Expr -> Expr
xor a b = (NOT a `AND` b) `OR` (NOT b `AND` a)

nand :: Expr -> Expr -> Expr
nand a b = NOT a `OR` NOT b

nor :: Expr -> Expr -> Expr
nor a b = NOT a `AND` NOT b

xnor :: Expr -> Expr -> Expr
xnor a b = NOT a `OR` b

instance Show Expr where
  show (Const b)             = if b then "1" else "0"
  show (Var   c)             = [c]
  show (NOT   e)             = '!' : condBrack 8 e
  show (AND (NOT l) (NOT r)) = condBrack 3 l ++ " NOR "  ++ condBrack 3 r
  show (AND l r)             = condBrack 5 l ++ condBrack 5 r
  show (OR (NOT l) (NOT r))  = condBrack 5 l ++ " NAND " ++ condBrack 5 r
  show (OR (NOT l) r      )  = condBrack 2 l ++ " XNOR " ++ condBrack 2 r
  show (OR l@(AND (NOT a) b) r@(AND (NOT c) d))
    | a == d && b == c       = condBrack 2 a ++ " XOR "  ++ condBrack 2 b
    | otherwise              = condBrack 3 l ++ " + "    ++ condBrack 3 r
  show (OR  l r)             = condBrack 3 l ++ " + "    ++ condBrack 3 r
          
allVars :: Expr -> S.Set Char
allVars (Const _) = S.empty
allVars (Var   c) = S.singleton c
allVars (NOT   e) = allVars e
allVars (AND l r) = S.union (allVars l) (allVars r)
allVars (OR  l r) = S.union (allVars l) (allVars r)

subIn :: (Char, Expr) -> Expr -> Expr
subIn _ (Const c) = Const c
subIn (c,e) (Var   l) | c == l    = e
                      | otherwise = Var l
subIn p (NOT   x) = NOT (subIn p x)
subIn p (AND a b) = AND (subIn p a) (subIn p b)
subIn p (OR  a b) = OR  (subIn p a) (subIn p b)

powerSet :: Ord a => (S.Set a) -> S.Set (M.Map a Bool)
powerSet = S.foldr f (S.singleton M.empty) where 
  f e a = S.union (with True) (with False) where 
    with = flip (S.map . M.insert e) a
  
subInSet :: Expr -> M.Map Char Bool -> Bool
subInSet (Const b) _ = b
subInSet (Var   c) s = fromJust (M.lookup c s)
subInSet (NOT   e) s = not (subInSet e s)
subInSet (AND l r) s = subInSet l s && subInSet r s
subInSet (OR  l r) s = subInSet l s || subInSet r s

minTerms :: Expr -> S.Set (M.Map Char Bool)
minTerms = powerSet . allVars

minTermsTrue :: Expr -> S.Set (M.Map Char Bool)
minTermsTrue e = S.filter (subInSet e) (minTerms e)

mapMaybeSet :: Ord b => (a -> Maybe b) -> S.Set a -> S.Set b
mapMaybeSet f = S.foldr ff S.empty
  where ff e a = case f e of Nothing -> a
                             Just x -> S.insert x a

tryInsert :: S.Set (M.Map Char Bool) -> M.Map Char Bool -> S.Set (M.Map Char Bool)
tryInsert l o | S.null inserted = S.singleton o
              | otherwise       = inserted
  where inserted = mapMaybeSet tryMerge l
        tryMerge b | M.size diff == 1 = Just (M.difference o diff)
                   | otherwise        = Nothing
                   where diff = symDiff const o b

minOnce :: S.Set (M.Map Char Bool) -> S.Set (M.Map Char Bool)
minOnce = S.foldr S.union S.empty . (S.map =<< tryInsert)

converge :: Eq a => (a -> a) -> a -> a
converge f x | x == y    = y
             | otherwise = converge f y
             where y = f x
             
uncons :: [a] -> Maybe (a,[a])
uncons []     = Nothing
uncons (x:xs) = Just (x,xs)

symDiff :: Eq b => Ord a => (b -> b -> b) -> (M.Map a b) -> (M.Map a b) -> (M.Map a b)
symDiff f = M.mergeWithKey nilIfSame id id
  where nilIfSame _ x y | x == y    = Nothing
                        | otherwise = Just (f x y)

primeImpl :: Expr -> S.Set (M.Map Char Bool)
primeImpl = converge minOnce . minTermsTrue
            
toExpr :: (Char,Bool) -> Expr
toExpr (c,True ) = Var c
toExpr (c,False) = NOT (Var c)

toAnd :: (M.Map Char Bool) -> Expr
toAnd = fromMaybe (Const True)     . 
        fmap (uncurry (foldr AND)) . 
        uncons . map toExpr . M.toList

toOr :: S.Set (M.Map Char Bool) -> Expr
toOr = fromMaybe (Const False)     . 
       fmap (uncurry $ S.foldr OR) . 
       S.minView . S.map toAnd

simplified :: Expr -> Expr
simplified = toOr . primeImpl


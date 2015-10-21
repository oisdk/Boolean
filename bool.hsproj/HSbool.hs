import Control.Applicative hiding (Const)
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe

data Expr = Const Bool
          | Var Char
          | NOT Expr
          | AND Expr Expr
          | OR  Expr Expr deriving (Eq, Show, Ord)
          
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

subInAll :: Expr -> [(Char, Expr)] -> Expr
subInAll = foldr subIn

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

data TermCond = T | F | N deriving (Eq, Show, Ord)

fromBool :: Bool -> TermCond
fromBool True  = T
fromBool False = F

mapMaybeSet :: Ord b => (a -> Maybe b) -> S.Set a -> S.Set b
mapMaybeSet f = (S.map fromJust) . (S.filter isJust) . (S.map f)

tryInsert :: S.Set (M.Map Char TermCond) -> M.Map Char TermCond -> S.Set (M.Map Char TermCond)
tryInsert l o | S.null inserted = S.singleton o
              | otherwise = inserted
  where inserted = mapMaybeSet ifTry l
        ifTry b | M.size diff == 1 = Just (M.union diff o)
                | otherwise        = Nothing
                where diff = M.differenceWith (\x y -> if x == y then Nothing else Just N) o b

minOnce :: S.Set (M.Map Char TermCond) -> S.Set (M.Map Char TermCond)
minOnce l = S.foldr S.union S.empty (S.map (tryInsert l) l)

converge :: Eq a => (a -> a) -> a -> a
converge f x | x == y    = y
             | otherwise = converge f y
             where y = f x

mapMaybeMap :: (a -> Maybe b) -> M.Map c a -> M.Map c b
mapMaybeMap f = (M.map fromJust) . (M.filter isJust) . (M.map f)

toBool :: TermCond -> Maybe Bool
toBool T = Just True
toBool F = Just False
toBool N = Nothing

primeImpl :: Expr -> S.Set (M.Map Char Bool)
primeImpl = S.map (mapMaybeMap toBool) . 
            converge minOnce           . 
            S.map (M.map fromBool)     . 
            minTermsTrue
            
toExpr :: (Char,Bool) -> Expr
toExpr (c,True ) = Var c
toExpr (c,False) = NOT (Var c)

toAnd :: (M.Map Char Bool) -> Expr
toAnd m | M.null m  = Const True
        | otherwise = (foldr1 AND . map toExpr . M.toList) m

toOr :: S.Set (M.Map Char Bool) -> Expr
toOr s | S.null s = Const False
       | otherwise = S.foldr OR h t
       where (h,t) = S.deleteFindMin (S.map toAnd s)

simplified :: Expr -> Expr
simplified = converge (toOr . primeImpl)


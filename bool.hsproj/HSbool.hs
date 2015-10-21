import Control.Applicative hiding (Const)
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Maybe as O
import Data.Monoid
import Prelude hiding (until)

data Expr = Const Bool
          | Var Char
          | NOT Expr
          | AND Expr Expr
          | OR  Expr Expr
          
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

powerSet :: Ord a => (S.Set a) -> [M.Map a Bool]
powerSet = S.foldr f [M.empty] where 
  f e a = with True ++ with False where 
    with = flip (map . M.insert e) a
  
subInSet :: Expr -> M.Map Char Bool -> Bool
subInSet (Const b) _ = b
subInSet (Var   c) s = O.fromJust (M.lookup c s)
subInSet (NOT   e) s = not (subInSet e s)
subInSet (AND l r) s = subInSet l s && subInSet r s
subInSet (OR  l r) s = subInSet l s || subInSet r s

minTerms :: Expr -> [M.Map Char Bool]
minTerms = powerSet . allVars

minTermsTrue :: Expr -> [M.Map Char Bool]
minTermsTrue e = filter (subInSet e) (minTerms e)

data TermCond = T | F | N deriving (Eq, Show)

fromBool :: Bool -> TermCond
fromBool True  = T
fromBool False = F

tryInsert :: [M.Map Char TermCond] -> M.Map Char TermCond -> [M.Map Char TermCond]
tryInsert l o = case O.mapMaybe ifTry l of [] -> [o]
                                           r  -> r
  where ifTry b | M.size diff == 1 = Just (M.union diff o)
                | otherwise        = Nothing
                where diff = M.differenceWith (\x y -> if x == y then Nothing else Just N) o b

minOnce :: [M.Map Char TermCond] -> [M.Map Char TermCond]
minOnce l = l >>= tryInsert l

until :: (a -> Maybe a) -> a -> a
until f x = O.fromMaybe x (until f <$> f x)

converge :: Eq a => (a -> a) -> a -> a
converge f x | x == y    = y
             | otherwise = converge f y
             where y = f x

mapMaybe :: (a -> Maybe b) -> M.Map c a -> M.Map c b
mapMaybe f = (M.map O.fromJust) . (M.filter O.isJust) . (M.map f)

toBool :: TermCond -> Maybe Bool
toBool T = Just True
toBool F = Just False
toBool N = Nothing

primeImpl :: Expr -> [M.Map Char Bool]
primeImpl = map (mapMaybe toBool) . 
            converge minOnce      . 
            map (M.map fromBool)  . 
            minTermsTrue

toExpr :: (Char,Bool) -> Expr
toExpr (c,True ) = Var c
toExpr (c,False) = NOT (Var c)

toAnd :: (M.Map Char Bool) -> Expr
toAnd = foldr1 AND . map toExpr . M.toList

toOr :: [(M.Map Char Bool)] -> Expr
toOr = foldr1 OR . map toAnd

simplified :: Expr -> Expr
simplified = toOr . primeImpl


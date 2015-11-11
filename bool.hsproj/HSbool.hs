import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Applicative hiding (Const)
import Data.Foldable
import Prelude hiding (foldr, all)
import Data.Monoid

data Expr = Const Bool
          | Var Char
          | NOT Expr
          | AND [Expr]
          | OR  Expr Expr deriving (Ord)
          
prec :: Expr -> Int
prec (Const _) = 8
prec (Var   _) = 8
prec (NOT   _) = 8
prec (AND   _) = 5
prec (OR  _ _) = 3

newtype SumExpr = SumExpr { getSumE :: Expr }

instance Monoid SumExpr where
  mempty = SumExpr (Const False)
  mappend (SumExpr a) (SumExpr b) = SumExpr $ simplified $ OR a b

newtype ProdExpr = ProdExpr { getProdE :: Expr }

instance Monoid ProdExpr where
  mempty = ProdExpr (Const True)
  mappend (ProdExpr (AND a)) (ProdExpr (AND b)) = ProdExpr $ simplified $ AND (a ++ b)
  mappend (ProdExpr a) (ProdExpr (AND b)) = ProdExpr $ simplified $ AND (a:b)
  mappend (ProdExpr (AND a)) (ProdExpr b) = ProdExpr $ simplified $ AND (b:a)
  mappend (ProdExpr a) (ProdExpr b) = ProdExpr $ simplified $ AND [a,b]

condBrack :: Int -> Expr -> String
condBrack n e | prec e < n = "(" ++ show e ++ ")"
              | otherwise  = show e

instance Show Expr where
  show (Const b) = if b then "1" else "0"
  show (Var   c) = [c]
  show (NOT   e) = '!' : condBrack 8 e
  show (AND   e) = e >>= condBrack 5
  show (OR  l r) = condBrack 3 l ++ " + "    ++ condBrack 3 r
  
instance Eq Expr where
  a == b = (primeImpl a) == (primeImpl b)
          
allVars :: Expr -> S.Set Char
allVars (Const _) = S.empty
allVars (Var   c) = S.singleton c
allVars (NOT   e) = allVars e
allVars (AND   e) = S.unions (allVars <$> e)
allVars (OR  l r) = S.union (allVars l) (allVars r)

subIn :: (Char, Expr) -> Expr -> Expr
subIn _ (Const c) = Const c
subIn (c,e) (Var   l) | c == l    = e
                      | otherwise = Var l
subIn p (NOT   x) = NOT (subIn p x)
subIn p (AND   e) = AND (subIn p <$> e)
subIn p (OR  a b) = OR  (subIn p a) (subIn p b)

powerSet :: Ord a => (S.Set a) -> S.Set (M.Map a Bool)
powerSet = S.foldr f (S.singleton M.empty) where 
  f e a = S.union (with True) (with False) where 
    with = flip (S.map . M.insert e) a
  
subInSet :: M.Map Char Bool -> Expr -> Bool
subInSet _ (Const b) = b
subInSet s (Var   c) = fromJust (M.lookup c s)
subInSet s (NOT   e) = not (subInSet s e)
subInSet s (AND   e) = all (subInSet s) e
subInSet s (OR  l r) = subInSet s l || subInSet s r

minTerms :: Expr -> S.Set (M.Map Char Bool)
minTerms = powerSet . allVars

minTermsTrue :: Expr -> S.Set (M.Map Char Bool)
minTermsTrue e = S.filter (flip subInSet e) (minTerms e)

mapMaybeSet :: Ord b => (a -> Maybe b) -> S.Set a -> S.Set b
mapMaybeSet f = S.foldr ff S.empty
  where ff e a = fromMaybe a (flip S.insert a <$> f e)


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
toAnd = AND . map toExpr . M.toList

toOr :: S.Set (M.Map Char Bool) -> Expr
toOr = fromMaybe (Const False)     . 
       fmap (uncurry $ S.foldr OR) . 
       S.minView . S.map toAnd

simplified :: Expr -> Expr
simplified = toOr . primeImpl

xor :: Expr -> Expr -> Expr
xor a b = AND [(a `OR` b) NOT (a `AND` b)




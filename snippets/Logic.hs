{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Logic
    ( Logic (..)
    , sample
    , takeLogic )
    where

import Control.Applicative
import Control.Monad hiding (fail)
import Data.List (inits)
import Data.Maybe (catMaybes)
    
newtype Logic a = Logic { samples :: [a] }
  deriving (Functor, Foldable, Semigroup, Monoid)

instance Show a => Show (Logic a) where
    show (Logic xs) = case splitAt 5 xs of
                        (h,[]) -> "Logic " <> show h
                        (h,_) -> "Logic " <> init (show h) <> "...]"

instance Applicative Logic where
  pure x = Logic [x]
  Logic fs <*> Logic xs = Logic
                          $ foldMap catMaybes
                          $ zipWith zipper (inits a) (inits b)
    where a = (Just <$> fs) ++ (Nothing <$ xs)
          b = (Just <$> xs) ++ (Nothing <$ fs)
          zipper xs ys = zipWith (liftA2 ($)) xs (reverse ys)

instance Alternative Logic where
  empty = Logic []
  Logic as <|> Logic bs = Logic $ interleave as bs

interleave m1 m2 =
    msplit m1 >>= maybe m2 (\(a, m1') -> pure a <|> interleave m2 m1')
        where
          msplit []     = pure Nothing
          msplit (x:xs) = pure $ Just (x, xs)

instance Monad Logic where
    Logic [] >>= f = Logic []
    Logic (x:xs) >>= f = f x <|> (Logic xs >>= f)

instance Traversable Logic where
    {-# INLINE traverse #-} 
    traverse f xs = foldr cons_f (pure empty) xs
      where cons_f x ys = liftA2 cons (f x) ys

cons x (Logic xs) = Logic (x:xs)
                          
sample (Logic []) = empty
sample (Logic (h:t)) = pure h

takeLogic n = take n . samples

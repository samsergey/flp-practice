{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language StandaloneDeriving #-}

import Control.Applicative hiding (many, some)

data Grammar a where
  Epsilon :: Grammar a
  Fail :: Grammar a
  Term :: a -> Grammar a
  Kleene :: Grammar a -> Grammar a
  Alt :: Grammar a -> Grammar a -> Grammar a
  Chain :: Grammar a -> Grammar a -> Grammar a
  Ap :: Grammar (b -> a) -> Grammar b -> Grammar a
     
deriving instance Functor Grammar

instance Semigroup (Grammar a) where
  (<>) = Chain
    
instance Monoid (Grammar a) where
  mempty = Epsilon

instance Applicative Grammar where
  pure = Term
  (<*>) = Ap

instance Alternative Grammar where
  empty = Fail
  (<|>) = Alt

generate r = case r of
   Epsilon -> pure []
   Fail -> empty
   Term c -> pure [c]
   Kleene x -> generate Epsilon <|> generate (x <> Kleene x)
   Alt r1 r2 -> generate r1 <|> generate r2
   Chain r1 r2 -> (++) <$> generate r1 <*> generate r2
   Ap f r -> (<*>) <$> generate f <*> generate r

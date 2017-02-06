import Data.Monoid
import Control.Arrow ((&&&))

data Resumable a = Frozen { getResumable :: a }
                 | Awaken { getResumable :: a }
                 deriving Show

instance Monoid m => Monoid (Resumable m) where
  mempty = Awaken mempty
  Frozen _ `mappend` Frozen b = Frozen b
  Frozen a `mappend` Awaken b = Frozen (a <> b)
  Awaken a `mappend` Frozen b = Awaken (a <> b)
  Awaken a `mappend` Awaken b = Awaken (a <> b)

wc :: String -> (Int, Int, Int)
wc = getResult . foldMap (countLines &&& countWords &&& countChars)
  where
    countWords = unless (`elem` " \n") (Frozen 1)
    countLines = unless (`elem` "\n") (Frozen 1)
    countChars = const 1
    getResult (a,(b,c)) = ( getSum $ getResumable a
                          , getSum $ getResumable b
                          , getSum c)

unless p m x = if p x then mempty else m

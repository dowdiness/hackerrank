{-# LANGUAGE TupleSections #-}

import           Control.Monad
import           Control.Monad.ST
import qualified Data.ByteString.Char8       as C
import           Data.List
import qualified Data.Set                    as S
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as UM

main = C.interact $ C.pack . show . sol . get

get = V.unfoldr (C.readInt . C.dropWhile (<'0')) . last . C.lines

sol as = V.sum $ runST $ do
  h <- V.unsafeThaw hst
  V.forM_ as $ \a -> do
    x <- UM.unsafeRead h a
    when (x>0) $ do
      when (x>1) $ UM.write h a 0
      mapM_ (\i -> UM.write h i 0) [2*a,3*a..m]
  V.unsafeFreeze h
  where
  m = V.maximum as
  hst = V.accumulate (+) (V.replicate (m+1) 0) $ V.map (,1) as :: V.Vector Int

unDiv :: [Integer] -> Int
unDiv xs
  | null cnt   = 0
  | otherwise  = length $ S.difference (S.fromList xs) cnt
  where
    uniq = S.toList $ S.fromList xs
    ma    = maximum xs
    se  = foldl' union [] $ map (\x -> [x, x*2..ma]) xs
    cnt   = S.fromList $ [cnt | el <- uniq, cnt <-[el*2,el*3..ma]]

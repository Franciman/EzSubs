module SubtitleFuncs where

import Text.Subtitles.SRT
import Control.Monad
import Data.Text

newtype SubtitleFunc a = SubtitleFunc { run :: Subtitles -> a }

instance Functor SubtitleFunc where
  fmap g (SubtitleFunc f) = SubtitleFunc (g . f)

instance Applicative SubtitleFunc where
  pure a = SubtitleFunc (const a)
  SubtitleFunc transformer <*> SubtitleFunc f = SubtitleFuncFunc (\subs -> transformer subs (f subs))

instance Monad SubtitleFunc where
  sm >>= g = SubtitleFunc (\subs -> runIt ( g (runIt sm subs) ) subs)

noSubs :: SubtitleFunc a -> a
noSubs sf = run sf []

withSubs :: Subtitles -> SubtitleFunc a -> a
withSubs sf subs = run sf subs



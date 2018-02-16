
module Editor.Buffer
  ( Buffer
  , empty, load
  , moveUp, moveDown, moveToTop, moveToBottom
  , window
  , insert
  ) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (uncons)
import qualified Data.Sequence as S
import           Data.Sequence
                  ( Seq
                  , pattern Empty, pattern (:<|), pattern (:|>)
                  , (<|), (|>)
                  )
import           Data.Monoid ((<>))
import           Data.Foldable (toList)

data Buffer
  = Buffer
    { linesAbove :: Seq Text
    , linesRest  :: Seq Text
    }
    -- all in forward order
    -- invariant: empty linesRest => empty linesAbove

fixup :: Buffer -> Buffer
fixup (Buffer (a :|> l) Empty) = Buffer a (S.singleton l)
fixup b                        = b

empty :: Buffer
empty = Buffer S.empty S.empty

fromLines :: [Text] -> Buffer
fromLines = Buffer S.empty . S.fromList

load :: Text -> Buffer
load = fromLines . T.lines

moveAbs, moveRel :: Int -> Buffer -> Buffer
moveAbs i (Buffer a r) = fixup $ Buffer a' r' where
  (a', r') = S.splitAt i (a <> r)
moveRel d b@(Buffer a _) = moveAbs (S.length a + d) b

moveUp, moveDown :: Int -> Buffer -> Buffer
moveUp   = moveRel . negate
moveDown = moveRel

moveToTop, moveToBottom :: Buffer -> Buffer
moveToTop    = moveAbs 0
moveToBottom = moveAbs maxBound

blanks :: [Text]
blanks = repeat "~"

window :: Buffer -> Int -> Int -> [Text]
window (Buffer a r) off n = take n' $ toList (above <> r) <> blanks where
  above = S.drop (S.length a - off') a
  n'   = max 1 n
  off' = min (n'-1) (max 0 off)

insert :: Char -> Buffer -> Buffer
insert c (Buffer a (l :<| r)) = Buffer a ((l `T.snoc` c) <| r)


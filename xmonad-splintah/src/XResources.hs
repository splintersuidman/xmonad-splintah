module XResources
  ( getXResources
  , getXResources'
  ) where

import           Control.Category          ((>>>))
import           Data.Bifunctor            (second)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Graphics.X11.Xlib.Display (openDisplay, resourceManagerString)
import           Graphics.X11.Xlib.Types   (Display)

-- | Open a 'Display' and get its X resources.
--
-- This function opens a new 'Display', and doesn't use the 'Display' in the X
-- monad, since X resources stay constant for that 'Display'. To get the most
-- recent X resources, it is therefore necessary to open a new 'Display'.
--
-- To get the X resources using the 'Display' in the X monad, use @withDisplay
-- getXResources'@.
getXResources :: IO (Map String String)
getXResources = getXResources' <$> openDisplay ""

-- | Get and parse the X resources for a given 'Display'.
getXResources' :: Display -> Map String String
getXResources' =
      resourceManagerString
  >>> lines
  >>> fmap (break (== ':') >>> second (dropWhile $ \c -> c == ':' || c == '\t'))
  >>> Map.fromList

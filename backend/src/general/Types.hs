module General.Types where

import Control.Lens (Lens')

class HasNetwork t where
  network :: Lens' t Network

data Network = TestNet3 | MainNet
  deriving (Show, Eq)



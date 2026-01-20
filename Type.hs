module Type where

import Data.List (nub)

-- | Types
data Type
  = TVar String
  | TFun Type Type
  deriving (Eq, Show)

-- | Polymorphic type schemes
data Scheme = Forall [String] Type
  deriving (Show)

-- | Type environment
import qualified Data.Map as Map
type TypeEnv = Map.Map String Scheme

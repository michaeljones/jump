module Jump.Data
    (
    Location(..),
    Name,
    Directory,
    Tags
    ) where

import qualified Data.Map.Strict as M

type Name = String
type Directory = String
type Tags = M.Map String String

data Location = Location { getName :: Name, getDirectory :: Directory, getTags :: Maybe Tags } deriving Show


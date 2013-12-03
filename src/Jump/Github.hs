module Jump.Github 
    (
    githubLabel
    ) where

import qualified Data.Map.Strict as M

import           Jump.Data ( Tags )

githubLabel :: Maybe Tags -> [String]
githubLabel Nothing  = []
githubLabel (Just m) =
    case M.lookup "github" m of
        (Just _) -> ["[gh]"]
        Nothing  -> []


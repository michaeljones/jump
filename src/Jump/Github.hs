module Jump.Github 
    (
    githubLabel,
    githubInfo
    ) where

import qualified Data.Map.Strict as M

import           Jump.Data ( Name, Tags )

githubLabel :: Maybe Tags -> [String]
githubLabel Nothing  = []
githubLabel (Just m) =
    case M.lookup "github" m of
        (Just _) -> ["[gh]"]
        Nothing  -> []

githubInfo :: Name -> Maybe Tags -> String
githubInfo _ Nothing     = ""
githubInfo _ (Just tags) =
    case M.lookup "github" tags of
        Nothing  -> ""
        (Just d) -> "Github: " ++ d ++ "\n"


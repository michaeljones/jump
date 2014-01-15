module Jump.Github 
    (
    githubLabel,
    githubInfo
    ) where

import qualified Data.Map.Strict as M
import           Data.Maybe ( fromMaybe )

import           Jump.Data ( Name, Tags )
import           Jump.Ui.Details ( DetailsData(..), DetailsDataMap )

{- Create the label for entries in the list which have github metadata
   TODO: This should probably be -> Maybe String -}
githubLabel :: Maybe Tags -> [String]
githubLabel Nothing  = []
githubLabel (Just m) =
    case M.lookup "github" m of
        (Just _) -> ["[gh]"]
        Nothing  -> []

{- Return information about the github metadata for the details panel for this
   list entry -}
githubInfo :: Name -> Maybe Tags -> Maybe DetailsDataMap -> String
githubInfo _ tags datamap =
    case processInfo tags datamap of
        Nothing  -> ""
        (Just d) -> "Github:\n" ++ d ++ "\n"

{- Check for data in the Tags and DetailsDataMap and format into a description
   if anything is found -}
processInfo :: Maybe Tags -> Maybe DetailsDataMap -> Maybe String
processInfo tags datamap =
    let datamap' = fromMaybe M.empty datamap
        value = M.findWithDefault NoData "github" datamap'
    in
        tags >>= M.lookup "github" >>=
        return . processTag >>= processDataMap value

processTag :: String -> String
processTag project = "  project: " ++ project ++ "\n"

processDataMap :: DetailsData -> String -> Maybe String
processDataMap (Github pr i) = return . formatInfo (show pr) (show i)
processDataMap NoData = return . formatInfo "?" "?"

formatInfo :: String -> String -> String -> String
formatInfo pullRequests issues info = info ++ "  pull requests: " ++ pullRequests ++ "\n  issues: " ++ issues


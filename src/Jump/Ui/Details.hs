module Jump.Ui.Details
    (
    updateDetails,
    DetailsMap,
    DetailsData(..),
    DetailsDataMap,
    titleInfo
    ) where

import           Graphics.Vty.Widgets.All ( Widget, Group, FormattedText, SelectionEvent(..), plainText, addToGroup )

import qualified Data.Map.Strict as M
import qualified Data.IORef as R
import qualified Data.Text as T

import           Jump.Data ( Location(..), Name, Tags )
import           Jump.Ui.Data ( ListVisual )

type InfoFunc = Name -> Maybe Tags -> Maybe DetailsDataMap -> String
type DetailGenerator = (String, InfoFunc)

--
type PullRequests = Int
type Issues = Int
type DetailsDataMap = M.Map String DetailsData
data DetailsData = Github PullRequests Issues
                 | NoData

-- 
type SwitchAction = IO ()
type DetailsMap = M.Map String (DetailsDataMap, SwitchAction)

{- Grab the Location from the Selection event, tries to find the corresponding
   switcher. If it isn't there then it creates it and then calls it to switch
   to the appropriate page -}
updateDetails :: R.IORef DetailsMap -> [DetailGenerator] -> Widget (Group FormattedText) -> SelectionEvent Location ListVisual -> IO ()
updateDetails dmapref dgen dgroup e = do

    let SelectionOn _ location _ = e
        name = getName location

    dmap <- R.readIORef dmapref
    case M.lookup name dmap of Nothing  -> createDetails dmapref dgen dgroup location
                               (Just _) -> return ()

    setDetails name dmapref

{- Creates the details widget for a particular location and adds it to the
   group, entering the switch function into the DetailsMap -}
createDetails :: R.IORef DetailsMap -> [DetailGenerator] -> Widget (Group FormattedText) -> Location -> IO ()
createDetails dmapref dgen dgroup location = do
    let name = getName location
        tags = getTags location

    dmap <- R.readIORef dmapref
    let ddatamap = fmap fst $ M.lookup name dmap

    widget <- plainText . T.pack . concat $ map (apply name tags ddatamap) dgen
    switchFunc <- addToGroup dgroup widget

    case ddatamap of
        Nothing  -> R.modifyIORef dmapref $ \m -> M.insert name (M.empty :: DetailsDataMap, switchFunc) m
        (Just datamap) -> R.modifyIORef dmapref $ \m -> M.insert name (datamap, switchFunc) m

{- Applies the provided InfoFunc to the rest of the arguments -}
apply :: Name -> Maybe Tags -> Maybe DetailsDataMap -> DetailGenerator -> String
apply name tags ddatamap (type_, f) = f name tags ddatamap

{- -}
setDetails :: String -> R.IORef DetailsMap -> IO ()
setDetails name dmapref = do
    dmap <- R.readIORef dmapref
    case M.lookup name dmap of
        Nothing  -> return ()
        (Just t) -> snd t

{- -}
titleInfo :: Name -> Maybe Tags -> Maybe DetailsDataMap -> String
titleInfo name _ _ = "Details: " ++ name ++ "\n"


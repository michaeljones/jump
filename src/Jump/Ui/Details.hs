module Jump.Ui.Details
    (
    updateDetails,
    DetailsMap
    ) where

import           Graphics.Vty.Widgets.All ( Widget, Group, FormattedText, SelectionEvent(..), plainText, addToGroup )

import qualified Data.Map.Strict as M
import qualified Data.IORef as R
import qualified Data.Text as T

import           Jump.Data ( Location(..), Name, Tags )
import           Jump.Ui.Data ( ListVisual )
import           Jump.Github ( githubInfo )
import           Jump.Venv ( virtualenvInfo )

type SwitchAction = IO ()
type DetailsMap = M.Map String SwitchAction

updateDetails :: R.IORef DetailsMap -> Widget (Group FormattedText) -> SelectionEvent Location ListVisual -> IO ()
updateDetails dmapref dgroup e = do

    let SelectionOn _ location _ = e
        name = getName location

    dmap <- R.readIORef dmapref
    case M.lookup name dmap of Nothing  -> createDetails dmapref dgroup location
                               (Just _) -> return ()

    setDetails name dmapref

createDetails :: R.IORef DetailsMap -> Widget (Group FormattedText) -> Location -> IO ()
createDetails dmapref dgroup location = do
    let name = getName location
        tags = getTags location
        infoFuncs = [titleInfo, virtualenvInfo, githubInfo]

    widget <- plainText . T.pack . concat $ map (\f -> f name tags) infoFuncs
    switchFunc <- addToGroup dgroup widget

    R.modifyIORef dmapref $ \m -> M.insert name switchFunc m

setDetails :: String -> R.IORef DetailsMap -> IO ()
setDetails name dmapref = do
    dmap <- R.readIORef dmapref
    case M.lookup name dmap of
        Nothing  -> return ()
        (Just f) -> f

titleInfo :: Name -> Maybe Tags -> String
titleInfo name _ = "Details: " ++ name ++ "\n"



import           Graphics.Vty.Widgets.All

import           Graphics.Vty.Attributes ( def_attr, bold, cyan )
import           Graphics.Vty.LLInput    ( Key( KASCII) )

import           System.Environment ( lookupEnv )
import           System.IO()
import           System.Exit ( exitSuccess )
import           System.Locale ( defaultTimeLocale )

import           Data.Maybe ( isJust )

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V()
import qualified Data.IORef as R
import           Data.Time ( getCurrentTime, getCurrentTimeZone, utcToLocalTime, formatTime )

import           Jump.Data ( Location(..), Name, Tags )
import           Jump.Config ( withConfig )
import           Jump.Venv ( newVirtualenvAction, lastVirtualEnvAction, virtualEnvLabel )
import           Jump.Github ( githubLabel )

type ListVisual = Box FormattedText FormattedText
type SwitchAction = IO ()
type DetailsMap = M.Map String SwitchAction

main :: IO ()
main = withConfig "JUMP_CONFIG" createUI

createUI :: [Location] -> IO ()
createUI locations = do

   let listLength = length locations
       entryWidth = maximum $ map (length . getName) locations
       padding = 4
       borderThickness = 2
       borderedWidth = entryWidth + borderThickness + padding
       borderedHeight = listLength * 2 -- + borderThickness
       middle = truncate $ ( toRational listLength ) / 2.0

   -- Create new list
   directoryList <- newList def_attr

   -- Populate list options
   mapM_ (addPairsToList directoryList) locations

   -- Add a border and put it + border in a fixed sized centered box
   fixedSizeDirectoryList <- boxFixed borderedWidth borderedHeight directoryList

   time <- getCurrentTime
   timezone <- getCurrentTimeZone
   let localTime = utcToLocalTime timezone time
       formattedTime = formatTime (defaultTimeLocale) "%H:%M:%S" localTime
       formattedDay = formatTime (defaultTimeLocale) "%A, %d %B %Y" localTime

   dateWidget <- (hFill ' ' 1) <++> (plainText $ T.pack formattedDay)
   timeWidget <- (hFill ' ' 1) <++> (plainText $ T.pack formattedTime)
   dateTimeBox <- (vBox dateWidget timeWidget) <--> (hFill ' ' 2)
   detailsGroup <- newGroup
   _ <- addToGroup detailsGroup =<< (plainText $ T.pack "Details:")
   infoBox <- vBox dateTimeBox detailsGroup
   infoPanel <- boxFixed 30 borderedHeight infoBox
   mainPanel <- hBox fixedSizeDirectoryList infoPanel
   borderedMainPanel <- bordered mainPanel

   ui <- centered borderedMainPanel

   fg <- newFocusGroup
   _ <- addToFocusGroup fg directoryList

   c <- newCollection
   _ <- addToCollection c ui fg

   -- Focus group event handlers
   fg `onKeyPressed` exit

   -- List event handlers
   detailsMap <- R.newIORef ( M.empty :: DetailsMap )
   directoryList `onItemActivated` handleSelection
   directoryList `onKeyPressed` navigate
   directoryList `onSelectionChange` updateDetails detailsMap detailsGroup

   -- We want to highlight the middle item of the list to begin with but
   -- we can't do that untill everything has been set up so we schedule
   -- the task so that it happens in the event loop when everything is up
   -- and running
   schedule $ scrollBy directoryList middle

   runUi c defaultContext

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

titleInfo :: Name -> Maybe Tags -> String
titleInfo name _ = "Details: " ++ name ++ "\n"

virtualenvInfo :: Name -> Maybe Tags -> String
virtualenvInfo _ Nothing     = ""
virtualenvInfo _ (Just tags) =
    case M.lookup "virtualenv" tags of
        Nothing  -> ""
        (Just d) -> "Virtualenv: " ++ d ++ "\n"

githubInfo :: Name -> Maybe Tags -> String
githubInfo _ Nothing     = ""
githubInfo _ (Just tags) =
    case M.lookup "github" tags of
        Nothing  -> ""
        (Just d) -> "Github: " ++ d ++ "\n"

setDetails :: String -> R.IORef DetailsMap -> IO ()
setDetails name dmapref = do
    dmap <- R.readIORef dmapref
    case M.lookup name dmap of
        Nothing  -> return ()
        (Just f) -> f


-- Callback for exiting via 'q'
exit :: a -> Key -> b -> IO Bool
exit _ key _ | key == KASCII 'q' = cleanup
             | otherwise         = return False

cleanup :: IO Bool
cleanup = do
    -- Clear the command file
    writeFile "/tmp/jump-hs.sh" []
    shutdownUi
    exitSuccess

-- Callback for list nagivation
navigate :: Widget (List a b) -> Key -> c -> IO Bool
navigate list key _ | key == KASCII 'j' = handle $ scrollDown list
                    | key == KASCII 'k' = handle $ scrollUp list
                    | otherwise         = return False
    where
        handle x = do { _ <- x; return True }

-- Callback for list item selection
handleSelection :: ActivateItemEvent Location b -> IO ()
handleSelection event = do
    let ActivateItemEvent _ a _ = event
        directory = getDirectory a
        tags = getTags a

    iolist <- R.newIORef ["cd " ++ directory ++ ";\n"]

    currentVirtualEnv <- lookupEnv "VIRTUAL_ENV"
    R.modifyIORef iolist $ lastVirtualEnvAction (isJust currentVirtualEnv)
    R.modifyIORef iolist $ newVirtualenvAction tags

    output <- R.readIORef iolist
    writeFile "/tmp/jump-hs.sh" $ concat output
    shutdownUi


-- Add processed yaml data to the list
addPairsToList :: Widget (List Location ListVisual) -> Location -> IO ()
addPairsToList list location@(Location name _ tags) = do
    directoryWidget <- plainText $ T.pack name
    setNormalAttribute directoryWidget (style bold)

    tagsWidget <- plainText . T.pack $ buildTagEntry tags
    setNormalAttribute tagsWidget (fgColor cyan)

    listEntry <- vBox directoryWidget tagsWidget
    addToList list location listEntry

labelFuncs :: [Maybe Tags -> [String]]
labelFuncs = [ virtualEnvLabel, githubLabel ]

buildTagEntry :: Maybe Tags -> String
buildTagEntry tags = formatTags . concat $ map applyToTags labelFuncs
  where
    applyToTags f = f tags
    formatTags []      = " "
    formatTags content = "  " ++ ( L.intercalate " " content )


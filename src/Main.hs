
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

import           Jump.Data ( Location(..), Directory, Tags )
import           Jump.Config ( withConfig )

main :: IO ()
main = withConfig "JUMP_CONFIG" createUI

createUI :: Maybe [Location] -> IO ()
createUI Nothing          = return ()
createUI (Just locations) = do

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
   infoBox <- vBox dateWidget timeWidget
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
   directoryList `onItemActivated` handleSelection
   directoryList `onKeyPressed` navigate

   -- We want to highlight the middle item of the list to begin with but
   -- we can't do that untill everything has been set up so we schedule
   -- the task so that it happens in the event loop when everything is up
   -- and running
   schedule $ scrollBy directoryList middle

   runUi c defaultContext

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
handleSelection :: ActivateItemEvent (Directory, Maybe Tags) b -> IO ()
handleSelection event = do
    let ActivateItemEvent _ a _ = event
        directory = fst a
        tags = snd a

    iolist <- R.newIORef ["cd " ++ directory ++ ";\n"]

    currentVirtualEnv <- lookupEnv "VIRTUAL_ENV"
    R.modifyIORef iolist $ lastVirtualEnvAction (isJust currentVirtualEnv)
    R.modifyIORef iolist $ newVirtualenvAction tags

    output <- R.readIORef iolist
    writeFile "/tmp/jump-hs.sh" $ concat output
    shutdownUi


-- If there is a "virtualenv" tag then return a function that appends the
-- correct commands to the file to be written
newVirtualenvAction :: Maybe Tags -> ([String] -> [String])
newVirtualenvAction (Just a) = case M.lookup "virtualenv" a of
    (Just value) -> let activate = "source " ++ value ++ "/bin/activate;\n"
                    in  (++ [activate])
    Nothing      -> id
newVirtualenvAction Nothing  = id

-- Returns action to perform if we're in a virtualenv already
lastVirtualEnvAction :: Bool -> ([String] -> [String])
lastVirtualEnvAction True  = (++ ["deactivate;\n"])
lastVirtualEnvAction False = id

type ListVisual = Box FormattedText FormattedText
type ListData   = (Directory, Maybe Tags)

-- Add processed yaml data to the list
addPairsToList :: Widget (List ListData ListVisual) -> Location -> IO ()
addPairsToList list (Location name dir tags) = do
    directoryWidget <- plainText $ T.pack name
    setNormalAttribute directoryWidget (style bold)

    tagsWidget <- plainText . T.pack $ buildTagEntry tags
    setNormalAttribute tagsWidget (fgColor cyan)

    listEntry <- vBox directoryWidget tagsWidget
    addToList list (dir, tags) listEntry

buildTagEntry :: Maybe Tags -> String
buildTagEntry tags = formatTags $ ( virtualEnvLabel tags ++ githubLabel tags )
  where
    formatTags []      = " "
    formatTags content = "  " ++ ( L.intercalate " " content )

virtualEnvLabel :: Maybe Tags -> [String]
virtualEnvLabel Nothing  = []
virtualEnvLabel (Just m) =
    case M.lookup "virtualenv" m of
        (Just _) -> ["[ve]"]
        Nothing  -> []

githubLabel :: Maybe Tags -> [String]
githubLabel Nothing  = []
githubLabel (Just m) =
    case M.lookup "github" m of
        (Just _) -> ["[gh]"]
        Nothing  -> []


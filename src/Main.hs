
import           Graphics.Vty.Widgets.All

import           Graphics.Vty.Attributes ( def_attr )
import           Graphics.Vty.LLInput    ( Key( KASCII) )

import           System.Environment ( lookupEnv )
import           System.IO()
import           System.Exit ( exitSuccess )

import           Control.Monad
import           Control.Applicative

import           Data.Maybe ( isJust, fromJust )

import qualified Data.Map.Strict as M
import qualified Data.List as L()
import qualified Data.Text as T
import qualified Data.Yaml as Y
import           Data.Yaml ( (.:), (.:?) )
import qualified Data.Vector as V()
import qualified Data.IORef as R

main :: IO ()
main = do

    jumprc <- lookupEnv "JUMP_CONFIG"

    results <- Y.decodeFile ( fromJust jumprc ) :: IO ( Maybe [Location] )

    when (isJust results) $ do

        let pairs = case results of (Just locations) -> locations
                                    Nothing -> []
            listLength = length pairs
            entryWidth = maximum $ map (length . getName) pairs
            padding = 4
            borderThickness = 2
            borderedWidth = entryWidth + borderThickness + padding
            borderedHeight = listLength + borderThickness
            middle = truncate $ ( toRational listLength ) / 2.0

        -- Create new list
        list <- newList def_attr

        -- Populate list options
        mapM_ (addPairsToList list) pairs

        -- Add a border and put it + border in a fixed sized centered box
        border <- bordered list
        fixed <- boxFixed borderedWidth borderedHeight border
        ui <- centered fixed

        fg <- newFocusGroup
        _ <- addToFocusGroup fg list

        c <- newCollection
        _ <- addToCollection c ui fg

        -- Focus group event handlers
        fg `onKeyPressed` exit

        -- List event handlers
        list `onItemActivated` handleSelection
        list `onKeyPressed` navigate

        -- We want to highlight the middle item of the list to begin with but
        -- we can't do that untill everything has been set up so we schedule
        -- the task so that it happens in the event loop when everything is up
        -- and running
        schedule $ scrollBy list middle

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

-- Add processed yaml data to the list
addPairsToList :: Widget (List (Directory, Maybe Tags) FormattedText) -> Location -> IO ()
addPairsToList list (Location name dir tags) = addToList list (dir, tags) =<< ( plainText $ T.pack name )

type Name = String
type Directory = String
type Tags = M.Map String String

data Location = Location { getName :: Name, _getDirectory :: Directory, _getTags :: Maybe Tags } deriving Show

-- Specify FromJSON instance for Location to tie into Yaml deserialisation
instance Y.FromJSON Location where
   parseJSON (Y.Object v) = Location <$>
                            v .:  T.pack "name" <*>
                            v .:  T.pack "directory" <*>
                            v .:? T.pack "tags"

   parseJSON _            = mzero


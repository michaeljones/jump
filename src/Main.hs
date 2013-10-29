
import           Graphics.Vty.Widgets.All

import           Graphics.Vty.Attributes ( def_attr )
import           Graphics.Vty.LLInput    ( Key( KASCII) )

import           System.IO   ( writeFile )
import           System.Exit ( exitSuccess )

import           Control.Monad
import           Control.Applicative

import           Data.Maybe ( isJust )

import qualified Data.Text as T
import qualified Data.Yaml as Y
import           Data.Yaml ( (.:) )
import qualified Data.Vector as V

main :: IO ()
main = do

    results <- Y.decodeFile "jumprc" :: IO ( Maybe [Location] )

    when (isJust results) $ do

        let pairs = case results of (Just locations) -> locations
            listLength = length pairs
            entryWidth = maximum $ map (length . getName) pairs
            padding = 4
            border = 2
            borderedWidth = entryWidth + border + padding
            borderedHeight = listLength + border

        -- Create new list
        list <- newList def_attr

        -- Populate list options
        mapM_ (addPairsToList list) pairs

        -- Add a border and put it + border in a fixed sized centered box
        border <- bordered list
        fixed <- boxFixed borderedWidth borderedHeight border
        ui <- centered fixed

        fg <- newFocusGroup
        addToFocusGroup fg list

        c <- newCollection
        _ <- addToCollection c ui fg

        -- Focus group event handlers
        fg `onKeyPressed` exit

        -- List event handlers
        list `onItemActivated` writeResult
        list `onKeyPressed` navigate

        runUi c defaultContext

-- Callback for exiting via 'q'
exit _ key _ | key == KASCII 'q' = do { shutdownUi; exitSuccess }
             | otherwise         = return False

-- Callback for list nagivation
navigate list key _ | key == KASCII 'j' = handle $ scrollDown list
                    | key == KASCII 'k' = handle $ scrollUp list
                    | otherwise         = return False
    where 
        handle x = do { x; return True }

-- Callback for list item selection
writeResult :: ActivateItemEvent String b -> IO ()
writeResult event = do
    let ActivateItemEvent _ a _ = event

    writeFile "/tmp/jump-hs.sh" a
    shutdownUi

-- Add processed yaml data to the list
addPairsToList :: Widget (List Directory FormattedText) -> Location -> IO ()
addPairsToList list (Location name dir) = addToList list dir =<< ( plainText $ T.pack name )

type Name = String
type Directory = String

data Location = Location { getName :: Name, getDirectory :: Directory } deriving Show

-- Specify FromJSON instance for Location to tie into Yaml deserialisation
instance Y.FromJSON Location where
   parseJSON (Y.Object v) = Location <$>
                            v .: T.pack "name" <*>
                            v .: T.pack "directory"

   parseJSON _            = mzero


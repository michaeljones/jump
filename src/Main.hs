
import           Graphics.Vty.Widgets.All

import           Graphics.Vty.Attributes ( def_attr )
import           Graphics.Vty.LLInput    ( Key( KASCII) )

import           System.IO   ( writeFile )
import           System.Exit ( exitSuccess )

import           Control.Monad ( when )

import           Data.Maybe ( isJust )

import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.Vector as V

main :: IO ()
main = do

    results <- Y.decodeFile "jumprc" :: IO ( Maybe Y.Value )

    when (isJust results) $ do

        -- Create new list
        list <- newList def_attr

        -- -- Populate options
        let pairs = process results
        mapM_ (addPairsToList list) pairs

        border <- bordered list

        fixed <- boxFixed 30 20 border

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
writeResult event =
    let ActivateItemEvent _ a _ = event
    in  do
        writeFile "/tmp/jump-hs.sh" a
        shutdownUi

-- Add processed yaml data to the list
addPairsToList :: Widget (List Directory FormattedText) -> (Name, Directory) -> IO ()
addPairsToList list (name, dir) = addToList list dir =<< ( plainText $ T.pack name )

-- Process the Yaml data
type Name = String
type Directory = String

process :: Maybe Y.Value -> [(Name,Directory)]
process (Just v) = processTop v

processTop :: Y.Value -> [(Name,Directory)]
processTop (Y.Array a) = V.foldl processGroup [] a

processGroup :: [(Name,Directory)] -> Y.Value -> [(Name,Directory)]
processGroup xs (Y.Array a) = foldl processPair xs $ V.toList a

processPair :: [(Name,Directory)] -> Y.Value -> [(Name,Directory)]
processPair xs (Y.Array a) = case V.toList a of
    [Y.String x, Y.String y] -> ((T.unpack x, T.unpack y)):xs




import           Graphics.Vty.Widgets.All

import           Graphics.Vty.Attributes ( def_attr )
import           Graphics.Vty.LLInput    ( Key( KASCII) )

import           System.IO   ( writeFile )
import           System.Exit ( exitSuccess )

import qualified Data.Text as T

main :: IO ()
main = do

    -- Create new list
    lst <- newList def_attr

    -- Populate options
    addToList lst "choice" =<< ( plainText $ T.pack "choice" )
    addToList lst "choice 1" =<< ( plainText $ T.pack "choice 1" )
    addToList lst "choice 2" =<< ( plainText $ T.pack "choice 2" )
    addToList lst "choice 3" =<< ( plainText $ T.pack "choice 3" )
    addToList lst "choice 4" =<< ( plainText $ T.pack "choice 4" )

    ui <- centered lst

    fg <- newFocusGroup
    addToFocusGroup fg lst

    c <- newCollection
    _ <- addToCollection c ui fg

    -- Focus group event handlers
    fg `onKeyPressed` exit

    -- List event handlers
    lst `onItemActivated` writeResult
    lst `onKeyPressed` navigate

    runUi c defaultContext

-- Callback for exiting via 'q'
exit _ key _ | key == KASCII 'q' = do { shutdownUi; exitSuccess }
             | otherwise         = return False

-- Callback for list nagivation
navigate list key _ | key == KASCII 'j' = handle $ scrollDown list
                    | key == KASCII 'k' = handle $ scrollUp list
                    | otherwise         = return False

handle x = do { x; return True }

-- Callback for list item selection
writeResult :: ActivateItemEvent String b -> IO ()
writeResult event =
    let ActivateItemEvent _ a _ = event
    in  do
        writeFile "/tmp/jump-hs.sh" a
        shutdownUi


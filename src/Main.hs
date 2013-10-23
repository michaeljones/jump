
import           Graphics.Vty.Widgets.All
import           Graphics.Vty.Attributes

import           System.IO as S
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

    lst `onItemActivated` writeResult

    runUi c defaultContext


-- Callback for list item selection
writeResult :: ActivateItemEvent String b -> IO ()
writeResult event =
    let ActivateItemEvent _ a _ = event
    in  do
        S.writeFile "/tmp/jump-hs.sh" a
        shutdownUi


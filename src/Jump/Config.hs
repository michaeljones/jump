module Jump.Config
    (
    withConfig
    ) where

import           System.Environment ( lookupEnv )

import qualified Data.Text as T
import qualified Data.Yaml as Y
import           Data.Yaml ( (.:), (.:?) )
import           Data.Maybe ( fromJust )

import           Control.Applicative ((<*>), (<$>))
import           Control.Monad (mzero)

import           Jump.Data ( Location(..) )

-- Specify FromJSON instance for Location to tie into Yaml deserialisation
instance Y.FromJSON Location where
   parseJSON (Y.Object v) = Location <$>
                            v .:  T.pack "name" <*>
                            v .:  T.pack "directory" <*>
                            v .:? T.pack "tags"

   parseJSON _            = mzero


withConfig :: String -> ([Location] -> IO ()) -> IO ()
withConfig envar f = do

    -- Grab the JUMP_CONFIG envar so we know what file to open
    -- Assume it is there
    jumprc <- lookupEnv "JUMP_CONFIG"

    -- Parse the specified file using the yaml API and create the UI
    results <- case jumprc of 
        (Just config) -> Y.decodeFile config :: IO ( Maybe [Location] )
        Nothing       -> return Nothing

    maybe (return ()) f results


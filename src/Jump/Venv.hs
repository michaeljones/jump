module Jump.Venv
    (
    newVirtualenvAction,
    lastVirtualEnvAction,
    virtualEnvLabel,
    virtualenvInfo
    ) where

import qualified Data.Map.Strict as M

import           Jump.Data ( Name, Tags )

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

virtualEnvLabel :: Maybe Tags -> [String]
virtualEnvLabel Nothing  = []
virtualEnvLabel (Just m) =
    case M.lookup "virtualenv" m of
        (Just _) -> ["[ve]"]
        Nothing  -> []

virtualenvInfo :: Name -> Maybe Tags -> String
virtualenvInfo _ Nothing     = ""
virtualenvInfo _ (Just tags) =
    case M.lookup "virtualenv" tags of
        Nothing  -> ""
        (Just d) -> "Virtualenv: " ++ d ++ "\n"


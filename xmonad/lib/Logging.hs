module Logging where

import qualified Codec.Binary.UTF8.String            as UTF8
import           Control.Arrow                       hiding ((<+>), (|||))
import           XMonad
import           XMonad.Actions.WorkspaceNames
import           XMonad.Hooks.DynamicLog
import Colours

import qualified DBus                                as D
import qualified DBus.Client                         as DC

myLogHookPP :: String -> DC.Client -> X PP
myLogHookPP accent dbus = do
	nameMap <- getWorkspaceNames'
	let names = nameFormat nameMap
	return def { ppOutput = dbusOutput dbus
		, ppCurrent = names >>> (addPolybarColour accent) >>> (wrap "[" "]")
		, ppVisible = names >>> (addPolybarColour blue)
		, ppUrgent  = names >>> (addPolybarColour red)
		, ppHidden  = names >>> (wrap "  " "  ")
		, ppWsSep = " "
		, ppSep = "  |   "
		, ppTitle = const ""
		}
		where
		addPolybarColour c = wrap ("%{F" ++ c ++ "} ") " %{F-}"
		nameFormat nameMap x = case nameMap x of
			Nothing -> x
			Just v -> (x ++ " : " ++ v)

-- Emit a DBus signal on log updates
dbusOutput :: DC.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    DC.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

myLogHook :: String -> DC.Client -> X ()
myLogHook accent dbus = myLogHookPP accent dbus >>= dynamicLogWithPP

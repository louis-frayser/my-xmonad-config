module Env
  where
import Data.Maybe
import XMonad
import System.Posix.Env (getEnv)
import System.Directory
{-
-- The purpose here is to extract XMonadConfigDir as a String
-- or variant that is usable in main IO ()
-}


xmonad_home :: IO String
xmonad_home = do
   m_xmh <- getEnv "XMONAD_HOME"
   home <-
      getEnv "HOME" >>= (\s -> return $ fromMaybe (error "No $HOME is set") s)
   let cfg_dir = home ++ "/.config/xmonad"
   let dfl_dir = home ++ "/.xmonad" -- Old/coventional xmonad home
   is_cfg_dir <- doesDirectoryExist cfg_dir
   is_dfl_dir <- doesDirectoryExist dfl_dir
   let out =
          case (m_xmh, is_cfg_dir, is_dfl_dir) of
             (Nothing, True, _) -> cfg_dir
             (Nothing, False, True) -> dfl_dir
             (Just xmh, _, _) -> xmh
   return out

getXMonadHome :: X FilePath
getXMonadHome = asks (cfgDir . directories) -- getXMonadDir

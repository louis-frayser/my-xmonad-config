import System.Posix.Env
import Data.Maybe
import System.Directory
import Control.Monad (liftM)
import System.IO
import XMonad.Core (X, cfgDir, directories, getXMonadDir)
import XMonad (asks,liftX, liftIO )
xmonad_home :: IO String
xmonad_home =
  do m_xmh <- getEnv "XMONAD_HOME"
     home <-  getEnv "HOME" >>= (\s -> return $ fromMaybe (error "No $HOME is set") s) 
     let cfg_dir=home ++ "/.config/xmonad"
     let dfl_dir=home ++ "/.xmonad" -- Old/coventional xmonad home
     is_cfg_dir <- doesDirectoryExist cfg_dir
     is_dfl_dir <- doesDirectoryExist dfl_dir
     let out = case (m_xmh,is_cfg_dir,is_dfl_dir) of
                   (Nothing, True, _) -> cfg_dir
                   (Nothing, False,True) -> dfl_dir
                   (Just xmh, _  ,  _ ) -> xmh
     return  out
     

xmonadHome :: IO String
xmonadHome = getEnv "XMONAD_HOME"  >>=
  
                (\m -> return $ fromMaybe "" m)  
{-
wtest :: IO Int
wtest = 
   do let foo = 123
      print $ show foo
      print $ show x
      print $ (bar foo) + bar
          where bar z =z
                x = 4
-}

getXMonadHome :: X FilePath
getXMonadHome =   asks (cfgDir . directories) -- getXMonadDir


        

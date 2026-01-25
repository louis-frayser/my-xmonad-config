import System.IO
import System.Process(system)
import Control.Exception
import Control.Concurrent

fifo = "fifo"
waitval = -1
delaysecs = 2
delayusecs = delaysecs * (round 1E6)
buttons="Admin,Home,PIM,Practice,Research,Graphics,A/V"
main =
  do system $ "{ sleep 5;xmessage -print -buttons "++buttons
       ++ " Workspaces "++ ">>" ++ fifo ++ " ;}&"
     withFile fifo ReadMode doIt
     
doIt h = do catch tryit  tryit_again
              where tryit =
                      do putStrLn "tryit..."
                         ready <- hWaitForInput h waitval
                         if ready then show_result
                                   else putStrLn " WTF"
                    tryit_again e =
                      do putStrLn $ "E :-" ++ show (e ::IOError)
                         putStrLn "Waiting for input..."
                         threadDelay delayusecs
                         doIt h
                    show_result = do result <- hGetLine h
                                     putStrLn $ "RESULT: " ++ result

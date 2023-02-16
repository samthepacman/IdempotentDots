module MyStartup(startup_run) where

import System.Process
import System.IO
import System.Exit

import Control.Monad

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- startup

startup_run :: IO ()
startup_run = do
    -- no need to use silent, as the std_err redirected
    let args = ["new_attr", "bool", "my_not_first_autostart"]
    
    (_, _, Just pipeerr, ph) <- 
        createProcess (proc "herbstclient" args)
        { std_err = CreatePipe } 

    capture_err <- hGetContents pipeerr   
    exitcode <- waitForProcess ph

    -- The test may use against, either exitcode or capture_err    
    when (exitcode == ExitSuccess) $ do
     -- non windowed app
        system $ "systemctl --restart picom &"
        system $ "dunst &"
        system $ "sh ~/.config/polybar/launch.sh &"
        system $ "sh ~/.fehbg &"
        system $ "mpd &"

     -- windowed app
      --  system $ "xfce4-terminal &"
      --  system $ "sleep 1 && firefox &"
      --  system $ "sleep 2 && geany &"
      --  system $ "sleep 2 && thunar &"

        return ()

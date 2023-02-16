module MyHelper
( hc
, do_config
, set_tags_with_name
, bind_cycle_layout
, do_panel
) where

import System.Process
import System.Exit

import System.Directory
import System.IO

import MyConfig

type Pair = (String, String)

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- helpers

hc :: String -> IO ExitCode
hc arguments = system $ "herbstclient " ++ arguments

-- http://epsi-rns.github.io/code/2017/05/12/haskell-loop-with-map.html

-- IO action procedure
do_config :: String -> [Pair] -> IO ()
do_config command pairs = do
    -- loop over a hash dictionary of tuples
    mapM_ (\(key, value) -> do 
            hc(command ++ " " ++ key ++ " " ++ value)

            -- uncomment to debug in terminal
            -- putStrLn(command ++ " " ++ key ++ " " ++ value)
        ) pairs   

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- tags related

set_tags_with_name :: IO ()
set_tags_with_name = do
    hc("rename default '" 
        ++ show (tag_names !! 0) ++ "' 2>/dev/null || true")

    mapM_ (\index -> do
            hc("add '" ++ show(tag_names !! index) ++ "'")
    
            -- uncomment to debug in terminal
            -- putStrLn $ show index

            let key = tag_keys !! index
            case (Just key) of
                Nothing   -> do return()
                Just akey -> do
                    hc("keybind Mod4-" ++ show(akey) 
                        ++ " use_index '" ++ show(index) ++ "'")
                    hc("keybind Mod4-Shift-" ++ show(akey) 
                        ++ " move_index '" ++ show(index) ++ "'")
                    return ()

        ) ([0 .. (length tag_names) - 1]) -- indices
    
-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- miscellanous

bind_cycle_layout :: IO ExitCode
bind_cycle_layout = do
    -- The following cycles through the available layouts
    -- within a frame, but skips layouts, if the layout change 
    -- wouldn't affect the actual window positions.
    -- I.e. if there are two windows within a frame,
    -- the grid layout is skipped.

    hc( "keybind Mod4-space "
        ++ "or , and . compare tags.focus.curframe_wcount = 2 "
        ++ ". cycle_layout +1 vertical horizontal max vertical grid "
        ++ ", cycle_layout +1 " )

-- do multi monitor setup here, e.g.:
-- hc("set_monitors 1280x1024+0+0 1280x1024+1280+0");
-- or simply:
-- hc("detect_monitors");

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- find the panel

-- Source directory is irrelevant in Haskell
-- So hardcoded, for the sake of learning
customFilename :: String -> String
customFilename home = home ++ path 
    where path = "/.config/herbstluftwm/haskell/panel-lemonbar"
    
panelFilename :: IO String
panelFilename = do
    home <- getHomeDirectory
    let file = customFilename home

    isExist <- doesFileExist file
    permission <- getPermissions file
    let exec_ok = executable permission
    
    -- need to check for executable permission also
    let panel = if(isExist && exec_ok)
            then file    
            else "/etc/xdg/herbstluftwm/panel.sh"
    
    -- uncomment to debug in terminal
    -- putStrLn panel
    
    return panel

listMonitors :: IO [String]
listMonitors = do
    (_, Just pipeout, _, _) <- 
        createProcess (proc "herbstclient" ["list_monitors"])
        { std_out = CreatePipe } 

    (_, Just cutout, _, ph)  <- 
        createProcess (proc "cut" ["-d:", "-f1"]) 
        { std_in = UseHandle pipeout, std_out = CreatePipe }
        
    raw <- hGetContents cutout   
    _ <- waitForProcess ph

    -- uncomment to debug in terminal     
    -- putStrLn raw
    let monitors = lines raw  -- or splitOn instead

    return monitors

do_panel :: IO ()
do_panel = do
    panel <- panelFilename
    monitors <- listMonitors
    
    mapM_ (\monitor -> do
            system(panel ++ " " ++ show(monitor) ++ " &")
          ) monitors

    return ()

-- ------------------------------------------------------------------
--
--     Description: unified config for herbstluftwm lemonbar
--     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
--
--     Source
--     https://github.com/epsi-rns/dotfiles/tree/master/standalone/lemon-hlwm/haskell
--
--     Blog
--     http://epsi-rns.github.io/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
--     http://epsi-rns.github.io/desktop/2017/06/08/herbstlustwm-tag-status-haskell.html
--     http://epsi-rns.github.io/desktop/2017/06/18/herbstlustwm-event-idle-haskell.html
--
-- ------------------------------------------------------------------

import System.Environment
import System.Process
import System.IO
import System.Exit
import System.Posix.Types
import System.Posix.Process
import System.Directory

import GHC.IO.Handle

import Control.Concurrent
import Control.Monad

import Data.IORef
import System.IO.Unsafe

import Data.Time.LocalTime
import Data.Time.Format

-- cabal install split
import Data.List.Split

import MyGMC

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- helper

getMonitor :: [String] -> Int
getMonitor args
  | length(args) > 0 = read (args !! 0) :: Int
  | otherwise        = 0

-- geometry calculation

getGeometry :: Int -> IO [Int]
getGeometry monitor = do 
    let args = ["monitor_rect", show(monitor)]

    (_, Just pipe_out, _, ph) <- 
        createProcess (proc "herbstclient" args)
        { std_out = CreatePipe } 
        
    raw <- hGetContents pipe_out   
    _ <- waitForProcess ph
    
    when (raw == "") $ do
        putStrLn $ "Invalid monitor " ++ show(monitor)
        exitSuccess

    let geometry = map (read::String->Int) (words raw)
    
    return geometry


-- geometry has the format X Y W H
data XYWH = XYWH String String String String

getTopPanelGeometry :: Int -> [Int] -> XYWH
getTopPanelGeometry 
    height geometry = XYWH 
                      (show (geometry !! 0))
                      (show (geometry !! 1))
                      (show (geometry !! 2))
                      (show height)

getBottomPanelGeometry :: Int -> [Int] -> XYWH
getBottomPanelGeometry 
    height geometry = XYWH 
                      (show ((geometry !! 0) + 0))
                      (show ((geometry !! 3) - height))
                      (show ((geometry !! 2) - 0))
                      (show height)

-- lemonbar Parameters

getParamsTop :: Int -> [Int] -> [String]
getParamsTop panelHeight geometry = [
          "-g", geom_res,  "-u", "2",
          "-B", bgcolor, "-F", fgcolor,
          "-f", font_takaop,
          "-f", font_awesome,
          "-f", font_symbol
        ]
      where
        -- calculate geometry
        XYWH xpos ypos width height = getTopPanelGeometry 
                                      panelHeight geometry        

        -- geometry: -g widthxheight++y
        geom_res = width ++ "x" ++ height
            ++ "+" ++ xpos ++ "+" ++ ypos

        -- color, with transparency    
        bgcolor = "#aa000000"
        fgcolor = "#ffffff"
        
        -- XFT: require lemonbar_xft_git 
        font_takaop  = "takaopgothic-9"
        font_symbol  = "PowerlineSymbols-11"
        font_awesome = "FontAwesome-9"

getParamsBottom :: Int -> [Int] -> [String]
getParamsBottom panelHeight geometry = [
          "-g", geom_res,  "-u", "2",
          "-B", bgcolor, "-F", fgcolor,
          "-f", font_mono,
          "-f", font_awesome,
          "-f", font_symbol
        ]
      where
        -- calculate geometry
        XYWH xpos ypos width height = getBottomPanelGeometry 
                                      panelHeight geometry        

        -- geometry: -g widthxheight++y
        geom_res = width ++ "x" ++ height
            ++ "+" ++ xpos ++ "+" ++ ypos

        -- color, with transparency    
        bgcolor = "#aa000000"
        fgcolor = "#ffffff"
        
        -- XFT: require lemonbar_xft_git 
        font_mono    = "monospace-9"
        font_symbol  = "PowerlineSymbols-11"
        font_awesome = "FontAwesome-9"

getLemonParameters :: Int -> [Int] -> [String]
getLemonParameters panelHeight geometry = 
    getParamsTop panelHeight geometry

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- output

-- initialize

-- assuming $ herbstclient tag_status
-- 	#1	:2	:3	:4	.5	.6	.7	.8	.9

-- custom tag names
tagShows :: [String] 
tagShows = ["一 ichi", "二 ni", "三 san", "四 shi", 
    "五 go", "六 roku", "七 shichi", "八 hachi", "九 kyū", "十 jū"]

-- initialize variable segment
-- simulate global variable using unsafe

segmentWindowtitle :: IORef String
segmentWindowtitle = unsafePerformIO $ newIORef "" -- empty string

tagsStatus :: IORef [String]
tagsStatus = unsafePerformIO $ newIORef []         -- empty string list

segmentDatetime :: IORef String
segmentDatetime = unsafePerformIO $ newIORef ""    -- empty string

-- decoration

separator = "%{B-}%{F" ++ myColor "yellow500" ++ "}|%{B-}%{F-}"

-- Powerline Symbol
rightHardArrow = "\57520"
rightSoftArrow = "\57521"
leftHardArrow  = "\57522"
leftSoftArrow  = "\57523"

-- theme
preIcon    = "%{F" ++ myColor "yellow500" ++ "}"
postIcon   = "%{F-}"

-- helper
 
wFormatTime :: FormatTime t => t -> String -> String
wFormatTime myUtcTime myTimeFormat = formatTime 
    Data.Time.Format.defaultTimeLocale myTimeFormat myUtcTime

-- main

getStatusbarText :: Int -> IO String
getStatusbarText monitor = do
    tags <- readIORef tagsStatus
    
    let tagText = "%{l}" ++ (join $ map (outputByTag monitor) tags)
    timeText  <- ("%{c}" ++) <$> outputByDatetime
    titleText <- ("%{r}" ++) <$> outputByTitle

    let text = tagText ++ timeText ++ titleText
    return text

-- each segments

outputByTag :: Int -> String -> String
outputByTag monitor tagStatus = 
    textPre ++ textName ++ textPost ++ textClear
  where
    -- text = ''

    tagIndex  = drop 1 tagStatus 
    tagMark   = take 1 tagStatus 
    index     = (read::String->Int) tagIndex - 1     -- zero based
    tagName   = tagShows !! index

    ----- pre tag
    
    textPre   = case tagMark of
        "#" -> "%{B" ++ myColor "blue500" ++ "}"
            ++ "%{F" ++ myColor "black" ++ "}"
            ++ "%{U" ++ myColor "white" ++ "}%{+u}" 
            ++ rightHardArrow
            ++ "%{B" ++ myColor "blue500" ++ "}"
            ++ "%{F" ++ myColor "white" ++ "}"
            ++ "%{U" ++ myColor "white" ++ "}%{+u}"
        "+" -> "%{B" ++ myColor "yellow500" ++ "}"
            ++ "%{F" ++ myColor "grey400" ++ "}"
        ":" -> "%{B-}"
            ++"%{F" ++ myColor "white" ++ "}"
            ++ "%{U" ++ myColor "red500" ++ "}%{+u}"
        "!" -> "%{B" ++ myColor "red500" ++ "}"
            ++ "%{F" ++ myColor "white" ++ "}"
            ++ "%{U" ++ myColor "white" ++ "}%{+u}"
        _   -> "%{B-}"
            ++ "%{F" ++ myColor "grey600" ++ "}%{-u}"

    ----- tag by number
    
    -- clickable tags
    textName  = "%{A:herbstclient focus_monitor \"" 
        ++ show(monitor) ++ "\" && " ++ "herbstclient use \"" 
        ++ tagIndex ++ "\":} " ++ tagName ++ " %{A} "
   
    -- non clickable tags
    -- textName = " " ++ tagName ++ " "

    ----- post tag

    textPost  = if (tagMark == "#")
                    then "%{B-}"
                      ++ "%{F" ++ myColor "blue500" ++ "}"
                      ++ "%{U" ++ myColor "red500" ++ "}%{+u}"
                      ++ rightHardArrow
                    else ""
    
    textClear = "%{B-}%{F-}%{-u}"

outputByTitle :: IO String
outputByTitle = do
    segment <- readIORef segmentWindowtitle
    let text  = segment ++ " " ++ separator ++ "  "

    return text

outputByDatetime :: IO String
outputByDatetime = do
    segment <- readIORef segmentDatetime
    return segment

-- setting variables, response to event handler
 
setTagValue :: Int -> IO ()
setTagValue monitor = do
    let args = ["tag_status", show(monitor)]

    (_, Just pipe_out, _, ph) <- 
        createProcess (proc "herbstclient" args)
        { std_out = CreatePipe } 
        
    raw <- hGetContents pipe_out   
    _ <- waitForProcess ph

    let statusList = words raw
    writeIORef tagsStatus statusList

setWindowtitle :: String -> IO ()
setWindowtitle windowtitle = do
    let icon = preIcon ++ "\61444" ++ postIcon
    let text = " " ++ icon ++ " %{B-}"
               ++ "%{F" ++ myColor "grey700" ++ "} " ++ windowtitle
    writeIORef segmentWindowtitle text

formatDatetime :: ZonedTime -> String
formatDatetime now = dateText ++ "  " ++ timeText
  where
    dateStr = wFormatTime now "%a %b %d"
    timeStr = wFormatTime now "%H:%M:%S"
     
    dateIcon = preIcon ++ "\61555" ++ postIcon
    timeIcon = preIcon ++ "\61463" ++ postIcon

    dateText = " " ++ dateIcon ++ " %{B-}"
               ++ "%{F" ++ myColor "grey700" ++ "} " ++ dateStr

    timeText = " " ++ timeIcon ++ " %{B-}"
               ++ "%{F" ++ myColor "blue500" ++ "} " ++ timeStr

setDatetime :: IO ()
setDatetime = do
    now <- getZonedTime     
    writeIORef segmentDatetime $ formatDatetime now

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- pipe handler


wSleep :: Int -> IO ()
wSleep mySecond = threadDelay (1000000 * mySecond)

-- pipe 

getColumnTitle :: [String] -> String
getColumnTitle column
  | length(column) > 2 = column !! 2
  | otherwise          = ""

handleCommandEvent :: Int -> String -> IO ()
handleCommandEvent monitor event
  | origin == "reload"      = do system("pkill lemonbar"); return ()
  | origin == "quit_panel"  = do exitSuccess; return ()
  | elem origin tagCmds     = do setTagValue monitor
  | elem origin titleCmds   = do setWindowtitle $ getColumnTitle column
  | origin == "interval"    = do setDatetime
  where
    tagCmds   = ["tag_changed", "tag_flags", "tag_added", "tag_removed"]
    titleCmds = ["window_title_changed", "focus_changed"]

    -- find out event origin
    column = splitOn "\t" event
    origin = column !! 0

contentInit :: Int -> Handle -> IO ()
contentInit monitor pipe_lemon_in = do
    -- initialize statusbar before loop
    setTagValue monitor 
    setWindowtitle ""
    setDatetime

    text <- getStatusbarText monitor

    hPutStrLn pipe_lemon_in text
    hFlush pipe_lemon_in

contentEventIdle :: Handle -> IO ()
contentEventIdle pipe_cat_in = do
    let command_in = "herbstclient"

    (_, Just pipe_idle_out, _, ph) <- 
        createProcess (proc command_in ["--idle"]) 
        { std_out = CreatePipe }

    forever $ do
        -- wait for next event 
        event <- hGetLine pipe_idle_out

        hPutStrLn pipe_cat_in event
        hFlush pipe_cat_in

    hClose pipe_idle_out

contentEventInterval :: Handle -> IO ()
contentEventInterval pipe_cat_in = forever $ do
     hPutStrLn pipe_cat_in "interval"
     hFlush pipe_cat_in

     wSleep 1

contentWalk :: Int -> Handle -> IO ()
contentWalk monitor pipe_lemon_in = do
    (Just pipe_cat_in, Just pipe_cat_out, _, ph) <- 
        createProcess (proc "cat" []) 
        { std_in = CreatePipe, std_out = CreatePipe }

    forkProcess $ contentEventIdle(pipe_cat_in)
    forkProcess $ contentEventInterval(pipe_cat_in)
    
    forever $ do
        -- wait for next event 
        event <- hGetLine pipe_cat_out
        handleCommandEvent monitor event
 
        text <- getStatusbarText monitor

        hPutStrLn pipe_lemon_in text
        hFlush pipe_lemon_in

    hClose pipe_cat_out
    hClose pipe_cat_in

runLemon :: Int -> [String] -> IO ()
runLemon monitor parameters = do
    let command_out = "lemonbar"

    (Just pipe_lemon_in, Just pipe_lemon_out, _, ph) <- 
        createProcess (proc command_out parameters) 
        { std_in = CreatePipe, std_out = CreatePipe }

    (_, _, _, ph) <- 
        createProcess (proc "sh" []) 
        { std_in = UseHandle pipe_lemon_out }

    contentInit monitor pipe_lemon_in
    contentWalk monitor pipe_lemon_in  -- loop for each event
    
    hClose pipe_lemon_in
    hClose pipe_lemon_out

detachLemon :: Int -> [String] -> IO ProcessID
detachLemon monitor parameters = forkProcess 
    $ runLemon monitor parameters 

detachLemonConky :: [String] -> IO ()
detachLemonConky parameters = do
    -- Source directory is irrelevant in Haskell
    -- but we'll do it anyway for the sake of learning
    dirName <- getCurrentDirectory
    let conkyFileName = dirName ++ "/../conky" ++ "/conky-lemonbar.lua" 

    (_, Just pipeout, _, _) <- 
        createProcess (proc "conky" ["-c", conkyFileName])
        { std_out = CreatePipe } 

    (_, _, _, ph)  <- 
        createProcess (proc "lemonbar" parameters) 
        { std_in = UseHandle pipeout }
      
    hClose pipeout

killZombie :: IO ()
killZombie = do
    system "pkill -x dzen2"
    system "pkill -x lemonbar"
    system "pkill -x cat"
    system "pkill conky"
    system "pkill herbstclient"
    
    return ()

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- main

-- initialize
panelHeight = 24

main = do
    args <- getArgs
    let monitor = getMonitor args

    geometry <- getGeometry monitor

    killZombie
    system $ "herbstclient pad " ++ show(monitor) ++ " "
        ++ show(panelHeight) ++ " 0 " ++ show(panelHeight) ++ " 0"

    -- run process in the background

    let paramsTop = getParamsTop panelHeight geometry
    detachLemon monitor paramsTop

    let paramsBottom = getParamsBottom panelHeight geometry
    detachLemonConky paramsBottom

    -- end of IO
    return ()

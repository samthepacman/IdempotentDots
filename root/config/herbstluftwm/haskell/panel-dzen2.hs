-- ------------------------------------------------------------------
--
--     Description: unified config for herbstluftwm dzen2 statusbar
--     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
--
--     Source
--     https://github.com/epsi-rns/dotfiles/tree/master/standalone/dzen2-hlwm/haskell
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

-- dzen Parameters

getParamsTop :: Int -> [Int] -> [String]
getParamsTop
    panelHeight geometry = [
          "-x", xpos,  "-y", ypos,
          "-w", width, "-h", height,
          "-ta", "l",
          "-bg", bgcolor,
          "-fg", fgcolor,
          "-title-name", "dzentop",
          "-fn", font
        ]
      where
        XYWH xpos ypos width height = getTopPanelGeometry 
                                      panelHeight geometry        
        bgcolor = "#000000"
        fgcolor = "#ffffff"
        font    = "-*-takaopgothic-medium-*-*-*-12-*-*-*-*-*-*-*"

getParamsBottom :: Int -> [Int] -> [String]
getParamsBottom
    panelHeight geometry = [
          "-x", xpos,  "-y", ypos,
          "-w", width, "-h", height,
          "-ta", "l",
          "-bg", bgcolor,
          "-fg", fgcolor,
          "-title-name", "dzenbottom",
          "-fn", font
        ]
      where
        XYWH xpos ypos width height = getBottomPanelGeometry 
                                      panelHeight geometry        
        bgcolor = "#000000"
        fgcolor = "#ffffff"
        font    = "-*-fixed-medium-*-*-*-11-*-*-*-*-*-*-*"

getDzen2Parameters :: Int -> [Int] -> [String]
getDzen2Parameters panelHeight geometry = 
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

separator = "^bg()^fg(" ++ myColor "black" ++ ")|^bg()^fg()"

-- http://fontawesome.io/
fontAwesome = "^fn(FontAwesome-9)"

-- Powerline Symbol
rightHardArrow = "^fn(powerlinesymbols-14)\57520^fn()"
rightSoftArrow = "^fn(powerlinesymbols-14)\57521^fn()"
leftHardArrow  = "^fn(powerlinesymbols-14)\57522^fn()"
leftSoftArrow  = "^fn(powerlinesymbols-14)\57523^fn()"

-- theme
preIcon    = "^fg(" ++ myColor "yellow500" ++ ")" ++ fontAwesome
postIcon   = "^fn()^fg()"

-- helper
 
wFormatTime :: FormatTime t => t -> String -> String
wFormatTime myUtcTime myTimeFormat = formatTime 
    Data.Time.Format.defaultTimeLocale myTimeFormat myUtcTime

-- main

getStatusbarText :: Int -> IO String
getStatusbarText monitor = do    
    tags <- readIORef tagsStatus
    let tagText = join $ map (outputByTag monitor) tags

    timeText  <- outputByDatetime
    titleText <- outputByTitle

    let text = tagText ++ timeText ++ titleText
    return text

-- each segments

outputByTag :: Int -> String -> String
outputByTag monitor tagStatus = textPre ++ textName ++ textPost
  where
    -- text = ''

    tagIndex  = drop 1 tagStatus 
    tagMark   = take 1 tagStatus 
    index     = (read::String->Int) tagIndex - 1     -- zero based
    tagName   = tagShows !! index

    ----- pre tag
    
    textPre   = case tagMark of
        "#" -> "^bg(" ++ myColor "blue500" ++ ")"
            ++ "^fg(" ++ myColor "black" ++ ")"
            ++ rightHardArrow
            ++ "^bg(" ++ myColor "blue500" ++ ")"
            ++ "^fg(" ++ myColor "white" ++ ")"
        "+" -> "^bg(" ++ myColor "yellow500" ++ ")"
            ++ "^fg(" ++ myColor "grey400" ++ ")"
        ":" -> "^bg()^fg(" ++ myColor "white" ++ ")"
        "!" -> "^bg(" ++ myColor "red500" ++ ")"
            ++ "^fg(" ++ myColor "white" ++ ")"
        _   -> "^bg()^fg(" ++ myColor "grey600" ++ ")"

    ----- tag by number
   
    -- assuming using dzen2_svn
    -- clickable tags if using SVN dzen
    textName  = "^ca(1,herbstclient focus_monitor \"" 
        ++ show(monitor) ++ "\" && " ++ "herbstclient use \"" 
        ++ tagIndex ++ "\") " ++ tagName ++ " ^ca() "

    ----- post tag

    textPost  = if (tagMark == "#")
                    then "^bg(" ++ myColor "black" ++ ")"
                      ++ "^fg(" ++ myColor "blue500" ++ ")"
                      ++ rightHardArrow
                    else ""

outputByTitle :: IO String
outputByTitle = do
    segment <- readIORef segmentWindowtitle
    let text  = " ^r(5x0) " ++ separator ++ " ^r(5x0) " ++ segment

    return text

outputByDatetime :: IO String
outputByDatetime = do
    segment <- readIORef segmentDatetime
    let text  = " ^r(5x0) " ++ separator ++ " ^r(5x0) " ++ segment

    return text

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
    let text = " " ++ icon ++ " ^bg()"
               ++ "^fg(" ++ myColor "grey700" ++ ") " ++ windowtitle
    writeIORef segmentWindowtitle text

formatDatetime :: ZonedTime -> String
formatDatetime now = dateText ++ "  " ++ timeText
  where
    dateStr = wFormatTime now "%a %b %d"
    timeStr = wFormatTime now "%H:%M:%S"
     
    dateIcon = preIcon ++ "\61555" ++ postIcon
    timeIcon = preIcon ++ "\61463" ++ postIcon

    dateText = " " ++ dateIcon ++ " ^bg()"
               ++ "^fg(" ++ myColor "grey700" ++ ") " ++ dateStr

    timeText = " " ++ timeIcon ++ " ^bg()"
               ++ "^fg(" ++ myColor "blue500" ++ ") " ++ timeStr

setDatetime :: IO ()
setDatetime = do
    now <- getZonedTime     
    writeIORef segmentDatetime $ formatDatetime now

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- pipe handler

-- for use with transset
wSleep :: Int -> IO ()
wSleep mySecond = threadDelay (1000000 * mySecond)

getColumnTitle :: [String] -> String
getColumnTitle column
  | length(column) > 2 = column !! 2
  | otherwise          = ""

handleCommandEvent :: Int -> String -> IO ()
handleCommandEvent monitor event
  | origin == "reload"      = do system("pkill dzen2"); return ()
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
contentInit monitor pipe_dzen2_in = do
    -- initialize statusbar before loop
    setTagValue monitor 
    setWindowtitle ""
    setDatetime
    
    text <- getStatusbarText monitor

    hPutStrLn pipe_dzen2_in text
    hFlush pipe_dzen2_in

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
contentWalk monitor pipe_dzen2_in = do
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

        hPutStrLn pipe_dzen2_in text
        hFlush pipe_dzen2_in

    hClose pipe_cat_out
    hClose pipe_cat_in

runDzen2 :: Int -> [String] -> IO ()
runDzen2 monitor parameters = do
    let command_out = "dzen2"

    (Just pipe_dzen2_in, _, _, ph)  <- 
        createProcess (proc command_out parameters) 
        { std_in = CreatePipe }
       
    contentInit monitor pipe_dzen2_in
    contentWalk monitor pipe_dzen2_in  -- loop for each event
    
    hClose pipe_dzen2_in

detachDzen2 :: Int -> [String] -> IO ProcessID
detachDzen2 monitor parameters = forkProcess 
    $ runDzen2 monitor parameters 

detachDzen2Conky :: [String] -> IO ()
detachDzen2Conky parameters = do
    -- Source directory is irrelevant in Haskell
    -- but we'll do it anyway for the sake of learning
    dirName <- getCurrentDirectory
    let conkyFileName = dirName ++ "/../conky" ++ "/conky-dzen2.lua" 

    (_, Just pipeout, _, _) <- 
        createProcess (proc "conky" ["-c", conkyFileName])
        { std_out = CreatePipe } 

    (_, _, _, ph)  <- 
        createProcess (proc "dzen2" parameters) 
        { std_in = UseHandle pipeout }
      
    hClose pipeout
    
detachTransset :: IO ProcessID
detachTransset = forkProcess $ do    
    wSleep 1
    system "transset .8 -n dzentop    >/dev/null"
    system "transset .8 -n dzenbottom >/dev/null"
    return ()

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
    -- initialize
    
    args <- getArgs
    let monitor = getMonitor args
        
    geometry <- getGeometry monitor

    killZombie
    system $ "herbstclient pad " ++ show(monitor) ++ " "
        ++ show(panelHeight) ++ " 0 " ++ show(panelHeight) ++ " 0"

    -- run process in the background

    let paramsTop = getParamsTop panelHeight geometry
    detachDzen2 monitor paramsTop

    let paramsBottom = getParamsBottom panelHeight geometry
    detachDzen2Conky paramsBottom

    -- optional transparency
    detachTransset

    -- end of IO
    return ()

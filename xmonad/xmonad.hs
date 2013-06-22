{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}
--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import Control.Exception
import Data.Ratio ((%))
import Control.Monad (filterM)
import Data.Monoid
import System.Exit
import System.IO (hPutStrLn)
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map        as M

import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import XMonad.Layout.Reflect
import XMonad.Layout.LayoutModifier
import XMonad.Layout.PerWorkspace
import XMonad.Util.WindowProperties
import XMonad.Util.Loggers
import XMonad.Layout.OneBig
import XMonad.Layout.Circle
import XMonad.Layout.DragPane
import XMonad.Layout.Dishes

myShorten :: Int -> String -> String
myShorten n xs | length xs < n = xs
               | otherwise     = take (n - length end) xs ++ end
 where
    end = "~"
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "sakura"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["web", "code", "term", "im", "remote_1", "remote_2", "remote_3", "misc", "audio"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#6c71c4"
myFocusedBorderColor = "#859900"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_r     ), spawn $ "dmenu_run -i -p 'Run command:' -nb '#1d1f21' -nf '#f0c674' -sb '#373b41' -sf '#f0c674'")

    -- launch emacs
    , ((modm,               xK_e     ), spawn $ "emacs")

    -- launch sonata
    , ((modm,               xK_a     ), spawn $ "sonata")

    -- launch firefox
    , ((modm,               xK_c     ), spawn $ "firefox-bin")

    -- launch thunar
    , ((modm,               xK_t     ), spawn $ "thunar")

    -- launch pidgin
    , ((modm,               xK_g     ), spawn $ "pidgin -f")

    -- play/pause mpd
    , ((modm .|. shiftMask, xK_p     ), spawn $ "mpc toggle &")

    -- stop mpd
    , ((modm .|. shiftMask, xK_s     ), spawn $ "mpc stop &")

    -- next mpd
    , ((modm .|. shiftMask, xK_d     ), spawn $ "mpc next &")

    -- prev mpd
    , ((modm .|. shiftMask, xK_a     ), spawn $ "mpc prev &")

    -- mute/volume up(down)
    , ((0 , xF86XK_AudioRaiseVolume), spawn "amixer -q -- sset PCM,0 10+")
    , ((0 , xF86XK_AudioLowerVolume), spawn "amixer -q -- sset PCM,0 10-")
    , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "amixer -q -- sset Master 1+")
    , ((shiftMask, xF86XK_AudioLowerVolume), spawn "amixer -q -- sset Master 1-")
    , ((0 , xF86XK_AudioMute), spawn "amixer -c0 -q -- sset Front,0 toggle")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- -- Swap the focused window with the next window
    -- , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)

    -- -- Swap the focused window with the previous window
    -- , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    , ((modm .|. shiftMask, xK_k), nextScreen)

    , ((modm .|. shiftMask, xK_l),  prevScreen)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    -- [((m .|. modm, k), windows $ onCurrentScreen f i)
    --      | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
    --      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    -- ++

    -- -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    -- [((m .|. modm, k), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++

    [ ((modm,               xK_Right),  nextWS)
    , ((modm,               xK_Left),    prevWS)
    ]


    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_Left, xK_Down, xK_Right] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [
    -- ((modm, button1), (\w -> focus w >> mouseMoveWindow w
    --                                        >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    -- , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- -- mod-button3, Set the window to floating mode and resize by dragging
    -- , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
    --                                    >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> W.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack = W.stack wksp
    let ws = W.integrate' $ stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= W.filter (`notElem` rosters)
    (a,b) <- runLayout (wksp {W.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)

data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)

instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"

hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w

withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props

imLayout = avoidStruts $ reflectHoriz $ withIMs ratio rosters chatLayout where
    chatLayout      = Grid
    ratio           = 1%6
    rosters         = [skypeRoster, pidginRoster]
    pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
    skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

gimpLayout = withIM (0.11) (Role "gimp-toolbox") $
             reflectHoriz $
             withIM (0.15) (Role "gimp-dock") Full

myLayout = onWorkspace "web" Full $
           onWorkspace "im" imLayout $
           onWorkspace "8" gimpLayout $
           tiled ||| Mirror tiled ||| Full ||| OneBig (1/2) (1/2) ||| Circle ||| dragPane Horizontal 0.1 0.5 ||| Dishes 2 (1/6)
               where
                 tiled   = Tall nmaster delta ratio
                 nmaster = 1
                 ratio   = 1/2
                 delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
               [resource  =? "desktop_window" --> doIgnore,
                className  =? "Plasma" --> doIgnore,
                className  =? "plasma-desktop" --> doIgnore,
                className =? "xfce4-notifyd" --> doIgnore,
                className =? "Pidgin" --> doF (W.shift "im"),
                className =? "Firefox" --> doF (W.shift "web"),
                className =? "Chromium-browser" --> doF (W.shift "web"),
                className =? "Sonata" --> doF (W.shift "audio"),
                className =? "Emacs" --> doF (W.shift "code"),
                className =? "Subl" --> doF (W.shift "code")]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- myLogHook = dynamicLogDzen
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawn "/home/aluuu/.screenlayout/home.sh"
  spawn "pkill trayer-srg || trayer-srg --monitor 1 --edge bottom --align right --height 20 --padding 1 --widthtype percent --width 7 --transparent true --alpha 0 --tint 0xfdf6e3"
  spawn "setxkbmap -model evdev -layout us,ru -option lv3:ralt_switch,grp:caps_toggle,misc:typo,grp_led:caps"
  spawn "pkill xxkb || xxkb"
  spawn "xsetroot -cursor_name left_ptr -solid \"#81a2be\""
  spawn "pkill nm-applet || nm-applet"
  spawn "pkill blueman-applet || blueman-applet"
  spawn "xset b off"
  spawn "pkill redshift || pkill gtk-redshift || gtk-redshift -l 43.39:79.23 -t 4600:5600 -g 0.8 -m vidmode -v"

-- Command to launch the bar.
myBar = "xmobar --screen 1"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#d33682" "" . wrap "<" ">",
                  ppUrgent = xmobarColor "#d33682" "" . wrap "*" "*",
                  ppHidden = xmobarColor "#002b36" "" . wrap "(" ")",
                  ppHiddenNoWindows = xmobarColor "#373b41" "" . wrap "" "",
                  ppSep = " | ",
                  ppWsSep = "-",
                  ppLayout = xmobarColor "#002b36" "" . myShorten 6,
                  ppOrder = id,
                  ppTitle = xmobarColor "#002b36"  "" . myShorten 0,
                  ppExtras = [
                   xmobarColorL "#002b36" "" dateLogger,
                   xmobarColorL "#002b36" "" battery
                   -- ,
                   -- xmobarColorL "#f0c674" "" mpcLogger
                  ]}
       where
         dateLogger = date "%d.%m.%y %H:%M"
         -- mpcLogger = logCmd "mpc status | head -n 1"

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        -- numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        -- logHook = dynamicLogWithPP defaultPP { ppOutput = hPutStrLn xmobar},
        startupHook        = myStartupHook
    }
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig --    xmobar <- spawnPipe "xmobar"

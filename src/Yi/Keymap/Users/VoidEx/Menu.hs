module Yi.Keymap.Users.VoidEx.Menu (
    mainMenu,
    windowsMenu,
    buffersMenu,
    tabsMenu,
    ghciMenu
    ) where

import Prelude ()

import Yi.Core
import Yi.File
import Yi.Keymap.Emacs.Utils (askQuitEditor)
import Yi.Mode.Haskell (ghciLoadBuffer, ghciInferType, ghciSend)
import Yi.TextCompletion

import Yi.Keymap.Menu

-- | Main menu
mainMenu :: Menu
mainMenu = [
    menu "File" [
        action_ "Quit" askQuitEditor,
        action "Save" (fwriteBufferE . parentBuffer)],
    menu "Edit" [
        action_ "Auto complete" wordComplete,
        action_ "Completion" completeWordB],
    menu "Tools" [
        menu "Ghci" ghciMenu],
    menu "View" [
        menu "Windows" windowsMenu,
        menu "Tabs" tabsMenu,
        menu "Buffers" buffersMenu,
        menu "Layout" [
            action_ "Next" layoutManagersNextE,
            action_ "Previous" layoutManagersPreviousE]]]

-- | Windows menu
windowsMenu :: Menu
windowsMenu = [
    action_ "Next" nextWinE,
    action_ "Previous" prevWinE,
    action_ "Split" splitE,
    action_ "sWap" swapWinWithFirstE,
    action_ "Close" tryCloseE,
    action_ "cLose-all-but-this" closeOtherE]

-- | Buffers menu
buffersMenu :: Menu
buffersMenu = [
    action_ "Next" nextBufW,
    action_ "Previous" prevBufW,
    action_ "Close" closeBufferAndWindowE,
    action_ "Show all" openAllBuffersE]

-- | Tabs menu
tabsMenu :: Menu
tabsMenu = [
    action_ "Next" nextTabE,
    action_ "Previous" previousTabE,
    action_ "neW" newTabE,
    action_ "Delete" deleteTabE]

-- | GHCi menu
ghciMenu :: Menu
ghciMenu = [
    action_ "Load" ghciLoad,
    action_ "Infer-type" ghciInfer]

-- | Load buffer in GHCi
ghciLoad :: YiM ()
ghciLoad = do
    ghciLoadBuffer
    ghciSend $ ":set prompt " ++ show "ghci> "

-- | Infer type
ghciInfer :: YiM ()
ghciInfer = do
    ghciLoad
    ghciInferType

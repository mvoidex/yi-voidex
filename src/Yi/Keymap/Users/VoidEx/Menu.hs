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
        actionY_ "Quit" askQuitEditor,
        actionY "Save" (fwriteBufferE . parentBuffer)],
    menu "Edit" [
        actionY_ "Auto complete" wordComplete,
        actionE_ "Completion" completeWordB],
    menu "Tools" [
        menu "Ghci" ghciMenu],
    menu "View" [
        menu "Windows" windowsMenu,
        menu "Tabs" tabsMenu,
        menu "Buffers" buffersMenu,
        menu "Layout" [
            actionE_ "Next" layoutManagersNextE,
            actionE_ "Previous" layoutManagersPreviousE]]]

-- | Windows menu
windowsMenu :: Menu
windowsMenu = [
            actionE_ "Next" nextWinE,
            actionE_ "Previous" prevWinE,
            actionE_ "Split" splitE,
            actionE_ "sWap" swapWinWithFirstE,
            actionE_ "Close" tryCloseE,
            actionE_ "cLose-all-but-this" closeOtherE]

-- | Buffers menu
buffersMenu :: Menu
buffersMenu = [
            actionE_ "Next" nextBufW,
            actionE_ "Previous" prevBufW,
            actionE_ "Close" closeBufferAndWindowE,
            actionE_ "Show all" openAllBuffersE]

-- | Tabs menu
tabsMenu :: Menu
tabsMenu = [
            actionE_ "Next" nextTabE,
            actionE_ "Previous" previousTabE,
            actionE_ "neW" newTabE,
            actionE_ "Delete" deleteTabE]

-- | GHCi menu
ghciMenu :: Menu
ghciMenu = [
            actionY_ "Load" ghciLoad,
            actionY_ "Infer-type" ghciInfer]

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

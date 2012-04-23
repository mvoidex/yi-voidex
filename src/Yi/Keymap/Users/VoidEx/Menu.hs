module Yi.Keymap.Users.VoidEx.Menu (
    mainMenu
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
        menu "Ghci" [
            actionY_ "Load" ghciLoad,
            actionY_ "Infer-type" ghciInfer]],
    menu "View" [
        menu "Windows" [
            actionE_ "Next" nextWinE,
            actionE_ "Previous" prevWinE,
            actionE_ "Split" splitE,
            actionE_ "sWap" swapWinWithFirstE,
            actionE_ "Close" tryCloseE,
            actionE_ "cLose-all-but-this" closeOtherE],
        menu "Tabs" [
            actionE_ "Next" nextTabE,
            actionE_ "Previous" previousTabE,
            actionE_ "neW" newTabE,
            actionE_ "Delete" deleteTabE],
        menu "Buffers" [
            actionE_ "Next" nextBufW,
            actionE_ "Previous" prevBufW,
            actionE_ "Close" closeBufferAndWindowE,
            actionE_ "Show all" openAllBuffersE],
        menu "Layout" [
            actionE_ "Next" layoutManagersNextE,
            actionE_ "Previous" layoutManagersPreviousE]]]

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

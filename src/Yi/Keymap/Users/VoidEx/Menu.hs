module Yi.Keymap.Users.VoidEx.Menu (
    mainMenu
    ) where

import Prelude ()

import Yi.Core
import Yi.File
import Yi.Keymap.Emacs.Utils (askQuitEditor)
import Yi.Mode.Haskell (ghciLoadBuffer, ghciInferType)

import Yi.Keymap.Menu

-- | Main menu
mainMenu :: Menu
mainMenu = [
    menu "File" [
        actionY_ "Quit" askQuitEditor,
        actionY "Save" (fwriteBufferE . parentBuffer)],
    menu "Tools" [
        menu "Ghci" [
            actionY_ "Load" ghciLoadBuffer,
            actionY_ "Infer-type" ghciInferType]],
    menu "View" [
        actionE_ "Next-window" nextWinE,
        actionE_ "Previous-window" prevWinE,
        actionE_ "Split" splitE,
        actionE_ "sWap-with-first" swapWinWithFirstE,
        actionE_ "Close-buffer" closeBufferAndWindowE,
        actionE_ "Try-close-window" tryCloseE,
        actionE_ "sHow-all" openAllBuffersE,
        actionE_ "nEw-tab" newTabE,
        actionE_ "neXt-tab" nextTabE,
        actionE_ "pRevious-tab" previousTabE,
        actionE_ "Delete-tab" deleteTabE]]

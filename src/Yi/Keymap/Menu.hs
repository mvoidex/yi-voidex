module Yi.Keymap.Menu (
    Menu,
    MenuItem,
    MenuContext(..),
    menu, actionB_, actionB, actionE_, actionE, actionY_, actionY,
    startMenu,
    test
    ) where

import Prelude ()

import Yi.Core
import Yi.File
import Yi.MiniBuffer (spawnMinibufferE)

import Yi.Mode.Haskell (ghciLoadBuffer, ghciInferType)

import Yi.Keymap.Emacs.Utils (askQuitEditor)

import Control.Monad (fmap, void)
import Data.List (map, intercalate)
import Data.Char (isUpper, toLower)
import Data.Maybe (mapMaybe)

-- | Menu
type Menu = [MenuItem]

-- | Menu utem
data MenuItem =
    MenuAction String (MenuContext -> Char -> Keymap) |
    SubMenu String Menu

-- | Menu action context
data MenuContext = MenuContext {
    parentBuffer :: BufferRef }

-- | Sub menu
menu :: String -> Menu -> MenuItem
menu = SubMenu

-- | Action on buffer
actionB_ :: String -> BufferM () -> MenuItem
actionB_ title act = actionB title (const act)

-- | Action on buffer with context
actionB :: String -> (MenuContext -> BufferM ()) -> MenuItem
actionB title act = MenuAction title act' where
    act' ctx c = char c ?>>! (do
        closeBufferAndWindowE
        withGivenBuffer0 (parentBuffer ctx) (act ctx))

-- | Action menu
actionE_ :: String -> EditorM () -> MenuItem
actionE_ title act = actionE title (const act)

-- | Action with context
actionE :: String -> (MenuContext -> EditorM ()) -> MenuItem
actionE title act = MenuAction title act' where
    act' ctx c = char c ?>>! (closeBufferAndWindowE >> act ctx)

-- | Action on Yi
actionY_ :: String -> YiM () -> MenuItem
actionY_ title act = actionY title (const act)

-- | Action on Yi with context
actionY :: String -> (MenuContext -> YiM ()) -> MenuItem
actionY title act = MenuAction title act' where
    act' ctx c = char c ?>>! (withEditor closeBufferAndWindowE >> act ctx)

-- | Fold menu item
foldItem
    :: (String -> (MenuContext -> Char -> Keymap) -> a)
    -> (String -> [a] -> a)
    -> MenuItem
    -> a
foldItem mA sM (MenuAction title act) = mA title act
foldItem mA sM (SubMenu title sm) = sM title (map (foldItem mA sM) sm)

-- | Fold menu
foldMenu
    :: (String -> (MenuContext -> Char -> Keymap) -> a)
    -> (String -> [a] -> a)
    -> Menu
    -> [a]
foldMenu mA sM = map (foldItem mA sM)

-- | Menu title to keymap
menuEvent :: String -> Maybe Char
menuEvent = fmap toLower . find isUpper

-- | Start menu action
startMenu :: Menu -> EditorM ()
startMenu m = do
    ctx <- fmap MenuContext (gets currentBuffer)
    startMenu' ctx m
    where
        startMenu' ctx = showMenu . foldMenu onItem onSub where
            showMenu :: [(String, Maybe Keymap)] -> EditorM ()
            showMenu is = do
                spawnMinibufferE (intercalate " " (map fst is)) (const (subMap is))
                return ()
            onItem title act = (title, fmap (act ctx) (menuEvent title)) where
            onSub title is = (title, fmap subMenu (menuEvent title)) where
                subMenu c = char c ?>>! closeBufferAndWindowE >> showMenu is
            subMap is = choice $ closeMenu : mapMaybe snd is where
                closeMenu = spec KEsc ?>>! closeBufferAndWindowE

-- | Test menu with only File - Quit item
test = [
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
        actionE_ "Close" closeBufferAndWindowE,
        actionE "New" (void . newWindowE False . parentBuffer),
        actionE_ "Enlarge" enlargeWinE,
        actionE_ "shRink" shrinkWinE]]

module Yi.Keymap.Menu (
    Menu,
    MenuItem,
    menu, action,
    startMenu,
    test
    ) where

import Prelude ()

import Yi.Core
import Yi.File
import Yi.MiniBuffer (spawnMinibufferE)

import Yi.Keymap.Emacs.Utils (askQuitEditor)

import Control.Monad (fmap)
import Data.List (map, intercalate)
import Data.Char (isUpper, toLower)
import Data.Maybe (mapMaybe)

-- | Menu
type Menu = [MenuItem]

-- | Menu utem
data MenuItem =
    MenuAction String (Char -> Keymap) |
    SubMenu String Menu

-- | Sub menu
menu :: String -> Menu -> MenuItem
menu = SubMenu

-- | Action menu
action :: (YiAction a x, Show x) => String -> a -> MenuItem
action title act = MenuAction title (\c -> char c ?>>! act)

-- | Fold menu item
foldItem :: (String -> (Char -> Keymap) -> a) -> (String -> [a] -> a) -> MenuItem -> a
foldItem mA sM (MenuAction title act) = mA title act
foldItem mA sM (SubMenu title sm) = sM title (map (foldItem mA sM) sm)

-- | Fold menu
foldMenu :: (String -> (Char -> Keymap) -> a) -> (String -> [a] -> a) -> Menu -> [a]
foldMenu mA sM = map (foldItem mA sM)

-- | Menu title to keymap
menuEvent :: String -> Maybe Char
menuEvent = fmap toLower . find isUpper

-- | Start menu action
startMenu :: Menu -> EditorM ()
startMenu = showMenu . foldMenu onItem onSub where
    showMenu ::[(String, Maybe Keymap)] -> EditorM ()
    showMenu is = do
        spawnMinibufferE (intercalate " " (map fst is)) (const (subMap is))
        return ()
    onItem title act = (title, fmap act (menuEvent title))
    onSub title is = (title, fmap (\c -> char c ?>>! showMenu is) (menuEvent title))
    subMap is = choice $ closeMenu : mapMaybe snd is where
        closeMenu = spec KEsc ?>>! closeBufferAndWindowE

-- | Test menu with only File - Quit item
test = [
    menu "File" [
        action "Quit" askQuitEditor,
        action "Save" fwriteE]]

module Yi.Keymap.Menu (
    Menu,
    MenuItem,
    MenuContext(..),
    menu, action, action_,
    startMenu
    ) where

import Prelude ()

import Yi.Core
import Yi.File
import Yi.MiniBuffer (spawnMinibufferE)

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

-- | Action on item
action_ :: (YiAction a x, Show x) => String -> a -> MenuItem
action_ title act = action title (const act)

-- | Action on item with context
action :: (YiAction a x, Show x) => String -> (MenuContext -> a) -> MenuItem
action title act = MenuAction title act' where
    act' ctx c = char c ?>>! (do
        withEditor closeBufferAndWindowE
        runAction $ makeAction (act ctx))

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
            showMenu is = void $ spawnMinibufferE menuItems (const (subMap is)) where
                menuItems = (intercalate " " (map fst is))
            onItem title act = (title, fmap (act ctx) (menuEvent title)) where
            onSub title is = (title, fmap subMenu (menuEvent title)) where
                subMenu c = char c ?>>! closeBufferAndWindowE >> showMenu is
            subMap is = choice $ closeMenu : mapMaybe snd is where
                closeMenu = spec KEsc ?>>! closeBufferAndWindowE

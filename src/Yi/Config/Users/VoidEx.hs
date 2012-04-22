module Yi.Config.Users.VoidEx (
    voidexConfig
    ) where

import Yi.Prelude
import Prelude ()

import Yi

import Yi.Keymap

import Yi.Keymap.Users.VoidEx (keymapSet)

-- | My config
voidexConfig :: Config
voidexConfig = defaultConfig {
    defaultKm = keymapSet,
    configUI = (configUI defaultConfig) {
        configWindowFill = ' ' }}

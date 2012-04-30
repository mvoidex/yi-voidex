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
--    initialActions = [BufferA setIndent],
    configUI = (configUI defaultConfig) {
        configWindowFill = ' ' }}

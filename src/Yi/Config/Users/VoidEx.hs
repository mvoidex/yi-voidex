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
    startActions = [BufferA setIndent],
    configUI = (configUI defaultConfig) {
        configWindowFill = ' ' }}

-- | Set default indentation size to 4
setIndent :: BufferM ()
setIndent = modifyMode $ modeIndentSettingsA ^: modifyIndent where
    modifyIndent is = is {
        tabSize = 4 }

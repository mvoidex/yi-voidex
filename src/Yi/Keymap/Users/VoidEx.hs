module Yi.Keymap.Users.VoidEx (
    keymapSet,
    keymap,

    input, move, select, other,
    moveKeys
    ) where

import Prelude (length, take, drop)

import Yi.Core

-- | Keymap set
keymapSet :: KeymapSet
keymapSet = modelessKeymapSet keymap

-- | Keymap
keymap :: Keymap
keymap = input <|> move <|> select <|> other

-- | Printable chars
input :: Keymap
input = do
    c <- printableChar
    write (withBuffer0 $ replaceSel [c])

-- | Move actions
move :: Keymap
move = choice [k ?>>! unsetMark >> a | (k, a) <- moveKeys]

-- | Select actions
select :: Keymap
select = choice [shift k ?>>! setMark False >> a | (k, a) <- moveKeys]

-- | Other commands
other :: Keymap
other = choice [
    spec KBS           ?>>! deleteSel bdeleteB,
    spec KDel          ?>>! deleteSel (deleteN 1),
    spec KEnter        ?>>! replaceSel "\n",
    spec KTab          ?>>! (replaceSel =<< tabB),
    ctrl (char 'q')    ?>>! askQuitEditor,
    ctrl (char 'f')    ?>>! isearchKeymap Forward,
    ctrl (char 'x')    ?>>! cut,
    ctrl (char 'c')    ?>>! copy,
    ctrl (char 'v')    ?>>! parse,
    ctrl (spec KIns)   ?>>! copy,
    shift (spec KIns)  ?>>! parse,
    ctrl (char 'z')    ?>>! undoB,
    ctrl (char 'y')    ?>>! redoB,
    ctrl (char 's')    ?>>! fwriteE,
    ctrl (char 'o')    ?>>! findFile]

-- | Move keys
moveKeys :: [(Event, BufferM ())]
moveKeys = [
    (spec KHome             , maybeMoveB Line Backward),
    (spec KEnd              , maybeMoveB Line Forward),
    (super (spec KRight)    , maybeMoveB Line Forward),
    (super (spec KLeft)     , maybeMoveB Line Backward),
    (ctrl (spec KHome)      , maybeMoveB Document Backward),
    (ctrl (spec KEnd)       , maybeMoveB Document Forward),
    (super (spec KUp)       , maybeMoveB Document Backward),
    (super (spec KDown)     , maybeMoveB Document Forward),
    (spec KPageUp           , maybeMoveB Document Backward),
    (spec KPageDown         , maybeMoveB Document Forward),
    (ctrl (spec KRight)     , maybeMoveB unitWord Forward),
    (ctrl (spec KLeft)      , maybeMoveB unitWord Backward),
    (ctrl (spec KUp)        , scrollB (-1)),
    (ctrl (spec KDown)      , scrollB 1),
    (spec KUp               , moveB VLine Backward),
    (spec KDown             , moveB VLine Forward),
    (spec KRight            , moveB Character Forward),
    (spec KLeft             , moveB Characted Backward)]

-- | Cut action
cut :: EditorM ()
cut = copy >> del

-- | Delete action
del :: EditorM ()
del = do
    asRect <- withBuffer0 $ getA rectangleSelectionA
    if asRect
        then killRectangle
        else withBuffer0 $ deleteRegionB =<< getSelectRegionB

-- | Copy action
copy = (setRegE <<=) $ withBuffer $ do
    asRect <- getA rectangleSelectionA
    if not asRect
        then readRegionB =<< getSelectRegionB
        else do
            (reg, l, r) <- getRectangle
            unlines' <$> fmap (take (r - l) . drop l) <$> lines' <$> readRegionB reg

-- | Paste action
paste :: EditorM ()
paste = do
    asRect <- withBuffer0 $ getA rectangleSelectionA
    if asRect
        then yankRectangle
        else withBuffer0 . replaceSel =<< getRegE

-- | Replace selection with string
replaceSel :: String -> BufferM ()
replaceSel s = do
    hasSel <- getA highlightSelectionA
    if hasSel
        then getSelectRegionB >>= flip replaceRegionB s
        else do
            when (length s == 1) (adjBlock 1)
            insertN s

-- | Delete selection
deleteSel :: BufferM () -> YiM ()
deleteSel act = do
    hasSel <- withBuffer $ getA highlightSelectionA
    if hasSel
        then withEditor del
        else withBuffer (abjBlock (-1) >> act)

-- | Set rectangle selection mode
setMark :: Bool -> BufferM ()
setMark b = do
    isSet <- getA highlightSelectionA
    putA rectangleSelectionA b
    when (not isSet) $ do
        putA highlightSelectionA True
        pointB >>= setSelectionMarkPointB

-- | Drop rectangle selection mode
unsetMark :: BufferM ()
unserMark = putA highlightSelectionA False

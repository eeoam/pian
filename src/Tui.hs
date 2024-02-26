{-# LANGUAGE OverloadedStrings #-}

module Tui where 

import System.Directory

import Brick.Main
    ( defaultMain
    , halt
    , App(..)
    , showFirstCursor
    )

import Brick.AttrMap
    ( attrMap
    )

import Brick.Types
    ( Widget(..)
    , BrickEvent(..)
    , EventM(..)
    )

import Brick.Widgets.Core
    ( strWrap
    )

import Graphics.Vty.Input.Events
    ( Event(..) -- EvKey
    , Key(..) -- KChar
    )

import Graphics.Vty.Attributes
    ( defAttr
    )

import Cursor.TextField
    ( TextFieldCursor
    , makeTextFieldCursor
    )

import Data.Text.IO qualified as TextIO

import Path {- from package path -}
    ( fromAbsFile
    )

import Path.IO {- from path-io -}
    ( resolveFile'
    , forgivingAbsence
    )

import Data.Maybe
    ( fromMaybe
    )

tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

data TuiState 
    = TuiState 
        { stateCursor :: TextFieldCursor }
    deriving stock (Show, Eq)

data ResourceName
    = ResourceName
    deriving stock (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp = 
    App 
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure ()
        , appAttrMap = const $ attrMap defAttr []
        }

buildInitialState :: IO TuiState
buildInitialState = do
    path <- resolveFile' "example.txt"
    maybeContents <- forgivingAbsence $ TextIO.readFile (fromAbsFile path) -- lose the readFile per Snoyman's warning
    let contents = fromMaybe "" maybeContents
    let tfc = makeTextFieldCursor contents
    pure TuiState { stateCursor = tfc }

drawTui :: TuiState -> [ Widget ResourceName ]
drawTui _ts = [ strWrap (show _ts) ]

handleTuiEvent :: BrickEvent n e -> EventM n TuiState ()
handleTuiEvent (VtyEvent (EvKey (KChar 'q') [])) = halt
handleTuiEvent _ = pure ()

-- stack install --file-watch
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

--import Brick.Widgets.Core

import Graphics.Vty.Input.Events
    ( Event(..) -- EvKey
    , Key(..) -- KChar
    )

import Graphics.Vty.Attributes
    ( defAttr
    )

tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

data TuiState 
    = TuiState
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
buildInitialState = pure TuiState

drawTui :: TuiState -> [ Widget ResourceName ]
drawTui _ts = []

handleTuiEvent :: BrickEvent n e -> EventM n TuiState ()
handleTuiEvent (VtyEvent (EvKey (KChar 'q') [])) = halt
handleTuiEvent _ = pure ()
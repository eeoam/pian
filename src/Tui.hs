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
    , attrName
    )

import Brick.Types
    ( Widget(..)
    , BrickEvent(..)
    , EventM(..)
    )

import Brick.Widgets.Core
    ( strWrap
    , padAll
    , padLeftRight
    , padRight
    , Padding (..) -- Max
    , withAttr
    , forceAttr
    , fill
    )

import Brick.Widgets.Border
    ( border
    )

import Brick.Widgets.Center
     ( centerLayer
     )

import Brick.Util
    ( fg
    )

import Graphics.Vty.Input.Events
    ( Event(..) -- EvKey
    , Key(..) -- KChar
    )

import Graphics.Vty.Attributes
    ( defAttr
    , red
    , blue
    )

import Cursor.TextField
    ( TextFieldCursor
    , makeTextFieldCursor
    , textFieldCursorInsertChar
    , textFieldCursorSelectNextChar
    , textFieldCursorSelectPrevChar
    , textFieldCursorSelectPrevLine
    , textFieldCursorSelectNextLine
    , textFieldCursorRemove
    , textFieldCursorDelete
    , textFieldCursorInsertNewline
    , rebuildTextFieldCursor
    )

import Cursor.Types ( dullMDelete )

import Cursor.Brick.TextField

import Data.Text ( Text )
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

import Text.Show.Pretty
    ( ppShow
    )

import Lens.Micro
    ( (^.)
    )

import Lens.Micro.TH 
    ( makeLenses
    )

import Lens.Micro.Mtl (use, (<~), (.=))

import System.Environment 
    ( getArgs
    )

import System.Exit 
    ( die
    )

import Control.Monad ( unless )

data TuiState 
    = TuiState 
        { _stateCursor :: TextFieldCursor }
    deriving stock (Show, Eq)
makeLenses ''TuiState

tui :: IO ()
tui = do
    args <- getArgs
    case args of
        [] -> die "No argument to choose file to edit."
        (fp : _) -> do 
            path <- resolveFile' fp
            maybeContents <- forgivingAbsence $ TextIO.readFile (fromAbsFile path) -- lose the readFile per Snoyman's warning
            let contents = fromMaybe "" maybeContents
            initialState <- buildInitialState contents
            endState <- defaultMain tuiApp initialState
            let contents' = rebuildTextFieldCursor (_stateCursor endState)
            unless (contents == contents') $
                TextIO.writeFile (fromAbsFile path) contents' -- lose the writeFile per Snoyman's warning



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
        , appAttrMap = const $ attrMap defAttr [ (textColor, fg red), (bg, fg blue) ]
        }

textColor = attrName "text"
bg = attrName "bg"

buildInitialState :: Text -> IO TuiState
buildInitialState contents = do
    let tfc = makeTextFieldCursor contents
    pure TuiState { _stateCursor = tfc }

drawTui :: TuiState -> [ Widget ResourceName ]
drawTui ts = [ forceAttr textColor 
               $ centerLayer 
               $ border 
               $ padLeftRight 1
               $ selectedTextFieldCursorWidget ResourceName (_stateCursor ts) 
            ,  forceAttr bg $ fill '@'
            ]

handleTuiEvent :: BrickEvent n e -> EventM n TuiState ()
handleTuiEvent event =
    case event of 
        VtyEvent vtyEvent ->
            let mDo :: (TextFieldCursor -> Maybe TextFieldCursor) -> EventM n TuiState ()
                mDo func = do
                    tfc <- use stateCursor
                    stateCursor .= (fromMaybe tfc $ func tfc)
            in case vtyEvent of
                EvKey (KChar c) [] -> mDo $ textFieldCursorInsertChar c . Just
                EvKey KRight [] -> mDo $ textFieldCursorSelectNextChar
                EvKey KLeft [] -> mDo $ textFieldCursorSelectPrevChar
                EvKey KUp [] -> mDo $ textFieldCursorSelectPrevLine
                EvKey KDown [] -> mDo $ textFieldCursorSelectNextLine
                EvKey KBS [] -> mDo $ dullMDelete . textFieldCursorRemove
                EvKey KDel [] -> mDo $ dullMDelete . textFieldCursorDelete
                EvKey KEnter [] -> mDo $ Just . textFieldCursorInsertNewline . Just
                EvKey KEsc []  -> halt
                _ -> pure ()
        _ -> pure ()

-- stack install --file-watch
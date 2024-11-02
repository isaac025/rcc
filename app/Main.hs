{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Main (App (..), defaultMain, halt, showCursorNamed)
import Brick.Types (BrickEvent (..), EventM, Widget)
import Brick.Util (bg, on)
import Brick.Widgets.Border (borderAttr, borderWithLabel)
import Brick.Widgets.Border.Style (borderStyleFromChar)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core (
    emptyWidget,
    padAll,
    str,
    withAttr,
    withBorderStyle,
 )
import Brick.Widgets.Dialog (Dialog, renderDialog)
import Brick.Widgets.Edit (
    Editor,
    editAttr,
    editFocusedAttr,
    editor,
    getEditContents,
    handleEditorEvent,
    renderEditor,
 )
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Graphics.Vty (brightWhite, defAttr, rgbColor)
import Graphics.Vty.Input.Events (Event (..), Key (..), Modifier (..))
import Language (Program)
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, zoom)
import Lens.Micro.TH (makeLenses)
import Parser
import Semantics
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

data SaveOpt = Yes | No

data NewFileOpt = Save | Cancel

data Name
    = TextEditor
    | TextEditorLines
    | SavePrompt
    | YesButton
    | NoButton
    | NewFilePrompt
    | SaveButton
    | CancelButton
    deriving (Eq, Ord, Show)

data St = St
    { _textEditor :: Editor String Name
    , _mfile :: Maybe FilePath
    , _savePrompt :: Maybe (Dialog SaveOpt Name)
    , _newFilePrompt :: Maybe (Dialog NewFileOpt Name)
    }
makeLenses ''St

initState :: Maybe FilePath -> String -> St
initState f c = St (editor TextEditor Nothing c) f Nothing Nothing

draw :: St -> [Widget Name]
draw st = drawSave st : [withBorderStyle (borderStyleFromChar ' ') $ withAttr borderLabelAttr $ borderWithLabel (str "Turbo Calculus") e]
  where
    e = renderEditor (str . unlines) True (st ^. textEditor)

drawSave :: St -> Widget Name
drawSave st =
    case st ^. savePrompt of
        Nothing -> emptyWidget
        Just sd -> renderDialog sd $ hCenter $ padAll 1 $ str "Really want to save?"

compile :: FilePath -> IO ()
compile f = do
    contents <- readFile f
    case parser contents :: Either String Program of
        Left err -> putStrLn err >> exitFailure
        Right ast -> do
            stack <- initStack
            result <- semanticsEval stack (analyzeProgram ast)
            print result
            exitSuccess

saveFile :: EventM Name St ()
saveFile = do
    contents <- unlines . getEditContents <$> use textEditor
    file <- use mfile
    liftIO $ writeFile (fromMaybe "New.rc" file) contents

appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent (VtyEvent (EvKey (KChar 'x') [MCtrl])) = halt
appEvent (VtyEvent (EvKey (KChar 's') [MCtrl])) = saveFile
appEvent (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt
appEvent ev = zoom textEditor $ handleEditorEvent ev

borderLabelAttr :: AttrName
borderLabelAttr = attrName "borderLabel"

appMap :: AttrMap
appMap =
    attrMap
        defAttr
        [ (editAttr, brightWhite `on` rgbColor (0 :: Integer) 0 255)
        , (editFocusedAttr, brightWhite `on` rgbColor (0 :: Integer) 0 255)
        , (borderAttr, bg $ rgbColor (0 :: Integer) 0 255)
        , (borderLabelAttr, rgbColor (0 :: Integer) 0 255 `on` brightWhite)
        ]

app :: App St e Name
app =
    App
        { appDraw = draw
        , appChooseCursor = const $ showCursorNamed TextEditor
        , appHandleEvent = appEvent
        , appStartEvent = pure ()
        , appAttrMap = const appMap
        }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> do
            contents <- readFile f
            void $ defaultMain app (initState (Just f) contents)
        [] -> void $ defaultMain app (initState Nothing "")
        _ -> putStrLn "Usage: rcc [FILE]"

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.DeleteFileDialog where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (
    BrickEvent (..),
    Widget,
    modify,
 )
import qualified Brick.Types as T
import Brick.Util (bg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (
    padAll,
    str,
    vBox,
 )
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V

data Choice = DeleteFile | Cancel
    deriving (Show)

data Name
    = DeleteFileButton
    | CancelButton
    deriving (Show, Eq, Ord)

drawUI :: FilePath -> D.Dialog Choice Name -> [Widget Name]
drawUI f d = [ui]
  where
    body =
        vBox
            [ str "Are you sure you want to delete:"
            , str f
            ]
    ui = D.renderDialog d $ C.hCenter $ padAll 1 body

appEvent :: BrickEvent Name e -> T.EventM Name (D.Dialog Choice Name) ()
appEvent (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt
        V.EvKey V.KEnter [] -> M.halt
        V.EvKey (V.KChar 'h') [] -> modify $ D.setDialogFocus DeleteFileButton
        V.EvKey (V.KChar 'l') [] -> modify $ D.setDialogFocus CancelButton
        _ -> D.handleDialogEvent ev
appEvent _ = return ()

initialState :: D.Dialog Choice Name
initialState = D.dialog (Just $ str " Confirm ") (Just (CancelButton, choices)) 50
  where
    choices =
        [ ("Delete", DeleteFileButton, DeleteFile)
        , ("Cancel", CancelButton, Cancel)
        ]

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (D.buttonSelectedAttr, V.black `on` V.white)
        ]

theApp :: FilePath -> M.App (D.Dialog Choice Name) e Name
theApp f =
    M.App
        { M.appDraw = drawUI f
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return ()
        , M.appAttrMap = const theMap
        }

showDeleteFileDialog :: FilePath -> IO (Maybe (Name, Choice))
showDeleteFileDialog f = do
    d <- M.defaultMain (theApp f) initialState
    putStrLn $ "You chose: " <> show (D.dialogSelection d)
    return $ D.dialogSelection d

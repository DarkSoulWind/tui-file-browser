{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.RenameFileDialog where

import qualified Data.Text as T
import Lens.Micro ((^.))
import Lens.Micro.TH
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)

import Brick
import Brick.Focus (
    focusGetCurrent,
    focusRingCursor,
 )
import Brick.Forms (
    Form,
    allFieldsValid,
    checkboxField,
    editPasswordField,
    editShowableField,
    editTextField,
    focusedFormInputAttr,
    formFocus,
    formState,
    handleFormEvent,
    invalidFields,
    invalidFormInputAttr,
    newForm,
    radioField,
    renderForm,
    setFieldValid,
    (@@=),
 )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Control.Monad (when)
import Control.Monad.State (MonadIO (liftIO))
import System.Directory

data Name
    = NameField
    deriving (Eq, Ord, Show)

newtype DialogState = DialogState
    { _name :: T.Text
    }
    deriving (Show)

makeLenses ''DialogState

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: DialogState -> Form DialogState e Name
mkForm =
    let label s w =
            padLeftRight 1 $
                padBottom (Pad 1) $
                    vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
     in newForm
            [ label "Name"
                @@= editTextField name NameField (Just 1)
            ]

theMap :: AttrMap
theMap =
    attrMap
        V.defAttr
        [ (E.editAttr, V.white `on` V.black)
        , (E.editFocusedAttr, V.black `on` V.yellow)
        , (invalidFormInputAttr, V.white `on` V.red)
        , (focusedFormInputAttr, V.black `on` V.yellow)
        ]

drawUI :: FilePath -> Form DialogState e Name -> [Widget Name]
drawUI file f = [C.vCenter $ C.hCenter form]
  where
    form = B.borderWithLabel (str $ " Rename " ++ file ++ " ") $ padTop (Pad 1) $ hLimit 50 $ renderForm f

app :: FilePath -> App (Form DialogState e Name) e Name
app file =
    App
        { appDraw = drawUI file
        , appHandleEvent = \ev -> do
            f <- gets formFocus
            case ev of
                VtyEvent (V.EvResize{}) -> return ()
                VtyEvent (V.EvKey V.KEsc []) -> halt
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter []) -> do
                    st <- get
                    when (allFieldsValid st) halt
                _ -> do
                    handleFormEvent ev

                    st <- gets formState
                    let nameField = T.unpack $ st ^. name
                    modify $ setFieldValid (not . null $ nameField) NameField
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

showRenameFileDialog :: FilePath -> IO DialogState
showRenameFileDialog file = do
    let buildVty = do
            v <- mkVty V.defaultConfig
            V.setMode (V.outputIface v) V.Mouse True
            return v

        initialUserInfo =
            DialogState
                { _name = T.pack file
                }
        f =
            if null $ T.unpack (initialUserInfo ^. name)
                then
                    setFieldValid False NameField $
                        mkForm initialUserInfo
                else mkForm initialUserInfo

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing (app file) f

    return $ formState f'

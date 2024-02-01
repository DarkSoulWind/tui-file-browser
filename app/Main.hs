{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (handleDelete, main) where

import Control.Monad.State (MonadIO (liftIO), void)
import Lens.Micro.Mtl
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.Directory (canonicalizePath, doesFileExist, listDirectory, removeFile, renameFile, setCurrentDirectory)
import System.FilePath (takeDirectory)
import qualified UI.DeleteFileDialog as DFD
import qualified UI.RenameFileDialog as RFD

data AppState = AppState
  { _cwd :: FilePath,
    _dirList :: L.List () Listing,
    _exitStatus :: ExitStatus
  }
  deriving (Show)

data Listing = Listing
  { listingType :: ListingType,
    filePath :: FilePath
  }
  deriving (Show, Eq)

data ListingType = File | Directory deriving (Show, Eq)

data ExitStatus = None | Rename | Delete deriving (Show, Eq)

makeLenses ''AppState

drawUI :: AppState -> [Widget ()]
drawUI st = [ui]
  where
    l = st ^. dirList
    total = Vec.length $ l ^. L.listElementsL
    box =
      if total == 0
        then str "Empty directory"
        else C.hCenter $ L.renderList listDrawElement True l
    ui =
      vBox
        [ str $ "Current directory: " ++ st ^. cwd,
          box
        ]

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey V.KEnter [] -> enterDirectory
    V.EvKey (V.KChar 'l') [] -> enterDirectory
    V.EvKey (V.KChar 'h') [] -> exitDirectory
    V.EvKey (V.KChar 'q') [] -> halt
    V.EvKey (V.KChar 'r') [] -> modify (setExitStatus Rename) >> halt
    V.EvKey (V.KChar 'd') [] -> modify (setExitStatus Delete) >> halt
    ev -> zoom dirList $ L.handleListEventVi L.handleListEvent ev
appEvent _ = return ()

exitDirectory :: T.EventM () AppState ()
exitDirectory = do
  st <- get
  let currentPath = st ^. cwd
  let newPath = takeDirectory currentPath
  liftIO $ setCurrentDirectory newPath
  newState <- liftIO $ initState newPath
  modify $ const newState

enterDirectory :: T.EventM () AppState ()
enterDirectory = do
  st <- get
  theDirList <- use dirList
  let currentPath = st ^. cwd
  let sel' = L.listSelectedElement theDirList
  case sel' of
    Nothing -> return ()
    Just (_, sel) -> do
      case listingType sel of
        File -> return ()
        Directory -> do
          let listingPath = filePath sel
          let newPath = currentPath ++ "/" ++ listingPath
          liftIO $ setCurrentDirectory newPath
          newState <- liftIO $ initState newPath
          modify $ const newState

setExitStatus :: ExitStatus -> AppState -> AppState
setExitStatus s st = st & exitStatus .~ s

listDrawElement :: Bool -> Listing -> Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then
            if listingType a == File
              then withAttr customAttr $ str s
              else withAttr dirSelectedAttr $ str s
          else colour $ str s
      colour w =
        if listingType a == File
          then w
          else withAttr dirAttr w
   in selStr $ filePath a

initDirectoryList :: FilePath -> IO (L.List () Listing)
initDirectoryList p = do
  directoryListings <- listDirectory p
  isFiles <- mapM doesFileExist directoryListings
  let f x = if x then File else Directory
  let fileTypes = map f isFiles
  let theListings = zipWith Listing fileTypes directoryListings
  return $ L.list () (Vec.fromList theListings) 2

initState :: FilePath -> IO AppState
initState p = do
  theDirectoryList <- initDirectoryList p
  absPath <- canonicalizePath p
  return $ AppState absPath theDirectoryList None

customAttr, dirAttr, dirSelectedAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"
dirAttr = A.attrName "directory"
dirSelectedAttr = A.attrName "directory-selected"

customBlue :: V.Color
customBlue = V.linearColor 4 174 245

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, V.blue `on` V.white),
      (customAttr, fg V.black),
      (dirAttr, fg customBlue `V.withStyle` V.bold),
      (dirSelectedAttr, V.black `on` customBlue `V.withStyle` V.bold)
    ]

theApp :: M.App AppState e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

handleRename :: AppState -> IO AppState
handleRename st = do
  let theList = st ^. dirList
  let sel = L.listSelectedElement theList
  case sel of
    Nothing -> return st
    (Just (_, selListing)) -> do
      if listingType selListing == Directory
        then return ()
        else do
          let oldPath = filePath selListing
          renameState <- RFD.showRenameFileDialog oldPath
          let newPath = renameState ^. RFD.name
          liftIO $ renameFile oldPath $ T.unpack newPath
      return st

handleDelete :: AppState -> IO AppState
handleDelete st =
  do
    let theList = st ^. dirList
    let sel = L.listSelectedElement theList
    case sel of
      Nothing -> do
        return ()
      (Just (_, selListing)) -> do
        if listingType selListing == Directory
          then do
            print $ show selListing
            return ()
          else do
            let oldPath = filePath selListing
            choice <- DFD.showDeleteFileDialog oldPath
            case choice of
              Nothing -> return ()
              (Just (_, action)) -> case action of
                DFD.Cancel -> return ()
                DFD.DeleteFile -> do
                  removeFile oldPath
    return st

runApp :: FilePath -> IO ()
runApp file = do
  st <- initState file
  newState <- M.defaultMain theApp st
  case newState ^. exitStatus of
    None -> return ()
    Rename -> do
      void $ handleRename newState
      runApp $ newState ^. cwd
    Delete ->
      do
        void $ handleDelete newState
        runApp $ newState ^. cwd

main :: IO ()
main = do
  runApp "."

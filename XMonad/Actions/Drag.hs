{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module XMonad.Actions.Drag
       ( -- * Usage
         -- $usage
         DragNDropMode(..)
       , dragOrDrop
       ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.ExtensibleState()
import qualified XMonad.Util.ExtensibleState as S

import Control.Monad.State

{-|$usage

-}

{- TODO
   - allow for reconfiguring XMonad on dragstart (border color, etc.)
-}

data DragNDropMode = Swap | Drop

data DragState = DragState
                 { draggingWindow :: Maybe Window
                 } deriving Typeable

instance Default DragState where
  def = DragState { draggingWindow = Nothing }

instance ExtensionClass DragState where
  initialValue = def

-- Polymorphic type alias for StackSets
type WindowMover i l a sid sd = a -> Maybe a -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd
-- Monomorphic type alias for WindowSets
type WindowSetMover = WindowMover WorkspaceId (Layout Window) Window ScreenId ScreenDetail

-- Map over all windows in the StackSet and apply the given Monad.State action
-- Note: the supplied action k needs to be careful to not violate StackSet invariants
-- (uniqueness!) because this doesn't do it by itself. It just allows a stateful action
-- to iterate (once) over all members in the StackSet
unsafeStatefulWindowMap :: forall i l a sid sd s. (Eq a, Eq sid) =>
    (a -> State s a) -> s -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd
unsafeStatefulWindowMap k s0 ss = flip evalState s0 $ do
  let computeScreen :: W.Screen i l a sid sd -> State s (W.Screen i l a sid sd)
      computeScreen scr = do ws <- computeWs (W.workspace scr)
                             return scr { W.workspace = ws }
      computeWs :: W.Workspace i l a -> State s (W.Workspace i l a)
      computeWs ws@W.Workspace { W.stack = Nothing } = return ws
      computeWs ws@W.Workspace { W.stack = Just stack } = do
          focus' <- k (W.focus stack)
          up' <- mapM k (W.up stack)
          down' <- mapM k (W.down stack)
          return ws { W.stack = Just stack { W.focus = focus'
                                           , W.up = up'
                                           , W.down = down' } }
  cur' <- computeScreen (W.current ss)
  vis' <- mapM computeScreen (W.visible ss)
  hid' <- mapM computeWs (W.hidden ss)
  return ss { W.current = cur', W.visible = vis', W.hidden = hid' }

data SwapState = TakeSource | TakeTarget | NothingYet

swapWindows, dropWindow :: forall i l a sid sd. (Eq a, Eq sid) => WindowMover i l a sid sd
swapWindows s (Just t) = 
  unsafeStatefulWindowMap computeWindow NothingYet
  where computeWindow :: a -> State SwapState a
        computeWindow w = do swapState <- get
                             case swapState of
                              TakeSource -> if w == t then return s else return w
                              TakeTarget -> if w == s then return t else return w
                              NothingYet | w == s -> put TakeSource >> return t
                                         | w == t -> put TakeTarget >> return s
                                         | otherwise -> return w
  
swapWindows source Nothing = dropWindow source Nothing -- if there's nothing to swap to, we drop
dropWindow source _target = W.insertUp source . W.delete' source

changeWindowBorder :: String -> Window -> X ()
changeWindowBorder p w =
  withDisplay $ \dpy -> io $ do
    color <- initColor dpy p
    case color of
     Just c -> setWindowBorder dpy w c
     Nothing -> return ()

startDrag :: X ()
startDrag = withFocused $ \w -> do
  S.put . DragState . Just $ w
  changeWindowBorder "green" w

finishDrag :: WindowSetMover -> Window -> X ()
finishDrag f sourceWindow = withFocusedMaybe $ windows . f sourceWindow 

-- Analogue to withFocused, but notifies continuation of absence of focused window
withFocusedMaybe :: (Maybe Window -> X ()) -> X ()
withFocusedMaybe k = withWindowSet $ \ws -> k (W.peek ws)
  
doDrop :: DragNDropMode -> Window -> X ()
doDrop Swap w = finishDrag swapWindows w
doDrop Drop w = finishDrag dropWindow w

dragOrDrop :: DragNDropMode -> X ()
dragOrDrop mode = do
  maybeSourceWindow <- S.get
  case maybeSourceWindow of
   DragState Nothing -> startDrag
   DragState (Just sourceWindow) ->
     doDrop mode sourceWindow >> S.put (def :: DragState)

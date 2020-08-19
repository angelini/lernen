module View
  ( tui,
  )
where

import qualified Brick.AttrMap as BAttr
import Brick.Main
import qualified Brick.Types as BT
import qualified Brick.Widgets.Core as BC
import qualified Data.Text as T
import qualified Graphics.Vty.Input.Events as Vty
import Lib

data ValidationState = Valid | Invalid | InProgress deriving (Show, Eq)

data TuiState = TuiState
  { tsPair :: TextPair,
    tsShuffled :: ShuffledWords,
    tsInput :: [Char],
    tsKeys :: [Key],
    tsValidation :: ValidationState
  }
  deriving (Show, Eq)

buildInitialState :: TextPair -> IO TuiState
buildInitialState pair = do
  shuffled <- shuffle (learningWords pair)
  return $ TuiState pair shuffled [] [] InProgress

addInput :: TuiState -> Char -> TuiState
addInput
  TuiState
    { tsPair = pair,
      tsShuffled = shuffled,
      tsInput = input,
      tsKeys = keys
    }
  char =
    TuiState pair shuffled (input ++ [char]) keys InProgress

removeLastInput :: TuiState -> TuiState
removeLastInput
  TuiState
    { tsPair = pair,
      tsShuffled = shuffled,
      tsInput = input,
      tsKeys = keys
    } =
    TuiState pair shuffled (init input) keys InProgress

addKey :: TuiState -> TuiState
addKey
  TuiState
    { tsPair = pair,
      tsShuffled = shuffled,
      tsInput = input,
      tsKeys = prevKeys
    } =
  let keys = prevKeys ++ [Key $ read input]
      validation
        | validate shuffled keys = Valid
        | size shuffled == length keys = Invalid
        | otherwise = InProgress
   in TuiState pair shuffled [] keys validation

data ResourceName
  = ResourceName
  deriving (Show, Eq, Ord)

drawTui :: TuiState -> [BT.Widget ResourceName]
drawTui ts =
  let known = knownWords $ tsPair ts
      shuffled = tsShuffled ts
      ordKeys = orderedKeys shuffled
      left =
        BC.vBox
          [ BC.txt "English:",
            BC.txt " ",
            BC.txt "German:"
          ]
      right =
        BC.vBox
          [ BC.txt $ renderCorrect known,
            BC.txt " ",
            BC.txt $ renderShuffled shuffled ordKeys,
            BC.txt $ renderKeys shuffled ordKeys
          ]
   in [ BC.vBox
          [ BC.hBox [BC.padRight (BT.Pad 10) left, right],
            BC.txt " ",
            BC.txt $ T.pack "> " <> T.pack (tsInput ts),
            BC.txt " ",
            BC.txt $ renderShuffled shuffled (tsKeys ts),
            BC.txt " ",
            BC.txt $ T.pack $ show (tsValidation ts)
          ]
      ]

handleTuiEvent :: TuiState -> BT.BrickEvent n e -> BT.EventM n (BT.Next TuiState)
handleTuiEvent s e =
  case e of
    BT.VtyEvent vtye ->
      case vtye of
        Vty.EvKey (Vty.KChar 'q') [] -> halt s
        Vty.EvKey (Vty.KChar input) [] -> continue $ addInput s input
        Vty.EvKey Vty.KEnter [] -> continue $ addKey s
        Vty.EvKey Vty.KDel [] -> continue $ removeLastInput s
        Vty.EvKey Vty.KBackTab [] -> continue $ removeLastInput s
        _ -> continue s
    _ -> continue s

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = pure,
      appAttrMap = const $ BAttr.attrMap mempty []
    }

tui :: TextPair -> IO ()
tui textPair = do
  initialState <- buildInitialState textPair
  endState <- defaultMain tuiApp initialState
  print endState

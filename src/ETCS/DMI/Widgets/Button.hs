{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

module ETCS.DMI.Widgets.Button (
    Button, ButtonType (..), ButtonLabelTitle (..), mkButton, mkEmptyButton
) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Writer
import           Data.Text                  (Text)
import           ETCS.DMI.Helpers
import           ETCS.DMI.Widgets.Sprites
import           GHCJS.DOM.Element          (setAttribute, setClassName,
                                             setInnerHTML)
import           GHCJS.DOM.HTMLElement      (setTitle)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           GHCJS.DOM.Types            (IsHTMLElement, castToElement,
                                             castToHTMLElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks

data ButtonLabelTitle
  = TextLabel Text
  | InnerHtmlLabel Text Text
  | SpriteLabel Text Text

data ButtonType = UpButton | DownButton | DelayButton | EmptyButton
  deriving (Eq, Ord, Show, Enum, Bounded)

data ButtonState = ButtonDisabled | ButtonEnabled | ButtonPressed
  deriving (Eq, Ord, Show, Enum, Bounded)

mkButton :: ButtonType -> Behavior ButtonLabelTitle -> Behavior Bool -> e ->
            WidgetInput (Button e)
mkButton = MkButton

mkEmptyButton :: e -> WidgetInput (Button e)
mkEmptyButton = mkButton EmptyButton (pure $ TextLabel mempty) (pure False)

data Button e = Button { buttonEvent :: Event e }

instance IsEventWidget (Button e) where
  type WidgetEventType (Button e) = e
  widgetEvent = buttonEvent

instance IsWidget (Button e) where
  data WidgetInput (Button e) = MkButton {
    _buttonType :: ButtonType,
    _buttonLabel :: Behavior ButtonLabelTitle,  -- Maybe (Behavior Text),
    _buttonEnabled :: Behavior Bool,
    _buttonValue :: e
    }
  mkWidgetInstance parent i = do
    doc        <- _getOwnerDocument parent
    button     <- _createDivElement doc


    case _buttonType i of
      EmptyButton -> do
        empty_div  <- _createDivElement doc
        setClassName empty_div "EmptyButton"
        void $ appendChild parent . pure $ castToHTMLElement empty_div
        return (Button never, castToElement button)
      button_type -> do
        inner_div  <- _createDivElement doc
        void $ appendChild button (pure inner_div)
        let bLabel = mkLabel inner_div <$> _buttonLabel i

        lift $ valueBLater bLabel >>= liftIOLater
        lift $ changes bLabel >>= reactimate'

        -- MVar for thread control
        mv_thread <- liftIO newEmptyMVar
        let killThreadIfExists :: IO ()
            killThreadIfExists = tryTakeMVar mv_thread >>= maybe (return ()) killThread
        registerCleanupIO killThreadIfExists


        -- construct and react on button pressed behavior
        (eButtonPressed, fireButtonPressed) <- lift newEvent
        bButtonPressed <- lift $ stepper False eButtonPressed
        let bButtonState = buttonState <$> _buttonEnabled i <*> bButtonPressed
        let stateHandler = setAttribute button "data-state" . show
        lift $ valueBLater bButtonState >>= liftIOLater . stateHandler
        lift $ changes bButtonState >>= reactimate' . fmap (fmap stateHandler)
        let bButtonNotDisabled = (/= ButtonDisabled) <$> bButtonState

        -- internal click event
        (eButtonClick, fireButton') <- lift newEvent
        let fireButtonEventValue = fireButton' (_buttonValue i)

        -- mouse out
        eMouseOut' <- registerMouseOut button
        let eMouseOut = whenE (bOr bButtonNotDisabled bButtonPressed) eMouseOut'

        let mouseOutHandler () = do
              () <$ killThreadIfExists
              fireButtonPressed False
        lift . reactimate $ fmap mouseOutHandler eMouseOut

        -- mouse down
        eMouseDown' <- registerMouseDown button
        let eMouseDown = whenE bButtonNotDisabled eMouseDown'

        let mouseDownHandler () = case button_type of
              EmptyButton -> return ()
              UpButton -> fireButtonPressed True
              DownButton -> do
                fireButtonEventValue
                fireButtonPressed True
                forkIO (repeatAction mv_thread fireButtonEventValue) >>=
                  putMVar mv_thread
              DelayButton ->
                forkIO (animateDelay mv_thread fireButtonPressed) >>=
                  putMVar mv_thread
        lift . reactimate $ fmap mouseDownHandler eMouseDown


        -- mouse up
        eMouseUp' <- registerMouseUp button
        let eMouseUp =  whenE bButtonNotDisabled eMouseUp'

        let mouseUpHandler () = do
              fireButtonPressed False
              -- FIXME: Should not fire on: down - move out - move in - up
              case button_type of
                EmptyButton -> return ()
                UpButton -> fireButtonEventValue
                DownButton -> () <$ killThreadIfExists
                DelayButton ->
                  tryTakeMVar mv_thread >>= maybe fireButtonEventValue killThread
        lift . reactimate $ fmap mouseUpHandler eMouseUp

        void $ appendChild parent . pure $ castToHTMLElement button
        return (Button eButtonClick, castToElement button)


mkLabel :: (MonadIO m, IsHTMLElement parent) => parent -> ButtonLabelTitle -> m ()
mkLabel parent (TextLabel l) = do
  spanElem <- _getOwnerDocument parent >>= _createSpanElement
  setTextContent spanElem . pure $ l
  setTitle parent l
  deleteChildNodes parent
  void $ appendChild parent (pure spanElem)
mkLabel parent (InnerHtmlLabel h t) = do
  setInnerHTML parent (pure h)
  setTitle parent t
mkLabel parent (SpriteLabel sid t) = do
  sprite <- mkSpriteElement parent $ pure sid
  deleteChildNodes parent
  void $ appendChild parent . pure $ sprite
  setTitle parent t


buttonState :: Bool -> Bool -> ButtonState
buttonState False     _ = ButtonDisabled
buttonState True  False = ButtonEnabled
buttonState True  True  = ButtonPressed

-- used by down down
repeatAction :: MVar a -> IO () -> IO ()
repeatAction mv_thread fireButtonEventValue = do
  threadDelay 1500000
  em <- isEmptyMVar mv_thread
  unless em repeatAction'
  where repeatAction' = do
          em <- isEmptyMVar mv_thread
          unless em $ do fireButtonEventValue
                         threadDelay 300000
                         repeatAction'
-- used by delay button
animateDelay :: MVar a -> (Bool -> IO ()) -> IO ()
animateDelay mv_thread fireButtonPressed = animateDelay' (8 :: Int) True
  where animateDelay' 0 v = do
          fireButtonPressed v
          _<- tryTakeMVar mv_thread
          return ()
        animateDelay' i v = do
          em <- isEmptyMVar mv_thread
          unless em $ do fireButtonPressed v
                         threadDelay 250000
                         animateDelay' (i - 1) (not v)


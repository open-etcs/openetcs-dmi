{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.Windows.Window ( Window, mkWindow, MenuWindow ) where


import           Control.Concurrent
import           Control.Monad                (unless)
import           Control.Monad.Writer         (lift)
import           Data.Maybe                   (isJust)
import           Data.Text                    (Text)
import           Data.Typeable
import           ETCS.DMI.Helpers
import           ETCS.DMI.Widgets.Button
import           ETCS.DMI.Widgets.ButtonGroup
import           ETCS.DMI.Widgets.Sprites
import           GHCJS.DOM.Element            (setClassName)
import           GHCJS.DOM.HTMLElement        (setHidden)
import           GHCJS.DOM.Node               (appendChild, setTextContent)
import           GHCJS.DOM.Types              (castToElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks

data WindowTitle = WindowTitle

instance IsWidget WindowTitle where
  data WidgetInput WindowTitle = MkWindowTitle {
    _windowTitleText :: Behavior Text,
    _windowTitleHasSprite :: Behavior (Maybe Text)
    }

  mkWidgetInstance parent (MkWindowTitle titleText sprite_id) = do
    doc <- _getOwnerDocument parent

    -- the window title
    titleElem <- _createDivElement doc
    titleDiv <- _createDivElement doc

    let setTitle = setTextContent titleDiv . pure
    lift $ do
      valueBLater titleText >>= liftIOLater . setTitle
      changes titleText >>= reactimate' . fmap (fmap setTitle)
    () <$ appendChild titleElem (pure titleDiv)


    -- the title sprite
    (e, fireE') <- lift newEvent
    let fireE = fireE' hourGlassStep
    eOffsetX <- lift $ accumE 42 e
    bOffsetX <- lift $ stepper 42 eOffsetX
    _ <- mkSubWidget titleElem . mkSprite sprite_id $ bOffsetX

    -- the title sprite animation
    mvar <- lift . liftIO $ newEmptyMVar
    let bDoAnimate = fmap isJust sprite_id
        animationAction = do
          threadDelay 1000000
          em <- isEmptyMVar mvar
          unless em $ fireE >> animationAction
        handleAnimate a =
          if a then forkIO animationAction >>= putMVar mvar
            else tryTakeMVar mvar >>= maybe (return ()) killThread
    registerCleanupIO $ handleAnimate False
    lift $ valueBLater bDoAnimate >>= liftIOLater . handleAnimate
    lift $ changes bDoAnimate >>= reactimate' . fmap (fmap handleAnimate)


    () <$ appendChild parent (pure titleElem)
    return (WindowTitle, castToElement titleElem)
      where hourGlassStep :: Int -> Int
            hourGlassStep = animationStep 42 20 26 306
              where animationStep x0 w s t x1 =
                      let x' = x1 + s
                      in if x' + w < t then x' else x0



{-


animateHourGlass :: (IsNode parent) =>
                    MVar ThreadId -> parent -> Maybe Text -> MomentIO ()
animateHourGlass mvar parent sprite = do

  let killThreadIfExists = tryTakeMVar mvar >>= maybe (return ()) killThread
  case hg' of
    Nothing -> liftIO $ killThreadIfExists
    Just hg ->
      let setHourGlassX l = do
            st <- getStyle hg
            case st of
              Nothing -> fail "unable to get hour glass style"
              Just st' ->
                setCssText st' . pure . mconcat $ ["left: ", show l, "px;"]
      in do
        (e, fireE') <- newEvent
        let fireE = fireE' hourGlassStep
        animationX <- accumE 42 e
        reactimate $ fmap setHourGlassX animationX
        liftIO $ setHourGlassX 42
        let animationAction = do
              threadDelay 1000000
              em <- isEmptyMVar mvar
              unless em $ fireE >> animationAction

        liftIO $ forkIO animationAction >>= putMVar mvar
  where
    removeChildSVGs parent = do
      doc <- _getOwnerDocument parent
      cs' <- querySelectorAll doc ("[data-sprite]" :: String)
      case cs' of
        Nothing -> return ()
        Just cs -> do
          l <- getLength cs
          csElems <- catMaybes <$>
                     sequence [ item cs i | i <- [0..(pred l)] ]
          sequence_ . fmap (_removeFromParentIfExists parent) $ csElems




-}





type MenuWindow = Window ButtonGroup
















data Window a =
  Window { windowEvent :: Event (Either () (WidgetEventType a)) }

mkWindow :: Behavior Text -> Behavior (Maybe Text) -> Behavior Bool -> WidgetInput a -> WidgetInput (Window a)
mkWindow = MkWindow

instance (Typeable a, IsEventWidget a) => IsEventWidget (Window a) where
  type WidgetEventType (Window a) = Either () (WidgetEventType a)
  widgetEvent = windowEvent

instance (Typeable a, IsEventWidget a) => IsWidget (Window a) where
  data WidgetInput (Window a) = MkWindow {
    _windowTitle   :: Behavior Text,
    _windowTitleIcon :: Behavior (Maybe Text),
    _windowVisible :: Behavior Bool,
    _windowWidget  :: WidgetInput a
  }


  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    win <- _createDivElement doc
    closeContainer <- _createDivElement doc


    liftIO $ do
      setClassName closeContainer ("MenuClose" :: String)
      setClassName win ("MenuWindow" :: String)
      () <$ appendChild parent (pure win)
      () <$ appendChild win (pure closeContainer)

    _ <- mkSubWidget win $ MkWindowTitle (_windowTitle i) (_windowTitleIcon i)

    inner_widget <- mkSubWidget win $ _windowWidget i

    -- the close button
    closeButton <- mkSubWidget closeContainer $
                   mkButton UpButton (Just $ pure "x") (pure True) ()

    let setShown = setHidden win . not
    lift $ do
      valueBLater (_windowVisible i) >>= liftIOLater . setShown
      changes (_windowVisible i) >>= reactimate' . fmap (fmap setShown)

    let outputC = Left  <$> widgetEvent (fromWidgetInstance closeButton)
        outputB = Right <$> widgetEvent (fromWidgetInstance inner_widget)
        output  = whenE (_windowVisible i)$ unionWith const outputB outputC
    return (Window output, castToElement win)




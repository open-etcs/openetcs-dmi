{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI.MenuWindow ( mkMenuWindow ) where

import           Control.Monad
import           Data.Text                  (Text)
import           ETCS.DMI.Button
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element          (setClassName)
import           GHCJS.DOM.HTMLElement      (setHidden)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks



mkMenuWindow :: (MonadIO m, IsNode p) => p
                -> Behavior Text -> Event Bool ->
                [( Int -> WidgetInput (Button Int))]
                -> m (MomentIO (Event Int))
mkMenuWindow parent bTitle eVisible bs = do
  doc <- _getOwnerDocument parent
  win <- _mkWindow doc

  titleElem <- _createDivElement doc
  setClassName titleElem ("MenuTitle" :: String)
  () <$ appendChild win (pure titleElem)

  closeContainer <- _createDivElement doc
  setClassName closeContainer ("MenuClose" :: String)
  closeButtonR <-
    mkWidgetIO closeContainer $ mkButton UpButton (pure "x") (pure True) ()
  () <$ appendChild win (pure closeContainer)

  buttonsR <- _mkButtons doc win
  () <$ appendChild parent (pure win)

  return $ do
    -- the title
    let titleHandler = setTextContent titleElem . pure
    valueBLater bTitle >>= liftIOLater . titleHandler
    changes bTitle >>= reactimate' . fmap (fmap titleHandler)

    -- the close button
    closeButton <- closeButtonR
    let eCloseIntern = unionWith const eVisible . fmap (const False) $ buttonEvent closeButton
    reactimate $ fmap (setHidden win . not) eCloseIntern

    -- union of all button events
    foldl (unionWith const) never <$> sequence (fmap (fmap buttonEvent) buttonsR)


  where _mkWindow doc = do
          win <- _createDivElement doc
          setClassName win ("MenuWindow" :: String)
          return win

        _mkButtons doc win = do
            bsContainer <- _createDivElement doc
            setClassName bsContainer ("MenuButtons" :: String)

            bs' <- zipWithM (\i f -> f i) [(0 :: Int) .. 9]
                   [ mkWidgetIO bsContainer . b | b <- bs]
            () <$ appendChild win (pure bsContainer)
            return bs'


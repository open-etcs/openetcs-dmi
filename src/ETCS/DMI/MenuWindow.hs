{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI.MenuWindow ( mkMenuWindow ) where

import           Control.Monad
import           Data.Text                  (Text)
import           ETCS.DMI.Button
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element          (setClassName)
import           GHCJS.DOM.HTMLElement      (setHidden)
import           GHCJS.DOM.Node             (appendChild, setTextContent)
import           GHCJS.DOM.Types            (IsDocument, IsNode)
import           Reactive.Banana
import           Reactive.Banana.Frameworks



mkMenuWindow :: (IsDocument d, IsNode p, Frameworks t) => d -> p
                -> Behavior t Text -> Event t Bool ->
                [(ButtonType, Behavior t Text, Behavior t Bool )]
                -> IO (Moment t (Event t Int))
mkMenuWindow doc parent bTitle eVisible bs = do
  win <- _mkWindow

  titleElem <- _createDivElement doc
  setClassName titleElem ("MenuTitle" :: String)
  () <$ appendChild win (pure titleElem)

  closeContainer <- _createDivElement doc
  setClassName closeContainer ("MenuClose" :: String)
  closeButtonR <-
    mkButton doc closeContainer (UpButton, pure "x", pure True) ()
  () <$ appendChild win (pure closeContainer)

  buttonsR <- _mkButtons win
  () <$ appendChild parent (pure win)

  return $ do
    -- the title
    let titleHandler = setTextContent titleElem . pure
    initial bTitle >>= liftIOLater . titleHandler
    changes bTitle >>= reactimate' . fmap (fmap titleHandler)

    -- the close button
    closeButton <- closeButtonR
    let eCloseIntern = union eVisible . fmap (const False) $ closeButton
    reactimate $ fmap (setHidden win . not) eCloseIntern

    -- union of all button events
    unions <$> sequence buttonsR

  where _mkWindow = do
          win <- _createDivElement doc
          setClassName win ("MenuWindow" :: String)
          return win

        _mkButtons win = do
            bsContainer <- _createDivElement doc
            setClassName bsContainer ("MenuButtons" :: String)

            bs' <- zipWithM (\i f -> f i) [(0 :: Int) .. 9]
                   [ mkButton doc bsContainer b | b <- bs]
            () <$ appendChild win (pure bsContainer)
            return bs'


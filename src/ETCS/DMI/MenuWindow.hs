{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI.MenuWindow (
    MenuWindow, mkMenuWindow, menuWinE, menuWinCleanup
) where

import           Control.Lens
import           Data.Text             (Text)
import           ETCS.DMI.Button
import           ETCS.DMI.Helpers
import           ETCS.DMI.Types
import           FRP.Sodium
import           GHCJS.DOM.Element     (setClassName)
import           GHCJS.DOM.HTMLElement (setHidden)
import           GHCJS.DOM.Node        (appendChild, setTextContent)
import           GHCJS.DOM.Types       (IsDocument, IsNode)

mkMenuWindow :: (IsDocument d, IsNode p) => d -> p
                -> Behavior Text -> Event Bool ->[(ButtonType, Behavior Text, Behavior Bool )]
                -> IO MenuWindow
mkMenuWindow doc parent bTitle eVisible bs = do
  win <- _mkWindow

  cTitle <- _mkWindowTitle win
  buttons <- _mkButtons win
  closeButton <- _mkCloseButton win
  () <$ appendChild parent (pure win)

  let eButtons = mconcat . fmap (view buttonE) $ buttons
      cleanup =
        (sequence $ cTitle : closeButton ^. buttonCleanup
         : (fmap (view buttonCleanup) $ buttons)) >> return ()

  return $ _MenuWindow # (eButtons, cleanup)

  where _mkWindow = do
          win <- _createDivElement doc
          setClassName win ("MenuWindow" :: String)
          return win

        _mkWindowTitle win = do
          t <- _createDivElement doc
          cTitle <- sync $ listen (value bTitle) $ setTextContent t . pure
          setClassName t ("MenuTitle" :: String)
          () <$ appendChild win (pure t)
          return cTitle

        _mkButtons win = do
            bsContainer <- _createDivElement doc
            setClassName bsContainer ("MenuButtons" :: String)

            bs' <- sequence $ zipWith (\i f -> f i) [minBound .. maxBound]
                   [ mkButton doc bsContainer b | b <- bs]
            () <$ appendChild win (pure bsContainer)
            return bs'

        _mkCloseButton win = do
            closeContainer <- _createDivElement doc
            setClassName closeContainer ("MenuClose" :: String)
            closeButton <-
              mkButton doc closeContainer (UpButton, pure "x", pure True) ()

            let eIntern =
                  mappend eVisible . fmap (const False) $ closeButton ^. buttonE


            () <$ do (sync . listen eIntern $ (setHidden win . not))
            () <$ appendChild win (pure closeContainer)
            return closeButton


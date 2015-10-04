{-# LANGUAGE OverloadedStrings #-}

module ETCS.DMI.MenuWindow (
    MenuWindow, mkMenuWindow, menuWinE, menuWinCleanup
) where

import           Control.Lens
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           ETCS.DMI.Button
import           ETCS.DMI.Types
import           FRP.Sodium
import           GHCJS.DOM.Document    (createElement)
import           GHCJS.DOM.Element     (setClassName)
import           GHCJS.DOM.HTMLElement (setHidden)
import           GHCJS.DOM.Node        (appendChild, setTextContent)
import           GHCJS.DOM.Types       (IsDocument, IsNode,
                                        castToHTMLDivElement)

mkMenuWindow :: (IsDocument d, IsNode p) => d -> p
                -> Behavior Text -> Event Bool ->[(Behavior Text, Behavior Bool )]
                -> IO MenuWindow
mkMenuWindow doc parent bTitle eVisible bs = do
  w <- _mkWindow
  cTitle <- _mkWindowTitle w
  bs' <- _mkButtons w
  closeButton <- _mkCloseButton w
  _ <- appendChild parent (pure w)

  let winE = foldl merge mempty . fmap _buttonE $ bs'
      cleanup = do
        _ <- sequence . fmap _buttonCleanup $ bs'
        cTitle
        _buttonCleanup closeButton
        return ()

  return $ _MenuWindow # (winE, cleanup)

  where _mkWindow = do
          w <- fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
               createElement doc $ pure ("div" :: String)
          setClassName w ("MenuWindow" :: String)
          return w
        _mkWindowTitle w = do
          t <- fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
               createElement doc $ pure ("div" :: String)
          cTitle <- sync $ listen (value bTitle) $ setTextContent t . pure
          setClassName t ("MenuTitle" :: String)
          _ <- appendChild w (pure t)
          return cTitle
        _mkButtons w = do
            bsContainer <-
              fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
              createElement doc $ pure ("div" :: String)
            setClassName bsContainer ("MenuButtons" :: String)

            bs' <- sequence $ zipWith (\i f -> f i) [minBound .. maxBound]
                   [ mkButton doc bsContainer b | b <- bs]
            _ <- appendChild w (pure bsContainer)
            return bs'
        _mkCloseButton w = do
            closeContainer <-
              fmap (castToHTMLDivElement . fromMaybe (error "unable to create div")) $
              createElement doc $ pure ("div" :: String)
            setClassName closeContainer ("MenuClose" :: String)
            closeButton <- mkButton doc closeContainer (pure "X", pure True) ()

            let eIntern = eVisible `merge` (fmap (const False) $ _buttonE closeButton)
            _ <- sync $ listen eIntern $ \v -> do
              setHidden w (not v)

            _ <- appendChild w (pure closeContainer)
            return closeButton

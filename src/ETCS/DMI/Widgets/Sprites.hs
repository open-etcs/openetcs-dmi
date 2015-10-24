{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.Widgets.Sprites (Sprite, mkSprite, mkEmptySprite, mkSpriteElement) where


import           Control.Monad.Writer
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           ETCS.DMI.Helpers
import           GHCJS.DOM.CSSStyleDeclaration (setCssText)
import           GHCJS.DOM.Element             (getStyle, setAttribute,
                                                setAttributeNS, setClassName)
import           GHCJS.DOM.Node                (IsNode, appendChild)
import           GHCJS.DOM.SVGElement          (SVGElement)
import           GHCJS.DOM.Types               (castToElement)
import           Reactive.Banana
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks

spritesFile :: Text
spritesFile = "./sprites.svg"


data Sprite = Sprite

xlinkNS :: String
xlinkNS = "http://www.w3.org/1999/xlink"

mkSprite :: Behavior (Maybe Text) -> Behavior Int -> WidgetInput Sprite
mkSprite = MkSprite

mkEmptySprite :: WidgetInput Sprite
mkEmptySprite = mkSprite (pure Nothing) (pure 0)

-- TODO: unify mkWidgetInstance - mkSpriteElement

instance IsWidget Sprite where
  data WidgetInput Sprite = MkSprite {
    _spriteId :: Behavior (Maybe Text),
    _spriteOffsetX :: Behavior Int
  }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent

    s <- _createSVGElement doc

    setClassName s ("sprite" :: String)
    u <- _createSVGUseElement doc

    let setSprite t =
          let href = maybe mempty (\t' -> mconcat [ spritesFile, "#", t' ]) t
          in do
            _ <- setAttribute s ("data-sprite" :: String) . maybe "" id $ t
            setAttributeNS u (pure xlinkNS) ("href" :: String) href
    lift $ valueBLater (_spriteId i) >>= liftIOLater . setSprite
    lift $ changes (_spriteId i) >>= reactimate' . fmap (fmap setSprite)

    let setOffsetX l = do
          st <- getStyle s
          case st of
            Nothing -> fail "unable to get sprite style"
            Just st' -> setCssText st' . pure . mconcat $ ["left: ", show l, "px;"]
    lift $ valueBLater (_spriteOffsetX i) >>= liftIOLater . setOffsetX
    lift $ changes (_spriteOffsetX i) >>= reactimate' . fmap (fmap setOffsetX)

    _ <- appendChild s $ pure u
    _ <- appendChild parent $ pure s
    return (Sprite, castToElement s)




mkSpriteElement :: (IsNode parent, MonadIO m) =>
                parent -> Maybe Text -> m SVGElement

mkSpriteElement parent sid = do
    doc <- _getOwnerDocument parent
    s <- _createSVGElement doc
    setClassName s ("sprite" :: String)
    u <- _createSVGUseElement doc
    let href = maybe mempty (\t' -> mconcat [ spritesFile, "#", t' ]) sid
    void $ setAttribute s ("data-sprite" :: String) . fromMaybe "" $ sid
    setAttributeNS u (pure xlinkNS) ("href" :: String) href
    void $ appendChild s $ pure u
    void $ appendChild parent $ pure s
    return s



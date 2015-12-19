{-# LANGUAGE DataKinds #-}



module ETCS.DMI.Helpers where


import Control.Monad.Fix
import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Reflex


dynAnd,dynOr :: (Reflex t, MonadHold t m) =>
                Dynamic t Bool -> Dynamic t Bool -> m (Dynamic t Bool)
dynAnd = combineDyn (&&)
dynOr = combineDyn (||)

bsAnd, bsOr :: (Applicative f, Traversable t) => t (f Bool) -> f Bool
bsAnd = fmap Prelude.and . sequenceA
bsOr  = fmap Prelude.or . sequenceA

bAnd, bOr :: Applicative f => f Bool -> f Bool -> f Bool
bAnd a b = (&&) <$> a <*> b
bOr  a b = (||) <$> a <*> b


kmh :: Fractional a => Unit 'NonMetric DVelocity a
kmh = kilo meter / hour

srFlipFlop ::
  (Reflex t, MonadSample t m, MonadHold t m, MonadFix m) =>
  Dynamic t Bool -> Dynamic t Bool -> m (Dynamic t Bool)
srFlipFlop sD rD =
  let sE = ffilter id $ updated sD
      rE = ffilter id $ updated rD
      cE = leftmost [True <$ sE, False <$ rE]
  in do
    s <- sample . current $ sD
    srD <- holdDyn s cE
    return $ nubDyn srD



{-


kmh :: (Fractional a) => Unit DVelocity a
kmh = kilo meter / hour


bsAnd, bsOr :: (Applicative f, Traversable t) => t (f Bool) -> f Bool
bsAnd = fmap Prelude.and . sequenceA
bsOr  = fmap Prelude.or . sequenceA

bAnd, bOr :: Applicative f => f Bool -> f Bool -> f Bool
bAnd a b = (&&) <$> a <*> b
bOr  a b = (||) <$> a <*> b


_removeFromParentIfExists :: (IsNode parent, IsNode a) => parent -> a -> IO ()
_removeFromParentIfExists parent a = do
  aParent <- getParentNode a
  isAParent <- isEqualNode parent aParent
  unless isAParent $ () <$ removeChild parent aParent ; return ()

_createDivElement :: (MonadIO m, IsDocument self) => self -> m HTMLDivElement
_createDivElement doc = _createElement doc "div" castToHTMLDivElement

_createButtonElement :: (MonadIO m, IsDocument self) => self -> m HTMLButtonElement
_createButtonElement doc = _createElement doc "button" castToHTMLButtonElement

_createSpanElement :: (MonadIO m, IsDocument self) => self -> m HTMLSpanElement
_createSpanElement doc = _createElement doc "span" castToHTMLSpanElement


svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

_createSVGElement :: (MonadIO m, IsDocument self) => self -> m SVGElement
_createSVGElement doc = _createElementNS doc svgNS "svg" castToSVGElement

_createSVGUseElement :: (MonadIO m, IsDocument self) => self -> m SVGUseElement
_createSVGUseElement doc = _createElementNS doc svgNS "use" castToSVGUseElement

_createSVGGElement :: (MonadIO m, IsDocument self) => self -> m SVGGElement
_createSVGGElement doc = _createElementNS doc svgNS "g" castToSVGGElement

_createSVGCircleElement :: (MonadIO m, IsDocument self) => self -> m SVGCircleElement
_createSVGCircleElement doc = _createElementNS doc svgNS "circle" castToSVGCircleElement

_createSVGPolygonElement :: (MonadIO m, IsDocument self) => self -> m SVGPolygonElement
_createSVGPolygonElement doc = _createElementNS doc svgNS "polygon" castToSVGPolygonElement

_createSVGTextElement :: (MonadIO m, IsDocument self) => self -> m SVGTextElement
_createSVGTextElement doc = _createElementNS doc svgNS "text" castToSVGTextElement

_createSVGLineElement :: (MonadIO m, IsDocument self) => self -> m SVGLineElement
_createSVGLineElement doc = _createElementNS doc svgNS "line" castToSVGLineElement

_createSVGPathElement :: (MonadIO m, IsDocument self) => self -> m SVGPathElement
_createSVGPathElement doc = _createElementNS doc svgNS "path" castToSVGPathElement



_createElement :: (MonadIO m, IsDocument self) => self -> String -> (Element -> t) -> m t
_createElement doc e c =
  createElement doc (pure e) >>= maybe (error $ "unable to create " ++ e) (return . c)

_createElementNS :: (MonadIO m, IsDocument self) => self -> String -> String -> (Element -> t) -> m t
_createElementNS doc ns e c =
  createElementNS doc (pure ns) (pure e) >>=
    maybe (error $ mconcat ["unable to create ", e, " in NS ", ns]) (return . c)

_getOwnerDocument :: (MonadIO m, IsNode self) => self -> m Document
_getOwnerDocument n = do
  getOwnerDocument n >>=
    maybe (fail "unable to determine OwnerDocument") return

-}

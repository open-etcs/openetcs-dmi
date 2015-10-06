module ETCS.DMI.Helpers
       (_createDivElement, _createButtonElement, _createSpanElement
       ,_removeFromParentIfExists
       , bAnd, bOr, bsAnd, bsOr
       ) where

import           Control.Monad
import           Data.Maybe         (fromMaybe)
import           GHCJS.DOM.Document (createElement)
import           GHCJS.DOM.Node     (getParentNode, isEqualNode, removeChild)
import           GHCJS.DOM.Types    (Element, HTMLButtonElement, HTMLDivElement,
                                     HTMLSpanElement, IsDocument, IsNode,
                                     castToHTMLButtonElement,
                                     castToHTMLDivElement,
                                     castToHTMLSpanElement)

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

_createDivElement :: (IsDocument self) => self -> IO HTMLDivElement
_createDivElement doc = _createElement doc "div" castToHTMLDivElement

_createButtonElement :: (IsDocument self) => self -> IO HTMLButtonElement
_createButtonElement doc = _createElement doc "button" castToHTMLButtonElement

_createSpanElement :: (IsDocument self) => self -> IO HTMLSpanElement
_createSpanElement doc = _createElement doc "span" castToHTMLSpanElement

_createElement :: (IsDocument self) => self -> String -> (Element -> t) -> IO t
_createElement doc e c =
  fmap (c . fromMaybe (error $ "unable to create " ++ e)) $
  createElement doc . pure $ e

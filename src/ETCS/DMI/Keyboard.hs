{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ETCS.DMI.Keyboard ( NumericKeyboard, mkNumericKeyboard
                         , EnhancedNumericKeyboard, mkEnhancedNumericKeyboard
                         , AlphaNumKeyboard, mkAlphaNumKeyboard
                         , DedicatedKeyboard, mkDedicatedKeyboard
                         , mkKeyboardBuffer
                         ) where

import           Control.Monad
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           ETCS.DMI.Button
import           ETCS.DMI.Helpers
import           GHCJS.DOM.Element          (setClassName)
import           GHCJS.DOM.HTMLElement      (setHidden)
import           GHCJS.DOM.Node             (appendChild)
import           GHCJS.DOM.Types            (Element, castToElement,
                                             castToHTMLElement)
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.Frameworks


newtype NumericKeyboard = NumericKeyboard (Keyboard (Maybe Char))

newtype EnhancedNumericKeyboard = EnhancedNumericKeyboard (Keyboard (Maybe Char))

newtype AlphaNumKeyboard = AlphaNumKeyboard (Keyboard (Maybe Int))

newtype DedicatedKeyboard a = DedicatedKeyboard (Event a)

mkNumericKeyboard :: WidgetInput NumericKeyboard
mkNumericKeyboard = MkNumericKeyboard

mkEnhancedNumericKeyboard :: WidgetInput EnhancedNumericKeyboard
mkEnhancedNumericKeyboard = MkEnhancedNumericKeyboard

mkAlphaNumKeyboard :: WidgetInput AlphaNumKeyboard
mkAlphaNumKeyboard = MkAlphaNumKeyboard

mkDedicatedKeyboard :: [(Behavior Text, a)] -> WidgetInput (DedicatedKeyboard a)
mkDedicatedKeyboard = MkDedicatedKeyboard


instance IsEventWidget EnhancedNumericKeyboard where
  type WidgetEventType EnhancedNumericKeyboard = Maybe Char
  widgetEvent (EnhancedNumericKeyboard kbd) = widgetEvent kbd

instance IsWidget EnhancedNumericKeyboard where
  data WidgetInput EnhancedNumericKeyboard = MkEnhancedNumericKeyboard
  mkWidgetIO = numKeyboard EnhancedNumericKeyboard True


instance IsEventWidget NumericKeyboard where
  type WidgetEventType NumericKeyboard = Maybe Char
  widgetEvent (NumericKeyboard kbd) = widgetEvent kbd

instance IsWidget NumericKeyboard where
  data WidgetInput NumericKeyboard = MkNumericKeyboard
  mkWidgetIO = numKeyboard NumericKeyboard False

instance IsEventWidget AlphaNumKeyboard where
  type WidgetEventType AlphaNumKeyboard = Maybe Int
  widgetEvent (AlphaNumKeyboard kbd) = widgetEvent kbd

instance IsWidget AlphaNumKeyboard where
  data WidgetInput AlphaNumKeyboard = MkAlphaNumKeyboard
  mkWidgetIO parent _ =
    let keyboard =
          MkKeyboard False
          (fmap pure [ "1", "2 abc", "3 def", "4 ghi", "5 jkl", "6 mno"
                     , "7 pqrs" ,"8 tuv", "9 wxyz", "Del", "0", "." ])
          mapKeysAlphaNumKeyboard
      in do
      kbdContainer <- _getOwnerDocument parent >>= _createDivElement
      () <$ appendChild parent (pure kbdContainer)

      return ( AlphaNumKeyboard <$> mkWidget kbdContainer keyboard
             , castToElement kbdContainer)


mapKeysAlphaNumKeyboard :: Int -> Maybe Int
mapKeysAlphaNumKeyboard i
  | 0 <= i && i < 9 = pure . succ $ i
  | i == 9          = Nothing
  | i == 10         = pure 0
  | otherwise       = error $ "undefined AlphaNumKeyboard value: " ++ show i


numKeyboard :: (MonadIO m, IsNode self) =>
               (Keyboard (Maybe Char) -> b)
               -> Bool -> self -> t -> m (MomentIO b, Element)
numKeyboard c dot parent _ =
    let numericKeyboard =
          MkKeyboard dot
          (fmap pure [ "1", "2", "3", "4", "5", "6", "7" ,"8", "9", "Del", "0", "." ])
          mapKeysNumericKeyboard
    in do
      kbdContainer <- _getOwnerDocument parent >>= _createDivElement
      () <$ appendChild parent (pure kbdContainer)

      return ( c <$> mkWidget kbdContainer numericKeyboard
             , castToElement kbdContainer)

instance Typeable a => IsEventWidget (DedicatedKeyboard a) where
  type WidgetEventType (DedicatedKeyboard a) = a
  widgetEvent (DedicatedKeyboard e) = e

instance Typeable a => IsWidget (DedicatedKeyboard a) where
  data WidgetInput (DedicatedKeyboard a) = MkDedicatedKeyboard {
    _keyboardData :: [(Behavior Text, a)]
    }
  mkWidgetIO parent i =
    let (as, bs') = splitAt 11 (_keyboardData i)
        bs = take 11 bs'
        n = length $ mappend as bs
    in do
      doc <- _getOwnerDocument parent
      kbdContainer <- _createDivElement doc
      page1 <- _createDivElement doc

      let mkKey p (b, e) = mkWidget p . mkButton DownButton (pure b) (pure True) $ e
          keyboardWidget = do
            liftIOLater $ do
              () <$ appendChild kbdContainer (pure page1)
              () <$ appendChild parent (pure kbdContainer)
              return ()
            if n <= 12
            then do
              bsP1 <- sequence . fmap (mkKey page1) $ _keyboardData i
              let e =  foldl (unionWith const) never . fmap widgetEvent $ bsP1
              return . DedicatedKeyboard $ e
            else do
              page2 <- liftIO $ _createDivElement doc
              more1 <- liftIO $ _createDivElement doc
              more2 <- liftIO $ _createDivElement doc
              setClassName more1 ("MoreButton" :: String)
              setClassName more2 ("MoreButton" :: String)


              bsP1 <- sequence . fmap (mkKey page1) $ as
              bsP2 <- sequence . fmap (mkKey page2) $ bs
              let  moreButton = mkButton DownButton (pure $ pure "[More]") (pure True) not
              btTP1 <- mkWidget more1 moreButton
              btTP2 <- mkWidget more2 moreButton
              () <$ appendChild page1 (pure more1)
              () <$ appendChild page2 (pure more2)
              eP1 <- accumE True $ unionWith const (widgetEvent btTP1) (widgetEvent btTP2)
              bP1 <- stepper True eP1

              let bP2 = fmap not bP1
                  sP1 = setHidden (castToHTMLElement page1) . not
                  sP2 = setHidden (castToHTMLElement page2) . not
              valueBLater bP1 >>= liftIOLater . sP1
              valueBLater bP2 >>= liftIOLater . sP2
              changes bP1 >>= reactimate' . fmap (fmap sP1)
              changes bP2 >>= reactimate' . fmap (fmap sP2)


              () <$ appendChild kbdContainer (pure page2)
              let kbdEvent =  foldl (unionWith const) never . fmap widgetEvent $
                       mappend bsP1 bsP2

              return . DedicatedKeyboard $ kbdEvent

      return (keyboardWidget, castToElement kbdContainer)


mapKeysNumericKeyboard :: Int -> Maybe Char
mapKeysNumericKeyboard i
  | 0 <= i && i < 9 = pure . head . show . succ $ i
  | i == 9          = Nothing
  | i == 10         = pure '0'
  | i == 11         = pure '.'
  | otherwise       = error $ "undefined NumericKeyboard value: " ++ show i





newtype Keyboard a =
  Keyboard { keyboardEvent :: Event a
           }

instance IsEventWidget (Keyboard a) where
  type WidgetEventType (Keyboard a) = a
  widgetEvent = keyboardEvent

instance IsWidget (Keyboard a) where
  data WidgetInput (Keyboard a) = MkKeyboard {
      _keyboardDot     :: Bool,
      _keyboardLabels  :: [ Behavior Text ],
      _keyboardMapping :: Int -> a
    }

  mkWidgetIO parent i = do
    kbdContainer <- _getOwnerDocument parent >>= _createDivElement


    let mkKbdButton :: Behavior Text -> Int -> MomentIO (Button Int)
        mkKbdButton b e =
          let dot = e /= 11 || _keyboardDot i
          in mkWidget kbdContainer . mkButton DownButton (pure b) (pure dot) $ e

        keyboardWidget = do
          liftIOLater $ do () <$ appendChild parent (pure kbdContainer) ; return ()
          bs <- zipWithM ($) (fmap mkKbdButton (_keyboardLabels i)) [0..11]
          let e =  foldl (unionWith const) never . fmap widgetEvent $ bs
          return . Keyboard . fmap (_keyboardMapping i) $ e

    return (keyboardWidget, castToElement kbdContainer)



mkKeyboardBuffer :: (Eq a, MonadMoment m) =>
                    Int -> Event r -> Event (Maybe a) -> m (Behavior [a])
mkKeyboardBuffer l r e = do
  bufferE' <- accumE [] (fmap (baseBuffer l) e)
  stepper [] . unionWith const bufferE' . fmap (const []) $ r


baseBuffer :: Eq a => Int -> Maybe a -> [a] -> [a]
baseBuffer l (Just c) bs = if l > length bs then  bs ++ [c] else bs
baseBuffer _ Nothing bs = if null bs then [] else init bs


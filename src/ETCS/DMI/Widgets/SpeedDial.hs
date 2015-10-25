{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}


module ETCS.DMI.Widgets.SpeedDial (SpeedDial, mkSpeedDial)  where

import           Control.Lens                         hiding ((*~))
import           Control.Monad
import           Control.Monad.Writer
import           ETCS.DMI.Helpers
import           ETCS.DMI.TrainBehavior
import           ETCS.DMI.Types
import           GHCJS.DOM.Element                    (setAttribute)
import           GHCJS.DOM.Node                       (appendChild,
                                                       setTextContent)
import           GHCJS.DOM.Types                      (IsDocument, IsElement,
                                                       IsNode, SVGPathElement,
                                                       castToElement)
import           Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude                              as P
import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget
import           Reactive.Banana.Frameworks

data SpeedDial = SpeedDial

mkSpeedDial :: TrainBehavior -> WidgetInput SpeedDial
mkSpeedDial = MkSpeedDial

instance IsWidget SpeedDial where
  data WidgetInput SpeedDial =
    MkSpeedDial {
      _speedDialTrainBehavior :: TrainBehavior
    }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    container <- _createDivElement doc
    svg <- _createSVGElement doc
    setAttribute svg "width" "280px"
    setAttribute svg "height" "300px"
    setAttribute svg "viewBox" "0 0 280 300"
    void $ appendChild container (pure svg)

    let pointerColor = pure Grey
    let pointerIsRed = (== Red) <$> pointerColor

    void $ mkSubWidget svg $ MkSpeedIndicatorLines (_speedDialTrainBehavior i)
    void $ mkSubWidget svg $ MkSpeedPointer (_speedDialTrainBehavior i) pointerColor
    void $ mkSubWidget svg $ MkDigitalSpeed (_speedDialTrainBehavior i) pointerIsRed

    drawCircular svg SpeedDial400 (160 *~ kmh)

    void $ appendChild parent (pure container)
    return (SpeedDial, castToElement container)



data CircularSpeedGauge = CircularSpeedGauge

instance IsWidget CircularSpeedGauge where
  data WidgetInput CircularSpeedGauge =
    MkCircularSpeedGauge {
      _csmInSuperVision :: InSuperVision,
      _csmVelocity :: Behavior (Velocity Double)
    }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    container <- _createSVGGElement doc

    let status = supervisionInformationStatus
                 (_csmInSuperVision i) (_csmVelocity i)

    return (CircularSpeedGauge, castToElement container)



drawBasicSpeedHook :: (MonadIO m, IsNode n) =>
            n -> SpeedDialType -> Velocity Double -> m SVGPathElement
drawBasicSpeedHook parent d v = do
  doc <- _getOwnerDocument parent
  p <- _createSVGPathElement doc

  let a = speedDialDegree d v /~ degree
  setAttribute p "d" $ mconcat
    ["M140,0 L140,20 L134,20 L134,0"]
  setAttribute p "transform" . mconcat $
    [ "rotate(", show a, ", 140, 140)" ]

  void $ appendChild parent (pure p)
  return p


drawCircular parent d v =
  let a = speedDialDegree d v
  in do
    void $ drawCircular' parent (9 *~ one) ((-144) *~ degree) a
    void $ drawBasicSpeedHook parent d v

drawCircular' :: (MonadIO m, IsNode n) =>
                n -> Dimensionless Double ->
                PlaneAngle Double -> PlaneAngle Double -> m SVGPathElement
drawCircular' parent r a b =
  let ox = 140 *~ one
      ix = ox - r
      ri = ix /~ one
      ro = ox /~ one
      a' = a * ((-1) *~ one)
      b' = b * ((-1) *~ one)
      dix0 = sin a' * ix
      dox0 = sin a' * ox
      diy0 = cos a' * ix
      doy0 = cos a' * ox
      dix1 = sin b' * ix
      dox1 = sin b' * ox
      diy1 = cos b' * ix
      doy1 = cos b' * ox

  in do
    doc <- _getOwnerDocument parent
    p <- _createSVGPathElement doc
    setAttribute p "d" $ mconcat
      [  "M", show (ox - dix0), ",", show (ox - diy0)
      , " L", show (ox - dox0), ",", show (ox - doy0)
      , " A ", show ro, " ", show ro, " 0 0 1", show (ox - dox1), ",", show (ox - doy1)
      , " L", show (ox - dix1), ",", show (ox - diy1)
      , " A ", show ri, " ", show ri, " 0 0 0", show (ox - dix0), ",", show (ox - diy0)
      ]

    void $ appendChild parent (pure p)
    return p


type PermittedSpeed    = Behavior (Velocity Double)
type IndicationSpeed   = Behavior (Velocity Double)
type TargetSpeed       = Behavior (Velocity Double)
type ReleaseSpeed      = Behavior (Velocity Double)
type WarningSpeed      = Behavior (Velocity Double)
type InterventionSpeed = Behavior (Velocity Double)

data InSuperVision where
  InCSM :: PermittedSpeed -> WarningSpeed -> InterventionSpeed -> InSuperVision
  InPIM :: PermittedSpeed -> WarningSpeed -> InterventionSpeed -> InSuperVision
  InTSM :: IndicationSpeed -> TargetSpeed ->
           PermittedSpeed -> WarningSpeed -> InterventionSpeed -> InSuperVision
  InRSM :: ReleaseSpeed -> Behavior Bool -> InSuperVision



supervisionInformationStatus ::
  InSuperVision -> Behavior (Velocity Double) -> Behavior StatusInformation
supervisionInformationStatus (InCSM p w int) v =
    let inNoS  = (<=) <$> v <*> p
        inOvS  = not  <$> inNoS
        inWaS  = (>)  <$> v <*> w
        inIntS = (>)  <$> v <*> int
        status' _ _ _ True = IntS
        status' _ _ True _ = WaS
        status' _ True _ _ = OvS
        status' _ _ _ _ = NoS
    in status' <$> inNoS <*> inOvS <*> inWaS <*> inIntS
supervisionInformationStatus (InPIM p w int) v =
  supervisionInformationStatus (InCSM p w int) v
supervisionInformationStatus (InTSM i target p w int) v =
  let inNoS  = (<=) <$> v <*> i
      inIndS = (\inNos' target' v' -> not inNos' && (v' >= target'))
               <$> inNoS <*> target <*> v
      inOvS  = (>) <$> v <*> p
      inWaS  = (>) <$> v <*> w
      inIntS = (>) <$> v <*> int
      status' _ _ _ _ True = IntS
      status' _ _ _ True _ = WaS
      status' _ _ True _ _ = OvS
      status' _ True _ _ _ = IndS
      status' _ _ _ _ _    = NoS
  in status' <$> inNoS <*> inIndS <*> inOvS <*> inWaS <*> inIntS
supervisionInformationStatus (InRSM release isBreaking) v =
  let inIndS  = (<=) <$> v <*> release
      inIntS  = (\inIndS' isBreaking' -> not inIndS' && isBreaking')
                <$> inIndS <*> isBreaking
  in (\a -> if a then IntS else IndS) <$> inIntS








data SpeedIndicatorLines = SpeedIndicatorLines

instance IsWidget SpeedIndicatorLines where
  data WidgetInput SpeedIndicatorLines =
    MkSpeedIndicatorLines {
      _speedIndicatorLinesTrainBehavior :: TrainBehavior
    }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    container <- _createSVGGElement doc

    let drawIndicators d = do
          deleteChildNodes container
          mkShortSpeedIndicatorLines doc container d
          mkLongSpeedIndicatorLines doc container d
          mkSpeedIndicatorNumbers doc container d

    let bSD = _speedIndicatorLinesTrainBehavior i ^. trainSpeedDial
    lift $ valueBLater bSD >>= liftIOLater . drawIndicators
    lift $ changes bSD >>= reactimate' . fmap (fmap drawIndicators)


    void $ appendChild parent (pure container)
    return (SpeedIndicatorLines, castToElement container)


mkLongSpeedIndicatorLines ::
  (IsDocument doc, IsNode parent, MonadIO m) => doc -> parent -> SpeedDialType -> m ()
mkLongSpeedIndicatorLines doc parent d =
   sequence_ $ mkSpeedIndicatorLine doc parent 25 d <$> speedIndicatorsLong d

mkShortSpeedIndicatorLines ::
  (IsDocument doc, IsNode parent, MonadIO m) => doc -> parent -> SpeedDialType -> m ()
mkShortSpeedIndicatorLines doc parent d =
   sequence_ $ mkSpeedIndicatorLine doc parent 15 d <$> speedIndicatorsShort d


mkSpeedIndicatorNumbers ::
  (IsDocument doc, IsNode parent, MonadIO m) => doc -> parent -> SpeedDialType -> m ()
mkSpeedIndicatorNumbers doc parent d =
   sequence_ $ mkSpeedIndicatorNumber doc parent d <$> speedIndicatorsNumber d

mkSpeedIndicatorLine ::
  (IsDocument doc, IsNode parent, MonadIO m) =>
  doc -> parent -> Double -> SpeedDialType -> Velocity Double -> m ()
mkSpeedIndicatorLine doc parent len d v = do
  line <- _createSVGLineElement doc
  setAttribute line "x1" "140"
  setAttribute line "y1" $ show ((P.-) (140 :: Int) 125)
  setAttribute line "x2" "140"
  setAttribute line "y2" $ show ((P.+) len $ (P.-) 140 125)
  setAttribute line "title" . mconcat $
    [ show (round (v /~ kmh) :: Int), " km/h" ]
  let deg = speedDialDegree d v /~ degree
  setAttribute line "transform" . mconcat $
    [ "rotate(", show deg, ", 140, 140)" ]
  void $ appendChild parent (pure line)

mkSpeedIndicatorNumber ::
  (IsDocument doc, IsNode parent, MonadIO m) =>
  doc -> parent -> SpeedDialType -> Velocity Double -> m ()
mkSpeedIndicatorNumber doc parent d v = do
  txt <- _createSVGTextElement doc
  setAttribute txt "x" "140"
  setAttribute txt "y" "52"
  let deg = speedDialDegree d v /~ degree
      lbl = round (v /~ kmh) :: Int
  setAttribute txt "title" . mconcat $ [ show lbl, " km/h" ]
  setTextContent txt . pure . show $ lbl

  setAttribute txt "transform" . mconcat $
    [ "rotate(", show deg, ", 140, 140) "
    , "rotate(", show ((P.*) (-1) deg), ", 140, 52)" ]
  void $ appendChild parent (pure txt)


speedIndicatorsLong :: SpeedDialType -> [Velocity Double]
speedIndicatorsLong SpeedDial400 =
  (*~ kmh) <$> [ 0, 50, 100, 150, 200, 250, 300, 350, 400 ]
speedIndicatorsLong SpeedDial250 = (*~ kmh) <$> [ (P.*) 20 i | i <- [0..12]]
speedIndicatorsLong SpeedDial180 = (*~ kmh) <$> [ (P.*) 20 i | i <- [0..9]]
speedIndicatorsLong SpeedDial140 = (*~ kmh) <$> [ (P.*) 20 i | i <- [0..7]]

speedIndicatorsNumber :: SpeedDialType -> [Velocity Double]
speedIndicatorsNumber SpeedDial400 = (*~ kmh) <$> [0, 50, 100, 150, 200, 300, 400]
speedIndicatorsNumber SpeedDial250 = (*~ kmh) <$> [(P.*) 20 i | i <- [0 .. 12]]
speedIndicatorsNumber SpeedDial180 = (*~ kmh) <$> [(P.*) 20 i | i <- [0 .. 9]]
speedIndicatorsNumber SpeedDial140 = (*~ kmh) <$> [(P.*) 20 i | i <- [0 .. 7]]

speedIndicatorsShort :: SpeedDialType -> [Velocity Double]
speedIndicatorsShort SpeedDial400
  = (*~ kmh) <$> [ (P.+) ((P.*) 50 i) $ (P.*) 10 j | i <- [0..7], j <- [1..4]]
speedIndicatorsShort SpeedDial250
  = (*~ kmh) <$> [ (P.+) 10 $ (P.*) 20 j | j <- [0..12]]
speedIndicatorsShort SpeedDial180
  = (*~ kmh) <$> [ (P.+) 10 $ (P.*) 20 j | j <- [0..8]]
speedIndicatorsShort SpeedDial140
  = (*~ kmh) <$> [ (P.+) 10 $ (P.*) 20 j | j <- [0..6]]



data DigitalSpeed = DigitalSpeed

instance IsWidget DigitalSpeed where
  data WidgetInput DigitalSpeed =
    MkDigitalSpeed {
      _digitalSpeedTrainBehavior :: TrainBehavior,
      _digitalSpeedIsRed :: Behavior Bool
      }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    container <- _createSVGGElement doc


    d0 <- _createSVGTextElement doc
    d1 <- _createSVGTextElement doc
    d2 <- _createSVGTextElement doc


    let setFontColor isRed =
          setAttribute container "style" . uiColorStyle $
          if isRed then White else Black

    lift $ valueBLater (_digitalSpeedIsRed i) >>=
      liftIOLater . setFontColor
    lift $ changes (_digitalSpeedIsRed i)     >>=
      reactimate' . fmap (fmap $ setFontColor)


    let delta_x :: Double -> Double
        delta_x j = (P.-) 160 . (P.*) j . (P./) 50 $ 3
    setAttribute d0 "x" . show . delta_x $ 0
    setAttribute d1 "x" . show . delta_x $ 1
    setAttribute d2 "x" . show . delta_x $ 2
    let y = "140"
    setAttribute d0 "y" y
    setAttribute d1 "y" y
    setAttribute d2 "y" y
    let bVels = iDigits <$>  _digitalSpeedTrainBehavior i ^. trainVelocityKmh
        b0 = (\(_,_,a) -> a) <$> bVels
        b1 = (\(_,b,_) -> b) <$> bVels
        b2 = (\(c,_,_) -> c) <$> bVels

    lift $ valueBLater b0 >>= liftIOLater . setTextContent d0 . pure
    lift $ changes b0 >>= reactimate' . fmap (fmap $ setTextContent d0 . pure)
    lift $ valueBLater b1 >>= liftIOLater . setTextContent d1 . pure
    lift $ changes b1 >>= reactimate' . fmap (fmap $ setTextContent d1 . pure)
    lift $ valueBLater b2 >>= liftIOLater . setTextContent d2 . pure
    lift $ changes b2 >>= reactimate' . fmap (fmap $ setTextContent d2 . pure)

    void $ appendChild container (pure d0)
    void $ appendChild container (pure d1)
    void $ appendChild container (pure d2)
    void $ appendChild parent (pure container)

    return (DigitalSpeed, castToElement container)

iDigits :: (Integral i, Show i) => i -> (String,String,String)
iDigits i =
  case show $ i `P.mod` 100 of
    [c,b,a] -> ([c],[b],[a])
    [b,a] -> ("",[b],[a])
    [a] -> ("","",[a])
    _ -> ("","","")


trainVelocityKmh :: Getter TrainBehavior (Behavior Int)
trainVelocityKmh = to $ fmap (\v -> round $ v /~ kmh) . view trainVelocity


data SpeedPointer = SpeedPointer

instance IsWidget SpeedPointer where
  data WidgetInput SpeedPointer =
    MkSpeedPointer {
      _speedPointerTrainBehavior :: TrainBehavior,
      _speedPointerColor :: Behavior UIColor
    }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    container <- _createSVGGElement doc

    let setUIColor c = setAttribute container "style" . uiColorStyle $ c
    lift $ valueBLater (_speedPointerColor i) >>=
      liftIOLater . setUIColor
    lift $ changes (_speedPointerColor i)     >>=
      reactimate' . fmap (fmap $ setUIColor)


    p <- _createSVGPolygonElement doc
    c <- _createSVGCircleElement doc
    setAttribute c "cx" "105"
    setAttribute c "cy" "25"
    setAttribute c "r" "25"
    void $ appendChild container (pure c)

    setAttribute p "points"
      "0,24 15,24 23,20 80,20 80,29 23,29 15,26 0,26"
    void $ appendChild container (pure p)
    let bPointer = _speedPointerTrainBehavior i ^. speedPointerBehavior
    lift $ valueBLater bPointer >>= liftIOLater . setSpeedPointer container
    lift $ changes bPointer >>= reactimate' . fmap (fmap $ setSpeedPointer container)

    void $ appendChild parent (pure container)
    return (SpeedPointer, castToElement container)


setSpeedPointer :: (MonadIO m, IsElement e, Floating r, Show r) =>
                   e -> PlaneAngle r -> m ()
setSpeedPointer p a =
  setAttribute p "transform" $ mconcat
  [ "translate(35, 115) rotate(", show $ a /~ degree, ",105,25) " ]


speedDialBehavior :: Getter TrainBehavior (Behavior (Dimensionless Double))
speedDialBehavior =
  to $ \td -> speedDialDegree <$> td ^. trainSpeedDial <*> td ^. trainVelocity

speedPointerBehavior :: Getter TrainBehavior (Behavior (Dimensionless Double))
speedPointerBehavior =
  to $ \td -> fmap (\a -> a + (90 *~ degree)) $ td ^. speedDialBehavior

speedDialMaxV :: SpeedDialType -> Velocity Double
speedDialMaxV SpeedDial140 = 140 *~ kmh
speedDialMaxV SpeedDial180 = 180 *~ kmh
speedDialMaxV SpeedDial250 = 250 *~ kmh
speedDialMaxV SpeedDial400 = 400 *~ kmh



speedDialDegree :: SpeedDialType -> Velocity Double -> Dimensionless Double
speedDialDegree d v
  | v < (0 *~ kmh) = speedDialDegree d (0 *~ kmh)
  | v > speedDialMaxV d = speedDialDegree d $ speedDialMaxV d
  | otherwise = speedDialDegree' d v

speedDialDegree' :: SpeedDialType -> Velocity Double -> Dimensionless Double
speedDialDegree' SpeedDial400 v
  | v <= (200 *~ kmh) = ((0.96 *~ (degree / kmh)) * v) - (144 *~ degree)
  | v <= (400 *~ kmh) = ((0.48 *~ (degree / kmh)) * (v - (200 *~ kmh))) + (48 *~ degree)
speedDialDegree' d v =
  let a = (288 *~ degree) / speedDialMaxV d
  in (a * v) - (144 *~ degree)


uiColorStyle :: UIColor -> String
uiColorStyle c' =
  let c = uiColorCSS c'
  in mconcat [ "color: ", c, "; stroke: ", c, "; fill: ", c, ";" ]

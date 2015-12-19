{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module ETCS.DMI.Widgets.SpeedDial (SpeedDial, speedDial)  where

import           Control.Lens hiding ((*~))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           ETCS.DMI
import           GHCJS.DOM.CSSStyleDeclaration (setProperty)
import           GHCJS.DOM.Element (getStyle)
import           GHCJS.DOM.Node (setTextContent)
import           GHCJS.DOM.Types hiding (Event)
import           Numeric.Units.Dimensional.Prelude
import qualified Prelude as P
import           Reflex.Dom


data SpeedDial t = SpeedDial



speedDial :: (MonadHold t m, MonadWidget t m) => TrainBehavior t -> m (SpeedDial t)
speedDial tb = do
  _ <- buildSVGElement attrs $ do
    infs <- informationStatus tb
    pColor <- pointerColor tb (current infs)
    pIsRed <- mapDyn (== Red) pColor

    _ <- buildSpeedIndicators tb
    _ <- buildSpeedPointer tb pColor
    _ <- buildDigitalSpeed tb pIsRed
    
    return ()

  return SpeedDial
     where attrs :: AttributeMap
           attrs = Map.fromList $
                   [ ("viewbox", "0 0 280 300")
                   , ("width", "280px")
                   , ("height", "300px")
                   , ("class", "SpeedDial")
                   ]
                          


--
-- Digital Speed
--

buildDigitalSpeed ::
  (MonadHold t m, MonadWidget t m) => TrainBehavior t -> Dynamic t Bool -> m SVGGElement
buildDigitalSpeed tb isRed =
  let gAttrs = Map.fromList [("class", "DigitalSpeed")]
      delta_x :: Double -> Double
      delta_x j = (P.-) 160 . (P.*) j . (P./) 50 $ 3
      dAttrs x = Map.fromList [("x", show . delta_x $ x), ("y", "140")]
      iDigits :: (Integral i, Show i) => i -> (String,String,String)
      iDigits i =
        case show $ i `P.mod` 1000 of
        [c,b,a] -> ([c],[b],[a])
        [b,a] -> ("",[b],[a])
        [a] -> ("","",[a])
        _ -> ("","","")
  in do 
    bDigits <- mapDyn (\v -> iDigits ((round $ v /~ kmh) :: Int)) $
               tb ^. trainVelocity
    b0 <- nubDyn <$> mapDyn (\(_,_,a) -> a) bDigits
    b1 <- nubDyn <$> mapDyn (\(_,b,_) -> b) bDigits
    b2 <- nubDyn <$> mapDyn (\(c,_,_) -> c) bDigits

--    
    g <- buildSVGGroup gAttrs $
         let setD _d = liftIO . setTextContent _d . pure
             buildDigit n bn = do
               dn <- buildSVGText $ dAttrs n
               bn' <- sample . current $ bn
               setD dn bn'
               performEvent_ $ setD dn <$> updated bn
         in sequence_ [ buildDigit 0 b0, buildDigit 1 b1, buildDigit 2 b2]

    let setGCol r = setUiColor g $ if r then White else Black
    isRed' <- sample . current $ isRed
    setGCol isRed'
    performEvent_ $ setGCol  <$> updated isRed 
    
    return g




--
-- Speed Pointer
--

buildSpeedPointer ::
  (MonadWidget t m) => TrainBehavior t -> Dynamic t UIColor -> m SVGGElement
buildSpeedPointer tb col = do
  transformV <-  speedPointerTransform tb
  gAttrs <-
    mapDyn (\trV -> Map.fromList $
                    [("class","SpeedPointer"), ("transform", trV)]) transformV
  g <- buildSVGGroup gAttrs $ do
    _ <- buildSVGPolygon pAttributes
    _ <- buildSVGCircle cAttributes
    return ()
  performEvent_$ (\v -> liftIO $ setUiColor g $! v) <$> updated col
  c <- sample . current $ col
  liftIO $ setUiColor g c
  return g
    where cAttributes,pAttributes :: AttributeMap          
          cAttributes = Map.fromList [ ("cx", "105"), ("cy", "25"), ("r", "25") ]
          pAttributes = Map.fromList [ ("points", pPoints) ]
          pPoints = "0,24 15,24 23,20 80,20 80,29 23,29 15,26 0,26"




speedPointerTransform ::
  (Reflex t, MonadHold t m) => TrainBehavior t -> m (Dynamic t String)
speedPointerTransform tb =
  let degDyn = combineDyn speedDialDegree
      spTransformValue a = mconcat
        [ "translate(35, 115) rotate(", show $ (a + (90 *~ degree))  /~ degree, ",105,25) " ]
  in do
    deg <- degDyn (tb ^. trainSpeedDial) (tb ^. trainVelocity)    
    mapDyn spTransformValue (nubDyn deg)


setUiColor :: (MonadIO m, IsElement e) => e -> UIColor -> m ()
setUiColor e c' = do
  st' <- getStyle e
  flip (maybe (fail "unable to get stlye")) st' $ \st -> do
    setProperty st ("color" :: String) (pure $ uiColorCSS c') (mempty :: String)
    setProperty st ("stroke" :: String) (pure $ uiColorCSS c') (mempty :: String)
    setProperty st ("fill" :: String) (pure $ uiColorCSS c') (mempty :: String)



pointerColor ::
  (MonadWidget t m, Reflex t) =>
  TrainBehavior t -> Behavior t StatusInformation -> m (Dynamic t UIColor)
pointerColor tb infs =
  let m = tb ^. trainMode
      st = tb ^. sdmStatus
      v = tb ^. trainVelocity
      vperm = tb ^. sdmVperm
      vrelease = tb ^. sdmVrelease
      vtarget = tb ^. sdmVtarget
      es = mconcat [ const () <$> updated m
                   , const () <$> updated st
                   , const () <$> updated v
                   , const () <$> updated vperm
                   , const () <$> updated vrelease
                   , const () <$> updated vtarget       
                   ]      
      pointerColorB =
        pointerColor'
        <$> current m <*> current st <*> infs <*> current v
        <*> current vperm <*> current vrelease <*> current vtarget        
  in do
    aa <- sample pointerColorB
    nubDyn <$> holdDyn aa (fst <$> attach pointerColorB es)     
  where pointerColor' _ _ OvS _ _ _ _ = Orange
        pointerColor' _ _ WaS _ _ _ _ = Orange
        pointerColor' FS s i v p r t = pointerColor' OS s i v p r t
        pointerColor' _ CSM IntS v p _ _ =
          if 0 *~ kmh <= v && v <= p then Grey else Red
        pointerColor' OS PIM NoS v _ _ t  =
          if 0 *~ kmh <= v && v < t then Grey else White
        pointerColor' OS PIM IntS v p _ t
          | 0 *~ kmh <= v && v < t = Grey
          | t <= v && v <= p = White
          | otherwise = Red
        pointerColor' OS TSM NoS v _ _ t  =
          if 0 *~ kmh <= v && v < t then Grey else White
        pointerColor' OS TSM IndS _ _ _ _ = Yellow
        pointerColor' OS TSM IntS v p _ t
          | 0 *~ kmh <= v && v < t = Grey
          | t <= v && v <= p = Yellow
          | otherwise = Red
        pointerColor' _ RSM IndS _ _ _ _ = Yellow
        pointerColor' _ RSM IntS v _ (Just r) _ =
          if 0 *~ kmh <= v && v <= r then Yellow else Red
        pointerColor' LS CSM s v p r t = pointerColor' OS CSM s v p r t
        pointerColor' LS PIM IntS v p _ _ = if v <= p then Grey else Red
        pointerColor' LS TSM IntS v p _ _ = if v <= p then Grey else Red
        pointerColor' SR s i v p r t = pointerColor' UN s i v p r t
        pointerColor' UN PIM i v p r t = pointerColor' FS PIM i v p r t
        pointerColor' UN TSM i v p r t = pointerColor' FS TSM i v p r t
        pointerColor' SH s i v p r t = pointerColor' FS s i v p r t
        pointerColor' TR _ _ _ _ _ _  = Red
        pointerColor' _ _ _ _ _ _ _ = Grey



--
-- Indicators 
--

buildSpeedIndicators :: (MonadWidget t m) => TrainBehavior t -> m (Event t ())
buildSpeedIndicators tb = mapDyn buildSpeedIndicatorsSVG (tb ^. trainSpeedDial) >>= dyn 

buildSpeedIndicatorsSVG :: (MonadWidget t m) => SpeedDialType -> m ()
buildSpeedIndicatorsSVG sd = do
    _ <- buildLongSpeedIndicatorLines sd
    _ <- buildShortSpeedIndicatorLines sd
    _ <- buildSpeedIndicatorNumbers sd
    return ()



buildLongSpeedIndicatorLines :: (MonadWidget t m) => SpeedDialType -> m SVGGElement
buildLongSpeedIndicatorLines d =
  buildSVGGroup (mempty :: AttributeMap) $ 
    sequence_ $ buildSpeedIndicatorLine 25 d <$> speedIndicatorsLong d


buildShortSpeedIndicatorLines :: (MonadWidget t m) => SpeedDialType -> m SVGGElement
buildShortSpeedIndicatorLines d =
  buildSVGGroup (mempty :: AttributeMap) $ 
    sequence_ $ buildSpeedIndicatorLine 15 d <$> speedIndicatorsShort d


buildSpeedIndicatorNumbers :: (MonadWidget t m) => SpeedDialType -> m SVGGElement
buildSpeedIndicatorNumbers d =
  buildSVGGroup (Map.fromList [("class","SpeedIndicatorNumbers")]) $ 
   sequence_ $ buildSpeedIndicatorNumber d <$> speedIndicatorsNumber d

buildSpeedIndicatorNumber :: (MonadWidget t m) => SpeedDialType -> Velocity Double -> m ()
buildSpeedIndicatorNumber d v =
  let deg = speedDialDegree d v /~ degree
      deg1 = P.negate deg
      lbl = round (v /~ kmh) :: Int
      trans = mconcat [ "rotate(", show deg, ", 140, 140) "
                      , "rotate(", show deg1, ", 140, 52)" ]
  in do
    txt <- buildSVGText $
           Map.fromList [ ("x","140"),("y","52"), ("title", show lbl ++ " km/h")
                        , ("transform", trans )
                        ]
    liftIO . setTextContent txt . pure . show $ lbl


buildSpeedIndicatorLine ::
  MonadWidget t m => Double -> SpeedDialType -> Velocity Double -> m SVGLineElement
buildSpeedIndicatorLine len d v = 
  let deg = speedDialDegree d v /~ degree      
  in buildSVGLine . Map.fromList $
     [ ("x1", "140"), ("y1", "15"), ("x2", "140"), ("y2", show $ (P.+) len 15)
     , ("title", mconcat [ show (round (v /~ kmh) :: Int), " km/h" ])
     , ("transform", mconcat [ "rotate(", show deg, ", 140, 140)" ])
     ]



--
-- svg helpers - TODO: move away
--
    
svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

buildSVGLine :: (MonadWidget t m, Attributes m attrs) => attrs -> m SVGLineElement
buildSVGLine =
  liftM castToSVGLineElement . buildEmptyElementNS (pure svgNS) "line"

buildSVGText :: (MonadWidget t m, Attributes m attrs) => attrs -> m SVGTextElement
buildSVGText =
  liftM castToSVGTextElement . buildEmptyElementNS (pure svgNS) "text"


buildSVGPolygon :: (MonadWidget t m, Attributes m attrs) => attrs -> m SVGPolygonElement
buildSVGPolygon =
  liftM castToSVGPolygonElement . buildEmptyElementNS (pure svgNS) "polygon"


buildSVGCircle :: (MonadWidget t m, Attributes m attrs) => attrs -> m SVGCircleElement
buildSVGCircle =
  liftM castToSVGCircleElement . buildEmptyElementNS (pure svgNS) "circle"


buildSVGElement :: (MonadWidget t m, Attributes m attrs) =>
                   attrs -> m () ->  m SVGElement
buildSVGElement args =
  liftM (castToSVGElement . fst) . buildElementNS (pure svgNS) "svg" args


buildSVGGroup :: (MonadWidget t m, Attributes m attrs) =>
                   attrs -> m () ->  m SVGGElement
buildSVGGroup args =
  liftM (castToSVGGElement . fst) . buildElementNS (pure svgNS) "g" args


--
-- helpers
--


-- | get the "rgb(r,g,b)" css color value
uiColorCSS :: UIColor -> String
uiColorCSS Grey = "rgb(195, 195, 195)"
uiColorCSS Yellow = "rgb(223, 223, 0)"
uiColorCSS Orange = "rgb(234, 145, 0)"
uiColorCSS Red = "rgb(191, 0, 2)"
uiColorCSS White = "rgb(255,255,255)"
uiColorCSS Black = "rgb(0,0,0)"
uiColorCSS DarkGrey = "rgb(85,85,85)"
uiColorCSS MediumGrey = "rgb(150,150,150)"
uiColorCSS DarkBlue = "rgb(3, 17, 34)"


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

speedDialMaxV :: SpeedDialType -> Velocity Double
speedDialMaxV SpeedDial140 = 140 *~ kmh
speedDialMaxV SpeedDial180 = 180 *~ kmh
speedDialMaxV SpeedDial250 = 250 *~ kmh
speedDialMaxV SpeedDial400 = 400 *~ kmh



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




{-
import           Control.Monad
import           Control.Monad.Writer
import           Data.Maybe                           (fromMaybe, isJust)
import           ETCS.DMI.Helpers
import           ETCS.DMI.SDMData
import           ETCS.DMI.TrainBehavior
import           ETCS.DMI.Types
import           GHCJS.DOM.CSSStyleDeclaration        (setProperty)
import           GHCJS.DOM.Element                    (getStyle)
import           GHCJS.DOM.Element                    (setAttribute)
import           GHCJS.DOM.Node                       (appendChild,
                                                       setTextContent)
import           GHCJS.DOM.Types                      (IsDocument, IsElement,
                                                       IsNode, castToElement)

import           Reactive.Banana
import           Reactive.Banana.DOM
import           Reactive.Banana.DOM.Widget

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

    let tb = _speedDialTrainBehavior i
    infs <- lift . informationStatus $ tb

    let pColor = pointerColor tb infs
    let pIsRed = (== Red) <$> pColor

    void $ mkSubWidget svg $ MkSpeedIndicatorLines tb
    void $ mkSubWidget svg $ MkSpeedPointer tb pColor
    void $ mkSubWidget svg $ MkDigitalSpeed tb pIsRed
    void $ mkSubWidget svg $ MkBasicSpeedHooks tb
    void $ mkSubWidget svg $ MkCircularSpeedGauge tb infs

    void $ appendChild parent (pure container)
    return (SpeedDial, castToElement container)





data CircularSpeedGauge = CircularSpeedGauge

instance IsWidget CircularSpeedGauge where
  data WidgetInput CircularSpeedGauge =
    MkCircularSpeedGauge {
      _csgTrainBehavior :: TrainBehavior,
      _csgSuperVisionInformation :: Behavior StatusInformation
    }

  mkWidgetInstance parent i =
    let tb = _csgTrainBehavior i
        hasVrelease = isJust <$> (tb ^. trainSDMVrelease)
        justgt (Just vr) vp = vr > vp
        justgt _ _ = False
        vrGTvp = justgt <$> (tb ^. trainSDMVrelease) <*> (tb ^. trainSDMVperm)
        onVrGTVp b c = (\a -> if a then b else c) <$> vrGTvp
        c12outer = onVrGTVp 134 140
        c12width = onVrGTVp 3 9
        bshWidth = onVrGTVp 14 20
        color23 = csgColorMapping <$> (tb ^. trainSDMstatus)
                  <*> _csgSuperVisionInformation i
        colorC2 = fst <$> color23
        colorC3M = snd <$> color23
        colorC3 = fromMaybe DarkGrey <$> colorC3M
        c3Hidden = (== Nothing) <$> colorC3M
        r01end' (Just rl) pe = if rl > pe then pe else rl
        r01end' Nothing _ = 0 *~ kmh
        r01end = r01end' <$> (tb ^. trainSDMVrelease) <*> (tb ^. trainSDMVperm)
        r01Hidden = not <$> hasVrelease
        r2end' (Just rl) pe = if rl > pe then rl else pe
        r2end' Nothing pe = pe
        r2end = r2end' <$> (tb ^. trainSDMVrelease) <*> (tb ^. trainSDMVperm)
        r2Hidden = (==) <$> r2end <*> (tb ^. trainSDMVperm)
    in do
     doc <- _getOwnerDocument parent
     container <- _createSVGGElement doc

     void $ mkSubWidget container $ MkBasicCircular (pure 140) (pure 9)
       (pure $ (-149) *~ degree)
       (pure $ (-144) *~ degree)
       (pure DarkGrey)
     handleBehavior (not <$> tb ^. trainInMode FS) $ _setCSSHidden container

     void $ mkSubWidget container $ MkCircular tb c12outer c12width
       (pure $ 0 *~ kmh) (tb ^. trainSDMVtarget) (pure DarkGrey)

     void $ mkSubWidget container $ MkCircular tb c12outer c12width
       (tb ^. trainSDMVtarget) (tb ^. trainSDMVperm) colorC2

     c3 <- mkSubWidget container $ MkCircular tb (pure 140) (pure 20)
           (tb ^. trainSDMVperm) (tb ^. trainSDMVsbi) colorC3

     handleBehavior c3Hidden $ _setCSSHidden (widgetRoot c3)

     void $ mkSubWidget container $ MkBasicSpeedHook
       tb (tb ^. trainSDMVperm) colorC2 bshWidth

     r0  <- mkSubWidget container $
            MkCircular tb (pure 140)
            (pure 5) (pure $ 0 *~ kmh) r01end (pure MediumGrey)
     r1  <- mkSubWidget container $
            MkCircular tb (pure 136)
            (pure 1) (pure $ 0 *~ kmh) r01end (pure DarkBlue)
     r2  <- mkSubWidget container $
            MkCircular tb (pure 140)
            (pure 9) (tb ^. trainSDMVperm) r2end (pure MediumGrey)

     handleBehavior r01Hidden $ \a -> do
       _setCSSHidden (widgetRoot r0) a
       _setCSSHidden (widgetRoot r1) a

     handleBehavior r2Hidden $ _setCSSHidden (widgetRoot r2)

     void $ appendChild parent (pure container)
     return (CircularSpeedGauge, castToElement container)



csgColorMapping :: SuperVisionStatus -> StatusInformation -> (UIColor, Maybe UIColor)
csgColorMapping CSM NoS = (DarkGrey, Nothing)
csgColorMapping CSM OvS = (DarkGrey, Just Orange)
csgColorMapping CSM WaS = (DarkGrey, Just Orange)
csgColorMapping CSM IntS = (DarkGrey, Just Red)

csgColorMapping PIM NoS = (White, Nothing)
csgColorMapping PIM OvS = (White, Just Orange)
csgColorMapping PIM WaS = (White, Just Orange)
csgColorMapping PIM IntS = (White, Just Red)

csgColorMapping TSM NoS  = (White, Nothing)
csgColorMapping TSM IndS = (Yellow, Nothing)
csgColorMapping TSM OvS  = (Yellow, Just Orange)
csgColorMapping TSM WaS  = (Yellow, Just Orange)
csgColorMapping TSM IntS = (Yellow, Just Red)

csgColorMapping RSM IndS = (Yellow, Nothing)
csgColorMapping RSM IntS = (Yellow, Nothing)

csgColorMapping _ _ = (DarkBlue ,Nothing)


data BasicSpeedHooks = BasicSpeedHooks

instance IsWidget BasicSpeedHooks where
  data WidgetInput BasicSpeedHooks =
    MkBasicSpeedHooks {
      _bshsTrainBehavior :: TrainBehavior
      }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    container <- _createSVGGElement doc
    let
      td = _bshsTrainBehavior i
      mkbsh v c =
          mkSubWidget container $
          MkBasicSpeedHook { _bshTrainBehavior = td
                           , _bshVelocity = v
                           , _bshColor = c
                           , _bshWidth = pure 20
                           }
    vperm   <- mkbsh (td ^. trainSDMVperm) (pure White)
    vtarget <- mkbsh (td ^. trainSDMVtarget) (pure MediumGrey)

    let visi_perm =
          (td ^. trainInModes [OS, SR]) `bOr`
          (((== CSM) <$> td ^. trainSDMstatus) `bAnd` (td ^. trainInModes [SH, RV]))
    let visi_target =
          (td ^. trainInModes [OS, SR]) `bAnd` (((/=) CSM) <$> td ^. trainSDMstatus)

    handleBehavior visi_perm $ _setCSSHidden (widgetRoot vperm) . not
    handleBehavior visi_target $ _setCSSHidden (widgetRoot vtarget) . not

    void $ appendChild parent (pure container)
    return (BasicSpeedHooks, castToElement container)


data BasicSpeedHook = BasicSpeedHook

instance IsWidget BasicSpeedHook where
  data WidgetInput BasicSpeedHook =
    MkBasicSpeedHook {
      _bshTrainBehavior :: TrainBehavior,
      _bshVelocity :: Behavior (Velocity Double),
      _bshColor :: Behavior UIColor,
      _bshWidth :: Behavior Double
    }
  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    p <- _createSVGPathElement doc

    -- definition binding
    let bshDef w =
          let a = (P.-) 20 w
          in mconcat ["M140,", show a,  " L140,20 L134,20 L134,", show a]
    handleBehavior (bshDef <$> _bshWidth i) $ setAttribute p "d"

    -- speed binding
    let bV = speedDialDegree
             <$> _bshTrainBehavior i ^. trainSpeedDial <*> _bshVelocity i
    handleBehavior bV $ \a ->
      setAttribute p "transform" . mconcat $
        [ "rotate(", show (a /~ degree), ", 140, 140)" ]

    -- color binding
    handleBehavior (_bshColor i) $ setUiColor p

    void $ appendChild parent (pure p)
    return (BasicSpeedHook, castToElement p)



data Circular = Circular

instance IsWidget Circular where
  data WidgetInput Circular =
    MkCircular {
      _cirTrainBehavior :: TrainBehavior,
      _cirOuter :: Behavior Double,
      _cirWidth :: Behavior Double,
      _cirStart :: Behavior (Velocity Double),
      _cirEnd   :: Behavior (Velocity Double),
      _cirColor :: Behavior UIColor
      }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    container <- _createSVGGElement doc

    let s = speedDialDegree <$> _cirTrainBehavior i ^. trainSpeedDial <*> _cirStart i
        e = speedDialDegree <$> _cirTrainBehavior i ^. trainSpeedDial <*> _cirEnd i
    void $ mkSubWidget container $ MkBasicCircular (_cirOuter i) (_cirWidth i)
      s e (_cirColor i)

    void $ appendChild parent (pure container)
    return (Circular, castToElement container)





data BasicCircular = BasicCircular

instance IsWidget BasicCircular where
  data WidgetInput BasicCircular =
    MkBasicCircular {
      _bcirOuter :: Behavior Double,
      _bcirWidth :: Behavior Double,
      _bcirStart :: Behavior (PlaneAngle Double),
      _bcirEnd   :: Behavior (PlaneAngle Double),
      _bcirColor :: Behavior UIColor
      }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    p <- _createSVGPathElement doc

    let bD = circularDef <$> _bcirOuter i <*> _bcirWidth i <*> _bcirStart i <*> _bcirEnd i
    handleBehavior bD $ setAttribute p "d"

    -- color binding
    handleBehavior (_bcirColor i) $ setUiColor p

    void $ appendChild parent (pure p)
    return (BasicCircular, castToElement p)


circularDef :: Double -> Double -> PlaneAngle Double -> PlaneAngle Double -> String
circularDef outer width a b =
  let cox = outer *~ one
      cix = cox - (width *~ one)
      ri = cix /~ one
      ro = cox /~ one
      a' = a * ((-1) *~ one)
      b' = b * ((-1) *~ one)
      sina = sin a'
      cosa = cos a'
      sinb = sin b'
      cosb = cos b'
      _140 = 140 *~ one
      (ix0, iy0) = (_140 - sina * cix, _140 - cosa * cix)
      (ox0, oy0) = (_140 - sina * cox, _140 - cosa * cox)
      (ix1, iy1) = (_140 - sinb * cix, _140 - cosb * cix)
      (ox1, oy1) = (_140 - sinb * cox, _140 - cosb * cox)

  in mconcat
     [  "M", show ix0, ",", show iy0
     , " L", show ox0, ",", show oy0
     , " A ", show ro, " ", show ro, " 0 0 1", show ox1, ",", show oy1
     , " L", show ix1, ",", show iy1
     , " A ", show ri, " ", show ri, " 0 0 0", show ix0, ",", show iy0
     ]


data SpeedIndicatorLines = SpeedIndicatorLines

instance IsWidget SpeedIndicatorLines where
  data WidgetInput SpeedIndicatorLines =
    MkSpeedIndicatorLines {
      _speedIndicatorLinesTrainBehavior :: TrainBehavior
    }

  mkWidgetInstance parent i = do
    doc <- _getOwnerDocument parent
    container <- _createSVGGElement doc

    let bSD = _speedIndicatorLinesTrainBehavior i ^. trainSpeedDial
    handleBehavior bSD $ \d -> do
      deleteChildNodes container
      mkShortSpeedIndicatorLines doc container d
      mkLongSpeedIndicatorLines doc container d
      mkSpeedIndicatorNumbers doc container d

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


    handleBehavior (_digitalSpeedIsRed i) $ \isRed ->
      setUiColor container $ if isRed then White else Black

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

    handleBehavior b0 $ setTextContent d0 . pure
    handleBehavior b1 $ setTextContent d1 . pure
    handleBehavior b2 $ setTextContent d2 . pure


    void $ appendChild container (pure d0)
    void $ appendChild container (pure d1)
    void $ appendChild container (pure d2)
    void $ appendChild parent (pure container)

    return (DigitalSpeed, castToElement container)

iDigits :: (Integral i, Show i) => i -> (String,String,String)
iDigits i =
  case show $ i `P.mod` 1000 of
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

    handleBehavior (_speedPointerColor i) $ setUiColor container

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
    handleBehavior bPointer $ setSpeedPointer container

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

setUiColor :: (MonadIO m, IsElement e) => e -> UIColor -> m ()
setUiColor e c' = do
  st' <- getStyle e
  flip (maybe (fail "unable to get stlye")) st' $ \st -> do
    setProperty st ("color" :: String) (pure $ uiColorCSS c') (mempty :: String)
    setProperty st ("stroke" :: String) (pure $ uiColorCSS c') (mempty :: String)
    setProperty st ("fill" :: String) (pure $ uiColorCSS c') (mempty :: String)


-}

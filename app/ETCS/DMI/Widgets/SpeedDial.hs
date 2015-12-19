{-# LANGUAGE Trustworthy #-}

module ETCS.DMI.Widgets.SpeedDial (speedDial)  where

import           Control.Lens hiding ((*~))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           Data.Maybe
import           ETCS.DMI
import           GHCJS.DOM.CSSStyleDeclaration (setProperty)
import           GHCJS.DOM.Element (getStyle)
import           GHCJS.DOM.Node (setTextContent)
import           GHCJS.DOM.Types hiding (Event)
import           Numeric.Units.Dimensional.Prelude
import qualified Prelude as P
import           Reflex.Dom


-- | build a SpeedDial widget
speedDial :: (MonadHold t m, MonadWidget t m) => TrainBehavior t -> m SVGElement
speedDial tb = do
  svg <- buildSVGElement attrs $ do
    infs <- informationStatus tb
    pColor <- pointerColor tb (current infs)
    pIsRed <- mapDyn (== Red) pColor

    void $ buildSpeedIndicators tb
    void $ buildSpeedPointer tb pColor
    void $ buildDigitalSpeed tb pIsRed
    void $ buildBasicSpeedHooks tb
    void $ buildCircularSpeedGauge tb infs
    
  return svg
     where attrs :: AttributeMap
           attrs = Map.fromList $
                   [ ("viewbox", "0 0 280 300")
                   , ("width", "280px")
                   , ("height", "300px")
                   , ("class", "SpeedDial")
                   ]
                          


--
-- Basic Speed Hooks
--

buildBasicSpeedHooks ::
  (MonadHold t m, MonadWidget t m) => TrainBehavior t -> m SVGGElement
buildBasicSpeedHooks tb =
  let gattrs = Map.fromList [("class","BasicSpeedHooks")]
  in do
    inCSM <- mapDyn (== CSM) $ tb ^. sdmStatus
    inOSSR <- trainInModes [OS,SR] tb
    inSHRV <- trainInModes [SH,RV] tb
    visi_perm <- nubDyn <$> dynAnd inCSM inSHRV >>= dynOr inOSSR
    visi_target <- nubDyn <$> mapDyn not inCSM >>= dynAnd inOSSR
    buildSVGGroup gattrs $ do
      vperm <- buildBasicSpeedHook tb (tb ^. sdmVperm) (constDyn White) (constDyn 20)
      visi_perm' <- sample . current $ visi_perm
      liftIO . setCssHidden vperm . not $ visi_perm'      
      performEvent_ $ setCssHidden vperm . not <$> updated visi_perm
      vtarget <- buildBasicSpeedHook tb (tb ^. sdmVtarget) (constDyn MediumGrey) (constDyn 20)
      visi_target' <- sample . current $ visi_target
      liftIO . setCssHidden vtarget . not $ visi_target'      
      performEvent_ $ setCssHidden vtarget . not <$> updated visi_target


--
-- Basic Speed Hook
--

buildBasicSpeedHook ::
  (MonadHold t m, MonadWidget t m) => TrainBehavior t -> Dynamic t (Velocity Double) -> Dynamic t UIColor -> Dynamic t Int -> m SVGGElement
buildBasicSpeedHook tb v c wi =
  let gattrs = Map.fromList [("class","BasicSpeedHook")]
      bshDef :: Int -> String
      bshDef w =
        let a = (P.-) 20 w
        in mconcat ["M140,", show a,  " L140,20 L134,20 L134,", show a]
      transformDef a = mconcat [ "rotate(", show (a /~ degree), ", 140, 140)" ]
      pathAttrs' w a =
         Map.fromList [("d", bshDef w), ("transform", transformDef a)] 
  in do
    g <- buildSVGGroup gattrs $ do
      aD <- combineDyn speedDialDegree (tb ^. trainSpeedDial) v
      pathAttrs <- combineDyn pathAttrs' wi aD
      void $ buildSVGPath pathAttrs
      return ()

    c' <- sample . current $ c
    setUiColor g c'
    performEvent_ $ setUiColor g <$> updated c 

    return g


--
-- Circular Speed Gauge
--

buildCircularSpeedGauge ::
  (MonadHold t m, MonadWidget t m) =>
  TrainBehavior t -> Dynamic t StatusInformation -> m SVGGElement
buildCircularSpeedGauge tb infs =
  let gattrs = Map.fromList [("class","CircularSpeedGauge")]
      justgt (Just vr) vp = vr > vp
      justgt _ _ = False
      r01end' (Just rl) pe = if rl > pe then pe else rl
      r01end' Nothing _ = 0 *~ kmh
      r2end' (Just rl) pe = if rl > pe then rl else pe
      r2end' Nothing pe = pe
  in do
    hasVrelease <- mapDyn isJust $ tb ^. sdmVrelease
    vrGTvp <- combineDyn justgt (tb ^. sdmVrelease) (tb ^. sdmVperm)
    c12outer <- mapDyn (\a -> if a then 134 else 140 :: Int) vrGTvp
    c12width <- mapDyn (\a -> if a then 3 else 9 :: Int) vrGTvp
    bshWidth <- mapDyn (\a -> if a then 14 else 20 :: Int) vrGTvp
    color23 <- combineDyn csgColorMapping (tb ^. sdmStatus) infs
    colorC2 <- mapDyn fst color23
    colorC3M <- mapDyn snd color23
    colorC3 <- mapDyn (fromMaybe DarkGrey) colorC3M
    c3Hidden <- mapDyn isNothing colorC3M
    r01end <- combineDyn r01end' (tb ^. sdmVrelease) (tb ^. sdmVperm)
    r01Hidden <- mapDyn not hasVrelease
    r2end <- combineDyn r2end' (tb ^. sdmVrelease) (tb ^. sdmVperm)
    r2Hidden <- combineDyn (==) r2end (tb ^. sdmVperm)
    
    g <- buildSVGGroup gattrs $ do
      void $ buildBasicCircular (constDyn 140) (constDyn 9)
        (constDyn $ (-149) *~ degree)
        (constDyn $ (-144) *~ degree)
        (constDyn DarkGrey)

      void $ buildCircular tb c12outer c12width
        (constDyn $ 0*~ kmh) (tb ^. sdmVtarget) (constDyn DarkGrey)

      void $ buildCircular tb c12outer c12width
        (tb ^. sdmVtarget) (tb ^. sdmVperm) colorC2

      c3 <- buildCircular tb (constDyn 140) (constDyn 20)
            (tb ^. sdmVperm) (tb ^. sdmVsbi)colorC3
    
      c3Hidden' <- sample . current $ c3Hidden
      liftIO . setCssHidden c3 $ c3Hidden'      
      performEvent_ $ setCssHidden c3 <$> updated c3Hidden

      void $ buildBasicSpeedHook tb (tb ^. sdmVperm) colorC2 bshWidth

      r0 <- buildCircular tb (constDyn 140) (constDyn 5) (constDyn $ 0 *~kmh)
            r01end (constDyn MediumGrey)

      r1 <- buildCircular tb (constDyn 136) (constDyn 1) (constDyn $ 0 *~kmh)
            r01end (constDyn DarkBlue)

      r2 <- buildCircular tb (constDyn 140) (constDyn 5) (constDyn $ 0 *~kmh)
            r01end (constDyn MediumGrey)

      let hideR01 a = sequence_ [setCssHidden r0 a, setCssHidden r1 a]
      r01Hidden' <- sample . current $ r01Hidden
      liftIO $ hideR01 r01Hidden'
      performEvent_ $ hideR01 <$> updated r01Hidden

      r2Hidden' <- sample . current $ r2Hidden
      liftIO $ setCssHidden r2 r2Hidden'
      performEvent_ $ setCssHidden r2 <$> updated r2Hidden

      return ()

    inFSMode <- trainInMode FS tb
    inFSMode' <- sample . current $ inFSMode
    liftIO . setCssHidden g . not $ inFSMode'      
    performEvent_ $ setCssHidden g . not <$> updated inFSMode

    
    return g           



--
-- Basic Circular
-- 

buildCircular ::
  (MonadHold t m, MonadWidget t m) =>
  TrainBehavior t -> Dynamic t Int -> Dynamic t Int -> Dynamic t (Velocity Double) -> Dynamic t (Velocity Double) -> Dynamic t UIColor -> m SVGPathElement
buildCircular tb outer width start end c = do
  start' <- combineDyn speedDialDegree (tb ^. trainSpeedDial) start
  end'   <- combineDyn speedDialDegree (tb ^. trainSpeedDial) end
  buildBasicCircular outer width start' end' c

buildBasicCircular ::
  (MonadHold t m, MonadWidget t m) =>
  Dynamic t Int -> Dynamic t Int -> Dynamic t (PlaneAngle Double) -> Dynamic t (PlaneAngle Double) -> Dynamic t UIColor -> m SVGPathElement
buildBasicCircular outer width start end c =
  let attrs' a b = Map.fromList [("d", circularDef a b)]
  in do
    ow <- combineDyn (,) outer width
    se <- combineDyn (,) start end
    attrs <- combineDyn attrs' ow se
    path <- buildSVGPath attrs

    
    c' <- sample . current $ c
    liftIO $ setUiColor path c'
    performEvent_ $ setUiColor path  <$> updated c

    return path

    
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
    void $ buildSVGPolygon pAttributes
    void $ buildSVGCircle cAttributes
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
    void $ buildLongSpeedIndicatorLines sd
    void $ buildShortSpeedIndicatorLines sd
    void $ buildSpeedIndicatorNumbers sd



buildLongSpeedIndicatorLines :: (MonadWidget t m) => SpeedDialType -> m SVGGElement
buildLongSpeedIndicatorLines d =
  buildSVGGroup (Map.fromList [("class","LongSpeedIndicatorLines")]) $ 
    sequence_ $ buildSpeedIndicatorLine 25 d <$> speedIndicatorsLong d


buildShortSpeedIndicatorLines :: (MonadWidget t m) => SpeedDialType -> m SVGGElement
buildShortSpeedIndicatorLines d =
  buildSVGGroup (Map.fromList [("class","ShortSpeedIndicatorLines")]) $ 
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

buildSVGPath :: (MonadWidget t m, Attributes m attrs) => attrs -> m SVGPathElement
buildSVGPath =
  liftM castToSVGPathElement . buildEmptyElementNS (pure svgNS) "path"


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



setCssHidden :: (MonadIO m, IsElement e) => e -> Bool -> m ()
setCssHidden e h = do
  st' <- getStyle e
  flip (maybe (fail "unable to get stlye")) st' $ \st ->
    let v :: String
        v = if h then "none" else "initial"
    in setProperty st ("display" :: String) (pure v) (mempty :: String)


--
-- helpers
--

circularDef :: (Int, Int) -> (PlaneAngle Double, PlaneAngle Double) -> String
circularDef (o, w) (a, b) = circularDef' (fromIntegral o) (fromIntegral w) a b

circularDef' :: Double -> Double -> PlaneAngle Double -> PlaneAngle Double -> String
circularDef' outer width a b =
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

module DTD_SMIL20 where

import Xml2Haskell


{-Type decls-}

data Smil = Smil
    { smilId :: (Maybe Id)
    , smilClass :: (Maybe String)
    , smilTitle :: (Maybe String)
    , smilXml'lang :: (Maybe Xml'lang)
    , smilXmlns :: (Defaultable String)
    } deriving (Eq,Show)
data Head = Head
    { headId :: (Maybe Id)
    , headClass :: (Maybe String)
    , headTitle :: (Maybe String)
    , headXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Body = Body
    { bodyId :: (Maybe Id)
    , bodyClass :: (Maybe String)
    , bodyTitle :: (Maybe String)
    , bodyXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Animate = Animate
    { animateId :: (Maybe Id)
    , animateClass :: (Maybe String)
    , animateTitle :: (Maybe String)
    , animateXml'lang :: (Maybe Xml'lang)
    , animateCustomTest :: (Maybe CustomTest)
    , animateSystemBitrate :: (Maybe String)
    , animateSystemCaptions :: (Maybe SystemCaptions)
    , animateSystemLanguage :: (Maybe String)
    , animateSystemOverdubOrSubtitle :: (Maybe SystemOverdubOrSubtitle)
    , animateSystemRequired :: (Maybe SystemRequired)
    , animateSystemScreenSize :: (Maybe String)
    , animateSystemScreenDepth :: (Maybe String)
    , animateSystemAudioDesc :: (Maybe SystemAudioDesc)
    , animateSystemOperatingSystem :: (Maybe SystemOperatingSystem)
    , animateSystemCPU :: (Maybe SystemCPU)
    , animateSystemComponent :: (Maybe String)
    , animateSystem_bitrate :: (Maybe String)
    , animateSystem_captions :: (Maybe System_captions)
    , animateSystem_language :: (Maybe String)
    , animateSystem_overdub_or_caption :: (Maybe System_overdub_or_caption)
    , animateSystem_required :: (Maybe System_required)
    , animateSystem_screen_size :: (Maybe String)
    , animateSystem_screen_depth :: (Maybe String)
    , animateDur :: (Maybe String)
    , animateRepeatCount :: (Maybe String)
    , animateRepeatDur :: (Maybe String)
    , animateBegin :: (Maybe String)
    , animateEnd :: (Maybe String)
    , animateAttributeName :: String
    , animateAttributeType :: (Maybe String)
    , animateValues :: (Maybe String)
    , animateFrom :: (Maybe String)
    , animateTo :: (Maybe String)
    , animateBy :: (Maybe String)
    , animateCalcMode :: (Defaultable CalcMode)
    , animateAdditive :: (Defaultable Additive)
    , animateAccumulate :: (Defaultable Accumulate)
    } deriving (Eq,Show)
data SystemCaptions = On  |  Off
		    deriving (Eq,Show)
data SystemOverdubOrSubtitle = Overdub  |  Subtitle
			     deriving (Eq,Show)
data SystemAudioDesc = On  |  Off
		     deriving (Eq,Show)
data System_captions = On  |  Off
		     deriving (Eq,Show)
data System_overdub_or_caption = Overdub  |  Caption
			       deriving (Eq,Show)
data CalcMode = Discrete  |  Linear  |  Paced
	      deriving (Eq,Show)
data Additive = Replace  |  Sum
	      deriving (Eq,Show)
data Accumulate = None  |  Sum
		deriving (Eq,Show)
data Set = Set
    { setId :: (Maybe Id)
    , setClass :: (Maybe String)
    , setTitle :: (Maybe String)
    , setXml'lang :: (Maybe Xml'lang)
    , setCustomTest :: (Maybe CustomTest)
    , setSystemBitrate :: (Maybe String)
    , setSystemCaptions :: (Maybe SystemCaptions)
    , setSystemLanguage :: (Maybe String)
    , setSystemOverdubOrSubtitle :: (Maybe SystemOverdubOrSubtitle)
    , setSystemRequired :: (Maybe SystemRequired)
    , setSystemScreenSize :: (Maybe String)
    , setSystemScreenDepth :: (Maybe String)
    , setSystemAudioDesc :: (Maybe SystemAudioDesc)
    , setSystemOperatingSystem :: (Maybe SystemOperatingSystem)
    , setSystemCPU :: (Maybe SystemCPU)
    , setSystemComponent :: (Maybe String)
    , setSystem_bitrate :: (Maybe String)
    , setSystem_captions :: (Maybe System_captions)
    , setSystem_language :: (Maybe String)
    , setSystem_overdub_or_caption :: (Maybe System_overdub_or_caption)
    , setSystem_required :: (Maybe System_required)
    , setSystem_screen_size :: (Maybe String)
    , setSystem_screen_depth :: (Maybe String)
    , setDur :: (Maybe String)
    , setRepeatCount :: (Maybe String)
    , setRepeatDur :: (Maybe String)
    , setBegin :: (Maybe String)
    , setEnd :: (Maybe String)
    , setAttributeName :: String
    , setAttributeType :: (Maybe String)
    , setTo :: (Maybe String)
    } deriving (Eq,Show)
data AnimateMotion = AnimateMotion
    { animateMotionId :: (Maybe Id)
    , animateMotionClass :: (Maybe String)
    , animateMotionTitle :: (Maybe String)
    , animateMotionXml'lang :: (Maybe Xml'lang)
    , animateMotionCustomTest :: (Maybe CustomTest)
    , animateMotionSystemBitrate :: (Maybe String)
    , animateMotionSystemCaptions :: (Maybe SystemCaptions)
    , animateMotionSystemLanguage :: (Maybe String)
    , animateMotionSystemOverdubOrSubtitle :: (Maybe SystemOverdubOrSubtitle)
    , animateMotionSystemRequired :: (Maybe SystemRequired)
    , animateMotionSystemScreenSize :: (Maybe String)
    , animateMotionSystemScreenDepth :: (Maybe String)
    , animateMotionSystemAudioDesc :: (Maybe SystemAudioDesc)
    , animateMotionSystemOperatingSystem :: (Maybe SystemOperatingSystem)
    , animateMotionSystemCPU :: (Maybe SystemCPU)
    , animateMotionSystemComponent :: (Maybe String)
    , animateMotionSystem_bitrate :: (Maybe String)
    , animateMotionSystem_captions :: (Maybe System_captions)
    , animateMotionSystem_language :: (Maybe String)
    , animateMotionSystem_overdub_or_caption :: (Maybe System_overdub_or_caption)
    , animateMotionSystem_required :: (Maybe System_required)
    , animateMotionSystem_screen_size :: (Maybe String)
    , animateMotionSystem_screen_depth :: (Maybe String)
    , animateMotionDur :: (Maybe String)
    , animateMotionRepeatCount :: (Maybe String)
    , animateMotionRepeatDur :: (Maybe String)
    , animateMotionBegin :: (Maybe String)
    , animateMotionEnd :: (Maybe String)
    , animateMotionValues :: (Maybe String)
    , animateMotionFrom :: (Maybe String)
    , animateMotionTo :: (Maybe String)
    , animateMotionBy :: (Maybe String)
    , animateMotionCalcMode :: (Defaultable CalcMode)
    , animateMotionAdditive :: (Defaultable Additive)
    , animateMotionAccumulate :: (Defaultable Accumulate)
    , animateMotionOrigin :: (Defaultable Origin)
    } deriving (Eq,Show)
data Origin = Default
	    deriving (Eq,Show)
data AnimateColor = AnimateColor
    { animateColorId :: (Maybe Id)
    , animateColorClass :: (Maybe String)
    , animateColorTitle :: (Maybe String)
    , animateColorXml'lang :: (Maybe Xml'lang)
    , animateColorCustomTest :: (Maybe CustomTest)
    , animateColorSystemBitrate :: (Maybe String)
    , animateColorSystemCaptions :: (Maybe SystemCaptions)
    , animateColorSystemLanguage :: (Maybe String)
    , animateColorSystemOverdubOrSubtitle :: (Maybe SystemOverdubOrSubtitle)
    , animateColorSystemRequired :: (Maybe SystemRequired)
    , animateColorSystemScreenSize :: (Maybe String)
    , animateColorSystemScreenDepth :: (Maybe String)
    , animateColorSystemAudioDesc :: (Maybe SystemAudioDesc)
    , animateColorSystemOperatingSystem :: (Maybe SystemOperatingSystem)
    , animateColorSystemCPU :: (Maybe SystemCPU)
    , animateColorSystemComponent :: (Maybe String)
    , animateColorSystem_bitrate :: (Maybe String)
    , animateColorSystem_captions :: (Maybe System_captions)
    , animateColorSystem_language :: (Maybe String)
    , animateColorSystem_overdub_or_caption :: (Maybe System_overdub_or_caption)
    , animateColorSystem_required :: (Maybe System_required)
    , animateColorSystem_screen_size :: (Maybe String)
    , animateColorSystem_screen_depth :: (Maybe String)
    , animateColorDur :: (Maybe String)
    , animateColorRepeatCount :: (Maybe String)
    , animateColorRepeatDur :: (Maybe String)
    , animateColorBegin :: (Maybe String)
    , animateColorEnd :: (Maybe String)
    , animateColorAttributeName :: String
    , animateColorAttributeType :: (Maybe String)
    , animateColorValues :: (Maybe String)
    , animateColorFrom :: (Maybe String)
    , animateColorTo :: (Maybe String)
    , animateColorBy :: (Maybe String)
    , animateColorCalcMode :: (Defaultable CalcMode)
    , animateColorAdditive :: (Defaultable Additive)
    , animateColorAccumulate :: (Defaultable Accumulate)
    } deriving (Eq,Show)
data Switch = Switch
    { switchId :: (Maybe Id)
    , switchClass :: (Maybe String)
    , switchTitle :: (Maybe String)
    , switchXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Meta = Meta
    { metaContent :: (Maybe String)
    , metaName :: String
    } deriving (Eq,Show)
data Metadata = Metadata
    { metadataId :: (Maybe Id)
    , metadataClass :: (Maybe String)
    , metadataTitle :: (Maybe String)
    , metadataXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Layout = Layout
    { layoutId :: (Maybe Id)
    , layoutClass :: (Maybe String)
    , layoutTitle :: (Maybe String)
    , layoutXml'lang :: (Maybe Xml'lang)
    , layoutType :: (Defaultable String)
    } deriving (Eq,Show)
data Region = Region
    { regionId :: (Maybe Id)
    , regionClass :: (Maybe String)
    , regionTitle :: (Maybe String)
    , regionXml'lang :: (Maybe Xml'lang)
    , regionHeight :: (Defaultable String)
    , regionWidth :: (Defaultable String)
    , regionClose :: (Defaultable Close)
    , regionOpen :: (Defaultable Open)
    , regionBackgroundColor :: (Maybe String)
    , regionBackground_color :: (Maybe String)
    , regionBottom :: (Defaultable String)
    , regionLeft :: (Defaultable String)
    , regionRight :: (Defaultable String)
    , regionTop :: (Defaultable String)
    , regionZ_index :: (Maybe String)
    , regionShowBackground :: (Defaultable ShowBackground)
    , regionFit :: (Defaultable Fit)
    } deriving (Eq,Show)
data Close = Never  |  WhenNotActive
	   deriving (Eq,Show)
data Open = Always  |  WhenActive
	  deriving (Eq,Show)
data ShowBackground = Always  |  WhenActive
		    deriving (Eq,Show)
data Fit = Hidden  |  Fill  |  Meet  |  Scroll  | 
	   Slice
	 deriving (Eq,Show)
data Root_layout = Root_layout
    { root_layoutId :: (Maybe Id)
    , root_layoutClass :: (Maybe String)
    , root_layoutTitle :: (Maybe String)
    , root_layoutXml'lang :: (Maybe Xml'lang)
    , root_layoutHeight :: (Defaultable String)
    , root_layoutWidth :: (Defaultable String)
    , root_layoutClose :: (Defaultable Close)
    , root_layoutOpen :: (Defaultable Open)
    , root_layoutBackgroundColor :: (Maybe String)
    , root_layoutBackground_color :: (Maybe String)
    } deriving (Eq,Show)
data Ref = Ref
    { refId :: (Maybe Id)
    , refClass :: (Maybe String)
    , refTitle :: (Maybe String)
    , refXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Audio = Audio
    { audioId :: (Maybe Id)
    , audioClass :: (Maybe String)
    , audioTitle :: (Maybe String)
    , audioXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Img = Img
    { imgId :: (Maybe Id)
    , imgClass :: (Maybe String)
    , imgTitle :: (Maybe String)
    , imgXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Video = Video
    { videoId :: (Maybe Id)
    , videoClass :: (Maybe String)
    , videoTitle :: (Maybe String)
    , videoXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Text = Text
    { textId :: (Maybe Id)
    , textClass :: (Maybe String)
    , textTitle :: (Maybe String)
    , textXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Textstream = Textstream
    { textstreamId :: (Maybe Id)
    , textstreamClass :: (Maybe String)
    , textstreamTitle :: (Maybe String)
    , textstreamXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Animation = Animation
    { animationId :: (Maybe Id)
    , animationClass :: (Maybe String)
    , animationTitle :: (Maybe String)
    , animationXml'lang :: (Maybe Xml'lang)
    } deriving (Eq,Show)
data Transition = Transition
    { transitionId :: (Maybe Id)
    , transitionClass :: (Maybe String)
    , transitionTitle :: (Maybe String)
    , transitionXml'lang :: (Maybe Xml'lang)
    , transitionType :: (Maybe Type)
    , transitionSubtype :: (Maybe Subtype)
    , transitionHorzRepeat :: (Defaultable String)
    , transitionVertRepeat :: (Defaultable String)
    , transitionBorderWidth :: (Defaultable String)
    , transitionBorderColor :: (Defaultable String)
    , transitionFadeColor :: (Defaultable String)
    , transitionCoordinated :: (Defaultable Coordinated)
    , transitionClibBoundary :: (Defaultable ClibBoundary)
    , transitionDur :: (Maybe String)
    , transitionStartProgress :: (Defaultable String)
    , transitionEndProgress :: (Defaultable String)
    , transitionDirection :: (Defaultable Direction)
    } deriving (Eq,Show)
data Type = BarWipe  |  BoxWipe  |  FourBoxWipe  | 
	    BarnDoorWipe  |  DiagonalWipe  |  BowTieWipe  | 
	    MiscDiagonalWipe  |  VeeWipe  |  BarnVeeWipe  | 
	    ZigZagWipe  |  BarnZigZagWipe  |  MiscShapeWipe  | 
	    TriangleWipe  |  ArrowHeadWipe  |  PentagonWipe  | 
	    HexagonWipe  |  EllipseWipe  |  EyeWipe  | 
	    RoundRectWipe  |  StarWipe  |  ClockWipe  | 
	    PinWheelWipe  |  SingleSweepWipe  |  FanWipe  | 
	    DoubleFanWipe  |  DoubleSweepWipe  |  SaloonDoorWipe
	     |  WindshieldWipe  |  SnakeWipe  |  SpiralWipe  | 
	    ParallelSnakesWipe  |  BoxSnakesWipe  | 
	    WaterfallWipe  |  PushWipe  |  SlideWipe  |  Fade
	  deriving (Eq,Show)
data Subtype = Bottom  |  BottomCenter  |  BottomLeft
	        |  BottomLeftClockwise  | 
	       BottomLeftCounterClockwise  |  BottomLeftDiagonal  | 
	       BottomRight  |  BottomRightClockwise  | 
	       BottomRightCounterClockwise  |  BottomRightDiagonal
	        |  CenterRight  |  CenterTop  |  Circle  | 
	       ClockwiseBottom  |  ClockwiseBottomRight  | 
	       ClockwiseLeft  |  ClockwiseNine  |  ClockwiseRight
	        |  ClockwiseSix  |  ClockwiseThree  |  ClockwiseTop
	        |  ClockwiseTopLeft  |  ClockwiseTwelve  | 
	       CornersIn  |  CornersOut  | 
	       CounterClockwiseBottomLeft  | 
	       CounterClockwiseTopRight  |  Crossfade  | 
	       DiagonalBottomLeft  |  DiagonalBottomLeftOpposite  | 
	       DiagonalTopLeft  |  DiagonalTopLeftOpposite  | 
	       Diamond  |  DoubleBarnDoor  |  DoubleDiamond  |  Down
	        |  FadeFromColor  |  FadeToColor  |  FanInHorizontal
	        |  FanInVertical  |  FanOutHorizontal  | 
	       FanOutVertical  |  FivePoint  |  FourBlade  | 
	       FourBoxHorizontal  |  FourBoxVertical  |  FourPoint
	        |  FromBottom  |  FromLeft  |  FromRight  |  FromTop
	        |  Heart  |  Horizontal  |  HorizontalLeft  | 
	       HorizontalLeftSame  |  HorizontalRight  | 
	       HorizontalRightSame  |  HorizontalTopLeftOpposite  | 
	       HorizontalTopRightOpposite  |  Keyhole  |  Left  | 
	       LeftCenter  |  LeftToRight  |  OppositeHorizontal  | 
	       OppositeVertical  |  ParallelDiagonal  | 
	       ParallelDiagonalBottomLeft  | 
	       ParallelDiagonalTopLeft  |  ParallelVertical  | 
	       Rectangle  |  Right  |  RightCenter  |  SixPoint  | 
	       Top  |  TopCenter  |  TopLeft  |  TopLeftClockwise
	        |  TopLeftCounterClockwise  |  TopLeftDiagonal  | 
	       TopLeftHorizontal  |  TopLeftVertical  |  TopRight
	        |  TopRightClockwise  |  TopRightCounterClockwise
	        |  TopRightDiagonal  |  TopToBottom  | 
	       TwoBladeHorizontal  |  TwoBladeVertical  | 
	       TwoBoxBottom  |  TwoBoxLeft  |  TwoBoxRight  | 
	       TwoBoxTop  |  Up  |  Vertical  | 
	       VerticalBottomLeftOpposite  |  VerticalBottomSame  | 
	       VerticalLeft  |  VerticalRight  | 
	       VerticalTopLeftOpposite  |  VerticalTopSame
	     deriving (Eq,Show)
data Coordinated = True  |  False
		 deriving (Eq,Show)
data ClibBoundary = Parent  |  Children
		  deriving (Eq,Show)
data Direction = Forward  |  Reverse
	       deriving (Eq,Show)
data TransitionFilter = TransitionFilter
    { transitionFilterId :: (Maybe Id)
    , transitionFilterClass :: (Maybe String)
    , transitionFilterTitle :: (Maybe String)
    , transitionFilterXml'lang :: (Maybe Xml'lang)
    , transitionFilterType :: (Maybe Type)
    , transitionFilterSubtype :: (Maybe Subtype)
    , transitionFilterHorzRepeat :: (Defaultable String)
    , transitionFilterVertRepeat :: (Defaultable String)
    , transitionFilterBorderWidth :: (Defaultable String)
    , transitionFilterBorderColor :: (Defaultable String)
    , transitionFilterFadeColor :: (Defaultable String)
    , transitionFilterCoordinated :: (Defaultable Coordinated)
    , transitionFilterClibBoundary :: (Defaultable ClibBoundary)
    , transitionFilterDur :: (Maybe String)
    , transitionFilterRepeatCount :: (Maybe String)
    , transitionFilterRepeatDur :: (Maybe String)
    , transitionFilterBegin :: (Maybe String)
    , transitionFilterEnd :: (Maybe String)
    , transitionFilterValues :: (Maybe String)
    , transitionFilterFrom :: (Maybe String)
    , transitionFilterTo :: (Maybe String)
    , transitionFilterBy :: (Maybe String)
    , transitionFilterCalcMode :: (Defaultable CalcMode)
    } deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Smil where
    fromElem (CElem (Elem "smil" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "smil" (toAttrs as) [])]
instance XmlAttributes Smil where
    fromAttrs as =
	Smil
	  { smilId = possibleA fromAttrToTyp "id" as
	  , smilClass = possibleA fromAttrToStr "class" as
	  , smilTitle = possibleA fromAttrToStr "title" as
	  , smilXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  , smilXmlns = defaultA fromAttrToStr "http://www.w3.org/TR/REC-smil/SMIL20" "xmlns" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (smilId v)
	, maybeToAttr toAttrFrStr "class" (smilClass v)
	, maybeToAttr toAttrFrStr "title" (smilTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (smilXml'lang v)
	, defaultToAttr toAttrFrStr "xmlns" (smilXmlns v)
	]
instance XmlContent Head where
    fromElem (CElem (Elem "head" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "head" (toAttrs as) [])]
instance XmlAttributes Head where
    fromAttrs as =
	Head
	  { headId = possibleA fromAttrToTyp "id" as
	  , headClass = possibleA fromAttrToStr "class" as
	  , headTitle = possibleA fromAttrToStr "title" as
	  , headXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (headId v)
	, maybeToAttr toAttrFrStr "class" (headClass v)
	, maybeToAttr toAttrFrStr "title" (headTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (headXml'lang v)
	]
instance XmlContent Body where
    fromElem (CElem (Elem "body" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "body" (toAttrs as) [])]
instance XmlAttributes Body where
    fromAttrs as =
	Body
	  { bodyId = possibleA fromAttrToTyp "id" as
	  , bodyClass = possibleA fromAttrToStr "class" as
	  , bodyTitle = possibleA fromAttrToStr "title" as
	  , bodyXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (bodyId v)
	, maybeToAttr toAttrFrStr "class" (bodyClass v)
	, maybeToAttr toAttrFrStr "title" (bodyTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (bodyXml'lang v)
	]
instance XmlContent Animate where
    fromElem (CElem (Elem "animate" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "animate" (toAttrs as) [])]
instance XmlAttributes Animate where
    fromAttrs as =
	Animate
	  { animateId = possibleA fromAttrToTyp "id" as
	  , animateClass = possibleA fromAttrToStr "class" as
	  , animateTitle = possibleA fromAttrToStr "title" as
	  , animateXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  , animateCustomTest = possibleA fromAttrToTyp "customTest" as
	  , animateSystemBitrate = possibleA fromAttrToStr "systemBitrate" as
	  , animateSystemCaptions = possibleA fromAttrToTyp "systemCaptions" as
	  , animateSystemLanguage = possibleA fromAttrToStr "systemLanguage" as
	  , animateSystemOverdubOrSubtitle = possibleA fromAttrToTyp "systemOverdubOrSubtitle" as
	  , animateSystemRequired = possibleA fromAttrToTyp "systemRequired" as
	  , animateSystemScreenSize = possibleA fromAttrToStr "systemScreenSize" as
	  , animateSystemScreenDepth = possibleA fromAttrToStr "systemScreenDepth" as
	  , animateSystemAudioDesc = possibleA fromAttrToTyp "systemAudioDesc" as
	  , animateSystemOperatingSystem = possibleA fromAttrToTyp "systemOperatingSystem" as
	  , animateSystemCPU = possibleA fromAttrToTyp "systemCPU" as
	  , animateSystemComponent = possibleA fromAttrToStr "systemComponent" as
	  , animateSystem_bitrate = possibleA fromAttrToStr "system-bitrate" as
	  , animateSystem_captions = possibleA fromAttrToTyp "system-captions" as
	  , animateSystem_language = possibleA fromAttrToStr "system-language" as
	  , animateSystem_overdub_or_caption = possibleA fromAttrToTyp "system-overdub-or-caption" as
	  , animateSystem_required = possibleA fromAttrToTyp "system-required" as
	  , animateSystem_screen_size = possibleA fromAttrToStr "system-screen-size" as
	  , animateSystem_screen_depth = possibleA fromAttrToStr "system-screen-depth" as
	  , animateDur = possibleA fromAttrToStr "dur" as
	  , animateRepeatCount = possibleA fromAttrToStr "repeatCount" as
	  , animateRepeatDur = possibleA fromAttrToStr "repeatDur" as
	  , animateBegin = possibleA fromAttrToStr "begin" as
	  , animateEnd = possibleA fromAttrToStr "end" as
	  , animateAttributeName = definiteA fromAttrToStr "animate" "attributeName" as
	  , animateAttributeType = possibleA fromAttrToStr "attributeType" as
	  , animateValues = possibleA fromAttrToStr "values" as
	  , animateFrom = possibleA fromAttrToStr "from" as
	  , animateTo = possibleA fromAttrToStr "to" as
	  , animateBy = possibleA fromAttrToStr "by" as
	  , animateCalcMode = defaultA fromAttrToTyp Linear "calcMode" as
	  , animateAdditive = defaultA fromAttrToTyp Replace "additive" as
	  , animateAccumulate = defaultA fromAttrToTyp None "accumulate" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (animateId v)
	, maybeToAttr toAttrFrStr "class" (animateClass v)
	, maybeToAttr toAttrFrStr "title" (animateTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (animateXml'lang v)
	, maybeToAttr toAttrFrTyp "customTest" (animateCustomTest v)
	, maybeToAttr toAttrFrStr "systemBitrate" (animateSystemBitrate v)
	, maybeToAttr toAttrFrTyp "systemCaptions" (animateSystemCaptions v)
	, maybeToAttr toAttrFrStr "systemLanguage" (animateSystemLanguage v)
	, maybeToAttr toAttrFrTyp "systemOverdubOrSubtitle" (animateSystemOverdubOrSubtitle v)
	, maybeToAttr toAttrFrTyp "systemRequired" (animateSystemRequired v)
	, maybeToAttr toAttrFrStr "systemScreenSize" (animateSystemScreenSize v)
	, maybeToAttr toAttrFrStr "systemScreenDepth" (animateSystemScreenDepth v)
	, maybeToAttr toAttrFrTyp "systemAudioDesc" (animateSystemAudioDesc v)
	, maybeToAttr toAttrFrTyp "systemOperatingSystem" (animateSystemOperatingSystem v)
	, maybeToAttr toAttrFrTyp "systemCPU" (animateSystemCPU v)
	, maybeToAttr toAttrFrStr "systemComponent" (animateSystemComponent v)
	, maybeToAttr toAttrFrStr "system-bitrate" (animateSystem_bitrate v)
	, maybeToAttr toAttrFrTyp "system-captions" (animateSystem_captions v)
	, maybeToAttr toAttrFrStr "system-language" (animateSystem_language v)
	, maybeToAttr toAttrFrTyp "system-overdub-or-caption" (animateSystem_overdub_or_caption v)
	, maybeToAttr toAttrFrTyp "system-required" (animateSystem_required v)
	, maybeToAttr toAttrFrStr "system-screen-size" (animateSystem_screen_size v)
	, maybeToAttr toAttrFrStr "system-screen-depth" (animateSystem_screen_depth v)
	, maybeToAttr toAttrFrStr "dur" (animateDur v)
	, maybeToAttr toAttrFrStr "repeatCount" (animateRepeatCount v)
	, maybeToAttr toAttrFrStr "repeatDur" (animateRepeatDur v)
	, maybeToAttr toAttrFrStr "begin" (animateBegin v)
	, maybeToAttr toAttrFrStr "end" (animateEnd v)
	, toAttrFrStr "attributeName" (animateAttributeName v)
	, maybeToAttr toAttrFrStr "attributeType" (animateAttributeType v)
	, maybeToAttr toAttrFrStr "values" (animateValues v)
	, maybeToAttr toAttrFrStr "from" (animateFrom v)
	, maybeToAttr toAttrFrStr "to" (animateTo v)
	, maybeToAttr toAttrFrStr "by" (animateBy v)
	, defaultToAttr toAttrFrTyp "calcMode" (animateCalcMode v)
	, defaultToAttr toAttrFrTyp "additive" (animateAdditive v)
	, defaultToAttr toAttrFrTyp "accumulate" (animateAccumulate v)
	]
instance XmlAttrType SystemCaptions where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just On
	    translate "off" = Just Off
	    translate _ = Nothing
    toAttrFrTyp n On = Just (n, str2attr "on")
    toAttrFrTyp n Off = Just (n, str2attr "off")
instance XmlAttrType SystemOverdubOrSubtitle where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "overdub" = Just Overdub
	    translate "subtitle" = Just Subtitle
	    translate _ = Nothing
    toAttrFrTyp n Overdub = Just (n, str2attr "overdub")
    toAttrFrTyp n Subtitle = Just (n, str2attr "subtitle")
instance XmlAttrType SystemAudioDesc where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just On
	    translate "off" = Just Off
	    translate _ = Nothing
    toAttrFrTyp n On = Just (n, str2attr "on")
    toAttrFrTyp n Off = Just (n, str2attr "off")
instance XmlAttrType System_captions where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just On
	    translate "off" = Just Off
	    translate _ = Nothing
    toAttrFrTyp n On = Just (n, str2attr "on")
    toAttrFrTyp n Off = Just (n, str2attr "off")
instance XmlAttrType System_overdub_or_caption where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "overdub" = Just Overdub
	    translate "caption" = Just Caption
	    translate _ = Nothing
    toAttrFrTyp n Overdub = Just (n, str2attr "overdub")
    toAttrFrTyp n Caption = Just (n, str2attr "caption")
instance XmlAttrType CalcMode where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "discrete" = Just Discrete
	    translate "linear" = Just Linear
	    translate "paced" = Just Paced
	    translate _ = Nothing
    toAttrFrTyp n Discrete = Just (n, str2attr "discrete")
    toAttrFrTyp n Linear = Just (n, str2attr "linear")
    toAttrFrTyp n Paced = Just (n, str2attr "paced")
instance XmlAttrType Additive where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "replace" = Just Replace
	    translate "sum" = Just Sum
	    translate _ = Nothing
    toAttrFrTyp n Replace = Just (n, str2attr "replace")
    toAttrFrTyp n Sum = Just (n, str2attr "sum")
instance XmlAttrType Accumulate where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "none" = Just None
	    translate "sum" = Just Sum
	    translate _ = Nothing
    toAttrFrTyp n None = Just (n, str2attr "none")
    toAttrFrTyp n Sum = Just (n, str2attr "sum")
instance XmlContent Set where
    fromElem (CElem (Elem "set" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "set" (toAttrs as) [])]
instance XmlAttributes Set where
    fromAttrs as =
	Set
	  { setId = possibleA fromAttrToTyp "id" as
	  , setClass = possibleA fromAttrToStr "class" as
	  , setTitle = possibleA fromAttrToStr "title" as
	  , setXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  , setCustomTest = possibleA fromAttrToTyp "customTest" as
	  , setSystemBitrate = possibleA fromAttrToStr "systemBitrate" as
	  , setSystemCaptions = possibleA fromAttrToTyp "systemCaptions" as
	  , setSystemLanguage = possibleA fromAttrToStr "systemLanguage" as
	  , setSystemOverdubOrSubtitle = possibleA fromAttrToTyp "systemOverdubOrSubtitle" as
	  , setSystemRequired = possibleA fromAttrToTyp "systemRequired" as
	  , setSystemScreenSize = possibleA fromAttrToStr "systemScreenSize" as
	  , setSystemScreenDepth = possibleA fromAttrToStr "systemScreenDepth" as
	  , setSystemAudioDesc = possibleA fromAttrToTyp "systemAudioDesc" as
	  , setSystemOperatingSystem = possibleA fromAttrToTyp "systemOperatingSystem" as
	  , setSystemCPU = possibleA fromAttrToTyp "systemCPU" as
	  , setSystemComponent = possibleA fromAttrToStr "systemComponent" as
	  , setSystem_bitrate = possibleA fromAttrToStr "system-bitrate" as
	  , setSystem_captions = possibleA fromAttrToTyp "system-captions" as
	  , setSystem_language = possibleA fromAttrToStr "system-language" as
	  , setSystem_overdub_or_caption = possibleA fromAttrToTyp "system-overdub-or-caption" as
	  , setSystem_required = possibleA fromAttrToTyp "system-required" as
	  , setSystem_screen_size = possibleA fromAttrToStr "system-screen-size" as
	  , setSystem_screen_depth = possibleA fromAttrToStr "system-screen-depth" as
	  , setDur = possibleA fromAttrToStr "dur" as
	  , setRepeatCount = possibleA fromAttrToStr "repeatCount" as
	  , setRepeatDur = possibleA fromAttrToStr "repeatDur" as
	  , setBegin = possibleA fromAttrToStr "begin" as
	  , setEnd = possibleA fromAttrToStr "end" as
	  , setAttributeName = definiteA fromAttrToStr "set" "attributeName" as
	  , setAttributeType = possibleA fromAttrToStr "attributeType" as
	  , setTo = possibleA fromAttrToStr "to" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (setId v)
	, maybeToAttr toAttrFrStr "class" (setClass v)
	, maybeToAttr toAttrFrStr "title" (setTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (setXml'lang v)
	, maybeToAttr toAttrFrTyp "customTest" (setCustomTest v)
	, maybeToAttr toAttrFrStr "systemBitrate" (setSystemBitrate v)
	, maybeToAttr toAttrFrTyp "systemCaptions" (setSystemCaptions v)
	, maybeToAttr toAttrFrStr "systemLanguage" (setSystemLanguage v)
	, maybeToAttr toAttrFrTyp "systemOverdubOrSubtitle" (setSystemOverdubOrSubtitle v)
	, maybeToAttr toAttrFrTyp "systemRequired" (setSystemRequired v)
	, maybeToAttr toAttrFrStr "systemScreenSize" (setSystemScreenSize v)
	, maybeToAttr toAttrFrStr "systemScreenDepth" (setSystemScreenDepth v)
	, maybeToAttr toAttrFrTyp "systemAudioDesc" (setSystemAudioDesc v)
	, maybeToAttr toAttrFrTyp "systemOperatingSystem" (setSystemOperatingSystem v)
	, maybeToAttr toAttrFrTyp "systemCPU" (setSystemCPU v)
	, maybeToAttr toAttrFrStr "systemComponent" (setSystemComponent v)
	, maybeToAttr toAttrFrStr "system-bitrate" (setSystem_bitrate v)
	, maybeToAttr toAttrFrTyp "system-captions" (setSystem_captions v)
	, maybeToAttr toAttrFrStr "system-language" (setSystem_language v)
	, maybeToAttr toAttrFrTyp "system-overdub-or-caption" (setSystem_overdub_or_caption v)
	, maybeToAttr toAttrFrTyp "system-required" (setSystem_required v)
	, maybeToAttr toAttrFrStr "system-screen-size" (setSystem_screen_size v)
	, maybeToAttr toAttrFrStr "system-screen-depth" (setSystem_screen_depth v)
	, maybeToAttr toAttrFrStr "dur" (setDur v)
	, maybeToAttr toAttrFrStr "repeatCount" (setRepeatCount v)
	, maybeToAttr toAttrFrStr "repeatDur" (setRepeatDur v)
	, maybeToAttr toAttrFrStr "begin" (setBegin v)
	, maybeToAttr toAttrFrStr "end" (setEnd v)
	, toAttrFrStr "attributeName" (setAttributeName v)
	, maybeToAttr toAttrFrStr "attributeType" (setAttributeType v)
	, maybeToAttr toAttrFrStr "to" (setTo v)
	]
instance XmlContent AnimateMotion where
    fromElem (CElem (Elem "animateMotion" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "animateMotion" (toAttrs as) [])]
instance XmlAttributes AnimateMotion where
    fromAttrs as =
	AnimateMotion
	  { animateMotionId = possibleA fromAttrToTyp "id" as
	  , animateMotionClass = possibleA fromAttrToStr "class" as
	  , animateMotionTitle = possibleA fromAttrToStr "title" as
	  , animateMotionXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  , animateMotionCustomTest = possibleA fromAttrToTyp "customTest" as
	  , animateMotionSystemBitrate = possibleA fromAttrToStr "systemBitrate" as
	  , animateMotionSystemCaptions = possibleA fromAttrToTyp "systemCaptions" as
	  , animateMotionSystemLanguage = possibleA fromAttrToStr "systemLanguage" as
	  , animateMotionSystemOverdubOrSubtitle = possibleA fromAttrToTyp "systemOverdubOrSubtitle" as
	  , animateMotionSystemRequired = possibleA fromAttrToTyp "systemRequired" as
	  , animateMotionSystemScreenSize = possibleA fromAttrToStr "systemScreenSize" as
	  , animateMotionSystemScreenDepth = possibleA fromAttrToStr "systemScreenDepth" as
	  , animateMotionSystemAudioDesc = possibleA fromAttrToTyp "systemAudioDesc" as
	  , animateMotionSystemOperatingSystem = possibleA fromAttrToTyp "systemOperatingSystem" as
	  , animateMotionSystemCPU = possibleA fromAttrToTyp "systemCPU" as
	  , animateMotionSystemComponent = possibleA fromAttrToStr "systemComponent" as
	  , animateMotionSystem_bitrate = possibleA fromAttrToStr "system-bitrate" as
	  , animateMotionSystem_captions = possibleA fromAttrToTyp "system-captions" as
	  , animateMotionSystem_language = possibleA fromAttrToStr "system-language" as
	  , animateMotionSystem_overdub_or_caption = possibleA fromAttrToTyp "system-overdub-or-caption" as
	  , animateMotionSystem_required = possibleA fromAttrToTyp "system-required" as
	  , animateMotionSystem_screen_size = possibleA fromAttrToStr "system-screen-size" as
	  , animateMotionSystem_screen_depth = possibleA fromAttrToStr "system-screen-depth" as
	  , animateMotionDur = possibleA fromAttrToStr "dur" as
	  , animateMotionRepeatCount = possibleA fromAttrToStr "repeatCount" as
	  , animateMotionRepeatDur = possibleA fromAttrToStr "repeatDur" as
	  , animateMotionBegin = possibleA fromAttrToStr "begin" as
	  , animateMotionEnd = possibleA fromAttrToStr "end" as
	  , animateMotionValues = possibleA fromAttrToStr "values" as
	  , animateMotionFrom = possibleA fromAttrToStr "from" as
	  , animateMotionTo = possibleA fromAttrToStr "to" as
	  , animateMotionBy = possibleA fromAttrToStr "by" as
	  , animateMotionCalcMode = defaultA fromAttrToTyp Linear "calcMode" as
	  , animateMotionAdditive = defaultA fromAttrToTyp Replace "additive" as
	  , animateMotionAccumulate = defaultA fromAttrToTyp None "accumulate" as
	  , animateMotionOrigin = defaultA fromAttrToTyp Default "origin" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (animateMotionId v)
	, maybeToAttr toAttrFrStr "class" (animateMotionClass v)
	, maybeToAttr toAttrFrStr "title" (animateMotionTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (animateMotionXml'lang v)
	, maybeToAttr toAttrFrTyp "customTest" (animateMotionCustomTest v)
	, maybeToAttr toAttrFrStr "systemBitrate" (animateMotionSystemBitrate v)
	, maybeToAttr toAttrFrTyp "systemCaptions" (animateMotionSystemCaptions v)
	, maybeToAttr toAttrFrStr "systemLanguage" (animateMotionSystemLanguage v)
	, maybeToAttr toAttrFrTyp "systemOverdubOrSubtitle" (animateMotionSystemOverdubOrSubtitle v)
	, maybeToAttr toAttrFrTyp "systemRequired" (animateMotionSystemRequired v)
	, maybeToAttr toAttrFrStr "systemScreenSize" (animateMotionSystemScreenSize v)
	, maybeToAttr toAttrFrStr "systemScreenDepth" (animateMotionSystemScreenDepth v)
	, maybeToAttr toAttrFrTyp "systemAudioDesc" (animateMotionSystemAudioDesc v)
	, maybeToAttr toAttrFrTyp "systemOperatingSystem" (animateMotionSystemOperatingSystem v)
	, maybeToAttr toAttrFrTyp "systemCPU" (animateMotionSystemCPU v)
	, maybeToAttr toAttrFrStr "systemComponent" (animateMotionSystemComponent v)
	, maybeToAttr toAttrFrStr "system-bitrate" (animateMotionSystem_bitrate v)
	, maybeToAttr toAttrFrTyp "system-captions" (animateMotionSystem_captions v)
	, maybeToAttr toAttrFrStr "system-language" (animateMotionSystem_language v)
	, maybeToAttr toAttrFrTyp "system-overdub-or-caption" (animateMotionSystem_overdub_or_caption v)
	, maybeToAttr toAttrFrTyp "system-required" (animateMotionSystem_required v)
	, maybeToAttr toAttrFrStr "system-screen-size" (animateMotionSystem_screen_size v)
	, maybeToAttr toAttrFrStr "system-screen-depth" (animateMotionSystem_screen_depth v)
	, maybeToAttr toAttrFrStr "dur" (animateMotionDur v)
	, maybeToAttr toAttrFrStr "repeatCount" (animateMotionRepeatCount v)
	, maybeToAttr toAttrFrStr "repeatDur" (animateMotionRepeatDur v)
	, maybeToAttr toAttrFrStr "begin" (animateMotionBegin v)
	, maybeToAttr toAttrFrStr "end" (animateMotionEnd v)
	, maybeToAttr toAttrFrStr "values" (animateMotionValues v)
	, maybeToAttr toAttrFrStr "from" (animateMotionFrom v)
	, maybeToAttr toAttrFrStr "to" (animateMotionTo v)
	, maybeToAttr toAttrFrStr "by" (animateMotionBy v)
	, defaultToAttr toAttrFrTyp "calcMode" (animateMotionCalcMode v)
	, defaultToAttr toAttrFrTyp "additive" (animateMotionAdditive v)
	, defaultToAttr toAttrFrTyp "accumulate" (animateMotionAccumulate v)
	, defaultToAttr toAttrFrTyp "origin" (animateMotionOrigin v)
	]
instance XmlAttrType Origin where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "default" = Just Default
	    translate _ = Nothing
    toAttrFrTyp n Default = Just (n, str2attr "default")
instance XmlContent AnimateColor where
    fromElem (CElem (Elem "animateColor" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "animateColor" (toAttrs as) [])]
instance XmlAttributes AnimateColor where
    fromAttrs as =
	AnimateColor
	  { animateColorId = possibleA fromAttrToTyp "id" as
	  , animateColorClass = possibleA fromAttrToStr "class" as
	  , animateColorTitle = possibleA fromAttrToStr "title" as
	  , animateColorXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  , animateColorCustomTest = possibleA fromAttrToTyp "customTest" as
	  , animateColorSystemBitrate = possibleA fromAttrToStr "systemBitrate" as
	  , animateColorSystemCaptions = possibleA fromAttrToTyp "systemCaptions" as
	  , animateColorSystemLanguage = possibleA fromAttrToStr "systemLanguage" as
	  , animateColorSystemOverdubOrSubtitle = possibleA fromAttrToTyp "systemOverdubOrSubtitle" as
	  , animateColorSystemRequired = possibleA fromAttrToTyp "systemRequired" as
	  , animateColorSystemScreenSize = possibleA fromAttrToStr "systemScreenSize" as
	  , animateColorSystemScreenDepth = possibleA fromAttrToStr "systemScreenDepth" as
	  , animateColorSystemAudioDesc = possibleA fromAttrToTyp "systemAudioDesc" as
	  , animateColorSystemOperatingSystem = possibleA fromAttrToTyp "systemOperatingSystem" as
	  , animateColorSystemCPU = possibleA fromAttrToTyp "systemCPU" as
	  , animateColorSystemComponent = possibleA fromAttrToStr "systemComponent" as
	  , animateColorSystem_bitrate = possibleA fromAttrToStr "system-bitrate" as
	  , animateColorSystem_captions = possibleA fromAttrToTyp "system-captions" as
	  , animateColorSystem_language = possibleA fromAttrToStr "system-language" as
	  , animateColorSystem_overdub_or_caption = possibleA fromAttrToTyp "system-overdub-or-caption" as
	  , animateColorSystem_required = possibleA fromAttrToTyp "system-required" as
	  , animateColorSystem_screen_size = possibleA fromAttrToStr "system-screen-size" as
	  , animateColorSystem_screen_depth = possibleA fromAttrToStr "system-screen-depth" as
	  , animateColorDur = possibleA fromAttrToStr "dur" as
	  , animateColorRepeatCount = possibleA fromAttrToStr "repeatCount" as
	  , animateColorRepeatDur = possibleA fromAttrToStr "repeatDur" as
	  , animateColorBegin = possibleA fromAttrToStr "begin" as
	  , animateColorEnd = possibleA fromAttrToStr "end" as
	  , animateColorAttributeName = definiteA fromAttrToStr "animateColor" "attributeName" as
	  , animateColorAttributeType = possibleA fromAttrToStr "attributeType" as
	  , animateColorValues = possibleA fromAttrToStr "values" as
	  , animateColorFrom = possibleA fromAttrToStr "from" as
	  , animateColorTo = possibleA fromAttrToStr "to" as
	  , animateColorBy = possibleA fromAttrToStr "by" as
	  , animateColorCalcMode = defaultA fromAttrToTyp Linear "calcMode" as
	  , animateColorAdditive = defaultA fromAttrToTyp Replace "additive" as
	  , animateColorAccumulate = defaultA fromAttrToTyp None "accumulate" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (animateColorId v)
	, maybeToAttr toAttrFrStr "class" (animateColorClass v)
	, maybeToAttr toAttrFrStr "title" (animateColorTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (animateColorXml'lang v)
	, maybeToAttr toAttrFrTyp "customTest" (animateColorCustomTest v)
	, maybeToAttr toAttrFrStr "systemBitrate" (animateColorSystemBitrate v)
	, maybeToAttr toAttrFrTyp "systemCaptions" (animateColorSystemCaptions v)
	, maybeToAttr toAttrFrStr "systemLanguage" (animateColorSystemLanguage v)
	, maybeToAttr toAttrFrTyp "systemOverdubOrSubtitle" (animateColorSystemOverdubOrSubtitle v)
	, maybeToAttr toAttrFrTyp "systemRequired" (animateColorSystemRequired v)
	, maybeToAttr toAttrFrStr "systemScreenSize" (animateColorSystemScreenSize v)
	, maybeToAttr toAttrFrStr "systemScreenDepth" (animateColorSystemScreenDepth v)
	, maybeToAttr toAttrFrTyp "systemAudioDesc" (animateColorSystemAudioDesc v)
	, maybeToAttr toAttrFrTyp "systemOperatingSystem" (animateColorSystemOperatingSystem v)
	, maybeToAttr toAttrFrTyp "systemCPU" (animateColorSystemCPU v)
	, maybeToAttr toAttrFrStr "systemComponent" (animateColorSystemComponent v)
	, maybeToAttr toAttrFrStr "system-bitrate" (animateColorSystem_bitrate v)
	, maybeToAttr toAttrFrTyp "system-captions" (animateColorSystem_captions v)
	, maybeToAttr toAttrFrStr "system-language" (animateColorSystem_language v)
	, maybeToAttr toAttrFrTyp "system-overdub-or-caption" (animateColorSystem_overdub_or_caption v)
	, maybeToAttr toAttrFrTyp "system-required" (animateColorSystem_required v)
	, maybeToAttr toAttrFrStr "system-screen-size" (animateColorSystem_screen_size v)
	, maybeToAttr toAttrFrStr "system-screen-depth" (animateColorSystem_screen_depth v)
	, maybeToAttr toAttrFrStr "dur" (animateColorDur v)
	, maybeToAttr toAttrFrStr "repeatCount" (animateColorRepeatCount v)
	, maybeToAttr toAttrFrStr "repeatDur" (animateColorRepeatDur v)
	, maybeToAttr toAttrFrStr "begin" (animateColorBegin v)
	, maybeToAttr toAttrFrStr "end" (animateColorEnd v)
	, toAttrFrStr "attributeName" (animateColorAttributeName v)
	, maybeToAttr toAttrFrStr "attributeType" (animateColorAttributeType v)
	, maybeToAttr toAttrFrStr "values" (animateColorValues v)
	, maybeToAttr toAttrFrStr "from" (animateColorFrom v)
	, maybeToAttr toAttrFrStr "to" (animateColorTo v)
	, maybeToAttr toAttrFrStr "by" (animateColorBy v)
	, defaultToAttr toAttrFrTyp "calcMode" (animateColorCalcMode v)
	, defaultToAttr toAttrFrTyp "additive" (animateColorAdditive v)
	, defaultToAttr toAttrFrTyp "accumulate" (animateColorAccumulate v)
	]
instance XmlContent Switch where
    fromElem (CElem (Elem "switch" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "switch" (toAttrs as) [])]
instance XmlAttributes Switch where
    fromAttrs as =
	Switch
	  { switchId = possibleA fromAttrToTyp "id" as
	  , switchClass = possibleA fromAttrToStr "class" as
	  , switchTitle = possibleA fromAttrToStr "title" as
	  , switchXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (switchId v)
	, maybeToAttr toAttrFrStr "class" (switchClass v)
	, maybeToAttr toAttrFrStr "title" (switchTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (switchXml'lang v)
	]
instance XmlContent Meta where
    fromElem (CElem (Elem "meta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "meta" (toAttrs as) [])]
instance XmlAttributes Meta where
    fromAttrs as =
	Meta
	  { metaContent = possibleA fromAttrToStr "content" as
	  , metaName = definiteA fromAttrToStr "meta" "name" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "content" (metaContent v)
	, toAttrFrStr "name" (metaName v)
	]
instance XmlContent Metadata where
    fromElem (CElem (Elem "metadata" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "metadata" (toAttrs as) [])]
instance XmlAttributes Metadata where
    fromAttrs as =
	Metadata
	  { metadataId = possibleA fromAttrToTyp "id" as
	  , metadataClass = possibleA fromAttrToStr "class" as
	  , metadataTitle = possibleA fromAttrToStr "title" as
	  , metadataXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (metadataId v)
	, maybeToAttr toAttrFrStr "class" (metadataClass v)
	, maybeToAttr toAttrFrStr "title" (metadataTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (metadataXml'lang v)
	]
instance XmlContent Layout where
    fromElem (CElem (Elem "layout" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "layout" (toAttrs as) [])]
instance XmlAttributes Layout where
    fromAttrs as =
	Layout
	  { layoutId = possibleA fromAttrToTyp "id" as
	  , layoutClass = possibleA fromAttrToStr "class" as
	  , layoutTitle = possibleA fromAttrToStr "title" as
	  , layoutXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  , layoutType = defaultA fromAttrToStr "text/smil-basic-layout" "type" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (layoutId v)
	, maybeToAttr toAttrFrStr "class" (layoutClass v)
	, maybeToAttr toAttrFrStr "title" (layoutTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (layoutXml'lang v)
	, defaultToAttr toAttrFrStr "type" (layoutType v)
	]
instance XmlContent Region where
    fromElem (CElem (Elem "region" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "region" (toAttrs as) [])]
instance XmlAttributes Region where
    fromAttrs as =
	Region
	  { regionId = possibleA fromAttrToTyp "id" as
	  , regionClass = possibleA fromAttrToStr "class" as
	  , regionTitle = possibleA fromAttrToStr "title" as
	  , regionXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  , regionHeight = defaultA fromAttrToStr "auto" "height" as
	  , regionWidth = defaultA fromAttrToStr "auto" "width" as
	  , regionClose = defaultA fromAttrToTyp Never "close" as
	  , regionOpen = defaultA fromAttrToTyp Always "open" as
	  , regionBackgroundColor = possibleA fromAttrToStr "backgroundColor" as
	  , regionBackground_color = possibleA fromAttrToStr "background-color" as
	  , regionBottom = defaultA fromAttrToStr "auto" "bottom" as
	  , regionLeft = defaultA fromAttrToStr "auto" "left" as
	  , regionRight = defaultA fromAttrToStr "auto" "right" as
	  , regionTop = defaultA fromAttrToStr "auto" "top" as
	  , regionZ_index = possibleA fromAttrToStr "z-index" as
	  , regionShowBackground = defaultA fromAttrToTyp Always "showBackground" as
	  , regionFit = defaultA fromAttrToTyp Hidden "fit" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (regionId v)
	, maybeToAttr toAttrFrStr "class" (regionClass v)
	, maybeToAttr toAttrFrStr "title" (regionTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (regionXml'lang v)
	, defaultToAttr toAttrFrStr "height" (regionHeight v)
	, defaultToAttr toAttrFrStr "width" (regionWidth v)
	, defaultToAttr toAttrFrTyp "close" (regionClose v)
	, defaultToAttr toAttrFrTyp "open" (regionOpen v)
	, maybeToAttr toAttrFrStr "backgroundColor" (regionBackgroundColor v)
	, maybeToAttr toAttrFrStr "background-color" (regionBackground_color v)
	, defaultToAttr toAttrFrStr "bottom" (regionBottom v)
	, defaultToAttr toAttrFrStr "left" (regionLeft v)
	, defaultToAttr toAttrFrStr "right" (regionRight v)
	, defaultToAttr toAttrFrStr "top" (regionTop v)
	, maybeToAttr toAttrFrStr "z-index" (regionZ_index v)
	, defaultToAttr toAttrFrTyp "showBackground" (regionShowBackground v)
	, defaultToAttr toAttrFrTyp "fit" (regionFit v)
	]
instance XmlAttrType Close where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "never" = Just Never
	    translate "whenNotActive" = Just WhenNotActive
	    translate _ = Nothing
    toAttrFrTyp n Never = Just (n, str2attr "never")
    toAttrFrTyp n WhenNotActive = Just (n, str2attr "whenNotActive")
instance XmlAttrType Open where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "always" = Just Always
	    translate "whenActive" = Just WhenActive
	    translate _ = Nothing
    toAttrFrTyp n Always = Just (n, str2attr "always")
    toAttrFrTyp n WhenActive = Just (n, str2attr "whenActive")
instance XmlAttrType ShowBackground where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "always" = Just Always
	    translate "whenActive" = Just WhenActive
	    translate _ = Nothing
    toAttrFrTyp n Always = Just (n, str2attr "always")
    toAttrFrTyp n WhenActive = Just (n, str2attr "whenActive")
instance XmlAttrType Fit where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "hidden" = Just Hidden
	    translate "fill" = Just Fill
	    translate "meet" = Just Meet
	    translate "scroll" = Just Scroll
	    translate "slice" = Just Slice
	    translate _ = Nothing
    toAttrFrTyp n Hidden = Just (n, str2attr "hidden")
    toAttrFrTyp n Fill = Just (n, str2attr "fill")
    toAttrFrTyp n Meet = Just (n, str2attr "meet")
    toAttrFrTyp n Scroll = Just (n, str2attr "scroll")
    toAttrFrTyp n Slice = Just (n, str2attr "slice")
instance XmlContent Root_layout where
    fromElem (CElem (Elem "root-layout" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "root-layout" (toAttrs as) [])]
instance XmlAttributes Root_layout where
    fromAttrs as =
	Root_layout
	  { root_layoutId = possibleA fromAttrToTyp "id" as
	  , root_layoutClass = possibleA fromAttrToStr "class" as
	  , root_layoutTitle = possibleA fromAttrToStr "title" as
	  , root_layoutXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  , root_layoutHeight = defaultA fromAttrToStr "auto" "height" as
	  , root_layoutWidth = defaultA fromAttrToStr "auto" "width" as
	  , root_layoutClose = defaultA fromAttrToTyp Never "close" as
	  , root_layoutOpen = defaultA fromAttrToTyp Always "open" as
	  , root_layoutBackgroundColor = possibleA fromAttrToStr "backgroundColor" as
	  , root_layoutBackground_color = possibleA fromAttrToStr "background-color" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (root_layoutId v)
	, maybeToAttr toAttrFrStr "class" (root_layoutClass v)
	, maybeToAttr toAttrFrStr "title" (root_layoutTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (root_layoutXml'lang v)
	, defaultToAttr toAttrFrStr "height" (root_layoutHeight v)
	, defaultToAttr toAttrFrStr "width" (root_layoutWidth v)
	, defaultToAttr toAttrFrTyp "close" (root_layoutClose v)
	, defaultToAttr toAttrFrTyp "open" (root_layoutOpen v)
	, maybeToAttr toAttrFrStr "backgroundColor" (root_layoutBackgroundColor v)
	, maybeToAttr toAttrFrStr "background-color" (root_layoutBackground_color v)
	]
instance XmlContent Ref where
    fromElem (CElem (Elem "ref" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "ref" (toAttrs as) [])]
instance XmlAttributes Ref where
    fromAttrs as =
	Ref
	  { refId = possibleA fromAttrToTyp "id" as
	  , refClass = possibleA fromAttrToStr "class" as
	  , refTitle = possibleA fromAttrToStr "title" as
	  , refXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (refId v)
	, maybeToAttr toAttrFrStr "class" (refClass v)
	, maybeToAttr toAttrFrStr "title" (refTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (refXml'lang v)
	]
instance XmlContent Audio where
    fromElem (CElem (Elem "audio" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "audio" (toAttrs as) [])]
instance XmlAttributes Audio where
    fromAttrs as =
	Audio
	  { audioId = possibleA fromAttrToTyp "id" as
	  , audioClass = possibleA fromAttrToStr "class" as
	  , audioTitle = possibleA fromAttrToStr "title" as
	  , audioXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (audioId v)
	, maybeToAttr toAttrFrStr "class" (audioClass v)
	, maybeToAttr toAttrFrStr "title" (audioTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (audioXml'lang v)
	]
instance XmlContent Img where
    fromElem (CElem (Elem "img" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "img" (toAttrs as) [])]
instance XmlAttributes Img where
    fromAttrs as =
	Img
	  { imgId = possibleA fromAttrToTyp "id" as
	  , imgClass = possibleA fromAttrToStr "class" as
	  , imgTitle = possibleA fromAttrToStr "title" as
	  , imgXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (imgId v)
	, maybeToAttr toAttrFrStr "class" (imgClass v)
	, maybeToAttr toAttrFrStr "title" (imgTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (imgXml'lang v)
	]
instance XmlContent Video where
    fromElem (CElem (Elem "video" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "video" (toAttrs as) [])]
instance XmlAttributes Video where
    fromAttrs as =
	Video
	  { videoId = possibleA fromAttrToTyp "id" as
	  , videoClass = possibleA fromAttrToStr "class" as
	  , videoTitle = possibleA fromAttrToStr "title" as
	  , videoXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (videoId v)
	, maybeToAttr toAttrFrStr "class" (videoClass v)
	, maybeToAttr toAttrFrStr "title" (videoTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (videoXml'lang v)
	]
instance XmlContent Text where
    fromElem (CElem (Elem "text" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "text" (toAttrs as) [])]
instance XmlAttributes Text where
    fromAttrs as =
	Text
	  { textId = possibleA fromAttrToTyp "id" as
	  , textClass = possibleA fromAttrToStr "class" as
	  , textTitle = possibleA fromAttrToStr "title" as
	  , textXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (textId v)
	, maybeToAttr toAttrFrStr "class" (textClass v)
	, maybeToAttr toAttrFrStr "title" (textTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (textXml'lang v)
	]
instance XmlContent Textstream where
    fromElem (CElem (Elem "textstream" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "textstream" (toAttrs as) [])]
instance XmlAttributes Textstream where
    fromAttrs as =
	Textstream
	  { textstreamId = possibleA fromAttrToTyp "id" as
	  , textstreamClass = possibleA fromAttrToStr "class" as
	  , textstreamTitle = possibleA fromAttrToStr "title" as
	  , textstreamXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (textstreamId v)
	, maybeToAttr toAttrFrStr "class" (textstreamClass v)
	, maybeToAttr toAttrFrStr "title" (textstreamTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (textstreamXml'lang v)
	]
instance XmlContent Animation where
    fromElem (CElem (Elem "animation" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "animation" (toAttrs as) [])]
instance XmlAttributes Animation where
    fromAttrs as =
	Animation
	  { animationId = possibleA fromAttrToTyp "id" as
	  , animationClass = possibleA fromAttrToStr "class" as
	  , animationTitle = possibleA fromAttrToStr "title" as
	  , animationXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (animationId v)
	, maybeToAttr toAttrFrStr "class" (animationClass v)
	, maybeToAttr toAttrFrStr "title" (animationTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (animationXml'lang v)
	]
instance XmlContent Transition where
    fromElem (CElem (Elem "transition" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "transition" (toAttrs as) [])]
instance XmlAttributes Transition where
    fromAttrs as =
	Transition
	  { transitionId = possibleA fromAttrToTyp "id" as
	  , transitionClass = possibleA fromAttrToStr "class" as
	  , transitionTitle = possibleA fromAttrToStr "title" as
	  , transitionXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  , transitionType = possibleA fromAttrToTyp "type" as
	  , transitionSubtype = possibleA fromAttrToTyp "subtype" as
	  , transitionHorzRepeat = defaultA fromAttrToStr "0" "horzRepeat" as
	  , transitionVertRepeat = defaultA fromAttrToStr "0" "vertRepeat" as
	  , transitionBorderWidth = defaultA fromAttrToStr "0" "borderWidth" as
	  , transitionBorderColor = defaultA fromAttrToStr "black" "borderColor" as
	  , transitionFadeColor = defaultA fromAttrToStr "black" "fadeColor" as
	  , transitionCoordinated = defaultA fromAttrToTyp False "coordinated" as
	  , transitionClibBoundary = defaultA fromAttrToTyp Children "clibBoundary" as
	  , transitionDur = possibleA fromAttrToStr "dur" as
	  , transitionStartProgress = defaultA fromAttrToStr "0.0" "startProgress" as
	  , transitionEndProgress = defaultA fromAttrToStr "1.0" "endProgress" as
	  , transitionDirection = defaultA fromAttrToTyp Forward "direction" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (transitionId v)
	, maybeToAttr toAttrFrStr "class" (transitionClass v)
	, maybeToAttr toAttrFrStr "title" (transitionTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (transitionXml'lang v)
	, maybeToAttr toAttrFrTyp "type" (transitionType v)
	, maybeToAttr toAttrFrTyp "subtype" (transitionSubtype v)
	, defaultToAttr toAttrFrStr "horzRepeat" (transitionHorzRepeat v)
	, defaultToAttr toAttrFrStr "vertRepeat" (transitionVertRepeat v)
	, defaultToAttr toAttrFrStr "borderWidth" (transitionBorderWidth v)
	, defaultToAttr toAttrFrStr "borderColor" (transitionBorderColor v)
	, defaultToAttr toAttrFrStr "fadeColor" (transitionFadeColor v)
	, defaultToAttr toAttrFrTyp "coordinated" (transitionCoordinated v)
	, defaultToAttr toAttrFrTyp "clibBoundary" (transitionClibBoundary v)
	, maybeToAttr toAttrFrStr "dur" (transitionDur v)
	, defaultToAttr toAttrFrStr "startProgress" (transitionStartProgress v)
	, defaultToAttr toAttrFrStr "endProgress" (transitionEndProgress v)
	, defaultToAttr toAttrFrTyp "direction" (transitionDirection v)
	]
instance XmlAttrType Type where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "barWipe" = Just BarWipe
	    translate "boxWipe" = Just BoxWipe
	    translate "fourBoxWipe" = Just FourBoxWipe
	    translate "barnDoorWipe" = Just BarnDoorWipe
	    translate "diagonalWipe" = Just DiagonalWipe
	    translate "bowTieWipe" = Just BowTieWipe
	    translate "miscDiagonalWipe" = Just MiscDiagonalWipe
	    translate "veeWipe" = Just VeeWipe
	    translate "barnVeeWipe" = Just BarnVeeWipe
	    translate "zigZagWipe" = Just ZigZagWipe
	    translate "barnZigZagWipe" = Just BarnZigZagWipe
	    translate "miscShapeWipe" = Just MiscShapeWipe
	    translate "triangleWipe" = Just TriangleWipe
	    translate "arrowHeadWipe" = Just ArrowHeadWipe
	    translate "pentagonWipe" = Just PentagonWipe
	    translate "hexagonWipe" = Just HexagonWipe
	    translate "ellipseWipe" = Just EllipseWipe
	    translate "eyeWipe" = Just EyeWipe
	    translate "roundRectWipe" = Just RoundRectWipe
	    translate "starWipe" = Just StarWipe
	    translate "clockWipe" = Just ClockWipe
	    translate "pinWheelWipe" = Just PinWheelWipe
	    translate "singleSweepWipe" = Just SingleSweepWipe
	    translate "fanWipe" = Just FanWipe
	    translate "doubleFanWipe" = Just DoubleFanWipe
	    translate "doubleSweepWipe" = Just DoubleSweepWipe
	    translate "saloonDoorWipe" = Just SaloonDoorWipe
	    translate "windshieldWipe" = Just WindshieldWipe
	    translate "snakeWipe" = Just SnakeWipe
	    translate "spiralWipe" = Just SpiralWipe
	    translate "parallelSnakesWipe" = Just ParallelSnakesWipe
	    translate "boxSnakesWipe" = Just BoxSnakesWipe
	    translate "waterfallWipe" = Just WaterfallWipe
	    translate "pushWipe" = Just PushWipe
	    translate "slideWipe" = Just SlideWipe
	    translate "fade" = Just Fade
	    translate _ = Nothing
    toAttrFrTyp n BarWipe = Just (n, str2attr "barWipe")
    toAttrFrTyp n BoxWipe = Just (n, str2attr "boxWipe")
    toAttrFrTyp n FourBoxWipe = Just (n, str2attr "fourBoxWipe")
    toAttrFrTyp n BarnDoorWipe = Just (n, str2attr "barnDoorWipe")
    toAttrFrTyp n DiagonalWipe = Just (n, str2attr "diagonalWipe")
    toAttrFrTyp n BowTieWipe = Just (n, str2attr "bowTieWipe")
    toAttrFrTyp n MiscDiagonalWipe = Just (n, str2attr "miscDiagonalWipe")
    toAttrFrTyp n VeeWipe = Just (n, str2attr "veeWipe")
    toAttrFrTyp n BarnVeeWipe = Just (n, str2attr "barnVeeWipe")
    toAttrFrTyp n ZigZagWipe = Just (n, str2attr "zigZagWipe")
    toAttrFrTyp n BarnZigZagWipe = Just (n, str2attr "barnZigZagWipe")
    toAttrFrTyp n MiscShapeWipe = Just (n, str2attr "miscShapeWipe")
    toAttrFrTyp n TriangleWipe = Just (n, str2attr "triangleWipe")
    toAttrFrTyp n ArrowHeadWipe = Just (n, str2attr "arrowHeadWipe")
    toAttrFrTyp n PentagonWipe = Just (n, str2attr "pentagonWipe")
    toAttrFrTyp n HexagonWipe = Just (n, str2attr "hexagonWipe")
    toAttrFrTyp n EllipseWipe = Just (n, str2attr "ellipseWipe")
    toAttrFrTyp n EyeWipe = Just (n, str2attr "eyeWipe")
    toAttrFrTyp n RoundRectWipe = Just (n, str2attr "roundRectWipe")
    toAttrFrTyp n StarWipe = Just (n, str2attr "starWipe")
    toAttrFrTyp n ClockWipe = Just (n, str2attr "clockWipe")
    toAttrFrTyp n PinWheelWipe = Just (n, str2attr "pinWheelWipe")
    toAttrFrTyp n SingleSweepWipe = Just (n, str2attr "singleSweepWipe")
    toAttrFrTyp n FanWipe = Just (n, str2attr "fanWipe")
    toAttrFrTyp n DoubleFanWipe = Just (n, str2attr "doubleFanWipe")
    toAttrFrTyp n DoubleSweepWipe = Just (n, str2attr "doubleSweepWipe")
    toAttrFrTyp n SaloonDoorWipe = Just (n, str2attr "saloonDoorWipe")
    toAttrFrTyp n WindshieldWipe = Just (n, str2attr "windshieldWipe")
    toAttrFrTyp n SnakeWipe = Just (n, str2attr "snakeWipe")
    toAttrFrTyp n SpiralWipe = Just (n, str2attr "spiralWipe")
    toAttrFrTyp n ParallelSnakesWipe = Just (n, str2attr "parallelSnakesWipe")
    toAttrFrTyp n BoxSnakesWipe = Just (n, str2attr "boxSnakesWipe")
    toAttrFrTyp n WaterfallWipe = Just (n, str2attr "waterfallWipe")
    toAttrFrTyp n PushWipe = Just (n, str2attr "pushWipe")
    toAttrFrTyp n SlideWipe = Just (n, str2attr "slideWipe")
    toAttrFrTyp n Fade = Just (n, str2attr "fade")
instance XmlAttrType Subtype where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "bottom" = Just Bottom
	    translate "bottomCenter" = Just BottomCenter
	    translate "bottomLeft" = Just BottomLeft
	    translate "bottomLeftClockwise" = Just BottomLeftClockwise
	    translate "bottomLeftCounterClockwise" = Just BottomLeftCounterClockwise
	    translate "bottomLeftDiagonal" = Just BottomLeftDiagonal
	    translate "bottomRight" = Just BottomRight
	    translate "bottomRightClockwise" = Just BottomRightClockwise
	    translate "bottomRightCounterClockwise" = Just BottomRightCounterClockwise
	    translate "bottomRightDiagonal" = Just BottomRightDiagonal
	    translate "centerRight" = Just CenterRight
	    translate "centerTop" = Just CenterTop
	    translate "circle" = Just Circle
	    translate "clockwiseBottom" = Just ClockwiseBottom
	    translate "clockwiseBottomRight" = Just ClockwiseBottomRight
	    translate "clockwiseLeft" = Just ClockwiseLeft
	    translate "clockwiseNine" = Just ClockwiseNine
	    translate "clockwiseRight" = Just ClockwiseRight
	    translate "clockwiseSix" = Just ClockwiseSix
	    translate "clockwiseThree" = Just ClockwiseThree
	    translate "clockwiseTop" = Just ClockwiseTop
	    translate "clockwiseTopLeft" = Just ClockwiseTopLeft
	    translate "clockwiseTwelve" = Just ClockwiseTwelve
	    translate "cornersIn" = Just CornersIn
	    translate "cornersOut" = Just CornersOut
	    translate "counterClockwiseBottomLeft" = Just CounterClockwiseBottomLeft
	    translate "counterClockwiseTopRight" = Just CounterClockwiseTopRight
	    translate "crossfade" = Just Crossfade
	    translate "diagonalBottomLeft" = Just DiagonalBottomLeft
	    translate "diagonalBottomLeftOpposite" = Just DiagonalBottomLeftOpposite
	    translate "diagonalTopLeft" = Just DiagonalTopLeft
	    translate "diagonalTopLeftOpposite" = Just DiagonalTopLeftOpposite
	    translate "diamond" = Just Diamond
	    translate "doubleBarnDoor" = Just DoubleBarnDoor
	    translate "doubleDiamond" = Just DoubleDiamond
	    translate "down" = Just Down
	    translate "fadeFromColor" = Just FadeFromColor
	    translate "fadeToColor" = Just FadeToColor
	    translate "fanInHorizontal" = Just FanInHorizontal
	    translate "fanInVertical" = Just FanInVertical
	    translate "fanOutHorizontal" = Just FanOutHorizontal
	    translate "fanOutVertical" = Just FanOutVertical
	    translate "fivePoint" = Just FivePoint
	    translate "fourBlade" = Just FourBlade
	    translate "fourBoxHorizontal" = Just FourBoxHorizontal
	    translate "fourBoxVertical" = Just FourBoxVertical
	    translate "fourPoint" = Just FourPoint
	    translate "fromBottom" = Just FromBottom
	    translate "fromLeft" = Just FromLeft
	    translate "fromRight" = Just FromRight
	    translate "fromTop" = Just FromTop
	    translate "heart" = Just Heart
	    translate "horizontal" = Just Horizontal
	    translate "horizontalLeft" = Just HorizontalLeft
	    translate "horizontalLeftSame" = Just HorizontalLeftSame
	    translate "horizontalRight" = Just HorizontalRight
	    translate "horizontalRightSame" = Just HorizontalRightSame
	    translate "horizontalTopLeftOpposite" = Just HorizontalTopLeftOpposite
	    translate "horizontalTopRightOpposite" = Just HorizontalTopRightOpposite
	    translate "keyhole" = Just Keyhole
	    translate "left" = Just Left
	    translate "leftCenter" = Just LeftCenter
	    translate "leftToRight" = Just LeftToRight
	    translate "oppositeHorizontal" = Just OppositeHorizontal
	    translate "oppositeVertical" = Just OppositeVertical
	    translate "parallelDiagonal" = Just ParallelDiagonal
	    translate "parallelDiagonalBottomLeft" = Just ParallelDiagonalBottomLeft
	    translate "parallelDiagonalTopLeft" = Just ParallelDiagonalTopLeft
	    translate "parallelVertical" = Just ParallelVertical
	    translate "rectangle" = Just Rectangle
	    translate "right" = Just Right
	    translate "rightCenter" = Just RightCenter
	    translate "sixPoint" = Just SixPoint
	    translate "top" = Just Top
	    translate "topCenter" = Just TopCenter
	    translate "topLeft" = Just TopLeft
	    translate "topLeftClockwise" = Just TopLeftClockwise
	    translate "topLeftCounterClockwise" = Just TopLeftCounterClockwise
	    translate "topLeftDiagonal" = Just TopLeftDiagonal
	    translate "topLeftHorizontal" = Just TopLeftHorizontal
	    translate "topLeftVertical" = Just TopLeftVertical
	    translate "topRight" = Just TopRight
	    translate "topRightClockwise" = Just TopRightClockwise
	    translate "topRightCounterClockwise" = Just TopRightCounterClockwise
	    translate "topRightDiagonal" = Just TopRightDiagonal
	    translate "topToBottom" = Just TopToBottom
	    translate "twoBladeHorizontal" = Just TwoBladeHorizontal
	    translate "twoBladeVertical" = Just TwoBladeVertical
	    translate "twoBoxBottom" = Just TwoBoxBottom
	    translate "twoBoxLeft" = Just TwoBoxLeft
	    translate "twoBoxRight" = Just TwoBoxRight
	    translate "twoBoxTop" = Just TwoBoxTop
	    translate "up" = Just Up
	    translate "vertical" = Just Vertical
	    translate "verticalBottomLeftOpposite" = Just VerticalBottomLeftOpposite
	    translate "verticalBottomSame" = Just VerticalBottomSame
	    translate "verticalLeft" = Just VerticalLeft
	    translate "verticalRight" = Just VerticalRight
	    translate "verticalTopLeftOpposite" = Just VerticalTopLeftOpposite
	    translate "verticalTopSame" = Just VerticalTopSame
	    translate _ = Nothing
    toAttrFrTyp n Bottom = Just (n, str2attr "bottom")
    toAttrFrTyp n BottomCenter = Just (n, str2attr "bottomCenter")
    toAttrFrTyp n BottomLeft = Just (n, str2attr "bottomLeft")
    toAttrFrTyp n BottomLeftClockwise = Just (n, str2attr "bottomLeftClockwise")
    toAttrFrTyp n BottomLeftCounterClockwise = Just (n, str2attr "bottomLeftCounterClockwise")
    toAttrFrTyp n BottomLeftDiagonal = Just (n, str2attr "bottomLeftDiagonal")
    toAttrFrTyp n BottomRight = Just (n, str2attr "bottomRight")
    toAttrFrTyp n BottomRightClockwise = Just (n, str2attr "bottomRightClockwise")
    toAttrFrTyp n BottomRightCounterClockwise = Just (n, str2attr "bottomRightCounterClockwise")
    toAttrFrTyp n BottomRightDiagonal = Just (n, str2attr "bottomRightDiagonal")
    toAttrFrTyp n CenterRight = Just (n, str2attr "centerRight")
    toAttrFrTyp n CenterTop = Just (n, str2attr "centerTop")
    toAttrFrTyp n Circle = Just (n, str2attr "circle")
    toAttrFrTyp n ClockwiseBottom = Just (n, str2attr "clockwiseBottom")
    toAttrFrTyp n ClockwiseBottomRight = Just (n, str2attr "clockwiseBottomRight")
    toAttrFrTyp n ClockwiseLeft = Just (n, str2attr "clockwiseLeft")
    toAttrFrTyp n ClockwiseNine = Just (n, str2attr "clockwiseNine")
    toAttrFrTyp n ClockwiseRight = Just (n, str2attr "clockwiseRight")
    toAttrFrTyp n ClockwiseSix = Just (n, str2attr "clockwiseSix")
    toAttrFrTyp n ClockwiseThree = Just (n, str2attr "clockwiseThree")
    toAttrFrTyp n ClockwiseTop = Just (n, str2attr "clockwiseTop")
    toAttrFrTyp n ClockwiseTopLeft = Just (n, str2attr "clockwiseTopLeft")
    toAttrFrTyp n ClockwiseTwelve = Just (n, str2attr "clockwiseTwelve")
    toAttrFrTyp n CornersIn = Just (n, str2attr "cornersIn")
    toAttrFrTyp n CornersOut = Just (n, str2attr "cornersOut")
    toAttrFrTyp n CounterClockwiseBottomLeft = Just (n, str2attr "counterClockwiseBottomLeft")
    toAttrFrTyp n CounterClockwiseTopRight = Just (n, str2attr "counterClockwiseTopRight")
    toAttrFrTyp n Crossfade = Just (n, str2attr "crossfade")
    toAttrFrTyp n DiagonalBottomLeft = Just (n, str2attr "diagonalBottomLeft")
    toAttrFrTyp n DiagonalBottomLeftOpposite = Just (n, str2attr "diagonalBottomLeftOpposite")
    toAttrFrTyp n DiagonalTopLeft = Just (n, str2attr "diagonalTopLeft")
    toAttrFrTyp n DiagonalTopLeftOpposite = Just (n, str2attr "diagonalTopLeftOpposite")
    toAttrFrTyp n Diamond = Just (n, str2attr "diamond")
    toAttrFrTyp n DoubleBarnDoor = Just (n, str2attr "doubleBarnDoor")
    toAttrFrTyp n DoubleDiamond = Just (n, str2attr "doubleDiamond")
    toAttrFrTyp n Down = Just (n, str2attr "down")
    toAttrFrTyp n FadeFromColor = Just (n, str2attr "fadeFromColor")
    toAttrFrTyp n FadeToColor = Just (n, str2attr "fadeToColor")
    toAttrFrTyp n FanInHorizontal = Just (n, str2attr "fanInHorizontal")
    toAttrFrTyp n FanInVertical = Just (n, str2attr "fanInVertical")
    toAttrFrTyp n FanOutHorizontal = Just (n, str2attr "fanOutHorizontal")
    toAttrFrTyp n FanOutVertical = Just (n, str2attr "fanOutVertical")
    toAttrFrTyp n FivePoint = Just (n, str2attr "fivePoint")
    toAttrFrTyp n FourBlade = Just (n, str2attr "fourBlade")
    toAttrFrTyp n FourBoxHorizontal = Just (n, str2attr "fourBoxHorizontal")
    toAttrFrTyp n FourBoxVertical = Just (n, str2attr "fourBoxVertical")
    toAttrFrTyp n FourPoint = Just (n, str2attr "fourPoint")
    toAttrFrTyp n FromBottom = Just (n, str2attr "fromBottom")
    toAttrFrTyp n FromLeft = Just (n, str2attr "fromLeft")
    toAttrFrTyp n FromRight = Just (n, str2attr "fromRight")
    toAttrFrTyp n FromTop = Just (n, str2attr "fromTop")
    toAttrFrTyp n Heart = Just (n, str2attr "heart")
    toAttrFrTyp n Horizontal = Just (n, str2attr "horizontal")
    toAttrFrTyp n HorizontalLeft = Just (n, str2attr "horizontalLeft")
    toAttrFrTyp n HorizontalLeftSame = Just (n, str2attr "horizontalLeftSame")
    toAttrFrTyp n HorizontalRight = Just (n, str2attr "horizontalRight")
    toAttrFrTyp n HorizontalRightSame = Just (n, str2attr "horizontalRightSame")
    toAttrFrTyp n HorizontalTopLeftOpposite = Just (n, str2attr "horizontalTopLeftOpposite")
    toAttrFrTyp n HorizontalTopRightOpposite = Just (n, str2attr "horizontalTopRightOpposite")
    toAttrFrTyp n Keyhole = Just (n, str2attr "keyhole")
    toAttrFrTyp n Left = Just (n, str2attr "left")
    toAttrFrTyp n LeftCenter = Just (n, str2attr "leftCenter")
    toAttrFrTyp n LeftToRight = Just (n, str2attr "leftToRight")
    toAttrFrTyp n OppositeHorizontal = Just (n, str2attr "oppositeHorizontal")
    toAttrFrTyp n OppositeVertical = Just (n, str2attr "oppositeVertical")
    toAttrFrTyp n ParallelDiagonal = Just (n, str2attr "parallelDiagonal")
    toAttrFrTyp n ParallelDiagonalBottomLeft = Just (n, str2attr "parallelDiagonalBottomLeft")
    toAttrFrTyp n ParallelDiagonalTopLeft = Just (n, str2attr "parallelDiagonalTopLeft")
    toAttrFrTyp n ParallelVertical = Just (n, str2attr "parallelVertical")
    toAttrFrTyp n Rectangle = Just (n, str2attr "rectangle")
    toAttrFrTyp n Right = Just (n, str2attr "right")
    toAttrFrTyp n RightCenter = Just (n, str2attr "rightCenter")
    toAttrFrTyp n SixPoint = Just (n, str2attr "sixPoint")
    toAttrFrTyp n Top = Just (n, str2attr "top")
    toAttrFrTyp n TopCenter = Just (n, str2attr "topCenter")
    toAttrFrTyp n TopLeft = Just (n, str2attr "topLeft")
    toAttrFrTyp n TopLeftClockwise = Just (n, str2attr "topLeftClockwise")
    toAttrFrTyp n TopLeftCounterClockwise = Just (n, str2attr "topLeftCounterClockwise")
    toAttrFrTyp n TopLeftDiagonal = Just (n, str2attr "topLeftDiagonal")
    toAttrFrTyp n TopLeftHorizontal = Just (n, str2attr "topLeftHorizontal")
    toAttrFrTyp n TopLeftVertical = Just (n, str2attr "topLeftVertical")
    toAttrFrTyp n TopRight = Just (n, str2attr "topRight")
    toAttrFrTyp n TopRightClockwise = Just (n, str2attr "topRightClockwise")
    toAttrFrTyp n TopRightCounterClockwise = Just (n, str2attr "topRightCounterClockwise")
    toAttrFrTyp n TopRightDiagonal = Just (n, str2attr "topRightDiagonal")
    toAttrFrTyp n TopToBottom = Just (n, str2attr "topToBottom")
    toAttrFrTyp n TwoBladeHorizontal = Just (n, str2attr "twoBladeHorizontal")
    toAttrFrTyp n TwoBladeVertical = Just (n, str2attr "twoBladeVertical")
    toAttrFrTyp n TwoBoxBottom = Just (n, str2attr "twoBoxBottom")
    toAttrFrTyp n TwoBoxLeft = Just (n, str2attr "twoBoxLeft")
    toAttrFrTyp n TwoBoxRight = Just (n, str2attr "twoBoxRight")
    toAttrFrTyp n TwoBoxTop = Just (n, str2attr "twoBoxTop")
    toAttrFrTyp n Up = Just (n, str2attr "up")
    toAttrFrTyp n Vertical = Just (n, str2attr "vertical")
    toAttrFrTyp n VerticalBottomLeftOpposite = Just (n, str2attr "verticalBottomLeftOpposite")
    toAttrFrTyp n VerticalBottomSame = Just (n, str2attr "verticalBottomSame")
    toAttrFrTyp n VerticalLeft = Just (n, str2attr "verticalLeft")
    toAttrFrTyp n VerticalRight = Just (n, str2attr "verticalRight")
    toAttrFrTyp n VerticalTopLeftOpposite = Just (n, str2attr "verticalTopLeftOpposite")
    toAttrFrTyp n VerticalTopSame = Just (n, str2attr "verticalTopSame")
instance XmlAttrType Coordinated where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "true" = Just True
	    translate "false" = Just False
	    translate _ = Nothing
    toAttrFrTyp n True = Just (n, str2attr "true")
    toAttrFrTyp n False = Just (n, str2attr "false")
instance XmlAttrType ClibBoundary where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "parent" = Just Parent
	    translate "children" = Just Children
	    translate _ = Nothing
    toAttrFrTyp n Parent = Just (n, str2attr "parent")
    toAttrFrTyp n Children = Just (n, str2attr "children")
instance XmlAttrType Direction where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "forward" = Just Forward
	    translate "reverse" = Just Reverse
	    translate _ = Nothing
    toAttrFrTyp n Forward = Just (n, str2attr "forward")
    toAttrFrTyp n Reverse = Just (n, str2attr "reverse")
instance XmlContent TransitionFilter where
    fromElem (CElem (Elem "transitionFilter" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "transitionFilter" (toAttrs as) [])]
instance XmlAttributes TransitionFilter where
    fromAttrs as =
	TransitionFilter
	  { transitionFilterId = possibleA fromAttrToTyp "id" as
	  , transitionFilterClass = possibleA fromAttrToStr "class" as
	  , transitionFilterTitle = possibleA fromAttrToStr "title" as
	  , transitionFilterXml'lang = possibleA fromAttrToTyp "xml:lang" as
	  , transitionFilterType = possibleA fromAttrToTyp "type" as
	  , transitionFilterSubtype = possibleA fromAttrToTyp "subtype" as
	  , transitionFilterHorzRepeat = defaultA fromAttrToStr "0" "horzRepeat" as
	  , transitionFilterVertRepeat = defaultA fromAttrToStr "0" "vertRepeat" as
	  , transitionFilterBorderWidth = defaultA fromAttrToStr "0" "borderWidth" as
	  , transitionFilterBorderColor = defaultA fromAttrToStr "black" "borderColor" as
	  , transitionFilterFadeColor = defaultA fromAttrToStr "black" "fadeColor" as
	  , transitionFilterCoordinated = defaultA fromAttrToTyp False "coordinated" as
	  , transitionFilterClibBoundary = defaultA fromAttrToTyp Children "clibBoundary" as
	  , transitionFilterDur = possibleA fromAttrToStr "dur" as
	  , transitionFilterRepeatCount = possibleA fromAttrToStr "repeatCount" as
	  , transitionFilterRepeatDur = possibleA fromAttrToStr "repeatDur" as
	  , transitionFilterBegin = possibleA fromAttrToStr "begin" as
	  , transitionFilterEnd = possibleA fromAttrToStr "end" as
	  , transitionFilterValues = possibleA fromAttrToStr "values" as
	  , transitionFilterFrom = possibleA fromAttrToStr "from" as
	  , transitionFilterTo = possibleA fromAttrToStr "to" as
	  , transitionFilterBy = possibleA fromAttrToStr "by" as
	  , transitionFilterCalcMode = defaultA fromAttrToTyp Linear "calcMode" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "id" (transitionFilterId v)
	, maybeToAttr toAttrFrStr "class" (transitionFilterClass v)
	, maybeToAttr toAttrFrStr "title" (transitionFilterTitle v)
	, maybeToAttr toAttrFrTyp "xml:lang" (transitionFilterXml'lang v)
	, maybeToAttr toAttrFrTyp "type" (transitionFilterType v)
	, maybeToAttr toAttrFrTyp "subtype" (transitionFilterSubtype v)
	, defaultToAttr toAttrFrStr "horzRepeat" (transitionFilterHorzRepeat v)
	, defaultToAttr toAttrFrStr "vertRepeat" (transitionFilterVertRepeat v)
	, defaultToAttr toAttrFrStr "borderWidth" (transitionFilterBorderWidth v)
	, defaultToAttr toAttrFrStr "borderColor" (transitionFilterBorderColor v)
	, defaultToAttr toAttrFrStr "fadeColor" (transitionFilterFadeColor v)
	, defaultToAttr toAttrFrTyp "coordinated" (transitionFilterCoordinated v)
	, defaultToAttr toAttrFrTyp "clibBoundary" (transitionFilterClibBoundary v)
	, maybeToAttr toAttrFrStr "dur" (transitionFilterDur v)
	, maybeToAttr toAttrFrStr "repeatCount" (transitionFilterRepeatCount v)
	, maybeToAttr toAttrFrStr "repeatDur" (transitionFilterRepeatDur v)
	, maybeToAttr toAttrFrStr "begin" (transitionFilterBegin v)
	, maybeToAttr toAttrFrStr "end" (transitionFilterEnd v)
	, maybeToAttr toAttrFrStr "values" (transitionFilterValues v)
	, maybeToAttr toAttrFrStr "from" (transitionFilterFrom v)
	, maybeToAttr toAttrFrStr "to" (transitionFilterTo v)
	, maybeToAttr toAttrFrStr "by" (transitionFilterBy v)
	, defaultToAttr toAttrFrTyp "calcMode" (transitionFilterCalcMode v)
	]


{-Done-}

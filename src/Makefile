SOFTWARE = HaXml
VERSION  = 1.06

LIBSRCS = \
	Text/Xml/HaXml.hs Text/Xml/HaXml/Combinators.hs Text/Xml/HaXml/Lex.hs \
	Text/Xml/HaXml/Parse.hs Text/Xml/HaXml/Pretty.hs \
	Text/Xml/HaXml/Types.hs Text/Xml/HaXml/Validate.hs \
	Text/Xml/HaXml/Wrappers.hs Text/Xml/HaXml/OneOfN.hs \
	Text/Xml/HaXml/Xml2Haskell.hs Text/Xml/HaXml/Haskell2Xml.hs \
	Text/Xml/HaXml/Html/Generate.hs Text/Xml/HaXml/Html/Parse.hs \
	Text/Xml/HaXml/Html/Pretty.hs \
	Text/Xml/HaXml/Xtract/Combinators.hs \
	Text/Xml/HaXml/Xtract/Lex.hs \
	Text/Xml/HaXml/Xtract/Parse.hs \
	Text/Xml/HaXml/DtdToHaskell/TypeDef.hs \
	Text/Xml/HaXml/DtdToHaskell/Convert.hs \
	Text/Xml/HaXml/DtdToHaskell/Instance.hs \
	Text/ParserCombinators/HuttonMeijerWallace.hs \
	Text/PrettyPrint/HughesPJ.hs

TOOLSRCS = \
	tools/DtdToHaskell.hs tools/Xtract.hs tools/Validate.hs \
 	tools/Canonicalise.hs tools/MkOneOf.hs

COMPILER = $(findstring ghc, $(HC))
ifeq "$(COMPILER)" "ghc"
COMPILE  = hmake -HC=$(HC) -I. -package lang -package data -package-name HaXml
endif
COMPILER = $(findstring nhc98, $(HC))
ifeq "$(COMPILER)" "nhc98"
COMPILE  = hmake -HC=$(HC) -I. -K4M +CTS -H8M -CTS
endif

all: libs toolset
libs: HaXml Haskell2Xml Xml2Haskell
	ar r libHSHaXml.a `find Text -name *.o -print`
	tar cf interfaces.tar `find Text -name *.hi -print`
toolset: ../../DtdToHaskell ../../Xtract ../../Validate ../../Canonicalise


# library APIs

HaXml: $(LIBSRCS)
	$(COMPILE) Text/Xml/HaXml.hs
Haskell2Xml: $(LIBSRCS)
	$(COMPILE) Text/Xml/HaXml/Haskell2Xml.hs Text/Xml/HaXml/OneOfN.hs
Xml2Haskell: $(LIBSRCS)
	$(COMPILE) Text/Xml/HaXml/Xml2Haskell.hs


# standalone tools

../../DtdToHaskell: $(LIBSRCS) tools/DtdToHaskell.hs
	$(COMPILE) tools/DtdToHaskell
	mv tools/DtdToHaskell ../..
../../Xtract: $(LIBSRCS) tools/Xtract.hs
	$(COMPILE) tools/Xtract
	mv tools/Xtract ../..
../../Validate: $(LIBSRCS) tools/Validate.hs
	$(COMPILE) tools/Validate
	mv tools/Validate ../..
../../Canonicalise: $(LIBSRCS) tools/Canonicalise.hs
	$(COMPILE) tools/Canonicalise
	mv tools/Canonicalise ../..
../../MkOneOf: tools/MkOneOf.hs
	$(COMPILE) tools/MkOneOf
	mv tools/MkOneOf ../..

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
	Text/ParserCombinators/HuttonMeijerWallace.hs \
	Text/PrettyPrint/HughesPJ.hs

LIBOBJS = $(patsubst %.hs, %.o, $(LIBSRCS))

TOOLSRCS = \
	Text/Xml/HaXml/Xtract/Combinators.hs \
	Text/Xml/HaXml/Xtract/Lex.hs \
	Text/Xml/HaXml/Xtract/Parse.hs \
	Text/Xml/HaXml/DtdToHaskell/TypeDef.hs \
	Text/Xml/HaXml/DtdToHaskell/Convert.hs \
	Text/Xml/HaXml/DtdToHaskell/Instance.hs \
	tools/DtdToHaskell.hs tools/Xtract.hs tools/Validate.hs \
 	tools/Canonicalise.hs tools/MkOneOf.hs

TOOLSET = \
	../../DtdToHaskell ../../Xtract ../../Validate \
	../../Canonicalise ../../MkOneOf

# The caller *must* set the HC variable.
COMPILER = $(findstring ghc, $(HC))
ifeq "$(COMPILER)" "ghc"
COMPILE  = hmake -HC=$(HC) -I. -package lang -package data -package-name HaXml
endif
COMPILER = $(findstring nhc98, $(HC))
ifeq "$(COMPILER)" "nhc98"
COMPILE  = hmake -HC=$(HC) -I. -K4M +CTS -H8M -CTS
endif

.PHONY: all libs toolset

all: libs toolset
libs: libHSHaXml.a
toolset: $(TOOLSET)
install-ghc: libs
	cp libHSHaXml.a `cat ghclibdir`
	mkdir `cat ghcincdir`/HaXml || true
	cp interfaces.tar `cat ghcincdir`/HaXml
	cd `cat ghcincdir`/HaXml; tar xf interfaces.tar; rm interfaces.tar
	ghc-pkg --remove-package=HaXml || true
	ghc-pkg --add-package <pkg.conf
install-nhc98:
	cp libHSHaXml.a `cat nhc98libdir`/`harch`
	mkdir `cat nhc98incdir`/HaXml || true
	cp interfaces.tar `cat nhc98incdir`/HaXml
	cd `cat nhc98incdir`/HaXml; tar xf interfaces.tar; rm interfaces.tar

# update the sources if necessary
$(LIBSRCS) $(TOOLSRCS) Makefile: %: ../../src/%
	cp $< $@


# packaged library
libHSHaXml.a: $(LIBOBJS)
	ar r libHSHaXml.a `find Text -name *.o -print`
	tar cf interfaces.tar `find Text -name *.hi -print`


# library APIs
$(LIBOBJS): %.o : %.hs
	$(COMPILE) Text/Xml/HaXml.hs   Text/Xml/HaXml/Xml2Haskell.hs
	$(COMPILE) Text/Xml/HaXml/Haskell2Xml.hs   Text/Xml/HaXml/OneOfN.hs


# standalone tools
../../DtdToHaskell: $(LIBSRCS) $(TOOLSRCS)
	$(COMPILE) tools/DtdToHaskell
	mv tools/DtdToHaskell ../..
../../Xtract: $(LIBSRCS) $(TOOLSRCS)
	$(COMPILE) tools/Xtract
	mv tools/Xtract ../..
../../Validate: $(LIBSRCS) $(TOOLSRCS)
	$(COMPILE) tools/Validate
	mv tools/Validate ../..
../../Canonicalise: $(LIBSRCS) $(TOOLSRCS)
	$(COMPILE) tools/Canonicalise
	mv tools/Canonicalise ../..
../../MkOneOf: tools/MkOneOf.hs
	$(COMPILE) tools/MkOneOf
	mv tools/MkOneOf ../..

SOFTWARE = HaXml
VERSION  = 1.07

LIBSRCS = \
	Text/XML/HaXml.hs Text/XML/HaXml/Combinators.hs Text/XML/HaXml/Lex.hs \
	Text/XML/HaXml/Parse.hs Text/XML/HaXml/Pretty.hs \
	Text/XML/HaXml/Types.hs Text/XML/HaXml/Validate.hs \
	Text/XML/HaXml/Wrappers.hs Text/XML/HaXml/OneOfN.hs \
	Text/XML/HaXml/Xml2Haskell.hs Text/XML/HaXml/Haskell2Xml.hs \
	Text/XML/HaXml/Html/Generate.hs Text/XML/HaXml/Html/Parse.hs \
	Text/XML/HaXml/Html/Pretty.hs \
	Text/ParserCombinators/HuttonMeijerWallace.hs \
	Text/PrettyPrint/HughesPJ.hs

LIBOBJS = $(patsubst %.hs, %.o, $(LIBSRCS))

TOOLSRCS = \
	Text/XML/HaXml/Xtract/Combinators.hs \
	Text/XML/HaXml/Xtract/Lex.hs \
	Text/XML/HaXml/Xtract/Parse.hs \
	Text/XML/HaXml/DtdToHaskell/TypeDef.hs \
	Text/XML/HaXml/DtdToHaskell/Convert.hs \
	Text/XML/HaXml/DtdToHaskell/Instance.hs \
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
	cp libHSHaXml.a HSHaXml.o `cat ghclibdir`
	-rm -r `cat ghcincdir`/HaXml
	mkdir `cat ghcincdir`/HaXml
	cp interfaces.tar `cat ghcincdir`/HaXml
	cd `cat ghcincdir`/HaXml; tar xf interfaces.tar; rm interfaces.tar
	ghc-pkg --remove-package=HaXml || true
	ghc-pkg --add-package <pkg.conf
install-nhc98:
	cp libHSHaXml.a `cat nhc98libdir`/`harch`
	-rm -r `cat nhc98incdir`/HaXml
	mkdir `cat nhc98incdir`/HaXml
	cp interfaces.tar `cat nhc98incdir`/HaXml
	cd `cat nhc98incdir`/HaXml; tar xf interfaces.tar; rm interfaces.tar

# update the sources if necessary
$(LIBSRCS) $(TOOLSRCS) Makefile: %: ../../src/%
	cp $< $@


# packaged library
libHSHaXml.a: $(LIBOBJS)
	ar r libHSHaXml.a `find Text -name *.o -print`
	ld -r --whole-archive -o HSHaXml.o libHSHaXml.a		# for GHCi only
	tar cf interfaces.tar `find Text -name *.hi -print`


# library APIs
$(LIBOBJS): %.o : %.hs
	$(COMPILE) Text/XML/HaXml.hs   Text/XML/HaXml/Xml2Haskell.hs
	$(COMPILE) Text/XML/HaXml/Haskell2Xml.hs   Text/XML/HaXml/OneOfN.hs


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

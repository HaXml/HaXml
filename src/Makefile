SOFTWARE = HaXml
VERSION  = 1.08b

LIBSRCS = \
	Text/XML/HaXml.hs Text/XML/HaXml/Combinators.hs Text/XML/HaXml/Lex.hs \
	Text/XML/HaXml/Parse.hs Text/XML/HaXml/Pretty.hs \
	Text/XML/HaXml/Types.hs Text/XML/HaXml/Validate.hs \
	Text/XML/HaXml/Wrappers.hs Text/XML/HaXml/OneOfN.hs \
	Text/XML/HaXml/Xml2Haskell.hs Text/XML/HaXml/Haskell2Xml.hs \
	Text/XML/HaXml/Verbatim.hs \
	Text/XML/HaXml/Html/Generate.hs Text/XML/HaXml/Html/Parse.hs \
	Text/XML/HaXml/Html/Pretty.hs \
	Text/XML/HaXml/Xtract/Combinators.hs \
	Text/XML/HaXml/Xtract/Lex.hs \
	Text/XML/HaXml/Xtract/Parse.hs \
	Text/ParserCombinators/HuttonMeijerWallace.hs

LIBOBJS = $(patsubst %.hs, %.o, $(LIBSRCS))

TOOLSRCS = \
	Text/XML/HaXml/DtdToHaskell/TypeDef.hs \
	Text/XML/HaXml/DtdToHaskell/Convert.hs \
	Text/XML/HaXml/DtdToHaskell/Instance.hs \
	tools/DtdToHaskell.hs tools/Xtract.hs tools/Validate.hs \
 	tools/Canonicalise.hs tools/MkOneOf.hs

TOOLSET = \
	../../DtdToHaskell$(EXE) ../../Xtract$(EXE) ../../Validate$(EXE) \
	../../Canonicalise$(EXE) ../../MkOneOf$(EXE)

EXE = $(shell cat ../exe)

# The caller *must* set the HC variable.
COMPILER := $(findstring ghc, $(HC))
ifeq "$(COMPILER)" "ghc"
COMPILE  = $(HC) --make -cpp -i. $(shell cat ghcpkgs) -package-name HaXml
RENAME   = mv a.out
endif
COMPILER := $(findstring nhc98, $(HC))
ifeq "$(COMPILER)" "nhc98"
COMPILE  = hmake -hc=$(HC) -I. -K4M +CTS -H8M -CTS -package base
RENAME   = echo Built
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
	cp $(TOOLSET) `cat ../prefix`
install-nhc98:
	cp libHSHaXml.a `cat nhc98libdir`/`harch`
	-rm -r `cat nhc98incdir`/HaXml
	mkdir `cat nhc98incdir`/HaXml
	cp interfaces.tar `cat nhc98incdir`/HaXml
	cd `cat nhc98incdir`/HaXml; tar xf interfaces.tar; rm interfaces.tar
	cp $(TOOLSET) `cat ../prefix`

# update the sources if necessary
$(LIBSRCS) $(TOOLSRCS) Makefile Text/PrettyPrint/HughesPJ.hs: %: ../../src/%
	cp $< $@


# packaged library
libHSHaXml.a: $(LIBOBJS)
	ar r libHSHaXml.a `find Text -name *.o -print`
	ld -r --whole-archive -o HSHaXml.o libHSHaXml.a		# for GHCi only
	tar cf interfaces.tar `find Text -name *.hi -print`


# library APIs
$(LIBOBJS): $(LIBSRCS)
$(LIBOBJS): %.o : %.hs
	$(COMPILE) $<


# standalone tools
$(TOOLSET): $(LIBSRCS) $(TOOLSRCS)
	cd tools; $(COMPILE) -i.. $(patsubst ../../%${EXE}, %, $@)
	cd tools; $(RENAME) $(patsubst ../../%, %, $@)
	mv $(patsubst ../../%, tools/%, $@) ../..

#../../DtdToHaskell$(EXE): $(LIBSRCS) $(TOOLSRCS)
#	$(COMPILE) tools/DtdToHaskell.hs
#	$(RENAME) $@
#	mv $@ ../..
#../../Xtract$(EXE): $(LIBSRCS) $(TOOLSRCS)
#	$(COMPILE) tools/Xtract.hs
#	$(RENAME) $@
#	mv $@ ../..
#../../Validate$(EXE): $(LIBSRCS) $(TOOLSRCS)
#	$(COMPILE) tools/Validate.hs
#	$(RENAME) $@
#	mv $@ ../..
#../../Canonicalise$(EXE): $(LIBSRCS) $(TOOLSRCS)
#	$(COMPILE) tools/Canonicalise.hs
#	$(RENAME) $@
#	mv $@ ../..
#../../MkOneOf$(EXE): tools/MkOneOf.hs
#	$(COMPILE) tools/MkOneOf.hs
#	$(RENAME) $@
#	mv $@ ../..


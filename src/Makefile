SOFTWARE = HaXml
VERSION = 1.25.3

LIBSRCS = \
	Text/XML/HaXml.hs Text/XML/HaXml/Combinators.hs Text/XML/HaXml/Lex.hs \
	Text/XML/HaXml/Posn.hs \
	Text/XML/HaXml/Parse.hs Text/XML/HaXml/Pretty.hs \
	Text/XML/HaXml/Types.hs Text/XML/HaXml/Validate.hs \
	Text/XML/HaXml/Namespaces.hs \
	Text/XML/HaXml/Wrappers.hs \
	Text/XML/HaXml/OneOfN.hs \
	Text/XML/HaXml/ParseLazy.hs \
	Text/XML/HaXml/ByteStringPP.hs \
	Text/XML/HaXml/TypeMapping.hs \
	Text/XML/HaXml/XmlContent.hs \
	Text/XML/HaXml/XmlContent/Parser.hs \
	Text/XML/HaXml/XmlContent/Haskell.hs \
	Text/XML/HaXml/Verbatim.hs Text/XML/HaXml/Escape.hs \
	Text/XML/HaXml/SAX.hs \
	Text/XML/HaXml/ShowXmlLazy.hs \
	Text/XML/HaXml/Html/Generate.hs Text/XML/HaXml/Html/Parse.hs \
	Text/XML/HaXml/Html/Pretty.hs \
	Text/XML/HaXml/Html/ParseLazy.hs \
	Text/XML/HaXml/Xtract/Combinators.hs \
	Text/XML/HaXml/Xtract/Lex.hs \
	Text/XML/HaXml/Xtract/Parse.hs \
	Text/XML/HaXml/Schema/XSDTypeModel.hs \
	Text/XML/HaXml/Schema/Parse.hs \
        Text/XML/HaXml/Schema/HaskellTypeModel.hs \
        Text/XML/HaXml/Schema/PrettyHaskell.hs \
        Text/XML/HaXml/Schema/PrettyHsBoot.hs \
        Text/XML/HaXml/Schema/Environment.hs \
        Text/XML/HaXml/Schema/NameConversion.hs \
        Text/XML/HaXml/Schema/TypeConversion.hs \
        Text/XML/HaXml/Schema/PrimitiveTypes.hs \
        Text/XML/HaXml/Schema/Schema.hs \

LIBOBJS = $(patsubst %.hs, %.o, $(LIBSRCS))

TOOLSRCS = \
	Text/XML/HaXml/DtdToHaskell/TypeDef.hs \
	Text/XML/HaXml/DtdToHaskell/Convert.hs \
	Text/XML/HaXml/DtdToHaskell/Instance.hs \
	tools/DtdToHaskell.hs tools/Xtract.hs tools/Validate.hs \
 	tools/Canonicalise.hs tools/MkOneOf.hs \
 	tools/CanonicaliseLazy.hs \
 	tools/XsdToHaskell.hs \

TOOLSET = \
	../../DtdToHaskell$(EXE) ../../Xtract$(EXE) ../../Validate$(EXE) \
	../../Canonicalise$(EXE) ../../MkOneOf$(EXE) \
	../../CanonicaliseLazy$(EXE) \
	../../XsdToHaskell$(EXE)

EXE = $(shell cat ../exe)
INSTALLDIR = $(shell cat ../prefix)
WHOLEARCHIVE = $(shell cat ../ldopt)

# The caller *must* set the HC variable.
COMPILER := $(findstring ghc, $(HC))
ifeq "$(COMPILER)" "ghc"
COMPILEXEC = $(HC) --make -cpp -i. $(shell cat ghcpkgs) \
		-package polyparse -package bytestring -package filepath \
		-DMYVERSION='\"$(VERSION)\"'
COMPILE  = $(COMPILEXEC) -package-name HaXml-$(VERSION)
RENAME   = $(shell cat ../out)
endif
COMPILER := $(findstring nhc98, $(HC))
ifeq "$(COMPILER)" "nhc98"
COMPILEXEC = hmake -hc=$(HC) -I. -K4M +CTS -H8M -CTS -package base \
		-package polyparse -package bytestring -package filepath \
		-DMYVERSION='\"$(VERSION)\"'
COMPILE  = $(COMPILEXEC)
RENAME   = echo Built
endif

.PHONY: all libs toolset

all: libs toolset
libs: libHSHaXml.a
toolset: $(TOOLSET)
install-filesonly-ghc: libs $(INSTALLDIR)
	cp libHSHaXml.a `cat ghclibdir`
	-ranlib `cat ghclibdir`/libHSHaXml.a	# ignore if fails on Linux
	-cp HSHaXml.o `cat ghclibdir`		# file may not exist on MacOS X
	rm -rf `cat ghcincdir`/HaXml
	mkdir -p `cat ghcincdir`/HaXml
	cp interfaces.tar `cat ghcincdir`/HaXml
	cd `cat ghcincdir`/HaXml; tar xf interfaces.tar; rm interfaces.tar
	cp $(TOOLSET) $(INSTALLDIR)
install-filesonly-nhc98: libs $(INSTALLDIR)
	cp libHSHaXml.a `cat nhc98libdir`/`harch`
	-ranlib `cat nhc98libdir`/`harch`/libHSHaXml.a
	rm -rf `cat nhc98incdir`/HaXml
	mkdir -p `cat nhc98incdir`/HaXml
	cp interfaces.tar `cat nhc98incdir`/packages/HaXml
	cd `cat nhc98incdir`/packages/HaXml; tar xf interfaces.tar; rm interfaces.tar
	cp $(TOOLSET) $(INSTALLDIR)
install-ghc: install-filesonly-ghc HaXml.pkgconf
	`cat ghcpkgcmd` --remove-package=HaXml || true	# ignore any error
	`cat ghcpkgcmd` --add-package <pkg.conf || \
	`cat ghcpkgcmd` register HaXml.pkgconf
	echo "you can safely ignore any errors from --add-package"
install-nhc98: install-filesonly-nhc98
install-tools-hugs: toolset-hugs $(INSTALLDIR)
	cp $(patsubst ../../%, %, ${TOOLSET}) $(INSTALLDIR)

# update the sources if necessary
$(LIBSRCS) $(TOOLSRCS) Makefile: %: ../../src/%
	cp $< $@
HaXml.pkgconf: ../../HaXml.cabal ghcincdirraw ghclibdirraw
	cp ../../HaXml.cabal $@
	echo "import-dirs:	`cat ghcincdirraw`HaXml" >>$@
	echo "library-dirs:	`cat ghclibdirraw`" >>$@
	echo "depends:		base, haskell98, polyparse" >>$@
	echo "hs-libraries:	HS$(SOFTWARE)" >>$@



# packaged library
libHSHaXml.a: $(LIBSRCS)
	$(COMPILE) $(LIBSRCS)
	ar r libHSHaXml.a $(LIBOBJS)
	-ld -r $(WHOLEARCHIVE) -o HSHaXml.o libHSHaXml.a	# for GHCi only
	tar cf interfaces.tar `find Text -name *.hi -print`


# standalone tools
$(TOOLSET): $(LIBSRCS) $(TOOLSRCS)
	cd tools; $(COMPILEXEC) -i.. $(patsubst ../../%${EXE}, %, $@)
	cd tools; $(RENAME) $(patsubst ../../%, %, $@)
	mv $(patsubst ../../%, tools/%, $@) ../..

toolset-hugs:
	for file in tools/*.hs ;\
	do tool=`basename $$file .hs` ;\
	   echo '#!'`which runhugs` >$$tool ;\
	   cat $$file >>$$tool ;\
	   chmod +x $$tool ;\
	done

$(INSTALLDIR):
	if [ ! -d $(INSTALLDIR) ] ; then mkdir -p $(INSTALLDIR); fi


SOFTWARE = HaXml
VERSION  = 1.06

LIBSRCS = \
	src/Text/Xml/HaXml.hs src/Text/Xml/HaXml/Combinators.hs \
	src/Text/Xml/HaXml/Lex.hs \
	src/Text/Xml/HaXml/Parse.hs src/Text/Xml/HaXml/Pretty.hs \
	src/Text/Xml/HaXml/Types.hs src/Text/Xml/HaXml/Validate.hs \
	src/Text/Xml/HaXml/Wrappers.hs src/Text/Xml/HaXml/OneOfN.hs \
	src/Text/Xml/HaXml/Xml2Haskell.hs src/Text/Xml/HaXml/Haskell2Xml.hs \
	src/Text/Xml/HaXml/Html/Generate.hs src/Text/Xml/HaXml/Html/Parse.hs \
	src/Text/Xml/HaXml/Html/Pretty.hs \
	src/Text/Xml/HaXml/Xtract/Combinators.hs \
	src/Text/Xml/HaXml/Xtract/Lex.hs \
	src/Text/Xml/HaXml/Xtract/Parse.hs \
	src/Text/Xml/HaXml/DtdToHaskell/TypeDef.hs \
	src/Text/Xml/HaXml/DtdToHaskell/Convert.hs \
	src/Text/Xml/HaXml/DtdToHaskell/Instance.hs \
	src/Text/ParserCombinators/HuttonMeijerWallace.hs \
	src/Text/PrettyPrint/HughesPJ.hs

TOOLSRCS = \
	src/tools/DtdToHaskell.hs src/tools/Xtract.hs src/tools/Validate.hs \
	src/tools/Canonicalise.hs src/tools/MkOneOf.hs

AUX      = Makefile src/Makefile docs/* examples/* README LICENSE COPYRIGHT
ALLFILES = $(LIBSRCS) $(TOOLSRCS) $(AUX)

.PHONY: all libs tools haddock

COMPILERS = $(shell cat obj/compilers)
LIBS  = $(patsubst %, libs-%, $(COMPILERS))
TOOLS = $(patsubst %, tools-%, $(COMPILERS))

all: $(LIBS) $(TOOLS)
libs: $(LIBS)
tools: $(TOOLS)
libs-ghc:
	cd obj/ghc; make HC=ghc libs
libs-nhc98:
	cd obj/nhc98; make HC=nhc98 libs
tools-ghc:
	cd obj/ghc; make HC=ghc toolset
tools-nhc98:
	cd obj/nhc98; make HC=nhc98 toolset
haddock:
	for file in $(LIBSRCS); \
		do cpp -P -traditional -D__NHC__ $$file >$$file.uncpp; \
		done
	haddock -h -t HaXml -o docs/HaXml $(patsubst %, %.uncpp, $(LIBSRCS))

# packaging a distribution

srcDist: $(ALLFILES)
	rm -f $(SOFTWARE)-$(VERSION).tar $(SOFTWARE)-$(VERSION).tar.gz
	mkdir $(SOFTWARE)-$(VERSION)
	tar cf - $(ALLFILES) | ( cd $(SOFTWARE)-$(VERSION); tar xf - )
	tar cf $(SOFTWARE)-$(VERSION).tar $(SOFTWARE)-$(VERSION)
	rm -rf $(SOFTWARE)-$(VERSION)
	gzip $(SOFTWARE)-$(VERSION).tar

zipDist: $(ALLFILES)
	rm -f $(SOFTWARE)-$(VERSION).zip
	mkdir $(SOFTWARE)-$(VERSION)
	tar cf - $(ALLFILES) | ( cd $(SOFTWARE)-$(VERSION); tar xf - )
	zip -r $(SOFTWARE)-$(VERSION).zip $(SOFTWARE)-$(VERSION)
	rm -rf $(SOFTWARE)-$(VERSION)


# clear up rubbish
clean:
	-rm -r obj/ghc obj/nhc98
	-rm `find src/Text -name *.uncpp -print`
	cd examples;    rm -f *.hi *.o
realclean: clean
	rm -f DtdToHaskell Xtract Validate Canonicalise MkOneOf


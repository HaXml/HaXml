SOFTWARE = HaXml
VERSION  = 1.09

SRCS = \
	src/Text/XML/HaXml.hs src/Text/XML/HaXml/Combinators.hs \
	src/Text/XML/HaXml/Lex.hs \
	src/Text/XML/HaXml/Parse.hs src/Text/XML/HaXml/Pretty.hs \
	src/Text/XML/HaXml/Types.hs src/Text/XML/HaXml/Validate.hs \
	src/Text/XML/HaXml/Wrappers.hs src/Text/XML/HaXml/OneOfN.hs \
	src/Text/XML/HaXml/Xml2Haskell.hs src/Text/XML/HaXml/Haskell2Xml.hs \
	src/Text/XML/HaXml/Html/Generate.hs src/Text/XML/HaXml/Html/Parse.hs \
	src/Text/XML/HaXml/Html/Pretty.hs \
	src/Text/XML/HaXml/Xtract/Combinators.hs \
	src/Text/XML/HaXml/Xtract/Lex.hs \
	src/Text/XML/HaXml/Xtract/Parse.hs \
	src/Text/XML/HaXml/DtdToHaskell/TypeDef.hs \
	src/Text/XML/HaXml/DtdToHaskell/Convert.hs \
	src/Text/XML/HaXml/DtdToHaskell/Instance.hs \
	src/Text/ParserCombinators/HuttonMeijerWallace.hs \
	src/Text/PrettyPrint/HughesPJ.hs
TOOLSRCS = \
	src/tools/DtdToHaskell.hs src/tools/Xtract.hs src/tools/Validate.hs \
	src/tools/Canonicalise.hs src/tools/MkOneOf.hs

AUX =	configure Makefile src/Makefile src/pkg.conf docs/* examples/* \
	README LICENSE COPYRIGHT script/echo.c
ALLFILES = $(SRCS) $(TOOLSRCS) $(AUX)

.PHONY: all libs tools haddock

COMPILERS = $(shell cat obj/compilers)
LIBS  = $(patsubst %, libs-%, $(COMPILERS))
TOOLS = $(patsubst %, tools-%, $(COMPILERS))
INSTALL = $(patsubst %, install-%, $(COMPILERS))

all: $(LIBS) $(TOOLS)
libs: $(LIBS)
tools: $(TOOLS)
install: $(INSTALL)
libs-ghc:
	cd obj/ghc; $(MAKE) HC=$(shell cat obj/ghccmd) libs
libs-nhc98:
	cd obj/nhc98; $(MAKE) HC=nhc98 libs
tools-ghc:
	cd obj/ghc; $(MAKE) HC=$(shell cat obj/ghccmd) toolset
tools-nhc98:
	cd obj/nhc98; $(MAKE) HC=nhc98 toolset
install-ghc:
	cd obj/ghc; $(MAKE) HC=$(shell cat obj/ghccmd) install-ghc
install-nhc98:
	cd obj/nhc98; $(MAKE) HC=nhc98 install-nhc98
haddock:
	for file in $(SRCS); \
		do cpp -P -traditional -D__NHC__ $$file >$$file.uncpp; \
		done
	haddock -h -t HaXml -o docs/HaXml $(patsubst %, %.uncpp, $(SRCS))
	rm $(patsubst %, %.uncpp, $(SRCS))

# packaging a distribution

srcDist: $(ALLFILES)
	rm -f $(SOFTWARE)-$(VERSION).tar $(SOFTWARE)-$(VERSION).tar.gz
	mkdir $(SOFTWARE)-$(VERSION)
	tar cf - $(ALLFILES) | ( cd $(SOFTWARE)-$(VERSION); tar xf - )
	-rm -rf $(SOFTWARE)-$(VERSION)/docs/CVS
	-rm -rf $(SOFTWARE)-$(VERSION)/examples/CVS
	-rm -rf $(SOFTWARE)-$(VERSION)/examples/SMIL/CVS
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
	-cd examples;    rm -f *.hi *.o
realclean: clean
	-rm -f DtdToHaskell Xtract Validate Canonicalise MkOneOf


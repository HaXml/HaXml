#Makefile for GHC version of DrIFT
###################################

### edit this next var to point to your version of ghc (v4.02 or higher)
#GHC =/local/fp/bin/sparc-sun-solaris2/ghc-4.02 
###
#NHC = nhc98
HC = $(GHC)

#RTSFLAGS = -H10000000
#GHCFLAGS = -syslib ghc -cpp -fglasgow-exts -recomp
#OPTFLAGS = -O -fvia-C -O2-for-C

# for GHC use: -O -cpp -syslib exts
# for NHC use: -cpp
HCFLAGS = -O -cpp -syslib exts

############################################

SHELL = /bin/sh

.SUFFIXES : .o .hi .hs .lhs


.hs.o 	: 
	$(HC) $(RTSFLAGS) $(OPTFLAGS) $(HCFLAGS) -c $< 
.lhs.o : 
	$(HC) $(RTSFLAGS) $(OPTFLAGS) $(HCFLAGS) -c $< 

.o.hi 	:
	@:


#############################################
OBJECTS = ParseLib2.o DataP.o CommandP.o \
	  PreludData.o RuleUtils.o StandardRules.o \
	  UserRuleBinary.o UserRuleXml.o \
	  UserRules.o ChaseImports.o DrIFT.o

LIBOBJECTS = Pretty.o Literate.o
# $@ $
SOURCES = 
all : DrIFT.o
	$(HC) $(RTSFLAGS) $(OPTFLAGS) $(HCFLAGS) -o DrIFT $ \
	  $(LIBOBJECTS) $(OBJECTS)

clean : 
	rm *.hi *.o

# DO NOT DELETE: Beginning of Haskell dependencies
ChaseImports.o : ChaseImports.hs
ChaseImports.o : ./StandardRules.hi
ChaseImports.o : ./DataP.hi
ChaseImports.o : ./CommandP.hi
ChaseImports.o : ./ParseLib2.hi
ChaseImports.o : ./Literate.hi
CommandP.o : CommandP.hs
CommandP.o : ./ParseLib2.hi
CommandP.o : ./DataP.hi
DataP.o : DataP.lhs
DataP.o : ./ParseLib2.hi
Literate.o : Literate.lhs
ParseLib2.o : ParseLib2.hs
PreludData.o : PreludData.hs
PreludData.o : ./DataP.hi
Pretty.o : Pretty.lhs
RuleUtils.o : RuleUtils.hs
RuleUtils.o : ./Pretty.hi
RuleUtils.o : ./DataP.hi
StandardRules.o : StandardRules.lhs
StandardRules.o : ./RuleUtils.hi
UserRules.o : UserRules.hs
UserRules.o : ./StandardRules.hi
UserRules.o : ./RuleUtils.hi
UserRules.o : ./UserRuleBinary.hi
UserRules.o : ./UserRuleXml.hi
UserRuleBinary.o : ./UserRuleBinary.hs
UserRuleBinary.o : ./StandardRules.hi
UserRuleBinary.o : ./RuleUtils.hi
UserRuleXml.o : ./UserRuleXml.hs
UserRuleXml.o : ./StandardRules.hi
UserRuleXml.o : ./RuleUtils.hi
DrIFT.o : DrIFT.hs
DrIFT.o : ./ChaseImports.hi
DrIFT.o : ./UserRules.hi
DrIFT.o : ./StandardRules.hi
DrIFT.o : ./RuleUtils.hi
DrIFT.o : ./PreludData.hi
DrIFT.o : ./DataP.hi
DrIFT.o : ./Pretty.hi
# DO NOT DELETE: End of Haskell dependencies

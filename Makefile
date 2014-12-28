OPTS = --make
EXES = AcmIcpcTeam AlternatingCharacters AngryChildren FillingJars FillingJarsLA FillingJarsVect FindDigits GameOfThrones1 IsFibo LoveLetterMystery MaximizingXOR UtopianTree

# to enable profiling, execute ./Program +RTS -p
ifdef PROFILE
  OPTS += -prof -fprof-auto -rtsopts
endif

all: $(EXES)

AcmIcpcTeam:
	ghc $(OPTS) AcmIcpcTeam.hs

AlternatingCharacters:
	ghc $(OPTS) AlternatingCharacters.hs

AngryChildren:
	ghc $(OPTS) AngryChildren.hs

FillingJars:
	ghc $(OPTS) FillingJars.hs

FillingJarsLA:
	ghc $(OPTS) FillingJarsLA.hs

FillingJarsVect:
	ghc $(OPTS) FillingJarsVect.hs

FindDigits:
	ghc $(OPTS) FindDigits.hs

GameOfThrones1:
	ghc $(OPTS) GameOfThrones1.hs

IsFibo:
	ghc $(OPTS) IsFibo.hs

LoveLetterMystery:
	ghc $(OPTS) LoveLetterMystery.hs

MaximizingXOR:
	ghc $(OPTS) MaximizingXOR.hs

UtopianTree:
	ghc $(OPTS) UtopianTree.hs

clean:
	rm *.o *.hi $(EXES)

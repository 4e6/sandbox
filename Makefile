OPTS = $(OPTS)

# to enable profiling, execute ./Program +RTS -p
ifdef PROFILE
  OPTS += -prof -fprof-auto -rtsopts
endif

all: AlternatingCharacters AngryChildren FindDigits IsFibo LoveLetterMystery MaximizingXOR UtopianTree

AlternatingCharacters:
	ghc $(OPTS) AlternatingCharacters.hs

AngryChildren:
	ghc $(OPTS) AngryChildren.hs

FindDigits:
	ghc $(OPTS) FindDigits.hs

IsFibo:
	ghc $(OPTS) IsFibo.hs

LoveLetterMystery:
	ghc $(OPTS) LoveLetterMystery.hs

MaximizingXOR:
	ghc $(OPTS) MaximizingXOR.hs

UtopianTree:
	ghc $(OPTS) UtopianTree.hs

clean:
	rm *.o *.hi AlternatingCharacters AngryChildren FindDigits IsFibo LoveLetterMystery MaximizingXOR UtopianTree

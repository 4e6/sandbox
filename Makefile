all: AlternatingCharacters FindDigits IsFibo MaximizingXOR UtopianTree

AlternatingCharacters:
	ghc --make AlternatingCharacters.hs

FindDigits:
	ghc --make FindDigits.hs

IsFibo:
	ghc --make IsFibo.hs

MaximizingXOR:
	ghc --make MaximizingXOR.hs

UtopianTree:
	ghc --make UtopianTree.hs

clean:
	rm *.o *.hi AlternatingCharacters FindDigits IsFibo MaximizingXOR UtopianTree

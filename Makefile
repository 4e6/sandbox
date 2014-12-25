all: FindDigits IsFibo MaximizingXOR UtopianTree

FindDigits:
	ghc --make FindDigits.hs

IsFibo:
	ghc --make IsFibo.hs

MaximizingXOR:
	ghc --make MaximizingXOR.hs

UtopianTree:
	ghc --make UtopianTree.hs

clean:
	rm *.o *.hi FindDigits IsFibo MaximizingXOR UtopianTree

all: FindDigits IsFibo UtopianTree

FindDigits:
	ghc --make FindDigits.hs

IsFibo:
	ghc --make IsFibo.hs

UtopianTree:
	ghc --make UtopianTree.hs

clean:
	rm *.o *.hi FindDigits IsFibo UtopianTree

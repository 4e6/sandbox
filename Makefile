all: IsFibo UtopianTree

IsFibo:
	ghc --make IsFibo.hs

UtopianTree:
	ghc --make UtopianTree.hs

clean:
	rm *.o *.hi IsFibo UtopianTree

all: UtopianTree

UtopianTree:
	ghc --make UtopianTree.hs

clean:
	rm *.o *.hi UtopianTree

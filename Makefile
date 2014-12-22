all: utopian-tree

utopian-tree:
	ghc -o utopian-tree UtopianTree.hs

clean:
	rm *.o *.hi utopian-tree

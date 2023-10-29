all:
	happy -gcai ParLI.y
	alex -g LexLI.x
	ghc --make TestLI.hs -o TestLI

clean:
	del -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	del -f DocLI.* LexLI.* ParLI.* LayoutLI.* SkelLI.* PrintLI.* TestLI.* AbsLI.* TestLI ErrM.* SharedString.* ComposOp.* LI1.dtd XMLLI.* Makefile*
	


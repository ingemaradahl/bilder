all:
	happy -gca FrontEnd/ParGrammar.y
	alex -g FrontEnd/LexGrammar.x
	(cd FrontEnd/; latex DocGrammar.tex; dvips DocGrammar.dvi -o DocGrammar.ps)
	ghc --make FrontEnd/TestGrammar.hs -o FrontEnd/TestGrammar
clean:
	-rm -f FrontEnd/*.log FrontEnd/*.aux FrontEnd/*.hi FrontEnd/*.o FrontEnd/*.dvi
	-rm -f FrontEnd/DocGrammar.ps
distclean: clean
	-rm -f FrontEnd/DocGrammar.* FrontEnd/LexGrammar.* FrontEnd/ParGrammar.* FrontEnd/LayoutGrammar.* FrontEnd/SkelGrammar.* FrontEnd/PrintGrammar.* FrontEnd/TestGrammar.* FrontEnd/AbsGrammar.* FrontEnd/TestGrammar FrontEnd/ErrM.* FrontEnd/SharedString.* FrontEnd/Grammar.dtd FrontEnd/XMLGrammar.* Makefile*
	-rmdir -p FrontEnd/

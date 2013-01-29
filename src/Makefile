FL_FILES = FrontEnd/AbsGrammar.hs FrontEnd/ErrM.hs FrontEnd/Instances.hs \
	FrontEnd/LexGrammar.hs FrontEnd/ParGrammar.hs FrontEnd/PrintGrammar.hs \
	FrontEnd/SkelGrammar.hs
FL_OBJS = $(FL_FILES:*.hs=*.o)
GLSL_FILES = FrontEnd/AbsGLSL.hs FrontEnd/PrintGLSL.hs
GLSL_OBJS = $(GLSL_FILES:*.hs=*.o)
OBJS = $(shell find . -name '*.o')
HIS = $(shell find . -name '*.hi')

all: bildc cgibildc

$(FL_FILES): Grammar.cf
	bnfc -haskell -p FrontEnd Grammar.cf
	happy -gca FrontEnd/ParGrammar.y
	alex -g FrontEnd/LexGrammar.x
	cat FrontEnd/Instances_head.hs > FrontEnd/Instances.hs
	awk '/^newtype/ && $$2!="CFloat" { printf "instance Token %s where\n\
	  tkpos (%s (p,_)) = p\n\
	  tkident (%s (_,s)) = s\n\n", $$2, $$2, $$2 }' FrontEnd/AbsGrammar.hs >> FrontEnd/Instances.hs

$(GLSL_FILES): GLSL.cf
	bnfc -haskell -p GLSL GLSL.cf
	sed "s/GLSL\./FrontEnd./g" GLSL/AbsGLSL.hs > FrontEnd/AbsGLSL.hs
	sed "s/GLSL\./FrontEnd./g" GLSL/PrintGLSL.hs > FrontEnd/PrintGLSL.hs
	-rm -r GLSL

%.o: %.hs
	ghc $<

bildc: $(FL_FILES) $(GLSL_FILES) $(OBJS)
	ghc bildc.hs -o bildc

cgibildc: $(FL_FILES) $(GLSL_FILES) $(OBJS)
	ghc cgibildc.hs -o cgibildc

.NOTPARALLEL:

.PHONY: clean
clean:
	-rm -f FrontEnd/AbsGLSL.* FrontEnd/PrintGLSL.* FrontEnd/DocGrammar.* FrontEnd/LexGrammar.* FrontEnd/ParGrammar.* FrontEnd/LayoutGrammar.* FrontEnd/SkelGrammar.* FrontEnd/PrintGrammar.* FrontEnd/TestGrammar.* FrontEnd/AbsGrammar.* FrontEnd/TestGrammar FrontEnd/ErrM.* FrontEnd/SharedString.* FrontEnd/Grammar.dtd FrontEnd/Instances.o FrontEnd/Instances.hi $(OBJS) $(HIS)
	-rm -f bildc cgibildc

#!/bin/sh
echo "---------- Building Filter Language(?) Grammar"
bnfc -haskell -p FrontEnd Grammar.cf &&
# like the Makefile provided by bnfc -m - but without documentation
#   and compilation of TestGrammar binary.
happy -gca FrontEnd/ParGrammar.y &&
alex -g FrontEnd/LexGrammar.x

if [ $? -ne 0 ]
then
  echo "Something fialed."
  exit 1
fi

cat FrontEnd/Instances_head.hs > FrontEnd/Instances.hs &&
awk '/^newtype/ && $2!="CFloat" { printf "instance Token %s where\n\
  tkpos (%s (p,_)) = p\n\
  tkident (%s (_,s)) = s\n\n", $2, $2, $2 }' FrontEnd/AbsGrammar.hs >> FrontEnd/Instances.hs &&

echo &&
echo &&
echo "---------- Building GLSL Grammar" &&
# Build GLSL abstract tree and pretty printer
bnfc -haskell -p GLSL GLSL.cf &&
#happy -gca GLSL/ParGLSL.y
sed "s/GLSL\./FrontEnd./g" GLSL/AbsGLSL.hs > FrontEnd/AbsGLSL.hs &&
sed "s/GLSL\./FrontEnd./g" GLSL/PrintGLSL.hs > FrontEnd/PrintGLSL.hs &&
rm -r GLSL


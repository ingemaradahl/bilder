#!/bin/sh
bnfc -m -haskell -p FrontEnd Grammar.cf

cat FrontEnd/Instances_head.hs > FrontEnd/Instances.hs
awk '/^newtype/ && $2!="CFloat" { printf "instance Token %s where\n\
  tkpos (%s (p,_)) = p\n\
  tkident (%s (_,s)) = s\n\n", $2, $2, $2 }' FrontEnd/AbsGrammar.hs >> FrontEnd/Instances.hs

# Build GLSL abstract tree and pretty printer
bnfc -haskell -p GLSL GLSL.cf &&
sed "s/module GLSL\.(.*) where/module FrontEnd.$1 where/" GLSL/AbsGLSL.hs > FrontEnd/AbsGLSL.hs &&
sed "s/module GLSL\.(.*) where/module FrontEnd.$1 where/" GLSL/PrintGLSL.hs > FrontEnd/PrintGLSL.hs &&
rm -r GLSL


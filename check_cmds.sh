#!/bin/bash

DEPENDENCIES="ghc bnfc happy alex"

for DEP in $DEPENDENCIES
do
	command -v $DEP > /dev/null 2>&1 || { echo >&2 $DEP "not found!"; exit 1; }
done

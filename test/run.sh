#!/bin/bash

cd "`dirname $0`"

runhaskell -i../src Main.hs $*
runhaskell -i../src Spec.hs $*

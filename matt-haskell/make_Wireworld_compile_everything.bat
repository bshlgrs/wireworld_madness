ghc --make -O2 -W Sources/Wireworld.hs -fforce-recomp -iSources -odir "uname -m" -hidir "uname -m" -o Wireworld -XFlexibleInstances -XTypeSynonymInstances
stack build && stack ghc src/main.hs -- -O2 -threaded -rtsopts && rm src/*.o src/*.hi && mv src/main N-puzzle

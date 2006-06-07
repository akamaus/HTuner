all: Capturer.hs Gui.hs
	ghc -O2 -o gui --make Gui.hs -i/home/maus/src/Haskell/haskelldsp-snapshot
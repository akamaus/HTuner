all: Capturer.hs Gui.hs
	hsc2hs FFTW.hsc
	ghc -O2 -o gui --make Gui.hs -threaded -lfftw3 

tst: FFTW.hsc tst.hs
	hsc2hs FFTW.hsc
	ghc --make tst.hs -cpp -fffi -o tst -lfftw3
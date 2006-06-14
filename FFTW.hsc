{-# OPTIONS -fffi #-}
module FFTW (FFTW_Plan, make_plan, execute)
where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

import Data.Complex


#include <fftw3.h>

data FFTW_PLAN_C = FFTW_PLAN_C

type FFTW_COMPLEX = Complex Double

instance (Storable a, RealFloat a) => Storable (Complex a) where
    sizeOf (r :+ i) = sizeOf r + sizeOf i
    peek ptr = do
      real <- peek $! castPtr ptr
      let ptr' = plusPtr ptr (sizeOf real)
      complex <- peek $! castPtr ptr'
      return $! real :+ complex
    peekElemOff addr idx = peek (addr `plusPtr` (idx * 16)) -- BUG

data FFTW_Plan = FFTW_Plan
    { size :: Int,
      fftw_input :: ForeignPtr CDouble,
      fftw_output :: ForeignPtr FFTW_COMPLEX,
      fftw_plan :: ForeignPtr FFTW_PLAN_C
    }


foreign import ccall fftw_plan_dft_r2c_1d :: Int -> Ptr CDouble -> Ptr FFTW_COMPLEX -> CUInt -> IO (Ptr FFTW_PLAN_C)
foreign import ccall "&fftw_destroy_plan" p_fftw_destroy_plan :: FunPtr (Ptr FFTW_PLAN_C -> IO ())

foreign import ccall fftw_malloc :: Int -> IO (Ptr a)
foreign import ccall "&fftw_free" p_fftw_free :: FunPtr (Ptr a -> IO ())

foreign import ccall fftw_execute :: Ptr FFTW_PLAN_C -> IO ()

--(int n, double *in, fftw_complex *out,
--                               unsigned flags);


make_plan :: Int -> IO (Maybe FFTW_Plan)
make_plan n = do
  inp <- fftw_malloc $ #{size double} * n
  out <- fftw_malloc $ #{size fftw_complex} * n
  plan <- fftw_plan_dft_r2c_1d n inp out #{const FFTW_ESTIMATE}

  fi <- newForeignPtr p_fftw_free inp
  fo <- newForeignPtr p_fftw_free out
  fp <- newForeignPtr p_fftw_destroy_plan plan
  if plan == nullPtr then return Nothing
   else return $! Just $! FFTW_Plan n fi fo fp

execute :: FFTW_Plan -> [Double] -> IO [Complex Double]
execute plan inp = do
  withForeignPtr (fftw_input plan) $ \in_ptr ->
      withForeignPtr (fftw_output plan) $ \out_ptr ->
          withForeignPtr (fftw_plan plan) $ \plan_ptr -> do
                     pokeArray in_ptr $ map (realToFrac) $ take (size plan) inp
                     fftw_execute $ plan_ptr
                     peekArray (size plan `div` 2 + 1) out_ptr





{-# LANGUAGE OverloadedStrings #-}
import Development.Shake
import PkgConfig

main :: IO ()
main = do
  lookupPackage "Qt5Core" >>= print 
  shakeArgs shakeOptions $ do
    return ()
  

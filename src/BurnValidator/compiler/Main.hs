{-# LANGUAGE OverloadedStrings     #-}

import Data.Aeson
import System.Directory (createDirectoryIfMissing)
import Cardano.Api
import Cardano.Api.Shelley
import PlutusTx
import Data.ByteString.Lazy.Char8 (unpack)
import Burn

main :: IO ()
main = do
  let mintingScript = PlutusScriptSerialised burnSBS
      distDir = "../../dist/"
      unitJson = encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData ()))
  createDirectoryIfMissing True distDir
  writePlutusScript (distDir ++ "burn.plutus") mintingScript
  putStrLn "Burn Validator Plutus Script has been generated."
  writeFile (distDir ++ "redeemer.json") $ unpack unitJson
  writeFile (distDir ++ "datum.json") $ unpack unitJson
  print $ "Datum value: " <> unitJson
  print $ "Redeemer value: " <> unitJson

writePlutusScript :: FilePath -> PlutusScript PlutusScriptV1 -> IO ()
writePlutusScript fileName scriptSerial =
  do
  result <- writeFileTextEnvelope fileName Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
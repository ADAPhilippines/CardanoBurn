{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Burn ( burnValidator, burnSBS ) where
import              Plutus.V1.Ledger.Contexts
import              PlutusTx.Trace
import              Cardano.Api             ( PlutusScriptV1 )
import              Cardano.Api.Shelley     (PlutusScript (..))
import              Codec.Serialise
import              Ledger
import qualified    PlutusTx
import qualified    PlutusTx.Prelude        as P
import qualified    Data.ByteString.Lazy    as LBS
import qualified    Data.ByteString.Short   as SBS
import qualified    Ledger.Typed.Scripts    as Scripts

{-# INLINABLE burnValidate #-}
burnValidate :: P.BuiltinData -> P.BuiltinData -> ScriptContext -> P.Bool
burnValidate _ _ _ = traceIfFalse "Burned :)" False

data BurnData
instance Scripts.ValidatorTypes BurnData where
  type instance DatumType BurnData = P.BuiltinData
  type instance RedeemerType BurnData = P.BuiltinData

burnInstance :: Scripts.TypedValidator BurnData
burnInstance = Scripts.mkTypedValidator @BurnData
    $$(PlutusTx.compile [|| burnValidate ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @P.BuiltinData @P.BuiltinData

burnValidator :: Validator
burnValidator = Scripts.validatorScript burnInstance

burnScript :: Ledger.Script
burnScript = unValidatorScript burnValidator

burnSBS :: SBS.ShortByteString
burnSBS = SBS.toShort . LBS.toStrict $ serialise burnScript

burnSerialized :: PlutusScript PlutusScriptV1
burnSerialized = PlutusScriptSerialised burnSBS

burnAddress :: Ledger.Address
burnAddress = scriptAddress burnValidator



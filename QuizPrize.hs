----------------------------------------------------------------------------------------------------------------------------------------------------------------
-- LIBRARY
----------------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)
import           Data.Char

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
----------------------------------------------------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ONCHAIN
----------------------------------------------------------------------------------------------------------------------------------------------------------------
{-# INLINABLE execValidation #-}
execValidation :: BuiltinData -> BuiltinData -> BuiltinData -> ()
execValidation _ r _ 
    | r == Builtins.mkI 777 = ()
    | otherwise             = traceError "WRONG ANSWER!"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| execValidation ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
----------------------------------------------------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------------------------------------------------
-- OFFCHAIN
----------------------------------------------------------------------------------------------------------------------------------------------------------------
type QuizSchema =
            Endpoint "1st Question: When was Queen Elizabeth II born ? [A.1926-04-21] [B.1926-04-01] [C.1936-04-21] [D.1946-04-01]" ()
        .\/ Endpoint "2nd Question: What is Queen Elizabeth's II last name? [A.Maria Winsor] [B.Mary Windsor] [C.Martha Winda] [D.Marlowe Ada]" ()
        .\/ Endpoint "3rd Question: Who is Queen Elizabeth's II spouse? [A.Prince Of Piece] [B.Prince Mateen] [C.Prince Phillip] [D.Prince Joachim]" ()
        .\/ Endpoint "4th Question: When did Queen Elizabeth II die? ? [A.2022-09-06] [B.2022-09-07] [C.2020-03-11] [D.2022-09-08]" ()
        .\/ Endpoint "1st_Answer" String
        .\/ Endpoint "2nd_Answer" String
        .\/ Endpoint "3rd_Answer" String
        .\/ Endpoint "4th_Answer" String

question1 :: AsContractError e => Contract w s e ()
question1 = do
    let prize = 24000000
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkConstr 0 []) $ Ada.lovelaceValueOf prize
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "CONGRATULATION YOU GOT %d LOVELACE!" prize

question2 :: AsContractError e => Contract w s e ()
question2 = do
    let prize = 24000000
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkConstr 0 []) $ Ada.lovelaceValueOf prize
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "CONGRATULATION YOU GOT %d LOVELACE!" prize

question3 :: AsContractError e => Contract w s e ()
question3 = do
    let prize = 24000000
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkConstr 0 []) $ Ada.lovelaceValueOf prize
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "CONGRATULATION YOU GOT %d LOVELACE!" prize

question4 :: AsContractError e => Contract w s e ()
question4 = do
    let prize = 24000000
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkConstr 0 []) $ Ada.lovelaceValueOf prize
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "CONGRATULATION YOU GOT %d LOVELACE!" prize

answer1 :: forall w s e. AsContractError e => String -> Contract w s e ()
answer1 x = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI (answerKey1 $ fmap toUpper x) | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "ANSWER LOCKED!"

answer2 :: forall w s e. AsContractError e => String -> Contract w s e ()
answer2 x = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI (answerKey2 $ fmap toUpper x) | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "ANSWER LOCKED!"

answer3 :: forall w s e. AsContractError e => String -> Contract w s e ()
answer3 x = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI (answerKey3 $ fmap toUpper x) | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "ANSWER LOCKED!"

answer4 :: forall w s e. AsContractError e => String -> Contract w s e ()
answer4 x = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI (answerKey4 $ fmap toUpper x) | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "ANSWER LOCKED!"

-- ANSWER KEY -----------------
answerKey1 :: String -> Integer
answerKey1 "A" = 777
answerKey1 _   = 666  

answerKey2 :: String -> Integer
answerKey2 "B" = 777
answerKey2 _   = 666 

answerKey3 :: String -> Integer
answerKey3 "C" = 777
answerKey3 _   = 666 

answerKey4 :: String -> Integer
answerKey4 "D" = 777
answerKey4 _   = 666 
-------------------------------

endpoints :: Contract () QuizSchema Text ()
endpoints = awaitPromise (question1' `select` question2' `select` question3' `select` question4' `select` answer1' `select` answer2' `select` answer3' `select` answer4') >> endpoints
  where
    question1' = endpoint @"1st Question: When was Queen Elizabeth II born ? [A.1926-04-21] [B.1926-04-01] [C.1936-04-21] [D.1946-04-01]" $ const question1
    question2' = endpoint @"2nd Question: What is Queen Elizabeth's II last name? [A.Maria Winsor] [B.Mary Windsor] [C.Martha Winda] [D.Marlowe Ada]" $ const question2
    question3' = endpoint @"3rd Question: Who is Queen Elizabeth's II spouse? [A.Prince Of Piece] [B.Prince Mateen] [C.Prince Phillip] [D.Prince Joachim]" $ const question3
    question4' = endpoint @"4th Question: When did Queen Elizabeth II die? ? [A.2022-09-06] [B.2022-09-07] [C.2020-03-11] [D.2022-09-08]" $ const question4
    answer1'   = endpoint @"1st_Answer" answer1
    answer2'   = endpoint @"2nd_Answer" answer2
    answer3'   = endpoint @"3rd_Answer" answer3
    answer4'   = endpoint @"4th_Answer" answer4

mkSchemaDefinitions ''QuizSchema

mkKnownCurrencies []
----------------------------------------------------------------------------------------------------------------------------------------------------------------
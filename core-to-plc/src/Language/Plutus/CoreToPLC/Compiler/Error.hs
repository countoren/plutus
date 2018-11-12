{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Plutus.CoreToPLC.Compiler.Error (
    ConvError
    , Error (..)
    , WithContext (..)
    , withContext
    , withContextM
    , throwPlain) where

import qualified Language.PlutusIR.Compiler as PIR

import qualified Language.PlutusCore        as PLC
import qualified Language.PlutusCore.Pretty as PLC

import           Control.Monad.Except
import qualified Data.Text                  as T
import qualified Data.Text.Prettyprint.Doc  as PP
import           Data.Typeable

type ConvError = WithContext T.Text (Error ())

-- | An error with some (nested) context.
data WithContext c e = NoContext e | WithContext c (WithContext c e)
    deriving Functor

withContext :: (MonadError (WithContext c e) m) => c -> m a -> m a
withContext c act = catchError act $ \err -> throwError (WithContext c err)

withContextM :: (MonadError (WithContext c e) m) => m c -> m a -> m a
withContextM mc act = do
    c <- mc
    catchError act $ \err -> throwError (WithContext c err)

throwPlain :: MonadError (WithContext c e) m => e -> m a
throwPlain = throwError . NoContext

instance (PP.Pretty c, PP.Pretty e) => PP.Pretty (WithContext c e) where
    pretty = \case
        NoContext e     -> "Error:" PP.<+> (PP.align $ PP.pretty e)
        WithContext c e -> PP.vsep [
            PP.pretty e,
            "Context:" PP.<+> (PP.align $ PP.pretty c)
            ]

data Error a = PLCError (PLC.Error a)
             | PIRError (PIR.Error (PIR.Provenance a))
             | ConversionError T.Text
             | UnsupportedError T.Text
             | FreeVariableError T.Text
             | ValueRestrictionError T.Text
             deriving Typeable

instance (PP.Pretty a) => PP.Pretty (Error a) where
    pretty = PLC.prettyPlcClassicDebug

instance (PP.Pretty a) => PLC.PrettyBy PLC.PrettyConfigPlc (Error a) where
    prettyBy config = \case
        PLCError e -> PLC.prettyBy config e
        PIRError e -> PLC.prettyBy config e
        ConversionError e -> "Error during conversion:" PP.<+> PP.pretty e
        UnsupportedError e -> "Unsupported:" PP.<+> PP.pretty e
        FreeVariableError e -> "Used but not defined in the current conversion:" PP.<+> PP.pretty e
        ValueRestrictionError e -> "Violation of the value restriction:" PP.<+> PP.pretty e
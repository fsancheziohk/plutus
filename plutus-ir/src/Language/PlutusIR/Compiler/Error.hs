{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Language.PlutusIR.Compiler.Error
    ( Error (..)
    , TypeError (..)
    , AsTypeError (..)
    , AsError (..)
    , PLC.Normalized (..)
    , PLC.UnknownDynamicBuiltinNameError (..)
    , throwingEither
    ) where

import qualified Language.PlutusCore        as PLC
import qualified Language.PlutusCore.Pretty as PLC
--import qualified Language.PlutusCore.Error as PLC (throwingEither)
import qualified Language.PlutusIR as PIR

import           Control.Exception hiding (TypeError)
import           Control.Lens

import qualified Data.Text                  as T
import           Data.Text.Prettyprint.Doc  ((<+>))
import qualified Data.Text.Prettyprint.Doc  as PP
import           Data.Typeable
import GHC.Generics (Generic)
import Control.Monad.Error.Lens
import Control.Monad.Except


-- | Lifts an 'Either' into an error context where we can embed the 'Left' value into the error.
-- TODO: literally copy-pasted from PlutusCore.Error to avoid modifying the exposed modules/functions of language-plutus-core package
throwingEither :: MonadError e m => AReview e t -> Either t a -> m a
throwingEither r e = case e of
    Left t  -> throwing r t
    Right v -> pure v


data TypeError uni ann
    = KindMismatch ann (PLC.Type PLC.TyName uni ()) (PLC.Kind ())  (PLC.Kind ())
    | TypeMismatch ann
        (PIR.Term PLC.TyName PLC.Name uni ())
        (PLC.Type PLC.TyName uni ())
        (PLC.Normalized (PLC.Type PLC.TyName uni ()))
    | UnknownDynamicBuiltinName ann PLC.UnknownDynamicBuiltinNameError
    | InternalTypeErrorE ann (PLC.InternalTypeError uni ann)
    | FreeTypeVariableE ann PLC.TyName
    | FreeVariableE ann PLC.Name
    -- FIXME: attach info
    | ConflictingRecBinds
    | DuplicateDeclaredIdent
    | MalformedDataConstrType
    -- TODO: enable back deriving NFData because PIR.Term does not have it
    -- TODO: enable back deriving Eq because PIR.Term does not have it, see similar src/Language/PlutusCore/Core/Instance/Eq.hs
    deriving (Show, Generic) 
makeClassyPrisms ''TypeError


data Error uni a = CompilationError a T.Text -- ^ A generic compilation error.
                 | UnsupportedError a T.Text -- ^ An error relating specifically to an unsupported feature.
                 | PLCError (PLC.Error uni a) -- ^ An error from running some PLC function, lifted into this error type for convenience.
                 | TypeErrorE (TypeError uni a)
               deriving (Typeable)
makeClassyPrisms ''Error



instance PLC.AsTypeError (Error uni a) uni a where
    _TypeError = _PLCError . PLC._TypeError

instance AsTypeError (Error uni a) uni a where
    _TypeError = _TypeErrorE

instance (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty a) =>
            Show (Error uni a) where
    show e = show $ PLC.prettyPlcClassicDebug e

instance (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty a) =>
            PLC.PrettyBy PLC.PrettyConfigPlc (Error uni a) where
    prettyBy config = \case
        CompilationError x e -> "Error during compilation:" <+> PP.pretty e <> "(" <> PP.pretty x <> ")"
        UnsupportedError x e -> "Unsupported construct:" <+> PP.pretty e <+> "(" <> PP.pretty x <> ")"
        PLCError e -> PP.vsep [ "Error from the PLC compiler:", PLC.prettyBy config e ]
        -- TODO: add instance Pretty PIR.TypeError and hook it up here
        TypeErrorE e -> PP.vsep [ "Error from the PIR typechecker:"]
    
instance ( PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty a
         , Typeable uni, Typeable a
         ) => Exception (Error uni a)

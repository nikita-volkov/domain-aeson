{-|
TH utils for aeson.
-}
module DomainAeson.Util.AesonTH
where

import DomainAeson.Prelude
import Language.Haskell.TH.Syntax
import THLego.Helpers
import DomainAeson.Util.GeneralTH
import qualified THLego.Lambdas as Lambdas
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text


productParseJsonD :: Name -> [(Text, Bool)] -> Dec
productParseJsonD conName fields =
  FunD 'Aeson.parseJSON [clause]
  where
    clause =
      Clause [] (NormalB exp) []
      where
        exp =
          multiAppE (VarE 'Aeson.withObject) [
            LitE (StringL (nameString conName))
            ,
            productObjectParsingLamE conName fields
            ]

productObjectParsingLamE :: Name -> [(Text, Bool)] -> Exp
productObjectParsingLamE conName fields =
  LamE [VarP aName] (productObjectParserE (ConE aName) conName fields)

productObjectParserE :: Exp -> Name -> [(Text, Bool)] -> Exp
productObjectParserE objectE conName fields =
  applicativeChainE (ConE conName) (fmap fieldE fields)
  where
    fieldE (label, required) =
      InfixE (Just objectE) (VarE opName) (Just (textLitE label))
      where
        opName =
          if required
            then '(Aeson..:)
            else '(Aeson..:?)

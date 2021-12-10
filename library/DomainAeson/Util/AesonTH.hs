-- |
-- TH utils for aeson.
module DomainAeson.Util.AesonTH where

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as AeKey
import qualified Data.Aeson.KeyMap as AeKeyMap
import qualified Data.Text as Text
import DomainAeson.Prelude
import Language.Haskell.TH.Syntax
import THLego.Helpers
import qualified THLego.Lambdas as Lambdas
import qualified TemplateHaskell.Compat.V0208 as Compat

productParseJsonD :: Name -> [(Text, Bool)] -> Dec
productParseJsonD conName fields =
  FunD 'Ae.parseJSON [clause]
  where
    clause =
      Clause [] (NormalB exp) []
      where
        exp =
          multiAppE
            (VarE 'Ae.withObject)
            [ LitE (StringL (nameString conName)),
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
            then '(Ae..:)
            else '(Ae..:?)

-- *

productToJsonFunD :: Name -> [Text] -> Dec
productToJsonFunD conName members =
  FunD 'Ae.toJSON [clause]
  where
    clause =
      Clause [Compat.conp conName memberPats] body []
      where
        memberPats = fmap memberPat members
          where
            memberPat member =
              VarP (textName member)
        body = NormalB $ AppE (ConE 'Ae.Object) mapE
          where
            mapE =
              AppE (VarE 'AeKeyMap.fromList) (ListE (fmap memberPairE members))
            memberPairE member =
              appliedTupleE [memberKeyE member, memberJsonE member]
            memberKeyE member =
              AppE (VarE 'AeKey.fromString) (textLitE member)
            memberJsonE member =
              AppE (VarE 'Ae.toJSON) (VarE (textName member))

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

toJsonInstanceDec :: Type -> Dec -> Dec
toJsonInstanceDec type_ toJsonFunDec =
  InstanceD Nothing [] headType [toJsonFunDec]
  where
    headType =
      AppT (ConT ''Ae.ToJSON) type_

productToJsonInstanceDec :: Type -> Name -> [Text] -> Dec
productToJsonInstanceDec type_ conName members =
  toJsonInstanceDec type_ $ productToJsonFunD conName members

sumToJsonInstanceDec :: Type -> [(Text, Name)] -> Dec
sumToJsonInstanceDec type_ members =
  toJsonInstanceDec type_ $ sumToJsonFunD members

enumToJsonInstanceDec :: Type -> [(Text, Name)] -> Dec
enumToJsonInstanceDec type_ members =
  toJsonInstanceDec type_ $ enumToJsonFunD members

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
              appliedTupleE [textKeyE member, memberJsonE member]
            memberJsonE member =
              toJsonE (VarE (textName member))

sumToJsonFunD :: [(Text, Name)] -> Dec
sumToJsonFunD members =
  FunD 'Ae.toJSON clauses
  where
    clauses = fmap memberClause members
      where
        memberClause (jsonName, conName) =
          Clause [Compat.conp conName [VarP varName]] (NormalB bodyExp) []
          where
            varName = mkName "a"
            bodyExp =
              AppE
                (ConE 'Ae.Object)
                ( multiAppE
                    (VarE 'AeKeyMap.singleton)
                    [ textKeyE jsonName,
                      toJsonE (VarE varName)
                    ]
                )

enumToJsonFunD :: [(Text, Name)] -> Dec
enumToJsonFunD members =
  FunD 'Ae.toJSON clauses
  where
    clauses = fmap memberClause members
      where
        memberClause (jsonName, conName) =
          Clause [Compat.conp conName []] (NormalB bodyExp) []
          where
            bodyExp = stringJsonE jsonName

-- *

stringJsonE :: Text -> Exp
stringJsonE =
  AppE (ConE 'Ae.String) . textLitE

textKeyE :: Text -> Exp
textKeyE text =
  AppE (VarE 'AeKey.fromString) (textLitE text)

toJsonE :: Exp -> Exp
toJsonE =
  AppE (VarE 'Ae.toJSON)

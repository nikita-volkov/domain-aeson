-- |
-- TH utils for aeson.
module DomainAeson.Util.AesonTH where

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as AeKey
import qualified Data.Aeson.KeyMap as AeKeyMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
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

toJsonInstanceDec :: Type -> Dec -> Dec
toJsonInstanceDec type_ toJsonFunDec =
  InstanceD Nothing [] headType [toJsonFunDec]
  where
    headType =
      AppT (ConT ''Ae.ToJSON) type_

productToJsonInstanceDec :: Type -> Name -> [Text] -> Dec
productToJsonInstanceDec type_ conName members =
  toJsonInstanceDec type_ $ productToJsonFunD conName members

sumToJsonInstanceDec :: Type -> [(Text, Name, Int)] -> Dec
sumToJsonInstanceDec type_ members =
  toJsonInstanceDec type_ $ sumToJsonFunD members

enumToJsonInstanceDec :: Type -> [(Text, Name)] -> Dec
enumToJsonInstanceDec type_ members =
  toJsonInstanceDec type_ $ enumToJsonFunD members

productToJsonFunD :: Name -> [Text] -> Dec
productToJsonFunD conName members =
  FunD 'Ae.toJSON [clause]
  where
    varNamesAndJsonNames =
      mapWithAlphabeticName (,) members
    clause =
      Clause [Compat.conP conName memberPats] body []
      where
        memberPats = fmap memberPat varNamesAndJsonNames
          where
            memberPat (varName, _) = VarP varName
        body = NormalB $ AppE (ConE 'Ae.Object) mapE
          where
            mapE =
              AppE (VarE 'AeKeyMap.fromList) (ListE (fmap memberPairE varNamesAndJsonNames))
            memberPairE (varName, jsonName) =
              appliedTupleE [textKeyE jsonName, toJsonE (VarE varName)]

sumToJsonFunD :: [(Text, Name, Int)] -> Dec
sumToJsonFunD members =
  FunD 'Ae.toJSON clauses
  where
    clauses = fmap memberClause members
      where
        memberClause (jsonName, conName, components) =
          case components of
            0 ->
              Clause [Compat.conP conName []] (NormalB bodyExp) []
              where
                bodyExp = stringJsonE jsonName
            1 ->
              Clause [Compat.conP conName [VarP varName]] (NormalB bodyExp) []
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
            _ ->
              Clause [Compat.conP conName (fmap VarP varNames)] (NormalB bodyExp) []
              where
                varNames = enumAlphabeticNames components
                bodyExp =
                  AppE
                    (ConE 'Ae.Object)
                    ( multiAppE
                        (VarE 'AeKeyMap.singleton)
                        [ textKeyE jsonName,
                          jsonArrayE (fmap (toJsonE . VarE) varNames)
                        ]
                    )

enumToJsonFunD :: [(Text, Name)] -> Dec
enumToJsonFunD members =
  FunD 'Ae.toJSON clauses
  where
    clauses = fmap memberClause members
      where
        memberClause (jsonName, conName) =
          Clause [Compat.conP conName []] (NormalB bodyExp) []
          where
            bodyExp = stringJsonE jsonName

jsonArrayE :: [Exp] -> Exp
jsonArrayE exps =
  AppE (ConE 'Ae.Array) (AppE (VarE 'Vector.fromList) (ListE exps))

stringJsonE :: Text -> Exp
stringJsonE =
  AppE (ConE 'Ae.String) . AppE (VarE 'fromString) . textLitE

textKeyE :: Text -> Exp
textKeyE text =
  AppE (VarE 'AeKey.fromString) (textLitE text)

toJsonE :: Exp -> Exp
toJsonE =
  AppE (VarE 'Ae.toJSON)

module DomainAeson.InstanceDecs where

import DomainAeson.Prelude
import qualified DomainAeson.Util.AesonTH as AesonTH
import qualified DomainCore.Model as Model
import qualified DomainCore.TH as DomainTH
import Language.Haskell.TH.Syntax
import THLego.Helpers

toJson :: Model.TypeDec -> [Dec]
toJson (Model.TypeDec typeName typeDef) =
  pure $ case typeDef of
    Model.ProductTypeDef members ->
      AesonTH.productToJsonInstanceDec
        (ConT (textName typeName))
        (textName typeName)
        (fmap fst members)
    Model.SumTypeDef members ->
      AesonTH.sumToJsonInstanceDec
        (ConT (textName typeName))
        (fmap member members)
      where
        member (memberName, memberComponentTypes) =
          ( memberName,
            DomainTH.sumConstructorName typeName memberName,
            length memberComponentTypes
          )

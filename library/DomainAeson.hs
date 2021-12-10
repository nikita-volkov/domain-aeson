module DomainAeson where

import qualified DomainAeson.InstanceDecs as InstanceDecs
import DomainAeson.Prelude
import qualified DomainCore.Deriver as Deriver

toJsonDeriver :: Deriver.Deriver
toJsonDeriver =
  Deriver.effectless InstanceDecs.toJson

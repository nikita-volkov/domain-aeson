module Main where

import Domain
import DomainAeson
import Test.Hspec
import qualified Test.QuickCheck.Classes as Laws
import Prelude

declare
  Nothing
  (toJsonDeriver <> fromJsonDeriver)
  [schema|

    ServiceAddress:
      sum:
        network: NetworkAddress
        local: FilePath

    NetworkAddress:
      product:
        protocol: TransportProtocol
        host: Host
        port: Word16

    TransportProtocol:
      enum:
        - tcp
        - udp

    Host:
      sum:
        ip: Ip
        name: Text

    Ip:
      sum:
        v4: Word32
        v6: Word128

    Word128:
      product:
        part1: Word64
        part2: Word64

    |]

main :: IO ()
main =
  hspec $ do
    lawsSpec $ Laws.jsonLaws @Word128

lawsSpec :: _
lawsSpec (Laws.Laws name properties) =
  describe name $ for properties $ \(lawName, property) ->
    prop lawName property

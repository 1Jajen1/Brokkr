module Network.Handler.Play (
  finalizeLogin
, playProtocol
) where
import Effectful
import Data.Text
import Data.UUID
import Network.Effect.Network

finalizeLogin :: Network :> es => UUID -> Text -> Eff es ()
finalizeLogin uid uname = pure ()

playProtocol :: Network :> es => Eff es ()
playProtocol = pure ()

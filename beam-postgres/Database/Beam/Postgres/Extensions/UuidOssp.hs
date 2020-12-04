-- | The @uuid-ossp@ extension provides functions for constructing UUIDs.
--
-- For an example of usage, see the documentation for 'PgExtensionEntity'.
module Database.Beam.Postgres.Extensions.UuidOssp
  ( UuidOssp(..)
  ) where

import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.UUID.Types (UUID)

import           Database.Beam
import           Database.Beam.Postgres.Extensions
import           Database.Beam.Postgres.Extensions.Internal

-- | Data type representing definitions contained in the @uuid-ossp@ extension
data UuidOssp = UuidOssp
  { pgUuidNil ::
      forall ctxt s. LiftPg ctxt s UUID
  , pgUuidNsDns ::
      forall ctxt s. LiftPg ctxt s UUID
  , pgUuidNsUrl ::
      forall ctxt s. LiftPg ctxt s UUID
  , pgUuidNsOid ::
      forall ctxt s. LiftPg ctxt s UUID
  , pgUuidNsX500 ::
      forall ctxt s. LiftPg ctxt s UUID
  , pgUuidGenerateV1 ::
      forall ctxt s. LiftPg ctxt s UUID
  , pgUuidGenerateV1Mc ::
      forall ctxt s. LiftPg ctxt s UUID
  , pgUuidGenerateV3 ::
      forall ctxt s. LiftPg ctxt s (UUID -> Text -> UUID)
  , pgUuidGenerateV4 ::
      forall ctxt s. LiftPg ctxt s UUID
  , pgUuidGenerateV5 ::
      forall ctxt s. LiftPg ctxt s (UUID -> Text -> UUID)
  }

instance IsPgExtension UuidOssp where
  pgExtensionName Proxy = "uuid-ossp"
  pgExtensionBuild = UuidOssp
    { pgUuidNil =
        QExpr $ funcE "uuid_nil" <$> sequenceA []
    , pgUuidNsDns =
        QExpr $ funcE "uuid_ns_dns" <$> sequenceA []
    , pgUuidNsUrl =
        QExpr $ funcE "uuid_ns_url" <$> sequenceA []
    , pgUuidNsOid =
        QExpr $ funcE "uuid_ns_oid" <$> sequenceA []
    , pgUuidNsX500 =
        QExpr $ funcE "uuid_ns_x500" <$> sequenceA []
    , pgUuidGenerateV1 =
        QExpr $ funcE "uuid_generate_v1" <$> sequenceA []
    , pgUuidGenerateV1Mc =
        QExpr $ funcE "uuid_generate_v1mc" <$> sequenceA []
    , pgUuidGenerateV3 = \(QExpr ns) (QExpr t) ->
        QExpr $ funcE "uuid_generate_v3" <$> sequenceA [ns, t]
    , pgUuidGenerateV4 =
        QExpr $ funcE "uuid_generate_v4" <$> sequenceA []
    , pgUuidGenerateV5 = \(QExpr ns) (QExpr t) ->
        QExpr $ funcE "uuid_generate_v5" <$> sequenceA [ns, t]
    }

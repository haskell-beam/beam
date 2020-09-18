
-- | The @pgcrypto@ extension provides several cryptographic functions to
-- Postgres. This module provides a @beam-postgres@ extension to access this
-- functionality. For an example of usage, see the documentation for
-- 'PgExtensionEntity'.
module Database.Beam.Postgres.PgCrypto
  ( PgCrypto(..) ) where

import Database.Beam
import Database.Beam.Backend.SQL

import Database.Beam.Postgres.Extensions
import Database.Beam.Postgres.Extensions.Internal

import Data.Int
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.UUID.Types (UUID)

-- | Data type representing definitions contained in the @pgcrypto@ extension
--
-- Each field maps closely to the underlying @pgcrypto@ function, which are
-- described in further detail in the
-- <https://www.postgresql.org/docs/current/static/pgcrypto.html pgcrypto manual>.
data PgCrypto
  = PgCrypto
  { pgCryptoDigestText ::
      forall ctxt s. LiftPg ctxt s (Text -> Text -> ByteString)
  , pgCryptoDigestBytes ::
      forall ctxt s. LiftPg ctxt s (ByteString -> Text -> ByteString)
  , pgCryptoHmacText ::
      forall ctxt s. LiftPg ctxt s (Text -> Text -> Text -> ByteString)
  , pgCryptoHmacBytes ::
      forall ctxt s. LiftPg ctxt s (ByteString -> Text -> Text -> ByteString)
  , pgCryptoCrypt ::
      forall ctxt s. LiftPg ctxt s (Text -> Text -> Text)
  , pgCryptoGenSalt ::
      forall ctxt s. LiftPg ctxt s (Text -> Maybe Int32 -> Text)

  -- Pgp functions
  , pgCryptoPgpSymEncrypt ::
      forall ctxt s. LiftPg ctxt s (Text -> Text -> Maybe Text -> ByteString)
  , pgCryptoPgpSymEncryptBytea ::
      forall ctxt s. LiftPg ctxt s (ByteString -> Text -> Maybe Text -> ByteString)

  , pgCryptoPgpSymDecrypt ::
      forall ctxt s. LiftPg ctxt s (ByteString -> Text -> Maybe Text -> Text)
  , pgCryptoPgpSymDecryptBytea ::
      forall ctxt s. LiftPg ctxt s (ByteString -> Text -> Maybe Text -> ByteString)

  , pgCryptoPgpPubEncrypt ::
      forall ctxt s. LiftPg ctxt s (Text -> ByteString -> Maybe Text -> ByteString)
  , pgCryptoPgpPubEncryptBytea ::
      forall ctxt s. LiftPg ctxt s (ByteString -> ByteString -> Maybe Text -> ByteString)

  , pgCryptoPgpPubDecrypt ::
      forall ctxt s. LiftPg ctxt s (ByteString -> ByteString -> Maybe Text -> Maybe Text -> Text)
  , pgCryptoPgpPubDecryptBytea ::
      forall ctxt s. LiftPg ctxt s (ByteString -> ByteString -> Maybe Text -> Maybe Text -> ByteString)

  , pgCryptoPgpKeyId ::
      forall ctxt s. LiftPg ctxt s (ByteString -> Text)

  , pgCryptoArmor ::
      forall ctxt s. PgExpr ctxt s ByteString ->
                     Maybe (PgExpr ctxt s (Vector Text), PgExpr ctxt s (Vector Text)) ->
                     PgExpr ctxt s Text
  , pgCryptoDearmor ::
      forall ctxt s. LiftPg ctxt s (Text -> ByteString)

-- TODO setof
--  , pgCryptoPgpArmorHeaders ::
--      forall ctxt s. LiftPg ctxt s (Text -> )

  , pgCryptoGenRandomBytes ::
      forall ctxt s i. Integral i => PgExpr ctxt s i -> PgExpr ctxt s ByteString
  , pgCryptoGenRandomUUID ::
      forall ctxt s. PgExpr ctxt s UUID
  }

instance IsPgExtension PgCrypto where
  pgExtensionName _ = "pgcrypto"
  pgExtensionBuild = PgCrypto {
    pgCryptoDigestText  =
        \(QExpr data_) (QExpr type_) -> QExpr (funcE "digest" <$> sequenceA [data_, type_]),
    pgCryptoDigestBytes =
        \(QExpr data_) (QExpr type_) -> QExpr (funcE "digest" <$> sequenceA [data_, type_]),
    pgCryptoHmacText =
        \(QExpr data_) (QExpr key) (QExpr type_) -> QExpr (funcE "hmac" <$> sequenceA [data_, key, type_]),
    pgCryptoHmacBytes =
        \(QExpr data_) (QExpr key) (QExpr type_) -> QExpr (funcE "hmac" <$> sequenceA [data_, key, type_]),

    pgCryptoCrypt =
        \(QExpr pw) (QExpr salt) ->
           QExpr (funcE "crypt" <$> sequenceA [pw, salt]),
    pgCryptoGenSalt =
        \(QExpr text) iterCount ->
           QExpr (funcE "gen_salt" <$> sequenceA ([text] ++ maybe [] (\(QExpr iterCount') -> [iterCount']) iterCount)),

    pgCryptoPgpSymEncrypt =
        \(QExpr data_) (QExpr pw) options ->
           QExpr (funcE "pgp_sym_encrypt" <$> sequenceA ([data_, pw] ++ maybe [] (\(QExpr options') -> [options']) options)),
    pgCryptoPgpSymEncryptBytea =
        \(QExpr data_) (QExpr pw) options ->
           QExpr (funcE "pgp_sym_encrypt_bytea" <$> sequenceA ([data_, pw] ++ maybe [] (\(QExpr options') -> [options']) options)),

    pgCryptoPgpSymDecrypt =
        \(QExpr data_) (QExpr pw) options ->
             QExpr
             (funcE "pgp_sym_decrypt" <$> sequenceA ([data_, pw] ++ maybe [] (\(QExpr options') -> [options']) options)),
    pgCryptoPgpSymDecryptBytea =
        \(QExpr data_) (QExpr pw) options ->
             QExpr
             (funcE "pgp_sym_decrypt_bytea" <$> sequenceA ([data_, pw] ++ maybe [] (\(QExpr options') -> [options']) options)),

    pgCryptoPgpPubEncrypt =
        \(QExpr data_) (QExpr key) options ->
             QExpr
             (funcE "pgp_pub_encrypt" <$> sequenceA ([data_, key] ++ maybe [] (\(QExpr options') -> [options']) options)),
    pgCryptoPgpPubEncryptBytea =
        \(QExpr data_) (QExpr key) options ->
             QExpr
             (funcE "pgp_pub_encrypt_bytea" <$> sequenceA ([data_, key] ++ maybe [] (\(QExpr options') -> [options']) options)),

    pgCryptoPgpPubDecrypt =
        \(QExpr msg) (QExpr key) pw options ->
              QExpr
              (funcE "pgp_pub_decrypt" <$>
                   sequenceA
                   ( [msg, key] ++
                     case (pw, options) of
                       (Nothing, Nothing) -> []
                       (Just (QExpr pw'), Nothing) -> [pw']
                       (Nothing, Just (QExpr options')) -> [ \_ -> valueE (sqlValueSyntax ("" :: String))
                                                           , options' ]
                       (Just (QExpr pw'), Just (QExpr options')) -> [pw', options'] )),
    pgCryptoPgpPubDecryptBytea =
        \(QExpr msg) (QExpr key) pw options ->
              QExpr $
              (funcE "pgp_pub_decrypt_bytea" <$>
                   sequenceA
                   ( [msg, key] ++
                     case (pw, options) of
                       (Nothing, Nothing) -> []
                       (Just (QExpr pw'), Nothing) -> [pw']
                       (Nothing, Just (QExpr options')) -> [ \_ -> valueE (sqlValueSyntax ("" :: String))
                                                           , options' ]
                       (Just (QExpr pw'), Just (QExpr options')) -> [pw', options'] )),

    pgCryptoPgpKeyId =
        \(QExpr data_) -> QExpr (funcE "pgp_key_id" <$> sequenceA [data_]),

    pgCryptoArmor =
        \(QExpr data_) keysData ->
            QExpr (funcE "armor" <$> sequenceA
                     ([data_] ++
                      case keysData of
                        Nothing -> []
                        Just (QExpr keys, QExpr values) ->
                          [keys, values])),
    pgCryptoDearmor =
        \(QExpr data_) -> QExpr (funcE "dearmor" <$> sequenceA [data_]),

    pgCryptoGenRandomBytes =
        \(QExpr count) ->
            QExpr (funcE "gen_random_bytes" <$> sequenceA [count]),
    pgCryptoGenRandomUUID =
         QExpr (\_ -> funcE "gen_random_uuid" [])
    }

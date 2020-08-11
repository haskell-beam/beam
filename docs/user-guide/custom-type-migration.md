**Note**: The code used in this guide is in `beam-postgres/examples/Pagila/Schema/CustomMigrateExample.hs`.

### Using Custom Types in migration ###
In Beam [Tutorial 3](https://haskell-beam.github.io/beam/tutorials/tutorial3/) we looked at marshalling custom types.
Beam provides functionality to represent custom defined types in migration as well.

From the tutorials, let us take our custom type `ShippingCarrier`.
```haskell
data ShippingCarrier = USPS | FedEx | UPS | DHL
  deriving (Show, Read, Eq, Ord, Enum)
```

From this [example](https://github.com/haskell-beam/beam/blob/master/beam-postgres/examples/Pagila/Schema/V0001.hs),
let us take the `AddressT` table and add a column `addressShipper` to it.
```haskell
-- | Address table
data AddressT f
  = AddressT
  { addressId         :: Columnar f (SqlSerial Int32)
  , addressAddress1   :: Columnar f T.Text
  , addressAddress2   :: Columnar f (Maybe T.Text)
  , addressDistrict   :: Columnar f T.Text
  , addressShipper    :: Columnar f ShippingCarrier
  , addressPostalCode :: Columnar f T.Text
  , addressPhone      :: Columnar f T.Text
  , addressLastUpdate :: Columnar f LocalTime
  } deriving Generic
type Address = AddressT Identity
deriving instance Show Address
deriving instance Eq Address

instance Table AddressT where
  data PrimaryKey AddressT f = AddressId (Columnar f (SqlSerial Int32)) deriving Generic
  primaryKey = AddressId . addressId
type AddressId = PrimaryKey AddressT Identity
deriving instance Show AddressId
deriving instance Eq AddressId

instance Beamable (PrimaryKey AddressT)
instance Beamable AddressT
```

In order to use `ShippingCarrier` in migration, we need to define a value of `DataType`,
which is defined in `Database.Beam.Migrate.SQL.Types` and re-exported by the exposed module `Database.Beam.Migrate`.

If we want our `ShippingCarrier` type to take up the postgres `TEXT` type,
```haskell
import Database.Beam.Postgres.Syntax (PgDataTypeSyntax, pgTextType)
import Database.Beam.Migrate (DataType(..))

shippingCarrierType :: DataType PgDataTypeSyntax ShippingCarrier
shippingCarrierType = DataType pgTextType
```

Afterwards, we also need to instantiate `FromBackendRow` and `SqlValueSyntax` for `ShippingCarrier`
for whichever backend and syntax we are using (postgres, in our case).

```haskell
import qualified Data.Text as T

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ShippingCarrier where
  sqlValueSyntax = autoSqlValueSyntax

-- | An explicit definition of ``fromBackendRow`` is required for each custom type
instance (BeamBackend be, FromBackendRow be T.Text) => FromBackendRow be ShippingCarrier where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: T.Text of
      "usps" -> pure USPS
      "fedex" -> pure FedEx
      "ups"  -> pure UPS
      "dhl"  -> pure DHL
      _ -> fail ("Invalid value for ShippingCarrier: " ++ T.unpack val)
```

For data type defaulting support, instantiate `HasDefaultSqlDataType` and `HasDefaultSqlDataTypeConstraints`.
```haskell
instance (IsSql92ColumnSchemaSyntax be) => HasDefaultSqlDataTypeConstraints be ShippingCarrier
```

Finally, if we defined our `Database` like this:
```haskell
-- | Pagila db
data PagilaDb f
  = PagilaDb
  {
    address    :: f (TableEntity AddressT)
  } deriving Generic
instance Database PagilaDb
```

We can write our migration function like this:
```haskell
lastUpdateField :: TableFieldSchema PgColumnSchemaSyntax LocalTime
lastUpdateField = field "last_update" timestamp (defaultTo_ now_) notNull

migration :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres PagilaDb)
migration () = do
--  year_ <- createDomain "year" integer (check (\yr -> yr >=. 1901 &&. yr <=. 2155))
  PagilaDb <$> createTable "address"
                 (AddressT
                    (field "address_id" smallserial)
                    (field "address" (varchar (Just 50)) notNull)
                    (field "address2" (maybeType $ varchar (Just 50)))
                    (field "district" (varchar (Just 20)) notNull)
                    (field "shipper" shippingCarrierType)
                    (field "postal_code" (varchar (Just 10)))
                    (field "phone" (varchar (Just 20)) notNull) lastUpdateField)

```

The export list of the module `Database.Beam.Postgres.Syntax` should serve as a good guide
for which existing types can be used to represent our custom data types in migration.

A beam model is any single-constructor Haskell record type parameterized by a
type of kind `* -> *`. The model must have an instance of `Generic`, `Beamable`,
and `Table`. `Generic` can be derived using the `DeriveGeneric` extension of
GHC. `Beamable` must be given an empty instance declaration (`instance Beamable
Tbl` for a table of type `Tbl`). `Table` is discussed next.

Each field in the record type must either be a sub-table (another parameterized
type with a `Beamable` instance) or an explicit column. A column is specified
using the `Columnar` type family applied to the type's parameter and the
underlying Haskell type of the field.

## The `Table` type class

`Table` is a type class that must be instantiated for all types that you would
like to use as a table. It has one associated `data` instance and one function.

You must create a type to represent the primary key of the table. The primary
key of a table `Tbl` is the associated data type `PrimaryKey Tbl`. Like `Tbl`,
it takes one type parameter of kind `* -> *`. It must have only one constructor
which can hold all fields in the primary key. The constructor need not be a
record constructor (although it can be).

You must also write a function `primaryKey` that takes an instance of `Tbl`
(parameterized over any functor `f`) and returns the associated `PrimaryKey`
type. It is sometimes easiest to use the `Applicative` instance for `r ->` to
write this function. For example, if `tblField1` and `tblField2` are part of the
primary key, you can write

```haskell
instance Table Tbl where
  data PrimaryKey Tbl f = TblKey (Columnar f ..) (Columnar f ..)
  primaryKey t = TblKey (tblField1 t) (tblField2 t)
```

more simply as

```haskell
instance Table Tbl where
  data PrimaryKey Tbl f = TblKey (Columnar f ..) (Columnar f ..)
  primaryKey = TblKey <$> tblField1 <*> tblField2
```

## The `Identity` trick

Beam table types are commonly prefixed by a `T` to indicate the name of the
generic table type. Usually, a type synonym named by leaving out the `T` is
defined by applying the table to `Identity`. Recall each field in the table is
either another table or an application of `Columnar` to the type parameter. When
the type is parameterized by `Identity`, every column is also parameterized by
`Identity`.

`Columnar` is a type family defined such that `Columnar Identity x ~ x`. Thus,
when parameterized over `Identity`, every field in the table type takes on the
underlying Haskell type.

Suppose you have a table type `ModelT` and a type synonym `type Model = ModelT
Identity`. Notice that deriving `Show`, `Eq`, and other standard Haskell type
classes won't generally work for `ModelT`. However, you can use the standalone
deriving mechanism to derive these instances for `Model`.

```haskell
data ModelT f = Model { .. } deriving (Generic, Beamable)

-- deriving instance Show (ModelT f) -- Won't work because GHC won't get the constraints right

type Model = ModelT Identity
deriving instance Show Model
deriving instance Eq Model
deriving instance Ord Model
```

## Allowed data types

Any data type can be used within a `Columnar`. Beam does no checking that a
field can be used against a particular database when the data type is defined.
Instead, type errors will occur when the table is being used as a query. For
example, the following is allowed, even though many backends will not work with
array data types.

```haskell
import qualified Data.Vector as V

data ArrayTable f
    = ArrayTable
    { arrayTablePoints :: Columnar f (V.Vector Int32)
    } deriving Generic
```

You can construct values of type `ArrayTable Identity` and even write queries
over it (relying on type inference to get the constraints right). However, if
you attempt to solve the constraints over a database that doesn't support
columns of type `V.Vector Int32`, GHC will throw an error. Thus, it's important
to understand the limits of your backend when deciding which types to use. In
general, numeric, floating-point, and text types are well supported.

## `Maybe` types

Optional fields (those that allow a SQL `NULL`) can usually be given a `Maybe`
type. However, you cannot use `Maybe` around an embedded table (you will be
unable to instantiate `Beamable`).

Beam offers a way around this. Instead of embedding the table applied to the
type parameter `f`, apply it to `Nullable f`. `Columnar (Nullable f) a ~ Maybe
(Columnar f a)` for all `a`. Thus, this will make every column in the embedded
table take on the corresponding `Maybe` type.

!!! warning "Warning"
    `Nullable` will nest `Maybe`s. That is `Columnar (Nullable f) (Maybe a) ~
    Maybe (Maybe a)`. This is bad from a SQL perspective, since SQL has no
    concept of a nested optional type. Beam treats a `Nothing` at any 'layer' of
    the `Maybe` stack as a corresponding SQL `NULL`. When marshalling data back,
    a SQL `NULL` is read in as a top-level `Nothing`.
    
    The reasons for this misfeature is basically code simplicity. Fixing this is
    a top priority of future versions of beam.

## Column tags

Above, we saw that applying `Identity` to a table type results in a type whose
columns are the underlying Haskell type. Beam uses other column tags for
querying and describing databases. Below is a table of common column tags and
their meaning.

### Converting between tags

Suppose you have a `Beamable` type paramaterized over a tag `f` and needed one
parameterized over a tag `g`. Given a function `conv :: forall a. Columnar f a
-> Columnar g a`, you can use `changeBeamRep` to convert between the tables.

There is one caveat however -- since `Columnar` is a type family, the type of
`conv` is actually ambiguous. We need a way to carry the type of `f`, `g`, and
`a` into the code. For this reason, `conv` must actually be written over the
`Columnar'`(notice the tick) `newtype`. `Columnar'` is a newtype defined as such

```haskell
newtype Columnar' f a = Columnar' (Columnar f a)
```

Notice that, unlinke `Columnar` (a non-injective type family), `Columnar'` is a
full type. The type of `conv' :: forall a. Columnar' f a -> Columnar' g a` is
now unambiguous. You can easily use `conv` to implement `conv'`:

```haskell
conv' (Columnar' a) = Columnar' (conv a)
```

## The `Beamable` type class

All beam tables, primary keys, and shared data fields must be instances of the
`Beamable` class. You cannot override the methods of `Beamable`. Rather, they
are derived using GHC's generics mechanism. Once you've declared your data type,
you can simply write `instance Beamable <your-type-name>` to instantiate the
correct `Beamable` instance for your type.

## The `Table` type class

All `Beamable` data types that you want to include as a `TableEntity` in your
database must be members of the `Table` type class. The `Table` type class
defines one associated type family `PrimaryKey` and a function `primaryKey` that
takes a table over an arbitrary column tag and produces that table's
`PrimaryKey`. For example, if you have a model

```haskell
data PersonT f
    = Person
    { personEmail     :: Columnar f Text
    , personFirstName :: Columnar f Text
    , personLastName  :: Columnar f Text
    , personAge       :: Columnar f Int
    } deriving (Generic, Beamable)
```

and you want the `personEmail` field to form the primary key, you would define a `Table` instance as such

```haskell
instance Table PersonT where
  data PrimaryKey PersonT f
      = PersonKey (Columnar f Text) deriving (Generic, Beamable)
  primaryKey person = PersonKey <$> personEmail
```

!!! tip "Tip"
    Many people find it useful to use the `Applicative` instance for `(->) a` to
    write `primaryKey`. For example, we could have written the above `primaryKey
    person = PersonKey (personFirstName person) (personLastName person)` as
    `primaryKey = PersonKey <$> personFirstName <*> personLastName`.

!!! tip "Tip"
    Typing `Columnar` may become tiresome. `Database.Beam` also exports `C` as a
    type alias for `Columnar`, which may make writing models easier. Since `C`
    may cause name clashes, all examples are given using `Columnar`.

Many also like defining type synonyms for their table and primary key types. For
example, for the table `PersonT` above, a programmer may define.

```haskell
type Person = PersonT Identity
type PersonKey = PrimaryKey PersonT Identity
deriving instance Show Person; deriving instance Eq Person
deriving instance Show PersonKey; deriving instance Eq PersonKey
```

By convention, beam table types are suffixed with `T` to distinguish their type
names from the same type parameterized over `Identity` (the 'regular' Haskell
data type).

### What about tables without primary keys?

Tables without primary keys are considered bad style. However, sometimes you
need to use beam with a schema that you have no control over. To declare a table
without a primary key, simply instantiate the `Table` class and set `PrimaryKey
tbl` to a type with no fields. Then just produce this type in `primaryKey`.

For example

```haskell
data BadT f
  = BadT
  { badFirstName :: C f Text
  , badLastName  :: C f Text
  } deriving (Generic, Beamable)
instance Beamable BadT
instance Table BadT where
  data PrimaryKey BadT f = BadNoId
    deriving (Generic, Beamable)
  primaryKey _ = BadNoId
```

## Foreign references

Foreign references are also easily supported in models by simply
embedding the `PrimaryKey` of the referred to table directly in the
parent. For example, suppose we want to create a new model
representing a post by a user.

```haskell
data PostT f
    = Post
    { postId       :: Columnar f (SqlSerial Int)
    , postPostedAt :: Columnar f LocalTime
    , postContent  :: Columnar f Text
    , postPoster   :: PrimaryKey PersonT f
    } deriving (Generic, Beamable)

instance Table PostT where
  data PrimaryKey PostT f
      = PostId (Columnar f (SqlSerial Int)) deriving (Generic, Beamable)
  primaryKey = PostId . postId

type Post = PostT Identity
type PostId = PrimaryKey PostT Identity
deriving instance Show Post; deriving instance Eq Post
deriving instance Show PostId; deriving instance Eq PostId
```

### Nullable foreign references

Above, any non-bottom value of type `PostT Identity` must carry a concrete value
of `PrimaryKey PersonT Identity`. Sometimes, you may want to optionally include
a foreign key. You can make a foreign key nullable by embedding the primary key
and adding the `Nullable` column tag modifier.

For example, to make the poster optional above.

```haskell
data PostT f
    = Post
    { postId       :: Columnar f (SqlSerial Int)
    , postPostedAt :: Columnar f LocalTime
    , postContent  :: Columnar f Text
    , postPoster   :: PrimaryKey PersonT (Nullable f)
    } deriving (Generic, Beamable)
```

### More complicated relationships

This is the extent of beam's support for defining models. Although
similar packages in other languages provide support for declaring
one-to-many, many-to-one, and many-to-many relationships, beam's
focused is providing a direct mapping of relational database concepts
to Haskell, not on abstracting away the complexities of database
querying. Thus, beam does not use 'lazy-loading' or other tricks that
obfuscate performance. Because of this, the bulk of the functionality
dealing with different types of relations is found in the querying
support, rather than in the model declarations.

Also, notice that beam does not allow you to specify any kind of reference
constraints between tables in your data types. This is because references are a
property of the database, not a particular table schema. Such relationships can
be defined using the `beam-migrate` package.

## Embedding

Sometimes, we want to declare multiple models with fields in common. Beam allows
you to simple embed such fields in common types and embed those directly into
models. For example, in
the
[Chinook example schema](https://github.com/tathougies/beam/blob/master/beam-sqlite/examples/Chinook/Schema.hs),
we define the following structure for addresses.

```haskell
data AddressMixin f
  = Address
  { address           :: Columnar f (Maybe Text)
  , addressCity       :: Columnar f (Maybe Text)
  , addressState      :: Columnar f (Maybe Text)
  , addressCountry    :: Columnar f (Maybe Text)
  , addressPostalCode :: Columnar f (Maybe Text)
  } deriving (Generic, Beamable)
type Address = AddressMixin Identity
deriving instance Show (AddressMixin Identity)
```

We can then use `AddressMixin` in our models.

```haskell
data EmployeeT f
  = Employee
  { employeeId        :: Columnar f Int32
  , employeeLastName  :: Columnar f Text
  , employeeFirstName :: Columnar f Text
  , employeeTitle     :: Columnar f (Maybe Text)
  , employeeReportsTo :: PrimaryKey EmployeeT (Nullable f)
  , employeeBirthDate :: Columnar f (Maybe LocalTime)
  , employeeHireDate  :: Columnar f (Maybe LocalTime)
  , employeeAddress   :: AddressMixin f
  , employeePhone     :: Columnar f (Maybe Text)
  , employeeFax       :: Columnar f (Maybe Text)
  , employeeEmail     :: Columnar f (Maybe Text)
  } deriving (Generic, Beamable)
-- ...
data CustomerT f
  = Customer
  { customerId        :: Columnar f Int32
  , customerFirstName :: Columnar f Text
  , customerLastName  :: Columnar f Text
  , customerCompany   :: Columnar f (Maybe Text)
  , customerAddress   :: AddressMixin f
  , customerPhone     :: Columnar f (Maybe Text)
  , customerFax       :: Columnar f (Maybe Text)
  , customerEmail     :: Columnar f Text
  , customerSupportRep :: PrimaryKey EmployeeT (Nullable f)
  } deriving (Generic, Beamable)
```

## Defaults

Based on your data type declarations, beam can already guess a lot
about your tables. For example, it already assumes that the
`personFirstName` field is accessible in SQL as `first_name`. This
defaulting behavior makes it very easy to interact with typical
databases.

For the easiest user experience, it's best to follow beam's
conventions for declaring models. In particular, the defaulting
mechanisms rely on each table type declaring only one constructor
which has fields named in the camelCase style.

When defaulting the name of a table field or column, beam
un-camelCases the field name (after dropping leading underscores) and
drops the first word. The remaining words are joined with
underscores. If there is only one component, it is not
dropped. Trailing and internal underscores are preserved in the name
and if the name consists solely of underscores, beam makes no
changes. A summary of these rules is given in the table below.

| Haskell field name | Beam defaulted column name |
|:-------------------|:---------------------------|
| `personFirstName`  | `first_name`               |
| `_personLastName`  | `last_name`                |
| `name`             | `name`                     |
| `first_name`       | `first_name`               |
| `_first_name`      | `first_name`               |
| `___` (three underscores) | `___` (no changes)  |

Note that beam only uses lower case in field names. While typically
case does not matter for SQL queries, beam always quotes
identifiers. Many DBMS's are case-sensitive for quoted
identifiers. Thus, queries can sometimes fail if your tables use
mixtures of lower- and upper-case to distinguish between fields.

For information on modifying the defaults, see the [next section](databases.md).

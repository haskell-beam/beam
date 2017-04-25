A beam model is any single-constructer Haskell record type parameterized by a
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
data ModelT f = Model { .. } deriving Generic
instance Beamable ModelT

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

You will often need to write explicit type signatures in order to get the
compiler to accept your code.




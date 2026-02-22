DuckDB is an in-process database with a focus on high-performance analytics. It is a column-oriented database, which
make it suitable for data science and data engineering workloads.

## DuckDB-specific functionality

The `beam-duckdb` backend is the most recent SQL backend for beam. As such, it might not implement all of the
DuckDB features you want. Feel free to raise [an issue!](https://github.com/haskell-beam/beam/issues/new)

## Data sources

One of the key features of DuckDB is its support for popular data formats such as Parquet or Apache Iceberg.
`beam-duckdb` adds support for declaring files as a source of data for your database, and thus allow you to
query data from these files, as if they were regular database tables.

<!-- Note that the duckdb-ffi package isn't available in Nix, and so
the following examples are not runnable at this time.
If you read this notice in the future, this might have changed!
-->
### Parquet

Parquet is an open-source file format that is commonly used in data science.

`beam-duckdb` exports the `ParquetEntity` type, which allows you to declare one or more Parquet files
as a datasource, in the same way a `ViewEntity` is used to declare a database view.

Consider a Parquet file for which the following schema holds:

```haskell
data ExamT f = Exam
  { _examId :: Columnar f Int32,
    _examName :: Columnar f Text,
    _examScore :: Columnar f Double,
    _examDate :: Columnar f Day
  }

-- Omitted various instances
```

Then, we can declare the database as having one "table" sources from a Parquet file:

```haskell
data ExampleDB f = ExampleDB
  { _exams :: f (ParquetEntity ExamT),
  }
  deriving (Generic, Database DuckDB)

exampleDb :: DatabaseSettings DuckDB ExampleDB
exampleDb =
  defaultDbSettings
    `withDbModification` (dbModification @_ @DuckDB)
      { _exams =
          parquet (NonEmpty.singleton "exams.parquet")
            <> modifyParquetFields
              tableModification
                { _examId = "id",
                  _examName = "name",
                  _examScore = "score",
                  _examDate = "exam_date"
                }
      }
```

Note here that we have specified as single file, but we could have specified
multiple files with the same schema, or even one or more globs.

Once this is done, you can query the "table" just like any other beam entity. For
example, to fetch the maximum exam score:

```haskell
runSelectReturningOne
  $ select
    $ aggregate_
        (max_ . _examScore)
        (allFromParquet_ (_exams exampleDb))
```

Note the one difference: instead of pulling all rows using `all_` (for a database table),
or `allFromView_` (for a database view), we use `allFromParquet_`. That's the only difference!

## Apache Iceberg

Apache Iceberg is an open-source format for *large* analytics tables.

Assume that we have a large Iceberg table, with the same `ExamT` schema as our Parquet example above.
We can use `IcebergTableEntity` as a replacement for `TableEntity` and instruct beam that we'll be
querying from an Apache Iceberg table:

```haskell
data ExampleDB f = ExampleDB
  { _exams :: f (IcebergTableEntity ExamT),
  }
  deriving (Generic, Database DuckDB)

exampleDb :: DatabaseSettings DuckDB ExampleDB
exampleDb =
  defaultDbSettings
    `withDbModification` (dbModification @_ @DuckDB)
      { _exams =
          icebergTable "s3://.../exams"
            <> modifyIcebergTableFields
              tableModification
                { _examId = "id",
                  _examName = "name",
                  _examScore = "score",
                  _examDate = "exam_date"
                }
      }
```

We can declare the location of our Iceberg table using `icebergTable`. In this case,
the table data is stored in an Amazon S3 bucket (assuming that you have set up the appropriate
authentication, which is not handled by `beam-duckdb`).

Again, just like with Parquet, we can perform queries using all of beam's machinery, using
`allFromIceberg_` instead of `all_`:

```haskell
runSelectReturningOne
  $ select
    $ aggregate_
        (max_ . _examScore)
        (allFromIceberg_ (_exams exampleDb))
```

## CSV

Finally, DuckDB supports loading data from comma-separated-value (CSV) files.

Assume that we have multiple CSV files, with the same `ExamT` schema as our examples above. Each file
has a header line:

```csv
id|name|score|exam_date
1|alice|76.5|2025-01-01
2|bob|74.0|2025-01-01
...
```

You can see the added wrinkle that the separator isn't a comma, but a pipe `|` instead. The CSV format is full of
fun twists like that.

We can use `CSVEntity` as a replacement for `TableEntity` and instruct beam that we'll be
querying from CSV files:

```haskell
data ExampleDB f = ExampleDB
  { _exams :: f (CSVEntity ExamT),
  }
  deriving (Generic, Database DuckDB)
```

Contrary to `parquet` and `icebergTable`, we'll need to tweak the default CSV options by changing
the default delimiter and specifying that files have a header row, using `csvWith`:

```haskell
exampleDb :: DatabaseSettings DuckDB ExampleDB
exampleDb =
  defaultDbSettings
    `withDbModification` (dbModification @_ @DuckDB)
      { _exams =
          csvWith
            (defaultCSVOptions{delim = Just "|", header = Just True})
            (NonEmpty.singleton "scores/*.csv")
            <> modifyCSVFields
              tableModification
                { _examId = "id",
                  _examName = "name",
                  _examScore = "score",
                  _examDate = "exam_date"
                }
      }
```

Once more, just like with Parquet and Apache iceberg, we can perform queries using all of beam's machinery, using
`allFromCSV_` instead of `all_`:

```haskell
runSelectReturningOne
  $ select
    $ aggregate_
        (max_ . _examScore)
        (allFromCSV_ (_exams exampleDb))
```

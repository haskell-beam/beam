This page documents other advanced features that beam supports across backends
that support them.

## SQL2003 T611: Elementary OLAP operations

This optional SQL2003 feature allows attaching arbitrary `FILTER (WHERE ..)`
clauses to aggregates. During querying only rows matching the given expression
are included in computing the aggregate. This can often be simulated in other
databases by an appropriate `CASE` expression, but beam will not do this
translation.

!beam-query
```haskell
!example chinook t611
aggregate_ (\i -> (group_ (invoiceCustomer i), as_ @Int32 $ countAll_ `filterWhere_` (invoiceTotal i >. 500), as_ @Int32 $ countAll_ `filterWhere_` (invoiceTotal i <. 100))) $
all_ (invoice chinookDb)
```

These combine as you'd expect with window functions. For example, to return each
invoice along with the average total of all invoices by the same customer where
the invoice was billed to an address in Los Angeles,

!beam-query
```haskell
!example chinook t611
withWindow_ (\i -> frame_ (partitionBy_ (invoiceCustomer i)) noOrder_ noBounds_)
            (\i w -> (i, avg_ (invoiceTotal i) `filterWhere_` (addressCity (invoiceBillingAddress i) ==. just_ "Los Angeles") `over_` w))
            (all_ (invoice chinookDb))
```

!!! danger "Danger""
    `FILTER (WHERE ..)` must be applied directly to a SQL aggregate function,
    but this isn't enforced at compile time. This may be fixed in a later
    version of beam.

This extension also provides various window functions for SQL. The only one beam
currently implements is `RANK()` via the `rank_` function. Contributions are
appreciated!

### Null Ordering

This optional SQL2003 feature allows nulls to appear before or after non-null
values in the sort ordering.

!beam-query
```haskell
!example chinook t611
limit_ 10 $
orderBy_ (\e -> (asc_ (addressState (employeeAddress e)), nullsLast_ (desc_ (addressCity (employeeAddress e))))) $
all_ (employee chinookDb)
```

## SQL2003 T612: Advanced OLAP operations

This provides both the `PERCENT_RANK()` and `CUME_DIST()` functions as
`percentRank_` and `cumeDist_` respectively.

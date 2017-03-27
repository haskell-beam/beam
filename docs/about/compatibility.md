Beam strives to cover the full breadth of the relevant SQL
standards. In general, if there is something in a SQL standard that is
not implemented in a generic manner in `beam-core`, feel free to file
an issue requesting support. There are some features that beam
purposefully omits because no major RDBMS implements them. For
example, database-level assertions are not supported in any of the
default beam backends, and thus are not supported by `beam-core`. If
you have a need for these features, feel free to file an issue. Be
sure to motivate your use case with examples and a testing strategy.

The relevant SQL standards are SQL-92, SQL:1999, SQL:2003, SQL:2008,
and SQL:2011. Because not all the standards are not publicly
accessible, I've done my best to piece together features from various
documents available online. I believe I've covered most of the common
cases, but there may be pieces of functionality that are missing. File
an issue if this is the case.

The table below summarizes the features defined in each SQL standard
and beam's support for them.

| Feature                       | Standard | Status | Notes                                  |
|:------------------------------|:--------:|:------:|:---------------------------------------|
| TODO  | TODO | TODO | TODO |

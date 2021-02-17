# anvil (development version)

# anvil 0.4.0

* `av_create_table()` gains many new arguments.
  - `partition_cols` allows defining which columns to use as an Athena partition
  - `bucket_cols` and `bucket_count` allows arranging the underlying data into buckets based on a column.
  - `async` allows for returning the query_id of a create table instead of blocking until a table is created. Thanks @rkbarney !
* Added `av_ls_s3()` for exploring S3 buckets and the contents within. Columns are parsed to the correct type and useful ones are calculated.
* `av_get_table()` gains a `full_name` argument which can take a character value in the form of `<schema>.<table>`, instead of specifying each individually.

# anvil 0.3.0

* Added `show_docs()` which will download and display a website for anvil's docs.
* Added more Athena backend translations of R functions. Mainly `stringr` functions.
* Added `av_unpack()` which will unnest a nested column on the database end.
* Added `make_constring()` which will convert a DSN stored in a .ini file to it's string form for use as an env variable.
* `av_get_result()` gains a `...` argument for passing arguments down to `.f`. Thanks @ssupalla !
* `as.character()` and `as.integer()` now convert to Athena friendly SQL.
* `av_create_table()` gains an `overwrite` argument that will first drop the target table if it exists.
* `av_drop_table()` now uses the `IF EXISTS` clause.

# anvil 0.2.1

* Removed hacky method of engaging the Rstudio connection pane from `av_connect()`. It was causing a crash. Connect with `athena_old` to activate panel, if needed.

# anvil 0.2.0

* Added `av_create_table()` that makes tables on Athena for SQL, tbls, and local data frames.
* Added `av_drop_table()` which enables removing a table from Athena.
* Added `av_sample_table()` that enables pulling a random sample from a query/tbl.
* Added many tests across the package to bring coverage up to ~84%.
* Various documentation fixes and polishing.
* R CMD check should only result in 1 note when building vignettes.

# anvil 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* First release of anvil - focused on pulling in data from Athena and S3.

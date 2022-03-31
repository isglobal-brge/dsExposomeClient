# dsExposomeClient 2.0.3

+ Added Anderson-Darling test `ds.anderson.darling.test`.
+ `ds.exposure_histogram` now checks the number of samples and if over 5000 uses `ds.anderson.darling.test` instead of `ds.shapiro.test`.

# dsExposomeClient 2.0.2

+ Added datasources to function calls inside `ds.exposure_histogram`
+ Removed rogue `browser()` from `ds.invExWAS`, changed `tef` default to `FALSE`. Recieved object from the server is a list instead of a `ExposomeSet` object, therefore there is no need to have `S4Vectors` installed on the client.
+ Check for `variable` added to `ds.exposome_summary`.

# dsExposomeClient 2.0.1


First release with news file.

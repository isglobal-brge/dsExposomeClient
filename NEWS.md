# dsExposomeClient (development version)

# dsExposomeClient 2.0.2

+ Added datasources to function calls inside `ds.exposure_histogram`
+ Removed rogue `browser()` from `ds.invExWAS`, changed `tef` default to `FALSE`. Recieved object from the server is a list instead of a `ExposomeSet` object, therefore there is no need to have `S4Vectors` installed on the client.
+ Check for `variable` added to `ds.exposome_summary`.

# dsExposomeClient 2.0.1


First release with news file.

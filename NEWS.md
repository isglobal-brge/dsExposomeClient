# dsExposomeClient (development version)

+ added argument `rownames2col` to the function `exposures_pData` to be able to retrieve the ids and later merge

# dsExposomeClient 2.0.5

Added new functions to 

+ extract the exposure and spatial information from `NetCDF` data
+ relate this information with clinical location of individuals

This allows us to create new exposure data to be analyzed as an ExposomeSet object with the already present `dsExposome` functions.

`ds.NetCDF_fillvalue_matrix`, `ds.ncvar_get`, `ds.ncatt_get` and `ds.get_exposure_from_geo`

# dsExposomeClient 2.0.4

`ds.exposome_correlation`: Changed output. Now outputs split and combine results.
`corPlot`: Now plots the combined results instead of only just the first study server.
Passed the `datasources` value to internal call functions, solves problems when more than one connection is performed on a single R session.

# dsExposomeClient 2.0.3

+ Added Anderson-Darling test `ds.anderson.darling.test`.
+ `ds.exposure_histogram` now checks the number of samples and if over 5000 uses `ds.anderson.darling.test` instead of `ds.shapiro.test`.

# dsExposomeClient 2.0.2

+ Added datasources to function calls inside `ds.exposure_histogram`
+ Removed rogue `browser()` from `ds.invExWAS`, changed `tef` default to `FALSE`. Recieved object from the server is a list instead of a `ExposomeSet` object, therefore there is no need to have `S4Vectors` installed on the client.
+ Check for `variable` added to `ds.exposome_summary`.

# dsExposomeClient 2.0.1


First release with news file.

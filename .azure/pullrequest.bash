#!/bin/bash
RELEASE_SCOPE="patch"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', usethis::use_version('${RELEASE_SCOPE}'))"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::check(force_suggests = TRUE, error_on = 'note'))"

#!/bin/bash

AGENT_USER_HOMEDIRECTORY=$(echo "${AGENT_HOMEDIRECTORY}" | cut -d/ -f 1-3)
echo "Create user libraries directory R [ ${R_LIBS_USER} ]"
mkdir -p "${R_LIBS_USER}"
echo "Install required Linux dependencies"
sudo apt-get update
sudo apt-get -qq -y install libudunits2-dev
echo "Install required R dependencies"
Rscript -e "install.packages(c('git2r', 'covr', 'withr', 'devtools', 'lintr', 'mockery', 'pkgdown', 'corrplot', 'circlize', 'FactoMineR', 'units', 'sf'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "install.packages(c('DSI', 'DSOpal'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "install.packages(c('BiocManager'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "BiocManager::install('BiocStyle', lib='${R_LIBS_USER}')"
Rscript -e "BiocManager::install('S4Vectors', lib='${R_LIBS_USER}')"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::install_github('datashield/dsBaseClient', ref='6.1.1'))"
cd "${BUILD_REPOSITORY_LOCALPATH}"
library(DSLite)
library(data.table)
library(dsBaseClient)
library(rexposome)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# install development versions of dsExposome and dsExposomeClient
  # P
devtools::install("../dsExposome")
devtools::install("../dsExposomeClient")

# source development packages
library(dsExposome)
library(dsExposomeClient)

# prepare data in a light DS server
exposures <- utils::read.table("test_data/exposures.csv", header = TRUE,
                         row.names = "idnum", sep = ",", na.strings = c("NA", "-", "?", " ", ""))
phenotypes <- utils::read.table("test_data/phenotypes.csv", header = TRUE,
                         row.names = "idnum", sep = ",", na.strings = c("NA", "-", "?", " ", ""),
                         stringsAsFactors = TRUE)
description <- utils::read.table("test_data/description.csv", header = TRUE,
                          row.names = "Exposure", sep = ",", na.strings = c("NA", "-", "?", " ", ""))


dslite.server <- newDSLiteServer(tables=list(exposures = exposures, description = description, phenotypes = phenotypes),
                                 config = DSLite::defaultDSConfiguration(include=c("dsBase", "dsExposome")))
dslite.server$config()

# datashield logins and assignments
log <- data.frame(server = c("sim1", "sim2", "sim3"),
                  url = c("dslite.server"),
                  table = c("exposures", "description", "phenotypes"),
                  driver = c("DSLiteDriver"))
conns <- datashield.login(log, assign = T)

ds.loadExposome("exposome_set")
ds.exwas("blood_pre ~ age", "exposome_set", "gaussian")

# logout of dslite
datashield.logout(conns)


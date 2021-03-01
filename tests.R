library(DSLite)
library(dsBaseClient)
library(dsBase)
library(rexposome)
library(resourcer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# install development versions of dsExposome and dsExposomeClient
# P
devtools::install("../dsExposome-BRGE")
devtools::install("../dsExposomeClient-BRGE")

# source development packages
library(dsExposome)
library(dsExposomeClient)

exposures <- newResourceClient(
  resourcer::newResource(
    name = "exposures",
    url = "https://raw.githubusercontent.com/isglobal-brge/dsExposomeClient/master/inst/extdata/exposures.csv",
    format = "csv"
  )
) 

phenotypes <- newResourceClient(
  resourcer::newResource(
    name = "phenotypes",
    url = "https://raw.githubusercontent.com/isglobal-brge/dsExposomeClient/master/inst/extdata/phenotypes.csv",
    format = "csv"
  )
) 

description <- newResourceClient(
  resourcer::newResource(
    name = "description",
    url = "https://raw.githubusercontent.com/isglobal-brge/dsExposomeClient/master/inst/extdata/description.csv",
    format = "csv"
  )
) 

# pilots <- read.table('data/PILOT.DAT', col.names =  c('Group', 'Intelligence', 'Form Relations',
#                                                       'Dynamometer', 'Dotting', 'Sensory Motor Coordination',
#                                                       'Perservation'))
# pilots$Group <- ifelse(pilots$Group == 1, 'Apprentice', 'Pilot')
# pilots <- pilots[,2:7]

dslite.server <- newDSLiteServer(resources=list(exposures = exposures, description = description, phenotypes = phenotypes),
                                 config = DSLite::defaultDSConfiguration(include=c("dsBase", "dsExposome", "resourcer")))
dslite.server$config()

# datashield logins and assignments
log <- data.frame(server = c("sim1"),
                  url = c("dslite.server"),
                  resource = list("exposures", "description", "phenotypes"),
                  driver = c("DSLiteDriver"))
conns <- datashield.login(log, assign = T)

# base::assign(x = "pilots", value = pilots, envir = parent.frame())

datashield.assign.expr(conns, symbol = "description", expr = quote(as.resource.data.frame(description)))
datashield.assign.expr(conns, symbol = "exposures", expr = quote(as.resource.data.frame(exposures)))
datashield.assign.expr(conns, symbol = "phenotypes", expr = quote(as.resource.data.frame(phenotypes)))

ds.loadExposome("exposures", "description", "phenotypes", "idnum", "idnum", "Exposure", "Family", 5, FALSE, "exposome_set")
ds.plotFamily("exposome_set", family = "Phthalates", group = "sex")
ds.plotFamily("exposome_set", family = "Phthalates", group = "sex", group2 = "rhinitis")
ds.exposure_histogram("exposome_set", "BDE209")

ds.exposures_pData("exposome_set", "all", "wew")

ds.boxPlotGG_table("wew", c("Hg","Cu"), "sex", "cbmi")


exwas_res <- ds.exwas("blood_pre ~ sex + age", "exposome_set", "gaussian", TRUE, conns)

ds.plotExwas(exwas_res, "manhattan")

ds.standardize("exposome_set", name = "wew")

ds.exposome_pca("exposome_set", c("Metals", "Organochlorines"))
ds.exposome_pca_plot("ds.exposome_pca.Results", "exposures", labels = TRUE, phenotype = "sex", 1, 4, 20)



ds.pca.get("ds.pcaResults", "ind_coord")

ds.plotPCA(a, "wew")

ds.plotFamily("exposome_set", "all" ,"sex", "whistling_chest", datasources = conns)

ds.familyNames("exposome_set")





ds.imputation("exposome_set", NULL)



a=ds.exposure_histogram("exposome_set", "Zn", FALSE)





a=DSI::datashield.aggregate(conns, quote(histogramDS2('dta$cbmi', method.indicator = 1)))

a=ds.normalityTest("exposome_set")


ds.plotMissings("exposome_set", 'phenotypes', 'p')


a = ds.BoxPlot.o(x = "dta$blood_pre", "dta$whistling_chest", type = "combine")
do.call("bxp", a)


a = ds.exposome_pca("exposome_set")




a = ds.BoxPlot.o(x = "dta$blood_pre", "dta$sex", type = "combine")
a
do.call("bxp", a)

# logout of dslite
datashield.logout(conns)

























library(dsBaseClient)
library(dsExposomeClient)
library(DSOpal)

builder <- newDSLoginBuilder()
builder$append(server = "server1", url = "https://opal-demo.obiba.org/",
               user = "administrator", password = "password",
               driver = "OpalDriver")
logindata <- builder$build()
conns <- datashield.login(logins = logindata)

datashield.assign.resource(conns, symbol = 'description', resource = list(server1 = 'EXPOSOME.description'))
datashield.assign.expr(conns, symbol = "description", expr = quote(as.resource.data.frame(description)))

datashield.assign.resource(conns, symbol = 'exposures', resource = list(server1 = 'EXPOSOME.exposures'))
datashield.assign.expr(conns, symbol = "exposures", expr = quote(as.resource.data.frame(exposures)))

datashield.assign.resource(conns, symbol = 'phenotypes', resource = list(server1 = 'EXPOSOME.phenotypes'))
datashield.assign.expr(conns, symbol = "phenotypes", expr = quote(as.resource.data.frame(phenotypes)))

ds.loadExposome("exposures", "description", "phenotypes", "idnum", "idnum", "Exposure", "Family", 5, FALSE, "exposome_object")

a = ds.plotMissings("exposome_object", "phenotypes", conns)




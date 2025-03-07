rm(list = ls())
#renv::load()
library("tidyverse")
library("udpipe")
library("here")
library("stm")
library("data.table")
library("tidytext")
# load and initialize data
out = readRDS(file="outputs/stm_doc.rds")

# compute diagnostic for STM model
set.seed(123456)
diagnostic <- searchK(out$documents, out$vocab,
                      K = c(20, 30, 40, 50, 55, 60, 70, 80, 90, 100, 110),
                      prevalence =  ~ s(Year) + Journal + qsTop25 +  female_in_group + n.authors + no_idenfication,
                      data = out$data,init.type = "Spectral")
##### Save ####
saveRDS(diagnostic,file=here("outputs","diagnostic.rds"))
###############
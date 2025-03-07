####################
rm(list = ls())
#renv::load()
library("tidyverse")
library("udpipe")
library("here")
library("stm")
library("data.table")
library("tidytext")

#load and initialize data
out = readRDS(file="outputs/stm_doc.rds")

docs <- out$documents
vocab <- out$vocab
meta <- out$data

# create stm object
k=50 # number of topics

# calibrate the topic model
stm.output <- stm(docs, vocab, K=k, prevalence = ~ s(Year) + Journal + qsTop25 +  female_in_group + n.authors + no_idenfication,
                  max.em.its=500, data=meta, init.type="Spectral",
                  seed=123456)
##### SAVE ######
saveRDS(stm.output,file=paste0("outputs/stmoutput_filt_",k,".rds"))
#################
  rm(list = ls())
  #renv::load()
  library("tidyverse")
  library("udpipe")
  library("here")
  library("stm")
  library("data.table")
  library("tidytext")
  
  
  # Getting data
 # data <- read.csv("data/final_data.csv")
  data <- read.csv("data/final_data_filt.csv",encoding = "UFT-8")
  
  ####################
  
  # Annotate the data using udpipe in parallel and save it
  
  ####################
  
  # Load English model for udpipe
  ud_model <- udpipe_download_model(language = "english-ewt", overwrite = F)
  ud_en <- udpipe_load_model(ud_model)
  
  # Function for annotating split data
  annotate_splits <- function(x, file) {
    ud_model <- udpipe_load_model(file)
    x <- as.data.table(udpipe_annotate(ud_model,
                                       x = x$b_abstract,
                                       doc_id = x$ID))
    return(x)
  }
  # 
  # # load parallel library future.apply
  library(future.apply)
  
  
  
  # Define cores to be used
  ncores <- 12L
  plan(multisession, workers = ncores)
  
  # split comments based on available cores
  corpus_splitted <- split(data, (as.numeric(rownames(data))-1) %/% 200)
  
  # Get final annotation
  annotation <- future_lapply(corpus_splitted, annotate_splits, file = ud_model$file_model)
  annotation <- rbindlist(annotation)

sapply(annotation, function(x) sum(is.na(x)))


annofinance <- annotation

lowering = str_count(annofinance$lemma, "[A-Z]")
annofinance$lemma[lowering <= 1] = stringr::str_to_lower(annofinance$lemma[lowering <= 1])

saveRDS(annofinance, file = "data/annofinance.rds")





rm(list = ls())
#renv::load()
k=50
library("tidyverse")
library("udpipe")
library("here")
library("stm")
library("data.table")
library("tidytext")

stm.output <- readRDS(paste0("outputs/stmoutput_filt_",k,".rds"))
data <- read.csv("data/final_data_filt.csv",encoding = "UFT-8")

# Function to convert topics into a csv file.

prop_csv <- function(output = output, data = data, f, terms = 10, names = F){
  
  td_beta <- tidy(output)
  
  top_terms <- td_beta %>%
    arrange(beta) %>%
    group_by(topic) %>%
    top_n(terms, beta) %>%
    arrange(-beta) %>%
    select(topic, term) %>%
    summarise(terms = list(term)) %>%
    mutate(terms = map(terms, paste, collapse = ", ")) %>%
    unnest(cols=c(terms))
  
  
  td_gamma <- tidy(stm.output, matrix = "gamma",
                   document_names = data$ID)
  
  gamma_terms <- td_gamma %>%
    group_by(topic) %>%
    summarise(gamma = mean(gamma)) %>%
    left_join(top_terms, by = "topic") %>%
    mutate(topic = paste0("Topic:", topic))
  
  non_NA_DOI = data$DOI[!is.na(data$DOI)]
  non_NA_DOI_title = data$Article.Title[!is.na(data$DOI)]
  non_NA_DOI_abstract = data$Abstract[!is.na(data$DOI)]

  sage= stm::sageLabels(output,n = terms)
  gamma_terms$FREX =  apply(sage$marginal$frex,MARGIN = 1,FUN = function(x) paste0(x[1:terms],collapse = ", "))
  gamma_terms$LIFT =  apply(sage$marginal$lift,MARGIN = 1,FUN = function(x) paste0(x[1:terms],collapse = ", "))
  gamma_terms$SCORE =  apply(sage$marginal$score,MARGIN = 1,FUN = function(x) paste0(x[1:terms],collapse = ", "))
  gamma_terms$prob =  apply(sage$marginal$prob,MARGIN = 1,FUN = function(x) paste0(x[1:terms],collapse = ", "))
  gamma_terms$max_DOI = non_NA_DOI[apply(stm.output$theta[!is.na(data$DOI),],2,FUN = which.max)]
  gamma_terms$title = non_NA_DOI_title[apply(stm.output$theta[!is.na(data$DOI),],2,FUN = which.max)]
  gamma_terms$abstract = non_NA_DOI_abstract[apply(stm.output$theta[!is.na(data$DOI),],2,FUN = which.max)]

  write.csv(gamma_terms,f,row.names = F)
}

prop_csv(stm.output,data = data,f =  paste0("outputs/stmoutput_filt_",k,".csv"),terms = 10)

stop("break here")



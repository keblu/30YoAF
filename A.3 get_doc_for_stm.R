
rm(list = ls())
#renv::load()
library("tidyverse")
library("udpipe")
library("here")
library("stm")
library("data.table")
library("tidytext")
library("Matrix")


annofinance <- readRDS("data/annofinance.rds")
data <- read.csv("data/final_data_filt.csv",encoding = "UFT-8")

####################

# Add keywords

####################


# Define the identifier at which we will build a topic model
annofinance$topic_level_id <- unique_identifier(annofinance, fields = c("doc_id","paragraph_id", "sentence_id"))


length(annofinance$lemma[annofinance$lemma==""])


# Find keywords with RAKE
keyw_rake <- keywords_rake(annofinance,
                           term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"),
                           relevant = annofinance$upos %in% c("NOUN", "ADJ"),
                           ngram_max = 3, n_min = 15)
keyw_rake <- subset(keyw_rake,ngram >1)
keyw_rake <- keyw_rake[(keyw_rake$freq > 100 & keyw_rake$ngram == 2) | (keyw_rake$freq > 50 & keyw_rake$ngram == 3),]

write.csv(keyw_rake,"outputs/keyw_rake.csv",row.names = FALSE)

keyw_rake <- read.csv("outputs/keyw_rake.csv")



# Find simple noun phrases

annofinance$phrase_tag <- as_phrasemachine(annofinance$upos, type = "upos")
keyw_nounphrases <- keywords_phrases(annofinance$upos, term = annofinance$lemma,
                                     pattern = "(NOUN+NOUN+NOUN)|(ADJ+NOUN)|(NOUN+NOUN)|(ADJ+ADJ+NOUN)|(ADJ+NOUN+NOUN)|(NOUN+ADJ+NOUN)|(NOUN+ADP+NOUN)", is_regex = TRUE,
                                     detailed = FALSE)
keyw_nounphrases <- subset(keyw_nounphrases, ngram > 1)
keyw_nounphrases <- keyw_nounphrases[(keyw_nounphrases$freq>100 & keyw_nounphrases$ngram == 2) | (keyw_nounphrases$freq>50 & keyw_nounphrases$ngram == 3),]


write.csv(keyw_nounphrases,"outputs/keyw_nounphrases.csv",row.names = FALSE)

keyw_nounphrases <- read.csv("outputs/keyw_nounphrases.csv")

# Number of different parts of speech
table(annofinance$upos)

# # Recode terms to keywords 
annofinance$term <- annofinance$lemma
annofinance$term <- txt_recode_ngram(annofinance$term,
                                     compound = keyw_rake$keyword, ngram = keyw_rake$ngram)
annofinance$term <- txt_recode_ngram(annofinance$term,
                                     compound = keyw_nounphrases$keyword, ngram = keyw_nounphrases$ngram)
# # Keep only keyword or Nouns
annofinance$term <- ifelse(annofinance$upos %in% c("NOUN","PROPN"), annofinance$term,
                           ifelse(annofinance$term %in% c(keyw_rake$keyword, keyw_nounphrases$keyword), annofinance$term, NA))

annofinance$term = stringr::str_replace_all(annofinance$term, pattern = "-",replacement = " ")
annofinance$term = stringr::str_squish(annofinance$term)
# Create a doument term frequency matrix and sort by id
dtf <- document_term_frequencies(annofinance, document = "doc_id", term = "term")
a <- sort(unique(dtf$doc_id))
data <- data[data$ID %in% a, ]

# # Create a document/term/matrix for building a topic model
dtm <- document_term_matrix(x = dtf)
#
# # Remove words which occur less than ten times
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 50, remove_emptydocs = F)

dtm_clean <- dtm_clean[order(as.numeric(row.names(dtm_clean))),]

dummy = factor(stringr::str_to_lower(colnames(dtm_clean)))

dummy = as(dummy, "sparseMatrix")
dummy = t(dummy)

dtm_clean = dtm_clean %*% dummy
colnames(dtm_clean) = colnames(dummy)


dtm_clean = dtm_clean[,nchar(colnames(dtm_clean)) >= 3]
dtm_clean = dtm_clean[,stringr::str_sub(colnames(dtm_clean),start = 1,end = 1) != "-"]
dtm_clean = dtm_clean[,stringr::str_sub(colnames(dtm_clean),start = 1,end = 1) != "\\&"]
country_list = countrycode::codelist[!is.na(countrycode::codelist$cowc),]

dtm_clean = dtm_clean[,!(colnames(dtm_clean) %in% c("&as","elsevier","blackwell","blackwell publishers",'inc','inc.','iii',"ii",
                                     "finance association","oxford","springer","llc","llc.","jel",
                                     "southwestern finance association","reuters","hong","blackwell publishers","blackwell"
                                     ,"kong","michael","cfa","american finance association","amihud"
                                     ,"australia","bannks","california","campbell","kluwer","jagannathan",
                                     "washington","university","school","press","emerald","publishing",
                                     "wiley","ohio","john","johnson","journals","journal","korea","publishers","publisher",
                           country_list$country.name.en,"united","states",
                           stringr::str_to_lower(country_list$country.name.en),
                           stringr::str_to_lower(country_list$currency),
                           country_list$currency,
                           stringr::str_to_lower(country_list$currency),
                           stringr::str_to_upper(country_list$currency),
                           stringr::str_to_lower(country_list$cowc),
                           stringr::str_to_lower(country_list$dhs),
                           country_list$cowc,
                           country_list$dhs,
                           country_list$continent,
                           stringr::str_to_lower(country_list$continent)))]
save(dtm_clean,file = "outputs/dtm_clean.rda")

data <- data[order(data$ID),]

data$n.authors = pmax(sapply(stringr::str_split(data$Authors,";"),FUN = length),sapply(stringr::str_split(data$Full.Name,";"),FUN = length))
out <-
  asSTMCorpus(dtm_clean, data = data[c(
    "Year",
    "abbrev",
    "Funding",
    "Journal",
    "Full.Name",
    "Article.Title",
    "Abstract",
    "qsTop25",
    "DOI",
    "Authors",
    "Addresses",
    "male",
    "female",
    "no_idenfication",
    "n.authors",
    "Australia",
    "Belgium",
    "Canada" ,
    "China" ,
    "France" ,
    "Germany"  ,
    "Greece" ,
    "India"      ,
    "Italy"   ,
    "Japan"    ,
    "Netherlands"   ,
    "New.Zealand"  ,
    "Singapore" ,
    "South.Korea"  ,
    "Spain"      ,
    "Switzerland"    ,
    "Taiwan"    ,
    "United.Kingdom" ,
    "United.States"  ,
    "other"
  )])
out$data$female_in_group = out$data$female > 0


saveRDS(out,file="outputs/stm_doc.rds")

rm(list = ls())

topics <- read.csv("outputs/stmoutput_filt_50.csv")

topics = topics[order(topics$label, decreasing = FALSE),]

topics$label = stringr::str_to_title(topics$label)

topics$label <- paste0("\\textsf{", topics$label, "}", sep = "")

topics = topics[,c("label","terms")]

topics$terms = stringr::str_replace_all(topics$terms," ","\\\\_")
topics$terms = stringr::str_replace_all(topics$terms,",\\\\_",", ")
head(topics)


print(xtable::xtable(topics), 
      include.rownames = FALSE, 
      sanitize.text.function = identity, 
      file = "tables/tbl_label_topic.txt")

require(quanteda)
load("outputs/dtm_clean.rda")
keywords = data.frame(keywords = colnames(dtm_clean),
                      value = colSums(dtm_clean),
                      precentage = round(colSums(dtm_clean != 0)/nrow(dtm_clean) *100,2))

keywords$keywords = stringr::str_replace_all(keywords$keywords," ","\\\\_")
n <- nrow(keywords)
dim(keywords) / 2
keywords$value = format(keywords$value, nsmall=0, big.mark=",")
keywords$value = stringr::str_squish(keywords$value)
tmp <- cbind(keywords[1:1933,], rbind(keywords[1934:n,]))
tmp = as.data.frame(tmp)
tmp[,c(1)] = paste0(tmp[,c(1)]," & ")
tmp[,c(2)] = paste0(tmp[,c(2)]," & ")
tmp[,c(3)] = paste0(tmp[,c(3)]," & ")
tmp[,c(4)] = paste0(tmp[,c(4)]," & ")
tmp[,c(5)] = paste0(tmp[,c(5)]," & ")
tmp[,c(6)] = paste0(tmp[,c(6)], "\\\\")

write.table(tmp,row.names = FALSE,quote =  FALSE,
            file = "tables/tbl_vocab.txt")

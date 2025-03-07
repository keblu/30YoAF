rm(list = ls())

#load and initialize data
tmp <- read.csv(file = "outputs/stmoutput_filt_50.csv")

tbl <- tmp[, c("label", "gamma")]

#remove unwanted topics
pos_rm <- which(tbl$label == "Remainder 2" |
                  tbl$label  == "Remainder 1" |
                  tbl$label  == "Financial Literature Analysis" |
                  tbl$label == "Literature Reviews" |
                  tbl$label == "Statistical Modeling")

#generate table
tbl <- tbl[-pos_rm, ]
tbl <- tbl[order(tbl$gamma, decreasing = TRUE), ]
tbl$gamma <- round(tbl$gamma * 100, 2)
tbl$label <- paste0("\\textsf{", tbl$label, "}", sep = "")
rownames(tbl) <- NULL
tbl

print(xtable::xtable(tbl),
      include.rownames = FALSE,
      sanitize.text.function = identity,
      file = "tables/tbl_2_topic.txt")

# get option pricing only part for text

stm_outputs <- readRDS("outputs/stmoutput_filt_50.rds")
beta <- stm_outputs$beta[[1]][[1]]
dim(beta)

tmp <- read.csv(file = "outputs/stmoutput_filt_50.csv")
pos <- which(tmp$label == "Option Pricing")

pos2 <- order(beta[pos, ], decreasing = TRUE)[1:10]

round(100 * exp(beta[pos, pos2]), 2)
tmp$terms <- stringr::str_replace_all(tmp$terms, " ", "\\\\_")
tmp$terms <- stringr::str_replace_all(tmp$terms, ",\\\\_", ", ")
tmp$terms[pos]

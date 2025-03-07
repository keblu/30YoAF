rm(list = ls())
library("tidyverse")
library("readxl")
library("DataExplorer")
library("dplyr")
library("stringr")

#load and initialize data
final_data <- read.csv(file = "data/final_data_filt.csv")

listsource <- unique(final_data$Journal)

# compute statistics
tbl <- c()
for (s in listsource) {
  pos    <- final_data$Journal == s
  nam_   <- str_to_title(s)
  abbr_  <- final_data$abbrev[which(pos == 1)[1]]
  y_min  <- min(final_data$Year[pos])
  y_max  <- max(final_data$Year[pos])
  n_     <- sum(pos)
  a_     <- round(mean(table(final_data$Year[pos])))
  tbl    <- rbind(tbl, cbind(nam_, abbr_, n_, a_, y_min, y_max))
}
tbl
tbl[, 1] <- stringr::str_replace(tbl[, 1], "And", "and")
tbl[, 1] <- stringr::str_replace(tbl[, 1], "Of", "of")
head(tbl)

xtable::xtable(tbl)

#generate table
tmp <- as.numeric(tbl[, 3])
tmp1 <- c(min(tmp), max(tmp), median(tmp),  mean(tmp), sum(tmp))

tmp <- as.numeric(tbl[, 4])
tmp2 <- c(min(tmp), max(tmp), median(tmp), mean(tmp), sum(tmp))


print(xtable::xtable(tbl),
      include.rownames = FALSE,
      sanitize.text.function = identity,
      file = "tables/tbl_1_journal.txt")

tbl_app <- cbind(tmp1, tmp2)
tbl_app <- round(tbl_app)

data.frame(c(" ", "", "", "", ""),
           c("Minumum", "Maximum", "Median", "Average", "Total"),
           tbl_app,
           c(" ", "", "", "", ""),
           c(" ", "", "", "", ""))
print(xtable::xtable(tbl_app, digits = 0),
      include.rownames = FALSE,
      sanitize.text.function = identity,
      file = "tables/tbl_1_journal_panel2.txt")


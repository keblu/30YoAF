rm(list = ls())
library("stm")
library("corrr")
library("tidyverse")
library("corrr")
library("igraph")
library("ggraph")

#load and initialize data
stm_outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data
topics      <- read.csv("outputs/stmoutput_filt_50.csv")
u_year      <- sort(unique(data$Year))

#generate average theta per year
means_per_year <- list()

for (i in seq_along(u_year)) {
  id_match <- which(data$Year == u_year[i])
  means_per_year[[i]] <- data.frame(t(colMeans(stm_outputs$theta[id_match, ])))
}

means_per_year <- data.table::rbindlist(means_per_year)
means_per_year <- t(means_per_year)

#remove unwated topics
pos_rm <- which(topics$label == "Remainder 2" |
                  topics$label  == "Remainder 1" |
                  topics$label  == "Financial Literature Analysis" |
                  topics$label == "Literature Reviews" |
                  topics$label == "Statistical Modeling")

topics <- topics[-pos_rm, ]
means_per_year <- means_per_year[-pos_rm, ]

#generate statistics

top_lab <- topics$label[apply(means_per_year, MARGIN = 2, which.max)]
top_pct <- apply(means_per_year, MARGIN = 2, function(x) x[which.max(x)])
bot_lab <- topics$label[apply(means_per_year, MARGIN = 2, which.min)]
bot_pct <- apply(means_per_year, MARGIN = 2, function(x) x[which.min(x)])

norm_means_per_year <- means_per_year / matrix(apply(means_per_year, 2, sum),
                                               nrow(means_per_year),
                                               ncol(means_per_year),
                                               byrow = TRUE)
ntopics <- nrow(means_per_year)


#degree of concentration

hh <- apply(norm_means_per_year,
            MARGIN = 2,
            function(x) sum(x^2))

hh <- (hh - (1 / ntopics)) / (1 - (1 / ntopics))

#generate table
max_topic <- data.frame("Year"   = u_year,
                       "Top"    = top_lab,
                       "TopPct" = round(100 * top_pct, 2),
                       "Bot"    = bot_lab,
                       "BotPct" = round(100 * bot_pct, 2),
                       "TC"     = round(100 * hh, 2))

max_topic[, "Top"] <- paste0("\\textsf{", max_topic[, "Top"], "}", sep = "")
max_topic[, "Bot"] <- paste0("\\textsf{", max_topic[, "Bot"], "}", sep = "")

#regression for the text
y <- max_topic$TC
x <- 1992:2021

summary(lm(y ~ 1 + x))

library("MASS")
summary(MASS::rlm(y ~ 1 + x))

print(xtable::xtable(max_topic),
      include.rownames = FALSE,
      sanitize.text.function = identity,
      file = "tables/tbl_3_topic_max_min_year.txt")

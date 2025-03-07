rm(list = ls())
library("stm")
library("corrr")
library("tidyverse")
library("corrr")
library("igraph")
library("ggraph")
library("plm")
library("sandwich")
library("betareg")
library("plm")
source("functions.R")

#load and initialize data
stm_outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data
topics      <- read.csv("outputs/stmoutput_filt_50.csv")

#compute average theta by journal
u_journal <- unique(data$abbrev)

means_per_journal <- list()
for (i in seq_along(u_journal)) {
  id_match <- which(data$abbrev == u_journal[i])
  means_per_journal[[i]] <- data.frame(t(colMeans(stm_outputs$theta[id_match, ])))
}

means_per_journal <- data.table::rbindlist(means_per_journal)
means_per_journal <- t(means_per_journal)

#filter unewated topics
pos_rm <- which(topics$label == "Remainder 2" |
                  topics$label  == "Remainder 1" |
                  topics$label  == "Financial Literature Analysis" |
                  topics$label == "Literature Review" |
                  topics$label == "Statistical Modeling")


means_per_journal <- means_per_journal[-pos_rm, ]
topics <- topics[-pos_rm, ]

#generate statitistics
ntop <- nrow(topics)
top_lab <- topics$label[apply(means_per_journal, MARGIN = 2, which.max)]
top_pct <- apply(means_per_journal, MARGIN = 2, function(x) x[which.max(x)])
bot_lab <- topics$label[apply(means_per_journal, MARGIN = 2, which.min)]
bot_pct <- apply(means_per_journal, MARGIN = 2, function(x) x[which.min(x)])

norm_means_per_journal <- means_per_journal / matrix(apply(means_per_journal,
                                                           2,
                                                           sum),
                                                     nrow(means_per_journal),
                                                     ncol(means_per_journal),
                                                     byrow = TRUE)
ntopics <- nrow(norm_means_per_journal)

#degree of complexity
hh <- apply(norm_means_per_journal, MARGIN = 2, function(x) sum(x^2))
hh <- (hh - (1 / ntopics)) / (1 - (1 / ntopics))

#generate table first part
max_topic <- data.frame(u_journal,
                       "Top"    = top_lab,
                       "TopPct" = round(100 * top_pct, 2),
                       "Bot"    = bot_lab,
                       "BotPct" = round(100 * bot_pct, 2),
                       "TC"     = round(100 * hh, 2))

max_topic[, "Top"] <- paste0("\\textsf{", max_topic[, "Top"], "}", sep = "")
max_topic[, "Bot"] <- paste0("\\textsf{", max_topic[, "Bot"], "}", sep = "")



################################################################################
##

#compute signficance of complexty trend
nsim <- 10
set.seed(1234)
list_df <- vector("list", nsim)
for (i in 1:nsim) {
  cat(i, "\n")
  thetasims    <- stm:::thetaPosterior(stm.outputs, nsims = 1, type = "Global")
  thetasims    <- do.call(rbind, thetasims)
  list_df[[i]] <- f_df_theta2tc(thetasims)
}
save(list_df, file = "outputs/fit_C.5.rda")

###############################################################################

load(file = "outputs/fit_C.5.rda")

set.seed(1234)
list_fit <- lapply(list_df, f_fit,
                   fml = as.formula(tc ~ 0 + journal + dyear:journal),
                   type = "betareg")

nsims <- length(list_df)
fml <- as.formula(tc ~ 1 + dyear)
nJ <- length(u_journal)

storage <- vector(mode = "list", length = nJ)
for (i in 1:nsims) {
  for (j in 1:nJ) {
    cat("i: ", i, " j: ", j, "\n")
    df_i   <- list_df[[i]]
    df_i_j <- df_i[df_i$journal == u_journal[j], ]
    fit    <- f_fit(fml, df_i_j, "betareg")
    storage[[j]][[i]] <- list(est = fit$est, vcov = fit$vcov)
  }
}
object <- list(parameters = storage)

nsim <- 100
tables <- vector(mode = "list", length = nJ)
set.seed(1234)
for (j in 1:nJ) {
  sims <- lapply(object$parameters[[j]], function(x) {
    stm:::rmvnorm(nsim,
                  x$est,
                  x$vcov)
    })
  sims <- do.call(rbind, sims)
  est  <- colMeans(sims)
  se   <- sqrt(apply(sims, 2, stats::var))
  tval <- est / se
  rdf  <- nrow(object$data) - length(est)
  p    <- 2 * stats::pnorm(abs(tval), lower.tail = FALSE)
  coefficients <- cbind(est, se, tval, p)
  rownames(coefficients) <- attr(object$parameters[[1]][[1]]$est,
                                 "names")
  colnames(coefficients) <- c("Estimate", "Std. Error",
                              "t value", "Pr(>|t|)")
  tables[[j]] <- coefficients
}

### generate table part 2
tbl <- c()
for (j in 1:nJ){
  tbl <- rbind(tbl, tables[[j]][2, ])
}

tbl2 <- tbl3 <- format(round(tbl[, 1, drop = FALSE] * 100, 3),
                       trim = FALSE,
                       digits = 2,
                       nsmall = 2)
id_10 <- which(tbl[, 4] < 0.10)
tbl3[id_10, ] <- paste0("\\grb{", tbl2[id_10, ], "}")
id_05 <- which(tbl[, 4] < 0.05)
tbl3[id_05, ] <- paste0("\\grbb{", tbl2[id_05, ], "}")
id_01 <- which(tbl[, 4] < 0.01)
tbl3[id_01, ] <- paste0("\\grbbb{", tbl2[id_01, ], "}")
rownames(tbl3) <- u_journal

base::write(print(xtable::xtable(cbind(max_topic, tbl3)),
                  include.rownames = FALSE,
                  sanitize.text.function = identity),
            file = "tables/tbl_5_topic_max_min_journal.txt")

rm(list = ls())
library("stm")
library("corrr")
library("tidyverse")
library("corrr")
library("igraph")
library("ggraph")
source("functions.R")

#load and initialize data
stm_outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data
topics      <- read.csv("outputs/stmoutput_filt_50.csv")
u_year      <- sort(unique(data$Year))
u_journal   <- sort(unique(data$abbrev))

# add delta year in the data frame
colnames(data)
n_journal <- length(u_journal)
startyear <- rep(NA, n_journal)
for (i in seq_along(u_journal)) {
  startyear[i] <- min(data$Year[data$abbrev == u_journal[i]])
}
names(startyear) <- u_journal
data$dyear <- data$Year - startyear[data$abbrev]

# perform estimation
nsims <- 10
uncertainty <- "Global"
set.seed(1234)
reg_lm <- stm::estimateEffect(~ 0 + as.factor(Journal) +
                                 as.factor(Journal):dyear,
                               stmobj = stm_outputs,
                               metadata = data,
                               uncertainty = uncertainty,
                               nsims = nsims)

set.seed(1234)
reg_beta <- f_estimateEffect(~ 0 + as.factor(Journal) +
                                as.factor(Journal):dyear,
                               stmobj = stm_outputs,
                               metadata = data,
                               stm_outputs = uncertainty,
                               type = "betareg",
                               nsims = nsims)

save(reg_lm, reg_beta, file = "outputs/fit_C.6.rda")

############################################################

rm(list = ls())
library("stm")
library("corrr")
library("tidyverse")
library("corrr")
library("igraph")
library("ggraph")
source("functions.R")

# perform estimation
stm_outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data
topics      <- read.csv("outputs/stmoutput_filt_50.csv")
u_year      <- sort(unique(data$Year))
u_journal   <- sort(unique(data$abbrev))

# post-process estimation
load(file = "outputs/fit_C.6.rda")

reg <- reg_beta

#organize results
set.seed(1234)
reg_res <- summary(reg)
coef_list <- list()
for (j in seq_along(reg_res$tables)) {
  coef_list[[j]] <- data.frame(t(reg_res$tables[[j]][-1:-32, 1]))
}

coef <- data.table::rbindlist(coef_list)
coef <- data.frame(coef)
colnames(coef) <-  sort(unique(data$abbrev))
rownames(coef) <- topics$label

#remove unwanted topics
pos_rm <- which(topics$label == "Remainder 2" |
                  topics$label  == "Remainder 1" |
                  topics$label  == "Financial Literature Analysis" |
                  topics$label == "Literature Review" |
                  topics$label == "Statistical Modeling")

coef <- coef[-pos_rm, ]
rownames(coef) <- rownames(coef)

#create table

res <- data.frame(journal = colnames(coef),
                  max_topic = rownames(coef)[apply(coef, MARGIN = 2, FUN = order, decreasing = TRUE)[1, ]],
                  coef_max = apply(coef, MARGIN = 2, FUN = function(x) x[order(x, decreasing = TRUE)[1]]) * 100,
                  max_topic = rownames(coef)[apply(coef, MARGIN = 2, FUN = order, decreasing = TRUE)[2, ]],
                  coef_max = apply(coef, MARGIN = 2, FUN = function(x) x[order(x, decreasing = TRUE)[2]]) * 100,
                  min_topic = rownames(coef)[apply(coef, MARGIN = 2, FUN = order, decreasing = FALSE)[1, ]],
                  coef_min = apply(coef, MARGIN = 2, FUN = function(x) x[order(x, decreasing = FALSE)[1]]) * 100,
                  min_topic = rownames(coef)[apply(coef, MARGIN = 2, FUN = order, decreasing = FALSE)[2, ]],
                  coef_min = apply(coef, MARGIN = 2, FUN = function(x) x[order(x, decreasing = FALSE)[2]]) * 100)

res[, c(3, 5, 7, 9)] <- round(res[, c(3, 5, 7, 9)], 2)
res[, c(3, 5, 7, 9)] <- format(res[, c(3, 5, 7, 9)],
                           trim = FALSE,
                           digits = 2,
                           nsmall = 2)

res[, c(2)] <- stringr::str_to_title(res[, c(2)])
res[, c(4)] <- stringr::str_to_title(res[, c(4)])
res[, c(6)] <- stringr::str_to_title(res[, c(6)])
res[, c(8)] <- stringr::str_to_title(res[, c(8)])

reg_res_table_print <- data.frame(journal = res[, 1],
                                 max_move = paste0("\\textsf{",
                                                   res[, 2],
                                                   "} (",
                                                   res[, 3],
                                                   "),\\textsf{",
                                                   res[, 4],
                                                   "} (",
                                                   res[, 5],
                                                   ")"),
                                 min_move = paste0("\\textsf{",
                                                   res[, 6],
                                                   "} (",
                                                   res[, 7],
                                                   "),\\textsf{",
                                                   res[, 8],
                                                   "} (",
                                                   res[, 9],
                                                   ") "))

head(reg_res_table_print)

base::write(print(xtable::xtable(reg_res_table_print),
                  include.rownames = FALSE,
                  sanitize.text.function = identity),
            file = "tables/tbl_6_topic_trend_journal.txt")
n_journal <- length(u_journal)
startyear <- rep(NA, n_journal)
for (i in seq_along(u_journal)) {
  startyear[i] <- min(data$Year[data$abbrev == u_journal[i]])
}

reg_res <- summary(reg)
res_table = list()
for(i in 1:32){
  
  pos_rm <- which(topics$label == "Remainder 2" |
                    topics$label  == "Remainder 1" |
                    topics$label  == "Financial Literature Analysis" |
                    topics$label == "Literature Review" |
                    topics$label == "Statistical Modeling")

  
  x = data.table::rbindlist(lapply(reg_res$tables,FUN = function(x) {

    data.frame(x[i,1],x[i+32,1])
  }))

  y = data.table::rbindlist(lapply(reg_res$tables,FUN = function(x) {
    
    data.frame(x[i,3],x[i+32,3])
  }))
  x = data.frame(x)
  x = x[-pos_rm,]
  y = y[-pos_rm,]
  
  x[,3] = exp(x[,1])/(exp(x[,1]) + 1)
  x[,4] = exp(x[,1] + x[,2]*(2023-startyear[i]))/(exp(x[,1] + x[,2]*(2023-startyear[i])) + 1)
  x[,3] = x[,3]/sum(x[,3])
  x[,4] = x[,4]/sum(x[,4])
  y = data.frame(y)
  rownames(x) = topics$label[-pos_rm]
  colnames(x) = c("Constant","Time-Trend")
  x$Constant = exp(x$Constant)/(exp(x$Constant)+1)
  x = format(round(x*100,2),nsmall = 2)
  x_tmp = x
  id.sig = 1 - pnorm(abs(y[,1])) < 0.05
  x[id.sig,1] = paste0("\\grb{",x_tmp[id.sig,1],"}")
  id.sig = 1 - pnorm(abs(y[,2])) < 0.05
  x[id.sig,2] = paste0("\\grb{",x_tmp[id.sig,2],"}")
 
  id.sig = 1 - pnorm(abs(y[,1])) < 0.025
  x[id.sig,1] = paste0("\\grbb{",x_tmp[id.sig,1],"}")
  id.sig = 1 - pnorm(abs(y[,2])) < 0.025
  x[id.sig,2] = paste0("\\grbb{",x_tmp[id.sig,2],"}")
  
  id.sig = 1 - pnorm(abs(y[,1])) < 0.005
  x[id.sig,1] = paste0("\\grbbb{",x_tmp[id.sig,1],"}")
  id.sig = 1 - pnorm(abs(y[,2])) < 0.005
  x[id.sig,2] = paste0("\\grbbb{",x_tmp[id.sig,2],"}")
  


  x  = x[order(x_tmp[,4],decreasing = TRUE),]

  x = cbind(rownames(x),x)
  x[,1] = paste0("\\textsf{",x[,1],"}")
  x[,1] = paste0(x[,1]," & ")
  x[,2] = paste0(x[,2]," & ")
  x[,3] = paste0(x[,3]," & ")
  x[,4] = paste0(x[,4]," & ")
  x[,5] = paste0(x[,5]," \\\\ ")
  
 write(append = TRUE,x =  paste0("\\begin{table}[H]
\\caption{\\textbf{Time-Trends in Topic Prevalence: ", colnames(coef)[i],"}\\\\
This table reports the first issue (YFI) and the 2023 prediction of the prevalence for the 45 topics covered in ", colnames(coef)[i] ,", according to the values obtained from the estimated model of Equation \\ref{eq:trendtopic2}.}
\\label{tab:topic}
\\centering
\\scalebox{0.95}{
\\begin{tabular}{lcccc}
\\toprule
Topic & YFI & 2023 &  \\\\
\\midrule",paste0(paste0(x[,1],x[,4],x[,5])
                  ,collapse = "\n"),"\n\\bottomrule
\\end{tabular}}
\\end{table}\n
\\newpage"),file = "tables/tbl_journal_ind_res.txt")
}

names(res_table) = colnames(coef)


  
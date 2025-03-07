rm(list = ls())
library("stm")
library("corrr")
library("tidyverse")
library("corrr")
library("igraph")
library("ggraph")
library("reshape2")
library("ggplot2")
library("betareg")
source("functions.R")

#load and initialize data
stm.outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data
topics      <- read.csv("outputs/stmoutput_filt_50.csv")
u.year      <- sort(unique(data$Year))

# perform estimation
nsims <- 10; uncertainty = "Global"
set.seed(1234)
reg_lm <- stm::estimateEffect( ~ 1 + I(Year - 1992), stm.outputs, metadata = data, 
                               uncertainty = uncertainty, nsims = nsims)

set.seed(1234)
reg_beta <- f_estimateEffect( ~ 1 + I(Year - 1992), stm.outputs, metadata = data, 
                              uncertainty = uncertainty, type = "betareg", nsims = nsims)

save(reg_lm, reg_beta, file = "outputs/fit_C.4.rda")

##############################################################

rm(list = ls())
library("stm")
library("corrr")
library("tidyverse")
library("corrr")
library("igraph")
library("ggraph")
library("reshape2")
library("ggplot2")
library("betareg")
source("functions.R")

stm.outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data
topics      <- read.csv("outputs/stmoutput_filt_50.csv")
u.year      <- sort(unique(data$Year))

# post-process estimation
load(file = "outputs/fit_C.4.rda")

#reg <- reg_lm
reg <- reg_beta

set.seed(1234)
reg_res <- summary(reg)
reg_res <- sapply(reg_res$tables,FUN = function(x) x[1:2,1:2])

# reg_res_table = round(cbind(reg_res[1,],reg_res[2,])*100,2)
reg_res_table = round(cbind(exp(reg_res[1,]) / (exp(reg_res[1,]) + 1), reg_res[2,])*100,2)

reg_res_table_print = reg_res_table
reg_res_table_print = format(reg_res_table_print, trim = FALSE, digits = 2, nsmall = 2)

id.sig = 1 - pnorm(abs(reg_res[1,]/reg_res[3,])) < 0.05
reg_res_table_print[id.sig,1] = paste0("\\grb{",reg_res_table[id.sig,1],"}")
id.sig = 1 - pnorm(abs(reg_res[2,]/reg_res[4,])) < 0.05
reg_res_table_print[id.sig,2] = paste0("\\grb{",reg_res_table[id.sig,2],"}")

id.sig = 1 - pnorm(abs(reg_res[1,]/reg_res[3,])) < 0.025
reg_res_table_print[id.sig,1] = paste0("\\grbb{",reg_res_table[id.sig,1],"}")
id.sig = 1 - pnorm(abs(reg_res[2,]/reg_res[4,])) < 0.025
reg_res_table_print[id.sig,2] = paste0("\\grbb{",reg_res_table[id.sig,2],"}")

id.sig = 1 - pnorm(abs(reg_res[1,]/reg_res[3,])) < 0.005
reg_res_table_print[id.sig,1] = paste0("\\grbbb{",reg_res_table[id.sig,1],"}")
id.sig = 1 - pnorm(abs(reg_res[2,]/reg_res[4,])) < 0.005
reg_res_table_print[id.sig,2] = paste0("\\grbbb{",reg_res_table[id.sig,2],"}")

rownames(reg_res_table_print) = topics$label
reg_res_table_print

reg_res_table_print = data.frame(reg_res_table_print)
reg_res_table_print$topic = rownames(reg_res_table_print)

colnames(reg_res_table_print) = c("alpha","beta","topic")
reg_res_table_print = reg_res_table_print[,c("topic","alpha","beta")]

pos_rm <- which(topics$label == "Remainder 2" | 
                  topics$label  == "Remainder 1" |
                  topics$label  == "Financial Literature Analysis" |
                  topics$label == "Literature Reviews" | 
                  topics$label == "Statistical Modeling")



reg_res_table       = reg_res_table[-pos_rm, ]
reg_res_table_print = reg_res_table_print[-pos_rm, ]
reg_res_table_print = reg_res_table_print[order(reg_res_table[,2], decreasing = TRUE),]

reg_res_table_print$topic <- paste0("\\textsf{", reg_res_table_print$topic, "}")

base::write(print(xtable::xtable(reg_res_table_print), 
                  include.rownames = FALSE, 
                  sanitize.text.function = identity),
            file = "tables/tbl_4_topic_trend.txt")


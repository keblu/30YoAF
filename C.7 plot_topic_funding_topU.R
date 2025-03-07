rm(list = ls())
library("stm")
library("corrr")
library("tidyverse")
library("corrr")
library("igraph")
library("ggraph")
library("xtable")
source("functions.R")

#load and initialize data
stm_outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data
topics      <- read.csv("outputs/stmoutput_filt_50.csv")


# Detect first year of publication of each journals within the sample
u_year      <- sort(unique(data$Year))
u_journal   <- sort(unique(data$abbrev))

n_journal <- length(u_journal)
startyear <- rep(NA, n_journal)
for (i in seq_along(u_journal)) {
  startyear[i] <- min(data$Year[data$abbrev == u_journal[i]])
}
names(startyear) <- n_journal
data$dyear <- data$Year - startyear[data$abbrev]

# perform estimation
nsims <- 10
uncertainty <- "Global"
set.seed(1234)
reg_lm <- stm::estimateEffect(~ 1 + dyear + qsTop25 +  female_in_group + 
                                n.authors + no_idenfication,
                               stmobj = stm.outputs,
                               metadata = data,
                               uncertainty = uncertainty,
                               nsims = nsims)

set.seed(1234)
reg_beta <- f_estimateEffect(~ 1 + dyear +  qsTop25 +  female_in_group +
                               n.authors + no_idenfication,
                              stmobj = stm.outputs,
                              metadata = data,
                              uncertainty = uncertainty,
                              type = "betareg",
                              nsims = nsims)

save(reg_lm, reg_beta, file = "outputs/fit_C.7.rda")

###################################################

# post-process estimation
rm(list = ls())
library("stm")
library("corrr")
library("tidyverse")
library("corrr")
library("igraph")
library("ggraph")
library("xtable")
source("functions.R")

#load and initialize data
load(file = "outputs/fit_C.7.rda")
stm.outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data
topics      <- read.csv("outputs/stmoutput_filt_50.csv")

reg <- reg_beta

set.seed(1234)
reg_res_sum <- summary(reg)

### Compile results ####
reg_res <- sapply(reg_res_sum$tables,
                  FUN = function(x) x[1:5, 1:4])

reg_res_table <- round(cbind(reg_res[3, ],
                            reg_res[4, ],
                            reg_res[5, ]) * 100,
                      digits = 2)
reg_res_table_pval <- cbind(reg_res[18, ],
                           reg_res[19, ],
                           reg_res[20, ])

colnames(reg_res_table) <- 
  colnames(reg_res_table_pval) <-
  c("Top25", "Gender Dummy", "N Authors")

dat <- data.frame(topic = topics$label,
                 x = reg_res_table[, "Gender Dummy"],
                 y = reg_res_table[, "Top25"],
                 z = reg_res_table[, "N Authors"])

dat_pval <- data.frame(topic = topics$label,
                      x = reg_res_table_pval[, "Gender Dummy"],
                      y =  reg_res_table_pval[, "Top25"],
                      z = reg_res_table_pval[, "N Authors"] )

#Remove unwanted topics
pos_rm <- which(topics$label == "Remainder 2" |
                  topics$label  == "Remainder 1" |
                  topics$label  == "Financial Literature Analysis" |
                  topics$label == "Literature Reviews" |
                  topics$label == "Statistical Modeling")

dat <- dat[-pos_rm, ]
dat_pval <- dat_pval[-pos_rm, ]

colnames(dat)  <- 
  colnames(dat_pval)  <- 
  c("topic", "gender", "top25", "n.author")

# retrieve estimate and pvalues to compile into table

n_to_show <- 6 # number of topic to show for each panels

gender = cbind(dat[order(dat$gender,decreasing = TRUE),1:2][c(1:n_to_show),],"",
         dat[order(dat$gender,decreasing = TRUE),1:2][c(nrow(dat):(nrow(dat)-n_to_show+1)),])

gender_pval = cbind(dat_pval[order(dat$gender,decreasing = TRUE),1:2][c(1:n_to_show),],"",
                    dat_pval[order(dat$gender,decreasing = TRUE),1:2][c(nrow(dat_pval):(nrow(dat_pval)-n_to_show+1)),])

top25 = cbind(dat[order(dat$top25,decreasing = TRUE),c(1,3)][c(1:n_to_show),],"",
               dat[order(dat$top25,decreasing = TRUE),c(1,3)][c(nrow(dat):(nrow(dat)-n_to_show+1)),])

top25_pval = cbind(dat_pval[order(dat$top25,decreasing = TRUE),c(1,3)][c(1:n_to_show),],"",
                   dat_pval[order(dat$top25,decreasing = TRUE),c(1,3)][c(nrow(dat_pval):(nrow(dat_pval)-n_to_show+1)),])

n.author = cbind(dat[order(dat$n.author,decreasing = TRUE),c(1,4)][c(1:n_to_show),],"",
              dat[order(dat$n.author,decreasing = TRUE),c(1,4)][c(nrow(dat):(nrow(dat)-n_to_show+1)),])
n.author_pval = cbind(dat_pval[order(dat$n.author,decreasing = TRUE),c(1,4)][c(1:n_to_show),],"",
                      dat_pval[order(dat$n.author,decreasing = TRUE),c(1,4)][c(nrow(dat_pval):(nrow(dat_pval)-n_to_show+1)),])

colnames(gender) = colnames(gender_pval) = colnames(top25) = colnames(top25_pval) =  colnames(n.author) =  colnames(n.author_pval) = c("topic","parameter","","topic","parameter")
gender_print = gender
gender_print[gender_pval[,c(2)] <= 0.05,c(2)] = paste0("\\grb{",gender[gender_pval[,c(2)] <= 0.05,c(2)],"}") 
gender_print[gender_pval[,c(5)] <= 0.05,c(5)] = paste0("\\grb{",gender[gender_pval[,c(5)] <= 0.05,c(5)],"}")

gender_print[gender_pval[,c(2)] <= 0.025,c(2)] = paste0("\\grbb{",gender[gender_pval[,c(2)] <= 0.025,c(2)],"}") 
gender_print[gender_pval[,c(5)] <= 0.025,c(5)] = paste0("\\grbb{",gender[gender_pval[,c(5)] <= 0.025,c(5)],"}")

gender_print[gender_pval[,c(2)] <= 0.005,c(2)] = paste0("\\grbbb{",gender[gender_pval[,c(2)] <= 0.005,c(2)],"}") 
gender_print[gender_pval[,c(5)] <= 0.005,c(5)] = paste0("\\grbbb{",gender[gender_pval[,c(5)] <= 0.005,c(5)],"}") 

top25_print = top25
top25_print[top25_pval[,c(2)] <= 0.05,c(2)] = paste0("\\grb{",top25[top25_pval[,c(2)] <= 0.05,c(2)],"}") 
top25_print[top25_pval[,c(5)] <= 0.05,c(5)] = paste0("\\grb{",top25[top25_pval[,c(5)] <= 0.05,c(5)],"}")

top25_print[top25_pval[,c(2)] <= 0.025,c(2)] = paste0("\\grbb{",top25[top25_pval[,c(2)] <= 0.025,c(2)],"}") 
top25_print[top25_pval[,c(5)] <= 0.025,c(5)] = paste0("\\grbb{",top25[top25_pval[,c(5)] <= 0.025,c(5)],"}")

top25_print[top25_pval[,c(2)] <= 0.005,c(2)] = paste0("\\grbbb{",top25[top25_pval[,c(2)] <= 0.005,c(2)],"}") 
top25_print[top25_pval[,c(5)] <= 0.005,c(5)] = paste0("\\grbbb{",top25[top25_pval[,c(5)] <= 0.005,c(5)],"}") 

n.author_print = n.author
n.author_print[n.author_pval[,c(2)] <= 0.05,c(2)] = paste0("\\grb{",n.author[n.author_pval[,c(2)] <= 0.05,c(2)],"}") 
n.author_print[n.author_pval[,c(5)] <= 0.05,c(5)] = paste0("\\grb{",n.author[n.author_pval[,c(5)] <= 0.05,c(5)],"}")

n.author_print[n.author_pval[,c(2)] <= 0.025,c(2)] = paste0("\\grbb{",n.author[n.author_pval[,c(2)] <= 0.025,c(2)],"}") 
n.author_print[n.author_pval[,c(5)] <= 0.025,c(5)] = paste0("\\grbb{",n.author[n.author_pval[,c(5)] <= 0.025,c(5)],"}")

n.author_print[n.author_pval[,c(2)] <= 0.005,c(2)] = paste0("\\grbbb{",n.author[n.author_pval[,c(2)] <= 0.005,c(2)],"}") 
n.author_print[n.author_pval[,c(5)] <= 0.005,c(5)] = paste0("\\grbbb{",n.author[n.author_pval[,c(5)] <= 0.005,c(5)],"}") 

table_print = rbind("gender","",gender_print,"","top25","",top25_print,"","n.author","",n.author_print)
table_print = as.matrix(table_print)
table_print[,1:4] = paste0(table_print[,1:4]," &")
table_print[,5] = paste0(table_print[,5]," \\\\")
table_print[,c(1,4)] =  paste0("\\textsf{",table_print[,c(1,4)],"}")
table_print[,c(1,4)] = stringr::str_replace_all(table_print[,c(1,4)],"\\\\textsf\\{ &\\}"," & ")
table_print[,] = stringr::str_replace_all(table_print[,]," &\\}"," \\} & ")

####### SAVe #######
write.table(table_print,file = "tables/tbl_7_gender_author_top.txt",row.names = FALSE,quote = FALSE)
###################

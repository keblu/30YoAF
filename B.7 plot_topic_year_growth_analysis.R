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
stm_outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data
topics      <- read.csv("outputs/stmoutput_filt_50.csv")
u_year      <- sort(unique(data$Year))

# parameter for estimation
nsims <- 10
uncertainty <- "Global"

# perform estimation
set.seed(1234)
reg_lm <- stm::estimateEffect(~ 1 + as.factor(Year),
                              stm_outputs,
                              metadata = data,
                              uncertainty = uncertainty,
                              nsims = nsims)

set.seed(1234)
reg_beta <- f_estimateEffect(~ 1 + as.factor(Year),
                             stm_outputs,
                             metadata = data,
                             uncertainty = uncertainty,
                             type = "betareg",
                             nsims = nsims)

##### Save regression results ######
save(reg_lm, reg_beta, file = "outputs/fit_B.7.rda")
####################################

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
stm_outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data
topics      <- read.csv("outputs/stmoutput_filt_50.csv")

# post-process estimation
load(file = "outputs/fit_B.7.rda")

reg <- reg_lm
limit <- c(-4, 5)
reg <- reg_beta
limit <- c(-40, 50)

set.seed(1234)
reg_res2 <- summary(reg)
reg_res2 <- sapply(reg_res2$tables, FUN = function(x) x[2:nrow(x), 1] * 100)

#Remove non-valid topics
pos_rm <- which(topics$label == "Remainder 2" |
                  topics$label == "Remainder 1" |
                  topics$label == "Financial Literature Analysis" |
                  topics$label == "Literature Review" |
                  topics$label == "Statistical Modeling")
reg_res2 <- reg_res2[, -pos_rm]
topics <- topics[-pos_rm, ]

colnames(reg_res2) <- topics$label
reg_res2 <- reg_res2[, order(colMeans(reg_res2[1:29,]))]

rownames(reg_res2) <- 1993:2021

melted_cormat <- melt(reg_res2)
##### generate figure ######
limit <- c(min(melted_cormat$value), max(melted_cormat$value))
p1 <- ggplot(data = melted_cormat,
             aes(x = Var1,
                 y = Var2,
                 fill = value)) +
  scale_fill_gradient2(low = "red",
                       high = "cyan",
                       mid = "gray10",
                       midpoint = 0,
                       limit = limit,
                       space = "Lab",
                       name = "Coefficient") +
  scale_x_continuous(breaks = seq(1992, 2021, 1)) +
  geom_tile() +
  xlab("Year") +
  ylab("Topic") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 30),
        axis.text.x = element_text(size = 26,
                                 angle = 90,
                                 vjust = 0.5,
                                 hjust = 1),
        axis.text.y = element_text(size = 26),
        legend.key.size = unit(2, "cm"))

####### SAVE ######
ggsave("figures/fig_7_growth_topic.pdf", plot = p1, width = 20, height = 20)
###################

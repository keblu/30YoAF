rm(list = ls())
library("stm")
library("corrr")
library("tidyverse")
library("corrr")
library("igraph")
library("ggraph")
library("tidygraph")

#load and initialize data
#topic model
k <- 50 #model to select
stm_outputs <- readRDS(paste0("outputs/stmoutput_filt_", k, ".rds"))
#annotation
topics      <- read.csv(paste0("outputs/stmoutput_filt_", k, ".csv"),
                        sep = ",")

rho <- 0.45 #threshold for network

theta <- stm_outputs$theta
colnames(theta) <- topics$label

#Remove non-valid topics
pos_rm <- which(topics$label == "Remainder 2" |
                  topics$label == "Remainder 1" |
                  topics$label == "Financial Literature Analysis" |
                  topics$label == "Literature Review" |
                  topics$label == "Statistical Modeling")
theta <- theta[, -pos_rm]

#compute correlation and put the right labels
tidy_cors <- theta %>%
  correlate(method = "spearman", diagonal = TRUE)
colnames(tidy_cors)[-1] <- tidy_cors$term
tidy_cors <- tidy_cors %>% stretch()
colnames(tidy_cors)[3] <- "correlation"

#transform into graph representation
graph_cors <- tidy_cors %>%
  filter(correlation > rho) %>%
  graph_from_data_frame(directed = FALSE)
graph_cors <- tidygraph::as_tbl_graph(graph_cors)
graph_cors <- graph_cors %>%
              mutate(centrality = centrality_degree(normalized = TRUE))
# generate figure
#set seed for replicability of figure
set.seed(3)
p1 <- ggraph(graph_cors, "stress") +
  geom_edge_arc(aes(edge_alpha = abs(correlation),
                    edge_width = abs(correlation),
                    color = correlation),
                strength = 0.2,
                alpha = 0.5,
                show.legend = FALSE) +
  scale_edge_colour_gradientn(limits = c(rho, 1),
                              colors = c("gray70",
                                         "gray30")) +
  scale_edge_width(range = c(0, 2),
                   guide = "none") +
  geom_node_point(size = 2,
                  color = "black") +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 6) +
  theme_graph(base_size = 12)

#### SAVE ####
ggsave("figures/fig_6_network_topic.pdf", plot = p1, width = 20, height = 15)
##############

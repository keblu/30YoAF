rm(list = ls())
library("stm")
library("corrr")
library("tidyverse")
library("ggraph")
library("igraph")
library("tidygraph")

#load and initialize data
stm_outputs <- readRDS("outputs/stmoutput_filt_50.rds")
data        <- readRDS("outputs/stm_doc.rds")$data

rho <- 0.45 # correlation limit to sparsify the graph

# compute average prevalence vector per journals
u_journal <- unique(data$abbrev)
means_per_journal <- list()

for (i in seq_along(u_journal)) {
  id_match <- which(data$abbrev == u_journal[i])
  means_per_journal[[i]] <- data.frame(t(colMeans(stm_outputs$theta[id_match, ])))
}

means_per_journal <- data.table::rbindlist(means_per_journal)
means_per_journal <- t(means_per_journal)
colnames(means_per_journal) <- stringr::str_to_title(u_journal)

#Compute journal topic correlation
tidy_cors <- means_per_journal %>%
  correlate(method = "spearman", diagonal = TRUE)
tidy_cors$term <- u_journal
colnames(tidy_cors)[-1] <- u_journal
tidy_cors <- tidy_cors %>% stretch()
colnames(tidy_cors)[3] <- "correlation"

# transform into graph format
graph_cors <- tidy_cors %>%
  filter(correlation > rho) %>%
  graph_from_data_frame(directed = FALSE)

graph_cors <- tidygraph::as_tbl_graph(graph_cors)

# set seed for graph
set.seed(12314)

# generate the figure
p1 <- ggraph(graph_cors, "stress") +
  geom_edge_arc(aes(edge_alpha = abs(correlation),
                    edge_width = abs(correlation),
                    color = correlation),
                strength = 0.2,
                alpha = 0.5,
                show.legend = FALSE) +
  scale_edge_colour_gradientn(limits = c(rho, 1),
                              colors = c("gray90",
                                         "gray30")) +
  scale_edge_width(range = c(0, 2),
                   guide = "none") +
  geom_node_point(size = 2,
                  color = "black") +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 6) +
  theme_graph(base_size = 18)

####### SAVE #####
ggsave("figures/fig_8_network_journal.pdf", plot = p1, width = 20, height = 15)
##################

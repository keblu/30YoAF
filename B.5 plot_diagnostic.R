rm(list = ls())
library("ggplot2")

#load and initialize data
diagnostic <- readRDS("outputs/diagnostic.rds")

# generate figure
p1 <- ggplot2::ggplot(diagnostic$results,
                      aes(x = unlist(semcoh),
                          y = unlist(exclus))) +
  geom_line() +
  geom_text(label = unlist(diagnostic$results$K),
            size = 10) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    size = 1)) +
  ylab("Exclusivity") +
  xlab("Semantic coherence") +
  theme(text = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28))

#### SAVE ####
ggsave("figures/fig_5_diagnostic.pdf", plot = p1, width = 20, height = 15)
##############

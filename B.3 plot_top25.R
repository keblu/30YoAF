rm(list = ls())
library("tidyverse")
library("udpipe")
library("here")
library("stm")
library("data.table")
library("tidytext")
library("grid")
require("gridExtra")
#load and initialize data
data <- readRDS(file = "outputs/stm_doc.rds")$data

#compute statistics for top institution authorship per year
n_top_authorship_year <- aggregate(data$qsTop25 == "Yes",
              by = list(year = data$Year),
              FUN = sum)

#normalize by number of articles
n_top_authorship_year[, 2] <- n_top_authorship_year$x /
                              aggregate(data$qsTop25,
                                        by = list(year = data$Year),
                                        FUN = length)[, 2]
n_top_authorship_year$x = n_top_authorship_year$x*100
#figure for across-journal average
p1 <- ggplot2::ggplot(n_top_authorship_year, aes(x = year, y = x)) +
  geom_line() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ylab("%") + xlab("Year") +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12)) +
  geom_point()

# compute statistics for top institution authorship  per year and journal
n_top_authorship_year_journal <- aggregate(data$qsTop25 == "Yes",
                                          by = list(year = data$Year,
                                                    abbrev = data$abbrev),
                                          FUN = sum)

# normalize by number of articles per journal per year
n_top_authorship_year_journal[, 3] <- n_top_authorship_year_journal$x /
                                    aggregate(data$qsTop25,
                                              by = list(year = data$Year,
                                                        abbrev = data$abbrev),
                                              FUN = length)[, 3]
n_top_authorship_year_journal$x = n_top_authorship_year_journal$x*100
# remove large outliers as they mess the figure
#(arises because evry low denomiator or some missing data in some journal/year)
n_top_authorship_year_journal$x <- DescTools::Winsorize(
                                    n_top_authorship_year_journal$x,
                                    probs = c(0.01, 0.99))

upper_limit <- max(n_top_authorship_year_journal$x)
lower_limit <- min(n_top_authorship_year_journal$x)

p2 <- ggplot(data = n_top_authorship_year_journal,
            aes(x = year,
                y = reorder(abbrev, x),
                fill = x)) +
  scale_fill_gradient2(low = "gray100",
                       high = "gray0",
                       mid = "gray50",
                       midpoint = median(n_top_authorship_year_journal$x,
                                         na.rm = TRUE),
                       limit = c(lower_limit,
                                 upper_limit),
                       space = "Lab",
                       name = "Percentage") +
  scale_x_continuous(breaks = seq(1992, 2021, 1)) +
  geom_tile() +
  xlab("Year") +
  ylab("Journal") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        axis.text.x = element_text(size = 12,
                                 angle = 90,
                                 vjust = 0.5,
                                 hjust = 1),
        axis.text.y = element_text(size = 12),
        legend.key.size = unit(1, "cm"))

# Combine figures

panel_a <- ggplot_gtable(ggplot_build(p1))
panel_b <- ggplot_gtable(ggplot_build(p2))

fig <- gtable:::rbind_gtable(panel_a, panel_b, "last")
panels <- fig$layout$t[grep("panel", fig$layout$name)]
fig$heights[panels[1]][[1]] <-  unit(1, "null")
fig$heights[panels[2]][[1]] <-  unit(6, "null")

#### SAVE #####
pdf("figures/fig_3_top25_year_journal.pdf",
    height = 10,
    width = 8,
    paper = "special")
grid.newpage()
grid.draw(fig)
dev.off()
#############

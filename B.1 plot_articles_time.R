rm(list = ls())
library("ggplot2")
library("RColorBrewer")
library("ggplot2")
library("grid")
require("grDevices")
library("scales")
#load and initialize data
final_data_filt <-  readRDS(file = "outputs/stm_doc.rds")$data
merged_data <- final_data_filt

# compute statistics for number of article published by journal and by years
# long representation
summary_journal_year <- aggregate(merged_data$Year,
                                  by = list(Journal = merged_data$abbrev,
                                            year = merged_data$Year),
                                  FUN = length)
# wide representation
summary_journal_year_wide <- reshape2::dcast(summary_journal_year,
                                             Journal ~ year)
summary_journal_year$year = as.Date(paste0(summary_journal_year$year,"-06-30"))
# seed for colors
set.seed(12)
# Generate figure
color <- grDevices::colors()[grep("gr(a|e)y",
                                  grDevices::colors(),
                                  invert = TRUE)]

p1 <- ggplot2::ggplot(summary_journal_year, aes(fill = Journal,
                                                y = x,
                                                x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = sample(color,
                                    nrow(summary_journal_year_wide))) +
  scale_x_date(breaks = unique(summary_journal_year$year), 
               date_labels =  "%Y") +
  scale_y_continuous(label = comma) +
  theme_minimal() + 
  theme(legend.position=c(0.15,0.70),
        text = element_text(size = 18),
        axis.text.x = element_text(size = 12,angle=90,vjust = 0.5, hjust=0),
        axis.text.y = element_text(size = 12)) +
  theme(panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    size = 1)) +
  ylab("Number of articles") +
  xlab("Year")
p1

##### Save figure ######
pdf("figures/fig_1_journal_evolution.pdf",
    height = 8,
    width = 10,
    paper = "special")
p1
dev.off()
########################
# rbind(c(sum(summary_journal_year_wide$`1992`,na.rm = TRUE),
# sum(summary_journal_year_wide$`2021`,na.rm = TRUE)),
# c(sum(!is.na(summary_journal_year_wide$`1992`)),
# sum(!is.na(summary_journal_year_wide$`2021`))),
# c(mean(summary_journal_year_wide$`1992`,na.rm = TRUE),
# mean(summary_journal_year_wide$`2021`,na.rm = TRUE)))

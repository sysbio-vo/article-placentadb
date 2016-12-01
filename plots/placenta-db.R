# install.packages("ggrepel")

library(ggplot2)
library(cowplot)
library(ggfortify)
library(ggrepel)

race <- read.table("race.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
diagnosis <- read.table("diagnosis.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)


## calculate percentage 

# Race
race$percentage = round(race$value / sum(race$value) * 100, 2)
race = race[rev(order(race$percentage)), ]
race$ymax = cumsum(race$percentage)
race$ymin = c(0, head(race$ymax, n = -1))
race

# Diagnosis
diagnosis$percentage = round(diagnosis$value / sum(diagnosis$value) * 100, 2)
diagnosis = diagnosis[rev(order(diagnosis$percentage)), ]
diagnosis$ymax = cumsum(diagnosis$percentage)
diagnosis$ymin = c(0, head(diagnosis$ymax, n = -1))
diagnosis

# Custom theme for the chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    plot.title=element_text(size=14, face="bold"),
    #legend.title = element_text(colour = "black", size = 16, face = "bold"), 
    legend.title = element_blank(),
    legend.text = element_text(family="serif", colour = "black", size = 20),
    legend.position="bottom",
    legend.direction="vertical"
  )

race.pie <- ggplot(race, aes(fill = variable, ymax = ymax, ymin = ymin, xmax = 100, xmin = 60)) +
  geom_rect(colour = "black") +
  coord_polar(theta = "y") + 
  xlim(c(0, 100)) +
  geom_label_repel(aes(label = paste(round(percentage,2),"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, show.legend = F, size = 6)+
  annotate("text", x = 0, y = 0, size = 11, label = "Race") +
  blank_theme + scale_fill_discrete(breaks = race$variable) +
  guides(fill=guide_legend(nrow=length(race$variable),byrow=TRUE))
  
diagnosis.pie <- ggplot(diagnosis, aes(fill = variable, ymax = ymax, ymin = ymin, xmax = 100, xmin = 60)) +
  geom_rect(colour = "black") +
  coord_polar(theta = "y") + 
  xlim(c(0, 100)) +
  geom_label_repel(aes(label = paste(round(percentage,2),"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, show.legend = F, size = 6)+
  annotate("text", x = 0, y = 0, size = 11, label = "Diagnosis") +
  blank_theme + scale_fill_discrete(breaks = diagnosis$variable) +
  guides(fill=guide_legend(nrow=length(diagnosis$variable),byrow=TRUE))


pl <- plot_grid(race.pie, diagnosis.pie, ncol=2, align="hv")

save_plot("plots.pdf", nrow=1,
          base_height=8, base_aspect_ratio = 2.6, pl)

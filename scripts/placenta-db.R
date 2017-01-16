# install.packages("cowplot")

library(ggplot2)
library(cowplot)
library(ggfortify)
library(ggrepel)

race <- read.table("../data/race.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
diagnosis <- read.table("../data/diagnosis.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
gestational_age <- read.table("../data/gestational_age.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
organism_part <- read.table("../data/organism_part.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)


## calculate percentage 

calculatePercentage <- function(table){
    table$percentage = round(table$value / sum(table$value) * 100, 2)
    race = table[rev(order(table$percentage)), ]
    table$ymax = cumsum(table$percentage)
    table$ymin = c(0, head(table$ymax, n = -1))
    table
    return(table)
}


race = calculatePercentage(race)
diagnosis = calculatePercentage(diagnosis)
gestational_age = calculatePercentage(gestational_age)
organism_part =  calculatePercentage(organism_part)


# Custom theme for the chart
blankTheme <- theme_minimal()+
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
        legend.text = element_text(family="sans", colour = "black", size = 25),
        legend.position="bottom",
        legend.direction="vertical"
  )

drawPie <- function(table, label){
    pie <- ggplot(table, aes(fill = variable, ymax = ymax, ymin = ymin, xmax = 100, xmin = 60)) +
        geom_rect(colour = "black") +
        coord_polar(theta = "y") + 
        xlim(c(0, 100)) +
        geom_label_repel(aes(label = paste(round(percentage,2),"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, show.legend = F, size = 6)+
        annotate("text", x = 0, y = 0, size = 11, label = label) +
        blankTheme + scale_fill_discrete(breaks = table$variable) +
        guides(fill=guide_legend(nrow=length(table$variable),byrow=TRUE)
    )

    return(pie)
}

# race.pie = drawPie(race, "Race")

diagnosis.pie = drawPie(diagnosis, "Diagnosis")
gestational_age.pie = drawPie(gestational_age, "Gestational\nAge")
organism_part.pie = drawPie(organism_part, "Organism\nPart")
#race.pie = drawPie(race, "Race")

pl <- plot_grid(diagnosis.pie, gestational_age.pie, organism_part.pie, ncol=3, nrow=1, align="hv")

save_plot("../plots/plots.pdf", pl, base_height=9, base_aspect_ratio = 2.3, nrow=1)
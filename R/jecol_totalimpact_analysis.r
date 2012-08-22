setwd("/Users/ScottMac/github/SChamberlain/bes_altmetrics/data") # set to path to directory with csv file
out <- read.csv("finaldata.csv")[,-1]
head(out); str(out)

# Summarize data and plot results
library(ggplot2)
library(plyr)
out <- ddply(out, .(DOI, metric), summarise,  # remove duplicate entries
		value = mean(value),
		year = mean(year),
		journal2 = unique(journal2)
)

# out$value <- log10(out$value) # log10 transform values
out_ <- ddply(out, .(journal2, metric), summarise, # summarise by journal and metric
    avgmet = mean(value, na.rm=T),              
    semet = sd(value, na.rm=T)/sqrt(na.omit(length(value))) 
)

# Mean value of each metric by journal, across years and papers
size <- 5
dodge <- position_dodge(width=0.3)
limits <- aes(ymax = avgmet + semet, ymin = avgmet - semet)
ggplot(out_, aes(journal2, avgmet, colour=metric)) +
  geom_point(size=size, position=dodge, alpha=0.6) +
  geom_errorbar(limits, width=0.05, position=dodge) +
  labs(x = "", y = "Mean value of metric") +
  theme_bw(base_size=20) +
  opts(axis.text.x=theme_text(angle=45, size=20))
ggsave("meanbyjournalmetric_all.png")

# same plot as above without mendeley_readers to make other data more clear
ggplot(out_[!out_$metric %in% "Mendeley_readers",], aes(journal2, avgmet, colour=metric)) +
	geom_point(size=size, position=dodge, alpha=0.6) +
	geom_errorbar(limits, width=0.05, position=dodge) +
	labs(x = "", y = "Mean value of metric") +
	theme_bw(base_size=20) +
	opts(axis.text.x=theme_text(angle=45, size=20))
ggsave("meanbyjournalmetric_short.png")

# Plot of value of metrics over time for each metric type and journal
out_2 <- ddply(out, .(journal2, year, metric), summarise, # summarise by journal, year, and metric
		avgmet = mean(value, na.rm=T)
)

ggplot(out_2, aes(year, avgmet, colour=journal2)) +
	geom_line() +
	theme_bw(base_size=16) +
	labs(y = "Mean value of metric") + 
	opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) +
	facet_wrap(~ metric, scales='free')
ggsave("metricsbyyear.png")
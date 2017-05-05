library(readr)
library(dplyr)
library(ggplot2)
library(extrafont)


# import Data
prev <- read_csv("./data/asd_prevalence_estimates.csv")

# path for final figure PDF
sink_fig <- "./figs/fig020_asd_prevalence_bw.pdf"

# create discrete variable for scale_fill_manual
prev <- prev %>%
  mutate(quartile = ntile(pop, 4))

# sort using base R
prev$study <- factor(prev$study, 
                    levels = prev$study[order(prev$year)])

#Discrete Grayscale by Population IQR with Cumulative Mean
fig020 <- ggplot(data = prev, aes(x = true_prev, y = study, group = 1))
fig020 <- fig020 + geom_errorbarh(aes(xmin=l95, xmax=u95), height=0,size=.4, na.rm=TRUE)
fig020 <- fig020 + geom_point(aes(fill=factor(quartile)), size=3, shape=21, colour="black", alpha=1, na.rm=TRUE) 
fig020 <- fig020 + geom_path(aes(x = cum_mean, y = order), linetype="dotted") 
fig020 <- fig020 + scale_y_discrete(limits = rev(levels(prev$study)), name = "")
fig020 <- fig020 + scale_fill_manual(limits= c(1:4),
                                   breaks= c(1:4),
                                   values= c("#FFFFFF", "#d9d9d9", "#969696", "#000000"),
                                   labels= c("First Quartile", "Second Quartile", 
                                             "Third Quartile", "Fourth Quartile"),
                                   name="Population \nQuartile Range")
fig020 <- fig020 + scale_x_continuous(breaks= seq(0,300, by=50))
fig020 <- fig020 + labs(x=NULL, y=NULL,
                      title="ASD Prevalence Estimates Since 2000 (per 10,000)",
                      subtitle="(with 95% Confidence Intervals)")
fig020 <- fig020 + theme_minimal(base_family="Lato")
fig020 <- fig020 + theme(panel.grid=element_line())
fig020 <- fig020 + theme(panel.grid.major.y=element_line(color="#bdbdbd", size=0.15)) 
fig020 <- fig020 + theme(panel.grid.major.x=element_line(color="#d9d9d9", size=0.15))
fig020 <- fig020 + theme(panel.grid.minor.x=element_blank()) 
fig020 <- fig020 + theme(panel.grid.minor.y=element_blank())
fig020 <- fig020 + theme(axis.line=element_line())
fig020 <- fig020 + theme(axis.line.x=element_blank()) # if wanted, (color="#2b2b2b", size=0.15)) 
fig020 <- fig020 + theme(axis.line.y=element_blank()) #if wanted, (color="#2b2b2b", size=0.15))
fig020 <- fig020 + theme(axis.ticks=element_line()) 
fig020 <- fig020 + theme(axis.ticks.x=element_line(color="#bdbdbd", size=0.15)) 
fig020 <- fig020 + theme(axis.ticks.y=element_line(color="#bdbdbd", size=0.15)) 
fig020 <- fig020 + theme(axis.ticks.length = unit(.25, "cm"))
fig020 <- fig020 + theme(axis.text.y=element_text(margin=margin(r = 2)))
fig020 <- fig020 + theme(plot.title=element_text(family="Lato")) 
fig020 <- fig020 + theme(plot.subtitle=element_text(family="Lato"))
fig020 <- fig020 + theme(plot.caption=element_text(size=11, hjust=0))




# save as pdf
ggsave(fig020, file = sink_fig, height=9, width=12, dpi = 600)

# embed font to keep them for pdf
embed_fonts(sink_fig)




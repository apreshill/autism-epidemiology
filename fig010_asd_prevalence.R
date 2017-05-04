library(readr)
library(dplyr)
library(ggplot2)
library(extrafont)


# import Data
prev <- read_csv("./data/asd_prevalence_estimates.csv")

# path for final figure PDF
sink_fig <- "./figs/fig010_asd_prevalence_estimates.pdf"

# create discrete variable for scale_fill_manual
prev <- prev %>%
  mutate(quartile = ntile(pop, 4))

# sort using base R
prev$study <- factor(prev$study, 
                    levels = prev$study[order(prev$year)])

#Discrete Grayscale by Population IQR with Cumulative Mean
fig_prev_bw <- ggplot(data = prev, aes(x = true_prev, y = study, group = 1))
fig_prev_bw <- fig_prev_bw + geom_errorbarh(aes(xmin=l95, xmax=u95), height=0,size=.4, na.rm=TRUE)
fig_prev_bw <- fig_prev_bw + geom_point(aes(fill=factor(quartile)), size=3, shape=21, colour="black", alpha=1, na.rm=TRUE) 
fig_prev_bw <- fig_prev_bw + geom_path(aes(x = cum_mean, y = order), linetype="dotted") 
fig_prev_bw <- fig_prev_bw + scale_y_discrete(limits = rev(levels(prev$study)), name = "")
fig_prev_bw <- fig_prev_bw + scale_fill_manual(limits= c(1:4),
                                   breaks= c(1:4),
                                   values= c("#FFFFFF", "#d9d9d9", "#969696", "#000000"),
                                   labels= c("First Quartile", "Second Quartile", 
                                             "Third Quartile", "Fourth Quartile"),
                                   name="Population \nQuartile Range")
fig_prev_bw <- fig_prev_bw + scale_x_continuous(breaks= seq(0,300, by=50))
fig_prev_bw <- fig_prev_bw + labs(x=NULL, y=NULL,
                      title="ASD Prevalence Estimates Since 2000 (per 10,000)",
                      subtitle="(with 95% Confidence Intervals)")
fig_prev_bw <- fig_prev_bw + theme_minimal(base_family="Lato")
fig_prev_bw <- fig_prev_bw + theme(panel.grid=element_line(),
                       panel.grid.major.y=element_line(color="#bdbdbd", size=0.15), 
                       panel.grid.major.x=element_line(color="#d9d9d9", size=0.15), 
                       panel.grid.minor.x=element_blank(), 
                       panel.grid.minor.y=element_blank(), 
                       axis.line=element_line(), 
                       axis.line.x=element_blank(), # if wanted, (color="#2b2b2b", size=0.15)) 
                       axis.line.y=element_blank(), #if wanted, (color="#2b2b2b", size=0.15))
                       axis.ticks=element_line(), 
                       axis.ticks.x=element_line(color="#bdbdbd", size=0.15), 
                       axis.ticks.y=element_line(color="#bdbdbd", size=0.15), 
                       axis.ticks.length = unit(.25, "cm"), 
                       axis.text.y=element_text(margin=margin(r= 2)),
                       plot.title=element_text(family="Lato"), 
                       plot.subtitle=element_text(family="Lato"),
                       plot.caption=element_text(size=11, hjust=0))




# save as pdf
ggsave(fig_prev_bw, file = sink_fig, height=9, width=12, dpi = 600)

# embed font to keep them for pdf
embed_fonts(sink_fig)




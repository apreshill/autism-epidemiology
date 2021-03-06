library(readr)
library(dplyr)
library(ggplot2)
library(extrafont)


# import Data
prev <- read_csv("./data/asd_prevalence_estimates.csv")

# path for final figure PDF
sink_fig <- "./figs/fig021_asd_prevalence_color.pdf"

# create discrete variable for scale_fill_manual
prev <- prev %>%
  mutate(quartile = ntile(pop, 4))

# sort using base R
prev$study <- factor(prev$study, 
                     levels = prev$study[order(prev$year)])

# add region based on Elsabbagh et al. (2012) regions
europe <- c("UK", "Sweden", "Denmark", "Netherlands", "Norway", "France", "Israel", "Iceland", "Poland")
north_america <- c("USA", "Canada", "Mexico", "Venezuela")
west_pacific <- c("Australia", "China", "Japan", "S. Korea", "Taiwan")                 
east_med <- c("Oman", "Iran")

prev <- prev %>% 
  mutate(region = ifelse(country %in% europe, "Europe",
                         ifelse(country %in% north_america, "North America",
                                ifelse(country %in% west_pacific, "West Pacific",
                                       ifelse(country %in% east_med, "East Mediterranean", NA)))))

#Discrete Grayscale by Population IQR with Cumulative Mean
cols <- c("#99FFFF", "#6ACC65","#B47CC7", "#0000CC")
fig021 <- ggplot(data = prev, aes(x = true_prev, y = study, group = 1))
fig021 <- fig021 + geom_errorbarh(aes(xmin = l95, xmax = u95), height = 0, size = .4, na.rm = TRUE)
fig021 <- fig021 + geom_point(aes(fill = factor(region)), size = 3, shape = 21, colour = "black", alpha = 1, na.rm = TRUE) 
fig021 <- fig021 + geom_path(aes(x = cum_mean, y = order), linetype = "dotted") 
fig021 <- fig021 + scale_y_discrete(limits = rev(levels(prev$study)), name = "")
fig021 <- fig021 + scale_fill_manual(values = cols, name="Region")
fig021 <- fig021 + scale_x_continuous(breaks= seq(0,300, by=50))
fig021 <- fig021 + labs(x=NULL, y=NULL,
                                  title="ASD Prevalence Estimates Since 2000 (per 10,000)",
                                  subtitle="(with 95% Confidence Intervals)")
fig021 <- fig021 + theme_minimal(base_family="Lato")
fig021 <- fig021 + theme(panel.grid=element_line())
fig021 <- fig021 + theme(panel.grid.major.y=element_line(color="#bdbdbd", size=0.15)) 
fig021 <- fig021 + theme(panel.grid.major.x=element_line(color="#d9d9d9", size=0.15))
fig021 <- fig021 + theme(panel.grid.minor.x=element_blank()) 
fig021 <- fig021 + theme(panel.grid.minor.y=element_blank())
fig021 <- fig021 + theme(axis.line=element_line())
fig021 <- fig021 + theme(axis.line.x=element_blank()) # if wanted, (color="#2b2b2b", size=0.15)) 
fig021 <- fig021 + theme(axis.line.y=element_blank()) #if wanted, (color="#2b2b2b", size=0.15))
fig021 <- fig021 + theme(axis.ticks=element_line()) 
fig021 <- fig021 + theme(axis.ticks.x=element_line(color="#bdbdbd", size=0.15)) 
fig021 <- fig021 + theme(axis.ticks.y=element_line(color="#bdbdbd", size=0.15)) 
fig021 <- fig021 + theme(axis.ticks.length = unit(.25, "cm"))
fig021 <- fig021 + theme(axis.text.y=element_text(margin=margin(r = 2)))
fig021 <- fig021 + theme(plot.title=element_text(family="Lato")) 
fig021 <- fig021 + theme(plot.subtitle=element_text(family="Lato"))
fig021 <- fig021 + theme(plot.caption=element_text(size=11, hjust=0))




# save as pdf
ggsave(fig021, file = sink_fig, height=9, width=12, dpi = 600)

# embed font to keep them for pdf
embed_fonts(sink_fig)

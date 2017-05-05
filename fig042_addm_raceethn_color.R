# This is another figure based on CDC ADDM data, made only in color for presentations.
# Caption: Estimated prevalence of ASDs (with 95% confidence intervals) 
# among children aged 8 years in the United States by race/ethnicity, ADDM site, 
# and type of records access across four ADDM survey years. 
# Data shown only for sites included in at least three of the four 
# most recent survey years: 2006 (CDC, 2010), 2008 (CDC, 2012), 
# 2010 (CDC, 2014), and 2012 (CDC, 2016).
# Thus, ADDM data from Florida and Pennsylvania sites are excluded.

# load packages
library(readr)
library(dplyr)
library(ggplot2)
library(extrafont)
library(tidyr)
library(ggthemes)

# source file path
source_addm <- "./data/addm-since-2006.csv"

# saved final PDF
sink_fig <- "./figs/fig042_addm_raceethn_color.pdf"

# load data files & keep var names using readr::read_csv
addm <- read_csv(source_addm, col_names = TRUE) 

# filter sites 
race_eth <- c("white", "black", "hispanic")
time_by_reth <- addm %>%
  filter(!(site %in% c("Total", "Florida", "Pennsylvania")), 
         strata %in% race_eth) %>%
  mutate(for_line = as.factor(for_line))

time_by_reth %>%
  group_by(strata, year_survey) %>%
  tally()

# reorder factor
levels(time_by_reth$strata) <- c("White", "Black", "Hispanic")

# color version of plot for presentations
cols <- c("#C4AD66", "#77BEDB","#D65F5F","#B47CC7")
fig042 <- ggplot(time_by_reth) 
fig042 <- fig042 + geom_line(aes(group = interaction(strata, for_line), x = year_survey, y = prev_per_1k, colour = strata, alpha = data_source, size = data_source)) 
fig042 <- fig042 + geom_point(aes(x = year_survey, y = prev_per_1k, colour = strata, shape = data_source, fill = data_source)) 
fig042 <- fig042 + facet_wrap(~ site) 
fig042 <- fig042 + scale_x_continuous(breaks = c(2006, 2008, 2010, 2012), name = "Survey Year") 
fig042 <- fig042 + scale_color_manual(values = cols, name = "Race/Ethnicity", breaks = c("white", "black", "hispanic"), labels = c("White", "Black", "Hispanic")) 
fig042 <- fig042 + scale_fill_manual(values = c(NA, "white"), guide=FALSE) #fill points conditionally
fig042 <- fig042 + scale_alpha_manual(values = c(1, .6), name = "Primary Record Source", labels = c("Educational + Health", "Health only"))
fig042 <- fig042 + scale_size_manual(values = c(.5, .75), guide = FALSE)
fig042 <- fig042 + scale_shape_manual(values = c(16, 21), name = "Primary Record Source", labels = c("Educational + Health", "Health only")) # change shapes
fig042 <- fig042 + expand_limits(x = c(2005, 2013), y = 0) 
fig042 <- fig042 + scale_y_continuous(name = "Prevalence per 1000 children") 
fig042 <- fig042 + theme_hc(base_family = "Lato")  
fig042 <- fig042 + theme(panel.grid.major.y=element_line(size=.2))
fig042 <- fig042 + theme(axis.ticks.y = element_blank())
fig042 <- fig042 + theme(axis.ticks.length = unit(.1, "cm"))
fig042 <- fig042 + labs(title = "Prevalence of Autism Spectrum Disorder Among Children Aged 8 Years in the US by Race/Ethnicity, 2006-2012")
fig042 <- fig042 + labs(subtitle = "Content source: CDC Autism and Developmental Disabilities Monitoring Network")
#fig042 <- fig042 + labs(caption = "Alison Presmanes Hill @apreshill")
fig042 <- fig042 + theme(plot.title=element_text(face="bold", size=8, margin=margin(b=6)))
fig042 <- fig042 + theme(plot.subtitle = element_text(size=8))
fig042 <- fig042 + theme(plot.caption = element_text(size=5, face = "italic", colour = "darkgray"))
fig042 <- fig042 + theme(legend.text = element_text(margin=margin(r=-3), size=8))
fig042 <- fig042 + theme(legend.title = element_text(margin=margin(r=-3), size=8))
fig042 <- fig042 + theme(axis.title = element_text(margin=margin(r=-3), size=8))
fig042 <- fig042 + theme(axis.text = element_text(margin=margin(r=-3), size=6))
fig042 <- fig042 + theme(legend.key.height = unit(.1, "line"))
fig042 <- fig042 + theme(legend.box = "horizontal")
fig042 <- fig042 + guides(color=guide_legend(nrow=3,byrow=TRUE, title.position = "top", order = 1))
fig042 <- fig042 + guides(alpha=guide_legend(nrow=2,byrow=TRUE, title.position = "top", override.aes = list(fill = "white")))
fig042 <- fig042 + theme(strip.text = element_text(margin=margin(t = 0, b = 0), lineheight = .01, size = 8))
fig042

# Saving 7.64 x 5.03 in image
ggsave(fig042, file = sink_fig, width = 8, height = 5.8, dpi = 600)

# embed font to keep them for pdf
embed_fonts(sink_fig)
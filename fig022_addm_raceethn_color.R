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
sink_fig <- "./figs/fig022_addm_raceethn_color.pdf"

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
reth <- ggplot(time_by_reth) 
reth <- reth + geom_line(aes(group = interaction(strata, for_line), x = year_survey, y = prev_per_1k, colour = strata, alpha = data_source, size = data_source)) 
reth <- reth + geom_point(aes(x = year_survey, y = prev_per_1k, colour = strata, shape = data_source, fill = data_source)) 
reth <- reth + facet_wrap(~ site) 
reth <- reth + scale_x_continuous(breaks = c(2006, 2008, 2010, 2012), name = "Survey Year") 
reth <- reth + scale_color_manual(values = cols, name = "Race/Ethnicity", breaks = c("white", "black", "hispanic"), labels = c("White", "Black", "Hispanic")) 
reth <- reth + scale_fill_manual(values = c(NA, "white"), guide=FALSE) #fill points conditionally
reth <- reth + scale_alpha_manual(values = c(1, .6), name = "Primary Record Source", labels = c("Educational + Health", "Health only"))
reth <- reth + scale_size_manual(values = c(.5, .75), guide = FALSE)
reth <- reth + scale_shape_manual(values = c(16, 21), name = "Primary Record Source", labels = c("Educational + Health", "Health only")) # change shapes
reth <- reth + expand_limits(x = c(2005, 2013), y = 0) 
reth <- reth + scale_y_continuous(name = "Prevalence per 1000 children") 
reth <- reth + theme_hc(base_family = "Lato")  
reth <- reth + theme(panel.grid.major.y=element_line(size=.2))
reth <- reth + theme(axis.ticks.y = element_blank())
reth <- reth + theme(axis.ticks.length = unit(.1, "cm"))
reth <- reth + labs(title = "Prevalence of Autism Spectrum Disorder Among Children Aged 8 Years in the US by Race/Ethnicity, 2006-2012")
reth <- reth + labs(subtitle = "Content source: CDC Autism and Developmental Disabilities Monitoring Network")
#reth <- reth + labs(caption = "Alison Presmanes Hill @apreshill")
reth <- reth + theme(plot.title=element_text(face="bold", size=8, margin=margin(b=6)))
reth <- reth + theme(plot.subtitle = element_text(size=8))
reth <- reth + theme(plot.caption = element_text(size=5, face = "italic", colour = "darkgray"))
reth <- reth + theme(legend.text = element_text(margin=margin(r=-3), size=8))
reth <- reth + theme(legend.title = element_text(margin=margin(r=-3), size=8))
reth <- reth + theme(axis.title = element_text(margin=margin(r=-3), size=8))
reth <- reth + theme(axis.text = element_text(margin=margin(r=-3), size=6))
reth <- reth + theme(legend.key.height = unit(.1, "line"))
reth <- reth + theme(legend.box = "horizontal")
reth <- reth + guides(color=guide_legend(nrow=3,byrow=TRUE, title.position = "top", order = 1))
reth <- reth + guides(alpha=guide_legend(nrow=2,byrow=TRUE, title.position = "top", override.aes = list(fill = "white")))
reth <- reth + theme(strip.text = element_text(margin=margin(t = 0, b = 0), lineheight = .01, size = 8))
reth

# Saving 7.64 x 5.03 in image
ggsave(reth, file = sink_fig, width = 8, height = 5.8, dpi = 600)

# embed font to keep them for pdf
embed_fonts(sink_fig)
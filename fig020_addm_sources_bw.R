# This is figure 2 from our book chapters.
# Caption: Estimated prevalence of ASDs (with 95% confidence intervals) 
# among children aged 8 years in the United States by ADDM site 
# and type of fig020ords access across four ADDM survey years. 
# Data shown only for sites included in at least three of the four 
# most fig020ent survey years: 2006 (CDC, 2010), 2008 (CDC, 2012), 
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
sink_fig <- "./figs/fig020_addm_source_bw.pdf"

# load data files & keep var names using readr::read_csv
addm <- read_csv(source_addm, col_names = TRUE) 

# filter sites 
addm <- addm %>%
  filter(!(site %in% c("Total", "Florida", "Pennsylvania")), 
         strata == "overall")

# black-and-white version of plot for print
cols <- c("black", "white")
fig020 <- ggplot(addm) 
fig020 <- fig020 + geom_line(aes(group = for_line, x = year_survey, y = prev_per_1k, lty = data_source), lwd = .4) 
fig020 <- fig020 + geom_linerange(aes(x = year_survey, y = prev_per_1k, ymin = l95_per_1k, ymax = u95_per_1k), size = .5, alpha = .8) 
fig020 <- fig020 + geom_point(aes(x = year_survey, y = prev_per_1k, fill = data_source), pch = 21, size = 1.5)
fig020 <- fig020 + facet_wrap(~ site) 
fig020 <- fig020 + scale_x_continuous(breaks = c(2006, 2008, 2010, 2012), name = "Survey Year") 
fig020 <- fig020 + scale_linetype_manual(values=c("solid", "dotted"), guide = FALSE) 
fig020 <- fig020 + scale_fill_manual(values = cols, name = "Primary Record Source", labels = c("Educational + Health", "Health only")) 
fig020 <- fig020 + expand_limits(x = c(2005, 2013), y = 0) 
fig020 <- fig020 + scale_y_continuous(name = "Prevalence per 1000 children (95% CI)") 
fig020 <- fig020 + theme_hc(base_family = "Lato")  
fig020 <- fig020 + theme(panel.grid.major.y=element_line(size=.25))
fig020 <- fig020 + theme(axis.ticks.y = element_blank())
fig020 <- fig020 + theme(axis.ticks.length = unit(.1, "cm"))
fig020 <- fig020 + labs(title = "Prevalence of Autism Spectrum Disorder Among Children Aged 8 Years in the US by Record Source, 2006-2012")
fig020 <- fig020 + labs(subtitle = "Content source: CDC Autism and Developmental Disabilities Monitoring Network")
#fig020 <- fig020 + labs(caption = "Alison Presmanes Hill @apreshill")
fig020 <- fig020 + theme(plot.title=element_text(face="bold", size=9, margin=margin(b=6)))
fig020 <- fig020 + theme(plot.subtitle = element_text(size=8))
fig020 <- fig020 + theme(plot.caption = element_text(size=5, face = "italic", color = "darkgray"))
fig020 <- fig020 + theme(legend.text = element_text(margin=margin(r=-3), size=8))
fig020 <- fig020 + theme(legend.title = element_text(margin=margin(r=-3), size=8))
fig020 <- fig020 + theme(axis.title = element_text(margin=margin(r=-3), size=8))
fig020 <- fig020 + theme(axis.text = element_text(margin=margin(r=-3), size=6))
fig020 <- fig020 + theme(strip.text = element_text(margin=margin(t = 0, b = 0), lineheight = .01, size = 8))
fig020



# save as pdf
ggsave(fig020, file = sink_fig, width = 8, height = 5.8, dpi = 600)

# embed font to keep them for pdf
embed_fonts(sink_fig)


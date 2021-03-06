# This is figure 4 from our book chapters, but in color for use in presentations.
# Caption: Estimated prevalence of ASDs (with 95% confidence intervals) 
# among children aged 8 years in the United States by ADDM site 
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


# source file paths
source_cdc <- "./data/addm-since-2006.csv"

# output figure files
rec_pdf <- "./figs/fig041_addm_sources_color.pdf"
rec_jpg <- "./figs/fig041_addm_sources_color.jpg"

# load data files & keep var names
cdc <- read_csv(source_cdc, col_names = TRUE) # readr

# filter sites 
time_by_rec <- cdc %>%
  filter(!(site %in% c("Total", "Florida", "Pennsylvania")), strata == "overall")

time_by_rec %>%
  group_by(year_survey) %>%
  tally()


cols <- c("#4878CF", "#6ACC65")
fig041 <- ggplot(time_by_rec) 
fig041 <- fig041 + geom_line(aes(group = for_line, x = year_survey, y = prev_per_1k, colour = data_source), lty = 3, lwd = .4) 
fig041 <- fig041 + geom_point(aes(x = year_survey, y = prev_per_1k, colour = data_source), size = 1.5) 
fig041 <- fig041 + geom_linerange(aes(x = year_survey, y = prev_per_1k, ymin = l95_per_1k, ymax = u95_per_1k,  colour = data_source), size = .5, alpha = .9) 
fig041 <- fig041 + facet_wrap(~ site) 
fig041 <- fig041 + scale_x_continuous(breaks = c(2006, 2008, 2010, 2012), name = "Survey Year") 
fig041 <- fig041 + scale_color_manual(values = cols, name = "Primary Record Source", labels = c("Educational + Health", "Health only")) 
fig041 <- fig041 + expand_limits(x = c(2005, 2013), y = 0) 
fig041 <- fig041 + scale_y_continuous(name = "Prevalence per 1000 children (95% CI)") 
fig041 <- fig041 + theme_hc(base_family = "Lato")  
fig041 <- fig041 + theme(panel.grid.major.y=element_line(size=.25))
fig041 <- fig041 + theme(axis.ticks.y = element_blank())
fig041 <- fig041 + theme(axis.ticks.length = unit(.1, "cm"))
fig041 <- fig041 + labs(title = "Prevalence of Autism Spectrum Disorder Among Children Aged 8 Years in the US by Record Source, 2006-2012")
fig041 <- fig041 + labs(subtitle = "Content source: CDC Autism and Developmental Disabilities Monitoring Network")
#fig041 <- fig041 + labs(caption = "Alison Presmanes Hill @apreshill")
fig041 <- fig041 + theme(plot.title=element_text(face="bold", size=9, margin=margin(b=6)))
fig041 <- fig041 + theme(plot.subtitle = element_text(size=8))
fig041 <- fig041 + theme(plot.caption = element_text(size=5, face = "italic", color = "darkgray"))
fig041 <- fig041 + theme(legend.text = element_text(margin=margin(r=-3), size=8))
fig041 <- fig041 + theme(legend.title = element_text(margin=margin(r=-3), size=8))
fig041 <- fig041 + theme(axis.title = element_text(margin=margin(r=-3), size=8))
fig041 <- fig041 + theme(axis.text = element_text(margin=margin(r=-3), size=6))
fig041 <- fig041 + theme(strip.text = element_text(margin=margin(t = 0, b = 0), lineheight = .01, size = 8))
fig041

# Saving 7.64 x 5.79 in image
ggsave(fig041, file = rec_pdf, width = 8, height = 5.8, dpi = 600)
embed_fonts(rec_pdf, outfile = rec_pdf)
ggsave(fig041, file = rec_jpg, width = 8, height = 5.8, dpi = 600)

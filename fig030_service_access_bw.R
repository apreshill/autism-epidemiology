# This is figure 3 from our book chapters.
# Caption: Assuming a constant incidence and prevalence of 100/10,000 
# between Time 1 and Time 2 (meaning there is no “epidemic”), 
# prevalence estimates that rely solely on service access counts 
# not only underestimate the true prevalence but may also create 
# the illusion of rising prevalence over time (see discussion in text).

# Thanks to Jeff Harrison on HelpMeViz.com for this figure! 
# You can see the original figure here: http://helpmeviz.com/2015/05/06/combination-chart/

# load packages
library(readr)
library(dplyr)
library(ggplot2)
library(extrafont)
library(tidyr)
library(ggthemes)

# saved final PDF
sink_fig <- "./figs/fig030_service_access_bw.pdf"


#create hypothetical data for figure
set.seed(1000)
time1 <- sample(1:105, 105, replace=F)
time2 <- time1
all_3 <- as.data.frame(cbind(time1, time2))

#reshape the fig030 data to looooong
fig030 <- all_3 %>%
  gather(time, estimate, time1:time2) %>%
  mutate(access = ifelse(time == "time1" & estimate <= 30, 1, 
                         ifelse(time == "time1" & estimate > 30, 0, 
                                ifelse(time == "time2" & estimate <= 60, 1, 0))))

# initialize figure
fig030 <- ggplot(fig030, aes(x = factor(time), y = estimate))

# adding jittered points
fig030 <- fig030 + geom_jitter(aes(fill = factor(access)), 
              position=position_jitter(width=.25, height = 0), 
              pch = 21,
              colour = "black",
              size = 2) 

# formatting axes, plot area, + colors
fig030 <- fig030 + scale_x_discrete(name = "", labels = c("Time 1", "Time 2")) 
fig030 <- fig030 + scale_y_continuous(name = "ASD Cases per 10,000") 
fig030 <- fig030 + coord_cartesian(ylim = c(0,120), xlim = c(.6, 4)) 
fig030 <- fig030 + scale_fill_manual(name = "ASD cases who are:", 
                    values = c("black", "white"), 
                    labels = c("Not accessing services", 
                               "Accessing services"))  

# adding horizontal line for true prevalence
fig030 <- fig030 + geom_segment(aes(x = .6, xend = 2.5, y = 105, yend = 105), 
               lty = 3, lwd = .5, colour = "black") 

# creating jagged line segments
fig030 <- fig030 + geom_segment(aes(x = .6, xend = 1.2, y = 30, yend = 30), 
               lty = 3, lwd = .5, colour = "black") 
fig030 <- fig030 + geom_segment(aes(x = 1.2, xend = 1.8, y = 30, yend = 60), 
               lty = 3, lwd = .5, colour = "black") 
fig030 <- fig030 + geom_segment(aes(x = 1.8, xend = 2.5, y = 60, yend = 60), 
               lty = 3, lwd = .5, colour = "black") 
  
# adding right text
fig030 <- fig030 + annotate("text", 
           label = "Estimates of prevalence based\non population sampling will remain\nstable over time if true prevalence\nis stable.", 
           x = 2.6, y = 105, size = 4, hjust = 0, family = "Lato") 
fig030 <- fig030 + annotate("text", x = 2.6, y = 60, 
           label = "Estimates of prevalence based\non individuals accessing services\ncan create an illusion of an\nincrease in prevalence over time,\nyet still underestimate prevalence\nat both time points.", 
           size=4, hjust=0, family = "Lato")  
  
# more formatting
fig030 <- fig030 + guides(fill = guide_legend(keywidth = 1.5, keyheight = 1.5))
fig030 <- fig030 + theme_bw(base_family = "Lato") 
fig030 <- fig030 + theme(axis.ticks = element_blank())
fig030 <- fig030 + theme(panel.border = element_blank()) 
fig030 <- fig030 + theme(axis.line = element_blank())
fig030 <- fig030 + theme(panel.grid.major = element_blank())
fig030 <- fig030 + theme(panel.grid.minor = element_blank())
fig030 <- fig030 + theme(legend.position=c(.7,.2))
fig030 <- fig030 + theme(legend.text = element_text(size = 10))
fig030 <- fig030 + theme(legend.title = element_text(size = 10))
fig030 <- fig030 + theme(legend.background = element_rect(fill="gray90", size=.25, 
                                         linetype="dotted"))
fig030 <- fig030 + theme(axis.title.y = element_text(size=10))
fig030 <- fig030 + theme(axis.text = element_text(size=10))


ggsave(fig030, file = sink_fig, width = 7, height = 6, dpi = 600)

# embed font to keep them for pdf
embed_fonts(sink_fig)
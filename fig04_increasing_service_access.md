# Figure 4
Alison Presmanes Hill  



<style>
body {
    font-family: "Cabin", sans-serif;
}
p {
    font-family: "Cabin", sans-serif;
}
</style>

<a href="mailto:hillali@ohsu.edu"><i class="fa fa-envelope fa-fw"></i>&nbsp; hillali@ohsu.edu</a><br>





```r
#create hypothetical data for fig3
set.seed(1000)
time1 <- sample(1:105, 105, replace=F)
time2 <- time1
all_3 <- as.data.frame(cbind(time1, time2))

#reshape the fig3 data to looooong
fig3 <- all_3 %>%
  gather(time, estimate, time1:time2) %>%
  mutate(access = ifelse(time == "time1" & estimate <= 30, 1, 
                         ifelse(time == "time1" & estimate > 30, 0, 
                                ifelse(time == "time2" & estimate <= 60, 1, 0))))

#final fig3
fig3_dots <- ggplot(fig3, aes(x = factor(time), 
                              y = estimate)) +
  
  # adding jittered points
  geom_jitter(aes(fill = factor(access)), 
              position=position_jitter(width=.2, height = 0), 
              pch = 21,
              colour = "black",
              alpha = .75,
              size = 2) +
  
  # formatting axes, plot area, + colors
  scale_x_discrete(name = "", labels = c("Time 1", "Time 2")) +
  scale_y_continuous(name = "ASD Cases per 10,000") +
  coord_cartesian(ylim = c(0,120), xlim = c(.6, 4)) +
  scale_fill_manual(name = "ASD cases who are:", 
                     values = c("black", "white"), 
                     labels = c("Not accessing services", 
                                "Accessing services")) + 
  
  # adding horizontal line for true prevalence
  geom_segment(aes(x = .6, xend = 2.5, y = 105, yend = 105), 
               lty = 3, lwd = .5, colour = "black") +
  
  # creating jagged line segments
  geom_segment(aes(x = .6, xend = 1.2, y = 30, yend = 30), 
               lty = 3, lwd = .5, colour = "black") +
  geom_segment(aes(x = 1.2, xend = 1.8, y = 30, yend = 60), 
               lty = 3, lwd = .5, colour = "black") +
  geom_segment(aes(x = 1.8, xend = 2.5, y = 60, yend = 60), 
               lty = 3, lwd = .5, colour = "black") +
  
  # adding right text
  annotate("text", 
           label = "Estimates of prevalence based\non population sampling will remain\nstable over time if true prevalence\nis stable.", 
           x = 2.6, y = 105, size = 7, hjust = 0) +
  annotate("text", x = 2.6, y = 60, 
           label = "Estimates of prevalence based\non individuals accessing services\ncan create an illusion of an\nincrease in prevalence over time,\nyet still underestimate prevalence\nat both time points.", 
           size=7, hjust=0) +
  
  # more formatting
  guides(colour = guide_legend(keywidth = 3, 
                               keyheight = 3, 
                               override.aes = list(alpha = 1))) +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        panel.border=element_blank(), 
        axis.line = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position=c(.7,.2), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.background = element_rect(fill="gray90", size=.25, 
                                         linetype="dotted"), 
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

fig3_dots
```

![](figs/unnamed-chunk-1-1.png)<!-- -->

```r
ggsave(fig3_dots, file = "./figs/fig04_increasing_service_access.pdf", height=9, width=12, dpi = 600)
```


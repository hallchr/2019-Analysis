#2020 Analysis by basin
AllCountsAmbient2020 <- subset(AllCountsAmbient, Year %in% "2020")
AllCountsAmbient2019_2020 <- subset(AllCountsAmbient, Year %in% c("2019", "2020"))


AllCountsAmbient$CurrentYear <- '2010-2019'
AllCountsAmbient$CurrentYear[AllCountsAmbient$Year == '2020'] <- '2020'

AllCountsSUmmer2020 <-subset(AllCountsAmbient2020, Season == "Summer")
AllCountsAmbientSummer <-subset(AllCountsAmbient, Season == "Summer")

BIBISub2020 <-subset(AllCountsAmbient2020, !is.na(BIBI))

Fecalsub <- subset(AllCountsAmbient2020, !is.na(Fecal.Coliform))


#temp by basin by site for 2020

Temp_HC <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Horse Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees Celsius)", x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Temp_LS <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Temp_LNC <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Lower North Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_LSR <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_PC <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Perry Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_UNC <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Upper North Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

#Parr Creek didnt have a summer sample....same for DO
#Temp_PA <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Parr Creek"), aes(Site, TEMP)) + 
# facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
# stat_summary(fun.y= "mean", colour="black", geom="point", 
#              shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
# labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
# theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
# theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
#   axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


tiff("Temperature_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Temp_HC, Temp_LS, Temp_PC, Temp_LNC, Temp_UNC, Temp_LSR, nrow = 1)
dev.off()


tiff("Temperature_By_Basin_small.tiff", units="in", width=11, height=3, res=500)
grid.arrange(Temp_HC, Temp_LS, Temp_PC, Temp_LNC, Temp_UNC, Temp_LSR, nrow = 1)
dev.off()

Temp_Table_Basin <- summaryStats(TEMP ~ Monitoring.Basin, data = AllCountsSUmmer2020, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                                 test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Temp_Table_Basin

write.table(Temp_Table_Basin, file = "Temp_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)
#DO by basin by site for 2019

DO_HC <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Horse Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(8,11) +
  labs(title = NULL, subtitle=NULL, y="Dissolved Oxygen (mg/L)", x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_LS <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Little Swamp Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(8,11) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_LNC <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Lower North Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +  ylim(8,11) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_LSR <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Lower Sammamish River"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +  ylim(8,11) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_PC <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Perry Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +  ylim(8,11) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_UNC <- ggplot(subset(AllCountsSUmmer2020, Monitoring.Basin == "Upper North Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +  ylim(8,11) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("DO_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(DO_HC, DO_LS, DO_PC, DO_LNC, DO_UNC, DO_LSR, nrow = 1)
dev.off()


DO_Table_Basin <- summaryStats(DO ~ Monitoring.Basin, data = AllCountsSUmmer2020, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

DO_Table_Basin

write.table(DO_Table_Basin, file = "DO_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)


#Conductivity

SC_HC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Horse Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,400) +
  labs(title = NULL, subtitle=NULL, y="Specific Conductivity (uS)", x=NULL) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SC_LS <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,400) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SC_LNC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Lower North Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,400) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


SC_LSR <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,400) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


SC_PC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Perry Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,400) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


SC_UNC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Upper North Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,400) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + #geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SC_PA <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Parr Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,400) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_blank(), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")



tiff("SpC_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(SC_HC, SC_LS, SC_PC, SC_LNC, SC_UNC, SC_LSR, SC_PA, nrow = 1)
dev.off()


SpC_Table_Basin <- summaryStats(Specific.Cond ~ Monitoring.Basin, data = AllCountsAmbient2020, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                                test.arg.list=list(var.equal = FALSE, test="nonparametric"))

SpC_Table_Basin

write.table(SpC_Table_Basin, file = "SpC_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#Nutrients 

TP_HC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Horse Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y="Total Phosphorous (mg/L)", x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) +geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_LS <- ggplot(subset(Metals_sub, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_LNC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Lower North Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_LSR <- ggplot(subset(Metals_sub, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_PC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Perry Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_UNC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Upper North Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_PA <- ggplot(subset(Metals_sub, Monitoring.Basin == "Parr Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_blank(), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_HC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Horse Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y="Total Nitrogen (mg/L)") + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Total Nitrogen (mg/L)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_LS <- ggplot(subset(Metals_sub, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_LNC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Lower North Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_LSR <- ggplot(subset(Metals_sub, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_PC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Perry Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_UNC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Upper North Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_PA <- ggplot(subset(Metals_sub, Monitoring.Basin == "Parr Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("Nutrients_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(TP_HC, TP_LS, TP_PC, TP_LNC, TP_UNC, TP_LSR, TN_HC, TN_LS, TN_PC, TN_LNC, TN_UNC, TN_LSR, nrow = 2)

dev.off()


TP_Table_Basin <- summaryStats(TP ~ Monitoring.Basin, data = AllCountsAmbient2020, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

TP_Table_Basin

write.table(TP_Table_Basin, file = "TP_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

TN_Table_Basin <- summaryStats(TN ~ Monitoring.Basin, data = AllCountsAmbient2020, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

TN_Table_Basin

write.table(TN_Table_Basin, file = "TN_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#metals

Metals_sub <- subset(subset(AllCountsAmbient2020, !is.na(Zn)))

Zn_HC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Horse Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y="Zinc (ug/L)", x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_LS <- ggplot(subset(Metals_sub, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_LNC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Lower North Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_LSR <- ggplot(subset(Metals_sub, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_PC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Perry Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_UNC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Upper North Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_PA <- ggplot(subset(Metals_sub, Monitoring.Basin == "Parr Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")



Cu_HC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Horse Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y="Copper (ug/L)") + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Copper (ug/L)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Cu_LS <- ggplot(subset(Metals_sub, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Cu_LNC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Lower North Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18),  axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_LSR <- ggplot(subset(Metals_sub, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18),  axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_PC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Perry Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18),  axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_UNC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Upper North Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18),  axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_PA <- ggplot(subset(Metals_sub, Monitoring.Basin == "Parr Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18),  axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_HC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Horse Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y="Lead (ug/L)", x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Pb_LS <- ggplot(subset(Metals_sub, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Pb_LNC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Lower North Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_LSR <- ggplot(subset(Metals_sub, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_PC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Perry Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_UNC <- ggplot(subset(Metals_sub, Monitoring.Basin == "Upper North Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_PA <- ggplot(subset(Metals_sub, Monitoring.Basin == "Parr Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")



grid.arrange(Zn_HC, Zn_LS, Zn_PC, Zn_LNC, Zn_UNC, Zn_LSR, Cu_HC, Cu_LS, Cu_PC, Cu_LNC, Cu_UNC, Cu_LSR, Pb_HC, Pb_LS, Pb_PC, Pb_LNC, Pb_UNC, Pb_LSR, nrow = 3)

tiff("Metals_By_Basin.tiff", units="in", width=11, height=5, res=500)
grid.arrange(Zn_HC, Zn_LS, Zn_PC, Zn_LNC, Zn_UNC, Zn_LSR, Cu_HC, Cu_LS, Cu_PC, Cu_LNC, Cu_UNC, Cu_LSR, Pb_HC, Pb_LS, Pb_PC, Pb_LNC, Pb_UNC, Pb_LSR, nrow = 3)
dev.off()


Pb_Table_Basin <- summaryStats(Pb ~ Monitoring.Basin, data = AllCountsAmbient2020, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Pb_Table_Basin

write.table(Pb_Table_Basin, file = "Pb_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

Zn_Table_Basin <- summaryStats(Zn ~ Monitoring.Basin, data = AllCountsAmbient2020, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Zn_Table_Basin

write.table(Zn_Table_Basin, file = "Zn_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

Cu_Table_Basin <- summaryStats(Cu ~ Monitoring.Basin, data = AllCountsAmbient2020, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Cu_Table_Basin

write.table(Cu_Table_Basin, file = "Cu_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)



#sediment with fill= precip

Turb_HC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Horse Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,25) +
  labs(title = NULL, subtitle=NULL, y="Turbidity (NTU)", x=NULL) + geom_hline(yintercept = 4, color = "orange2", size = 1) + geom_hline(yintercept = 70, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Turb_LS <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,25) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 4, color = "orange2", size = 1) + geom_hline(yintercept = 70, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Turb_LNC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Lower North Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,25) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 4, color = "orange2", size = 1) + geom_hline(yintercept = 70, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Turb_LSR <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,25) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 4, color = "orange2", size = 1) + geom_hline(yintercept = 70, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Turb_PC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Perry Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,25) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 4, color = "orange2", size = 1) + geom_hline(yintercept = 70, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Turb_UNC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Upper North Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,25) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 4, color = "orange2", size = 1) + geom_hline(yintercept = 70, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")



#Turb_PA <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Parr Creek"), aes(Site, Turbidity)) + 
 # facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
#  stat_summary(fun.y= "mean", colour="black", geom="point", 
 #              shape=18, size=3,show_guide = FALSE) + ylim(0,25) +
 # labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 4, color = "orange2", size = 1) + geom_hline(yintercept = 70, color = "red", size=1) +
 # theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
 # theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
  #                   axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")




TSS_HC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Horse Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,180) +
  labs(title = NULL, subtitle=NULL, y="TSS (mg/L)", x=NULL) + geom_hline(yintercept = 33, color = "orange2", size = 1) +
  labs(title = NULL, subtitle=NULL, y="TSS (mg/L)") +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TSS_LS <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,180) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 33, color = "orange2", size = 1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TSS_LNC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Lower North Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,180) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 33, color = "orange2", size = 1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TSS_LSR <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,180) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 33, color = "orange2", size = 1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TSS_PC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Perry Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,180) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 33, color = "orange2", size = 1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TSS_UNC <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Upper North Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,180) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 33, color = "orange2", size = 1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


#TSS_PA <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Parr Creek"), aes(Site, TSS)) + 
 #facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
 # stat_summary(fun.y= "mean", colour="black", geom="point", 
  #             shape=18, size=3,show_guide = FALSE) + ylim(0,180) +
 # labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 33, color = "orange2", size = 1) +
  #theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
 # theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
      #               axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

grid.arrange(Turb_HC, Turb_LS, Turb_PC, Turb_LNC, Turb_UNC, Turb_LSR, TSS_HC, TSS_LS, TSS_PC, TSS_LNC, TSS_UNC, TSS_LSR, nrow = 2)

tiff("Sediment_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Turb_HC, Turb_LS, Turb_PC, Turb_LNC, Turb_UNC, Turb_LSR, TSS_HC, TSS_LS, TSS_PC, TSS_LNC, TSS_UNC, TSS_LSR, nrow = 2)
dev.off()


Turb_Table_Basin <- summaryStats(Turbidity ~ Monitoring.Basin, data = AllCountsAmbient2020, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                                 test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Turb_Table_Basin

write.table(Turb_Table_Basin, file = "Turb_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

TSS_Table_Basin <- summaryStats(TSS ~ Monitoring.Basin, data = AllCountsAmbient2020, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                                test.arg.list=list(var.equal = FALSE, test="nonparametric"))

TSS_Table_Basin

write.table(TSS_Table_Basin, file = "TSS_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)



# fecals

Fecal_HC <- ggplot(subset(Fecalsub, Monitoring.Basin == "Horse Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform Bacteria (CFU)", x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_LS <- ggplot(subset(Fecalsub, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_LNC <- ggplot(subset(Fecalsub, Monitoring.Basin == "Lower North Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_LSR <- ggplot(subset(Fecalsub, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_PC <- ggplot(subset(Fecalsub, Monitoring.Basin == "Perry Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

#Fecal_PA <- ggplot(subset(AllCountsAmbient2020, Monitoring.Basin == "Parr Creek"), aes(Site, Fecal.Coliform)) + 
 # facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  #stat_summary(fun.y= "mean", colour="black", geom="point", 
   #            shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  #labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  #theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  #theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
   #                  axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_UNC <- ggplot(subset(Fecalsub, Monitoring.Basin == "Upper North Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("FC_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Fecal_HC, Fecal_LS, Fecal_PC, Fecal_LNC, Fecal_UNC, Fecal_LSR, nrow = 1)
dev.off()

FC_Table_Basin <- summaryStats(Fecal.Coliform ~ Monitoring.Basin, data = AllCountsAmbient2020, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

FC_Table_Basin

write.table(FC_Table_Basin, file = "FC_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#Appendix for How to Read  a Boxplot
tiff("boxplot_example.tiff", units="in", width=8, height=12, res=500)
ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + ylim(0,800) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size=15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")
dev.off()

#BIBI

BIBI_HC <- ggplot(subset(BIBISub2020, Monitoring.Basin == "Horse Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y="BIBI", x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

BIBI_LS <- ggplot(subset(BIBISub2020, Monitoring.Basin == "Little Swamp Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

BIBI_LNC <- ggplot(subset(BIBISub2020, Monitoring.Basin == "Lower North Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y="BIBI", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size=12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


BIBI_LSR <- ggplot(subset(BIBISub2020, Monitoring.Basin == "Lower Sammamish River"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


BIBI_PC <- ggplot(subset(BIBISub2020, Monitoring.Basin == "Perry Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

BIBI_PA <- ggplot(subset(BIBISub2020, Monitoring.Basin == "Parr Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


BIBI_UNC <- ggplot(subset(BIBISub2020, Monitoring.Basin == "Upper North Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("BIBI_By_Basin.tiff", units="in", width=13, height=6, res=500)
grid.arrange(BIBI_LNC, BIBI_LSR, BIBI_PC, nrow = 1)
dev.off()


BIBI_Table_Basin <- summaryStats(BIBI ~ Monitoring.Basin, data = BIBISub2020, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                                 test.arg.list=list(var.equal = FALSE, test="nonparametric"))

BIBI_Table_Basin

write.table(BIBI_Table_Basin, file = "BIBI_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)


#testing

stat_box_data <- function(y, upper_limit = max(AllCountsAmbientSummer$TEMP) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}


ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Horse Creek"), aes(Site, TEMP)) + 
  geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  ) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees Celsius)", x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

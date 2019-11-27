#temp by basin by site for 2019

Temp_HC <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Horse Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees Celsius)") + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Temp_LS <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Temp_LNC <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Lower North Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_LSR <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_PC <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Perry Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_UNC <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Upper North Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


grid.arrange(Temp_HC, Temp_LS, Temp_PC, Temp_LNC, Temp_UNC, Temp_LSR, nrow = 1)

#DO by basin by site for 2019

DO_HC <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Horse Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(7,12) +
  labs(title = NULL, subtitle=NULL, y="Dissolved Oxygen (mg/L)") + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_LS <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Little Swamp Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(7,12) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_LNC <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Lower North Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(7,12) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_LSR <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Lower Sammamish River"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(7,12) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_PC <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Perry Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(7,12) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_UNC <- ggplot(subset(AllCountsSUmmer2019, Monitoring.Basin == "Upper North Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(7,12) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


grid.arrange(DO_HC, DO_LS, DO_PC, DO_LNC, DO_UNC, DO_LSR, nrow = 1)

#Nutrients 

TP_HC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Horse Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.3) +
  labs(title = NULL, subtitle=NULL, y="Total Phosphorous (mg/L)", x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) +geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_LS <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_LNC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Lower North Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_LSR <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_PC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Perry Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_UNC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Upper North Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_HC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Horse Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y="Total Nitrogen (mg/L)") + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Total Nitrogen (mg/L)") +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_LS <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_LNC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Lower North Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL)  + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_LSR <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_PC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Perry Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL)  + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_UNC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Upper North Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


grid.arrange(TP_HC, TP_LS, TP_PC, TP_LNC, TP_UNC, TP_LSR, TN_HC, TN_LS, TN_PC, TN_LNC, TN_UNC, TN_LSR, nrow = 2)

#metals

Zn_HC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Horse Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y="Dissolved Zinc (ug/L)", x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_LS <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_LNC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Lower North Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_LSR <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_PC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Perry Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_UNC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Upper North Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Cu_HC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Horse Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y="Dissolved Copper (ug/L)") + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Dissolved Copper (ug/L)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Cu_LS <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Cu_LNC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Lower North Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18),  axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_LSR <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18),  axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_PC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Perry Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18),  axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_UNC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Upper North Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18),  axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_HC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Horse Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y="Dissolved Lead (ug/L)") + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Pb_LS <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Pb_LNC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Lower North Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL)  + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_LSR <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_PC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Perry Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL)  + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_UNC <- ggplot(subset(AllCountsAmbient2019, Monitoring.Basin == "Upper North Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


grid.arrange(Zn_HC, Zn_LS, Zn_PC, Zn_LNC, Zn_UNC, Zn_LSR, Cu_HC, Cu_LS, Cu_PC, Cu_LNC, Cu_UNC, Cu_LSR, Pb_HC, Pb_LS, Pb_PC, Pb_LNC, Pb_UNC, Pb_LSR, nrow = 3)
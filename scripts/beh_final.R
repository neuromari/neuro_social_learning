################## Behavior Analysis for 'Differential neural activity across learning contexts' manuscript
######### run by M. Rodriguez-Santiago, Hofmann lab 2016-2020
####



trials_to_beh <- read.csv("group_behavior.csv", na.strings = "NA")
colnames(trials_to_beh)

library(dplyr)
library(ggplot2)
library(cowplot)
library(Rmisc)
library(survminer)
require("survival")
library(effsize)


#### subsetting fig 1 data ####

ch1_surv <- read.csv("survival_analysis.csv", header = TRUE, sep = ",")
colnames(ch1_surv)

dom_surv<-ch1_surv[(ch1_surv$INFORMANT=="dom"),]
sub_surv<-ch1_surv[(ch1_surv$INFORMANT=="sub"),]
male_surv<-ch1_surv[(ch1_surv$INFORMANT=="male"),]
fem_surv<-ch1_surv[(ch1_surv$INFORMANT=="female"),]
naive_surv<-ch1_surv[(ch1_surv$INFORMANT=="naive"),]

surv_social_context_comb<-rbind(dom_surv,sub_surv)
surv_asocial_context_comb<-rbind(male_surv,fem_surv)

surv_comparison<- rbind(dom_surv, sub_surv, male_surv, fem_surv)
colnames(surv_comparison)


#### fig 1b: learning social v asocial context ####

fit_1 <- survfit(Surv(TRIAL, SURV70) ~ CONTEXT, data = surv_comparison)
summary(fit_1)
summary(fit_1)$table
print(fit_1)

surv_diff_1 <- survdiff(Surv(TRIAL, SURV70) ~ CONTEXT, data = surv_comparison)
surv_diff_1

panel_1_surv <- ggsurvplot(fit_1,
                           font.x=16,
                           font.y=16,
                           font.tickslab=12,
                           font.legend=12,
                           pval = TRUE,
                           pval.coord=c(17.5,.01),
                           size=2,
                           risk.table = FALSE,
                           ncensor.plot = FALSE,
                           ylim = c(0,1),
                           xlim = c(0,22),
                           xlab="trial",
                           ylab="cumulative response probability",
                           conf.int = FALSE,
                           fun = "event",
                           legend.title="",
                           legend.labs=c("asocial context","social context"),
                           palette = c("#7fbf7b","#af8dc3"),
                           legend=c(.185,.98))
panel_1_surv

#### supplemental: learning social context by informant ####

fit_2 <- survfit(Surv(TRIAL, SURV70) ~ INFORMANT, data = surv_social_context_comb)
summary(fit_2)
summary(fit_2)$table
print(fit_2)

surv_diff_2 <- survdiff(Surv(TRIAL, SURV70) ~ INFORMANT, data = surv_social_context_comb)
surv_diff_2

panel_2_surv <- ggsurvplot(fit_2,
                           font.x=16,
                           font.y=16,
                           font.tickslab=12,
                           font.legend=12,
                           pval = TRUE,
                           pval.coord=c(18,.01),
                           size=2,
                           risk.table = FALSE,
                           ncensor.plot = FALSE,
                           ylim = c(0,1),
                           xlim = c(0,22),
                           xlab="trial",
                           ylab="cumulative response probability",
                           conf.int = FALSE,
                           fun = "event",
                           legend.title="informant status",
                           legend.labs=c("DOM","SUB"),
                           palette = c("#7b3294","#c2a5cf"),
                           legend=c(.2,.89))
panel_2_surv

#### supplemental: learning asocial context by sex ####

fit_3 <- survfit(Surv(TRIAL, SURV70) ~ INFORMANT, data = surv_asocial_context_comb)
summary(fit_3)
summary(fit_3)$table
print(fit_3)

surv_diff_3 <- survdiff(Surv(TRIAL, SURV70) ~ INFORMANT, data = surv_asocial_context_comb)
surv_diff_3

panel_3_surv <- ggsurvplot(fit_3,
                           font.x=16,
                           font.y=16,
                           font.tickslab=12,
                           font.legend=12,
                           pval = TRUE,
                           pval.coord=c(18,.01),
                           size=2,
                           risk.table = FALSE,
                           ncensor.plot = FALSE,
                           ylim = c(0,1),
                           xlim = c(0,22),
                           xlab="trial",
                           ylab="cumulative response probability",
                           conf.int = FALSE,
                           fun = "event",
                           legend.title="sex of individuals",
                           legend.labs=c("females","males"),
                           palette = c("#a6dba0","#008837"),
                           legend=c(.19,.9))
panel_3_surv


#### supplemental: survival plots of sensitivity analysis ####

data1<-read.csv("beh_sensitvityanalysis_surv50.2.csv")
colnames(data1)

data2<-read.csv("beh_sensitvityanalysis_surv50.3.csv")
data3<-read.csv("beh_sensitvityanalysis_surv50.4.csv")
data4<-read.csv("beh_sensitvityanalysis_surv60.2.csv")
data5<-read.csv("beh_sensitvityanalysis_surv60.3.csv")
data6<-read.csv("beh_sensitvityanalysis_surv60.4.csv")
data7<-read.csv("beh_sensitvityanalysis_surv70.2.csv")
data8<-read.csv("beh_sensitvityanalysis_surv70.3.csv")
data9<-read.csv("beh_sensitvityanalysis_surv70.4.csv")
data10<-read.csv("beh_sensitvityanalysis_surv80.2.csv")
data11<-read.csv("beh_sensitvityanalysis_surv80.3.csv")
data12<-read.csv("beh_sensitvityanalysis_surv80.4.csv")
data13<-read.csv("beh_sensitvityanalysis_surv100.2.csv")
data14<-read.csv("beh_sensitvityanalysis_surv100.3.csv")
data15<-read.csv("beh_sensitvityanalysis_surv100.4.csv")

# adding PERC column to data1, data4, data7, data10, data13 DON'T DO EVERY TIME
data1$PERC<-50
colnames(data1)
head(data1)
data1$PERC<-as.factor(data1$PERC)

data4$PERC<-60
colnames(data4)
data4$PERC<-as.factor(data4$PERC)

data7$PERC<-70
head(data7)
data7$PERC<-as.factor(data7$PERC)

data10$PERC<-80
head(data10)
data10$PERC<-as.factor(data10$PERC)

data13$PERC<-100
head(data13)
data13$PERC<-as.factor(data13$PERC)

# renaming SURV columns for data1, data4, data7, data10, data13
names(data1)[names(data1)=="SURV50"] <- "SURV"
names(data2)[names(data2)=="SURV50"] <- "SURV"
names(data3)[names(data3)=="SURV50"] <- "SURV"
names(data5)[names(data5)=="SURV60"] <- "SURV"
names(data6)[names(data6)=="SURV60"] <- "SURV"
names(data8)[names(data8)=="SURV70"] <- "SURV"
names(data9)[names(data9)=="SURV70"] <- "SURV"
names(data11)[names(data11)=="SURV80"] <- "SURV"
names(data12)[names(data12)=="SURV80"] <- "SURV"
names(data14)[names(data14)=="SURV100"] <- "SURV"
names(data15)[names(data15)=="SURV100"] <- "SURV"


surv50 <- rbind(data1, data2, data3)
surv60 <- rbind(data4, data5, data6)
surv70 <- rbind(data7, data8, data9)
surv80 <- rbind(data10, data11, data12)
surv100 <- rbind(data13, data14, data15)

surv_2_consec_50_80 <- rbind(data1, data7, data10)
surv_3_consec_50_100 <- rbind(data1, data4, data7, data10, data13)


## sensitivity analysis for groups (50-80%)
fit_50 <- survfit(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv50)
summary(fit_50)
summary(fit_50)$table
print(fit_50)

surv_diff_50 <- survdiff(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv50)
surv_diff_50

panel_50_surv <- ggsurvplot(fit_50,
                            font.x=14,
                            font.y=14,
                            font.tickslab=12,
                            font.legend=12,
                            pval = TRUE,
                            pval.coord=c(17.5,.01),
                            size=2,
                            risk.table = FALSE,
                            ncensor.plot = FALSE,
                            ylim = c(0,1),
                            xlim = c(0,22),
                            xlab="trial",
                            ylab="cumulative response probability",
                            conf.int = FALSE,
                            fun = ("event"),
                            legend.title="",
                            legend.labs=c("two consec.","three consec.","four consec."),
                            palette = c("#c2a5cf","#9970ab","#762a83"),
                            legend=c(.2,.93),
                            title="50% of group response")
panel_50_surv


fit_60 <- survfit(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv60)
summary(fit_60)
summary(fit_60)$table
print(fit_60)

surv_diff_60 <- survdiff(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv60)
surv_diff_60

panel_60_surv <- ggsurvplot(fit_60,
                            font.x=14,
                            font.y=14,
                            font.tickslab=12,
                            font.legend=12,
                            pval = TRUE,
                            pval.coord=c(17.5,.01),
                            size=2,
                            risk.table = FALSE,
                            ncensor.plot = FALSE,
                            ylim = c(0,1),
                            xlim = c(0,22),
                            xlab="trial",
                            ylab="cumulative response probability",
                            conf.int = FALSE,
                            fun = ("event"),
                            legend.title="",
                            legend.labs=c("two consec.","three consec.","four consec."),
                            palette = c("#c2a5cf","#9970ab","#762a83"),
                            legend=c(.2,.93),
                            title="60% of group response")
panel_60_surv


fit_70 <- survfit(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv70)
summary(fit_70)
summary(fit_70)$table
print(fit_70)

surv_diff_70 <- survdiff(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv70)
surv_diff_70

panel_70_surv <- ggsurvplot(fit_70,
                            font.x=14,
                            font.y=14,
                            font.tickslab=12,
                            font.legend=12,
                            pval = TRUE,
                            pval.coord=c(17.5,.01),
                            size=2,
                            risk.table = FALSE,
                            ncensor.plot = FALSE,
                            ylim = c(0,1),
                            xlim = c(0,22),
                            xlab="trial",
                            ylab="cumulative response probability",
                            conf.int = FALSE,
                            fun = ("event"),
                            legend.title="",
                            legend.labs=c("two consec.","three consec.","four consec."),
                            palette = c("#c2a5cf","#9970ab","#762a83"),
                            legend=c(.2,.93),
                            title="70% of group response")
panel_70_surv


fit_80 <- survfit(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv80)
summary(fit_80)
summary(fit_80)$table
print(fit_80)

surv_diff_80 <- survdiff(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv80)
surv_diff_80

panel_80_surv <- ggsurvplot(fit_80,
                            font.x=14,
                            font.y=14,
                            font.tickslab=12,
                            font.legend=12,
                            pval = TRUE,
                            pval.coord=c(17.5,.01),
                            size=2,
                            risk.table = FALSE,
                            ncensor.plot = FALSE,
                            ylim = c(0,1),
                            xlim = c(0,22),
                            xlab="trial",
                            ylab="cumulative response probability",
                            conf.int = FALSE,
                            fun = ("event"),
                            legend.title="",
                            legend.labs=c("two consec.","three consec.","four consec."),
                            palette = c("#c2a5cf","#9970ab","#762a83"),
                            legend=c(.2,.93),
                            title="80% of group response")
panel_80_surv


##sensitivity analysis for 100% groups and individuals
surv100 %>%
  filter(TREATMENT == "social") -> surv100_groups
surv100 %>%
  filter(TREATMENT =="individual") -> surv100_ind


## groups
fit_100_g <- survfit(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv100_groups)
summary(fit_100_g)
summary(fit_100_g)$table
print(fit_100_g)

surv_diff_100_g <- survdiff(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv100_groups)
surv_diff_100_g

panel_100_surv_g <- ggsurvplot(fit_100_g,
                               font.x=14,
                               font.y=14,
                               font.tickslab=12,
                               font.legend=12,
                               pval = TRUE,
                               pval.coord=c(17.5,.01),
                               size=2,
                               risk.table = FALSE,
                               ncensor.plot = FALSE,
                               ylim = c(0,1),
                               xlim = c(0,22),
                               xlab="trial",
                               ylab="cumulative response probability",
                               conf.int = FALSE,
                               fun = ("event"),
                               legend.title="",
                               legend.labs=c("two consec.","three consec.","four consec."),
                               palette = c("#c2a5cf","#9970ab","#762a83"),
                               legend=c(.2,.93),
                               title="100% of group response")
panel_100_surv_g

## individuals
fit_100_i <- survfit(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv100_ind)
summary(fit_100_i)
summary(fit_100_i)$table
print(fit_100_i)

surv_diff_100_i <- survdiff(Surv(TRIAL, SURV) ~ CONSECUTIVE, data = surv100_ind)
surv_diff_100_i

panel_100_surv_i <- ggsurvplot(fit_100_i,
                               font.x=14,
                               font.y=14,
                               font.tickslab=12,
                               font.legend=12,
                               pval = TRUE,
                               pval.coord=c(17.5,.93),
                               size=2,
                               risk.table = FALSE,
                               ncensor.plot = FALSE,
                               ylim = c(0,1),
                               xlim = c(0,22),
                               xlab="trial",
                               ylab="cumulative response probability",
                               conf.int = FALSE,
                               fun = ("event"),
                               legend.title="",
                               legend.labs=c("two consec.","three consec.","four consec."),
                               palette = c("#a6dba0","#5aae61","#1b7837"),
                               legend=c(.2,.93),
                               title="alone individual response")
panel_100_surv_i

##sensitivity analysis for groups, 2 consec by percentage (50-80%) 
# adding PERC column to data1, data4, data7, data10, data13 DON'T DO EVERY TIME
data1$PERC<-50
colnames(data1)
head(data1)
data1$PERC<-as.factor(data1$PERC)

data4$PERC<-60
colnames(data4)
data4$PERC<-as.factor(data4$PERC)

data7$PERC<-70
head(data7)
data7$PERC<-as.factor(data7$PERC)

data10$PERC<-80
head(data10)
data10$PERC<-as.factor(data10$PERC)

data13$PERC<-100
head(data13)
data13$PERC<-as.factor(data13$PERC)


surv_2_consec_50_80 <- rbind(data1, data7, data10)


# graphing comparison of response by % group required to meet criterion
fit_2_consec <- survfit(Surv(TRIAL, SURV) ~ PERC, data = surv_2_consec_50_80)
summary(fit_2_consec)

surv_diff_2_consec <- survdiff(Surv(TRIAL, SURV) ~ PERC, data = surv_2_consec_50_80)
surv_diff_2_consec

ggsurvplot(fit_2_consec,
           font.x=14,
           font.y=14,
           font.tickslab=12,
           font.legend=12,
           pval = TRUE,
           pval.coord=c(17.5,.01),
           size=2,
           risk.table = FALSE,
           ncensor.plot = FALSE,
           ylim = c(0,1),
           xlim = c(0,22),
           xlab="trial",
           ylab="cumulative response probability",
           conf.int = FALSE,
           fun = ("event"),
           legend.title="",
           legend.labs=c("50% group response", "70% group response","80% group response"),
           palette = c("#c2a5cf","#762a83","#40004b"),
           legend=c(.2,.93))

##
fit_2_consec_all <- survfit(Surv(TRIAL, SURV) ~ PERC, data = surv_3_consec_50_100)
summary(fit_2_consec_all)

surv_diff_2_consec_all <- survdiff(Surv(TRIAL, SURV) ~ PERC, data = surv_3_consec_50_100)
surv_diff_2_consec_all

ggsurvplot(fit_2_consec_all,
           font.x=16,
           font.y=16,
           font.tickslab=12,
           font.legend=12,
           pval = TRUE,
           pval.coord=c(17.5,.01),
           size=2,
           risk.table = FALSE,
           ncensor.plot = FALSE,
           ylim = c(0,1),
           xlim = c(0,22),
           xlab="trial",
           ylab="cumulative response probability",
           conf.int = FALSE,
           fun = ("event"),
           legend.title="",
           legend.labs=c("50% group response","60% group response","70% group response","80% group response","100% group response"),
           palette = c("#e7d4e8","#c2a5cf","#9970ab","#762a83","#40004b"),
           legend=c(.2,.91))


#### fig 1c: plots of trials to criterion by context ####

tto_no23 <- read.csv("ch1_groupbeh_learning_trials_only_learners.csv", header = TRUE, sep = ",")
colnames(tto_no23)

avg_context = summarySE(tto_no23, measurevar = "TRIAL", groupvars = c("CONTEXT"))

fig_1_box <- 
  ggplot(tto_no23, aes(CONTEXT, TRIAL, color = CONTEXT)) +
  geom_boxplot(size=1, outlier.alpha = 0) +
  geom_jitter(size=5,position=position_jitter(0.1),alpha=0.5) +
  ylim(0,22) +
  labs(x="", y="trial of learning response") +
  scale_x_discrete(limits=c("group","alone"),
                   labels=c("group"="social\ncontext", "alone"="asocial\ncontext")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(values = c("#7fbf7b","#af8dc3"))
fig_1_box

# statistics
test_box_1 <- wilcox.test(TRIAL ~ CONTEXT, data = tto_no23,
                          exact = FALSE)
test_box_1

box_cohen <- cohen.d(TRIAL ~ CONTEXT, data = tto_no23)

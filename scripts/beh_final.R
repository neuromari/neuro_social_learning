########################## Behavior Analysis for 'Differential neural activity across learning contexts' manuscript
############# run by M. Rodriguez-Santiago, Hofmann lab 2016-2020



trials_to_beh <- read.csv("group_behavior.csv", na.strings = "NA")
colnames(trials_to_beh)

library(dplyr)
library(ggplot2)
library(cowplot)

##### subsetting data by grouping variables #####
dom<-trials_to_beh[(trials_to_beh$INFORMANT=="dom"),]
sub<-trials_to_beh[(trials_to_beh$INFORMANT=="sub"),]
male<-trials_to_beh[(trials_to_beh$INFORMANT=="male"),]
fem<-trials_to_beh[(trials_to_beh$INFORMANT=="female"),]
naive<-trials_to_beh[(trials_to_beh$INFORMANT=="naive"),]

#group<-trials_to_beh[(trials_to_beh$CONTEXT=="group"),]
#individual<-trials_to_beh[(trials_to_beh$CONTEXT=="alone"),]

#social<-trials_to_beh[(trials_to_beh$TREATMENT=="social"),]
#naive<-trials_to_beh[(trials_to_beh$TREATMENT=="allnaive"),]
#asocial<-trials_to_beh[(trials_to_beh$TREATMENT=="individual"),]

social_context_comb<-rbind(naive,dom,sub)
asocial_context_comb<-rbind(male,fem)
informant_comb<-rbind(dom,sub)

##### graphing learning response avg by groups #####

library(Rmisc)

# panel 1: social/asocial comparison
avg_context = summarySE(trials_to_beh, measurevar = "TRIAL", groupvars = c("CONTEXT"))

panel_1_box <- 
  ggplot(trials_to_beh, aes(CONTEXT, TRIAL, color = CONTEXT)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(size=3,position=position_jitter(0.1),alpha=0.5) +
  ylim(0,25) +
  labs(x="", y="trial of learning response") +
  scale_x_discrete(limits=c("group","alone"),
                   labels=c("group"="social \n context", "alone"="asocial \n context")) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(values = c("#7fbf7b","#af8dc3"))+
  geom_hline(yintercept=22, linetype="dashed", color = "gray50")
panel_1_box

shapiro.test(trials_to_beh$TRIAL)
#data:  trials_to_beh$TRIAL
#W = 0.90153, p-value = 0.01243 = not normal, significantly different from assumption of normality
test_panel_1 <- wilcox.test(TRIAL ~ CONTEXT, data = trials_to_beh,
                   exact = FALSE)
test_panel_1
#data:  TRIAL by CONTEXT
#W = 103, p-value = 0.2499
#alternative hypothesis: true location shift is not equal to 0



#panel 2: social context distilled
avg_soc_context = summarySE(social_context_comb, measurevar = "TRIAL", groupvars = c("INFORMANT"))

panel_2_box <- 
  ggplot(social_context_comb, aes(INFORMANT, TRIAL, color = INFORMANT)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(size=3,position=position_jitter(0.1),alpha=0.5) +
  ylim(0,25) +
  labs(x="", y="trial of learning response") +
  scale_x_discrete(limits=c("naive","dom","sub"),
                   labels=c("naive"="naive \n groups", "dom"="DOM dem \n groups","sub"="SUB dem \n groups")) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(values = c("#7b3294","black","#c2a5cf"))+
  geom_hline(yintercept=22, linetype="dashed", color = "gray50")
panel_2_box

#anova
group.inf.aov <- aov(TRIAL ~ INFORMANT, data = social_context_comb)
summary(group.inf.aov)
# F.value = 1.146, p=0.341

#tukey
TukeyHSD(group.inf.aov)
#               diff        lwr       upr     p adj
#naive-dom  5.475000  -3.835077 14.785077 0.3117954
#sub-dom    1.732143  -6.719929 10.184215 0.8598981
#sub-naive -3.742857 -13.305285  5.819571 0.5843215



#panel 3: asocial context distilled
avg_asoc_context = summarySE(asocial_context_comb, measurevar = "TRIAL", groupvars = c("INFORMANT"))

panel_3_box <- 
  ggplot(asocial_context_comb, aes(INFORMANT, TRIAL, color = INFORMANT)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(size=3,position=position_jitter(0.1),alpha=0.5) +
  ylim(0,25) +
  labs(x="", y="trial of learning response") +
  scale_x_discrete(limits=c("male","female"),
                   labels=c("male"="individual \n males", "female"="individual \n females")) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(values = c("#a6dba0","#008837"))+
  geom_hline(yintercept=22, linetype="dashed", color = "gray50")
panel_3_box


#panel 4: dem v naive v alone distilled
avg_treatment_context = summarySE(trials_to_beh, measurevar = "TRIAL", groupvars = c("TREATMENT"))

panel_4_box <- 
  ggplot(trials_to_beh, aes(TREATMENT, TRIAL, color = TREATMENT)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(size=3,position=position_jitter(0.1),alpha=0.5) +
  ylim(0,25) +
  labs(x="", y="trial of learning response") +
  scale_x_discrete(limits=c("allnaive","social","individual"),
                   labels=c("allnaive"="naive\ngroups", "social"="groups w/ \ninformant","individual"="asocial\ncontext")) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(values = c("black","#7fbf7b","#af8dc3"))+
  geom_hline(yintercept=22, linetype="dashed", color = "gray50")
panel_4_box

#anova
all.aov <- aov(TRIAL ~ TREATMENT, data = trials_to_beh)
summary(all.aov)
# F.value = 1.762, p=0.192

#tukey
TukeyHSD(all.aov)
#                         diff        lwr      upr     p adj
#individual-allnaive -0.100000  -9.294414 9.094414 0.9995955
#social-allnaive     -4.666667 -12.995166 3.661832 0.3583154
#social-individual   -4.566667 -11.627495 2.494162 0.2597349


##### informant aggression at response trial #####

aggression <- read.csv("ch1_groupbeh_final.csv", header = TRUE, sep = ",")
colnames(aggression)

aggression_na<-aggression[!is.na(aggression$INFORMANT),]

agg_dom <- aggression_na[(aggression_na$INFORMANT=="dom"),]
agg_sub <- aggression_na[(aggression_na$INFORMANT=="sub"),]

agg_informant_comb <- rbind(agg_dom,agg_sub)

library(Rmisc)


## approaches by informant
avg_approaches = summarySE(agg_informant_comb, measurevar = "APPROACHES", groupvars = c("INFORMANT","TRIAL"))

ggplot(agg_informant_comb, aes(x=TRIAL, y=APPROACHES, color=INFORMANT)) +
  stat_summary(geom="ribbon", fun.data)

agg_1 <- ggplot(avg_approaches, aes(x=TRIAL, y=APPROACHES, color=INFORMANT)) + 
  geom_line(size=2) +
  geom_errorbar(aes(ymin=APPROACHES-se, ymax=APPROACHES+se), width=1, position = position_dodge(0.05)) +
  ylim(0,30) +
  labs(x="trial",y="approaches",colour="informant status") +
  scale_color_manual(values = c("#7b3294","#c2a5cf")) +
    theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        legend.position = c(0.01,.9),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))

inf_aov <- aov(APPROACHES ~ INFORMANT * TRIAL + Error(GROUP), data=agg_informant_comb)
summary(inf_aov)


# fancier line graph with mean line + CI for approaches
approach_tidy <- unite(agg_informant_comb, GROUP_INF, c(GROUP, INFORMANT), sep = "_", remove = FALSE)
head(approach_tidy)

ggplot(approach_tidy, aes(x=TRIAL, y=APPROACHES)) +
  geom_line(aes(color=INFORMANT, group=GROUP_INF)) +
  labs(x="trial",y="approaches",colour="informant status") +
  scale_color_manual(values = c("#7b3294","#c2a5cf")) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        legend.position = c(0.01,.9),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))

approach_mean <- approach_tidy %>%
  group_by(TRIAL, INFORMANT) %>%
  dplyr::summarise(n = (count = n()), 
            mean=mean(APPROACHES),
            median=median(APPROACHES),
            sd=sd(APPROACHES)) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = mean + qt((1-0.95)/2, n-1) * sem,
         CI_upper = mean - qt((1-0.95)/2, n-1) * sem)
head(approach_mean)  #this is a tibble

ggplot(approach_mean, aes(x=TRIAL, y=mean, color=INFORMANT)) +
  geom_line(aes(x=TRIAL, y=mean, color=INFORMANT), size=2) +
  scale_color_manual(values = c("#7b3294","#c2a5cf")) +
  labs(x="trial",y="approaches",color="informant status") +
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=INFORMANT), color="gray70", alpha=0.4) +
  scale_fill_manual(values = c("#7b3294","#c2a5cf")) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        legend.position = c(0.01,1),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))


## displacements by informant
avg_displacements = summarySE(agg_informant_comb, measurevar = "DISPLACEMENTS", groupvars = c("INFORMANT","TRIAL"))

agg_2 <- ggplot(avg_displacements, aes(x=TRIAL, y=DISPLACEMENTS, color=INFORMANT)) + 
  geom_line(size=2) +
  geom_errorbar(aes(ymin=DISPLACEMENTS-se, ymax=DISPLACEMENTS+se), width=1, position = position_dodge(0.05)) +
  ylim(0,30) +
  labs(x="trial",y="displacements",colour="informant status") +
  scale_color_manual(values = c("#7b3294","#c2a5cf")) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        legend.position = c(0.01,.9),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))

inf_displace_aov <- aov(DISPLACEMENTS ~ INFORMANT * TRIAL + Error(GROUP), data=agg_informant_comb)
summary(inf_displace_aov)


# displacements mean and CI
displace_mean <- approach_tidy %>%
  group_by(TRIAL, INFORMANT) %>%
  dplyr::summarise(n = (count = n()), 
                   mean=mean(DISPLACEMENTS),
                   median=median(DISPLACEMENTS),
                   sd=sd(DISPLACEMENTS)) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = mean + qt((1-0.95)/2, n-1) * sem,
         CI_upper = mean - qt((1-0.95)/2, n-1) * sem)
head(displace_mean)

ggplot(displace_mean, aes(x=TRIAL, y=mean, color=INFORMANT)) +
  geom_line(aes(x=TRIAL, y=mean, color=INFORMANT), size=2) +
  scale_color_manual(values = c("#7b3294","#c2a5cf")) +
  labs(x="trial",y="displacements",color="informant status") +
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=INFORMANT), color="gray70", alpha=0.4) +
  scale_fill_manual(values = c("#7b3294","#c2a5cf")) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        legend.position = c(0.01,1),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))


## agonistic efficiency by informant
agg_informant_comb %>%
  mutate(AGGEFF.TRIAL.PERC = AGGEFF.TRIAL*100) -> agg_informant_comb_perc
colnames(agg_informant_comb_perc)

avg_aggeff = summarySE(agg_informant_comb_perc, measurevar = "AGGEFF.TRIAL.PERC", groupvars = c("INFORMANT", "TRIAL"))

agg_3 <- ggplot(avg_aggeff, aes(x=TRIAL, y=AGGEFF.TRIAL.PERC, color=INFORMANT)) + 
  geom_line(size=2) +
  geom_errorbar(aes(ymin=AGGEFF.TRIAL.PERC-se, ymax=AGGEFF.TRIAL.PERC+se), width=1, position = position_dodge(0.05)) +
  ylim(0,100) +
  labs(x="trial",y="agonistic efficiency (%)",colour="informant status") +
  scale_color_manual(values = c("#7b3294","#c2a5cf")) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        legend.position = 'none',
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))

inf_aggeff_aov <- aov(AGGEFF.TRIAL.PERC ~ INFORMANT * TRIAL + Error(GROUP), data=agg_informant_comb_perc)
summary(inf_aggeff_aov)

inf_aggeff_trial_aov <- aov(AGGEFF.TRIAL ~ INFORMANT * TRIAL + Error(GROUP), data=agg_informant_comb_perc)
summary(inf_aggeff_trial_aov)


# agg efficiency mean and CI
aggeff_tidy <- unite(agg_informant_comb_perc, GROUP_INF, c(GROUP, INFORMANT), sep = "_", remove = FALSE)
head(aggeff_tidy)

aggeff_mean <- aggeff_tidy %>%
  group_by(TRIAL, INFORMANT) %>%
  dplyr::summarise(n = (count = n()), 
                   mean=mean(AGGEFF.TRIAL.PERC),
                   median=median(AGGEFF.TRIAL.PERC),
                   sd=sd(AGGEFF.TRIAL.PERC)) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = mean + qt((1-0.95)/2, n-1) * sem,
         CI_upper = mean - qt((1-0.95)/2, n-1) * sem)

ggplot(aggeff_mean, aes(x=TRIAL, y=mean, color=INFORMANT)) +
  geom_line(aes(x=TRIAL, y=mean, color=INFORMANT), size=2) +
  scale_color_manual(values = c("#7b3294","#c2a5cf")) +
  labs(x="trial",y="agonistic efficiency",color="informant status") +
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=INFORMANT), color="gray70", alpha=0.4) +
  scale_fill_manual(values = c("#7b3294","#c2a5cf")) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        legend.position = c(0.01,1),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))


library(ggpubr)
ggarrange(agg_3,
          ggarrange(agg_1, agg_2, ncol=2, labels = c("B","C")),
          nrow = 2,
          labels = "A")

## agonistic efficiency v proportion response
ggplot(agg_informant_comb_perc, aes(x=AGGEFF.TRIAL.PERC, y=PROPORTION, color=INFORMANT)) +
  geom_jitter()

colnames(agg_informant_comb_perc)

avg_aggeff = summarySE(agg_informant_comb_perc, measurevar = "AGGEFF.TRIAL.PERC", groupvars = c("INFORMANT", "TRIAL"))

ggplot(agg_informant_comb_perc, aes(x=ORIENT, y=MOVE, color=INFORMANT)) +
  geom_jitter(size=3, hjust=0.2, alpha=0.7)

ggplot(agg_informant_comb_perc, aes(x=ORIENT, y=PERCENT)) +
  geom_jitter()
ggplot(agg_informant_comb_perc, aes(x=LINECROSS, y=PERCENT)) +
  geom_jitter()
ggplot(agg_informant_comb_perc, aes(x=LINECROSS.NORM, y=PERCENT)) +
  geom_jitter()


ageff_trial <- read.csv("group_behavior.csv", sep = ",")
colnames(ageff_trial)


ageff_trial<-ageff_trial[!is.na(ageff_trial$INFORMANT),]

ageff_trial_dom <- ageff_trial[(ageff_trial$INFORMANT=="dom"),]
ageff_trial_sub <- ageff_trial[(ageff_trial$INFORMANT=="sub"),]

ageff_trial_comb <- rbind(ageff_trial_dom,ageff_trial_sub)



ggplot(ageff_trial_comb, aes(x=TRIAL, y=AGGEFF.TRIAL, color=INFORMANT)) +
  geom_jitter(size=4, position=position_jitter(0.1), alpha=0.5) +
  ylim(0,1) +
  geom_vline(xintercept = 22.5,linetype="dashed", 
             color = "gray50") +
  labs(x="learning trial",y="agonistic efficiency",colour="informant status") +
  scale_color_manual(values = c("#7b3294","#c2a5cf")) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        legend.position = c(0.001,.1))

ggplot(ageff_trial_comb, aes(x=PROPORTION, y=AGGEFF.TRIAL, color=INFORMANT)) +
  geom_point(size=4, alpha=0.5) +
  ylim(0,1) +
  labs(x="agonistic efficiency of informant",y="proportion of group response",colour="informant status") +
  scale_color_manual(values = c("#7b3294","#c2a5cf")) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        legend.position = c(0.001,.1)) +
  geom_smooth(method="lm",se=FALSE, fullrange=TRUE)



#### locomotion and learning ####

locomo <- read.csv("ch1_groupbeh_final.csv", header = TRUE, sep = ",")
colnames(locomo)

locomo$INFORMANT <- factor(locomo$INFORMANT, levels = c("dom", "sub", "male","female"))

dom_groups<-locomo[(locomo$INFORMANT=="dom"),]
sub_groups<-locomo[(locomo$INFORMANT=="sub"),]
fem_ind <- locomo[(locomo$INFORMANT=="female"),]
male_ind <- locomo[(locomo$INFORMANT=="male"),]

social <- rbind(dom_groups, sub_groups)
asocial <- rbind(male_ind, fem_ind)

all_avg_by_informant <- locomo %>%
  dplyr::group_by(INFORMANT, TRIAL) %>%
  dplyr::summarise(avg_activity=mean(LINECROSS.NORM), se=sd(LINECROSS.NORM)/sqrt(length(LINECROSS.NORM)), avg_displace=mean(DISPLACEMENTS), se_dis=(sd(DISPLACEMENTS)/sqrt(length(DISPLACEMENTS))))
View(all_avg_by_informant)

all_avg_by_informant$INFORMANT <- factor(all_avg_by_informant$INFORMANT, levels = c("dom", "sub", "male","female"))

displace_inf <- all_avg_by_informant %>%
  filter(INFORMANT=="dom" | INFORMANT=="sub")
View(displace_inf)

## color palettes
palette1_soc <- c("#7b3294","#c2a5cf")
# supp. figure 3, panel a
ggplot(displace_inf, aes(x=TRIAL, y=avg_displace, group=INFORMANT)) +
  geom_line(aes(colour=factor(INFORMANT)), size=1.7, alpha=.8) +
  scale_color_manual(values = palette1_soc,
                     labels=c("dominant informants", "subordinate informants")) +
  scale_fill_manual(values = palette1_soc,
                    guide = FALSE) +
  geom_ribbon(aes(ymax = avg_displace+se_dis, 
                  ymin = avg_displace-se_dis, 
                  fill=factor(INFORMANT)),
              alpha=0.5) +
  labs(x="trial", y="# of displacements (in 10 min)") +
  xlim(0,22)+
  ylim(0,25) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position = c(.21,.92),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))
## repeated measure anova for supp. fig 3 panel a
library(rstatix)
all(is.na(social$DISPLACEMENTS))
all(is.na(social$GROUP))
all(is.na(social$INFORMANT))
aov.activity.inf <- rstatix::anova_test(data=social, dv = DISPLACEMENTS, wid = GROUP, within = c(INFORMANT,TRIAL)) # this line doesn't work b/c "one of the terms is a linear combination of the other"
inf_aov <- aov(DISPLACEMENTS ~ INFORMANT * TRIAL + Error(GROUP), data=social)
summary(inf_aov) # this works



#supplemental figure 3, panel b: displacements at trial and the trial at criterion
tto_no23 <- read.csv("ch1_groupbeh_learning_trials_only_learners.csv", header = TRUE, sep = ",")
View(tto_no23)

library(Rmisc)

social_learners <- tto_no23 %>%
  filter(INFORMANT==c("dom","sub"))
colnames(social_learners)

ggplot(social_learners, aes(x=DISPLACEMENTS, y=TRIAL)) +
  geom_point(size=4,alpha=.7) +
  scale_color_manual(values = "#40004b") +
  labs(x="# of displacements", y="trial of group response") +
  xlim(0,30)+
  ylim(0,30) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position = c(.27,.92),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))
#stats for supp. figure 3 panel b
cor1 <- cor(social_learners$DISPLACEMENTS, social_learners$TRIAL, method = "pearson")
cor1  # cor coefficient = 0.65
summary(cor1)
social_displace_fit <- lm(DISPLACEMENTS ~ TRIAL, data=social)
summary(social_displace_fit)



# supplemental figure 4, panel a: trial x activity (4 groups)
palette2_all <- c("#7b3294","#c2a5cf","#008837","#a6dba0")

ggplot(all_avg_by_informant, aes(x=TRIAL, y=avg_activity, group=INFORMANT)) +
  geom_line(aes(colour=factor(INFORMANT)), size=1.7, alpha=.8) +
  scale_color_manual(values = palette2_all,
                     labels=c("dom informant groups", "sub informant groups", "individual males", "individual females")) +
  scale_fill_manual(values = palette2_all,
                    guide = FALSE) +
  geom_ribbon(aes(ymax = avg_activity+se, 
                  ymin = avg_activity-se, 
                  fill=factor(INFORMANT)),
              alpha=0.5) +
  labs(x="trial", y="mean activity in 10 minutes") +
  xlim(0,22)+
  ylim(0,20) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 1),
        legend.position = c(.18,.86),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))
#stats for trial x activity x informant
activity_inf_aov <- aov(LINECROSS.NORM ~ INFORMANT * TRIAL + Error(GROUP), data=locomo)
summary(activity_inf_aov) # this works


#supplemental figure 4, panel b: activity at trial x displacements at trial (social df)
colnames(social)
ggplot(social, aes(x=LINECROSS.NORM, y=DISPLACEMENTS, color=INFORMANT)) +
  geom_point(size=3.5, alpha=0.7) +
  scale_color_manual(values = palette1_soc,
                     labels=c("dominant informants", "subordinate informants")) +
  labs(x="tank activity", y="informant displacements by aggression") +
  xlim(0,30)+
  ylim(0,40) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + #line doesn't work
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position = c(.21,.93),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))
#stats for supp. figure 4 panel b
cor2 <- cor(social$LINECROSS.NORM, social$DISPLACEMENTS, method = "pearson")
cor2  # cor coefficient = 0.02
summary(cor2)
act_agg_fit <- lm(DISPLACEMENTS ~ LINECROSS.NORM, data=social)
summary(act_agg_fit)


#supplemental figure 4, panel c: showing activity over trials by soc/asoc learners/non
colnames(social)
colnames(asocial)

social_learn_rate_avg <- social %>%
  dplyr::group_by(LEARN.RATE, TRIAL) %>%
  dplyr::summarise(avg_activity=mean(LINECROSS.NORM), se=sd(LINECROSS.NORM)/sqrt(length(LINECROSS.NORM)))
View(social_learn_rate_avg)

asocial_learn_rate_avg <- asocial %>%
  dplyr::group_by(LEARN.RATE, TRIAL) %>%
  dplyr::summarise(avg_activity=mean(LINECROSS.NORM), se=sd(LINECROSS.NORM)/sqrt(length(LINECROSS.NORM)))
View(asocial_learn_rate_avg)

palette3_learn <- c("#99d594","#3288bd","#fc8d59")

ggplot(social_learn_rate_avg, aes(x=TRIAL, y=avg_activity, group=LEARN.RATE)) +
  geom_line(aes(colour=factor(LEARN.RATE)), size=1.7, alpha=.8) +
  scale_color_manual(values = palette3_learn,
                     labels=c("early social learners", "late social learners", "social non-learners")) +
  scale_fill_manual(values = palette3_learn,
                    guide = FALSE) +
  geom_ribbon(aes(ymax = avg_activity+se, 
                  ymin = avg_activity-se, 
                  fill=factor(LEARN.RATE)),
              alpha=0.5) +
  labs(x="trial", y="mean activity in 10 minutes") +
  xlim(1,22)+
  ylim(0,20) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 1),
        legend.position = c(.17,.88),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))
#stats for trial x activity x informant
social_learn_aov <- aov(LINECROSS.NORM ~ LEARN.RATE * TRIAL + Error(GROUP), data=social)
summary(social_learn_aov) # this works

ggplot(asocial_learn_rate_avg, aes(x=TRIAL, y=avg_activity, group=LEARN.RATE)) +
  geom_line(aes(colour=factor(LEARN.RATE)), size=1.7, alpha=.8) +
  scale_color_manual(values = palette3_learn,
                     labels=c("early asocial learners", "late asocial learners", "asocial non-learners")) +
  scale_fill_manual(values = palette3_learn,
                    guide = FALSE) +
  geom_ribbon(aes(ymax = avg_activity+se, 
                  ymin = avg_activity-se, 
                  fill=factor(LEARN.RATE)),
              alpha=0.5) +
  labs(x="trial", y="mean activity in 10 minutes") +
  xlim(1,22)+
  ylim(0,25) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 1),
        legend.position = c(.19,.88),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))
#stats for trial x activity x informant
asocial_learn_aov <- aov(LINECROSS.NORM ~ LEARN.RATE * TRIAL + Error(GROUP), data=asocial)
summary(asocial_learn_aov) # this works


## supplementary fig 3, panel b: activity and displacement at trial collected
#cols of interest: INFORMANT (dom, sub, male, fem)
last_trial <- read.csv("ch1_groupbeh_final_last_trial.csv", header = TRUE, sep = ",")
colnames(last_trial)

last_trial$INFORMANT <- factor(last_trial$INFORMANT, levels = c("dom", "sub", "male","female"))

ggplot(last_trial, aes(x=TRIAL, y=LINECROSS.NORM, group=INFORMANT, color=INFORMANT)) +
  geom_point(size=4, alpha=0.5)



#### locomotion by learning trial ####

ttc_loco <- read.csv(("groupbeh_learning_trials.csv"), header = TRUE, sep = ",")
colnames(ttc_loco)


ggplot(ttc_loco, aes(x=LINECROSS.NORM.LAST.4.AVG, y=TRIAL, color=INFORMANT)) +
  geom_point(size=5, alpha=0.4) +
  xlim(0,30) +
  ylim(0,25) +
  geom_hline(yintercept=22) +
  labs(x="activity (last 4 trials)", y="trial of learning response") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(breaks=c("dom", "female", "male", "sub"),
                     labels=c("dom informant groups", "individual females", "individual males", "sub informant groups"),
                     values=c("#7b3294","#a6dba0","#008837","#c2a5cf"))



## locomotion averaged by informant

avg_loco = summarySE(locomo, measurevar = "LINECROSS.NORM", groupvars = c("TRIAL","INFORMANT"))

ggplot(avg_loco, aes(x=TRIAL, y=LINECROSS.NORM, color=INFORMANT)) + 
  geom_line(size=1.5) +
  geom_errorbar(aes(ymin=LINECROSS.NORM-se, ymax=LINECROSS.NORM+se), width=1, position = position_dodge(0.05)) +
  ylim(0,30) +
  labs(x="trial",y="activity (in 5 min)", color="") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        legend.position = c(0.01,.9)) +
  scale_color_manual(breaks=c("dom", "female", "male", "sub"),
                     labels=c("dom informant groups", "individual females", "individual males", "sub informant groups"),
                     values=c("#7b3294","#a6dba0","#008837","#c2a5cf"))

## social learners and their t1-t10 activity
dom_ttc_loco<-ttc_loco[(ttc_loco$INFORMANT=="dom"),]
sub_ttc_loco<-ttc_loco[(ttc_loco$INFORMANT=="sub"),]

inf_ttc_loco<-rbind(dom_ttc_loco, sub_ttc_loco)
colnames(inf_ttc_loco)

ggplot(inf_ttc_loco, aes(x=LEARN.RATE, y=LINECROSS.NORM.T1.T10)) +
  geom_boxplot(size=1, outlier.alpha = 0) +
  geom_jitter(size=4, alpha=0.5)

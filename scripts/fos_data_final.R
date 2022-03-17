################## Neural Activity Analysis for 'Differential neural activity across learning contexts' manuscript
######### run by M. Rodriguez-Santiago, Hofmann lab 2016-2020
####



counts<-read.csv("ch1_fos_data_beh_final.csv", header = TRUE)
colnames(counts)


library(ggplot2)
library(cowplot)
library(dplyr)
library(ggrepel)
library(ggbeeswarm)
library(ggstance)
library(grid)
library(magrittr)
library(praise)
library(reshape2)
library(RColorBrewer)
library(tidyr)
library(viridis)
library(sjstats)


counts$TIMEPOINT<-as.factor(counts$TIMEPOINT)


#### filtering by group ####
counts %>%
  filter(GROUP == "dom1") -> dom1
counts %>%
  filter(GROUP == "dom2") -> dom2
counts %>%
  filter(GROUP == "dom3") -> dom3
counts %>%
  filter(GROUP == "dom4") -> dom4
counts %>%
  filter(GROUP == "dom10") -> dom10
counts %>%
  filter(GROUP == "dom11") -> dom11
counts %>%
  filter(GROUP == "dom13") -> dom13
counts %>%
  filter(GROUP == "dom5") -> dom5
counts %>%
  filter(GROUP == "dom7") -> dom7
counts %>%
  filter(GROUP == "dom8") -> dom8
counts %>%
  filter(GROUP == "dom9") -> dom9

counts %>%
  filter(GROUP == "sub1") -> sub1
counts %>%
  filter(GROUP == "sub2") -> sub2
counts %>%
  filter(GROUP == "sub3") -> sub3
counts %>%
  filter(GROUP == "sub4") -> sub4
counts %>%
  filter(GROUP == "sub10") -> sub10
counts %>%
  filter(GROUP == "sub11") -> sub11
counts %>%
  filter(GROUP == "sub12") -> sub12
counts %>%
  filter(GROUP == "sub5") -> sub5
counts %>%
  filter(GROUP == "sub6") -> sub6
counts %>%
  filter(GROUP == "sub8") -> sub8
counts %>%
  filter(GROUP == "sub9") -> sub9

all_social <- rbind(dom1, dom2, dom3, dom4, dom10, dom11, dom13, dom5, dom7, dom8, dom9, sub1, sub2, sub3, sub4, sub10, sub11, sub12, sub5, sub6, sub8, sub9)

counts %>%
  filter(GROUP == "male1") -> male1
counts %>%
  filter(GROUP == "male2") -> male2
counts %>%
  filter(GROUP == "male3") -> male3
counts %>%
  filter(GROUP == "male4") -> male4
counts %>%
  filter(GROUP == "male5") -> male5
counts %>%
  filter(GROUP == "male6") -> male6
counts %>%
  filter(GROUP == "male7") -> male7
counts %>%
  filter(GROUP == "male8") -> male8
counts %>%
  filter(GROUP == "male9") -> male9
counts %>%
  filter(GROUP == "male10") -> male10
counts %>%
  filter(GROUP == "male11") -> male11
counts %>%
  filter(GROUP == "male12") -> male12
counts %>%
  filter(GROUP == "male13") -> male13
counts %>%
  filter(GROUP == "male14") -> male14

allmales<- rbind(male1, male2, male3, male4, male5, male6, male7, male8, male9, male10, male11, male12, male13, male14)

counts %>%
  filter(GROUP == "female1") -> female1
counts %>%
  filter(GROUP == "female2") -> female2
counts %>%
  filter(GROUP == "female3") -> female3
counts %>%
  filter(GROUP == "female4") -> female4
counts %>%
  filter(GROUP == "female5") -> female5
counts %>%
  filter(GROUP == "female6") -> female6
counts %>%
  filter(GROUP == "female7") -> female7
counts %>%
  filter(GROUP == "female8") -> female8
counts %>%
  filter(GROUP == "female9") -> female9
counts %>%
  filter(GROUP == "female10") -> female10
counts %>%
  filter(GROUP == "female11") -> female11
counts %>%
  filter(GROUP == "female12") -> female12
counts %>%
  filter(GROUP == "female13") -> female13
counts %>%
  filter(GROUP == "female14") -> female14

allfem <- rbind(female1, female2, female3, female4, female5, female6, female7, female8, female9, female10, female11, female12, female13, female14)

all_asocial <- rbind(allmales, allfem)

counts %>%
  gather(REGION, FOS, 15:19) -> counts_long
counts_long$ID <- as.character(counts_long$ID)


#### figure 2: PCA for both learning contexts (social and non-social) ####

fospca <- subset(counts, select = c(-ORIENT,-MOVE,-TOTAL,-PERCENT,-APPROACHES, -APPROACH.AVG, -APPROACH.DIFF, -DISPLACEMENTS, -DISPLACE.AVG, -DISPLACE.DIFF, -AGGEFF.TRIAL, -AGGEFF.CUMUL, -LINECROSS, -LINECROSS.NORM, -LINECROSS.NORM.LAST.4.AVG, -LINECROSS.NORM.AVG, -ID,-RANK,-SEX,-LENGTH,-WEIGHT,-GROUP,-INFORMANT,-TREATMENT,-TIMEPOINT,-LEARNED,-LEARN.RATE)) %>%
  droplevels()

fospca<-as.data.frame(fospca)
colnames(fospca)

fospca %>% 
  prcomp() ->
  fos

summary(fos)
head(fos$x)

fos_data <- data.frame(fos$x, RANK=counts$RANK, INFORMANT=counts$INFORMANT, TIMEPOINT=counts$TIMEPOINT, LEARNED=counts$LEARNED, TREATMENT=counts$TREATMENT)


#rotation matrix
fos$rotation
rotation_fos <- data.frame(fos$rotation, variable=row.names(fos$rotation))
arrow_style <- arrow(length = unit(0.05, "inches"),
                     type = "closed")
# rotation matrix
ggplot(rotation_fos) +
  geom_segment(aes(xend=PC1, yend=PC2), x=0, y=0, arrow = arrow_style, size=2) +
  geom_text_repel(aes(x=PC1, y=PC2, label=variable), size=7, color='black') +
  xlim(-1.,1.) +
  ylim(-1., 1.) +
  coord_fixed() +
  theme_cowplot(font_size = 16, line_size = 1) +
  xlab("PC1 (59.6%)") +
  ylab("PC2 (22.8%)") +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12))
# percent plot
percent_all <- 100*fos$sdev^2/sum(fos$sdev^2)
percent_all
perc_all_data <- data.frame(percent=percent_all, PC=1:length(percent_all))
pca_perc_all <- ggplot(perc_all_data, aes(x=PC, y=percent)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(percent, 2)), size=4, vjust=-.5) + 
  ylim(0, 80) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))

#pca plot for all treatments
pca_all <- ggplot(fos_data, aes(x=PC1, y=PC2, color=TREATMENT, shape=LEARNED)) +
  xlab("PC1 (59.6%)") +
  ylab("PC2 (22.8%)") +
  geom_point(size=4, alpha=.9) +
  scale_colour_manual(name="context",
                      breaks=c("DEMONSTRATOR","INDIVIDUAL"),
                      labels=c("social","asocial"),
                      values=c("#af8dc3","#7fbf7b"),
                      guide = guide_legend(title.position = "top")) +
  scale_shape_manual(name="learning?",
                     breaks=c("YES","NO"),
                     labels=c("yes","no"),
                     values=c(19,17),
                     guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) 


## PCA eigenvector plot
rotated_pc <- rotation_fos %>%
  dplyr::select(PC1:PC5)
dist_rotate_pc <- dist(rotated_pc, method= "euclidean") #distance matrix
fit_dist_rotate_pc <- hclust(dist_rotate_pc, method= "average")

rotated_pc <- cbind.data.frame(trait = row.names(rotated_pc),
                               row.number = 1:nrow(rotated_pc),
                               rotated_pc)

rotated_pc$row.number <- factor(rotated_pc$row.number, 
                                levels = as.vector(fit_dist_rotate_pc$order))


rotated_pc <- arrange(rotated_pc, row.number)

trait_order <- as.vector(rotated_pc$trait)
rotated_pc$trait <- factor(rotated_pc$trait, 
                           levels = trait_order)

rotated_pc <- gather(rotated_pc,
                     "PC",
                     "eigenvector",
                     3:ncol(rotated_pc))

rotated_pc$trait <- factor(rotated_pc$trait,
                           levels = trait_order)

# Make a tile plot of eigenvectors
rotated_PCs <- ggplot(rotated_pc, aes(y=trait, x=as.factor(PC))) +
  geom_tile(aes(fill=cut(eigenvector,
                         breaks = c(-1, -0.75, -.5, -.25, 0, .25, .5, .75, 1)),
                include.lowest = TRUE),
            color = "white",
            size = 0.5) +
  scale_fill_manual(values = c("#01665e", "#35978f", "#80cdc1","#c7eae5", "white", "#f6e8c3", "#dfc27d", "#bf812d", "8c510a"),
                    labels = c("-1", "-.75","-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1"),
                    name = "Eigenvector") +
  labs(x=NULL, y=NULL) +
  theme_cowplot() +
  theme(axis.ticks=element_blank()) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  NULL
rotated_PCs


# Put the prop variation explained and eigenvectors together
fig_2_pca <- plot_grid(pca_all,
                       pca_perc_all,
                       rotated_PCs,
                       labels = "AUTO",
                       align="v", axis ="lr", 
                       ncol=3)
fig_2_pca


## box plots and statistics
ggplot(fos_data, aes(x=TIMEPOINT, y=PC1, color=TREATMENT)) + 
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(size=4, alpha=.5, position=position_jitterdodge(0.05)) +
  xlab("Trial") + 
  ylab("PC1 (59.6%)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position= c(0.1,.95),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=20),  # X axis title
        axis.title.y=element_text(size=20),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(name="context:",
                     breaks=c("DEMONSTRATOR","INDIVIDUAL"),
                     labels=c("social","non-social"),
                     values=c("#af8dc3","#7fbf7b"))
ggsave('pc1_trial.pdf', width=8, height=6, dpi=300)
#stats
pca_all_aov <- aov(PC1 ~ TREATMENT * TIMEPOINT, data=fos_data)
summary(pca_all_aov)
TukeyHSD(pca_all_aov)


ggplot(fos_data, aes(x=TIMEPOINT, y=PC2, color=TREATMENT)) + 
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(size=4, alpha=.5, position=position_jitterdodge(0.05)) +
  xlab("Trial") + 
  ylab("PC2 (22.8%)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position= "none",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=20),  # X axis title
        axis.title.y=element_text(size=20),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(name="context:",
                     breaks=c("DEMONSTRATOR","INDIVIDUAL"),
                     labels=c("social","non-social"),
                     values=c("#af8dc3","#7fbf7b"))
ggsave('pc2_trial.pdf', width=8, height=6, dpi=300)
#stats
pc2_all_aov <- aov(PC2 ~ TREATMENT * TIMEPOINT, data=fos_data)
summary(pc2_all_aov)
TukeyHSD(pc2_all_aov)


# PC1 learners v non learners across all groups
ggplot(fos_data, aes(x=TIMEPOINT, y=PC1, fill=LEARNED)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .99)) +
  xlab("Trial") + 
  ylab("PC1 (59.6%)") +
  scale_fill_manual(name="learning?",
                    breaks=c("NO","YES"),
                    labels=c("no","yes"),
                    values=c("#fc8d59","#99d594")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pc1_all_trial_learn_aov <- aov(PC1 ~ LEARNED * TIMEPOINT, data=fos_data)
summary(pc1_all_trial_learn_aov) 


#pc1 learned v not by treatment
pca_pabel_f <- ggplot(fos_data, aes(x=LEARNED, y=PC1, fill=TREATMENT)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .99)) +
  xlab("") + 
  ylab("PC1 (22.8%)") +
  scale_fill_manual(name="context:",
                    breaks=c("DEMONSTRATOR","INDIVIDUAL"),
                    labels=c("social","asocial"),
                    values=c("#af8dc3","#7fbf7b")) +
  scale_x_discrete(breaks=c("NO","YES"),
                   labels=c("no learning", "learned")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_all_pc1_lear_treat <- aov(PC1 ~ LEARNED * TREATMENT, data=fos_data)
summary(pca_all_pc1_lear_treat)  


# PC2 learners v non learners across all groups
pca_pabel_h <- ggplot(fos_data, aes(x=TIMEPOINT, y=PC2, fill=LEARNED)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .99)) +
  xlab("Trial") + 
  ylab("PC2 (22.8%)") +
  scale_fill_manual(name="learning?",
                    breaks=c("NO","YES"),
                    labels=c("no","yes"),
                    values=c("#fc8d59","#99d594")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_all_trial_learn_aov <- aov(PC2 ~ LEARNED * TIMEPOINT, data=fos_data)
summary(pca_all_trial_learn_aov)  


#PC2 social v asocial
pca_pabel_g <- ggplot(fos_data, aes(x=TIMEPOINT, y=PC2, fill=TREATMENT)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .99)) +
  xlab("Trial") + 
  ylab("PC2 (22.8%)") +
  scale_fill_manual(name="context:",
                    breaks=c("DEMONSTRATOR","INDIVIDUAL"),
                    labels=c("social","asocial"),
                    values=c("#af8dc3","#7fbf7b")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_all_trial_treat_aov <- aov(PC2 ~ TREATMENT * TIMEPOINT, data=fos_data)
summary(pca_all_trial_treat_aov)  


#pc2 learned v not by treatment
pca_pabel_i <- ggplot(fos_data, aes(x=LEARNED, y=PC2, fill=TREATMENT)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .99)) +
  xlab("") + 
  ylab("PC2 (22.8%)") +
  scale_fill_manual(name="context:",
                    breaks=c("DEMONSTRATOR","INDIVIDUAL"),
                    labels=c("social","asocial"),
                    values=c("#af8dc3","#7fbf7b")) +
  scale_x_discrete(breaks=c("NO","YES"),
                   labels=c("no learning", "learned")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_all_pc2_lear_treat <- aov(PC2 ~ LEARNED * TREATMENT, data=fos_data)
summary(pca_all_pc2_lear_treat)  


library(cowplot)  
top_row <- plot_grid(pca_pabel_a, pca_pabel_b, pca_pabel_c, labels = c('A', 'B','C'), label_size = 12, nrow=1)
mid_row <- plot_grid(pca_pabel_d, pca_pabel_e, pca_pabel_f, labels = c('D', 'E','F'), label_size = 12, nrow=1)
bottom_row <- plot_grid(pca_pabel_g, pca_pabel_h, pca_pabel_i, labels = c('G', 'H','I'), label_size = 12, nrow=1)



#### supplemental: PCA social context subset ####

socialpca <- subset(all_social, select = c(-ID, -RANK, -SEX, -LENGTH, -WEIGHT, -GROUP, -INFORMANT, -TREATMENT, -TIMEPOINT, -ORIENT,-MOVE,-TOTAL,-PERCENT,-LEARNED, -LEARN.RATE, -APPROACHES, -APPROACH.AVG, -APPROACH.DIFF, -DISPLACEMENTS, -DISPLACE.AVG, -DISPLACE.DIFF, -AGGEFF.TRIAL, -AGGEFF.CUMUL, -LINECROSS, -LINECROSS.NORM, -LINECROSS.NORM.LAST.4.AVG, -LINECROSS.NORM.AVG)) %>%
  droplevels()

socialpca<-as.data.frame(socialpca)
colnames(socialpca)

socialpca %>% 
  prcomp() ->
  socialfos

summary(socialfos)
head(socialfos$x)

socialfos_data <- data.frame(socialfos$x, RANK=all_social$RANK, INFORMANT=all_social$INFORMANT, TIMEPOINT=all_social$TIMEPOINT, LEARNED=all_social$LEARNED, TREATMENT=all_social$TREATMENT)


#rotation matrix
socialfos$rotation
rotation_social_fos <- data.frame(socialfos$rotation, variable=row.names(socialfos$rotation))
arrow_style <- arrow(length = unit(0.05, "inches"),
                     type = "closed")
# rotation matrix
rotation_social <- ggplot(rotation_social_fos) +
  geom_segment(aes(xend=PC1, yend=PC2), x=0, y=0, arrow = arrow_style, size=2) +
  geom_text_repel(aes(x=PC1, y=PC2, label=variable), size=7, color='black') +
  xlim(-1.,1.) +
  ylim(-1., 1.) +
  coord_fixed() +
  theme_cowplot(font_size = 16, line_size = 1) +
  xlab("PC1 (43.98%)") +
  ylab("PC2 (30.2%)") +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) 

# skree plot
percent_soc <- 100*socialfos$sdev^2/sum(socialfos$sdev^2)
percent_soc
perc_data <- data.frame(percent=percent_soc, PC=1:length(percent_soc))
skree_social <- ggplot(perc_data, aes(x=PC, y=percent)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(percent, 2)), size=4, vjust=-.5) + 
  ylim(0, 80) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))

# social 
social_a <- ggplot(socialfos_data, aes(x=PC1, y=PC2, color=INFORMANT, shape=LEARNED)) +
  xlab("PC1 (43.98%)") +
  ylab("PC2 (30.2%)") +
  geom_point(size=4, alpha=.9) +
  scale_colour_manual(name="informant status",
                      breaks=c("DOM","SUB"),
                      labels=c("dominant male","subordinate male"),
                      values=c("#7b3294","#c2a5cf"),
                      guide = guide_legend(title.position = "top")) +
  scale_shape_manual(name="learning?",
                     breaks=c("YES","NO"),
                     labels=c("yes","no"),
                     values=c(19,17),
                     guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))

social_d <- ggplot(socialfos_data, aes(x=TIMEPOINT, y=PC1, fill=INFORMANT)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("Trial") + 
  ylab("PC1 (43.98%)") +
  scale_fill_manual(name="informant status",
                    breaks=c("DOM","SUB"),
                    labels=c("dominant male","subordinate male"),
                    values=c("#7b3294","#c2a5cf"),
                    guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_social_d <- aov(PC1 ~ INFORMANT * TIMEPOINT, data=socialfos_data)
summary(pca_social_d)

social_e <- ggplot(socialfos_data, aes(x=TIMEPOINT, y=PC1, fill=LEARNED)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("Trial") + 
  ylab("PC1 (43.98%)") +
  scale_fill_manual(name="learning?",
                    breaks=c("NO","YES"),
                    labels=c("no","yes"),
                    values=c("#fc8d59","#99d594"),
                    guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_social_e <- aov(PC1 ~ LEARNED * TIMEPOINT, data=socialfos_data)
summary(pca_social_e)

social_f <- ggplot(socialfos_data, aes(x=LEARNED, y=PC1, fill=INFORMANT)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("") + 
  ylab("PC1 (43.98%)") +
  scale_fill_manual(name="informant status",
                    breaks=c("DOM","SUB"),
                    labels=c("dominant male","subordinate male"),
                    values=c("#7b3294","#c2a5cf"),
                    guide = guide_legend(title.position = "top")) +
  scale_x_discrete(breaks=c("NO","YES"),
                   labels=c("no learning", "learned")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_social_f <- aov(PC1 ~ LEARNED * INFORMANT, data=socialfos_data)
summary(pca_social_f)

#pc2
social_g <- ggplot(socialfos_data, aes(x=TIMEPOINT, y=PC2, fill=INFORMANT)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("Trial") + 
  ylab("PC2 (30.2%)") +
  scale_fill_manual(name="informant status",
                    breaks=c("DOM","SUB"),
                    labels=c("dominant male","subordinate male"),
                    values=c("#7b3294","#c2a5cf"),
                    guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_social_g <- aov(PC2 ~ TIMEPOINT * INFORMANT, data=socialfos_data)
summary(pca_social_g)

social_h <- ggplot(socialfos_data, aes(x=TIMEPOINT, y=PC2, fill=LEARNED)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("Trial") + 
  ylab("PC2 (30.2%)") +
  scale_fill_manual(name="learning?",
                    breaks=c("NO","YES"),
                    labels=c("no","yes"),
                    values=c("#fc8d59","#99d594"),
                    guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_social_h <- aov(PC2 ~ TIMEPOINT * LEARNED, data=socialfos_data)
summary(pca_social_h)

social_i <- ggplot(socialfos_data, aes(x=LEARNED, y=PC2, fill=INFORMANT)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("") + 
  ylab("PC2 (30.2%)") +
  scale_fill_manual(name="informant status",
                    breaks=c("DOM","SUB"),
                    labels=c("dominant male","subordinate male"),
                    values=c("#7b3294","#c2a5cf"),
                    guide = guide_legend(title.position = "top")) +
  scale_x_discrete(breaks=c("NO","YES"),
                   labels=c("no learning", "learned")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_social_i <- aov(PC2 ~ INFORMANT * LEARNED, data=socialfos_data)
summary(pca_social_i)

social_top <- plot_grid(social_a, rotation_social, skree_social, labels = c('A', 'B','C'), label_size = 12, nrow=1)
social_mid <- plot_grid(social_d, social_e, social_f, labels = c('D', 'E','F'), label_size = 12, nrow=1)
social_bottom <- plot_grid(social_g, social_h, social_i, labels = c('G', 'H','I'), label_size = 12, nrow=1)


## PCA eigenvector plot
rotated_pc_social <- rotation_social_fos %>%
  dplyr::select(PC1:PC5)
dist_rotate_pc_social <- dist(rotated_pc_social, method= "euclidean") #distance matrix
fit_dist_rotate_pc_social <- hclust(dist_rotate_pc_social, method= "average")

rotated_pc_social <- cbind.data.frame(trait = row.names(rotated_pc_social),
                                      row.number = 1:nrow(rotated_pc_social),
                                      rotated_pc_social)

rotated_pc_social$row.number <- factor(rotated_pc_social$row.number, 
                                       levels = as.vector(fit_dist_rotate_pc_social$order))


rotated_pc_social <- arrange(rotated_pc_social, row.number)

trait_order_social <- as.vector(rotated_pc_social$trait)
rotated_pc_social$trait <- factor(rotated_pc_social$trait, 
                                  levels = trait_order_social)

rotated_pc_social <- gather(rotated_pc_social,
                            "PC",
                            "eigenvector",
                            3:ncol(rotated_pc_social))

rotated_pc_social$trait <- factor(rotated_pc_social$trait,
                                  levels = trait_order_social)

# Make a tile plot of eigenvectors
rotated_PCs_social <- ggplot(rotated_pc_social, aes(y=trait, x=as.factor(PC))) +
  geom_tile(aes(fill=cut(eigenvector,
                         breaks = c(-1, -0.75, -.5, -.25, 0, .25, .5, .75, 1)),
                include.lowest = TRUE),
            color = "white",
            size = 0.5) +
  scale_fill_manual(values = c("#01665e", "#35978f", "#80cdc1","#c7eae5", "white", "#f6e8c3", "#dfc27d", "#bf812d", "8c510a"),
                    labels = c("-1", "-.75","-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1"),
                    name = "Eigenvector") +
  labs(x=NULL, y=NULL) +
  theme_cowplot() +
  theme(axis.ticks=element_blank()) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  NULL
rotated_PCs_social


# Put the prop variation explained and eigenvectors together
supp_social <- plot_grid(social_a,
                         skree_social,
                         rotated_PCs_social,
                         labels = "AUTO",
                         align="v", axis ="lr", 
                         ncol=3)
supp_social



#### supplemental: PCA asocial context ####

asocialpca <- subset(all_asocial, select = c(-ID, -RANK, -SEX, -LENGTH, -WEIGHT, -GROUP, -INFORMANT, -TREATMENT, -TIMEPOINT, -ORIENT,-MOVE,-TOTAL,-PERCENT,-LEARNED, -LEARN.RATE, -APPROACHES, -APPROACH.AVG, -APPROACH.DIFF, -DISPLACEMENTS, -DISPLACE.AVG, -DISPLACE.DIFF, -AGGEFF.TRIAL, -AGGEFF.CUMUL, -LINECROSS, -LINECROSS.NORM, -LINECROSS.NORM.LAST.4.AVG, -LINECROSS.NORM.AVG)) %>%
  droplevels()

asocialpca<-as.data.frame(asocialpca)
colnames(asocialpca)

asocialpca %>% 
  prcomp() ->
  asocialfos

summary(asocialfos)
head(asocialfos$x)

asocialfos_data <- data.frame(asocialfos$x, SEX=all_asocial$SEX, TIMEPOINT=all_asocial$TIMEPOINT, LEARNED=all_asocial$LEARNED, TREATMENT=all_asocial$TREATMENT)

#rotation matrix for asocial
asocialfos$rotation
rotation_asocial_fos <- data.frame(asocialfos$rotation, variable=row.names(asocialfos$rotation))
arrow_style <- arrow(length = unit(0.05, "inches"),
                     type = "closed")
# rotation matrix
rotation_asocial <- ggplot(rotation_asocial_fos) +
  geom_segment(aes(xend=PC1, yend=PC2), x=0, y=0, arrow = arrow_style, size=2) +
  geom_text_repel(aes(x=PC1, y=PC2, label=variable), size=7, color='black') +
  xlim(-1.,1.) +
  ylim(-1., 1.) +
  coord_fixed() +
  theme_cowplot(font_size = 16, line_size = 1) +
  xlab("PC1 (53.97%)") +
  ylab("PC2 (31%)") +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) 

# skree plot
percent_asoc <- 100*asocialfos$sdev^2/sum(asocialfos$sdev^2)
percent_asoc
perc_data_asoc <- data.frame(percent=percent_asoc, PC=1:length(percent_asoc))
skree_asocial <- ggplot(perc_data_asoc, aes(x=PC, y=percent)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(percent, 2)), size=4, vjust=-.5) + 
  ylim(0, 80) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))

# asocial plot 
asocial_a <- ggplot(asocialfos_data, aes(x=PC1, y=PC2, color=SEX)) +
  xlab("PC1 (53.97%)") +
  ylab("PC2 (31%)") +
  geom_point(size=4, alpha=.7, aes(shape=LEARNED)) +
  scale_colour_manual(values=c("#008837","#a6dba0"),
                      name="sex",
                      breaks=c("male","female"),
                      labels=c("male","female"),
                      guide = guide_legend(title.position = "top")) +
  scale_shape_manual(name="learning?",
                     breaks=c("YES","NO"),
                     labels=c("yes","no"),
                     values=c(19,17),
                     guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12))


asocial_d <- ggplot(asocialfos_data, aes(x=TIMEPOINT, y=PC1, fill=SEX)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("Trial") + 
  ylab("PC1 (53.97%)") +
  scale_fill_manual(values=c("#008837","#a6dba0"),
                    name="sex",
                    breaks=c("male","female"),
                    labels=c("male","female"),
                    guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_asocial_d <- aov(PC1 ~ SEX * TIMEPOINT, data=asocialfos_data)
summary(pca_asocial_d)

asocial_e <- ggplot(asocialfos_data, aes(x=TIMEPOINT, y=PC1, fill=LEARNED)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("Trial") + 
  ylab("PC1 (53.97%)") +
  scale_fill_manual(name="learning?",
                    breaks=c("NO","YES"),
                    labels=c("no","yes"),
                    values=c("#fc8d59","#99d594"),
                    guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_asocial_e <- aov(PC1 ~ LEARNED * TIMEPOINT, data=asocialfos_data)
summary(pca_asocial_e)

asocial_f <- ggplot(asocialfos_data, aes(x=LEARNED, y=PC1, fill=SEX)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("") + 
  ylab("PC1 (53.97%)") +
  scale_fill_manual(values=c("#008837","#a6dba0"),
                    name="sex",
                    breaks=c("male","female"),
                    labels=c("male","female"),
                    guide = guide_legend(title.position = "top")) +
  scale_x_discrete(breaks=c("NO","YES"),
                   labels=c("no learning", "learned")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_asocial_f <- aov(PC1 ~ LEARNED * SEX, data=asocialfos_data)
summary(pca_asocial_f)

#pc2
asocial_g <- ggplot(asocialfos_data, aes(x=TIMEPOINT, y=PC2, fill=SEX)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("Trial") + 
  ylab("PC2 (31%%)") +
  scale_fill_manual(values=c("#008837","#a6dba0"),
                    name="sex",
                    breaks=c("male","female"),
                    labels=c("male","female"),
                    guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_asocial_g <- aov(PC2 ~ SEX * TIMEPOINT, data=asocialfos_data)
summary(pca_asocial_g)

asocial_h <- ggplot(asocialfos_data, aes(x=TIMEPOINT, y=PC2, fill=LEARNED)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("Trial") + 
  ylab("PC2 (31%)") +
  scale_fill_manual(name="learning?",
                    breaks=c("NO","YES"),
                    labels=c("no","yes"),
                    values=c("#fc8d59","#99d594"),
                    guide = guide_legend(title.position = "top")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_asocial_h <- aov(PC2 ~ LEARNED * TIMEPOINT, data=asocialfos_data)
summary(pca_asocial_h)

asocial_i <- ggplot(asocialfos_data, aes(x=LEARNED, y=PC2, fill=SEX)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("") + 
  ylab("PC2 (31%)") +
  scale_fill_manual(values=c("#008837","#a6dba0"),
                    name="sex",
                    breaks=c("male","female"),
                    labels=c("male","female"),
                    guide = guide_legend(title.position = "top")) +
  scale_x_discrete(breaks=c("NO","YES"),
                   labels=c("no learning", "learned")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.box = "horizontal",
        legend.position="top",
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis text
        axis.text.y=element_text(size=12))
#stats
pca_asocial_i <- aov(PC2 ~ LEARNED * SEX, data=asocialfos_data)
summary(pca_asocial_i)


asocial_top <- plot_grid(asocial_a, rotation_asocial, skree_asocial, labels = c('A', 'B','C'), label_size = 12, nrow=1)
asocial_mid <- plot_grid(asocial_d, asocial_e, asocial_f, labels = c('D', 'E','F'), label_size = 12, nrow=1)
asocial_bottom <- plot_grid(asocial_g, asocial_h, asocial_i, labels = c('G', 'H','I'), label_size = 12, nrow=1)


## PCA eigenvector plot
rotated_pc_asocial <- rotation_asocial_fos %>%
  dplyr::select(PC1:PC5)
dist_rotate_pc_asocial <- dist(rotated_pc_asocial, method= "euclidean") #distance matrix
fit_dist_rotate_pc_asocial <- hclust(dist_rotate_pc_asocial, method= "average")

rotated_pc_asocial <- cbind.data.frame(trait = row.names(rotated_pc_asocial),
                                       row.number = 1:nrow(rotated_pc_asocial),
                                       rotated_pc_asocial)

rotated_pc_asocial$row.number <- factor(rotated_pc_asocial$row.number, 
                                        levels = as.vector(fit_dist_rotate_pc_asocial$order))


rotated_pc_asocial <- arrange(rotated_pc_asocial, row.number)

trait_order_asocial <- as.vector(rotated_pc_asocial$trait)
rotated_pc_asocial$trait <- factor(rotated_pc_asocial$trait, 
                                   levels = trait_order_asocial)

rotated_pc_asocial <- gather(rotated_pc_asocial,
                             "PC",
                             "eigenvector",
                             3:ncol(rotated_pc_asocial))

rotated_pc_asocial$trait <- factor(rotated_pc_asocial$trait,
                                   levels = trait_order_asocial)

# Make a tile plot of eigenvectors
rotated_PCs_asocial <- ggplot(rotated_pc_asocial, aes(y=trait, x=as.factor(PC))) +
  geom_tile(aes(fill=cut(eigenvector,
                         breaks = c(-1, -0.75, -.5, -.25, 0, .25, .5, .75, 1)),
                include.lowest = TRUE),
            color = "white",
            size = 0.5) +
  scale_fill_manual(values = c("#01665e", "#35978f", "#80cdc1","#c7eae5", "white", "#f6e8c3", "#dfc27d", "#bf812d", "8c510a"),
                    labels = c("-1", "-.75","-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1"),
                    name = "Eigenvector") +
  labs(x=NULL, y=NULL) +
  theme_cowplot() +
  theme(axis.ticks=element_blank()) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  NULL
rotated_PCs_asocial


# Put the prop variation explained and eigenvectors together
supp_asocial <- plot_grid(asocial_a,
                          skree_asocial,
                          rotated_PCs_asocial,
                          labels = "AUTO",
                          align="v", axis ="lr", 
                          ncol=3)
supp_asocial


#### figure 3: fos across trials by 'context' and 'learning' + statistics ####

dlg_timepoint <- ggplot(counts, aes(x=TIMEPOINT, y=DLG, color=TREATMENT)) +
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(position=position_jitterdodge(0.1),alpha=0.6, size=4) +
  labs(x="trial", y="Fos+ Dlg") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position= c(0.2,.95),
        legend.text = element_text(size=12),
        legend.title=element_text("none"),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

ggplot(counts, aes(x=LEARNED, y=DLG, color=TREATMENT)) +
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(position=position_jitterdodge(0.1),alpha=0.6, size=4) +
  labs(x="", y="Fos+ Dlg") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position= c(0.2,.95),
        legend.text = element_text(size=12),
        legend.title=element_text("none"),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

## dm-1
dm1_timepoint <- ggplot(counts, aes(x=TIMEPOINT, y=DM1, color=TREATMENT)) +
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(position=position_jitterdodge(0.1),alpha=0.6, size=4) +
  labs(x="trial", y="Fos+ Dm-1") +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

ggplot(counts, aes(x=LEARNED, y=DM1, color=TREATMENT)) +
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(position=position_jitterdodge(0.1),alpha=0.6, size=4) +
  labs(x="", y="Fos+ Dm-1") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position= c(0.2,.95),
        legend.text = element_text(size=12),
        legend.title=element_text("none"),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))


# dm3
dm3_timepoint <- ggplot(counts, aes(x=TIMEPOINT, y=DM3, color=TREATMENT)) +
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(position=position_jitterdodge(0.1),alpha=0.6, size=4) +
  labs(x="trial", y="Fos+ Dm-3") +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

ggplot(counts, aes(x=LEARNED, y=DM3, color=TREATMENT)) +
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(position=position_jitterdodge(0.1),alpha=0.6, size=4) +
  labs(x="", y="Fos+ Dm-3") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position= c(0.2,.95),
        legend.text = element_text(size=12),
        legend.title=element_text("none"),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))


# dlv
dlv_timepoint <- ggplot(counts, aes(x=TIMEPOINT, y=DLV, color=TREATMENT)) +
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(position=position_jitterdodge(0.1),alpha=0.6, size=4) +
  labs(x="trial", y="Fos+ Dlv") +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

ggplot(counts, aes(x=LEARNED, y=DLV, color=TREATMENT)) +
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(position=position_jitterdodge(0.1),alpha=0.6, size=4) +
  labs(x="", y="Fos+ Dlv") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position= c(0.2,.95),
        legend.text = element_text(size=12),
        legend.title=element_text("none"),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))


# vs
vs_timepoint <- ggplot(counts, aes(x=TIMEPOINT, y=VS, color=TREATMENT)) +
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(position=position_jitterdodge(0.1),alpha=0.6, size=4) +
  labs(x="trial", y="Fos+ Vs") +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

ggplot(counts, aes(x=LEARNED, y=VS, color=TREATMENT)) +
  geom_boxplot(size=1,outlier.size = 0) +
  geom_point(position=position_jitterdodge(0.1),alpha=0.6, size=4) +
  labs(x="", y="Fos+ Vs") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = .7),
        legend.position= c(0.2,.95),
        legend.text = element_text(size=12),
        legend.title=element_text("none"),
        axis.title.x=element_text(size=16),  # X axis title
        axis.title.y=element_text(size=16),  # Y axis title
        axis.text.x=element_text(size=12),  # X axis text
        axis.text.y=element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))


# dlg ANOVA
dlg.time.anova.i <- aov(DLG ~ TIMEPOINT*TREATMENT, data = counts) 
summary(dlg.time.anova.i)
TukeyHSD(dlg.time.anova.i)
effectsize::cohens_f(dlg.time.anova.i)

dlg.learn.anova.i <- aov(DLG ~ LEARNED*TREATMENT, data = counts) 
summary(dlg.learn.anova.i)
TukeyHSD(dlg.learn.anova.i)
effectsize::cohens_f(dlg.learn.anova.i)

# dlv ANOVA
dlv.time.anova.i <- aov(DLV ~ TIMEPOINT*TREATMENT, data = counts) 
summary(dlv.time.anova.i)
TukeyHSD(dlv.time.anova.i)
effectsize::cohens_f(dlv.time.anova.i)

dlv.learn.anova.i <- aov(DLV ~ LEARNED*TREATMENT, data = counts) 
summary(dlv.learn.anova.i)
TukeyHSD(dlv.learn.anova.i)
effectsize::cohens_f(dlv.learn.anova.i)


# dm1 ANOVA
dm1.time.anova.i <- aov(DM1 ~ TIMEPOINT*TREATMENT, data = counts) 
summary(dm1.time.anova.i)
TukeyHSD(dm1.time.anova.i)
effectsize::cohens_f(dm1.time.anova.i)

dm1.learn.anova.i <- aov(DM1 ~ LEARNED*TREATMENT, data = counts) 
summary(dm1.learn.anova.i)
TukeyHSD(dm1.learn.anova.i)
effectsize::cohens_f(dm1.learn.anova.i)


# dm3 ANOVA
dm3.time.anova.i <- aov(DM3 ~ TIMEPOINT*TREATMENT, data = counts) 
summary(dm3.time.anova.i)
TukeyHSD(dm3.time.anova.i)
effectsize::cohens_f(dm3.time.anova.i)

dm3.learn.anova.i <- aov(DM3 ~ LEARNED*TREATMENT, data = counts) 
summary(dm3.learn.anova.i)
TukeyHSD(dm3.learn.anova.i)
effectsize::cohens_f(dm3.learn.anova.i)


# vs ANOVA
vs.time.anova.i <- aov(VS ~ TIMEPOINT*TREATMENT, data = counts) 
summary(vs.time.anova.i)
TukeyHSD(vs.time.anova.i)
effectsize::cohens_f(vs.time.anova.i)

vs.learn.anova.i <- aov(VS ~ LEARNED*TREATMENT, data = counts) 
summary(vs.time.anova.i)
TukeyHSD(vs.time.anova.i)
effectsize::cohens_f(vs.learn.anova.i)

#### fos expression by region correlation with tank movement ####

norm_linecross_avg <- lm(DLG ~ LINECROSS.NORM.AVG, data = counts)
ggplot(counts, aes(x=LINECROSS.NORM.AVG, y=DLG)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(norm_linecross_avg)$adj.r.squared, 2),
                     "Intercept =",signif(norm_linecross_avg$coef[[1]],2 ),
                     " P =",signif(summary(norm_linecross_avg)$coef[2,4], 2)), size=12)

norm_linecross_avg <- lm(DM1 ~ LINECROSS.NORM.AVG, data = counts)
ggplot(counts, aes(x=LINECROSS.NORM.AVG, y=DM1)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(norm_linecross_avg)$adj.r.squared, 2),
                     "Intercept =",signif(norm_linecross_avg$coef[[1]],2 ),
                     " P =",signif(summary(norm_linecross_avg)$coef[2,4], 2)), size=12)

norm_linecross_avg <- lm(VS ~ LINECROSS.NORM.AVG, data = counts)
ggplot(counts, aes(x=LINECROSS.NORM.AVG, y=VS)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(norm_linecross_avg)$adj.r.squared, 2),
                     "Intercept =",signif(norm_linecross_avg$coef[[1]],2 ),
                     " P =",signif(summary(norm_linecross_avg)$coef[2,4], 2)), size=12)

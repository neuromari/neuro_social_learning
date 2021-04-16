########################## Survival Analysis for 'Differential neural activity across learning contexts' manuscript
############# run by M. Rodriguez-Santiago, Hofmann lab 2016-2020

counts<-read.csv("fos_data_beh.csv", header = TRUE)
colnames(counts)


library(ggplot2)
library(cowplot)
library(dplyr)
library(ggrepel)

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

counts %>%
  gather(REGION, FOS, 15:19) -> counts_long
counts_long$ID <- as.character(counts_long$ID)

#### fos by 'learn.rate' across all five brain regions ####

dlg.learn <- ggplot(counts, aes(x=LEARN.RATE, y=DLG)) +
  geom_boxplot(size=0.8, outlier.alpha = 0) +
  geom_jitter(width = 0.2, alpha=0.5, size=3) +
  labs(x="learn phase", y="dlg fos est. pop/um") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12))

dlv.learn <- ggplot(counts, aes(x=LEARN.RATE, y=DLV)) +
  geom_boxplot(size=0.8, outlier.alpha = 0) +
  geom_jitter(width = 0.2, alpha=0.5, size=3) +
  labs(x="learn phase", y="dlv fos est. pop/um") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12))

dm1.learn <- ggplot(counts, aes(x=LEARN.RATE, y=DM1)) +
  geom_boxplot(size=0.8, outlier.alpha = 0) +
  geom_jitter(width = 0.2, alpha=0.5, size=3) +
  labs(x="learn phase", y="dm-1 fos est. pop/um") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12))

dm3.learn <- ggplot(counts, aes(x=LEARN.RATE, y=DM3)) +
  geom_boxplot(size=0.8, outlier.alpha = 0) +
  geom_jitter(width = 0.2, alpha=0.5, size=3) +
  labs(x="learn phase", y="dm-3 fos est. pop/um") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12))

vs.learn <- ggplot(counts, aes(x=LEARN.RATE, y=VS)) +
  geom_boxplot(size=0.8, outlier.alpha = 0) +
  geom_jitter(width = 0.2, alpha=0.5, size=3) +
  labs(x="learn phase", y="vs fos est. pop/um") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12))

plot_grid(dlg.learn, dm1.learn, vs.learn, dlv.learn, dm3.learn, labels = c('A','B','C','D','E'), label_size = 16)


#anova
res.dlg.learn.rate<-aov(DLG ~ LEARN.RATE, data=counts)
summary(res.dlg.learn.rate)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#LEARN.RATE   2 0.002431 0.0012154   26.07 9.83e-10 ***
#Residuals   94 0.004382 0.0000466

res.dlv.learn.rate<-aov(DLV ~ LEARN.RATE, data=counts)
summary(res.dlv.learn.rate)  
#             Df  Sum Sq   Mean Sq  F value   Pr(>F)    
#LEARN.RATE   2 0.0003709 1.854e-04   7.608 0.000866 ***
#Residuals   94 0.0022911 2.437e-05 

res.dm1.learn.rate<-aov(DM1 ~ LEARN.RATE, data=counts)
summary(res.dm1.learn.rate)  
#             Df  Sum Sq   Mean Sq  F value   Pr(>F)    
#LEARN.RATE   2 0.000832 4.16e-04   4.929 0.00921 **
#Residuals   94 0.007933 8.44e-05 

res.dm3.learn.rate<-aov(DM3 ~ LEARN.RATE, data=counts)
summary(res.dm3.learn.rate)  
#             Df  Sum Sq   Mean Sq  F value   Pr(>F)    
#LEARN.RATE   2 0.0003388 1.694e-04   5.768 0.00434 **
#Residuals   94 0.0027606 2.937e-05

res.vs.learn.rate<-aov(VS ~ LEARN.RATE, data=counts)
summary(res.vs.learn.rate)  
#             Df  Sum Sq   Mean Sq  F value   Pr(>F)    
#LEARN.RATE   2 0.002941 0.0014705   15.75 1.26e-06 ***
#Residuals   94 0.008778 0.0000934


#### fos by 'learn.rate' across all five brain regions by treatment ####

dlg.learn.context <- ggplot(counts, aes(x=LEARN.RATE, y=DLG, color=TREATMENT)) +
  geom_boxplot(size=1, outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.7,size=3) +
  labs(x="learn phase", y="dlg fos est. pop/um") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

dlv.learn.context <- ggplot(counts, aes(x=LEARN.RATE, y=DLV, color=TREATMENT)) +
  geom_boxplot(size=1, outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.7,size=3) +
  labs(x="learn phase", y="dlv fos est. pop/um") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

dm1.learn.context <- ggplot(counts, aes(x=LEARN.RATE, y=DM1, color=TREATMENT)) +
  geom_boxplot(size=1, outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.7,size=3) +
  labs(x="learn phase", y="dm-1 fos est. pop/um") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

dm3.learn.context <- ggplot(counts, aes(x=LEARN.RATE, y=DM3, color=TREATMENT)) +
  geom_boxplot(size=1, outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.7,size=3) +
  labs(x="learn phase", y="dm-3 fos est. pop/um") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

vs.learn.context <- ggplot(counts, aes(x=LEARN.RATE, y=VS, color=TREATMENT)) +
  geom_boxplot(size=1, outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.7,size=3) +
  labs(x="learn phase", y="vs fos est. pop/um") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("social context", 
                                "asocial context"), 
                     values = c("DEMONSTRATOR"="#af8dc3", 
                                "INDIVIDUAL"="#7fbf7b"))

plot_grid(dlg.learn.context, dm1.learn.context, vs.learn.context, dlv.learn.context, dm3.learn.context, labels = c("A","B","C","D","E"), label_size=14)

#### COMPARISONS BETWEEN TIMEPOINT and SOCIAL RANK and FOS + statistics ####

counts %>%
  filter(RANK == "dom"| RANK=="fem"| RANK=="sub") -> groups_fos
counts %>%
  filter(RANK=="male" | RANK == "female") -> individuals_fos


  #groups dlg
dlg_rank_groups <- ggplot(groups_fos, aes(x=TIMEPOINT, y=DLG, color=RANK)) +
  geom_boxplot(size=1.5,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.7, size=4.5) +
  labs(x="previous trial", y="dlg(hipp) fos est. pop/um") +
  theme(legend.position = c(0.005,.98)) +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_manual(name="", 
                     labels = c("dom males", 
                                "females",
                                "sub males"), 
                     values = c("dom"="#980043", 
                                "fem"="#d7b5d8",
                                "sub"="#df65b0"))

dlg_rank_groups
#two group anova
res.dlg.rank<-aov(DLG ~ TIMEPOINT + RANK, data=groups_fos)
summary(res.dlg.rank)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 0.0004047 2.024e-04   7.512 0.00117 **
#RANK         2 0.0000727 3.635e-05   1.349 0.26675   
#Residuals   64 0.0017242 2.694e-05
res.dlg.rank.i <- aov(DLG ~ TIMEPOINT*RANK, data = groups_fos) 
summary(res.dlg.rank.i)
#                  Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT       2 0.0004047 2.024e-04   7.254 0.00151 **
#RANK            2 0.0000727 3.635e-05   1.303 0.27935   
#TIMEPOINT:RANK  4 0.0000503 1.257e-05   0.451 0.77159   
#Residuals      60 0.0016739 2.790e-05


dlv_rank_groups <- ggplot(groups_fos, aes(x=TIMEPOINT, y=DLV, color=RANK)) +
  geom_boxplot(size=1.5,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.7, size=4.5) +
  labs(x="previous trial", y="dlv fos est. pop/um") +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_manual(name="", 
                     labels = c("dom males", 
                                "females",
                                "sub males"), 
                     values = c("dom"="#980043", 
                                "fem"="#d7b5d8",
                                "sub"="#df65b0"))

#two group anova
res.dlv.rank<-aov(DLV ~ TIMEPOINT + RANK, data=groups_fos)
summary(res.dlv.rank)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 0.0001229 6.147e-05   5.246 0.00777 **
#RANK         2 0.0000456 2.282e-05   1.948 0.15095   
#Residuals   64 0.0007499 1.172e-05
res.dlv.rank.i <- aov(DLV ~ TIMEPOINT*RANK, data = groups_fos) 
summary(res.dlv.rank.i)
#                  Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT       2 0.0001229 6.147e-05   6.429 0.00295 **
#RANK            2 0.0000456 2.282e-05   2.387 0.10060   
#TIMEPOINT:RANK  4 0.0001762 4.405e-05   4.607 0.00261 **
#Residuals      60 0.0005737 9.560e-06


dm1_rank_groups <- ggplot(groups_fos, aes(x=TIMEPOINT, y=DM1, color=RANK)) +
  geom_boxplot(size=1.5,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.7, size=4.5) +
  labs(x="previous trial", y="dm-1 fos est. pop/um") +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_manual(name="", 
                     labels = c("dom males", 
                                "females",
                                "sub males"), 
                     values = c("dom"="#980043", 
                                "fem"="#d7b5d8",
                                "sub"="#df65b0"))
dm1_rank_groups

#two group anova
res.dm1.rank<-aov(DM1 ~ TIMEPOINT + RANK, data=groups_fos)
summary(res.dm1.rank)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 0.003867 0.0019334  57.370 5.33e-15 ***
#RANK         2 0.000089 0.0000445   1.321    0.274    
#Residuals   64 0.002157 0.0000337
res.dm1.rank.i <- aov(DM1 ~ TIMEPOINT*RANK, data = groups_fos) 
summary(res.dm1.rank.i)
#                  Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT          2 0.003867 0.0019334  56.652 1.51e-14 ***
#RANK            2 0.000089 0.0000445   1.305    0.279    
#TIMEPOINT:RANK  4 0.000109 0.0000273   0.800    0.530    
#Residuals      60 0.002048 0.0000341 

dm3_rank_groups <- ggplot(groups_fos, aes(x=TIMEPOINT, y=DM3, color=RANK)) +
  geom_boxplot(size=1.5,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.7, size=4.5) +
  labs(x="previous trial", y="dm-3 fos est. pop/um") +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_manual(name="", 
                     labels = c("dom males", 
                                "females",
                                "sub males"), 
                     values = c("dom"="#980043", 
                                "fem"="#d7b5d8",
                                "sub"="#df65b0"))
dm3_rank_groups

#two group anova
res.dm3.rank<-aov(DM3 ~ TIMEPOINT + RANK, data=groups_fos)
summary(res.dm3.rank)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 0.0001923 9.616e-05   4.245 0.0186 *
#RANK         2 0.0000212 1.060e-05   0.468 0.6284  
#Residuals   64 0.0014498 2.265e-05
res.dm3.rank.i <- aov(DM3 ~ TIMEPOINT*RANK, data = groups_fos) 
summary(res.dm3.rank.i)
#                  Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT         2 0.0001923 9.616e-05   4.083 0.0218 *
#RANK            2 0.0000212 1.060e-05   0.450 0.6397  
#TIMEPOINT:RANK  4 0.0000367 9.180e-06   0.390 0.8153  
#Residuals      60 0.0014131 2.355e-05


vs_rank_groups <- ggplot(groups_fos, aes(x=TIMEPOINT, y=VS, color=RANK)) +
  geom_boxplot(size=1.5,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.7, size=4.5) +
  labs(x="previous trial", y="vs fos est. pop/um") +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_manual(name="", 
                     labels = c("dom males", 
                                "females",
                                "sub males"), 
                     values = c("dom"="#980043", 
                                "fem"="#d7b5d8",
                                "sub"="#df65b0"))
vs_rank_groups

#two group anova
res.vs.rank<-aov(VS ~ TIMEPOINT + RANK, data=groups_fos)
summary(res.vs.rank)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 0.001248 0.0006241   8.204 0.000673 ***
#RANK         2 0.000090 0.0000450   0.592 0.556121    
#Residuals   64 0.004868 0.0000761
res.vs.rank.i <- aov(VS ~ TIMEPOINT*RANK, data = groups_fos) 
summary(res.vs.rank.i)
#                  Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT         2 0.001248 0.0006241   7.959 0.000859 ***
#RANK            2 0.000090 0.0000450   0.575 0.566041    
#TIMEPOINT:RANK  4 0.000164 0.0000410   0.523 0.719275    
#Residuals      60 0.004704 0.0000784

plot_grid(dlg_rank_groups, dm1_rank_groups, vs_rank_groups, dlv_rank_groups, dm3_rank_groups, labels = c('A','B','C','D'), label_size = 14)
  

#individuals 
dlg_rank_ind <- ggplot(individuals_fos, aes(x=TIMEPOINT, y=DLG, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="previous trial", y="dlg(hipp) fos est. pop/um") +
  theme(legend.position = c(0.005,.98)) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("females",
                                "males"), 
                     values = c("female"="#a6dba0", 
                                "male"="#008837"))
dlg_rank_ind

#two group anova
res.dlg.sex<-aov(DLG ~ TIMEPOINT + RANK, data=individuals_fos)
summary(res.dlg.sex)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 7.230e-06 3.617e-06   0.547  0.586
#RANK         1 4.000e-07 4.040e-07   0.061  0.807
#Residuals   24 1.587e-04 6.614e-06
res.dlg.sex.i <- aov(DLG ~ TIMEPOINT*RANK, data = individuals_fos) 
summary(res.dlg.sex.i)
#                Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT       2 7.230e-06 3.617e-06   0.563  0.577
#RANK            1 4.000e-07 4.040e-07   0.063  0.804
#TIMEPOINT:RANK  2 1.747e-05 8.735e-06   1.360  0.277
#Residuals      22 1.413e-04 6.421e-06


dlv_rank_ind <- ggplot(individuals_fos, aes(x=TIMEPOINT, y=DLV, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="previous trial", y="dlv fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("females",
                                "males"), 
                     values = c("female"="#a6dba0", 
                                "male"="#008837"))
dlv_rank_ind

#two group anova
res.dlv.sex<-aov(DLV ~ TIMEPOINT + RANK, data=individuals_fos)
summary(res.dlv.sex)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 2.922e-06 1.461e-06   1.987  0.159
#RANK         1 2.050e-07 2.052e-07   0.279  0.602
#Residuals   24 1.765e-05 7.353e-07
res.dlv.sex.i <- aov(DLV ~ TIMEPOINT*RANK, data = individuals_fos) 
summary(res.dlv.sex.i)
#                Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT       2 2.922e-06 1.461e-06   1.984  0.161
#RANK            1 2.050e-07 2.052e-07   0.279  0.603
#TIMEPOINT:RANK  2 1.449e-06 7.244e-07   0.984  0.390
#Residuals      22 1.620e-05 7.363e-07


dm1_rank_ind <- ggplot(individuals_fos, aes(x=TIMEPOINT, y=DM1, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="previous trial", y="dm-1 fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("females",
                                "males"), 
                     values = c("female"="#a6dba0", 
                                "male"="#008837"))
dm1_rank_ind

#two group anova
res.dm1.sex<-aov(DM1 ~ TIMEPOINT + RANK, data=individuals_fos)
summary(res.dm1.sex)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 9.106e-05 4.553e-05  17.404 2.13e-05 ***
#RANK         1 2.550e-06 2.550e-06   0.975    0.333    
#Residuals   24 6.278e-05 2.620e-06
res.dm1.sex.i <- aov(DM1 ~ TIMEPOINT*RANK, data = individuals_fos) 
summary(res.dm1.sex.i)
#                Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT       2 9.106e-05 4.553e-05  16.292 4.56e-05 ***
#RANK            1 2.550e-06 2.550e-06   0.913    0.350    
#TIMEPOINT:RANK  2 1.300e-06 6.500e-07   0.233    0.794    
#Residuals      22 6.148e-05 2.790e-06


dm3_rank_ind <- ggplot(individuals_fos, aes(x=TIMEPOINT, y=DM3, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="previous trial", y="dm-3 fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("females",
                                "males"), 
                     values = c("female"="#a6dba0", 
                                "male"="#008837"))
dm3_rank_ind

#two group anova
res.dm3.sex<-aov(DM3 ~ TIMEPOINT + RANK, data=individuals_fos)
summary(res.dm3.sex)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 3.271e-06 1.636e-06   5.172 0.0136 *
#RANK         1 8.100e-07 8.102e-07   2.562 0.1225  
#Residuals   24 7.589e-06 3.162e-07
res.dm3.sex.i <- aov(DM3 ~ TIMEPOINT*RANK, data = individuals_fos) 
summary(res.dm3.sex.i)
#                Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT       2 3.271e-06 1.636e-06   5.447  0.012 *
#RANK            1 8.100e-07 8.102e-07   2.698  0.115  
#TIMEPOINT:RANK  2 9.830e-07 4.914e-07   1.636  0.217  
#Residuals      22 6.606e-06 3.003e-07


vs_rank_ind <- ggplot(individuals_fos, aes(x=TIMEPOINT, y=VS, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="previous trial", y="vs fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("females",
                                "males"), 
                     values = c("female"="#a6dba0", 
                                "male"="#008837"))
vs_rank_ind

#two group anova
res.vs.sex<-aov(VS ~ TIMEPOINT + RANK, data=individuals_fos)
summary(res.vs.sex)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 5.910e-06 2.956e-06   1.022  0.375
#RANK         1 3.900e-07 3.894e-07   0.135  0.717
#Residuals   24 6.941e-05 2.892e-06
res.vs.sex.i <- aov(VS ~ TIMEPOINT*RANK, data = individuals_fos) 
summary(res.vs.sex.i)
#                Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT       2 5.910e-06 2.956e-06   0.973  0.394
#RANK            1 3.900e-07 3.894e-07   0.128  0.724
#TIMEPOINT:RANK  2 2.540e-06 1.272e-06   0.418  0.663
#Residuals      22 6.687e-05 3.040e-06

plot_grid(dlg_rank_ind, dm1_rank_ind, vs_rank_ind, dlv_rank_ind, dm3_rank_ind, labels = c('A','B','C','D','E', label_size = 14))


#### COMPARISON BETWEEN GROUPS AND INDIVIDUALS LEARNING AND FOS ####

#groups dlg
dlg_rank_groups_learn <- ggplot(groups_fos, aes(x=LEARNED, y=DLG, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="learning?", y="dlg fos est. pop/um") +
  theme(legend.position = c(0.005,.98)) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("large males", 
                                "females",
                                "small males"), 
                     values = c("dom"="#8e0152", 
                                "fem"="#de77ae",
                                "sub"="#c51b7d"))
dlg_rank_groups_learn

#two group anova
res.dlg.rank.learn<-aov(DLG ~ LEARNED + RANK, data=groups_fos)
summary(res.dlg.rank.learn)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 0.0004047 2.024e-04   7.512 0.00117 **
#RANK         2 0.0000727 3.635e-05   1.349 0.26675   
#Residuals   64 0.0017242 2.694e-05
res.dlg.rank.learn.i <- aov(DLG ~ LEARNED*RANK, data = groups_fos) 
summary(res.dlg.rank.learn.i)
#                  Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT       2 0.0004047 2.024e-04   7.254 0.00151 **
#RANK            2 0.0000727 3.635e-05   1.303 0.27935   
#TIMEPOINT:RANK  4 0.0000503 1.257e-05   0.451 0.77159   
#Residuals      60 0.0016739 2.790e-05


dlv_rank_groups_learn <- ggplot(groups_fos, aes(x=LEARNED, y=DLV, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="learning?", y="dlv fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("large males", 
                                "females",
                                "small males"), 
                     values = c("dom"="#8e0152", 
                                "fem"="#de77ae",
                                "sub"="#c51b7d"))
dlv_rank_groups_learn

#two group anova
res.dlv.rank.learn<-aov(DLV ~ LEARNED + RANK, data=groups_fos)
summary(res.dlv.rank.learn)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 0.0001229 6.147e-05   5.246 0.00777 **
#RANK         2 0.0000456 2.282e-05   1.948 0.15095   
#Residuals   64 0.0007499 1.172e-05
res.dlv.rank.learn.i <- aov(DLV ~ LEARNED*RANK, data = groups_fos) 
summary(res.dlv.rank.learn.i)
#                  Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT       2 0.0001229 6.147e-05   6.429 0.00295 **
#RANK            2 0.0000456 2.282e-05   2.387 0.10060   
#TIMEPOINT:RANK  4 0.0001762 4.405e-05   4.607 0.00261 **
#Residuals      60 0.0005737 9.560e-06


dm1_rank_groups_learn <- ggplot(groups_fos, aes(x=LEARNED, y=DM1, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="learning?", y="dm-1 fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("large males", 
                                "females",
                                "small males"), 
                     values = c("dom"="#8e0152", 
                                "fem"="#de77ae",
                                "sub"="#c51b7d"))
dm1_rank_groups_learn

#two group anova
res.dm1.rank.learn<-aov(DM1 ~ LEARNED + RANK, data=groups_fos)
summary(res.dm1.rank.learn)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 0.003867 0.0019334  57.370 5.33e-15 ***
#RANK         2 0.000089 0.0000445   1.321    0.274    
#Residuals   64 0.002157 0.0000337
res.dm1.rank.learn.i <- aov(DM1 ~ LEARNED*RANK, data = groups_fos) 
summary(res.dm1.rank.learn.i)
#                  Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT          2 0.003867 0.0019334  56.652 1.51e-14 ***
#RANK            2 0.000089 0.0000445   1.305    0.279    
#TIMEPOINT:RANK  4 0.000109 0.0000273   0.800    0.530    
#Residuals      60 0.002048 0.0000341 

dm3_rank_groups_learn <- ggplot(groups_fos, aes(x=LEARNED, y=DM3, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="learning?", y="dm-3 fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("large males", 
                                "females",
                                "small males"), 
                     values = c("dom"="#8e0152", 
                                "fem"="#de77ae",
                                "sub"="#c51b7d"))
dm3_rank_groups_learn

#two group anova
res.dm3.rank.learn<-aov(DM3 ~ LEARNED + RANK, data=groups_fos)
summary(res.dm3.rank.learn)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 0.0001923 9.616e-05   4.245 0.0186 *
#RANK         2 0.0000212 1.060e-05   0.468 0.6284  
#Residuals   64 0.0014498 2.265e-05
res.dm3.rank.learn.i <- aov(DM3 ~ LEARNED*RANK, data = groups_fos) 
summary(res.dm3.rank.learn.i)
#                  Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT         2 0.0001923 9.616e-05   4.083 0.0218 *
#RANK            2 0.0000212 1.060e-05   0.450 0.6397  
#TIMEPOINT:RANK  4 0.0000367 9.180e-06   0.390 0.8153  
#Residuals      60 0.0014131 2.355e-05


vs_rank_groups_learn <- ggplot(groups_fos, aes(x=LEARNED, y=VS, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="learning?", y="vs fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("large males", 
                                "females",
                                "small males"), 
                     values = c("dom"="#8e0152", 
                                "fem"="#de77ae",
                                "sub"="#c51b7d"))
vs_rank_groups_learn

#two group anova
res.vs.rank.learn<-aov(VS ~ LEARNED + RANK, data=groups_fos)
summary(res.vs.rank.learn)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    2 0.001248 0.0006241   8.204 0.000673 ***
#RANK         2 0.000090 0.0000450   0.592 0.556121    
#Residuals   64 0.004868 0.0000761
res.vs.rank.learn.i <- aov(VS ~ LEARNED*RANK, data = groups_fos) 
summary(res.vs.rank.learn.i)
#                  Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT         2 0.001248 0.0006241   7.959 0.000859 ***
#RANK            2 0.000090 0.0000450   0.575 0.566041    
#TIMEPOINT:RANK  4 0.000164 0.0000410   0.523 0.719275    
#Residuals      60 0.004704 0.0000784

plot_grid(dlg_rank_groups_learn, dm1_rank_groups_learn, vs_rank_groups_learn, dlv_rank_groups_learn, dm3_rank_groups_learn, labels = c('A','B','C','D'), label_size = 14)


#individuals
dlg_rank_ind_resp <- ggplot(individuals_fos, aes(x=LEARNED, y=DLG, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="was learning response met?", y="dlg fos est. pop/um") +
  theme(legend.position = c(0.005,.98)) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("females",
                                "males"), 
                     values = c("female"="#a6dba0", 
                                "male"="#008837"))
dlg_rank_ind_resp

#two group anova
res.dlg.sex.learn<-aov(DLG ~ LEARNED + RANK, data=individuals_fos)
summary(res.dlg.sex.learn)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    1 2.709e-05 2.709e-05   4.865 0.0368 *
#RANK         1 8.000e-08 8.400e-08   0.015 0.9035  
#Residuals   25 1.392e-04 5.568e-06
res.dlg.sex.learn.i <- aov(DLG ~ LEARNED*RANK, data = individuals_fos) 
summary(res.dlg.sex.learn.i)
#             Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT     1 2.709e-05 2.709e-05   4.930 0.0361 *
#RANK          1 8.000e-08 8.400e-08   0.015 0.9029  
#LEARNED:RANK  1 7.320e-06 7.321e-06   1.332 0.2598  
#Residuals    24 1.319e-04 5.495e-06


dlv_rank_ind_resp <- ggplot(individuals_fos, aes(x=LEARNED, y=DLV, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="was learning response met?", y="dlv fos est. pop/um") +
  theme(legend.position = c(0.005,.98)) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("females",
                                "males"), 
                     values = c("female"="#a6dba0", 
                                "male"="#008837"))
dlv_rank_ind_resp

#two group anova
res.dlv.sex.learn<-aov(DLV ~ LEARNED + RANK, data=individuals_fos)
summary(res.dlv.sex.learn)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT    1 0.000e+00 3.000e-10     0.0  0.984
#RANK         1 5.650e-07 5.655e-07     0.7  0.411
#Residuals   25 2.021e-05 8.084e-07
res.dlv.sex.learn.i <- aov(DLV ~ LEARNED*RANK, data = individuals_fos) 
summary(res.dlv.sex.learn.i)
#             Df   Sum Sq  Mean Sq F value   Pr(>F)    
#TIMEPOINT     1 0.000e+00 3.000e-10   0.000  0.983
#RANK          1 5.650e-07 5.655e-07   0.713  0.407
#LEARNED:RANK  1 1.185e-06 1.185e-06   1.495  0.233
#Residuals    24 1.903e-05 7.927e-07


dm1_rank_ind_resp <- ggplot(individuals_fos, aes(x=LEARNED, y=DM1, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="was learning response met?", y="dm-1 fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("females",
                                "males"), 
                     values = c("female"="#a6dba0", 
                                "male"="#008837"))
dm1_rank_ind_resp

#two group anova
res.dm1.sex.learn<-aov(DM1 ~ LEARNED + RANK, data=individuals_fos)
summary(res.dm1.sex.learn)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#LEARNED      1 1.946e-05 1.946e-05   3.555  0.071 .
#RANK         1 1.100e-07 1.110e-07   0.020  0.888  
#Residuals   25 1.368e-04 5.473e-06
res.dm1.sex.learn.i <- aov(DM1 ~ LEARNED*RANK, data = individuals_fos) 
summary(res.dm1.sex.learn.i)
#                Df   Sum Sq  Mean Sq F value   Pr(>F)    
#LEARNED       1 1.946e-05 1.946e-05   3.909 0.0596 .
#RANK          1 1.100e-07 1.110e-07   0.022 0.8824  
#LEARNED:RANK  1 1.736e-05 1.735e-05   3.486 0.0741 .
#Residuals    24 1.195e-04 4.978e-06


dm3_rank_ind_resp <- ggplot(individuals_fos, aes(x=LEARNED, y=DM3, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="was learning response met?", y="dm-3 fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("females",
                                "males"), 
                     values = c("female"="#a6dba0", 
                                "male"="#008837"))
dm3_rank_ind_resp

#two group anova
res.dm3.sex.learn<-aov(DM3 ~ LEARNED + RANK, data=individuals_fos)
summary(res.dm3.sex.learn)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#LEARNED      1 6.540e-07 6.542e-07    1.72 0.2016  
#RANK         1 1.510e-06 1.510e-06    3.97 0.0573 .
#Residuals   25 9.506e-06 3.803e-07
res.dm3.sex.learn.i <- aov(DM3 ~ LEARNED*RANK, data = individuals_fos) 
summary(res.dm3.sex.learn.i)
#                Df   Sum Sq  Mean Sq F value   Pr(>F)    
#LEARNED       1 6.540e-07 6.542e-07   1.661  0.210  
#RANK          1 1.510e-06 1.510e-06   3.833  0.062 .
#LEARNED:RANK  1 5.200e-08 5.240e-08   0.133  0.719  
#Residuals    24 9.454e-06 3.939e-07


vs_rank_ind_resp <- ggplot(individuals_fos, aes(x=LEARNED, y=VS, color=RANK)) +
  geom_boxplot(size=1,outlier.alpha = 0) +
  geom_jitter(position=position_jitterdodge(),alpha=0.6) +
  labs(x="was learning response met?", y="vs fos est. pop/um") +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12)) +
  scale_color_manual(name="", 
                     labels = c("females",
                                "males"), 
                     values = c("female"="#a6dba0", 
                                "male"="#008837"))
vs_rank_ind_resp

#two group anova
res.vs.sex.learn<-aov(VS ~ LEARNED + RANK, data=individuals_fos)
summary(res.vs.sex.learn)  
#            Df  Sum Sq  Mean Sq F value   Pr(>F)    
#LEARNED      1 3.000e-08 3.470e-08   0.012  0.915
#RANK         1 9.900e-07 9.878e-07   0.331  0.570
#Residuals   25 7.469e-05 2.988e-06  
res.vs.sex.learn.i <- aov(VS ~ LEARNED*RANK, data = individuals_fos) 
summary(res.vs.sex.learn.i)
#             Df   Sum Sq  Mean Sq F value   Pr(>F)    
#LEARNED       1 3.000e-08 3.470e-08   0.011  0.917
#RANK          1 9.900e-07 9.878e-07   0.317  0.578
#LEARNED:RANK  1 2.000e-08 1.820e-08   0.006  0.940
#Residuals    24 7.467e-05 3.111e-06

plot_grid(dlg_rank_ind_resp, dm1_rank_ind_resp, vs_rank_ind_resp, dlv_rank_ind_resp, dm3_rank_ind_resp, labels = c('A','B','C','D','E', label_size = 14))


#### AGGRESSION, ACTIVITY, AND FOS ####

# normalize aggression by overall tank activity
mutate(all_social, NORM.DISPLACE = DISPLACE.AVG/LINECROSS.NORM.AVG) -> all_social_2

ggplot(all_social_2, aes(x=NORM.DISPLACE, y=DLG, color=INFORMANT)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black")+
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_color_manual(name="informant status", 
                     labels = c("DOM", 
                                "SUB"), 
                     values = c("DOM"="#7b3294", 
                                "SUB"="#c2a5cf")) 

ggplot(all_social_2, aes(x=NORM.DISPLACE, y=DLV, color=INFORMANT)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black")+
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_color_manual(name="informant status", 
                     labels = c("DOM", 
                                "SUB"), 
                     values = c("DOM"="#7b3294", 
                                "SUB"="#c2a5cf")) 

ggplot(all_social_2, aes(x=NORM.DISPLACE, y=DM1, color=INFORMANT)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black")+
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_color_manual(name="informant status", 
                     labels = c("DOM", 
                                "SUB"), 
                     values = c("DOM"="#7b3294", 
                                "SUB"="#c2a5cf")) +
  labs(title = paste("Adj R2 = ",signif(summary(norm_agg_dm1_fit)$adj.r.squared, 2),
                     "Intercept =",signif(norm_agg_dm1_fit$coef[[1]],2 ),
                     " P =",signif(summary(norm_agg_dm1_fit)$coef[2,4], 2)), size=12)
norm_agg_dm1_fit <- lm(DM1~NORM.DISPLACE, data=all_social_2)
summary(norm_agg_dm1_fit)

ggplot(all_social_2, aes(x=NORM.DISPLACE, y=DM3, color=INFORMANT)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black")+
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_color_manual(name="informant status", 
                     labels = c("DOM", 
                                "SUB"), 
                     values = c("DOM"="#7b3294", 
                                "SUB"="#c2a5cf"))

ggplot(all_social_2, aes(x=NORM.DISPLACE, y=VS, color=INFORMANT)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black")+
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_color_manual(name="informant status", 
                     labels = c("DOM", 
                                "SUB"), 
                     values = c("DOM"="#7b3294", 
                                "SUB"="#c2a5cf")) +
  labs(title = paste("Adj R2 = ",signif(summary(norm_agg_vs_fit)$adj.r.squared, 2),
                     "Intercept =",signif(norm_agg_vs_fit$coef[[1]],2 ),
                     " P =",signif(summary(norm_agg_vs_fit)$coef[2,4], 2)), size=12)

norm_agg_vs_fit <- lm(VS ~ NORM.DISPLACE, data=all_social_2)
summary(norm_agg_vs_fit)


# neural activity correlated to normalized group activity

norm_linecross_avg <- lm(DLG ~ LINECROSS.NORM.AVG, data = all_social_2)
ggplot(all_social_2, aes(x=LINECROSS.NORM.AVG, y=DLG)) +
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


norm_linecross_avg_dlv <- lm(DLV ~ LINECROSS.NORM.AVG, data = all_social_2)
ggplot(all_social_2, aes(x=LINECROSS.NORM.AVG, y=DLV)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(norm_linecross_avg_dlv)$adj.r.squared, 2),
                     "Intercept =",signif(norm_linecross_avg_dlv$coef[[1]],2 ),
                     " P =",signif(summary(norm_linecross_avg_dlv)$coef[2,4], 2)), size=12)


norm_linecross_avg_dm1 <- lm(DM1 ~ LINECROSS.NORM.AVG, data = all_social_2)
ggplot(all_social_2, aes(x=LINECROSS.NORM.AVG, y=DM1)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(norm_linecross_avg_dm1)$adj.r.squared, 2),
                     "Intercept =",signif(norm_linecross_avg_dm1$coef[[1]],2 ),
                     " P =",signif(summary(norm_linecross_avg_dm1)$coef[2,4], 2)), size=12)


norm_linecross_avg_dm3 <- lm(DM3 ~ LINECROSS.NORM.AVG, data = all_social_2)
ggplot(all_social_2, aes(x=LINECROSS.NORM.AVG, y=DM3)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(norm_linecross_avg_dm3)$adj.r.squared, 2),
                     "Intercept =",signif(norm_linecross_avg_dm3$coef[[1]],2 ),
                     " P =",signif(summary(norm_linecross_avg_dm3)$coef[2,4], 2)), size=12)


norm_linecross_avg_vs <- lm(VS ~ LINECROSS.NORM.AVG, data = all_social_2)
ggplot(all_social_2, aes(x=LINECROSS.NORM.AVG, y=VS)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(norm_linecross_avg_vs)$adj.r.squared, 2),
                     "Intercept =",signif(norm_linecross_avg_vs$coef[[1]],2 ),
                     " P =",signif(summary(norm_linecross_avg_vs)$coef[2,4], 2)), size=12)


# neural activity and informant approaches (not normalized)

approaches_dlg <- lm(DLG ~ APPROACHES, data=all_social_2)
ggplot(all_social_2, aes(x=APPROACHES, y=DLG)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(approaches_dlg)$adj.r.squared, 2),
                     "Intercept =",signif(approaches_dlg$coef[[1]],2 ),
                     " P =",signif(summary(approaches_dlg)$coef[2,4], 2)), size=12)

approaches_dlv <- lm(DLV ~ APPROACHES, data=all_social_2)
ggplot(all_social_2, aes(x=APPROACHES, y=DLV)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(approaches_dlv)$adj.r.squared, 2),
                     "Intercept =",signif(approaches_dlv$coef[[1]],2 ),
                     " P =",signif(summary(approaches_dlv)$coef[2,4], 2)), size=12)

approaches_dm1 <- lm(DM1 ~ APPROACHES, data=all_social_2)
ggplot(all_social_2, aes(x=APPROACHES, y=DM1)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(approaches_dm1)$adj.r.squared, 2),
                     "Intercept =",signif(approaches_dm1$coef[[1]],2 ),
                     " P =",signif(summary(approaches_dm1)$coef[2,4], 2)), size=12)

approaches_dm3 <- lm(DM3 ~ APPROACHES, data=all_social_2)
ggplot(all_social_2, aes(x=APPROACHES, y=DM3)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(approaches_dm3)$adj.r.squared, 2),
                     "Intercept =",signif(approaches_dm3$coef[[1]],2 ),
                     " P =",signif(summary(approaches_dm3)$coef[2,4], 2)), size=12)

approaches_vs <- lm(VS ~ APPROACHES, data=all_social_2)
ggplot(all_social_2, aes(x=APPROACHES, y=VS)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(approaches_vs)$adj.r.squared, 2),
                     "Intercept =",signif(approaches_vs$coef[[1]],2 ),
                     " P =",signif(summary(approaches_vs)$coef[2,4], 2)), size=12)


# neural activity and informant approaches (averaged) (not normalized)

approach_avg_dlg <- lm(DLG ~ APPROACH.AVG, data=all_social_2)
ggplot(all_social_2, aes(x=APPROACH.AVG, y=DLG)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(approach_avg_dlg)$adj.r.squared, 2),
                     "Intercept =",signif(approach_avg_dlg$coef[[1]],2 ),
                     " P =",signif(summary(approach_avg_dlg)$coef[2,4], 2)), size=12)

approach_avg_dlv <- lm(DLV ~ APPROACH.AVG, data=all_social_2)
ggplot(all_social_2, aes(x=APPROACH.AVG, y=DLV)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(approach_avg_dlv)$adj.r.squared, 2),
                     "Intercept =",signif(approach_avg_dlv$coef[[1]],2 ),
                     " P =",signif(summary(approach_avg_dlv)$coef[2,4], 2)), size=12)

approach_avg_dm1 <- lm(DM1 ~ APPROACH.AVG, data=all_social_2)
ggplot(all_social_2, aes(x=APPROACH.AVG, y=DM1)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(approach_avg_dm1)$adj.r.squared, 2),
                     "Intercept =",signif(approach_avg_dm1$coef[[1]],2 ),
                     " P =",signif(summary(approach_avg_dm1)$coef[2,4], 2)), size=12)

approach_avg_dm3 <- lm(DM3 ~ APPROACH.AVG, data=all_social_2)
ggplot(all_social_2, aes(x=APPROACH.AVG, y=DM3)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(approach_avg_dm3)$adj.r.squared, 2),
                     "Intercept =",signif(approach_avg_dm3$coef[[1]],2 ),
                     " P =",signif(summary(approach_avg_dm3)$coef[2,4], 2)), size=12)

approach_avg_vs <- lm(VS ~ APPROACH.AVG, data=all_social_2)
ggplot(all_social_2, aes(x=APPROACH.AVG, y=VS)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(approach_avg_vs)$adj.r.squared, 2),
                     "Intercept =",signif(approach_avg_vs$coef[[1]],2 ),
                     " P =",signif(summary(approach_avg_vs)$coef[2,4], 2)), size=12)


# neural activity correlated to informant displacements

displace_dlg <- lm(DLG ~ DISPLACEMENTS, data=all_social_2)
ggplot(all_social_2, aes(x=DISPLACEMENTS, y=DLG)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(displace_dlg)$adj.r.squared, 2),
                     "Intercept =",signif(displace_dlg$coef[[1]],2 ),
                     " P =",signif(summary(displace_dlg)$coef[2,4], 2)), size=12)

displace_dlv <- lm(DLV ~ DISPLACEMENTS, data=all_social_2)
ggplot(all_social_2, aes(x=DISPLACEMENTS, y=DLV)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(displace_dlv)$adj.r.squared, 2),
                     "Intercept =",signif(displace_dlv$coef[[1]],2 ),
                     " P =",signif(summary(displace_dlv)$coef[2,4], 2)), size=12)

displace_dm1 <- lm(DM1 ~ DISPLACEMENTS, data=all_social_2)
ggplot(all_social_2, aes(x=DISPLACEMENTS, y=DM1)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(displace_dm1)$adj.r.squared, 2),
                     "Intercept =",signif(displace_dm1$coef[[1]],2 ),
                     " P =",signif(summary(displace_dm1)$coef[2,4], 2)), size=12)

displace_dm3 <- lm(DM3 ~ DISPLACEMENTS, data=all_social_2)
ggplot(all_social_2, aes(x=DISPLACEMENTS, y=DM3)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(displace_dm3)$adj.r.squared, 2),
                     "Intercept =",signif(displace_dm3$coef[[1]],2 ),
                     " P =",signif(summary(displace_dm3)$coef[2,4], 2)), size=12)

displace_vs <- lm(VS ~ DISPLACEMENTS, data=all_social_2)
ggplot(all_social_2, aes(x=DISPLACEMENTS, y=VS)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(displace_vs)$adj.r.squared, 2),
                     "Intercept =",signif(displace_vs$coef[[1]],2 ),
                     " P =",signif(summary(displace_vs)$coef[2,4], 2)), size=12)


# neural activity correlated to informant average displacements

displace_avg_dlg <- lm(DLG ~ DISPLACE.AVG, data=all_social_2)
ggplot(all_social_2, aes(x=DISPLACE.AVG, y=DLG)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(displace_avg_dlg)$adj.r.squared, 2),
                     "Intercept =",signif(displace_avg_dlg$coef[[1]],2 ),
                     " P =",signif(summary(displace_avg_dlg)$coef[2,4], 2)), size=12)

displace_avg_dlv <- lm(DLV ~ DISPLACE.AVG, data=all_social_2)
ggplot(all_social_2, aes(x=DISPLACE.AVG, y=DLV)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(displace_avg_dlv)$adj.r.squared, 2),
                     "Intercept =",signif(displace_avg_dlv$coef[[1]],2 ),
                     " P =",signif(summary(displace_avg_dlv)$coef[2,4], 2)), size=12)

displace_avg_dm1 <- lm(DM1 ~ DISPLACE.AVG, data=all_social_2)
ggplot(all_social_2, aes(x=DISPLACE.AVG, y=DM1)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(displace_avg_dm1)$adj.r.squared, 2),
                     "Intercept =",signif(displace_avg_dm1$coef[[1]],2 ),
                     " P =",signif(summary(displace_avg_dm1)$coef[2,4], 2)), size=12)

displace_avg_dm3 <- lm(DM3 ~ DISPLACE.AVG, data=all_social_2)
ggplot(all_social_2, aes(x=DISPLACE.AVG, y=DM3)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(displace_avg_dm3)$adj.r.squared, 2),
                     "Intercept =",signif(displace_avg_dm3$coef[[1]],2 ),
                     " P =",signif(summary(displace_avg_dm3)$coef[2,4], 2)), size=12)

displace_avg_vs <- lm(VS ~ DISPLACE.AVG, data=all_social_2)
ggplot(all_social_2, aes(x=DISPLACE.AVG, y=VS)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(displace_avg_vs)$adj.r.squared, 2),
                     "Intercept =",signif(displace_avg_vs$coef[[1]],2 ),
                     " P =",signif(summary(displace_avg_vs)$coef[2,4], 2)), size=12)


# neural activity correlated to informant average displacements

aggeff_trial_dlg <- lm(DLG ~ AGGEFF.TRIAL, data=all_social_2)
ggplot(all_social_2, aes(x=AGGEFF.TRIAL, y=DLG)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(aggeff_trial_dlg)$adj.r.squared, 2),
                     "Intercept =",signif(aggeff_trial_dlg$coef[[1]],2 ),
                     " P =",signif(summary(aggeff_trial_dlg)$coef[2,4], 2)), size=12)

aggeff_trial_dlv <- lm(DLV ~ AGGEFF.TRIAL, data=all_social_2)
ggplot(all_social_2, aes(x=AGGEFF.TRIAL, y=DLV)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(aggeff_trial_dlv)$adj.r.squared, 2),
                     "Intercept =",signif(aggeff_trial_dlv$coef[[1]],2 ),
                     " P =",signif(summary(aggeff_trial_dlv)$coef[2,4], 2)), size=12)

aggeff_trial_dm1 <- lm(DM1 ~ AGGEFF.TRIAL, data=all_social_2)
ggplot(all_social_2, aes(x=AGGEFF.TRIAL, y=DM1)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(aggeff_trial_dm1)$adj.r.squared, 2),
                     "Intercept =",signif(aggeff_trial_dm1$coef[[1]],2 ),
                     " P =",signif(summary(aggeff_trial_dm1)$coef[2,4], 2)), size=12)

aggeff_trial_dm3 <- lm(DM3 ~ AGGEFF.TRIAL, data=all_social_2)
ggplot(all_social_2, aes(x=AGGEFF.TRIAL, y=DM3)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(aggeff_trial_dm3)$adj.r.squared, 2),
                     "Intercept =",signif(aggeff_trial_dm3$coef[[1]],2 ),
                     " P =",signif(summary(aggeff_trial_dm3)$coef[2,4], 2)), size=12)

aggeff_trial_vs <- lm(VS ~ AGGEFF.TRIAL, data=all_social_2)
ggplot(all_social_2, aes(x=AGGEFF.TRIAL, y=VS)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(aggeff_trial_vs)$adj.r.squared, 2),
                     "Intercept =",signif(aggeff_trial_vs$coef[[1]],2 ),
                     " P =",signif(summary(aggeff_trial_vs)$coef[2,4], 2)), size=12)


# neural activity correlated to informant agonistic efficiency

aggeff_dlg <- lm(DLG ~ AGGEFF.CUMUL, data=all_social_2)
ggplot(all_social_2, aes(x=AGGEFF.CUMUL, y=DLG)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(aggeff_dlg)$adj.r.squared, 2),
                     "Intercept =",signif(aggeff_dlg$coef[[1]],2 ),
                     " P =",signif(summary(aggeff_dlg)$coef[2,4], 2)), size=12)

aggeff_dlv <- lm(DLV ~ AGGEFF.CUMUL, data=all_social_2)
ggplot(all_social_2, aes(x=AGGEFF.CUMUL, y=DLV)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(aggeff_dlv)$adj.r.squared, 2),
                     "Intercept =",signif(aggeff_dlv$coef[[1]],2 ),
                     " P =",signif(summary(aggeff_dlv)$coef[2,4], 2)), size=12)

aggeff_dm1 <- lm(DM1 ~ AGGEFF.CUMUL, data=all_social_2)
ggplot(all_social_2, aes(x=AGGEFF.CUMUL, y=DM1)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(aggeff_dm1)$adj.r.squared, 2),
                     "Intercept =",signif(aggeff_dm1$coef[[1]],2 ),
                     " P =",signif(summary(aggeff_dm1)$coef[2,4], 2)), size=12)

aggeff_dm3 <- lm(DM3 ~ AGGEFF.CUMUL, data=all_social_2)
ggplot(all_social_2, aes(x=AGGEFF.CUMUL, y=DM3)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(aggeff_dm3)$adj.r.squared, 2),
                     "Intercept =",signif(aggeff_dm3$coef[[1]],2 ),
                     " P =",signif(summary(aggeff_dm3)$coef[2,4], 2)), size=12)

aggeff_vs <- lm(VS ~ AGGEFF.CUMUL, data=all_social_2)
ggplot(all_social_2, aes(x=AGGEFF.CUMUL, y=VS)) +
  geom_point(size=4, alpha=0.6) +
  stat_smooth(method = "lm", col="black") +
  theme_classic() +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=15),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(title = paste("Adj R2 = ",signif(summary(aggeff_vs)$adj.r.squared, 2),
                     "Intercept =",signif(aggeff_vs$coef[[1]],2 ),
                     " P =",signif(summary(aggeff_vs)$coef[2,4], 2)), size=12)

#### PCA ####

# both learning contexts (social and asocial)

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
pca_pabel_b <- ggplot(rotation_fos) +
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
# skree plot
percent <- 100*fos$sdev^2/sum(fos$sdev^2)
percent
perc_data <- data.frame(percent=percent, PC=1:length(percent))
pca_pabel_c <- ggplot(perc_data, aes(x=PC, y=percent)) + 
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
pca_pabel_a <- ggplot(fos_data, aes(x=PC1, y=PC2, color=TREATMENT, shape=LEARNED)) +
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

#### PCA box plots and statistics ####
pca_pabel_d <- ggplot(fos_data, aes(x=TIMEPOINT, y=PC1, fill=TREATMENT)) + 
  geom_boxplot(outlier.shape = NA,size=1) + 
  geom_point(size=3, alpha=.3, position = position_jitterdodge(dodge.width = .9)) +
  xlab("Trial") + 
  ylab("PC1 (59.6%)") +
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
pca_all_aov <- aov(PC1 ~ TREATMENT * TIMEPOINT, data=fos_data)
summary(pca_all_aov)


# PC1 learners v non learners across all groups
pca_pabel_e <- ggplot(fos_data, aes(x=TIMEPOINT, y=PC1, fill=LEARNED)) + 
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


# data mining
ggplot(fos_data, aes(x=PC1, y=PC2, color=INFORMANT)) +
  xlab("PC1 (59.6%)") +
  ylab("PC2 (22.8%)") +
  geom_point(size=4, alpha=.7) +
  theme(legend.position = c(0.75, 0.12)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour="black", size=10)) +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02)) +
  scale_y_continuous(breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02))



ggplot(fos_data, aes(x=PC1, y=PC2, color=RANK)) +
  xlab("PC1 (59.6%)") +
  ylab("PC2 (22.8%)") +
  geom_point(size=4, alpha=.7) +
  theme(legend.position = c(0.75, 0.15)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour="black", size=10)) +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02)) +
  scale_y_continuous(breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02))

ggplot(fos_data, aes(x=PC1, y=PC2, color=RANK)) +
  xlab("PC1 (59.6%)") +
  ylab("PC2 (22.8%)") +
  geom_point(size=4, alpha=.7) +
  theme(legend.position = c(0.75, 0.15)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour="black", size=10)) +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02)) +
  scale_y_continuous(breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02))

ggplot(fos_data, aes(x=PC1, y=PC2, color=LEARNED)) +
  xlab("PC1 (59.6%)") +
  ylab("PC2 (22.8%)") +
  geom_point(size=4, alpha=.7) +
  theme(legend.position = c(0.75, 0.15)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour="black", size=10)) +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02)) +
  scale_y_continuous(breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02))

ggplot(fos_data, aes(x=PC1, y=PC2, color=TREATMENT)) +
  xlab("PC1 (59.6%)") +
  ylab("PC2 (22.8%)") +
  geom_point(size=4, alpha=.7) +
  scale_colour_manual(values=c("#af8dc3","#7fbf7b"),
                      name="learning context",
                      breaks=c("DEMONSTRATOR","INDIVIDUAL"),
                      labels=c("social","asocial")) +
  theme(legend.position = c(0.75, 0.1)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour="black", size=10)) +
  theme(axis.title.x = element_text(size=17, face="bold"),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=17, face="bold"),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02)) +
  scale_y_continuous(breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02))


#### PCA on just social context subset + rotation matrix ####
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

library(cowplot)  
social_top <- plot_grid(social_a, rotation_social, skree_social, labels = c('A', 'B','C'), label_size = 12, nrow=1)
social_mid <- plot_grid(social_d, social_e, social_f, labels = c('D', 'E','F'), label_size = 12, nrow=1)
social_bottom <- plot_grid(social_g, social_h, social_i, labels = c('G', 'H','I'), label_size = 12, nrow=1)



#### PCA just on the asocial context ####
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

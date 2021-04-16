### generalized linear models for social learning neural activity manuscript ###
## goal: modeling Fos activity across five brain regions by conditions ##


#### analysis code ####
counts<-read.csv("fos_data_final.csv", header = TRUE)
colnames(counts)

counts$TIMEPOINT<-as.factor(counts$TIMEPOINT)

library(dplyr)

#### subsetting data into social and non-social conditions ####

counts %>%
  filter(TREATMENT == "DEMONSTRATOR") -> social_condition

counts %>%
  filter(TREATMENT == "INDIVIDUAL") -> asocial_condition


#### GLM: overall model (includes 5 factors) ####
# factors to keep: GROUP, TREATMENT, TRIAL, SEX, LEARNED
# check GROUP as a random effect
# compare AIC values between models

library(glmmTMB)
library(gvlma)
library(MuMIn)

#### overall models (models are per brain region, 4 predictors, 1 RE) ####

#dlg (best model is no_RE model 2)
all_dlg_re_1 <- glmmTMB(DLG ~ SEX+TREATMENT+TIMEPOINT+LEARNED+(1|GROUP), data = counts, family = beta_family())
all_dlg_no_re_1 <- glmmTMB(DLG ~ SEX+TREATMENT+TIMEPOINT+LEARNED, data = counts, family = beta_family())
summary(all_dlg_re_1)
summary(all_dlg_no_re_1) # AIC score diff = 2.5, model with no_re has lowest value of -791.8
#drop RE and compare with a model that includes interaction
all_dlg_no_re_int <- glmmTMB(DLG ~ TREATMENT*TIMEPOINT+SEX+LEARNED, data = counts, family = beta_family())
summary(all_dlg_no_re_int) #AIC = -787.9
#remove sex and trial from no_re
all_dlg_no_re_2 <- glmmTMB(DLG ~ TREATMENT+LEARNED, data=counts, family = beta_family())
summary(all_dlg_no_re_2) #AIC = -795.2


#dlv (best model is RE model 2 but delta AIC is less than 2)
hist(counts$DLV)
all_dlv_re_1 <- glmmTMB(DLV ~ SEX+TREATMENT+TIMEPOINT+LEARNED+(1|GROUP), data = counts, family = beta_family())
all_dlv_no_re_1 <- glmmTMB(DLV ~ SEX+TREATMENT+TIMEPOINT+LEARNED, data = counts, family = beta_family())
summary(all_dlv_re_1)  # AIC= -855.8, RE:GROUP variance = 0.01, st.dev = 0.1126
summary(all_dlv_no_re_1) # AIC= -854.5
# compare to interaction model
all_dlv_re_int <- glmmTMB(DLV ~ TREATMENT*TIMEPOINT+SEX+LEARNED+(1|GROUP), data = counts, family = beta_family())
summary(all_dlv_re_int) #AIC= -851.9, keep model 1
#remove sex and learning
all_dlv_re_2 <- glmmTMB(DLV ~ TREATMENT+TIMEPOINT+(1|GROUP), data = counts, family = beta_family())
summary(all_dlv_re_2) # AIC= -857.6


#dm-1 
hist(counts$DM1) #neg binomial distribution??
all_dm1_re_1 <- glmmTMB(DM1 ~ SEX+TREATMENT+TIMEPOINT+LEARNED+(1|GROUP), data = counts, family = beta_family())
all_dm1_no_re_1 <- glmmTMB(DM1 ~ SEX+TREATMENT+TIMEPOINT+LEARNED, data = counts, family = beta_family())
summary(all_dm1_re_1) # AIC = -764.4, keep model 1
summary(all_dm1_no_re_1) # AIC = -762.7
# compare to interaction model
all_dm1_re_int <- glmmTMB(DM1 ~ TREATMENT*TIMEPOINT+SEX+LEARNED, data=counts, family=beta_family())
summary(all_dm1_re_int) # keep model 1
# remove sex
all_dm_re_2 <- glmmTMB(DM1 ~ TREATMENT+TIMEPOINT+LEARNED+(1|GROUP), data = counts, family=beta_family())
summary(all_dm_re_2) # AIC = -764.8


#dm-3
hist(counts$DM3)
all_dm3_re_1 <- glmmTMB(DM3 ~ SEX+TREATMENT+TIMEPOINT+LEARNED+(1|GROUP), data = counts, family = beta_family())
all_dm3_no_re_1 <- glmmTMB(DM3 ~ SEX+TREATMENT+TIMEPOINT+LEARNED, data = counts, family = beta_family())
summary(all_dm3_re_1) # AIC = -829.8
summary(all_dm3_no_re_1) # AIC = -828.3
# remove sex and learning
all_dm3_re_2 <- glmmTMB(DM3 ~ TREATMENT+TIMEPOINT+(1|GROUP), data = counts, family = beta_family())
summary(all_dm3_re_2) # AIC = - 832.5


#vs
hist(counts$VS)
all_vs_re_1 <- glmmTMB(VS ~ SEX+TREATMENT+TIMEPOINT+LEARNED+(1|GROUP), data = counts, family = beta_family())
all_vs_no_re_1 <- glmmTMB(VS ~ SEX+TREATMENT+TIMEPOINT+LEARNED, data = counts, family = beta_family())
summary(all_vs_re_1) # AIC = -719.8, keep model 1
summary(all_vs_no_re_1) # AIC = -697.1
# remove timepoint and learning
all_vs_re_2 <- glmmTMB(VS ~ SEX+TREATMENT+(1|GROUP), data = counts, family = beta_family())
summary(all_vs_re_2) # AIC = -719.4



#### social condition models (models are per brain region, 5 predictors, 1 RE) ####

colnames(social_condition)

#dlg 
hist(social_condition$DLG)
social_dlg_re_1 <- glmmTMB(DLG ~ RANK+INFORMANT+TIMEPOINT+LEARNED+(1|SEX), data=social_condition, family = gaussian())
summary(social_dlg_re_1) # RE sex: low variance
social_dlg_no_re_1 <- glmmTMB(DLG ~ TIMEPOINT+RANK+INFORMANT+LEARNED, data = social_condition, family = gaussian())
summary(social_dlg_no_re_1) #AIC = -528.4
#remove TIMEPOINT and INFORMANT
social_dlg_no_re_2 <- glmmTMB(DLG ~ RANK+LEARNED, data = social_condition, family = gaussian())
summary(social_dlg_no_re_2)


# dlv
hist(social_condition$DLV)
social_dlv_re_1 <- glmmTMB(DLV ~ SEX+RANK+INFORMANT+TIMEPOINT+LEARNED+(1|GROUP), data = social_condition, family = beta_family()) # non-positive definite hessian matrix error
#remove SEX
social_dlv_re_2 <- glmmTMB(DLV ~ RANK+INFORMANT+TIMEPOINT+LEARNED+(1|GROUP), data=social_condition, family = beta_family())
summary(social_dlv_re_2) #AIC = -558.9
#remove random effect
social_dlv_no_re <- glmmTMB(DLV ~ RANK+INFORMANT+TIMEPOINT+LEARNED, data=social_condition, family = beta_family())
summary(social_dlv_no_re) #AIC = -560
#remove INFORMANT
social_dlv_no_re_2 <- glmmTMB(DLV ~ TIMEPOINT+LEARNED, data=social_condition, family = beta_family())
summary(social_dlv_no_re_2) #AIC = -562.5


# dm-1
hist(social_condition$DM1)
social_dm1_re_1 <- glmmTMB(DM1 ~ RANK+INFORMANT+TIMEPOINT+LEARNED+(1|GROUP), data = social_condition, family = beta_family())
social_dm_1_no_re_1 <- glmmTMB(DM1 ~ RANK+INFORMANT+TIMEPOINT+LEARNED, data = social_condition, family = beta_family())
summary(social_dm1_re_1) #AIC = -499.4, use this model
summary(social_dm_1_no_re_1) #AIC = -500
#remove RANK, INFORMANT, and LEARNING
social_dm1_re_2 <- glmmTMB(DM1 ~ TIMEPOINT+(1|GROUP), data = social_condition, family = beta_family())
summary(social_dm1_re_2) #AIC = -504.5, best model


# dm-3
hist(social_condition$DM3)
social_dm3_re_1 <- glmmTMB(DM3 ~ RANK+INFORMANT+TIMEPOINT+LEARNED+(1|GROUP), data = social_condition, family = beta_family())
social_dm3_no_re_1 <- glmmTMB(DM3 ~ RANK+INFORMANT+TIMEPOINT+LEARNED, data = social_condition, family = beta_family())
summary(social_dm3_re_1) #AIC = -538.8, keep RE GROUP
summary(social_dm3_no_re_1) #AIC = -539.9
#remove all but timepoint
social_dm3_re_2 <- glmmTMB(DM3 ~ TIMEPOINT+(1|GROUP), data = social_condition, family = beta_family())
summary(social_dm3_re_2) #AIC=-543.7


# vs
hist(social_condition$VS)
social_vs_re_1 <- glmmTMB(VS ~ RANK+INFORMANT+TIMEPOINT+LEARNED+(1|GROUP), data=social_condition, family = beta_family())
social_vs_no_re_1 <- glmmTMB(VS ~ RANK+INFORMANT+TIMEPOINT+LEARNED, data=social_condition, family = beta_family())
summary(social_vs_re_1) #AIC = -465.7, keep RE|GROUP
summary(social_vs_no_re_1) #AIC = -452.9
#remove INFORMANT and LEARNING
social_vs_re_2 <- glmmTMB(VS ~ RANK+TIMEPOINT+(1|GROUP), data=social_condition, family=beta_family())
summary(social_vs_re_2) #AIC = -468.3


#### asocial condition models (models are per brain region, 3 predictors, no RE) ####

colnames(asocial_condition)

# dlg
asocial_model_dlg <- glmmTMB(DLG ~ SEX+TIMEPOINT+LEARNED, data=asocial_condition, family=beta_family())
summary(asocial_model_dlg) #-254.1
#remove SEX and TIMEPOINT
asocial_dlg_2 <- glmmTMB(DLG ~ LEARNED, data = asocial_condition, family = beta_family())
summary(asocial_dlg_2) #AIC = -259.1


# dlv
asocial_model_dlv <- glmmTMB(DLV ~ SEX+TIMEPOINT+LEARNED, data=asocial_condition, family=beta_family())
summary(asocial_model_dlv) # AIC = -312.5
# remove SEX and LEARNING
asocial_dlv_2 <- glmmTMB(DLV ~ TIMEPOINT, data = asocial_condition, family=beta_family())
summary(asocial_dlv_2) # AIC = -314.5


# dm-1
asocial_model_dm1 <- glmmTMB(DM1 ~ SEX+TIMEPOINT+LEARNED, data=asocial_condition, family=beta_family())
summary(asocial_model_dm1) # AIC = -276.6
#remove SEX and LEARNING
asocial_model_dm1_2 <- glmmTMB(DM1 ~ TIMEPOINT, data = asocial_condition, family = beta_family())
summary(asocial_model_dm1_2) # AIC = -278.2


# dm-3
asocial_model_dm3 <- glmmTMB(DM3 ~ SEX+TIMEPOINT+LEARNED, data=asocial_condition, family=beta_family())
summary(asocial_model_dm3) # AIC = -332.8
#remove LEARNING
asocial_model_dm3_2 <- glmmTMB(DM3 ~ SEX+TIMEPOINT, data=asocial_condition, family = beta_family())
summary(asocial_model_dm3_2) # AIC = -334.8


# vs
asocial_model_vs <- glmmTMB(VS ~ SEX+TIMEPOINT+LEARNED, data=asocial_condition, family=beta_family())
summary(asocial_model_vs) # AIC = -269.8
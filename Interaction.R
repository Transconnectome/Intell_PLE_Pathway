rm(list=ls())
data<-read.csv("ABCD_urbanicity_synthetic.csv", header=TRUE)

urb_list<-c("nihtbx_fluidcomp_uncorrected_0y", "nihtbx_cryst_uncorrected_0y", 
           "nihtbx_totalcomp_uncorrected_0y", "totalscore_pps_0y", "distress_score_pps_0y", 
           "pps_parent_0y", "totalscore_pps_1y", "distress_score_pps_1y", "pps_parent_1y", 
           "totalscore_pps_2y", "distress_score_pps_2y", "pps_parent_2y", "rh_adi_perc1_0y", 
           "good_parent_0y", "good_school_0y", "rh_years1_0y", "poverty1_below125_0y", "age_0y", 
           "bmi_0y", "high_educ_0y", "income_0y", "history_ratio_0y", "family_adversity_0y", 
           "sex_0y", "married_0y", "abcd_site_0y", "race_g",
           "cpeur2", "eaeur1", "scz_eurauto", "scz_easauto", "scz_metaauto")

urb_data<-data[urb_list]
data<-na.omit(urb_data)


cov_cat<-c("sex_0y", "married_0y", "race_ethnicity_0y", "abcd_site_0y", "family_id_0y", "race_g")
data[cov_cat]<-lapply(data[cov_cat], factor)

main<-c('rh_adi_perc1_0y', 'rh_years1_0y', 'poverty1_below125_0y', 'good_parent_0y', 'good_school_0y', 
        'high_educ_0y', 'income_0y', 'family_adversity_0y')

cov_list <- c('age_0y', 'sex_0y', 'married_0y', 'race_g', 'bmi_0y', 'history_ratio_0y')

MAIN<-paste(main, collapse = " + ")
COVARS <- paste(cov_list, collapse=" + ")

library(lmerTest)


######################## Multiethnic Cohort: Single Model #######################################
res_lmm<-matrix(, nrow=10)

main<-c('rh_adi_perc1_0y', 'rh_years1_0y', 'poverty1_below125_0y', 'good_parent_0y', 'good_school_0y', 
        'high_educ_0y', 'income_0y', 'family_adversity_0y')

outcomes<-c("nihtbx_fluidcomp_uncorrected_0y", "nihtbx_cryst_uncorrected_0y",
            "nihtbx_totalcomp_uncorrected_0y", "totalscore_pps_0y", "distress_score_pps_0y",
            "pps_parent_0y", "totalscore_pps_1y", "distress_score_pps_1y", "pps_parent_1y",
            "totalscore_pps_2y", "distress_score_pps_2y", "pps_parent_2y")

for (y in outcomes) {
  formula <- formula(paste0(y, '~', COVARS, ' + cpeur2*good_parent_0y+', MAIN, '+ (1|abcd_site_0y)'))
  res_cp<-lmer(formula, data = data)
  
  beta<-as.matrix(round(summary(res_cp)$coefficients[15:24,1],4))
  se<-as.matrix(round(summary(res_cp)$coefficients[15:24,2],4))
  p<-as.matrix(round(summary(res_cp)$coefficients[15:24,5],4))
  
  ci_cp<-confint(res_cp, level = 0.95, method = "boot", nsim = 5000, boot.type ="basic")
  lowci<- round(ci_cp[17:26,1],4)
  highci<-round(ci_cp[17:26,2],4)
  
  # beta_lmm<-rbind(beta_lmm, beta)
  # se_lmm<-rbind(se_lmm, se)
  # lowci_lmm<-rbind(lowci_lmm, lowci)
  # highci_lmm<-rbind(highci_lmm, highci)
  # p_lmm<-rbind(p_lmm, p)
  
  p_fdr<-round((p.adjust(as.matrix(p), method = 'fdr')),4)
  
  res<-cbind(beta, se, lowci, highci, p, p_fdr)
  colnames(res)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR")
  rownames(res)<-c("CP GPS", "ADI", "Years", "Poverty", "Good Parenting", "Good Schooling", 
                   "Parental Education", "Family Income", "Financial Adversity",
                   "CP GPS x Good Parenting")
  res_lmm<-cbind(res_lmm, res)
}

print(res_lmm)



res_lmm2<-matrix(, nrow=10)

for (y in outcomes) {
  formula <- formula(paste0(y, '~', COVARS, ' + eaeur1*good_parent_0y+', MAIN, '+ (1|abcd_site_0y)'))
  res_ea<-lmer(formula, data = data)
  
  beta<-as.matrix(round(summary(res_ea)$coefficients[15:24,1],4))
  se<-as.matrix(round(summary(res_ea)$coefficients[15:24,2],4))
  p<-as.matrix(round(summary(res_ea)$coefficients[15:24,5],4))
  
  ci_ea<-confint(res_ea, level = 0.95, method = "boot", nsim = 5000, boot.type ="basic")
  lowci<- round(ci_ea[17:26,1],4)
  highci<-round(ci_ea[17:26,2],4)
  
  p_fdr<-round((p.adjust(as.matrix(p), method = 'fdr')),4)
  
  res<-cbind(beta, se, lowci, highci, p, p_fdr)
  colnames(res)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR")
  rownames(res)<-c("EA GPS", "ADI", "Years", "Poverty", "Good Parenting", "Good Schooling", 
                   "Parental Education", "Family Income", "Financial Adversity",
                   "EA GPS x Good Parenting")
  res_lmm2<-cbind(res_lmm2, res)
}

print(res_lmm2)


res_lmm3<-matrix(, nrow=10)

for (y in outcomes) {
  formula <- formula(paste0(y, '~', COVARS, ' + cpeur2*good_school_0y+', MAIN, '+ (1|abcd_site_0y)'))
  res_cp<-lmer(formula, data = data)
  
  beta<-as.matrix(round(summary(res_cp)$coefficients[15:24,1],4))
  se<-as.matrix(round(summary(res_cp)$coefficients[15:24,2],4))
  p<-as.matrix(round(summary(res_cp)$coefficients[15:24,5],4))
  
  ci_cp<-confint(res_cp, level = 0.95, method = "boot", nsim = 5000, boot.type ="basic")
  lowci<- round(ci_cp[17:26,1],4)
  highci<-round(ci_cp[17:26,2],4)
  
  # beta_lmm<-rbind(beta_lmm, beta)
  # se_lmm<-rbind(se_lmm, se)
  # lowci_lmm<-rbind(lowci_lmm, lowci)
  # highci_lmm<-rbind(highci_lmm, highci)
  # p_lmm<-rbind(p_lmm, p)
  
  p_fdr<-round((p.adjust(as.matrix(p), method = 'fdr')),4)
  
  res<-cbind(beta, se, lowci, highci, p, p_fdr)
  colnames(res)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR")
  rownames(res)<-c("CP GPS", "ADI", "Years", "Poverty", "Good Parenting", "Good Schooling", 
                   "Parental Education", "Family Income", "Financial Adversity",
                   "CP GPS x Good Schooling")
  res_lmm3<-cbind(res_lmm3, res)
}

print(res_lmm3)



res_lmm4<-matrix(, nrow=10)

for (y in outcomes) {
  formula <- formula(paste0(y, '~', COVARS, ' + eaeur1*good_school_0y+', MAIN, '+ (1|abcd_site_0y)'))
  res_ea<-lmer(formula, data = data)
  
  beta<-as.matrix(round(summary(res_ea)$coefficients[15:24,1],4))
  se<-as.matrix(round(summary(res_ea)$coefficients[15:24,2],4))
  p<-as.matrix(round(summary(res_ea)$coefficients[15:24,5],4))
  
  ci_ea<-confint(res_ea, level = 0.95, method = "boot", nsim = 5000, boot.type ="basic")
  lowci<- round(ci_ea[17:26,1],4)
  highci<-round(ci_ea[17:26,2],4)
  
  p_fdr<-round((p.adjust(as.matrix(p), method = 'fdr')),4)
  
  res<-cbind(beta, se, lowci, highci, p, p_fdr)
  colnames(res)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR")
  rownames(res)<-c("EA GPS", "ADI", "Years", "Poverty", "Good Parenting", "Good Schooling", 
                   "Parental Education", "Family Income", "Financial Adversity",
                   "EA GPS x Good Schooling")
  res_lmm4<-cbind(res_lmm4, res)
}

print(res_lmm4)



######################## Multiethnic Cohort #######################################
res_lmm3<-matrix(, nrow=17)

outcomes<-c("nihtbx_fluidcomp_uncorrected_0y", "nihtbx_cryst_uncorrected_0y",
            "nihtbx_totalcomp_uncorrected_0y", "totalscore_pps_0y", "distress_score_pps_0y",
            "pps_parent_0y", "totalscore_pps_1y", "distress_score_pps_1y", "pps_parent_1y",
            "totalscore_pps_2y", "distress_score_pps_2y", "pps_parent_2y")

for (y in outcomes) {
  formula <- formula(paste0(y, '~', COVARS, ' + cpeur2*(', MAIN, ')', '+ (1|abcd_site_0y)'))
  res_cp<-lmer(formula, data = data)
  
  beta<-as.matrix(round(summary(res_cp)$coefficients[15:31,1],4))
  se<-as.matrix(round(summary(res_cp)$coefficients[15:31,2],4))
  p<-as.matrix(round(summary(res_cp)$coefficients[15:31,5],4))
  
  ci_cp<-confint(res_cp, level = 0.95, method = "boot", nsim = 5000, boot.type ="basic")
  lowci<- round(ci_cp[17:33,1],4)
  highci<-round(ci_cp[17:33,2],4)
  
  # beta_lmm<-rbind(beta_lmm, beta)
  # se_lmm<-rbind(se_lmm, se)
  # lowci_lmm<-rbind(lowci_lmm, lowci)
  # highci_lmm<-rbind(highci_lmm, highci)
  # p_lmm<-rbind(p_lmm, p)
  
  p_fdr<-round((p.adjust(as.matrix(p), method = 'fdr')),4)
  
  res<-cbind(beta, se, lowci, highci, p, p_fdr)
  colnames(res)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR")
  rownames(res)<-c("CP GPS", "ADI", "Years", "Poverty", "Good Parenting", "Good Schooling", 
                   "Parental Education", "Family Income", "Financial Adversity",
                   "CP GPS x ADI", "CP GPS x Years", "CP GPS x Poverty", 
                   "CP GPS x Good Parenting", "CP GPS x Good Schooling",
                   "CP GPS x Parental Education", "CP GPS x Family Income", "CP GPS x Financial Adversity")
  res_lmm3<-cbind(res_lmm3, res)
}

print(res_lmm3)



res_lmm4<-matrix(, nrow=17)

for (y in outcomes) {
  formula <- formula(paste0(y, '~', COVARS, ' + eaeur1*(', MAIN, ')', '+ (1|abcd_site_0y)'))
  res_ea<-lmer(formula, data = data)
  
  beta<-as.matrix(round(summary(res_ea)$coefficients[15:31,1],4))
  se<-as.matrix(round(summary(res_ea)$coefficients[15:31,2],4))
  p<-as.matrix(round(summary(res_ea)$coefficients[15:31,5],4))
  
  ci_ea<-confint(res_ea, level = 0.95, method = "boot", nsim = 5000, boot.type ="basic")
  lowci<- round(ci_ea[17:33,1],4)
  highci<-round(ci_ea[17:33,2],4)
  
  p_fdr<-round((p.adjust(as.matrix(p), method = 'fdr')),4)
  
  res<-cbind(beta, se, lowci, highci, p, p_fdr)
  colnames(res)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR")
  rownames(res)<-c("EA GPS", "ADI", "Years", "Poverty", "Good Parenting", "Good Schooling", 
                   "Parental Education", "Family Income", "Financial Adversity",
                   "EA GPS x ADI", "EA GPS x Years", "EA GPS x Poverty", 
                   "EA GPS x Good Parenting", "EA GPS x Good Schooling",
                   "EA GPS x Parental Education", "EA GPS x Family Income", "EA GPS x Financial Adversity")
  res_lmm4<-cbind(res_lmm4, res)
}

print(res_lmm4)




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
euro_data<-subset(urb_data, race_g==4)
euro_data<-na.omit(euro_data)

cov_cat<-c("sex_0y", "married_0y", "abcd_site_0y", "race_g")
data[cov_cat]<-lapply(data[cov_cat], factor)

main<-c('rh_adi_perc1_0y', 'rh_years1_0y', 'poverty1_below125_0y', 'good_parent_0y', 'good_school_0y')
cov_list_euro<- c('high_educ_0y', 'income_0y', 'family_adversity_0y', 'age_0y', 'sex_0y', 'married_0y', 'bmi_0y', 'history_ratio_0y')

MAIN<-paste(main, collapse = " + ")
COVARS_euro <- paste(cov_list_euro, collapse=" + ")


library(lmerTest)
library(car)



######################## European Cohort #######################################
# beta_lmm<-data.frame()
# se_lmm<-data.frame()
# p_lmm<-data.frame()
# lowci_lmm<-data.frame()
# highci_lmm<-data.frame()
res_lmm<-matrix(, nrow=9)


outcomes<-c("nihtbx_fluidcomp_uncorrected_0y", "nihtbx_cryst_uncorrected_0y",
            "nihtbx_totalcomp_uncorrected_0y", "totalscore_pps_0y", "distress_score_pps_0y",
            "pps_parent_0y", "totalscore_pps_1y", "distress_score_pps_1y", "pps_parent_1y",
            "totalscore_pps_2y", "distress_score_pps_2y", "pps_parent_2y")

for (y in outcomes) {
  formula <- formula(paste0(y, '~cpeur2 +', MAIN, '+', COVARS_euro, '+ (1|abcd_site_0y)'))
  res_cp<-lmer(formula, data = euro_data)
  
  beta<-as.matrix(round(summary(res_cp)$coefficients[2:10,1],4))
  se<-as.matrix(round(summary(res_cp)$coefficients[2:10,2],4))
  p<-as.matrix(round(summary(res_cp)$coefficients[2:10,5],4))
  
  ci_cp<-confint(res_cp, level = 0.95, method = "boot", nsim = 5000, boot.type ="basic")
  lowci<- round(ci_cp[4:12,1],4)
  highci<-round(ci_cp[4:12,2],4)
  
  # beta_lmm<-rbind(beta_lmm, beta)
  # se_lmm<-rbind(se_lmm, se)
  # lowci_lmm<-rbind(lowci_lmm, lowci)
  # highci_lmm<-rbind(highci_lmm, highci)
  # p_lmm<-rbind(p_lmm, p)
  
  p_fdr<-round((p.adjust(as.matrix(p), method = 'fdr')),4)
  
  res<-cbind(beta, se, lowci, highci, p, p_fdr)
  colnames(res)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR")
  rownames(res)<-c("CP GPS", "ADI", "Years", "Poverty", "Good Parenting", "Good Schooling", 
                   "Parental Education", "Family Income", "Financial Adversity")
  res_lmm<-cbind(res_lmm, res)
}

print(res_lmm)



res_lmm2<-matrix(, nrow=9)

for (y in outcomes) {
  formula <- formula(paste0(y, '~eaeur1 +', MAIN, '+', COVARS_euro, '+ (1|abcd_site_0y)'))
  res_ea<-lmer(formula, data = euro_data)
  
  beta<-as.matrix(round(summary(res_ea)$coefficients[2:10,1],4))
  se<-as.matrix(round(summary(res_ea)$coefficients[2:10,2],4))
  p<-as.matrix(round(summary(res_ea)$coefficients[2:10,5],4))
  
  ci_ea<-confint(res_ea, level = 0.95, method = "boot", nsim = 5000, boot.type ="basic")
  lowci<- round(ci_ea[4:12,1],4)
  highci<-round(ci_ea[4:12,2],4)
  
  p_fdr<-round((p.adjust(as.matrix(p), method = 'fdr')),4)
  
  res<-cbind(beta, se, lowci, highci, p, p_fdr)
  colnames(res)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR")
  rownames(res)<-c("EA GPS", "ADI", "Years", "Poverty", "Good Parenting", "Good Schooling", 
                   "Parental Education", "Family Income", "Financial Adversity")
  res_lmm2<-cbind(res_lmm2, res)
}

print(res_lmm2)



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


cov_cat<-c("sex_0y", "married_0y", "abcd_site_0y", "race_g")
data[cov_cat]<-lapply(data[cov_cat], factor)

main<-c('rh_adi_perc1_0y', 'rh_years1_0y', 'poverty1_below125_0y', 'good_parent_0y', 'good_school_0y')

cov_list <- c('high_educ_0y', 'income_0y', 'family_adversity_0y', 'age_0y', 'sex_0y', 'married_0y', 'race_g', 'bmi_0y', 'history_ratio_0y')
cov_list_euro<- c('high_educ_0y', 'income_0y', 'family_adversity_0y', 'age_0y', 'sex_0y', 'married_0y', 'bmi_0y', 'history_ratio_0y')

MAIN<-paste(main, collapse = " + ")
COVARS <- paste(cov_list, collapse=" + ")
COVARS_euro <- paste(cov_list_euro, collapse=" + ")


library(lmerTest)


###################################Miao et al (2022)##################################
source('Miao/routineFun.R')

# set RNG 
RNGkind("L'Ecuyer-CMRG")
set.seed(1)
ncpus <- 20

nfact <- 2
# nfact <- 2
# nfact <- 3
# 17 treatments, 5 SNP as IVs

library(fastDummies)
library(parallel)
library(boot)
library(MASS)


########Multi-Ethnic################

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g'))

outcomes<-c("nihtbx_fluidcomp_uncorrected_0y", "nihtbx_cryst_uncorrected_0y",
            "nihtbx_totalcomp_uncorrected_0y", "totalscore_pps_0y", "distress_score_pps_0y",
            "pps_parent_0y", "totalscore_pps_1y", "distress_score_pps_1y", "pps_parent_1y",
            "totalscore_pps_2y", "distress_score_pps_2y", "pps_parent_2y")

x.name<-c('cpeur2', 'rh_adi_perc1_0y', 'rh_years1_0y', 'poverty1_below125_0y', 'good_parent_0y', 'good_school_0y', 
          'high_educ_0y', 'income_0y', 'family_adversity_0y', 'age_0y', 'bmi_0y', 'history_ratio_0y', 
          'sex_0y_F', 'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6', 
          'race_g_0', 'race_g_1', 'race_g_2', 'race_g_3')

#y.name <- 'totalscore_pps_0y' 

res_miao<-matrix(, nrow=9)

for (y.name in outcomes) {
  # null treatments estimation
  null.esti <- esti.null(y=y.name, x=x.name, dta=data, nfact=nfact, subset=NULL)
  null.bt.esti <- bt.ci(data=data, statistic=esti.null,
                        R=5000, stype='i',
                        y=y.name, x=x.name, v=NULL, nfact=nfact,
                        parallel = 'multicore',
                        ncpus = ncpus)
  
  null.ci.95 <- apply(null.bt.esti, 2, quantile,
                      probs=c((1-0.95)/2, (1+0.95)/2))
  # null.ci.90 <- apply(null.bt.esti, 2, quantile,
  #                     probs=c((1-0.90)/2, (1+0.90)/2))
  null.signif.95 <- (0 < null.ci.95[1,]) | (0 > null.ci.95[2,])
  # null.signif.90 <- (0 < null.ci.90[1,]) | (0 > null.ci.90[2,])
  # null.signif <- ifelse(null.signif.90, '*', 0)
  # null.signif[null.signif.95] <- '**'
  ci<-data.frame(t(null.ci.95))
  colnames(ci)<-c("low", "high")
  se<-((ci$high-ci$low)/(2*qnorm(0.975)))
  tstat<-null.esti/se
  pval<-2*(1-pnorm(abs(tstat)))
  p_fdr<-p.adjust(as.matrix(pval[1:9]), method = "fdr")
  
  null.result.all <- data.frame(null.esti[1:9], ci[1:9, ], pval[1:9], p_fdr)
  null.result.all[,1:5] <- round(null.result.all[,1:5], digits=4)
  colnames(null.result.all) <- c("esti", "Low 95%CI", "High 95%CI", "P-val", "P-FDR")
  
  rownames(null.result.all) <- c("CP GPS", "ADI", "Years", "Poverty", "Good Parenting", "Good Schooling", 
                                 "Parental Education", "Family Income", "Financial Adversity")
  
  res_miao<-cbind(res_miao, null.result.all)
  
}


print(res_miao)



x.name<-c('eaeur1', 'rh_adi_perc1_0y', 'rh_years1_0y', 'poverty1_below125_0y', 'good_parent_0y', 'good_school_0y', 
          'high_educ_0y', 'income_0y', 'family_adversity_0y', 'age_0y', 'bmi_0y', 'history_ratio_0y', 
          'sex_0y_F', 'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6', 
          'race_g_0', 'race_g_1', 'race_g_2', 'race_g_3')

res_miao2<-matrix(, nrow=9)

for (y.name in outcomes) {
  # null treatments estimation
  null.esti <- esti.null(y=y.name, x=x.name, dta=data, nfact=nfact, subset=NULL)
  null.bt.esti <- bt.ci(data=data, statistic=esti.null,
                        R=5000, stype='i',
                        y=y.name, x=x.name, v=NULL, nfact=nfact,
                        parallel = 'multicore',
                        ncpus = ncpus)
  
  null.ci.95 <- apply(null.bt.esti, 2, quantile,
                      probs=c((1-0.95)/2, (1+0.95)/2))
  # null.ci.90 <- apply(null.bt.esti, 2, quantile,
  #                     probs=c((1-0.90)/2, (1+0.90)/2))
  null.signif.95 <- (0 < null.ci.95[1,]) | (0 > null.ci.95[2,])
  # null.signif.90 <- (0 < null.ci.90[1,]) | (0 > null.ci.90[2,])
  # null.signif <- ifelse(null.signif.90, '*', 0)
  # null.signif[null.signif.95] <- '**'
  ci<-data.frame(t(null.ci.95))
  colnames(ci)<-c("low", "high")
  se<-((ci$high-ci$low)/(2*qnorm(0.975)))
  tstat<-null.esti/se
  pval<-2*(1-pnorm(abs(tstat)))
  p_fdr<-p.adjust(as.matrix(pval[1:9]), method = "fdr")
  
  null.result.all <- data.frame(null.esti[1:9], ci[1:9, ], pval[1:9], p_fdr)
  null.result.all[,1:5] <- round(null.result.all[,1:5], digits=4)
  colnames(null.result.all) <- c("esti", "Low 95%CI", "High 95%CI", "P-val", "P-FDR")
  
  rownames(null.result.all) <- c("EA GPS", "ADI", "Years", "Poverty", "Good Parenting", "Good Schooling", 
                                 "Parental Education", "Family Income", "Financial Adversity")
  
  res_miao2<-cbind(res_miao2, null.result.all)
  
}


print(res_miao2)

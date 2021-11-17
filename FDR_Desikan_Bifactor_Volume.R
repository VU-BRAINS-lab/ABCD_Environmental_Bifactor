setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(MplusAutomation)

## Convert Mplus to R
# read mplus output file
modelResults <- readModels("SEMAnalysisVol_Original_catfixed_rescor.out") 

# read standardized results
standardizedResults <- modelResults[["parameters"]][["stdyx.standardized"]]

# read the index of regression results (can find in "paramHeader" column of standardizedResults; ending in ".ON")
reg_i=grep(".ON", standardizedResults$paramHeader)

# read the index of results with "GENERAL" factor (can find in "param" column of standardizedResults)
gen_i=grep("GENERAL", standardizedResults$param)
# read the index of results with "FamHX" factor
fam_i=grep("FAMHX", standardizedResults$param)
# read the index of results with "CareGvr" factor
cgvr_i=grep("CAREGVR", standardizedResults$param)
# read the index of results with "Environ" factor
env_i=grep("ENVIRON", standardizedResults$param)
# read the index of results with "SES" factor
ses_i=grep("SES", standardizedResults$param)

# Find the index of Vol regression results for each factor
reg_gen=intersect(reg_i, gen_i)
reg_fam=intersect(reg_i, fam_i)
reg_cgvr=intersect(reg_i, cgvr_i)
reg_env=intersect(reg_i, env_i)
reg_ses=intersect(reg_i, ses_i)


# Find the stat estimation/se of Vol regression results for each factor
gen_est_se <- standardizedResults$est_se[reg_gen]
fam_est_se <- standardizedResults$est_se[reg_fam]
cgvr_est_se <- standardizedResults$est_se[reg_cgvr]
env_est_se <- standardizedResults$est_se[reg_env]
ses_est_se <- standardizedResults$est_se[reg_ses]


# Find the stat estimation of Vol regression results for each factor
gen_est <- standardizedResults$est[reg_gen]
fam_est <- standardizedResults$est[reg_fam]
cgvr_est <- standardizedResults$est[reg_cgvr]
env_est <- standardizedResults$est[reg_env]
ses_est <- standardizedResults$est[reg_env]


# Find the se of Vol regression results for each factor
gen_se <- standardizedResults$se[reg_gen]
fam_se <- standardizedResults$se[reg_fam]
cgvr_se <- standardizedResults$se[reg_cgvr]
env_se <- standardizedResults$se[reg_env]
ses_se <- standardizedResults$se[reg_ses]


# Find the p value of Vol regression results for each factor
gen_p=standardizedResults$pval[reg_gen]
fam_p=standardizedResults$pval[reg_fam]
cgvr_p=standardizedResults$pval[reg_cgvr]
env_p=standardizedResults$pval[reg_env]
ses_p=standardizedResults$pval[reg_ses]


# Perform fdr
gen_p_fdr <- p.adjust(gen_p, method="fdr")
fam_p_fdr <- p.adjust(fam_p, method="fdr")
cgvr_p_fdr <- p.adjust(cgvr_p, method="fdr")
env_p_fdr <- p.adjust(env_p, method="fdr")
ses_p_fdr <- p.adjust(ses_p, method="fdr")


# combine stat results
est_comb <- cbind(gen_est, fam_est, cgvr_est, env_est, ses_est)
p_comb <- cbind(gen_p, fam_p, cgvr_p, env_p, ses_p)
fdr_comb <- cbind(gen_p_fdr, fam_p_fdr, cgvr_p_fdr, env_p_fdr, ses_p_fdr)

# get R^2
#R2<-cbind(modelResults[["parameters"]][["r2"]][["param"]], modelResults[["parameters"]][["r2"]][["est"]])
#BrainR2<-R2[which(R2[,1]=="VOL_1"):which(R2[,1]=="VOL_87"),2]

gen<-cbind(gen_est, gen_se, gen_est_se, gen_p, gen_p_fdr)
fam<-cbind(fam_est, fam_se, fam_est_se, fam_p, fam_p_fdr)
cgvr<-cbind(cgvr_est, cgvr_se, cgvr_est_se, cgvr_p, cgvr_p_fdr)
env<-cbind(env_est, env_se, env_est_se, env_p, env_p_fdr)
ses<-cbind(ses_est, ses_se, ses_est_se, ses_p, ses_p_fdr)
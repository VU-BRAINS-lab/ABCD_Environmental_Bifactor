setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(MplusAutomation)

## Convert Mplus to R
# read mplus output file
modelResults <- readModels("env_psy_rescor.out")

# read standardized results
standardizedResults <- modelResults[["parameters"]][["stdyx.standardized"]]

# read the index of regression results (can find in "paramHeader" column of standardizedResults; ending in ".ON")
reg_i=grep(".ON", standardizedResults$paramHeader)

# read the index of results with "GENERAL" factor (can find in "param" column of standardizedResults)
gen_i=grep("GENERALP", standardizedResults$paramHeader)
# read the index of results with "INT" factor
int_i=grep("INT", standardizedResults$paramHeader)
# read the index of results with "EXT" factor
ext_i=grep("EXT", standardizedResults$paramHeader)
# read the index of results with "ADHD" factor
adhd_i=grep("ADHD", standardizedResults$paramHeader)

# Find the index of regression results for each factor
reg_gen=intersect(reg_i, gen_i)
reg_int=intersect(reg_i, int_i)
reg_ext=intersect(reg_i, ext_i)
reg_adhd=intersect(reg_i, adhd_i)

# Find the stat estimation/se of regression results for each factor
gen_est_se <- standardizedResults$est_se[reg_gen]
int_est_se <- standardizedResults$est_se[reg_int]
ext_est_se <- standardizedResults$est_se[reg_ext]
adhd_est_se <- standardizedResults$est_se[reg_adhd]

# Find the stat estimation of CT regression results for each factor
gen_est <- standardizedResults$est[reg_gen]
int_est <- standardizedResults$est[reg_int]
ext_est <- standardizedResults$est[reg_ext]
adhd_est <- standardizedResults$est[reg_adhd]

# Find the se of CT regression results for each factor
gen_se <- standardizedResults$se[reg_gen]
int_se <- standardizedResults$se[reg_int]
ext_se <- standardizedResults$se[reg_ext]
adhd_se <- standardizedResults$se[reg_adhd]

# Find the p value of CT regression results for each factor
gen_p=standardizedResults$pval[reg_gen]
int_p=standardizedResults$pval[reg_int]
ext_p=standardizedResults$pval[reg_ext]
adhd_p=standardizedResults$pval[reg_adhd]

# Perform fdr
gen_p_fdr <- p.adjust(gen_p, method="fdr")
int_p_fdr <- p.adjust(int_p, method="fdr")
ext_p_fdr <- p.adjust(ext_p, method="fdr")
adhd_p_fdr <- p.adjust(adhd_p, method="fdr")


gen<-cbind(gen_est, gen_se, gen_est_se, gen_p, gen_p_fdr)
int<-cbind(int_est, int_se, int_est_se, int_p, int_p_fdr)
ext<-cbind(ext_est, ext_se, ext_est_se, ext_p, ext_p_fdr)
adhd<-cbind(adhd_est, adhd_se, adhd_est_se, adhd_p, adhd_p_fdr)

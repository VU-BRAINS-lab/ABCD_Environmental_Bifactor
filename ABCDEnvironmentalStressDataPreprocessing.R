library(compare);library(gdata); library(psych); library(corrplot); 
library(dplyr); library(multiplex); library(moments); library(qgraph)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load data ##
data<-readRDS('no_exclusions_cv.rds')
# change characters to numeric
indx<-sapply(data, is.character)
data[indx]<-lapply(data[indx], function(x) as.numeric(x))

## get average if data from multiple current addresses is available ##
# add pm25_2016_annual average variable (average pm25 across primary, secondary, teritiary addresses)
data$reshist_pm25_2016_annual_avg<-rowMeans(data[,c(which(colnames(data)=="reshist_addr1_pm252016aa"), which(colnames(data)=="reshist_addr2_pm252016aa"), which(colnames(data)=="reshist_addr3_pm252016aa"))], na.rm=TRUE)
# add lead risk housing average variable (average lead risk across primary, secondary, tertiary addresses)
data$reshist_leadrisk_housing<-rowMeans(data[,c(which(colnames(data)=="reshist_addr1_leadrisk_housing"), which(colnames(data)=="reshist_addr2_leadrisk_housing"), which(colnames(data)=="reshist_addr3_leadrisk_housing"))], na.rm=TRUE)
# add lead risk average variable (average lead risk across primary, secondary, tertiary addresses)
data$reshist_leadrisk<-rowMeans(data[,c(which(colnames(data)=="reshist_addr1_leadrisk"), which(colnames(data)=="reshist_addr2_leadrisk"), which(colnames(data)=="reshist_addr3_leadrisk"))], na.rm=TRUE)
# add lead risk housing average variable (average lead risk across primary, secondary, tertiary addresses)
data$reshist_leadrisk_poverty<-rowMeans(data[,c(which(colnames(data)=="reshist_addr1_leadrisk_poverty"), which(colnames(data)=="reshist_addr2_leadrisk_poverty"), which(colnames(data)=="reshist_addr3_leadrisk_poverty"))], na.rm=TRUE)

## select stress variables for factor analysis ##
# read stress variables
stress.variables<-read.csv("Variables.csv", header=TRUE)
# unlist for comparison
stress.variables<-unlist(stress.variables)
# get colnames of data
stress.colnames<-colnames(data)
# unlist for comparison
stress.colnames<-unlist(stress.colnames)
# find the index of data that are stress variables
stress.indx<-unique(match(stress.variables, stress.colnames))
# make data frame of stress variables
stressor.data<-data[,stress.colnames[stress.indx]]
stressor.data<-as.data.frame(stressor.data)

## exclusion (criteria 1 & 2 only apply to ordinal variables) ##
# read ordinal variables
ordinal.variables<-read.csv("Ordinal_Variables.csv", header=TRUE)
# unlsit for comparison
ordinal.variables<-unlist(ordinal.variables)
# find the index of data that are ordinal variables
ordinal.indx<-unique(match(ordinal.variables, stress.colnames))
# make data frame of ordinal variables
ordinal.data<-data[,stress.colnames[ordinal.indx]]
ordinal.data<-as.data.frame(ordinal.data)

## exclusion criteria 1: Endorsement < .5%
# make count table and proportion table
count_table<-as.matrix(list(data=NA))
prop_table<-as.matrix(list(data=NA))

for(i in 1:ncol(ordinal.data)){
  count_table[[i]]<-table(ordinal.data[i])
  prop_table[[i]]<-count_table[[i]]/11878*100
}

count_table<-setNames(count_table, colnames(ordinal.data))
prop_table<-setNames(prop_table, colnames(ordinal.data))

# get the list of variables with endorsement less than .5%
for(i in 1:ncol(ordinal.data)){
  if(any(prop_table[[i]]<.5)){
    print(names(prop_table[i]))
  }
}

# list of variables with low endorsement: 
# binary: ksads_ptsd_raw_758_p, ksads_ptsd_raw_759_p, ksads_ptsd_raw_761_p, ksads_ptsd_raw_762_p, 
# ksads_ptsd_raw_767_p
# ordinal: crpbi_parent4_y, kbi_p_how_well_c_school, kbi_p_grades_in_school
# kbi_p_c_det_susp, su_risk_p_5, parent_monitor_q1_y

## binary variables: remove from the analysis ##
table(stressor.data$ksads_ptsd_raw_758_p)/11878*100
table(stressor.data$ksads_ptsd_raw_759_p)/11878*100
table(stressor.data$ksads_ptsd_raw_761_p)/11878*100
table(stressor.data$ksads_ptsd_raw_762_p)/11878*100
table(stressor.data$ksads_ptsd_raw_767_p)/11878*100

stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="ksads_ptsd_raw_758_p"))]
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="ksads_ptsd_raw_759_p"))]
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="ksads_ptsd_raw_761_p"))]
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="ksads_ptsd_raw_762_p"))]
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="ksads_ptsd_raw_767_p"))]

## ordinal variables: combine responses with low endorsement ##
# 1. crpbi_parent4_y
table(stressor.data$crpbi_parent4_y)/11878*100
stressor.data$crpbi_parent4_y[stressor.data$crpbi_parent4_y==2]<-1 # combine 1 and 2
stressor.data$crpbi_parent4_y[stressor.data$crpbi_parent4_y==3]<-2 # recode 3 to 2
table(stressor.data$crpbi_parent4_y)/11878*100
# 2. kbi_p_how_well_c_school
table(stressor.data$kbi_p_how_well_c_school)/11878*100
stressor.data$kbi_p_how_well_c_school[stressor.data$kbi_p_how_well_c_school==4]<-3 # combine 4 and 3
table(stressor.data$kbi_p_how_well_c_school)/11878*100
# 3. kbi_p_grades_in_school
table(stressor.data$kbi_p_grades_in_school)/11878*100
stressor.data$kbi_p_grades_in_school[stressor.data$kbi_p_grades_in_school==6]<-NA # response 6 = ungraded
stressor.data$kbi_p_grades_in_school[stressor.data$kbi_p_grades_in_school==5]<-4
table(stressor.data$kbi_p_grades_in_school)/11878*100
# 4. kbi_p_c_det_susp
table(stressor.data$kbi_p_c_det_susp)/11878*100
stressor.data$kbi_p_c_det_susp[stressor.data$kbi_p_c_det_susp==777]<-NA # response 777 = decline to answer
table(stressor.data$kbi_p_c_det_susp)/11878*100
# 5. su_risk_p_5
table(stressor.data$su_risk_p_5)/11878*100
stressor.data$su_risk_p_5[stressor.data$su_risk_p_5==3]<-2 # combine 3 and 2
table(stressor.data$su_risk_p_5)/11878*100
# 6. parent_monitor_q1_y
table(stressor.data$parent_monitor_q1_y)/11878*100
stressor.data$parent_monitor_q1_y[stressor.data$parent_monitor_q1_y==2]<-1 # recode 2 to 1
stressor.data$parent_monitor_q1_y[stressor.data$parent_monitor_q1_y==3]<-2 # recode 3 to 2
stressor.data$parent_monitor_q1_y[stressor.data$parent_monitor_q1_y==4]<-3 # recode 4 to 3
stressor.data$parent_monitor_q1_y[stressor.data$parent_monitor_q1_y==5]<-4 # recode 5 to 4
table(stressor.data$parent_monitor_q1_y)/11878*100

## save new list to double check low endorsement items in the new list ##
write.csv(colnames(stressor.data), "LowEndorsementRemoved.csv")

## double check low endorsement items in the new list ##
# read updated ordinal variables (With continuous variables removed from LowEndorsementRemoved.csv)
ordinal.variables.updated<-read.csv("Ordinal_Variables_Updated.csv", header=TRUE)
# unlsit for comparison
ordinal.variables.updated<-unlist(ordinal.variables.updated)
# find the index of data that are ordinal variables
ordinal.indx.updated<-unique(match(ordinal.variables.updated, stress.colnames))
# make data frame of ordinal variables
ordinal.data.updated<-stressor.data[,stress.colnames[ordinal.indx.updated]]
ordinal.data.updated<-as.data.frame(ordinal.data.updated)

## exclusion criteria 1: Endorsement < .5%
# make count table and proportion table
count_table<-as.matrix(list(data=NA))
prop_table<-as.matrix(list(data=NA))

for(i in 1:ncol(ordinal.data.updated)){
  count_table[[i]]<-table(ordinal.data.updated[i])
  prop_table[[i]]<-count_table[[i]]/11878*100
}

count_table<-setNames(count_table, colnames(ordinal.data.updated))
prop_table<-setNames(prop_table, colnames(ordinal.data.updated))

# get the list of variables with endorsement less than .5% (use for binary variables)
for(i in 1:ncol(ordinal.data.updated)){
  if(any(prop_table[[i]]<.5)){
    print(names(prop_table[i]))
  }
}
                   
# get the list of variables with endorsement less than .5% (use for non-binary variables)
for(i in 1:ncol(ordinal.data.updated)){
  if(any(prop_table[[i]]>99.5)){
    print(names(prop_table[i]))
  }
}

## exclusion criteria 2: contingency table with 0
## make contingency table with all variables and check for cell with 0 ##
# if nothing comes up, there is no item with 0 cell in contingency table
contingency_table<-as.matrix(list(data=NA))
prop_vector<-list()
sublist<-list()
item1.name<-list()
item2.name<-list()
for(i in 1:ncol(ordinal.data.updated)){
  contingency_table[[i]]<-list()
  for(j in 1:ncol(ordinal.data.updated)){
    if(i != j){
      prop_vector<-table(ordinal.data.updated[,i], ordinal.data.updated[,j])
      item1.name<-names(ordinal.data.updated[i])
      item2.name<-names(ordinal.data.updated[j])
      sublist[[1]]<-item1.name
      sublist[[2]]<-item2.name
      sublist[[3]]<-prop_vector
      contingency_table[[i]][[j]]=sublist
      if(any(sublist[[3]]==0)){
        print(names(sublist[[1]]), names(sublist[[2]]))
      }
    }
  }
}

## check skewness of continuous items ##
# read continuous variables
continuous.variables<-read.csv("Continuous_Variables.csv", header=TRUE)
# unlsit for comparison
continuous.variables<-unlist(continuous.variables)
# find the index of data that are ordinal variables
continuous.indx<-unique(match(continuous.variables, stress.colnames))
# make data frame of continuous variables
continuous.data<-stressor.data[,stress.colnames[continuous.indx]]
continuous.data<-as.data.frame(continuous.data)

# get histogram of all continuous variables to check for skewness
my_hist <- lapply(c(1:ncol(continuous.data)), function(x) hist(continuous.data[,x], xlab=colnames(continuous.data[x])))
## check plots for skewness ##
# 1. items with negative skewness: reshist_addr1_adi_edu_h, reshist_addr1_adi_work_c, reshist_addr1_adi_home_o
# 2. items with positve skewness: demo_roster_v2, resiliency5a_y, resiliency6a_y, resiliency5b_y, resiliency6b_y
# reshist_addr1_elevation, reshist_addr1_d1a, reshist_addr1_grndtot, reshist_addr1_adi_edu_l
# reshist_addr1_adi_income, reshist_addr1_adi_home_v, reshist_addr1_adi_mortg
# reshist_addr1_adi_crowd, reshist_addr1_adi_unemp,
# reshist_addr1_adi_pov, reshist_addr1_adi_b138, reshist_addr1_adi_sp, reshist_addr1_adi_ncar, 
# reshist_addr1_adi_ntel, reshist_addr1_adi_nplumb, reshist_addr1_adi_perc,
# reshist_addr1_popdensity, reshist_addr1_no2, 
# reshist_addr1_proxrd, reshist_leadrisk_poverty, reshist_leadrisk_housing

## double check with skewness function ##
# cut-off is |.5|
skewness(continuous.data, na.rm=TRUE)
# consistent with decision based on histogram (check SkewnessChecking.xlsx for outcome)

## remove outliers ##
stressor.data$demo_roster_v2[stressor.data$demo_roster_v2>11]<-NA
stressor.data$resiliency5a_y[stressor.data$resiliency5a_y>43]<-NA
stressor.data$resiliency5b_y[stressor.data$resiliency5b_y>13]<-NA
stressor.data$resiliency6a_y[stressor.data$resiliency6a_y>46]<-NA
stressor.data$resiliency6b_y[stressor.data$resiliency6b_y>14]<-NA

## log transformation ##
# reverse negatively skewed items and log transform
stressor.data$reshist_addr1_adi_edu_h<-log(101-stressor.data$reshist_addr1_adi_edu_h)
stressor.data$reshist_addr1_adi_work_c<-log(101-stressor.data$reshist_addr1_adi_work_c)
stressor.data$reshist_addr1_adi_home_o<-log(101-stressor.data$reshist_addr1_adi_home_o)

# 1. add 1 for all variables that will be log transformed (log(0) will cause error)
stressor.data$reshist_addr1_elevation<-stressor.data$reshist_addr1_elevation+1
stressor.data$reshist_addr1_d1a<-stressor.data$reshist_addr1_d1a+1
stressor.data$reshist_addr1_grndtot<-stressor.data$reshist_addr1_grndtot+1
stressor.data$reshist_addr1_adi_edu_l<-stressor.data$reshist_addr1_adi_edu_l+1
stressor.data$reshist_addr1_adi_income<-stressor.data$reshist_addr1_adi_income+1
stressor.data$reshist_addr1_adi_home_v<-stressor.data$reshist_addr1_adi_home_v+1
stressor.data$reshist_addr1_adi_mortg<-stressor.data$reshist_addr1_adi_mortg+1
stressor.data$reshist_addr1_adi_crowd<-stressor.data$reshist_addr1_adi_crowd+1
stressor.data$reshist_addr1_adi_unemp<-stressor.data$reshist_addr1_adi_unemp+1
stressor.data$reshist_addr1_adi_pov<-stressor.data$reshist_addr1_adi_pov+1
stressor.data$reshist_addr1_adi_b138<-stressor.data$reshist_addr1_adi_b138+1
stressor.data$reshist_addr1_adi_sp<-stressor.data$reshist_addr1_adi_sp+1
stressor.data$reshist_addr1_adi_ncar<-stressor.data$reshist_addr1_adi_ncar+1
stressor.data$reshist_addr1_adi_ntel<-stressor.data$reshist_addr1_adi_ntel+1
stressor.data$reshist_addr1_adi_nplumb<-stressor.data$reshist_addr1_adi_nplumb+1
#stressor.data$reshist_addr1_adi_perc <-stressor.data$reshist_addr1_adi_perc+1
stressor.data$reshist_addr1_popdensity<-stressor.data$reshist_addr1_popdensity+1
stressor.data$reshist_addr1_no2<-stressor.data$reshist_addr1_no2+1
#stressor.data$reshist_addr1_proxrd<-stressor.data$reshist_addr1_proxrd+1 # the min of proxrd is .01 so no need to add 1
stressor.data$reshist_leadrisk_poverty<-stressor.data$reshist_leadrisk_poverty+1
stressor.data$reshist_leadrisk_housing<-stressor.data$reshist_leadrisk_housing+1
# 2. log transformation
stressor.data$reshist_addr1_elevation<-log(stressor.data$reshist_addr1_elevation)
stressor.data$reshist_addr1_d1a<-log(stressor.data$reshist_addr1_d1a)
stressor.data$reshist_addr1_grndtot<-log(stressor.data$reshist_addr1_grndtot)
stressor.data$reshist_addr1_adi_edu_l<-log(stressor.data$reshist_addr1_adi_edu_l)
stressor.data$reshist_addr1_adi_income<-log(stressor.data$reshist_addr1_adi_income)
stressor.data$reshist_addr1_adi_home_v<-log(stressor.data$reshist_addr1_adi_home_v)
stressor.data$reshist_addr1_adi_mortg<-log(stressor.data$reshist_addr1_adi_mortg)
stressor.data$reshist_addr1_adi_crowd<-log(stressor.data$reshist_addr1_adi_crowd)
stressor.data$reshist_addr1_adi_unemp<-log(stressor.data$reshist_addr1_adi_unemp)
stressor.data$reshist_addr1_adi_pov<-log(stressor.data$reshist_addr1_adi_pov)
stressor.data$reshist_addr1_adi_b138<-log(stressor.data$reshist_addr1_adi_b138)
stressor.data$reshist_addr1_adi_sp<-log(stressor.data$reshist_addr1_adi_sp)
stressor.data$reshist_addr1_adi_ncar<-log(stressor.data$reshist_addr1_adi_ncar)
stressor.data$reshist_addr1_adi_ntel<-log(stressor.data$reshist_addr1_adi_ntel)
stressor.data$reshist_addr1_adi_nplumb<-log(stressor.data$reshist_addr1_adi_nplumb)
#stressor.data$reshist_addr1_adi_perc<-log(stressor.data$reshist_addr1_adi_perc)
stressor.data$reshist_addr1_popdensity<-log(stressor.data$reshist_addr1_popdensity)
stressor.data$reshist_addr1_no2<-log(stressor.data$reshist_addr1_no2)
stressor.data$reshist_addr1_proxrd<-log(stressor.data$reshist_addr1_proxrd)
stressor.data$reshist_leadrisk_poverty<-log(stressor.data$reshist_leadrisk_poverty)
stressor.data$reshist_leadrisk_housing<-log(stressor.data$reshist_leadrisk_housing)

## Adjust high variance ##
## # make data frame of continuous variables: updated after log transformation
continuous.data.updated<-stressor.data[,stress.colnames[continuous.indx]]
continuous.data.updated<-as.data.frame(continuous.data.updated)
# check mean and variance of continuous variables 
mean.continuous<-sapply(continuous.data.updated, mean, na.rm=TRUE)
sd.continuous<-sapply(continuous.data.updated, sd, na.rm=TRUE)
min.continuous<-sapply(continuous.data.updated, min, na.rm=TRUE)
max.continuous<-sapply(continuous.data.updated, max, na.rm=TRUE)
values.continuous<-cbind(mean.continuous, sd.continuous, min.continuous, max.continuous)
write.csv(values.continuous, "Continuous_Values.csv")

## variables with high variance: reshist_addr1_adi_rent
# adjust variables with high variance by dividing with arbitrary numbers

# reshist_addr1_adi_rent
sd(stressor.data$reshist_addr1_adi_rent, na.rm=TRUE)
rent_adj<-stressor.data$reshist_addr1_adi_rent/100
sd(rent_adj, na.rm=TRUE)
stressor.data$reshist_addr1_adi_rent<-rent_adj

## check for mean, sd, min, max for all stressor variables. check for any weird values ##
mean.data<-sapply(stressor.data, mean, na.rm=TRUE)
sd.data<-sapply(stressor.data, sd, na.rm=TRUE)
min.data<-sapply(stressor.data, min, na.rm=TRUE)
max.data<-sapply(stressor.data, max, na.rm=TRUE)
values.data<-cbind(mean.data, sd.data, min.data, max.data)
write.csv(values.data, "Stressor_Values.csv")

## check for correlation > .9 or < -.9 ##
xcor <- cor_auto(stressor.data)
names<-colnames(xcor)
sublist<-list()
item1.name<-list()
item2.name<-list()
coeff<-list()
correlation_table<-as.matrix(list(data=NA))
for(i in 1:ncol(xcor)){
  for (j in 1:ncol(xcor)){
    if(i!=j){
      if(xcor[i,j] < -.9){ # switch to < -.9
        item1.name<-names[i]
        item2.name<-names[j]
        coeff<-xcor[i,j]
        print(item1.name)
        print(item2.name)
        print(coeff)
      }
    }
  }
}

## correlation pairs higher than .9 ##
### positive (> .9)
# 1. reshist_addr1_adi_home_v, reshist_addr1_adi_mortg: 0.9507501
# 2. reshist_addr1_adi_b138, reshist_addr1_leadrisk_poverty: 0.9828699


## retain more representative variables and remove redundant variables ##
# poverty: retain reshist_addr1_adi_b138 and drop reshist_addr1_leadrisk_poverty
# adi: drop reshist_addr1_adi_home_v (retain reshist_addr1_adi_perc and reshist_addr1_adi_mortg by default)
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="reshist_leadrisk_poverty"))]
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="reshist_addr1_adi_mortg"))]


## ICLUST to identify items forming doublets ##
xcor_updated<-cor_auto(stressor.data)
clust<-ICLUST(xcor_updated)
pdf(ICLUST(xcor_updated), height=20, width=12)
# save PDF to check for ICLUST

## ICLUST results: item pairs forming doublets (>=.85) ##
# su_risk_p_5, su_risk_p_4: 0.93
# * leave su_risk_p_4
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="su_risk_p_5"))]
# reshist_addr1_popdensity, reshist_addr1_d1a: 0.9
# * leave reshist_addr1_popdensity
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="reshist_addr1_d1a"))]
# kbi_p_grades_in_school, kbi_p_how_well_c_school: 0.95
# * leave kbi_p_grades_in_school
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="kbi_p_how_well_c_school"))]
# demo_fam_exp7_v2, demo_fam_exp6_v2: 0.92
# * collapse
stressor.data$demo_fam_exp6_exp7<-rowMeans(stressor.data[,c(which(colnames(stressor.data)=="demo_fam_exp6_v2"), which(colnames(stressor.data)=="demo_fam_exp7_v2"))], na.rm=TRUE) 
stressor.data$demo_fam_exp6_exp7[stressor.data$demo_fam_exp6_exp7==0.5]<-1
# ksads_ptsd_raw_768_p, ksads_ptsd_raw_764_p: 0.9# ksads_ptsd_raw_768_p, ksads_ptsd_raw_764_p: 0.9
# * collapse
stressor.data$ksads_764_768<-rowMeans(stressor.data[,c(which(colnames(stressor.data)=="ksads_ptsd_raw_764_p"), which(colnames(stressor.data)=="ksads_ptsd_raw_768_p"))], na.rm=TRUE) 
stressor.data$ksads_764_768[stressor.data$ksads_764_768==0.5]<-1
# reshist_addr1_adi_b138, reshist_addr1_adi_pov: 0.95
# * leave reshist_addr1_adi_b138
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="reshist_addr1_adi_pov"))]
# reshist_addr1_adi_edu_h, reshist_addr1_adi_edu_l: 0.94
# * leave reshist_addr1_adi_edu_h
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="reshist_addr1_adi_edu_l"))]
# reshist_leadrisk, reshist_leadrisk_housing: 0.93
# * leave reshist_leadrisk_housing
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="reshist_leadrisk"))]
# neighborhood3r_p, neighborhood2r_p: 0.91
# * leave neighborhood3r_p
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="neighborhood2r_p"))]
# su_risk_p_2, su_risk_p_3: ICLUST=0.89
# * leave su_risk_p_2
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="su_risk_p_3"))]
# kbi_p_c_spec_serv___1, kbi_p_c_spec_serv___2: ICLUST=0.85
# * leave kbi_p_c_spec_serv___2
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="kbi_p_c_spec_serv___1"))]
# fam_history_6_yes_no, fam_history_11_yes_no: ICLUST: 0.86
# * leave fam_history_11_yes_no
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="fam_history_6_yes_no"))]
# school_15_y, school_12_y: ICLUST: 0.85
# * leave school_15_y
stressor.data<-stressor.data[,-c(which(colnames(stressor.data)=="school_12_y"))]
# ksads_ptsd_raw_765_p, ksads_ptsd_raw_763_p: ICLUST=0.89
# * collapse
stressor.data$ksads_763_765<-rowMeans(stressor.data[,c(which(colnames(stressor.data)=="ksads_ptsd_raw_763_p"), which(colnames(stressor.data)=="ksads_ptsd_raw_765_p"))], na.rm=TRUE) 
stressor.data$ksads_763_765[stressor.data$ksads_763_765==0.5]<-1
# demo_fam_exp4_v2, demo_fam_exp3_v2: ICLUST=0.89, r=.79
# * collapse
stressor.data$demo_fam_exp3_exp4<-rowMeans(stressor.data[,c(which(colnames(stressor.data)=="demo_fam_exp3_v2"), which(colnames(stressor.data)=="demo_fam_exp4_v2"))], na.rm=TRUE) 
stressor.data$demo_fam_exp3_exp4[stressor.data$demo_fam_exp3_exp4==0.5]<-1
# demo_fam_exp2_v2, demo_fam_exp1_v2: ICLUST=0.89, r=.80
# * collapse
stressor.data$demo_fam_exp1_exp2<-rowMeans(stressor.data[,c(which(colnames(stressor.data)=="demo_fam_exp1_v2"), which(colnames(stressor.data)=="demo_fam_exp2_v2"))], na.rm=TRUE) 
stressor.data$demo_fam_exp1_exp2[stressor.data$demo_fam_exp1_exp2==0.5]<-1

# randomly select 9000 samples for ESEM
set.seed(1234)
# extract variables for factor analysis (subject, psweight, famid, siten, wt_NR_mwacs)
data.fa<-data[,c(1,6,7,10,808)]
stressor.data<-cbind(data.fa, stressor.data)
indx<-sapply(stressor.data, is.factor) # some variables are saved as factors in the new version (eg. siten) Change them to numeric for NA conversion. Otherwise, <NA> will not be converted to .
stressor.data[indx]<-lapply(stressor.data[indx], function(x) as.numeric(x))
# esem data: 9000, cfa data: rest (2878)
esem_data<-sample_n(stressor.data, 9000)
cfa_data<-stressor.data[-c(esem_data$subnum_char),]

## save .dat for Mplus running ##
# replace NA with .
esem_data[is.na(esem_data)] <- "." 
cfa_data[is.na(cfa_data)]<-"."
## 1. save data to run with wt_NR_mwacs (decided over wt_NR_cmwacs)
# exclude subjects with missing wt_NR_mwacs
Missing_NR_mwacs_Indx_esem<-which(esem_data$wt_NR_mwacs==".") # missing N= 1555
Missing_NR_mwacs_Indx_cfa<-which(cfa_data$wt_NR_mwacs==".") # missing N = 505
esem_data_mwacs<-esem_data[-c(Missing_NR_mwacs_Indx_esem),] # N = 7445
cfa_data_mwacs<-cfa_data[-c(Missing_NR_mwacs_Indx_cfa),] # N = 2373
# write dat
write.dat(esem_data_mwacs, "esem_data")
write.dat(cfa_data_mwacs, "cfa_data")
# save variable names (same for esem and cfa data)
write.csv(names(esem_data_mwacs), "esem_names.csv")
                            
## scree plot to check number of factors ##
plot(eigen(cor(stressor.data, use="pairwise"))$values[1:50])

saveRDS(stressor.data, "stressor.data.RDA")
## make dataset with brain variables ##

## Cortical Thickness
CT.data<-readRDS("Cortical_Thickness_no_cbcl.rds")
stressor.data<-readRDS("stressor.data")
ct_id<-CT.data$subnum_char # id with ct data
stressor.data.ct<-stressor.data[match(ct_id, stressor.data$subnum_char),]
#stressor.data.ct<-stressor.data[ct_id,] # stressor data with participants that have ct data
data.ct<-CT.data[,c(1,3,4,12:15,119,126:130,131,133,749,752:819)] # extract relevant data from ct
# subnum_char, age, female, NHWhite, African, Hispanic, Other, ticv, coil1-5, income, parent_education
# desikan_mean, desikan

# merge data
AnalysisDataCT<-merge(stressor.data.ct, data.ct, by="subnum_char")

## save Rdata
saveRDS(AnalysisDataCT, "AnalysisDataCT.Rda")

## save .dat for Mplus
# replace NA with .
AnalysisDataCT[is.na(AnalysisDataCT)] <- "." 
write.dat(AnalysisDataCT, "Analysis Data CT")
write.csv(names(AnalysisDataCT), "AnalysisDataCTNames.csv")

## Volume
Vol.data<-readRDS("Cortical_Volume_no_cbcl.rds")
vol_id<-Vol.data$subnum_char
stressor.data.vol<-stressor.data[match(vol_id, stressor.data$subnum_char),]
data.vol<-Vol.data[,c(1,3,4,12:15,119,126:131,133,748,752:819,
                      863:867,870:872,875,876,881:887,889,890,858)]

# subnum_char, age, female, NHWhite, African, Hispanic, Other, ticv, coil1-5, income, parent_education
# subcortical_sum, 
# Desikan (smri_vol_cdk_banksstslh:smri_vol_cdk_insularh)
# smri_vol_scs_crbcortexlh: 863
# smri_vol_scs_tplh: 864
# smri_vol_scs_caudatelh: 865
# smri_vol_scs_putamenlh: 866
# smri_vol_scs_pallidumlh: 867
# smri_vol_scs_bstem: 870
# smri_vol_scs_hpuslh: 871
# smri_vol_scs_amygdalalh: 872
# smri_vol_scs_aal: 875
# smri_vol_scs_vedclh: 876
# smri_vol_scs_crbcortexrh: 881
# smri_vol_scs_tprh: 882
# smri_vol_scs_caudaterh: 883
# smri_vol_scs_putamenrh: 884
# smri_vol_scs_pallidumrh: 885
# smri_vol_scs_hpusrh: 886
# smri_vol_scs_amygdalarh: 887
# smri_vol_scs_aar: 889
# smri_vol_scs_vedcrh: 890
# whole brain: 858
AnalysisDataVol<-merge(stressor.data.vol, data.vol, by="subnum_char")
## save Rdata
saveRDS(AnalysisDataVol, "AnalysisDataVol.Rda")

## save .dat for Mplus
# replace NA with .
AnalysisDataVol[is.na(AnalysisDataVol)] <- "." 
write.dat(AnalysisDataVol, "Analysis Data Vol")
write.csv(names(AnalysisDataVol), "AnalysisDataVolNames.csv")

## because variance is too high for volume, let's reduce by dividing vol by 1000
AnalysisDataVol.Adj<-merge(stressor.data.vol, data.vol, by="subnum_char")
AnalysisDataVol.Adj[,142:229]<-AnalysisDataVol.Adj[,142:229]/1000
saveRDS(AnalysisDataVol.Adj, "AnalysisDataVol_Adj.RDS")

## save .dat for Mplus
# replace NA with .
AnalysisDataVol.Adj[is.na(AnalysisDataVol.Adj)] <- "."
write.dat(AnalysisDataVol.Adj, "Analysis Data Vol Adj")
write.csv(names(AnalysisDataVol.Adj), "AnalysisDataVolAdjNames.csv")

## save cbcl and stressor data
no.exclusion.data<-readRDS("no_exclusions_cv.rds")
cbcl.data<-no.exclusion.data[,c(1,16:88)]
cbcl.data.ct<-cbcl.data[match(ct_id, cbcl.data$subnum_char),]
AnalysisDataStressorCBCL<-merge(cbcl.data.ct, stressor.data.ct, by="subnum_char")
## save .dat for Mplus
# replace NA with .
AnalysisDataStressorCBCL[is.na(AnalysisDataStressorCBCL)] <- "." 
write.dat(AnalysisDataStressorCBCL, "Analysis Data CBCL Stressor")
write.csv(names(AnalysisDataStressorCBCL), "AnalysisDataStressorCBCLNames.csv")

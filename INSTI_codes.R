rm(list=ls())

library(lubridate)
library(ggplot2)

orignal<-read.csv("C:/Users/cuisa/Desktop/PCCP/INSTI/original_data.csv",header = T)
updated<-read.csv("C:/Users/cuisa/Desktop/PCCP/INSTI/updated_data.csv",header = T)


orignal$startdate_hiv_vl_new<-as.numeric(as.character(orignal$startdate_hiv_vl))
orignal$startdate_hiv_vl_new[c(1,4,5,98,183,148,166)]<-24
orignal$startdate_hiv_vl_new[c(27,33,38,39,46,55,77,120,155,157,
                               176,178,194,236,238,243,244,
                               279,294,302,303,308,312,314,346,363,
                               368,395,410,421,425,426,435,454,487,488,
                               491,505,509,520,525,527,544,549,553,555,561,
                               567,573,579,581)]<-10

orignal$startdate_hiv_vl_new[c(45,143,292,304,374,449,468)]<-0
orignal$startdate_hiv_vl_new[c(30,36,48,52,132,362,393,420,
                               430,459,467,477,481,
                               131,149,242,511,514,515,286,429)]<-NA
orignal$startdate_hiv_vl_new[c(580,587)]<-10000000



####Merge data###
updated_pre_merge<-updated[,c(1,8)]
mergedata<-merge(orignal,updated_pre_merge,by.x = "STUDY_ID", by.y = "study_id")

length(which(is.na(mergedata$weight_12_months)=="FALSE"))
length(which(is.na(mergedata$weight_18_months)=="FALSE"))
length(which(is.na(mergedata$wght_12_mth_1_plus5)=="FALSE"))


###data cleaning###

which(names(mergedata)=="Other_ART")
which(names(mergedata)=="X_merge")
which(names(mergedata)=="complete")
which(names(mergedata)=="date_ofbirthfrom_careware")

mergedata<-mergedata[,-c(78,81,59,3)]


a<-as.character(mergedata$date_of_birth)[1]
as.numeric(strsplit(a, '/')[[1]])[3]

#### Calculate age###
mergedata$date_of_birth<-as.character(mergedata$date_of_birth)
date_of_birth<-mdy(mergedata$date_of_birth)
date_of_start<-mdy(mergedata$startdate)
days<-difftime(date_of_start,date_of_birth, units="days") 
age<-floor(days/365)


mergedata<-cbind(mergedata,age)







####Collapsed Race ####
mergedata$race[which(mergedata$race=="AFRICAN AMERICAN")]<-levels(mergedata$race)[5]
mergedata$race[which(mergedata$race=="Caucasian")]<-levels(mergedata$race)[11]
mergedata$race[which(mergedata$race=="Caucasian" )]<-levels(mergedata$race)[11]


unknown<-c(
which(mergedata$race==levels(mergedata$race)[7]),
which(mergedata$race==levels(mergedata$race)[9]),
which(mergedata$race==levels(mergedata$race)[10])
)

mergedata$race[unknown]<-levels(mergedata$race)[10]



mergedata$ethnicity[which(mergedata$ethnicity=="Not Reported" )]<-levels(mergedata$ethnicity)[5]

####Destring variables that should be numeric####
mergedata$startdate_a1c<-as.numeric(as.character(mergedata$startdate_a1c))
mergedata$startdate_a1c<-as.numeric(as.character(mergedata$a1c_18_months))
mergedata$startdate_a1c<-as.numeric(as.character(mergedata$cr_12_months))
mergedata$startdate_a1c<-as.numeric(as.character(mergedata$ldl_18_months))
mergedata$startdate_a1c<-as.numeric(as.character(mergedata$hdl_18_months))
mergedata$startdate_a1c<-as.numeric(as.character(mergedata$total_chol_18_months))



####Created new useful variables (see Data Dictionary) ###
Weight_at_12<-rep(NA,607)
Weight_at_12[which(is.na(mergedata$weight_12_months)=="TRUE")]=0
Weight_at_12[which(is.na(mergedata$weight_12_months)=="FALSE")]=1
mergedata<-cbind(mergedata,Weight_at_12)

Weight_at_18<-rep(NA,607)
Weight_at_18[which(is.na(mergedata$weight_18_months)=="TRUE")]=0
Weight_at_18[which(is.na(mergedata$weight_18_months)=="FALSE")]=1
mergedata<-cbind(mergedata,Weight_at_18)

Weight_12_m1_p5<-rep(NA,607)
Weight_12_m1_p5[which(is.na(mergedata$wght_12_mth_1_plus5)=="TRUE")]=0
Weight_12_m1_p5[which(is.na(mergedata$wght_12_mth_1_plus5)=="FALSE")]=1
mergedata<-cbind(mergedata,Weight_12_m1_p5)


Weight_fu<-rep(NA,607)
Weight_fu[which(mergedata$Weight_at_12==0&mergedata$Weight_at_18==0&mergedata$Weight_12_m1_p5==0)]<-0
Weight_fu[which(mergedata$Weight_at_12==1)]<-1
Weight_fu[which(mergedata$Weight_at_12==0&mergedata$Weight_12_m1_p5==1)]<-2
Weight_fu[which(mergedata$Weight_at_12==0&mergedata$Weight_12_m1_p5==0&mergedata$Weight_at_18==1)]<-3
mergedata<-cbind(mergedata,Weight_fu)



length(which(mergedata$Weight_fu!=0))



### Make Indicators for Exclusion Criteria ###



preg_check<-rep(0,607)
preg_check[which(mergedata$gender=="Female"&mergedata$age<=45&mergedata$Weight_fu!=0)]<-1
mergedata<-cbind(mergedata,preg_check)










start_date1<-mdy(mergedata$START_DATE1)
stop_ART1<-mdy(mergedata$STOP_DATEN1)
stop_ART2<-mdy(mergedata$STOP_DATEN2)
stop_ART3<-mdy(mergedata$STOP_DATEN3)
stop_ART4<-mdy(mergedata$STOP_DATEN4)



days_on_ART1<-difftime(stop_ART1,start_date1, units="days") 
days_on_ART2<-difftime(stop_ART2,start_date1, units="days") 
days_on_ART3<-difftime(stop_ART3,start_date1, units="days") 
days_on_ART4<-difftime(stop_ART4,start_date1, units="days") 

mergedata<-cbind(mergedata,days_on_ART1,days_on_ART2,days_on_ART3,days_on_ART4)




mergedata1<-mergedata[,-c(2,3)]

mergedata2<-data.frame( mergedata1[,c(1:8)], weight_12_m1_p5=mergedata1[,77],hyperlipidemia=mergedata1[,46],diabetes=mergedata1[,49],mergedata1[,c(61:64)],
                       mergedata1[,c(78:87)], mergedata1[,c(71:73)],baselineVL=mergedata1[,76],start_bmi=mergedata1[,c(11:13)])


mergedata2$Diff_12<-mergedata2$weight_12_months-mergedata2$startdate_weight
mergedata2$Diff_18<-mergedata2$weight_18_months-mergedata2$startdate_weight
mergedata2$Diff_12m1p5<-mergedata2$weight_12_m1_p5-mergedata2$startdate_weight
mergedata2$Diff_12_18<-mergedata2$weight_12_months-mergedata2$weight_18_months



delete<-unique(c(which(mergedata2$age<18),
which(mergedata1$esrd=="Yes"),
which(mergedata2$days_on_ART1<365|mergedata2$days_on_ART2<365|mergedata2$days_on_ART3<365|mergedata2$days_on_ART4<365),
which(mergedata1$NNRTI==1&mergedata1$PI==1),
which(mergedata1$NNRTI==1&mergedata1$INSTI==1),
which(mergedata1$PI==1&mergedata1$INSTI==1),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==573368)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==802008)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==120098)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==327151)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==296865)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==104220)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==795708)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==944340)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==933323)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==053986)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==201331)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==339335)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==992332)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==033215)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==879166)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==206691)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==669507)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==356815)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==277234)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==245209)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==288406)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==772738)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==739717)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==441597)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==331195)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==053986)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==573368)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==033215)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==691302)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==690929)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==606927)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==490413)]),
which(mergedata2$gender==""),
which(mergedata2$race==""),
which(mergedata2$ethnicity==""),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==691302)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==773396)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==231343)]),
which(mergedata2$STUDY_ID== orignal$STUDY_ID[which(orignal$mrn==365546)])))


mergedata3<-mergedata2[-delete,]
mergedata3<-mergedata3[-490,]


missing_fu<-which(mergedata3$Weight_fu==0)

obs_fu<-which(mergedata3$Weight_fu!=0)

missing_at_12<-which(mergedata3$Weight_at_12==0)

obs_at_12<-which(mergedata3$Weight_at_12!=0)

missing_at_18<-which(mergedata3$Weight_at_18==0)

obs_at_18<-which(mergedata3$Weight_at_18!=0)

missing_12m1p5<-which(mergedata3$Weight_12_m1_p5==0)

obs_12m1p5<-which(mergedata3$Weight_12_m1_p5!=0)


# ttest weight

t.test(mergedata3$startdate_weight[obs_fu],mergedata3$startdate_weight[missing_fu],
       alternative = "two.sided",conf.level = 0.95)


t.test(mergedata3$startdate_weight[obs_fu],mergedata3$startdate_weight[missing_fu],
       alternative = "greater",conf.level = 0.95)

t.test(mergedata3$startdate_weight[obs_fu],mergedata3$startdate_weight[missing_fu],
       alternative = "less",conf.level = 0.95)


t.test(mergedata3$startdate_weight[obs_at_12],mergedata3$startdate_weight[missing_at_12],
       alternative = "two.sided",conf.level = 0.95)


t.test(mergedata3$startdate_weight[obs_at_12],mergedata3$startdate_weight[missing_at_12],
       alternative = "greater",conf.level = 0.95)

t.test(mergedata3$startdate_weight[obs_at_12],mergedata3$startdate_weight[missing_at_12],
       alternative = "less",conf.level = 0.95)



t.test(mergedata3$startdate_weight[obs_at_18],mergedata3$startdate_weight[missing_at_18],
       alternative = "two.sided",conf.level = 0.95)


t.test(mergedata3$startdate_weight[obs_at_18],mergedata3$startdate_weight[missing_at_18],
       alternative = "greater",conf.level = 0.95)

t.test(mergedata3$startdate_weight[obs_at_18],mergedata3$startdate_weight[missing_at_18],
       alternative = "less",conf.level = 0.95)



t.test(mergedata3$startdate_weight[obs_12m1p5],mergedata3$startdate_weight[missing_12m1p5],
       alternative = "two.sided",conf.level = 0.95)


t.test(mergedata3$startdate_weight[obs_12m1p5],mergedata3$startdate_weight[missing_12m1p5],
       alternative = "greater",conf.level = 0.95)

t.test(mergedata3$startdate_weight[obs_12m1p5],mergedata3$startdate_weight[missing_12m1p5],
       alternative = "less",conf.level = 0.95)
















# ttest baseline log vl
t.test(log(mergedata3$baselineVL[obs_fu][!is.na(mergedata3$baselineVL[obs_fu])])[-c(17,45,132)],
log(mergedata3$baselineVL[missing_fu][!is.na(mergedata3$baselineVL[missing_fu])])[-c(36,58,62)],
alternative = "two.sided",conf.level = 0.95)


t.test(log(mergedata3$baselineVL[obs_fu][!is.na(mergedata3$baselineVL[obs_fu])])[-c(17,45,132)],
       log(mergedata3$baselineVL[missing_fu][!is.na(mergedata3$baselineVL[missing_fu])])[-c(36,58,62)],
       alternative = "greater",conf.level = 0.95)


t.test(log(mergedata3$baselineVL[obs_fu][!is.na(mergedata3$baselineVL[obs_fu])])[-c(17,45,132)],
       log(mergedata3$baselineVL[missing_fu][!is.na(mergedata3$baselineVL[missing_fu])])[-c(36,58,62)],
       alternative = "less",conf.level = 0.95)


t.test(log(mergedata3$baselineVL[obs_at_12][!is.na(mergedata3$baselineVL[obs_at_12])])[-c(13,32,93)],
       log(mergedata3$baselineVL[missing_at_12][!is.na(mergedata3$baselineVL[missing_at_12])])[-c(63,106,115)],
       alternative = "two.sided",conf.level = 0.95)


t.test(log(mergedata3$baselineVL[obs_at_12][!is.na(mergedata3$baselineVL[obs_at_12])])[-c(13,32,93)],
       log(mergedata3$baselineVL[missing_at_12][!is.na(mergedata3$baselineVL[missing_at_12])])[-c(63,106,115)],
       alternative = "greater",conf.level = 0.95)


t.test(log(mergedata3$baselineVL[obs_at_12][!is.na(mergedata3$baselineVL[obs_at_12])])[-c(13,32,93)],
       log(mergedata3$baselineVL[missing_at_12][!is.na(mergedata3$baselineVL[missing_at_12])])[-c(63,106,115)],
       alternative = "less",conf.level = 0.95)



t.test(log(mergedata3$baselineVL[obs_at_18][!is.na(mergedata3$baselineVL[obs_at_18])])[-c(35,89)],
       log(mergedata3$baselineVL[missing_at_18][!is.na(mergedata3$baselineVL[missing_at_18])])[-c(10,63,118,124)],
       alternative = "two.sided",conf.level = 0.95)


t.test(log(mergedata3$baselineVL[obs_at_18][!is.na(mergedata3$baselineVL[obs_at_18])])[-c(35,89)],
       log(mergedata3$baselineVL[missing_at_18][!is.na(mergedata3$baselineVL[missing_at_18])])[-c(10,63,118,124)],
       alternative = "greater",conf.level = 0.95)


t.test(log(mergedata3$baselineVL[obs_at_18][!is.na(mergedata3$baselineVL[obs_at_18])])[-c(35,89)],
       log(mergedata3$baselineVL[missing_at_18][!is.na(mergedata3$baselineVL[missing_at_18])])[-c(10,63,118,124)],
       alternative = "less",conf.level = 0.95)


t.test(log(mergedata3$baselineVL[obs_12m1p5][!is.na(mergedata3$baselineVL[obs_12m1p5])])[-c(15,39,122)],
       log(mergedata3$baselineVL[missing_12m1p5][!is.na(mergedata3$baselineVL[missing_12m1p5])])[-c(45,69,74)],
       alternative = "two.sided",conf.level = 0.95)


t.test(log(mergedata3$baselineVL[obs_12m1p5][!is.na(mergedata3$baselineVL[obs_12m1p5])])[-c(15,39,122)],
       log(mergedata3$baselineVL[missing_12m1p5][!is.na(mergedata3$baselineVL[missing_12m1p5])])[-c(45,69,74)],
       alternative = "greater",conf.level = 0.95)


t.test(log(mergedata3$baselineVL[obs_12m1p5][!is.na(mergedata3$baselineVL[obs_12m1p5])])[-c(15,39,122)],
       log(mergedata3$baselineVL[missing_12m1p5][!is.na(mergedata3$baselineVL[missing_12m1p5])])[-c(45,69,74)],
       alternative = "less",conf.level = 0.95)




# ttest age
t.test(mergedata3$age[obs_fu],mergedata3$age[missing_fu],alternative = "two.sided",conf.level = 0.95)
t.test(mergedata3$age[obs_fu],mergedata3$age[missing_fu],alternative = "greater",conf.level = 0.95)
t.test(mergedata3$age[obs_fu],mergedata3$age[missing_fu],alternative = "less",conf.level = 0.95)

t.test(mergedata3$age[obs_at_12],mergedata3$age[missing_at_12],alternative = "two.sided",conf.level = 0.95)
t.test(mergedata3$age[obs_at_12],mergedata3$age[missing_at_12],alternative = "greater",conf.level = 0.95)
t.test(mergedata3$age[obs_at_12],mergedata3$age[missing_at_12],alternative = "less",conf.level = 0.95)


t.test(mergedata3$age[obs_at_18],mergedata3$age[missing_at_18],alternative = "two.sided",conf.level = 0.95)
t.test(mergedata3$age[obs_at_18],mergedata3$age[missing_at_18],alternative = "greater",conf.level = 0.95)
t.test(mergedata3$age[obs_at_18],mergedata3$age[missing_at_18],alternative = "less",conf.level = 0.95)

t.test(mergedata3$age[obs_12m1p5],mergedata3$age[missing_12m1p5],alternative = "two.sided",conf.level = 0.95)
t.test(mergedata3$age[obs_12m1p5],mergedata3$age[missing_12m1p5],alternative = "greater",conf.level = 0.95)
t.test(mergedata3$age[obs_12m1p5],mergedata3$age[missing_12m1p5],alternative = "less",conf.level = 0.95)








mergedata3$obs_or_missing<-rep(1,length(mergedata3[,1]))
mergedata3$obs_or_missing[missing_fu]<-0

# chi test gender
chisq.test(mergedata3$obs_or_missing,mergedata3$gender)
chisq.test(mergedata3$Weight_at_12,mergedata3$gender)
chisq.test(mergedata3$Weight_at_18,mergedata3$gender)
chisq.test(mergedata3$Weight_12_m1_p5,mergedata3$gender)




# chi test race
chisq.test(mergedata3$obs_or_missing,mergedata3$race)
chisq.test(mergedata3$Weight_at_12,mergedata3$race)
chisq.test(mergedata3$Weight_at_18,mergedata3$race)
chisq.test(mergedata3$Weight_12_m1_p5,mergedata3$race)

# chi test ethnicity
chisq.test(mergedata3$obs_or_missing,mergedata3$ethnicity)
chisq.test(mergedata3$Weight_at_12,mergedata3$ethnicity)
chisq.test(mergedata3$Weight_at_18,mergedata3$ethnicity)
chisq.test(mergedata3$Weight_12_m1_p5,mergedata3$ethnicity)




### Begin analysis

## Demographic
mergedata4<-mergedata3[obs_fu,]
mergedata4$age<-as.numeric(mergedata4$age)
median(mergedata4$age)
median(mergedata4$startdate_weight)
sum(1*(mergedata4$gender=="Male"))
sum(1*(mergedata4$gender=="Female"))
sum(1*(mergedata4$race=="Black or African American"))
sum(1*(mergedata4$race=="White"))
sum(1*(mergedata4$INSTI==1))
sum(1*(mergedata4$PI==1))
sum(1*(mergedata4$NNRTI==1))
quantile(mergedata4$startdate_weight[mergedata4$race=="Black or African American"],c(0.25,0.5,0.75))
quantile(mergedata4$startdate_weight[mergedata4$race=="White"],c(0.25,0.5,0.75))
quantile(mergedata4$startdate_weight[mergedata4$race!="Black or African American"&mergedata4$race!="White"],c(0.25,0.5,0.75))

quantile(mergedata4$startdate_weight[mergedata4$gender=="Male"],c(0.25,0.5,0.75))
quantile(mergedata4$startdate_weight[mergedata4$gender=="Female"],c(0.25,0.5,0.75))
quantile(mergedata4$startdate_weight[mergedata4$INSTI==1],c(0.25,0.5,0.75))
quantile(mergedata4$startdate_weight[mergedata4$PI==1],c(0.25,0.5,0.75))
quantile(mergedata4$startdate_weight[mergedata4$NNRTI==1],c(0.25,0.5,0.75))



quantile(mergedata4$start_bmi[mergedata4$race=="Black or African American"],c(0.25,0.5,0.75),na.rm = T)
quantile(mergedata4$start_bmi[mergedata4$race=="White"],c(0.25,0.5,0.75),na.rm = T)
quantile(mergedata4$start_bmi[mergedata4$race!="Black or African American"&mergedata4$race!="White"],c(0.25,0.5,0.75),na.rm = T)

quantile(mergedata4$start_bmi[mergedata4$gender=="Male"],c(0.25,0.5,0.75),na.rm = T)
quantile(mergedata4$start_bmi[mergedata4$gender=="Female"],c(0.25,0.5,0.75),na.rm = T)
quantile(mergedata4$start_bmi[mergedata4$INSTI==1],c(0.25,0.5,0.75),na.rm = T)
quantile(mergedata4$start_bmi[mergedata4$PI==1],c(0.25,0.5,0.75),na.rm = T)
quantile(mergedata4$start_bmi[mergedata4$NNRTI==1],c(0.25,0.5,0.75),na.rm = T)



#Analysis

t.test(mergedata4$startdate_weight[obs_at_12],mergedata4$weight_12_months[obs_at_12],
       alternative = "two.sided", conf.level = 0.95)


t.test(mergedata4$startdate_weight[obs_at_12],mergedata4$weight_12_months[obs_at_12],
       alternative = "greater", conf.level = 0.95)

t.test(mergedata4$startdate_weight[obs_at_12],mergedata4$weight_12_months[obs_at_12],
       alternative = "less", conf.level = 0.95)



t.test(mergedata4$startdate_weight[obs_at_18],mergedata4$weight_12_months[obs_at_18],
       alternative = "two.sided", conf.level = 0.95)


t.test(mergedata4$startdate_weight[obs_at_18],mergedata4$weight_12_months[obs_at_12],
       alternative = "greater", conf.level = 0.95)

t.test(mergedata4$startdate_weight[obs_at_18],mergedata4$weight_12_months[obs_at_12],
       alternative = "less", conf.level = 0.95)


t.test(mergedata4$startdate_weight[obs_12m1p5],mergedata4$weight_12_months[obs_at_18],
       alternative = "two.sided", conf.level = 0.95)


t.test(mergedata4$startdate_weight[obs_12m1p5],mergedata4$weight_12_months[obs_at_12],
       alternative = "greater", conf.level = 0.95)

t.test(mergedata4$startdate_weight[obs_12m1p5],mergedata4$weight_12_months[obs_at_12],
       alternative = "less", conf.level = 0.95)

mergedata4$merge_weight<-rep(0,348)

mergedata4$merge_weight[which(mergedata4$Weight_at_12==1)]<-mergedata4$weight_12_months[which(mergedata4$Weight_at_12==1)]
mergedata4$merge_weight[which(mergedata4$Weight_at_12==0&mergedata4$Weight_at_18==1)]<-mergedata4$weight_18_months[which(mergedata4$Weight_at_12==0&mergedata4$Weight_at_18==1)]
mergedata4$merge_weight[which(mergedata4$Weight_at_12==0&mergedata4$Weight_at_18==0&mergedata4$Weight_12_m1_p5==1)]<-mergedata4$weight_12_m1_p5[which(mergedata4$Weight_at_12==0&mergedata4$Weight_at_18==0&mergedata4$Weight_12_m1_p5==1)]












kruskal.test(list(mergedata4$startdate_weight[which(mergedata4$INSTI==1)]-mergedata4$merge_weight[which(mergedata4$INSTI==1)],rep(0,length(mergedata4$merge_weight[which(mergedata4$INSTI==1)]))))
kruskal.test(list(mergedata4$startdate_weight[which(mergedata4$PI==1)], mergedata4$merge_weight[which(mergedata4$PI==1)]))
kruskal.test(list(mergedata4$startdate_weight[which(mergedata4$NNRTI==1)], mergedata4$merge_weight[which(mergedata4$NNRTI==1)]))



quantile(mergedata4$merge_weight[which(mergedata4$PI==1)]-mergedata4$startdate_weight[which(mergedata4$PI==1)],c(0.25,0.5,0.75))
quantile(mergedata4$merge_weight[which(mergedata4$INSTI==1)]-mergedata4$startdate_weight[which(mergedata4$INSTI==1)],c(0.25,0.5,0.75))
quantile(mergedata4$merge_weight[which(mergedata4$NNRTI==1)]-mergedata4$startdate_weight[which(mergedata4$NNRTI==1)],c(0.25,0.5,0.75))




quantile(mergedata4$merge_weight[which(mergedata4$INSTI==1&mergedata4$gender=="Female")]-mergedata4$startdate_weight[which(mergedata4$INSTI==1&mergedata4$gender=="Female")],c(0.25,0.5,0.75))
quantile(mergedata4$merge_weight[which(mergedata4$INSTI==1&mergedata4$gender=="Male")]-mergedata4$startdate_weight[which(mergedata4$INSTI==1&mergedata4$gender=="Male")],c(0.25,0.5,0.75))


kruskal.test(list(mergedata4$merge_weight[which(mergedata4$PI==1&mergedata4$gender=="Female")]-mergedata4$startdate_weight[which(mergedata4$PI==1&mergedata4$gender=="Female")],mergedata4$merge_weight[which(mergedata4$PI==1&mergedata4$gender=="Male")]-mergedata4$startdate_weight[which(mergedata4$PI==1&mergedata4$gender=="Male")]))










boxplot(mergedata4$merge_weight[which(mergedata4$PI==1)]-mergedata4$startdate_weight[which(mergedata4$PI==1)])
boxplot(mergedata4$merge_weight[which(mergedata4$INSTI==1)]-mergedata4$startdate_weight[which(mergedata4$INSTI==1)])
boxplot(mergedata4$merge_weight[which(mergedata4$NNRTI==1)]-mergedata4$startdate_weight[which(mergedata4$NNRTI==1)])




length(which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American"))

length(which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American"&mergedata4$INSTI==1))
length(which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American"&mergedata4$PI==1))
length(which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American"&mergedata4$NNRTI==1))




quantile(mergedata4$merge_weight[which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American")]-mergedata4$startdate_weight[which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American")],c(0.25,0.5,0.75))
quantile(mergedata4$merge_weight[which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American"&mergedata4$INSTI==1)]-mergedata4$startdate_weight[which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American"&mergedata4$INSTI==1)],c(0.25,0.5,0.75))
quantile(mergedata4$merge_weight[which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American"&mergedata4$PI==1)]-mergedata4$startdate_weight[which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American"&mergedata4$PI==1)],c(0.25,0.5,0.75))
quantile(mergedata4$merge_weight[which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American"&mergedata4$NNRTI==1)]-mergedata4$startdate_weight[which(mergedata4$gender=="Female"&mergedata4$race=="Black or African American"&mergedata4$NNRTI==1)],c(0.25,0.5,0.75))


mergedata5<-mergedata1[,c(1,56,57,58,59,60)]
mergedata6<-merge(mergedata4,mergedata5,by.x = "STUDY_ID", by.y = "STUDY_ID")
mergedata6$TAF<-rep(2,348)
mergedata6$TAF[c(2,5,24,25,26,27,28,
                 30,31,32,36,38,40,44,69,71,86,133,150,154,164,165,169,
                 173,177,181,184,188,189,196,199,200,205,207,208,210,
                 212,213,216,217,218,220,226,227,234,236,238,240,242,
                 246,247,248,251,253,255,256,257,261,262,263,265,266,
                 270,272,273,274,275,276,279,281,283,284,289,292,296,
                 297,298,300,304,307,311,312,313,314,315,320,321,324,325,
                 328,329,332,338,342,343,347)]<-1
mergedata6$TAF[c(22,29,33,34,35,37,42,43,102,137,151,250,252,267,
                 268,269,271,277,278,280,282,285,286,287,288,290,291,
                 293,294,295,299,301,302,303,305,306,308,310,316,318,
                 319,322,323,326,327,330,331,333,334,335,336,337,339,340,
                 341,344,345,346,348)]<-0




length(which(mergedata6$TAF==1))
length(which(mergedata6$TAF==0))


quantile(mergedata6$merge_weight[which(mergedata6$TAF==1&mergedata6$INSTI==1)]-mergedata6$startdate_weight[which(mergedata6$TAF==1&mergedata6$INSTI==1)],c(0.25,0.5,0.75))
quantile(mergedata6$merge_weight[which(mergedata6$TAF==0)]-mergedata6$startdate_weight[which(mergedata6$TAF==0)],c(0.25,0.5,0.75))









kruskal.test(list(mergedata4$startdate_weight[which(mergedata4$INSTI==1)]-mergedata4$merge_weight[which(mergedata4$INSTI==1)],rep(0,length(mergedata4$merge_weight[which(mergedata4$INSTI==1)]))))
kruskal.test(list(mergedata4$startdate_weight[which(mergedata4$PI==1)], mergedata4$merge_weight[which(mergedata4$PI==1)]))
kruskal.test(list(mergedata4$startdate_weight[which(mergedata4$NNRTI==1)], mergedata4$merge_weight[which(mergedata4$NNRTI==1)]))


quantile(mergedata6$start_bmi,c(0.25,0.5,0.75),na.rm = T)
quantile(mergedata6$merge_weight[which(mergedata6$INSTI==1)]-mergedata6$startdate_weight[which(mergedata6$INSTI==1)],c(0.25,0.5,0.75))
quantile(mergedata6$merge_weight[which(mergedata6$PI==1)]-mergedata6$startdate_weight[which(mergedata6$PI==1)],c(0.25,0.5,0.75))
quantile(mergedata6$merge_weight[which(mergedata6$NNRTI==1)]-mergedata6$startdate_weight[which(mergedata6$NNRTI==1)],c(0.25,0.5,0.75))


INSTI_weight_change<-mergedata6$merge_weight[which(mergedata6$INSTI==1)]-mergedata6$startdate_weight[which(mergedata6$INSTI==1)]
PI_weight_change<-mergedata6$merge_weight[which(mergedata6$PI==1)]-mergedata6$startdate_weight[which(mergedata6$PI==1)]
NNRTI_weight_change<-mergedata6$merge_weight[which(mergedata6$NNRTI==1)]-mergedata6$startdate_weight[which(mergedata6$NNRTI==1)]

NON_INSTI<-c(PI_weight_change,NNRTI_weight_change)
NON_PI<-c(INSTI_weight_change,NNRTI_weight_change)
NON_NNRTI<-c(INSTI_weight_change,PI_weight_change)



kruskal.test(list(INSTI_weight_change,PI_weight_change,NNRTI_weight_change))

wilcox.test(INSTI_weight_change,NON_INSTI)
wilcox.test(PI_weight_change,NON_PI)
wilcox.test(NNRTI_weight_change,NON_NNRTI)



Female_INSTI_change<-mergedata6$merge_weight[which(mergedata6$gender=="Female"&mergedata6$INSTI==1)]-mergedata6$startdate_weight[which(mergedata6$gender=="Female"&mergedata6$INSTI==1)]
Male_INSTI_change<-mergedata6$merge_weight[which(mergedata6$gender=="Male"&mergedata6$INSTI==1)]-mergedata6$startdate_weight[which(mergedata6$gender=="Male"&mergedata6$INSTI==1)]

quantile(mergedata6$merge_weight[which(mergedata6$gender=="Female"&mergedata6$INSTI==1)]-mergedata6$startdate_weight[which(mergedata6$gender=="Female"&mergedata6$INSTI==1)],c(0.25,0.5,0.75))

quantile(mergedata6$merge_weight[which(mergedata6$gender=="Male"&mergedata6$INSTI==1)]-mergedata6$startdate_weight[which(mergedata6$gender=="Male"&mergedata6$INSTI==1)],c(0.25,0.5,0.75))

wilcox.test(Female_INSTI_change,Male_INSTI_change)



write.csv(mergedata6,file = "C:/Users/cuisa/Desktop/INSTI/data_after_cleaning.csv")



quantile(mergedata6$age[mergedata6$NNRTI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$age[mergedata6$NNRTI==1],mergedata6$age[mergedata6$NNRTI==0])

quantile(mergedata6$age[mergedata6$PI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$age[mergedata6$PI==1],mergedata6$age[mergedata6$PI==0])


quantile(mergedata6$age[mergedata6$INSTI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$age[mergedata6$INSTI==1],mergedata6$age[mergedata6$INSTI==0])



quantile(mergedata6$age,c(0.25,0.5,0.75))

sum(1*(mergedata6$gender=="Male"))
sum(1*(mergedata6$gender=="Female"))

sum(1*(mergedata6$gender[mergedata6$NNRTI==1]=="Male"))
sum(1*(mergedata6$gender[mergedata6$NNRTI==1]=="Female"))
chisq.test(matrix(c(112,162,23,51),2,2))


sum(1*(mergedata6$gender[mergedata6$PI==1]=="Male"))
sum(1*(mergedata6$gender[mergedata6$PI==1]=="Female"))
chisq.test(matrix(c(29,245,29,45),2,2))


sum(1*(mergedata6$gender[mergedata6$INSTI==1]=="Male"))
sum(1*(mergedata6$gender[mergedata6$INSTI==1]=="Female"))
chisq.test(matrix(c(133,141,22,52),2,2))


sum(1*(mergedata6$race=="White"))
sum(1*(mergedata6$race=="Black or African American"))

sum(1*(mergedata6$race[mergedata6$NNRTI==1]=="White"))
sum(1*(mergedata6$race[mergedata6$NNRTI==1]=="Black or African American"))


sum(1*(mergedata6$race[mergedata6$PI==1]=="White"))
sum(1*(mergedata6$race[mergedata6$PI==1]=="Black or African American"))


sum(1*(mergedata6$race[mergedata6$INSTI==1]=="White"))
sum(1*(mergedata6$race[mergedata6$INSTI==1]=="Black or African American"))
chisq.test(matrix(c(19,28,116,185),2,2))
chisq.test(matrix(c(4,43,54,247),2,2))
chisq.test(matrix(c(26,21,129,172),2,2))
chisq.test(matrix(c(108,147,27,66),2,2))
chisq.test(matrix(c(48,207,10,83),2,2))
chisq.test(matrix(c(99,156,56,37),2,2))






sum(1*mergedata6$NNRTI==1)
sum(1*mergedata6$PI==1)
sum(1*mergedata6$INSTI==1)


quantile(mergedata6$CD4_FIRST[mergedata6$NNRTI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$CD4_FIRST[mergedata6$NNRTI==1],mergedata6$CD4_FIRST[mergedata6$NNRTI==0])

quantile(mergedata6$CD4_FIRST[mergedata6$PI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$CD4_FIRST[mergedata6$PI==1],mergedata6$CD4_FIRST[mergedata6$PI==0])


quantile(mergedata6$CD4_FIRST[mergedata6$INSTI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$CD4_FIRST[mergedata6$INSTI==1],mergedata6$CD4_FIRST[mergedata6$INSTI==0])



quantile(mergedata6$CD4_FIRST,c(0.25,0.5,0.75))



quantile(mergedata6$CD4_1YR[mergedata6$NNRTI==1],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(mergedata6$CD4_1YR[mergedata6$NNRTI==1],mergedata6$CD4_1YR[mergedata6$NNRTI==0])
sum(1*is.na(mergedata6$CD4_1YR[mergedata6$NNRTI==1]))


quantile(mergedata6$CD4_1YR[mergedata6$PI==1],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(mergedata6$CD4_1YR[mergedata6$PI==1],mergedata6$CD4_1YR[mergedata6$PI==0])
sum(1*is.na(mergedata6$CD4_1YR[mergedata6$PI==1]))

quantile(mergedata6$CD4_1YR[mergedata6$INSTI==1],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(mergedata6$CD4_1YR[mergedata6$INSTI==1],mergedata6$CD4_1YR[mergedata6$INSTI==0])
sum(1*is.na(mergedata6$CD4_1YR[mergedata6$INSTI==1]))


quantile(mergedata6$CD4_1YR,c(0.25,0.5,0.75),na.rm = T)
sum(1*is.na(mergedata6$CD4_1YR))




quantile(mergedata6$VL_FIRST[mergedata6$NNRTI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$VL_FIRST[mergedata6$NNRTI==1],mergedata6$VL_FIRST[mergedata6$NNRTI==0])

quantile(mergedata6$VL_FIRST[mergedata6$PI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$VL_FIRST[mergedata6$PI==1],mergedata6$VL_FIRST[mergedata6$PI==0])


quantile(mergedata6$VL_FIRST[mergedata6$INSTI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$VL_FIRST[mergedata6$INSTI==1],mergedata6$VL_FIRST[mergedata6$INSTI==0])



quantile(mergedata6$VL_FIRST,c(0.25,0.5,0.75))


quantile(mergedata6$VL_1YR[mergedata6$NNRTI==1],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(mergedata6$VL_1YR[mergedata6$NNRTI==1],mergedata6$CD4_1YR[mergedata6$NNRTI==0])
sum(1*is.na(mergedata6$VL_1YR[mergedata6$NNRTI==1]))


quantile(mergedata6$VL_1YR[mergedata6$PI==1],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(mergedata6$VL_1YR[mergedata6$PI==1],mergedata6$CD4_1YR[mergedata6$PI==0])
sum(1*is.na(mergedata6$VL_1YR[mergedata6$PI==1]))

quantile(mergedata6$VL_1YR[mergedata6$INSTI==1],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(mergedata6$VL_1YR[mergedata6$INSTI==1],mergedata6$CD4_1YR[mergedata6$INSTI==0])
sum(1*is.na(mergedata6$VL_1YR[mergedata6$INSTI==1]))


quantile(mergedata6$VL_1YR,c(0.25,0.5,0.75),na.rm = T)
sum(1*is.na(mergedata6$VL_1YR))




length(which(mergedata6$VL_1YR<200))
length(which(mergedata6$VL_1YR[mergedata6$NNRTI==1]<200))
length(which(mergedata6$VL_1YR[mergedata6$PI==1]<200))
length(which(mergedata6$VL_1YR[mergedata6$INSTI==1]<200))
chisq.test(matrix(c(100,165,15,32),2,2))
chisq.test(matrix(c(36,229,16,31),2,2))
chisq.test(matrix(c(129,136,16,31),2,2))



quantile(mergedata6$startdate_weight[mergedata6$NNRTI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$startdate_weight[mergedata6$NNRTI==1],mergedata6$startdate_weight[mergedata6$NNRTI==0])

quantile(mergedata6$startdate_weight[mergedata6$PI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$startdate_weight[mergedata6$PI==1],mergedata6$startdate_weight[mergedata6$PI==0])


quantile(mergedata6$startdate_weight[mergedata6$INSTI==1],c(0.25,0.5,0.75))
wilcox.test(mergedata6$startdate_weight[mergedata6$INSTI==1],mergedata6$startdate_weight[mergedata6$INSTI==0])



quantile(mergedata6$startdate_weight,c(0.25,0.5,0.75))



quantile(mergedata6$start_bmi[mergedata6$NNRTI==1],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(mergedata6$start_bmi[mergedata6$NNRTI==1],mergedata6$start_bmi[mergedata6$NNRTI==0])
sum(1*is.na(mergedata6$start_bmi[mergedata6$NNRTI==1]))


quantile(mergedata6$start_bmi[mergedata6$PI==1],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(mergedata6$start_bmi[mergedata6$PI==1],mergedata6$start_bmi[mergedata6$PI==0])
sum(1*is.na(mergedata6$start_bmi[mergedata6$PI==1]))


quantile(mergedata6$start_bmi[mergedata6$INSTI==1],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(mergedata6$start_bmi[mergedata6$INSTI==1],mergedata6$start_bmi[mergedata6$INSTI==0])
sum(1*is.na(mergedata6$start_bmi[mergedata6$INSTI==1]))


quantile(mergedata6$start_bmi,c(0.25,0.5,0.75),na.rm = T)
sum(1*is.na(mergedata6$start_bmi))


length(which(mergedata6$start_bmi[mergedata6$NNRTI==1]>=30))
length(which(mergedata6$start_bmi[mergedata6$PI==1]>=30))
length(which(mergedata6$start_bmi[mergedata6$INSTI==1]>=30))
chisq.test(matrix(c(20,30,115,70),2,2))
chisq.test(matrix(c(1,49,27,158),2,2))
chisq.test(matrix(c(29,21,109,76),2,2))




length(which(mergedata6$hyperlipidemia=="Yes"))
length(which(mergedata6$hyperlipidemia[mergedata6$NNRTI==1]=="Yes"))
length(which(mergedata6$hyperlipidemia[mergedata6$PI==1]=="Yes"))
length(which(mergedata6$hyperlipidemia[mergedata6$INSTI==1]=="Yes"))
chisq.test(matrix(c(15,23,120,190),2,2))
chisq.test(matrix(c(7,31,51,259),2,2))
chisq.test(matrix(c(16,22,139,171),2,2))



length(which(mergedata6$diabetes=="Yes"))
length(which(mergedata6$diabetes[mergedata6$NNRTI==1]=="Yes"))
length(which(mergedata6$diabetes[mergedata6$PI==1]=="Yes"))
length(which(mergedata6$diabetes[mergedata6$INSTI==1]=="Yes"))
chisq.test(matrix(c(8,11,127,202),2,2))
chisq.test(matrix(c(3,16,55,274),2,2))
chisq.test(matrix(c(8,11,147,182),2,2))




mergedata6$merge_diff<-mergedata6$merge_weight-mergedata6$startdate_weight
mergedata6$gain[mergedata6$merge_diff<=0]<-0
mergedata6$gain[mergedata6$merge_diff>0]<-1


mergedata6$merge_diff_5percent<-mergedata6$merge_weight-mergedata6$startdate_weight*1.05
mergedata6$gain_5percent[mergedata6$merge_diff_5percent<=0]<-0
mergedata6$gain_5percent[mergedata6$merge_diff_5percent>0]<-1


mergedata6$merge_diff_10percent<-mergedata6$merge_weight-mergedata6$startdate_weight*1.1
mergedata6$gain_10percent[mergedata6$merge_diff_10percent<=0]<-0
mergedata6$gain_10percent[mergedata6$merge_diff_10percent>0]<-1


mergedata6$merge_diff_20percent<-mergedata6$merge_weight-mergedata6$startdate_weight*1.2
mergedata6$gain_20percent[mergedata6$merge_diff_20percent<=0]<-0
mergedata6$gain_20percent[mergedata6$merge_diff_20percent>0]<-1


mergedata6$race_cate[mergedata6$race=="Black or African American"]<-1
mergedata6$race_cate[mergedata6$race!="Black or African American"]<-0

mergedata6$gender_cate[mergedata6$gender=="Female"]<-1
mergedata6$gender_cate[mergedata6$gender=="Male"]<-0


mergedata6<-mergedata6[,-c(43,45,47,49)]

model_5percent<-glm(mergedata6$gain_5percent~mergedata6$INSTI+
           mergedata6$PI+mergedata6$gender_cate+mergedata6$race_cate,
           family = binomial(link ='logit'))


model_10percent<-glm(mergedata6$gain_10percent~mergedata6$INSTI+
                            mergedata6$PI+mergedata6$gender_cate+mergedata6$race_cate,
                    family = binomial(link ='logit'))


model_20percent<-glm(mergedata6$gain_20percent~mergedata6$INSTI+
                            mergedata6$PI+mergedata6$gender_cate+mergedata6$race_cate,
                    family = binomial(link ='logit'))



summary(model_5percent)
summary(model_10percent)
summary(model_20percent)


mergedata6$TAF_INSTI<-rep(0,length(mergedata6$TAF))
mergedata6$TAF_INSTI[mergedata6$TAF==1]<-1

mergedata6$NONTAF_INSTI<-rep(0,length(mergedata6$TAF))
mergedata6$NONTAF_INSTI[mergedata6$TAF==0]<-1


model_5percent_TAF<-glm(mergedata6$gain_5percent~mergedata6$TAF_INSTI+mergedata6$NONTAF_INST+
                            mergedata6$PI+mergedata6$gender_cate+mergedata6$race_cate,
                    family = binomial(link ='logit'))


model_10percent_TAF<-glm(mergedata6$gain_10percent~mergedata6$TAF_INSTI+mergedata6$NONTAF_INST+
                             mergedata6$PI+mergedata6$gender_cate+mergedata6$race_cate,
                     family = binomial(link ='logit'))


model_20percent_TAF<-glm(mergedata6$gain_20percent~mergedata6$TAF_INSTI+mergedata6$NONTAF_INST+
                             mergedata6$PI+mergedata6$gender_cate+mergedata6$race_cate,
                     family = binomial(link ='logit'))


summary(model_5percent_TAF)
summary(model_10percent_TAF)
summary(model_20percent_TAF)

INSTI<-which(mergedata6$INSTI==1)

model_5percent_TAF1<-glm(mergedata6$gain_5percent[INSTI]~mergedata6$TAF[INSTI]+
                                mergedata6$gender_cate[INSTI]+mergedata6$race_cate[INSTI],
                        family = binomial(link ='logit'))


summary(model_5percent_TAF1)


as.factor(mergedata6$TAF_INSTI)

colnames(mergedata6)[30:32]<-c("start_bmi","bmi_12_months","bmi_18_months")
colnames(mergedata6)


mergedata6$merge_bmi<-rep(NA,length(mergedata6[,1]))


mergedata6$merge_bmi[!is.na(mergedata6$bmi_12_months)]=mergedata6$bmi_12_months[!is.na(mergedata6$bmi_12_months)]
mergedata6$merge_bmi[is.na(mergedata6$bmi_12_months)]=mergedata6$bmi_18_months[is.na(mergedata6$bmi_12_months)]

quantile(mergedata6$merge_weight,c(0.25,0.5,0.75))
quantile(mergedata6$merge_weight[which(mergedata6$INSTI==1)],c(0.25,0.5,0.75))
quantile(mergedata6$merge_weight[which(mergedata6$PI==1)],c(0.25,0.5,0.75))
quantile(mergedata6$merge_weight[which(mergedata6$NNRTI==1)],c(0.25,0.5,0.75))
wilcox.test(mergedata6$merge_weight[mergedata6$INSTI==1],mergedata6$merge_weight[mergedata6$INSTI==0])
wilcox.test(mergedata6$merge_weight[mergedata6$PI==1],mergedata6$merge_weight[mergedata6$PI==0])
wilcox.test(mergedata6$merge_weight[mergedata6$NNRTI==1],mergedata6$merge_weight[mergedata6$NNRTI==0])

sum(1*!is.na(mergedata6$merge_bmi))
quantile(mergedata6$merge_bmi,c(0.25,0.5,0.75),na.rm = T)
quantile(mergedata6$merge_bmi[which(mergedata6$NNRTI==1)],c(0.25,0.5,0.75),na.rm = T)
quantile(mergedata6$merge_bmi[which(mergedata6$PI==1)],c(0.25,0.5,0.75),na.rm = T)
quantile(mergedata6$merge_bmi[which(mergedata6$INSTI==1)],c(0.25,0.5,0.75),na.rm = T)
sum(1*!is.na(mergedata6$merge_bmi[which(mergedata6$NNRTI==1)]))
sum(1*!is.na(mergedata6$merge_bmi[which(mergedata6$PI==1)]))
sum(1*!is.na(mergedata6$merge_bmi[which(mergedata6$INSTI==1)]))
wilcox.test(mergedata6$merge_bmi[mergedata6$NNRTI==1],mergedata6$merge_bmi[mergedata6$NNRTI==0])
wilcox.test(mergedata6$merge_bmi[mergedata6$PI==1],mergedata6$merge_bmi[mergedata6$PI==0])
wilcox.test(mergedata6$merge_bmi[mergedata6$INSTI==1],mergedata6$merge_bmi[mergedata6$INSTI==0])


length(which(mergedata6$merge_bmi>=30))
length(which(mergedata6$merge_bmi>=30 & mergedata6$NNRTI == 1))
length(which(mergedata6$merge_bmi>=30 & mergedata6$PI == 1))
length(which(mergedata6$merge_bmi>=30 & mergedata6$INSTI == 1))


chisq.test(matrix(c(26,63,39,106),2))
chisq.test(matrix(c(5,28,60,141),2))
chisq.test(matrix(c(34,78,31,91),2))


mergedata6$age_cate=rep(NA,length(mergedata6[,1]))
mergedata6$age_cate[which(mergedata6$age<50)]=1
mergedata6$age_cate[which(mergedata6$age>=50)]=0

mergedata6$bmi_cate=rep(NA,length(mergedata6[,1]))
mergedata6$bmi_cate[which(mergedata6$start_bmi>=30)]=1
mergedata6$bmi_cate[which(mergedata6$start_bmi<30)]=0

mergedata6$cd4_cate=rep(NA,length(mergedata6[,1]))
mergedata6$cd4_cate[which(mergedata6$CD4_FIRST<200)]=1
mergedata6$cd4_cate[which(mergedata6$CD4_FIRST>=200)]=0



new_data=data.frame(mergedata6$ART1,mergedata6$ART2,mergedata6$ART3,mergedata6$ART4)
mergedata6$ART1=as.character(mergedata6$ART1)
mergedata6$ART2=as.character(mergedata6$ART2)
mergedata6$ART3=as.character(mergedata6$ART3)
mergedata6$ART4=as.character(mergedata6$ART4)
length(which(mergedata6$ART1=="Truvada" | mergedata6$ART2=="Truvada" | mergedata6$ART3 == "Truvada" | mergedata6$ART4 == "Truvada"))
TDF=which(mergedata6$ART1=="Truvada" | mergedata6$ART2=="Truvada" | mergedata6$ART3 == "Truvada" | mergedata6$ART4 == "Truvada")

mergedata6$TDF_cate=rep(0,length(mergedata6[,1]))
mergedata6$TAF_cate=rep(0,length(mergedata6[,1]))
mergedata6$TDF_cate[TDF]=1

length(which(mergedata6$ART1=="Descovy" | mergedata6$ART2=="Descovy" | mergedata6$ART3 == "Descovy" | mergedata6$ART4 == "Descovy"))

model_1012<-glm(mergedata6$gain_5percent~mergedata6$INSTI+
                        mergedata6$gender_cate+mergedata6$race_cate+mergedata6$age_cate+
                        mergedata6$cd4_cate+mergedata6$TDF_cate,
                         family = binomial(link ='logit'))

summary(model_1012)

mergedata6$regimen=rep(NA,length(mergedata6[,1]))
mergedata6$regimen[mergedata6$INSTI==1]="INSTI"
mergedata6$regimen[mergedata6$PI==1]="PI"
mergedata6$regimen[mergedata6$NNRTI==1]="NNRTI"
mergedata6$regimen=as.factor(mergedata6$regimen)
mergedata6$merge_diff= mergedata6$merge_weight-mergedata6$startdate_weight

ggplot(mergedata6, aes(x=regimen, y=merge_diff, fill=regimen)) + geom_boxplot()+
        stat_summary(fun=mean, geom="point", shape=5, size=3)+xlab("Regimen groups ")+ylab("Weight change  / lbs")+
        geom_text(aes(y =c(130), x = c(1), label =c("Median: 4.8 (-2.3, 13.1)")), size = 3.5)+
        geom_text(aes(y =c(120), x = c(1), label =c("N = 155")), size = 3.2)+
        geom_text(aes(y =c(110), x = c(1), label =c("p = 0.11  ")), size = 3.5)+
        geom_text(aes(y =c(130), x = c(2), label =c("Median: 0.9 (-7.2, 8.8)")), size = 3.5)+
        geom_text(aes(y =c(120), x = c(2), label =c("N = 135")), size = 3.2)+
        geom_text(aes(y =c(110), x = c(2), label =c("p < 0.01 ** ")), size = 3.5)+
        geom_text(aes(y =c(130), x = c(3), label =c("Median: 6.8 (-2.4, 18.9)")), size = 3.5)+
        geom_text(aes(y =c(120), x = c(3), label =c("N = 58")), size = 3.2)+
        geom_text(aes(y =c(110), x = c(3), label =c("p = 0.036 * ")), size = 3.5)+       
        ggtitle("Weight change by regimen")+theme(axis.title = element_text( face = "bold",size=12),plot.title = element_text(hjust = 0.5))

quantile(mergedata6$merge_diff[which(mergedata6$INSTI==1)],c(0.25,0.5,0.75))
mean(mergedata6$merge_diff[which(mergedata6$INSTI==1)])
sd(mergedata6$merge_diff[which(mergedata6$INSTI==1)])
quantile(mergedata6$merge_diff[which(mergedata6$NNRTI==1)],c(0.25,0.5,0.75))
mean(mergedata6$merge_diff[which(mergedata6$NNRTI==1)])
sd(mergedata6$merge_diff[which(mergedata6$NNRTI==1)])
quantile(mergedata6$merge_diff[which(mergedata6$PI==1)],c(0.25,0.5,0.75))
mean(mergedata6$merge_diff[which(mergedata6$PI==1)])
sd(mergedata6$merge_diff[which(mergedata6$PI==1)])
length(which(mergedata6$INSTI==1))
length(which(mergedata6$NNRTI==1))
length(which(mergedata6$PI==1))

wilcox.test(mergedata6$merge_diff[mergedata6$INSTI==1],mergedata6$merge_diff[mergedata6$INSTI==0])
wilcox.test(mergedata6$merge_diff[mergedata6$NNRTI==1],mergedata6$merge_diff[mergedata6$NNRTI==0])
wilcox.test(mergedata6$merge_diff[mergedata6$PI==1],mergedata6$merge_diff[mergedata6$PI==0])




mergedata6$new_race_cate=rep("Others",length(mergedata6[,1]))
mergedata6$new_race_cate[mergedata6$race=="Black or African American"]="Black or African American"
mergedata6$new_race_cate[mergedata6$race=="White"]="White"

mergedata6$new_race_cate=as.factor(mergedata6$new_race_cate)


ggplot(mergedata6, aes(x=new_race_cate, y=merge_diff, fill=new_race_cate)) + geom_boxplot()+
        stat_summary(fun=mean, geom="point", shape=5, size=3)+xlab("Race groups ")+ylab("Weight change  / lbs")+
        geom_text(aes(y =c(130), x = c(1), label =c("Median: 3.1 (-3.1, 13.8)")), size = 3.5)+
        geom_text(aes(y =c(120), x = c(1), label =c("N = 255")), size = 3.2)+
        geom_text(aes(y =c(110), x = c(1), label =c("p = 0.32")), size = 3.2)+        
        geom_text(aes(y =c(130), x = c(2), label =c("Median: 3.2 (-5.6, 13.4)")), size = 3.5)+
        geom_text(aes(y =c(120), x = c(2), label =c("N = 49")), size = 3.2)+
        geom_text(aes(y =c(110), x = c(2), label =c("p = 0.49")), size = 3.2)+         
        geom_text(aes(y =c(130), x = c(3), label =c("Median: 4.2 (-3.9, 8.6)")), size = 3.2)+
        geom_text(aes(y =c(120), x = c(3), label =c("N = 44")), size = 3.2)+        
        geom_text(aes(y =c(110), x = c(3), label =c("p = 0.54")), size = 3.2)+         
        ggtitle("Weight change by race")+theme(axis.title = element_text( face = "bold",size=12),plot.title = element_text(hjust = 0.5))

quantile(mergedata6$merge_diff[which(mergedata6$race=="Black or African American")],c(0.25,0.5,0.75))
mean(mergedata6$merge_diff[which(mergedata6$race=="Black or African American")])
sd(mergedata6$merge_diff[which(mergedata6$race=="Black or African American")])
quantile(mergedata6$merge_diff[which(mergedata6$race=="White")],c(0.25,0.5,0.75))
mean(mergedata6$merge_diff[which(mergedata6$race=="White")])
sd(mergedata6$merge_diff[which(mergedata6$race=="White")])
length(which(mergedata6$race=="Black or African American"))
length(which(mergedata6$race=="White"))
quantile(mergedata6$merge_diff[which(mergedata6$new_race_cate=="Others")],c(0.25,0.5,0.75))

wilcox.test(mergedata6$merge_diff[mergedata6$new_race_cate=="Black or African American"],
            mergedata6$merge_diff[mergedata6$new_race_cate!="Black or African American"])
wilcox.test(mergedata6$merge_diff[mergedata6$new_race_cate=="White"],
            mergedata6$merge_diff[mergedata6$new_race_cate!="White"])
wilcox.test(mergedata6$merge_diff[mergedata6$new_race_cate=="Others"],
            mergedata6$merge_diff[mergedata6$new_race_cate!="Others"])








class(mergedata6$gender)






ggplot(mergedata6, aes(x=gender, y=merge_diff, fill=gender)) + geom_boxplot()+
        stat_summary(fun=mean, geom="point", shape=5, size=3)+xlab("Gender groups ")+ylab("Weight change  / lbs")+
        geom_text(aes(y =c(130), x = c(1), label =c("Median: 8.0 (-1.6, 23.0)")), size = 3.5)+
        geom_text(aes(y =c(120), x = c(1), label =c("N = 74")), size = 3.2)+
        geom_text(aes(y =c(130), x = c(2), label =c("Median: 3.0 (-4.0, 11.0)")), size = 3.5)+
        geom_text(aes(y =c(120), x = c(2), label =c("N = 274")), size = 3.2)+
        geom_text(aes(y =c(-60), x = c(1.5), label =c("p < 0.01 **")), size = 3.2)+
        ggtitle("Weight change by gender")+theme(axis.title = element_text( face = "bold",size=12),plot.title = element_text(hjust = 0.5))

quantile(mergedata6$merge_diff[which(mergedata6$gender=="Female")],c(0.25,0.5,0.75))
mean(mergedata6$merge_diff[which(mergedata6$gender=="Female")])
sd(mergedata6$merge_diff[which(mergedata6$gender=="Female")])
quantile(mergedata6$merge_diff[which(mergedata6$gender=="Male")],c(0.25,0.5,0.75))
mean(mergedata6$merge_diff[which(mergedata6$gender=="Male")])
sd(mergedata6$merge_diff[which(mergedata6$gender=="Male")])
length(which(mergedata6$gender=="Female"))
length(which(mergedata6$gender=="Male"))


mergedata6$gr=rep(NA,length(mergedata6[,1]))
mergedata6$gr[which(mergedata6$race=="Black or African American"& mergedata6$gender == "Female")]="Black Female"
mergedata6$gr[which(mergedata6$race=="Black or African American"& mergedata6$gender == "Male")]="Black Male"
mergedata6$gr[which(mergedata6$race=="White"& mergedata6$gender == "Female")]="White Female"
mergedata6$gr[which(mergedata6$race=="White"& mergedata6$gender == "Male")]="White Male"

mergedata6$gr=as.factor(mergedata6$gr)
compare_gender_race=mergedata6[!is.na(mergedata6$gr),]

ggplot(compare_gender_race, aes(x=gr, y=merge_diff, fill=gr)) + geom_boxplot()+
        stat_summary(fun=mean, geom="point", shape=5, size=3)+xlab("Race and Gender groups ")+ylab("Weight change  / lbs")+
        geom_text(aes(y =c(130), x = c(1), label =c("Median: 8.8 (-1.7, 26.6)")), size = 3.2)+
        geom_text(aes(y =c(120), x = c(1), label =c("N = 64")), size = 3.2)+
        geom_text(aes(y =c(110), x = c(1), label =c("p = 0.01 *")), size = 3.2)+
        geom_text(aes(y =c(100), x = c(2), label =c("Median: 2.9 (-3.7, 11.7)")), size = 3.2)+
        geom_text(aes(y =c(90), x = c(2), label =c("N = 191")), size = 3.2)+
        geom_text(aes(y =c(80), x = c(2), label =c("p = 0.11")), size = 3.2)+        
        geom_text(aes(y =c(130), x = c(3), label =c("Median: 21.0 (-7.7, 28.8)")), size = 3.2)+
        geom_text(aes(y =c(120), x = c(3), label =c("N = 3")), size = 3.2)+
        geom_text(aes(y =c(110), x = c(3), label =c("p = 0.31")), size = 3.2)+       
        geom_text(aes(y =c(115), x = c(4), label =c("Median: 3.1 (-6.1, 8.3)")), size = 3.2)+
        geom_text(aes(y =c(105), x = c(4), label =c("N = 46")), size = 3.2)+
        geom_text(aes(y =c(95), x = c(4), label =c("p = 0.31")), size = 3.2)+        

        ggtitle("Weight change by gender and race")+theme(axis.title = element_text( face = "bold",size=12),plot.title = element_text(hjust = 0.5))


quantile(compare_gender_race$merge_diff[which(compare_gender_race$gr=="Black Female")],c(0.25,0.5,0.75))
mean(compare_gender_race$merge_diff[which(compare_gender_race$gr=="Black Female")])
sd(compare_gender_race$merge_diff[which(compare_gender_race$gr=="Black Female")])
quantile(compare_gender_race$merge_diff[which(compare_gender_race$gr=="Black Male")],c(0.25,0.5,0.75))
mean(compare_gender_race$merge_diff[which(compare_gender_race$gr=="Black Male")])
sd(compare_gender_race$merge_diff[which(compare_gender_race$gr=="Black Male")])
quantile(compare_gender_race$merge_diff[which(compare_gender_race$gr=="White Female")],c(0.25,0.5,0.75))
mean(compare_gender_race$merge_diff[which(compare_gender_race$gr=="White Female")])
sd(compare_gender_race$merge_diff[which(compare_gender_race$gr=="White Female")])
quantile(compare_gender_race$merge_diff[which(compare_gender_race$gr=="White Male")],c(0.25,0.5,0.75))
mean(compare_gender_race$merge_diff[which(compare_gender_race$gr=="White Male")])
sd(compare_gender_race$merge_diff[which(compare_gender_race$gr=="White Male")])

length(which(compare_gender_race$gr=="Black Female"))
length(which(compare_gender_race$gr=="Black Male"))
length(which(compare_gender_race$gr=="White Female"))
length(which(compare_gender_race$gr=="White Male"))


wilcox.test(compare_gender_race$merge_diff[which(compare_gender_race$gr=="Black Female")],
                 compare_gender_race$merge_diff[which(compare_gender_race$gr!="Black Female")])

wilcox.test(compare_gender_race$merge_diff[which(compare_gender_race$gr=="Black Male")],
            compare_gender_race$merge_diff[which(compare_gender_race$gr!="Black Male")])

wilcox.test(compare_gender_race$merge_diff[which(compare_gender_race$gr=="White Female")],
            compare_gender_race$merge_diff[which(compare_gender_race$gr!="White Female")])

wilcox.test(compare_gender_race$merge_diff[which(compare_gender_race$gr=="White Male")],
            compare_gender_race$merge_diff[which(compare_gender_race$gr!="White Male")])








women = rbind(mergedata6[mergedata6$race=="Black or African American"&mergedata6$gender=="Female",],
              mergedata6[mergedata6$race!="Black or African American"&mergedata6$gender=="Female",])


women$race_gender=rep("Other women", 74)
women$race_gender[1:64]=rep("Black or African American women", 64)

women$race_gender=as.factor(women$race_gender)



ggplot(women, aes(x=race_gender, y=merge_diff, fill=race_gender)) + geom_boxplot()+
        stat_summary(fun=mean, geom="point", shape=5, size=3)+xlab("Race and Gender groups ")+ylab("Weight change  / lbs")+
        geom_text(aes(y =c(130), x = c(1), label =c("Median: 8.8 (-1.7, 26.6)")), size = 3.2)+
        geom_text(aes(y =c(120), x = c(1), label =c("N = 64")), size = 3.2)+
        geom_text(aes(y =c(130), x = c(2), label =c("Median: 5.7 (2.3, 13.9) ")), size = 3.2)+
        geom_text(aes(y =c(120), x = c(2), label =c("N = 10")), size = 3.2)+
        geom_text(aes(y =c(-50), x = c(1.5), label =c("p = 0.73 ")), size = 3.2)+  
        ggtitle("Weight change by gender and race")+theme(axis.title = element_text( face = "bold",size=12),plot.title = element_text(hjust = 0.5))



quantile(women$merge_diff[women$race_gender=="Black or African American women"],c(0.25,0.5,0.75))

quantile(women$merge_diff[women$race_gender=="Other women"],c(0.25,0.5,0.75))


wilcox.test(women$merge_diff[women$race_gender=="Black or African American women"],
            women$merge_diff[women$race_gender=="Other women"])



black_female = mergedata6[mergedata6$gender=="Female"&mergedata6$race=="Black or African American",]

ggplot(black_female, aes(x=regimen, y=merge_diff, fill=regimen)) + geom_boxplot()+
        stat_summary(fun=mean, geom="point", shape=5, size=3)+xlab("Regimen groups ")+ylab("Weight change  / lbs")+
        geom_text(aes(y =c(130), x = c(1), label =c("Median: 10.2 (-0.5, 34.7)")), size = 3.5)+
        geom_text(aes(y =c(120), x = c(1), label =c("N = 18")), size = 3.2)+
        geom_text(aes(y =c(110), x = c(1), label =c("p = 0.54  ")), size = 3.5)+
        geom_text(aes(y =c(130), x = c(2), label =c("Median: 3.6 (-7.1, 17.1)")), size = 3.5)+
        geom_text(aes(y =c(120), x = c(2), label =c("N = 20")), size = 3.2)+
        geom_text(aes(y =c(110), x = c(2), label =c("p = 0.16 ")), size = 3.5)+
        geom_text(aes(y =c(130), x = c(3), label =c("Median: 11.2 (0, 31.4)")), size = 3.5)+
        geom_text(aes(y =c(120), x = c(3), label =c("N = 26")), size = 3.2)+
        geom_text(aes(y =c(110), x = c(3), label =c("p = 0.45 ")), size = 3.5)+       
        ggtitle("Weight change by regimen in black women")+theme(axis.title = element_text( face = "bold",size=12),plot.title = element_text(hjust = 0.5))



length(which(black_female$INSTI==1))
length(which(black_female$NNRTI==1))
length(which(black_female$PI==1))
quantile(black_female$merge_diff[black_female$INSTI==1],c(0.25,0.5,0.75))
quantile(black_female$merge_diff[black_female$NNRTI==1],c(0.25,0.5,0.75))
quantile(black_female$merge_diff[black_female$PI==1],c(0.25,0.5,0.75))


wilcox.test(black_female$merge_diff[black_female$INSTI==1],black_female$merge_diff[black_female$INSTI==0])
wilcox.test(black_female$merge_diff[black_female$PI==1],black_female$merge_diff[black_female$PI==0])
wilcox.test(black_female$merge_diff[black_female$NNRTI==1],black_female$merge_diff[black_female$NNRTI==0])




black_male = mergedata6[mergedata6$gender=="Male"&mergedata6$race=="Black or African American",]

ggplot(black_male, aes(x=regimen, y=merge_diff, fill=regimen)) + geom_boxplot()+
        stat_summary(fun=mean, geom="point", shape=5, size=3)+xlab("Regimen groups ")+ylab("Weight change  / lbs")+
        geom_text(aes(y =c(70), x = c(1), label =c("Median: 3.1 (-1.1, 12.6)")), size = 3.5)+
        geom_text(aes(y =c(65), x = c(1), label =c("N = 81")), size = 3.2)+
        geom_text(aes(y =c(60), x = c(1), label =c("p = 0.06  ")), size = 3.5)+
        geom_text(aes(y =c(70), x = c(2), label =c("Median: 0.3 (-5.8, 7.8)")), size = 3.5)+
        geom_text(aes(y =c(65), x = c(2), label =c("N = 88")), size = 3.2)+
        geom_text(aes(y =c(60), x = c(2), label =c("p = 0.28 ")), size = 3.5)+
        geom_text(aes(y =c(70), x = c(3), label =c("Median: 6.7 (-3.8, 16.4)")), size = 3.5)+
        geom_text(aes(y =c(65), x = c(3), label =c("N = 22")), size = 3.2)+
        geom_text(aes(y =c(60), x = c(3), label =c("p = 0.01* ")), size = 3.5)+       
        ggtitle("Weight change by regimen in black men")+theme(axis.title = element_text( face = "bold",size=12),plot.title = element_text(hjust = 0.5))

length(which(black_male$INSTI==1))
length(which(black_male$NNRTI==1))
length(which(black_male$PI==1))
quantile(black_male$merge_diff[black_male$INSTI==1],c(0.25,0.5,0.75))
quantile(black_male$merge_diff[black_male$NNRTI==1],c(0.25,0.5,0.75))
quantile(black_male$merge_diff[black_male$PI==1],c(0.25,0.5,0.75))


wilcox.test(black_male$merge_diff[black_male$INSTI==1],black_male$merge_diff[black_male$INSTI==0])
wilcox.test(black_male$merge_diff[black_male$PI==1],black_male$merge_diff[black_male$PI==0])
wilcox.test(black_male$merge_diff[black_male$NNRTI==1],black_male$merge_diff[black_male$NNRTI==0])





white_male = mergedata6[mergedata6$gender=="Male"&mergedata6$race=="White",]

ggplot(white_male, aes(x=regimen, y=merge_diff, fill=regimen)) + geom_boxplot()+
        stat_summary(fun=mean, geom="point", shape=5, size=3)+xlab("Regimen groups ")+ylab("Weight change  / lbs")+
        geom_text(aes(y =c(70), x = c(1), label =c("Median: 5.0 (-3.2, 15.2)")), size = 3.5)+
        geom_text(aes(y =c(65), x = c(1), label =c("N = 25")), size = 3.2)+
        geom_text(aes(y =c(60), x = c(1), label =c("p = 0.23  ")), size = 3.5)+
        geom_text(aes(y =c(70), x = c(2), label =c("Median: 1.2 (-10.7, 5.2)")), size = 3.5)+
        geom_text(aes(y =c(65), x = c(2), label =c("N = 18")), size = 3.2)+
        geom_text(aes(y =c(60), x = c(2), label =c("p = 0.35 ")), size = 3.5)+
        geom_text(aes(y =c(70), x = c(3), label =c("Median: 7.0 (3.5, 13.1)")), size = 3.5)+
        geom_text(aes(y =c(65), x = c(3), label =c("N = 3")), size = 3.2)+
        geom_text(aes(y =c(60), x = c(3), label =c("p = 0.09 ")), size = 3.5)+       
        ggtitle("Weight change by regimen in white men")+theme(axis.title = element_text( face = "bold",size=12),plot.title = element_text(hjust = 0.5))

length(which(white_male$INSTI==1))
length(which(white_male$NNRTI==1))
length(which(white_male$PI==1))
quantile(white_male$merge_diff[white_male$INSTI==1],c(0.25,0.5,0.75))
quantile(white_male$merge_diff[white_male$NNRTI==1],c(0.25,0.5,0.75))
quantile(white_male$merge_diff[white_male$PI==1],c(0.25,0.5,0.75))


wilcox.test(white_male$merge_diff[white_male$INSTI==1],white_male$merge_diff[white_male$INSTI==0])
wilcox.test(white_male$merge_diff[white_male$PI==1],white_male$merge_diff[white_male$PI==0])
wilcox.test(white_male$merge_diff[white_male$NNRTI==1],white_male$merge_diff[white_male$NNRTI==0])


model_1015_INSTI<-glm(mergedata6$gain_5percent~mergedata6$INSTI+
                           mergedata6$gender_cate+mergedata6$race_cate+mergedata6$age_cate+
                           mergedata6$cd4_cate+mergedata6$TDF_cate,
                   family = binomial(link ='logit'))

summary(model_1015_INSTI)





model_1015_PI<-glm(mergedata6$gain_5percent~mergedata6$PI+
                        mergedata6$gender_cate+mergedata6$race_cate+mergedata6$age_cate+
                        mergedata6$cd4_cate+mergedata6$TDF_cate,
                family = binomial(link ='logit'))

summary(model_1015_PI)


model_1015_NNRTI<-glm(mergedata6$gain_5percent~mergedata6$NNRTI+
                           mergedata6$gender_cate+mergedata6$race_cate+mergedata6$age_cate+
                           mergedata6$cd4_cate+mergedata6$TDF_cate,
                   family = binomial(link ='logit'))

summary(model_1015_NNRTI)


#install.packages("rJava")
#install.packages("ReporteRs")
#install.packages("Gmisc")
library(Gmisc)
#Sys.setenv("JAVA_HOME"="")
#library(ReporteRs)

#Load code and libraries
source("/Users/jrigdon/Box Sync/Rigdon/Useful Functions/Tables.R")
source("/Users/jrigdon/Box Sync/Rigdon/Useful Functions/Figures.R")

#Diet analysis
#Load data
all9 = read.csv("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Diet_2017-04-11.csv",header=TRUE)

#Blind diets
table(all9$diet)
all9$diet = as.character(all9$diet)
all9$diet[all9$diet=="Blue"] = "Diet1"
all9$diet[all9$diet=="Purple"] = "Diet2"
table(all9$diet)
all9$id = paste(all9$study_id, all9$redcap_event_name, sep="")

#Get RQ data in there too
rq = read.csv("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/REE_2017-06-08.csv",header=TRUE)
#keep = dta2[dta2$redcap_event_name=="screening_arm_1" & dta2$check_609___1==1,names(dta2)%in%c("study_id","check_609___1")]
keepID = bl_data$bl_id
rq2 = rq[rq$study_id %in% keepID, names(rq) %in% c("study_id","redcap_event_name","ree_rq_ave", "ree_ree_ave")] #subset
rq2$id = paste(rq2$study_id, rq2$redcap_event_name, sep="")
rq3 = rq2[, names(rq2) %in% c("id","ree_rq_ave", "ree_ree_ave")]

all10 = merge(all9, rq3, by="id", all=TRUE)

#Add in physical activity variable
pa = read.csv("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/PA_Data_2017-11-21.csv",header=TRUE)
#keep = dta2[dta2$redcap_event_name=="screening_arm_1" & dta2$check_609___1==1,names(dta2)%in%c("study_id","check_609___1")]
pa2 = pa[pa$study_id %in% keepID, names(pa) %in% c("study_id","redcap_event_name","par_kcal_kg_day")] #subset

pa2$id = paste(pa2$study_id, pa2$redcap_event_name, sep="")
pa3 = pa2[, names(pa2) %in% c("id","par_kcal_kg_day")]

all11 = merge(all10, pa3, by="id", all=TRUE)


##Save all11 (first set of outcomes)
write.csv(all11, "/Users/jrigdon/Box sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Outcomes_1.csv", row.names=FALSE)

##Get all other outcomes in there
d1 = read.csv("/Users/jrigdon/Box sync/Rigdon/Gardner/JAMA_submission/Revision/Data/DATA_2017-08-16.csv", header=TRUE)

#Keep only variables we need for people we need
keep = c("study_id", "redcap_event_name", "lipid_ldl_v2", "lipid_hdl_v2", "lipid_trig_v2",  "baseline_gluc",  "baseline_gluc2",  "baseline_gluc3", "baseline_ins", "dxa_percentfat", "sys_bp1_gcrc", "sys_bp2_gcrc", "sys_bp3_gcrc", "sys_bp4_gcrc", "sys_bp5_gcrc", "sys_bp6_gcrc", "sys_bp7_gcrc", "sys_bp8_gcrc", "diast_bp1_gcrc", "diast_bp2_gcrc", "diast_bp3_gcrc", "diast_bp4_gcrc", "diast_bp5_gcrc", "diast_bp6_gcrc", "diast_bp7_gcrc", "diast_bp8_gcrc", "waist_gcrc", "scr_gender", "weight_gcrc")

diet = read.csv("/Users/jrigdon/Box sync/Rigdon/Gardner/QSU_Shared/Nov16_presentation/zip_609.csv",header=TRUE)[, c(1,3)]
names(diet)[1] = "study_id"

IDs = diet$study_id

d2 = d1[d1$study_id %in% IDs, names(d1) %in% keep]

#Define variables
#SBP and DPB
avv = function(x) {
    avv = double()
    end = length(x)
    if (is.na(x[1])) {avv = NA}
    else if (!is.na(x[1]) & sum(is.na(x[2:end]))==(end-1)) {avv = x[1]}
    else if (!is.na(x[1]) & sum(is.na(x[2:end]))<(end-1)) {avv = mean(x[2:end],na.rm=TRUE)}
avv
}

#m = rbind(c(1,NA,NA),c(1,2,NA),c(1,2,3))
#apply(m,1,avv)


d2$sbp = apply(d2[, names(d2) %in% c("sys_bp1_gcrc", "sys_bp2_gcrc", "sys_bp3_gcrc", "sys_bp4_gcrc", "sys_bp5_gcrc", "sys_bp6_gcrc", "sys_bp7_gcrc", "sys_bp8_gcrc")], 1, avv)

d2$dbp = apply(d2[, names(d2) %in% c("diast_bp1_gcrc", "diast_bp2_gcrc", "diast_bp3_gcrc", "diast_bp4_gcrc", "diast_bp5_gcrc", "diast_bp6_gcrc", "diast_bp7_gcrc", "diast_bp8_gcrc")], 1, avv)

#Average whatever we have for glucose
d2$gluc = apply(d2[, names(d2) %in% c("baseline_gluc", "baseline_gluc2", "baseline_gluc3")], 1, function(x) mean(x,na.rm=TRUE))

#Only keep what we need
keep2 = c("study_id", "redcap_event_name", "scr_gender", "waist_gcrc", "sbp", "dbp", "gluc", "baseline_ins", "lipid_trig_v2", "lipid_ldl_v2", "lipid_hdl_v2", "dxa_percentfat", "weight_gcrc")

d3 = d2[, names(d2) %in% keep2]

#Merge screening gender and diet
scr = d3[d3$redcap_event_name=="screening_arm_1", names(d3) %in% c("study_id", "scr_gender")]

d4 = merge(d3[, -3], scr, by="study_id", all.x=TRUE)
names(diet) = c("study_id", "diet")
d5 = merge(d4, diet, by="study_id", all.x=TRUE)

d6 = d5[d5$redcap_event_name!="screening_arm_1", ]

##Look at body fat numbers
names(d6)
d6$fatNA = ifelse(is.na(d6$dxa_percentfat), 1, 0)
table(d6$redcap_event_name, d6$diet, d6$fatNA, exclude=NULL)

##Define metabolic syndrome##
#1. Waist circumference (waist_gcrc)
#>40 inches for men (40 x 2.54 = 101.6 cm)
#>35 inches for women (35 x 2.54 = 87.9 cm)
#If we don’t have waist circumference data cleaned, for now we can use BMI >30
d6$crit1 = ifelse((d6$scr_gender == 1 & d6$waist_gcrc > 2.54 * 40) | (d6$scr_gender==2 & d6$waist_gcrc>2.54*35), 1, 0)

# 2. Blood pressure
#SPB > 130 mmHg (diast_bp1_gcrc through diast_bp8_gcrc)
#or
#DBP > 85 mmHg (sys_bp1_gcrc through sys_bp8_gcrc)
#(if we don’t have these data cleaned up, never mind…we can’t do this with any alternates)
d6$crit2 = ifelse((d6$sbp > 130) | (d6$dbp > 85), 1, 0)

# 3. Fasting glucose > 100 mg/dL (baseline_gluc, baseline_gluc2, baseline_gluc3)
d6$crit3 = ifelse(d6$gluc > 100, 1, 0)

# 4. Fasting triglyceride > 150 mg/dL (lipid_trig_v2 for C1, C3, C4, C5; lipid_trig for C2)
d6$crit4 = ifelse(d6$lipid_trig_v2 > 150 , 1 , 0) #150 not 50

# 5. HDL-cholesterol < 40 mg/dL for men | < 50 for women (and yes, that is just “less than”, and not “less than or equal to”, even though all of the others are “…or equal to”) (lipid_hdl_v2 for C1, C3, C4, C5; lipid_hdl for C2)
d6$crit5 = ifelse((d6$scr_gender == 1 & d6$lipid_hdl_v2 < 40) | (d6$scr_gender == 2 & d6$lipid_hdl_v2 < 50), 1, 0)

d6$MetSyn = as.numeric(apply(d6[, names(d6) %in% c("crit1", "crit2", "crit3", "crit4", "crit5")] , 1 , function(x) if (sum(is.na(x))==5) NA else sum(x[!is.na(x)]) >=3))

table(d6$redcap_event_name , d6$MetSyn , d6$diet , exclude = NULL)


#BMI
#Height (scr and bl) [Look at Dataprep.R for e4]
height_bl = e4$ht.bl
sum(is.na(height_bl))
height_bl[is.na(height_bl)] = e4$ht.3[is.na(height_bl)]
sum(is.na(height_bl))
height_bl[is.na(height_bl)] = e4$ht.6[is.na(height_bl)]
sum(is.na(height_bl))
height_bl[is.na(height_bl)] = e4$ht.12[is.na(height_bl)]
sum(is.na(height_bl))
height_bl[is.na(height_bl)] = e4$ht.scr[is.na(height_bl)]
sum(is.na(height_bl))
summary(height_bl)

ht = cbind(e4, height_bl)
ht2 = ht[, names(ht) %in% c("study_id", "height_bl")]
d7 = merge(d6, ht2, by="study_id", all.x=TRUE)
d7$bmi = d7$weight_gcrc/(d7$height_bl^2)
summary(d7$bmi)
d7$MetSyn2 = -1*d7$MetSyn

##Save second set of outcomes
write.csv(d7, "/Users/jrigdon/Box sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Outcomes_2.csv", row.names=FALSE)



#Output 4 tables and then combine in Word


##Do same thing as above for non-diet endpoints

##Make tables for each of bl, m3, m6, m12




##MAIN ANALYSES##
#Load necessary functions and packages
library(lme4)
#install.packages("lmerTest")
library(lmerTest)
library(pbkrtest)
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Figures.R")

#Load data
d = read.csv("/Users/jrigdon/Box sync/Rigdon/Gardner/QSU_Shared/Oct6_presentation/model_data_new3.csv",header=TRUE)
d[d$study_id==2047,]

##Numbers of Genotyped individuals
#dd1 = d[d$time=="a.BL", ]
#table(dd1$LF_LC_NG, exclude=NULL)

summary(d$weight_gcrc[d$time=="a.BL"])
summary(d$weight_gcrc[d$time=="b.3M"])
summary(d$weight_gcrc[d$time=="c.6M"])
summary(d$weight_gcrc[d$time=="d.12M"])

#Do calculations for CG
dim(d)
names(d)

bl = d[d$time=="a.BL",]
m3 = d[d$time=="b.3M",][,1:2]
names(m3)[2] = "weight3"
m6 = d[d$time=="c.6M",][,1:2]
names(m6)[2] = "weight6"
m12 = d[d$time=="d.12M",][,1:2]
names(m12)[2] = "weight12"

dd1 = merge(bl,m3,by="study_id")
dd2 = merge(dd1,m6,by="study_id")
d3 = merge(dd2,m12,by="study_id")


d3$change3 = d3$weight3-d3$weight_gcrc
d3$change6 = d3$weight6-d3$weight_gcrc
d3$change12 = d3$weight12-d3$weight_gcrc

summary(d3$change3)
summary(d3$change6)
summary(d3$change12)
d3$changePR = apply(d3[, 14:16], 1, function(x) mean(x, na.rm=TRUE))

tt = mktab(data=d3, var.names=c("changePR"), ind.cat=c(0), group.name="LC_diet", cfn=describeMean, miss="always", pval=TRUE, tot="last", digit=1)

#Change summary by group
table(d3$LC_diet,exclude=NULL)
summary(d3$change12[d3$LC_diet==0]) #low fat
sd(d3$change12[d3$LC_diet==0],na.rm=TRUE)

summary(d3$change12[d3$LC_diet==1]) #low carb
sd(d3$change12[d3$LC_diet==1],na.rm=TRUE)


#Total weight lost
#Conservative
d3$cons = d3$change12
d3$cons[is.na(d3$cons)] = 0
sum(d3$cons) #2551.689 kg lost
sum(d3$cons)*2.20462 #5626.505 lbs lost
sum(d3$cons[d3$cons<0])*2.20462 #6135.538 if only gainers

#Optimistic
d3$opt = d3$change12
summary(d3$opt)
d3$opt[is.na(d3$opt)] = d3$change6[is.na(d3$opt)]
summary(d3$opt)
d3$opt[is.na(d3$opt)] = d3$change3[is.na(d3$opt)]
summary(d3$opt) #71 NAs now
d3$opt[is.na(d3$opt)] = 0
summary(d3$opt)
sum(d3$opt) #2966.191 kg lost
sum(d3$opt)*2.20462 #6539.324 lbs lost
sum(d3$opt[d3$opt<0])*2.20462


############
#DIET/MATCH#
############




#Diet/Genotype calculations that Christopher wanted
head(d2)
r = d2[d2$time=="a.BL",]
s = d2[d2$time=="d.12M",][,1:2]
names(s)[2] = "weight_gcrc_12"

rr = merge(r,s,by="study_id",all.x=TRUE)
rr$change = rr$weight_gcrc_12-rr$weight_gcrc
summary(rr$change)
rr$miss12 = ifelse(is.na(rr$change), 1, 0)
rr$mis12 = ifelse(is.na(rr$weight_gcrc_12), 1, 0)

t1 = addmargins(table(rr$LC_diet, rr$LC_geno, rr$miss12)) #there is one person w/ missing baseline weight
round(t1[,,2]/t1[,,3], 2)

#Calculation for Liana
head(d)
r = d[d$time=="a.BL",]
s = d[d$time=="d.12M",][,1:2]
names(s)[2] = "weight_gcrc_12"

rr = merge(r,s,by="study_id",all.x=TRUE)
rr$change = rr$weight_gcrc_12-rr$weight_gcrc
summary(rr$change)
rr$miss12 = ifelse(is.na(rr$change), 1, 0)
rr$mis12 = ifelse(is.na(rr$weight_gcrc_12), 1, 0)

t1 = addmargins(table(rr$LC_diet, rr$LC_geno, rr$miss12, exclude=NULL)) #there is one person w/ missing baseline weight
round(t1[,,2]/t1[,,3], 2)


table(rr$LC_diet,exclude=NULL)
table(rr$LC_geno,exclude=NULL)
summary(rr$change[rr$LC_diet==0]) #61 missing
sd(rr$change[rr$LC_diet==0],na.rm=TRUE)
summary(rr$change[rr$LC_diet==1])
sd(rr$change[rr$LC_diet==1],na.rm=TRUE)



############
#DIET/INS30#
############

#include only those with measured data



##Make change over time plots for the various groups
##GENOTYPE


##INSULIN



#Make the plots using the data set



#Whites
whites = mrg[mrg$white == 1 , ]
list6 = list(whites[whites$LC_diet==1 & whites$LC_geno==1,12],whites[whites$LC_diet==1 & whites$LC_geno==0,12],whites[whites$LC_diet==0 & whites$LC_geno==1,12],whites[whites$LC_diet==0 & whites$LC_geno==0,12])
round(unlist(lapply(list6,function(x) mean(x,na.rm=TRUE))),2)
round(unlist(lapply(list6,function(x) {y = x[!is.na(x)]; sd(y)/sqrt(length(y))})),2)

summary(whites[whites$LC_diet==1 & whites$LC_geno==1,12])

pdf("/Users/jrigdon/Box sync/Rigdon/Gardner/QSU_Shared/Oct6_presentation/geno_diet_whites_2016-11-15.pdf",width=8,height=8)
boxp(list6,pos=c(0.5,1,2,2.5),cols=c("white","white","white","white"),atx=c(0.5,1,2,2.5),labs=c("D1,G1","D1,G2","D2,G1","D2,G2"),ytitle="12-month weight loss (kg)",xtitle="",mtitle="Weight loss by match and diet for whites",ylim=c(-33,12),add.n=TRUE)
dev.off()






























#OLD WORK - DO WE WANT THIS?

#Make another set of tables separating by insulin category
ii = read.csv("/Users/jrigdon/Box sync/Rigdon/Gardner/JAMA_submission/Revision/Data/ins_data_609.csv", header=TRUE)
names(bl_data)[1] = "study_id"

bl2 = merge(bl_data, ii, by="study_id", all.x=TRUE)
bl2$diet.ins = paste(bl2$diet, bl2$ins30, sep="_")
table(bl2$diet.ins, exclude=NULL)
bl2$diet.ins[bl2$diet.ins=="Diet2_NA"] = NA
table(bl2$diet.ins, exclude=NULL)

#Table 1 will require clean up in word and/or extra programming
t1 = mktab(data=bl2,var.names=c("sex","age","education","race3","weight_bl","bmi_bl","bl_fat","bl_waist","bl_ldl","bl_hdl","bl_trig","bl_sys","bl_diast","bl_gluc","bl_ins","bl_ins_auc"),ind.cat=c(1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0),group.name="diet.ins",cfn=describeMean,miss="always",pval=FALSE,tot=FALSE,digit=1)

t1f = mktab(data=bl2[bl2$sex=="a.Female",],var.names=c("weight_bl","bmi_bl","bl_fat","bl_waist"),ind.cat=c(0,0,0,0),group.name="diet.ins",cfn=describeMean,miss="always",pval=FALSE,tot=FALSE,digit=1)
t1m = mktab(data=bl2[bl2$sex=="b.Male",],var.names=c("weight_bl","bmi_bl","bl_fat","bl_waist"),ind.cat=c(0,0,0,0),group.name="diet.ins",cfn=describeMean,miss="always",pval=FALSE,tot=FALSE,digit=1)

#Record in word doc
word.doc(obj.list=list(t1),obj.title=c("Table 1a: Baseline demographics (mean +/- SD, unless otherwise indicated) by diet and insulin-30 tertile at baseline"),dest='/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Tables/Table1a.docx',ftype="Arial",col.odd='white')

word.doc(obj.list=list(t1f, t1m),obj.title=c("Table 1f", "Table1m"),dest='/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Tables/TablesAA.docx',ftype="Arial",col.odd='white')


















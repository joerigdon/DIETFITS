##Load packages and code
library(Gmisc)
library(lme4)
library(lmerTest)
library(multcomp)
library(pbkrtest)

source("/Users/jrigdon/Box Sync/Rigdon/Useful Functions/Tables.R")
source("/Users/jrigdon/Box Sync/Rigdon/Useful Functions/Figures.R")

##Load data sets
geno = read.csv("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Gardner_Imputed.csv", header=TRUE)
table(geno$LF_LC_NG, exclude=NULL)
geno2 = geno[, c(1,5)]

bl_data = read.csv("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/bl_data_2017-06-08.csv", header=TRUE)
sum(!bl_data$bl_id %in% geno$id)

#all11 = read.csv("/Users/jrigdon/Box sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Outcomes_1.csv", header=TRUE)

#d7 = read.csv("/Users/jrigdon/Box sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Outcomes_2.csv", header=TRUE)

d8 = read.csv('/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/All_outcomes_v4.csv', header=TRUE)
d8$diet0 = NA
d8$diet0[d8$diet=="Purple"] = "a.HLF"
d8$diet0[d8$diet=="Blue"] = "b.HLC"
table(d8$diet0, d8$diet, exclude=NULL)

##Fix LIPIDS
##LDL-C and HDL-C to mmol/L, multiply the mg/dL values by 0.02586 (or divide by 38.67)
##Triglycerides to mmol/L, multiply the mg/dL values by 0.01129 (or divide by 88.57)

d8$lipid_ldl_v3 = d8$lipid_ldl_v2/38.67
d8$lipid_hdl_v3 = d8$lipid_hdl_v2/38.67
d8$lipid_trig_v3 = d8$lipid_trig_v2/88.57


#d8$bmi[is.na(d8$bmi) & d8$time=="a.BL"] = 74.25/(1.621003^2)
#write.csv(d8, '/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/All_outcomes_v3.csv', row.names=FALSE)
#ins = read.csv('/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Insulin_2017-11-27.csv', header=TRUE)
#ins2 = ins[, c(1,2,8)]
#ins2$id = paste(ins2$study_id, ins2$redcap_event_name, sep="")
#ins3 = ins2[, c(4,3)]
#d9 = merge(d8, ins3, by="id", all.x=TRUE)
#write.csv(d9, '/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/All_outcomes_v2.csv', row.names=FALSE)
##Fix lipids/bp with missing baseline
#names(d8)
#bld = bl_data[, c(25, 11:15)]
#d9 = merge(d8, bld, by="id", all.x=TRUE)
#d9$lipid_ldl_v2[is.na(d9$lipid_ldl_v2) & d9$time=="a.BL"] = d9$bl_#ldl[is.na(d9$lipid_ldl_v2) & d9$time=="a.BL"]
#d9$lipid_hdl_v2[is.na(d9$lipid_hdl_v2) & d9$time=="a.BL"] = d9$bl_#hdl[is.na(d9$lipid_hdl_v2) & d9$time=="a.BL"]
#d9$lipid_trig_v2[is.na(d9$lipid_trig_v2) & d9$time=="a.BL"] = d9$b#l_trig[is.na(d9$lipid_trig_v2) & d9$time=="a.BL"]
#d9$sbp[is.na(d9$sbp) & d9$time=="a.BL"] = d9$bl_sys[is.na(d9$sbp) & d9$time=="a.BL"]
#d9$dbp[is.na(d9$dbp) & d9$time=="a.BL"] = d9$bl_diast[is.na(d9$dbp) & d9$time=="a.BL"]
#write.csv(d9, '/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/All_outcomes_v4.csv', row.names=FALSE)

d = read.csv("/Users/jrigdon/Box sync/Rigdon/Gardner/JAMA_submission/Revision/Data/model_data_new3.csv",header=TRUE)

##Send subset to science media
cc = d[d$time=="a.BL", names(d) %in% c("study_id", "weight_gcrc", "LC_diet", "LF_LC_NG")]
vv = d[d$time=="d.12M", names(d) %in% c("study_id", "weight_gcrc")]
names(vv)[2] = "weight12"

cv = merge(cc, vv, by="study_id", all.x=TRUE)
cv$weight_change12 = cv$weight_gcrc-cv$weight12
cv2 = cv[, names(cv) %in% c("study_id", "LC_diet", "LF_LC_NG", "weight_change12")]
table(cv2$LF_LC_NG, exclude=NULL)
table(cv2$LC_diet, exclude=NULL)
write.csv(cv2, "/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Data_Science_News_2018-04-20.csv", row.names=FALSE)

dd = read.csv("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Data_Science_News_2018-04-20.csv", header=TRUE)

##Send subset to Jennifer
#write.csv(d, "/Users/jrigdon/Box sync/Rigdon/Gardner/JAMA_submission/Revision/Data/model_data_new3.csv", row.names=FALSE)
#write.csv(d[d$time=="a.BL", c(1,6)], "/Users/jrigdon/Box sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Insulin30_tertiles.csv", row.names=FALSE)

##Table 1: Baseline demographics without p-values (add RER, REE, TEE, AND total for the women/men separated variables)
bl_data$id = paste(bl_data$bl_id, "baseline_arm_1", sep="")
bl1 = bl_data[, c(25, 1, 2, 3, 4, 5, 6)]
bl2 = merge(bl1, d8, by="id", all.x=TRUE)
table(bl2$diet)
bl2$diet = as.character(bl2$diet.x)
bl2$diet[bl2$diet=="Diet2"] = "Diet0"
table(bl2$diet)
hist(bl2$par_kcal_kg_day)
bl2$par_kcal_kg_day[bl2$par_kcal_kg_day==60] = NA
names(geno2)[1] = "bl_id"
bl3 = merge(bl2, geno2, by="bl_id", all.x=TRUE)
#table(bl3$race3, bl3$LF_LC_NG, exclude=NULL)
table(bl3$LF_LC_NG, bl3$diet, exclude=NULL)

##Test normality of continuous variables and replace with median where necessary
varC = c("weight_gcrc", "bmi", "dxa_percentfat", "waist_gcrc", "lipid_ldl_v3", "lipid_hdl_v3", "lipid_trig_v3", "sbp", "dbp", "gluc", "baseline_ins", "thirty_ins", "ree_rq_ave", "ree_ree_ave", "par_kcal_kg_day")

plot(1:3, 1:3)
hist(bl3$weight_gcrc)
qqnorm(bl3$weight_gcrc)
mm = shapiro.test(bl3$weight_gcrc) #Weight OK if p<0.1

getSP = function(varname) {
    bl3$var1 = bl3[, which(names(bl3)==varname)]
    mm = shapiro.test(bl3$var1)
    mm$p.value
}

pvals = double()
for (i in 1:length(varC)) {
    pvals = c(pvals, getSP(varC[i]))
}
dfp = data.frame(vars=varC, pval=round(pvals,4)) #All check out as normal with Shapiro-Wilk test p<0.1

write.csv(bl3, "/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Sylwia/Data_2018-12-05.csv", row.names=FALSE)

t1 = mktab(data=bl3, var.names=c("sex", "age", "education", "race3", "weight_gcrc", "bmi", "dxa_percentfat", "waist_gcrc", "lipid_ldl_v3", "lipid_hdl_v3", "lipid_trig_v3", "sbp", "dbp", "gluc", "baseline_ins", "thirty_ins", "MetSyn", "ree_rq_ave", "ree_ree_ave", "par_kcal_kg_day", "LF_LC_NG"), ind.cat=c(1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1, 0, 0, 0, 1), group.name="diet", cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=4)

t1f = mktab(data=bl3[bl3$sex=="a.Female", ], var.names=c("age", "education", "race3", "weight_gcrc", "bmi", "dxa_percentfat", "waist_gcrc", "lipid_ldl_v2", "lipid_hdl_v2", "lipid_trig_v2", "sbp", "dbp", "gluc", "baseline_ins", "thirty_ins", "MetSyn", "ree_rq_ave", "ree_ree_ave", "par_kcal_kg_day", "LF_LC_NG"), ind.cat=c(0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1, 0, 0, 0, 1), group.name="diet", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=1)

t1m = mktab(data=bl3[bl3$sex=="b.Male", ], var.names=c("age", "education", "race3", "weight_gcrc", "bmi", "dxa_percentfat", "waist_gcrc", "lipid_ldl_v2", "lipid_hdl_v2", "lipid_trig_v2", "sbp", "dbp", "gluc", "baseline_ins", "thirty_ins", "MetSyn", "ree_rq_ave", "ree_ree_ave", "par_kcal_kg_day", "LF_LC_NG"), ind.cat=c(0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1, 0, 0, 0, 1), group.name="diet", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=1)

word.doc(obj.list=list(t1), obj.title=c("Table 1: Baseline demographics (mean +/- SD, unless otherwise indicated)"), dest='/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Tables/Table1_2017-11-27.docx', ftype="Arial", col.odd='white')


##Table 2: Dietary variables vars = ("cal", "carb.g", "protein.g", "fat.g", "saturated_fat.g", "fiber.g", "added_sugars", "carb..", "protein..", "fat..", "saturated_fat..", "fiber.cal", "sugar.cal", "GI_glucose", "GL_glucose") +  HLF-HLC difference + 95% CI from mixed model for each timepoint
vars = c("cal", "carb.g",  "carb..", "fat.g", "fat..", "protein.g", "protein..", "saturated_fat.g", "saturated_fat..", "fiber.g", "fiber.cal", "added_sugars", "sugar.cal", "GI_glucose", "GL_glucose")
d1 = d8[d8$redcap_event_name=="baseline_arm_1" & !is.na(d8$redcap_event_name), ]
d2 = d8[d8$redcap_event_name=="3_months_arm_1" & !is.na(d8$redcap_event_name),]
d3 = d8[d8$redcap_event_name=="6_months_arm_1" & !is.na(d8$redcap_event_name),]
d4 = d8[d8$redcap_event_name=="12_months_arm_1" & !is.na(d8$redcap_event_name),]

t1 = mktab(d1, var.names=vars, ind.cat=rep(0,15), group.name="diet0", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)

t2 = mktab(d2, var.names=vars, ind.cat=rep(0,15), group.name="diet0", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)

t3 = mktab(d3, var.names=vars, ind.cat=rep(0,15), group.name="diet0", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)

t4 = mktab(d4, var.names=vars, ind.cat=rep(0,15), group.name="diet0", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1) #change to miss="always" to get missing

#Combine into one long table
tt = double()
for (i in 2:16) {
    tt2 = rbind(c("", ""),t1[i,],t2[i,],t3[i,],t4[i,])
    tt = rbind(tt,tt2)
}


##Get mixed effects model CIs
contrast.matrix3 = rbind("diff0" =  c(0, 0, 0, 0, 1, 0, 0, 0),
    "diff3" =  c(0, 0, 0, 0, 1, 1, 0, 0),
    "diff6" = c(0, 0, 0, 0, 1, 0, 1, 0),
    "diff12" = c(0, 0, 0, 0, 1, 0, 0, 1))


getCI = function(varname) {
    d8$var1 = d8[, which(names(d8)==varname)]
    mm = lmer(var1 ~ time*diet +(1|study_id), data=d8)
    jj = round(confint(glht(mm, contrast.matrix3), calpha = univariate_calpha())$confint, 2)
    rbind(paste(paste(paste(paste(jj[1,1], " (", sep=""), jj[1,2], sep=""), jj[1,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[2,1], " (", sep=""), jj[2,2], sep=""), jj[2,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[3,1], " (", sep=""), jj[3,2], sep=""), jj[3,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[4,1], " (", sep=""), jj[4,2], sep=""), jj[4,3], sep=", "), ")", sep=""))
   }


sp = c("")
CI1 = c(sp, getCI("cal"), sp, getCI("carb.g"), sp, getCI("carb.."), sp, getCI("fat.g"), sp, getCI("fat.."), sp, getCI("protein.g"), sp, getCI("protein.."), sp, getCI("saturated_fat.g"), sp, getCI("saturated_fat.."), sp, getCI("fiber.g"), sp, getCI("fiber.cal"), sp, getCI("added_sugars"), sp, getCI("sugar.cal"), sp, getCI("GI_glucose"), sp, getCI("GL_glucose"))

ttt = cbind(tt, CI1)

dd = c(" Baseline"," 3 Months"," 6 Months"," 12 Months")
rownames(ttt) = c("Calories (kcal)", dd, "Carbohydrates (g)", dd, "Carbohydrates (%kcal)", dd, "Fat (g)", dd, "Fat (%kcal)", dd, "Protein (g)", dd, "Protein (%kcal)", dd, "Saturated fat (g)", dd,"Saturated fat (%kcal)", dd, "Fiber (g)", dd, "Fiber (g/1000 kcal)", dd, "Added sugars (g)", dd, "Sugar (g/1000 kcal)", dd, "Glycemic index (glucose ref)", dd, "Glycemic load (glucose ref)", dd)

word.doc(obj.list=list(ttt), obj.title=c("Table 2: Dietary intake in DIETFITS study by timepoint"),dest="/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Tables/Table2_2017-11-28.docx", ftype="Arial", col.odd="white")


##Table 3: 12 month changes in secondary outcomes (weight, BMI, body fat, waist, ldl, hdl, trig, sbp, dbp, glu, ins, MetSyn, RER, REE, TEE) from mixed effects model within (HLF, HLC) and between (HLF-HLC) with estimate and p-value for HLF-HLC difference
#names(d7)
#names(all11)
#d7$id = paste(d7$study_id, d7$redcap_event_name, sep="")
#d8 = merge(d7, all11[, c(1, 4:21, 24:26)], by="id", all.x=TRUE)
#write.csv(d8, '/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/All_outcomes.csv', row.names=FALSE)
#d8 = read.csv('/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/All_outcomes.csv', header=TRUE)
#summary(d8$bmi)
#d8[which.max(d8$bmi), ]  #1082
#bl2[bl2$bl_id==1082, ]
#d8$bmi[which.max(d8$bmi)] = 39.79328
#summary(d8$bmi)

table(d8$redcap_event_name)
d8$time = NA
d8$time[d8$redcap_event_name=="baseline_arm_1"] = "a.BL"
d8$time[d8$redcap_event_name=="3_months_arm_1"] = "b.3"
d8$time[d8$redcap_event_name=="6_months_arm_1"] = "c.6"
d8$time[d8$redcap_event_name=="12_months_arm_1"] = "d.12"
table(d8$redcap_event_name, d8$time, exclude=NULL)
table(d8$diet)

names(d8)
dim(d8)

#Comparisons of interest for Table 2
contrast.matrix = rbind("HLF12" =  c(0, 0, 0, 1, 0, 0, 0, 1),
    "HLC12" =  c(0, 0, 0, 1, 0, 0, 0, 0),
    "diff12" = c(0, 0, 0, 0, 0, 0, 0, 1))


##95% CIs
confint(glht(y1, contrast.matrix), calpha = univariate_calpha()) #Univariate
#confint(glht(y1, contrast.matrix)) #Family-wise default
#confint(summary(glht(y1, contrast.matrix), test=adjusted("Westfall"))) #Westfall test
#con = summary(glht(y1, contrast.matrix), test=adjusted("Westfall"))
#con$test$pvalues[1:3] #p-values
#con$test$sigma[1:3] #to get standard errors

##weight_gcrc
#d8$weight_gcrc[is.na(d8$weight_gcrc) & d8$time=="a.BL"] = 74.25
y1 = lmer(weight_gcrc ~ time*diet +(1|study_id), data=d8)
summary(y1) #for p-value
round(confint(glht(y1, contrast.matrix), calpha = univariate_calpha())$confint, 2) #Univariate

##bmi
y2 = lmer(bmi ~ time*diet +(1|study_id), data=d8)
summary(y2) #for p-value
round(confint(glht(y2, contrast.matrix), calpha = univariate_calpha())$confint, 2) #Univariate

##dxa_percentfat##
d87 = d8[d8$time!="b.6", ]
contrast.matrix2 = rbind("HLF12" =  c(0, 0, 1, 0, 0, 1),
    "HLC12" =  c(0, 0, 1, 0, 0, 0),
    "diff12" = c(0, 0, 0, 0, 0, 1))
y4 = lmer(dxa_percentfat ~ time*diet +(1|study_id), data=d87)
summary(y4)
round(confint(glht(y4, contrast.matrix2), calpha = univariate_calpha())$confint, 2) #Univariate

##waist_gcrc##
y3 = lmer(waist_gcrc ~ time*diet +(1|study_id), data=d8)
summary(y3)
round(confint(glht(y3, contrast.matrix), calpha = univariate_calpha())$confint, 2)

##lipid_ldl_v2##
y6 = lmer(lipid_ldl_v3 ~ time*diet +(1|study_id), data=d8)
summary(y6)
round(confint(glht(y6, contrast.matrix), calpha = univariate_calpha())$confint, 3)

##lipid_hdl_v2##
y7 = lmer(lipid_hdl_v3 ~ time*diet +(1|study_id), data=d8)
summary(y7)
round(confint(glht(y7, contrast.matrix), calpha = univariate_calpha())$confint, 3)

##lipid_trig_v2##
y5 = lmer(lipid_trig_v3 ~ time*diet +(1|study_id), data=d8)
summary(y5)
round(confint(glht(y5, contrast.matrix), calpha = univariate_calpha())$confint, 3)

##sbp##
y10 = lmer(sbp ~ time*diet +(1|study_id), data=d8)
summary(y10)
round(confint(glht(y10, contrast.matrix), calpha = univariate_calpha())$confint, 2)

##dbp##
y11 = lmer(dbp ~ time*diet +(1|study_id), data=d8)
summary(y11)
round(confint(glht(y11, contrast.matrix), calpha = univariate_calpha())$confint, 2)

##gluc##
y9 = lmer(gluc ~ time*diet +(1|study_id), data=d8)
summary(y9)
round(confint(glht(y9, contrast.matrix), calpha = univariate_calpha())$confint, 2)

##baseline_ins##
y8 = lmer(baseline_ins ~ time*diet +(1|study_id), data=d8)
summary(y8)
round(confint(glht(y8, contrast.matrix), calpha = univariate_calpha())$confint, 2)

##thirty_ins##
y8a = lmer(thirty_ins ~ time*diet +(1|study_id), data=d87)
summary(y8a)
round(confint(glht(y8a, contrast.matrix2), calpha = univariate_calpha())$confint, 2)

##RER##
y12 = lmer(ree_rq_ave ~ time*diet +(1|study_id), data=d8)
summary(y12)
round(confint(glht(y12, contrast.matrix2), calpha = univariate_calpha())$confint, 5)

##REE##
y13 = lmer(ree_ree_ave ~ time*diet +(1|study_id), data=d8)
summary(y13)
round(confint(glht(y13, contrast.matrix2), calpha = univariate_calpha())$confint, 2)

##TEE##
y14 = lmer(par_kcal_kg_day ~ time*diet +(1|study_id), data=d8)
summary(y14)
round(confint(glht(y14, contrast.matrix), calpha = univariate_calpha())$confint, 4)

##MetSyn2"
ms = glmer(MetSyn ~ time*diet +(1|study_id), family=binomial, data=d8)
summary(ms) #P of 1.00
#anova(ms,ddf="kenward-roger",type=3)
library(car)
Anova(ms, type=2)

##FILLED IN ABOVE BY HAND TO EXISTING TABLE


##Figure 1: CONSORT diagram

##Figure 2a: Genotype weight loss within diet (add Neither, but NOT in model; circle/diamond for mean; detailed description of numbers not in graphic)
names(d)
d$redcap_event_name = NA
d$redcap_event_name[d$time=="a.BL"] = "baseline_arm_1"
d$redcap_event_name[d$time=="b.3M"] = "3_months_arm_1"
d$redcap_event_name[d$time=="c.6M"] = "6_months_arm_1"
d$redcap_event_name[d$time=="d.12M"] = "12_months_arm_1"
table(d$redcap_event_name, d$time, exclude=NULL)
d$id = paste(d$study_id, d$redcap_event_name, sep="")
names(d)
dk = d[, c(12, 2, 5, 6)]
names(dk)[2:3] = c("weight_gcrc2", "LF_LC_NG2")

d9 = merge(d8, dk, by="id", all.x=TRUE)

table(d9$LF_LC_NG, d9$LF_LC_NG2, exclude=NULL)
plot(d9$weight_gcrc, d9$weight_gcrc2)

#only include those 424 with LC/LF genotype (1696 obs)
d9$LC_geno = ifelse(d9$LF_LC_NG=="LC", 1, NA)
table(d9$LC_geno, exclude=NULL)
d9$LC_geno[d9$LF_LC_NG=="LF"] = 0
table(d9$LC_geno, exclude=NULL)
d9$LC_diet = ifelse(d9$diet=="Blue", 1, 0)
table(d9$LC_diet, d9$diet)

#include only those with measured data
d2 = d9[!is.na(d9$LC_geno), ] #only include those with measured genotype info (424)


#Table of LC_diet, LC_geno
length(table(d2$study_id)) #424
dd = d2[!duplicated(d2$study_id), ]
table(dd$diet, dd$LC_geno, exclude=NULL)
addmargins(table(dd$LC_diet, dd$LC_geno))


#MAIN GENOTYPE ANALYSIS
j = lmer(weight_gcrc~time*diet*LF_LC_NG2+(1|study_id), data=d2)
summary(j)
cmatrix = matrix(c(rep(0, 15), 1), 1, 16)
round(confint(glht(j, cmatrix), calpha = univariate_calpha())$confint, 2)

##Whites only
race = bl_data[, names(bl_data) %in% c("bl_id", "sex", "race3")]
names(race)[1] = "study_id"
d2w = merge(d2, race, by="study_id", all.x=TRUE)
d2ww = d2w[d2w$race3=="a.White", ]
length(table(d2$study_id))
length(table(d2ww$study_id))

jw = lmer(weight_gcrc~time*diet*LF_LC_NG2+(1|study_id), data=d2ww)
summary(jw) #p=0.0673
cmatrix = matrix(c(rep(0, 15), 1), 1, 16)
round(confint(glht(jw, cmatrix), calpha = univariate_calpha())$confint, 2)

##Add sex variable
js = lmer(weight_gcrc~sex + time*diet*LF_LC_NG2+(1|study_id), data=d2w)
summary(js)
cmatrix2 = matrix(c(rep(0, 16), 1), 1, 17)
round(confint(glht(js, cmatrix2), calpha = univariate_calpha())$confint, 2)

#Type 1
#SS(A) for factor A.
#SS(B | A) for factor B.
#SS(AB | B, A) for interaction AB.

#Type 2
#SS(A | B) for factor A.
#SS(B | A) for factor B.
#This type tests for each main effect after the other main effect.
#Note that no significant interaction is assumed (in other words, you should test for interaction first (SS(AB | A, B)) and only if AB is not significant, continue with the analysis for main effects).

#Type 3 (default; what does factor add in presence of everything else)
#SS(A | B, AB) for factor A.
#SS(B | A, AB) for factor B.
#This type tests for the presence of a main effect after the other main effect and interaction. This approach is therefore valid in the presence of significant interactions.

#Can get the group interaction tests
anova(j,ddf="kenward-roger",type=3) #From lmerTest; figure out what the various "types" mean from SAS

#Or the individual model coefficients
summary(j)
print(summary(j,ddf="Satterthwaite"),correlation=FALSE) #p=0.20
cmatrix = matrix(c(rep(0, 15), 1), 1, 16)
round(confint(glht(j, cmatrix), calpha = univariate_calpha())$confint, 2)


##Boxplot for main paper
r = d9[d9$time=="a.BL", ]
s = d9[d9$time=="d.12", c(2,4)]
names(s)[2] = "weight12"

mrg = merge(r, s, by="study_id",all.x=TRUE)
mrg$change = mrg$weight12-mrg$weight_gcrc

##Summary of the info on boxplot
mrg$LF_LC_NG2[is.na(mrg$LF_LC_NG2)] = "NG"
table(mrg$diet, mrg$LF_LC_NG2, exclude=NULL)

summary(mrg$change[mrg$diet=="Purple" & mrg$LF_LC_NG2=="LF"]) #47/130 missing (n=83)
summary(mrg$change[mrg$diet=="Purple" & mrg$LF_LC_NG2=="LC"]) #13/83 missing (n=70)
summary(mrg$change[mrg$diet=="Purple" & mrg$LF_LC_NG2=="NG"]) #29/92 missing (n=63)
summary(mrg$change[mrg$diet=="Blue" & mrg$LF_LC_NG2=="LF"]) #33/114 missing (n=81)
summary(mrg$change[mrg$diet=="Blue" & mrg$LF_LC_NG2=="LC"]) #18/97 missing (n=79)
summary(mrg$change[mrg$diet=="Blue" & mrg$LF_LC_NG2=="NG"]) #33/93 missing (n=60)

list7 = list(LF_LF=mrg$change[mrg$diet=="Purple" & mrg$LF_LC_NG2=="LF"], LF_LC=mrg$change[mrg$diet=="Purple" & mrg$LF_LC_NG2=="LC"], LF_NG=mrg$change[mrg$diet=="Purple" & mrg$LF_LC_NG2=="NG"], LC_LF=mrg$change[mrg$diet=="Blue" & mrg$LF_LC_NG2=="LF"], LC_LC=mrg$change[mrg$diet=="Blue" & mrg$LF_LC_NG2=="LC"], LC_NG=mrg$change[mrg$diet=="Blue" & mrg$LF_LC_NG2=="NG"])

mDG = as.numeric(unlist(lapply(list7, function(x) mean(x, na.rm=TRUE))))

pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/Fig2a_2018-01-10_v4.pdf")
plot(x = c(0, 4), y=c(-33, 15), type="n", axes=F, xlab="", ylab="")
abline(h=10, col="grey", lwd=0.5)
abline(h=0, lwd=1.5)
abline(h=-10, col="grey", lwd=0.5)
abline(h=-20, col="grey", lwd=0.5)
abline(h=-30, col="grey", lwd=0.5)
boxp(list7,pos=c(0.5, 1, 1.5, 2.5, 3, 3.5),cols=c(rep("white", 3), rep("grey", 3)),atx=c(0.5, 1, 1.5, 2.5, 3, 3.5), labs=c("LFG","LCG","NG","LFG","LCG","NG"),ytitle="12-month weight change (kg)", xtitle="Genotype", mtitle="", ylim=c(-33,15), add.n=TRUE, rotx=NA, xcex=1, ad=TRUE)
legend("topright", fill=c("white", "grey"), legend=c("Healthy Low Fat Diet", "Healthy Low Carb Diet"), bty="n")
points(c(0.5, 1, 1.5, 2.5, 3, 3.5), mDG, pch=5)
dev.off()

##Re-order
list7r = list7[c(1, 4, 2, 5, 3, 6)]

mDGr = as.numeric(unlist(lapply(list7r, function(x) mean(x, na.rm=TRUE))))

pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/Fig2a_2018-01-10_v4_reorder.pdf")
plot(x = c(0, 4.5), y=c(-33, 15), type="n", axes=F, xlab="", ylab="")
abline(h=10, col="grey", lwd=0.5)
abline(h=0, lwd=1.5)
abline(h=-10, col="grey", lwd=0.5)
abline(h=-20, col="grey", lwd=0.5)
abline(h=-30, col="grey", lwd=0.5)
boxp(list7r,pos=c(0.5, 1, 2, 2.5, 3.5, 4),cols=rep(c("white", "grey"), 3), atx=c(0.5, 1, 2, 2.5, 3.5, 4), labs=c("LFG","LFG","LCG","LCG","NG","NG"),ytitle="12-month weight change (kg)", xtitle="Genotype", mtitle="", ylim=c(-33,15), add.n=TRUE, rotx=NA, xcex=1, ad=TRUE)
legend("topright", fill=c("white", "grey"), legend=c("Healthy Low Fat Diet", "Healthy Low Carb Diet"), bty="n")
points(c(0.5, 1, 2, 2.5, 3.5, 4), mDGr, pch=5)
dev.off()


##Figure 2b: Insulin tertile weight loss within diet (circle/diamond for mean; detailed description of numbers not in graphic)
j2 = lmer(weight_gcrc~time*diet*thirty_ins+(1|study_id),data=d9)


race = bl_data[, names(bl_data) %in% c("bl_id", "sex", "race3")]
names(race)[1] = "study_id"
d9s = merge(d9, race, by="study_id", all.x=TRUE)


##Add sex variable
j2s = lmer(weight_gcrc~sex + time*diet*thirty_ins+(1|study_id),data=d9s)
summary(j2s)
cmatrix3 = matrix(c(rep(0, 12), 10), 1, 13)
round(confint(glht(j2s, cmatrix3), calpha = univariate_calpha())$confint, 2)



#Or the individual model coefficients
print(summary(j2,ddf="Satterthwaite"),correlation=FALSE) #0.47
summary(j2)
cmatrix = matrix(c(rep(0, 11), 10), 1, 12)
round(confint(glht(j2, cmatrix), calpha = univariate_calpha())$confint, 2)

#2 df test
#L <- matrix(0, ncol = 24, nrow = 2)
#L[1,21] <- L[2,24] <- 1
#calcSatterth(j2, L) #p=0.84

#Can get the group interaction tests
#anova(j2,ddf="kenward-roger",type=3) #From lmerTest; figure out what the various "types" mean from SAS

##Insulin box plots
mrg2 = mrg[!is.na(mrg$ins30), ]
table(mrg$diet, mrg$ins30, exclude=NULL)
summary(mrg2$change[mrg2$diet=="Purple" & mrg2$ins30==1]) #30/96 missing (n=66)
summary(mrg2$change[mrg2$diet=="Purple" & mrg2$ins30==2]) #24/105 missing (n=81)
summary(mrg2$change[mrg2$diet=="Purple" & mrg2$ins30==3]) #33/101 missing (n=68)
summary(mrg2$change[mrg2$diet=="Blue" & mrg2$ins30==1]) #21/106 missing (n=85)
summary(mrg2$change[mrg2$diet=="Blue" & mrg2$ins30==2]) #26/97 missing (n=71)
summary(mrg2$change[mrg2$diet=="Blue" & mrg2$ins30==3]) #37/101 missing (n=64)

list8 = list(LF_LF=mrg2$change[mrg2$diet=="Purple" & mrg2$ins30==1], LF_LC=mrg2$change[mrg2$diet=="Purple" & mrg2$ins30==2], LF_NG=mrg2$change[mrg2$diet=="Purple" & mrg2$ins30==3], LC_LF=mrg2$change[mrg2$diet=="Blue" & mrg2$ins30==1], LC_LC=mrg2$change[mrg2$diet=="Blue" & mrg2$ins30==2], LC_NG=mrg2$change[mrg2$diet=="Blue" & mrg2$ins30==3])

mDI = as.numeric(unlist(lapply(list8, function(x) mean(x, na.rm=TRUE))))

pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/Fig2b_2018-01-10_v4.pdf")
plot(x = c(0, 4), y=c(-33, 15), type="n", axes=F, xlab="", ylab="")
abline(h=10, col="grey", lwd=0.5)
abline(h=0, lwd=1.5)
abline(h=-10, col="grey", lwd=0.5)
abline(h=-20, col="grey", lwd=0.5)
abline(h=-30, col="grey", lwd=0.5)
boxp(list8,pos=c(0.5, 1, 1.5, 2.5, 3, 3.5),cols=c(rep("white", 3), rep("grey", 3)),atx=c(0.5, 1, 1.5, 2.5, 3, 3.5), labs=c("Low","Mid","High","Low","Mid","High"),ytitle="12-month weight change (kg)", xtitle="Insulin-30 Tertile at Baseline", mtitle="", ylim=c(-33,15), add.n=TRUE, rotx=NA, xcex=0.75, ad=TRUE)
legend("topright", fill=c("white", "grey"), legend=c("Healthy Low Fat Diet", "Healthy Low Carb Diet"), bty="n")
points(c(0.5, 1, 1.5, 2.5, 3, 3.5), mDI, pch=5)
dev.off()

##Re-order
list8r = list8[c(1, 4, 2, 5, 3, 6)]

mDIr = as.numeric(unlist(lapply(list8r, function(x) mean(x, na.rm=TRUE))))

pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/Fig2b_2018-01-10_v4_reorder.pdf")
plot(x = c(0, 4.5), y=c(-33, 15), type="n", axes=F, xlab="", ylab="")
abline(h=10, col="grey", lwd=0.5)
abline(h=0, lwd=1.5)
abline(h=-10, col="grey", lwd=0.5)
abline(h=-20, col="grey", lwd=0.5)
abline(h=-30, col="grey", lwd=0.5)
boxp(list8r,pos=c(0.5, 1, 2, 2.5, 3.5, 4),cols=rep(c("white", "grey"), 3), atx=c(0.5, 1, 2, 2.5, 3.5, 4), labs=c("Low","Low","Mid","Mid","High","High"),ytitle="12-month weight change (kg)", xtitle="Insulin-30 Tertile at Baseline", mtitle="", ylim=c(-33,15), add.n=TRUE, rotx=NA, xcex=0.75, ad=TRUE)
legend("topright", fill=c("white", "grey"), legend=c("Healthy Low Fat Diet", "Healthy Low Carb Diet"), bty="n")
points(c(0.5, 1, 2, 2.5, 3.5, 4), mDIr, pch=5)
dev.off()


##eTable 1: Baseline data by genotype/diet (6 columns, no p-values)
names(d)
dj = d[d$time=="a.BL", ]
names(dj)[1] = "bl_id"
names(bl2)
bl33 = merge(bl2, dj[, c(1, 5, 6)], by="bl_id", all.x=TRUE)

jb1 = d8[d8$time=="a.BL", ]
jb2 = jb1[, c(2, 22)]
names(jb2) = c("bl_id", "MetSyn3")
bl3a = merge(bl33, jb2, by="bl_id", all.x=TRUE)


#bl3$sex = as.character(bl3$sex)
#bl3$MetSyn = as.character(bl3$MetSyn)
table(bl3a$LF_LC_NG, exclude=NULL)
bl3a$LF_LC_NG = as.character(bl3a$LF_LC_NG)
table(bl3a$LF_LC_NG, exclude=NULL)
bl3a$LF_LC_NG[is.na(bl3a$LF_LC_NG)] = "NG"
table(bl3a$LF_LC_NG, exclude=NULL)

bl3a$GD_group = paste(bl3a$diet, bl3a$LF_LC_NG, sep="_")
table(bl3a$GD_group, exclude=NULL)
#bl4 = bl3a[!is.na(bl3a$LF_LC_NG), ] #570
#table(bl4$GD_group, exclude=NULL)
#bl4$MetSyn2 = -1*(bl4$MetSyn-1)

et1 = mktab(data=bl3a, var.names=c("sex", "age", "education", "race3", "weight_gcrc", "bmi", "dxa_percentfat", "waist_gcrc", "lipid_ldl_v3", "lipid_hdl_v3", "lipid_trig_v3", "sbp", "dbp", "gluc", "baseline_ins", "thirty_ins", "MetSyn", "ree_rq_ave", "ree_ree_ave", "par_kcal_kg_day"), ind.cat=c(1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1, 0, 0, 0, 1), group.name="GD_group", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=2)

#et1f = mktab(data=bl4[bl4$sex=="a.Female", ], var.names=c("age", "education", "race3", "weight_bl", "bmi_bl", "bl_fat", "bl_waist", "bl_ldl", "bl_hdl", "bl_trig", "bl_sys", "bl_diast", "bl_gluc", "bl_ins", "bl_ins_30", "MetSyn3", "ree_ree_ave", "ree_rq_ave", "par_kcal_kg_day"), ind.cat=c(0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1, 0, 0, 0), group.name="GD_group", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=1)

#et1m = mktab(data=bl4[bl4$sex=="b.Male", ], var.names=c("age", "education", "race3", "weight_bl", "bmi_bl", "bl_fat", "bl_waist", "bl_ldl", "bl_hdl", "bl_trig", "bl_sys", "bl_diast", "bl_gluc", "bl_ins", "bl_ins_30", "MetSyn3", "ree_ree_ave", "ree_rq_ave", "par_kcal_kg_day"), ind.cat=c(0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1, 0, 0, 0), group.name="GD_group", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=1)

word.doc(obj.list=list(et1), obj.title=c("eTable 1: Baseline demographics by genotype-diet group"), dest='/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Tables/eTable2b_2017-12-01.docx', ftype="Arial", col.odd='white')


##eTable 2: Baseline data by insulin-30 tertile/diet (6 columns, no p-values)
bl3a$ID_group = paste(bl3a$diet, bl3a$ins30, sep="_")
table(bl3a$ID_group, exclude=NULL)
bl5 = bl3a[!is.na(bl3a$ins30), ] #606
table(bl5$ID_group, exclude=NULL)
#bl5$MetSyn2 = -1*(bl5$MetSyn-1)

et2 = mktab(data=bl5, var.names=c("sex", "age", "education", "race3", "weight_gcrc", "bmi", "dxa_percentfat", "waist_gcrc", "lipid_ldl_v3", "lipid_hdl_v3", "lipid_trig_v3", "sbp", "dbp", "gluc", "baseline_ins", "thirty_ins", "MetSyn", "ree_rq_ave", "ree_ree_ave", "par_kcal_kg_day"), ind.cat=c(1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1, 0, 0, 0, 1), group.name="ID_group", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=2)

#et2f = mktab(data=bl5[bl5$sex=="a.Female", ], var.names=c("age", "education", "race3", "weight_bl", "bmi_bl", "bl_fat", "bl_waist", "bl_ldl", "bl_hdl", "bl_trig", "bl_sys", "bl_diast", "bl_gluc", "bl_ins", "bl_ins_30", "MetSyn3", "ree_ree_ave", "ree_rq_ave", "par_kcal_kg_day"), ind.cat=c(0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1, 0, 0, 0), group.name="ID_group", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=1)

#et2m = mktab(data=bl5[bl5$sex=="b.Male", ], var.names=c("age", "education", "race3", "weight_bl", "bmi_bl", "bl_fat", "bl_waist", "bl_ldl", "bl_hdl", "bl_trig", "bl_sys", "bl_diast", "bl_gluc", "bl_ins", "bl_ins_30", "MetSyn3", "ree_ree_ave", "ree_rq_ave", "par_kcal_kg_day"), ind.cat=c(0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1, 0, 0, 0), group.name="ID_group", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=1)

word.doc(obj.list=list(et2), obj.title=c("eTable 2: Baseline demographics by ins30-diet group"), dest='/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Tables/eTable2a_2017-12-01.docx', ftype="Arial", col.odd='white')

range(bl5$thirty_ins[bl5$ID_group=="Diet0_1"])
range(bl5$thirty_ins[bl5$ID_group=="Diet0_2"])
range(bl5$thirty_ins[bl5$ID_group=="Diet0_3"])
range(bl5$thirty_ins[bl5$ID_group=="Diet1_1"])
range(bl5$thirty_ins[bl5$ID_group=="Diet1_2"])
range(bl5$thirty_ins[bl5$ID_group=="Diet1_3"])


##eTable 3: Secondary outcomes at all timepoints + HLF-HLC difference + 95% CIs from mixed models for each timepoint
d8$par_kcal_kg_day[d8$par_kcal_kg_day==60] = NA
dd1 = d8[d8$redcap_event_name=="baseline_arm_1" & !is.na(d8$redcap_event_name), ]
dd2 = d8[d8$redcap_event_name=="3_months_arm_1" & !is.na(d8$redcap_event_name),]
dd3 = d8[d8$redcap_event_name=="6_months_arm_1" & !is.na(d8$redcap_event_name),]
dd4 = d8[d8$redcap_event_name=="12_months_arm_1" & !is.na(d8$redcap_event_name),]


##Check MetSyn data
#ms1 = dd1[, names(dd1) %in% c("crit1", "crit2", "crit3", "crit4", "crit5", "MetSyn", "gluc", "sbp", "dbp", "lipid_trig_v2", "lipid_ldl_v2", "lipid_hdl_v2", "scr_gender", "waist_gcrc", "study_id", "diet")]

#write.csv(ms1, "/Users/jrigdon/Box sync/Rigdon/Gardner/JAMA_submission/Revision/Data/MetSyn_baseline_2017-08-29.csv", row.names=FALSE)
vars1 = c("weight_gcrc", "bmi", "dxa_percentfat", "waist_gcrc", "lipid_ldl_v3", "lipid_hdl_v3", "lipid_trig_v3", "sbp", "dbp", "baseline_ins", "thirty_ins", "gluc", "MetSyn2", "ree_rq_ave", "ree_ree_ave", "par_kcal_kg_day")

#dd1$weight_gcrc[is.na(dd1$weight_gcrc)] = 74.25
tt1 = mktab(dd1, var.names=vars1, ind.cat=c(rep(0,12), 1, 0, 0, 0), group.name="diet0", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)[-14, ]

tt2 = mktab(dd2, var.names=vars1, ind.cat=c(rep(0,12), 1, 0, 0, 0), group.name="diet0", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)[-14, ]

tt3 = mktab(dd3, var.names=vars1, ind.cat=c(rep(0,12), 1, 0, 0, 0), group.name="diet0", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)[-14, ]

tt4 = mktab(dd4, var.names=vars1, ind.cat=c(rep(0,12), 1, 0, 0, 0), group.name="diet0", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)[-14, ]

##Number of observations at each timepoint
vars2 = vars1[-13]
vars3 = vars2[-c(3, 11, 13, 14)]
tt1 = mktab(dd1, var.names=vars2, ind.cat=rep(0,15), group.name="diet0", cfn=describeMean, miss="no", pval=FALSE, tot=FALSE, digit=1)[-1, ]

tt2 = mktab(dd2, var.names=vars3, ind.cat=rep(0,11), group.name="diet0", cfn=describeMean, miss="no", pval=FALSE, tot=FALSE, digit=1)

tt3 = mktab(dd3, var.names=vars2, ind.cat=rep(0,15), group.name="diet0", cfn=describeMean, miss="no", pval=FALSE, tot=FALSE, digit=1)[-1, ]

tt4 = mktab(dd4, var.names=vars2, ind.cat=rep(0,15), group.name="diet0", cfn=describeMean, miss="no", pval=FALSE, tot=FALSE, digit=1)[-1, ]

##MetSyn numbers
table(dd1$MetSyn, dd1$diet0, exclude=NULL)
table(dd2$MetSyn, dd2$diet0, exclude=NULL)
table(dd3$MetSyn, dd3$diet0, exclude=NULL)
table(dd4$MetSyn, dd4$diet0, exclude=NULL)


tt2a = rbind(tt2[2:3,], c("",""), tt2[4:10,], c("",""), tt2[11,], c("",""), c("",""), tt2[12,])

ttvv = rbind(tt1[1, ] , tt2a[1,], tt3[1,], tt4[1,], c("", ""))
for (i in c(2:15)) {
    jj = rbind(tt1[i,], tt2a[i,], tt3[i,], tt4[i,], c("", ""))
    ttvv = rbind(ttvv, jj)
}

ttvv2 = ttvv[-c(12, 52, 62, 67), ]
word.doc(obj.list=list(ttvv2), obj.title=c("eTable 3: Sample sizes"), dest='/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Tables/eTable3_ONE_DIGIT.docx', ftype="Arial", col.odd='white')


##Get mixed effects model CIs
contrast.matrix3 = rbind("diff0" =  c(0, 0, 0, 0, 1, 0, 0, 0),
    "diff3" =  c(0, 0, 0, 0, 1, 1, 0, 0),
    "diff6" = c(0, 0, 0, 0, 1, 0, 1, 0),
    "diff12" = c(0, 0, 0, 0, 1, 0, 0, 1))

contrast.matrix4 = rbind("diff0" =  c(0, 0, 0, 1, 0, 0),
    "diff6" =  c(0, 0, 0, 1, 1, 0),
    "diff12" =  c(0, 0, 0, 1, 0, 1))

getCI2 = function(varname) {
    d8$var1 = d8[, which(names(d8)==varname)]
    mm = lmer(var1 ~ time*diet +(1|study_id), data=d8)
    jj = round(confint(glht(mm, contrast.matrix4), calpha = univariate_calpha())$confint, 4)
    rbind(paste(paste(paste(paste(jj[1,1], " (", sep=""), jj[1,2], sep=""), jj[1,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[2,1], " (", sep=""), jj[2,2], sep=""), jj[2,3], sep=", "), ")", sep=""), paste(paste(paste(paste(jj[3,1], " (", sep=""), jj[3,2], sep=""), jj[3,3], sep=", "), ")", sep=""))
   }

##Take out missings
ttv3 = ttv2[-c(14, 54, 69, 74), ]

sp = c("")
CI2 = c(sp, sp, getCI("weight_gcrc"), sp, getCI("bmi"), sp, getCI2("dxa_percentfat"), sp, getCI("waist_gcrc"), sp, getCI("lipid_ldl_v3"), sp, getCI("lipid_hdl_v3"), sp, getCI("lipid_trig_v3"), sp, getCI("sbp"), sp,  getCI("dbp"), sp, getCI("baseline_ins"), sp, getCI2("thirty_ins"), sp, getCI("gluc"), sp, sp, sp, sp, sp, sp, getCI2("ree_rq_ave"), sp, getCI2("ree_ree_ave"), sp, getCI("par_kcal_kg_day"))

ttt2 = cbind(ttv3, CI2)

dd = c(" Baseline"," 3 Months"," 6 Months"," 12 Months")
dd2 = c(" Baseline"," 6 Months"," 12 Months")
rownames(ttt2) = c("", "Weight (kg)", dd, "BMI (kg/m^2)", dd, "Fat (%)", dd2, "Waist circumference (cm)", dd, "LDL (mg/dL)", dd, "HDL (mg/dL)", dd, "Triglycerides (mg/dL)", dd, "Systolic BP (mmHg)", dd, "Diastolic BP (mmHg)", dd, "Fasting insulin (uu/mL)", dd, "Insulin-30 (uu/mL)", dd2, "Fasting glucose (mg/dL)", dd, "Metabolic syndrome (%)", dd, "RER", dd2, "REE", dd2, "TEE", dd)

word.doc(obj.list=list(ttt2), obj.title=c("eTable 3"),dest="/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Tables/eTable3_2017-11-28.docx", ftype="Arial", col.odd="white")


##eFigure 1: Waterfall plot with minor changes (shading)
##Two waterfall plots
dim(mrg)
dta.LC = mrg[mrg$diet=="Blue", ]
dta.LF = mrg[mrg$diet=="Purple", ]

dta.LC2 = dta.LC[order(dta.LC$change), ]
dta.LF2 = dta.LF[order(dta.LF$change), ]

pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/eFig1_2018-01-08_v1.pdf")
par(mfrow=c(1,2))
par(lwd = 1)
plot(x = c(0, 260), y=c(-35, 15), type="n", axes=F, xlab="", ylab="", main="(a) Healthy Low Fat")
abline(h=10, col="grey", lwd=0.5)
abline(h=0, lwd=1.5)
abline(h=-10, col="grey", lwd=0.5)
abline(h=-20, col="grey", lwd=0.5)
abline(h=-30, col="grey", lwd=0.5)
barplot(dta.LF2$change, col="grey",ylim=c(-35,15), xlim=c(0,260), xlab="", main="",ylab="12-month weight change (kg)", add=TRUE, axes=FALSE)
axis(2, at=c(-30, -20, -10, 0, 10), cex.axis=0.8, las=2)
mtext(text="n=215", side=1, at=130, cex=1, line=-0.6, font=3)

plot(x = c(0, 260), y=c(-35, 15), type="n", axes=F, xlab="", ylab="", main="(b) Healthy Low Carb")
abline(h=10, col="grey", lwd=0.5)
abline(h=0, lwd=1.5)
abline(h=-10, col="grey", lwd=0.5)
abline(h=-20, col="grey", lwd=0.5)
abline(h=-30, col="grey", lwd=0.5)
barplot(dta.LC2$change, col="grey",ylim=c(-35,15), xlim=c(0,260), xlab="", main="",ylab="12-month weight change (kg)", add=TRUE, axes=FALSE)
axis(2, at=c(-30, -20, -10, 0, 10), cex.axis=0.8, las=2)
mtext(text="n=221", side=1, at=130, cex=1, line=-0.6, font=3)
dev.off()

pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/eFig1_2018-01-08_v2.pdf")
par(mfrow=c(2,1))
par(lwd = 0.0001)
plot(x = c(0, 260), y=c(-35, 15), type="n", axes=F, xlab="", ylab="", main="(a) Healthy Low Fat")
abline(h=10, col="grey", lwd=0.5)
abline(h=0, lwd=1.5)
abline(h=-10, col="grey", lwd=0.5)
abline(h=-20, col="grey", lwd=0.5)
abline(h=-30, col="grey", lwd=0.5)
barplot(dta.LF2$change, col="grey",ylim=c(-35,15), xlim=c(0,260), xlab="", main="",ylab="12-month weight change (kg)", add=TRUE, axes=FALSE)
axis(2, at=c(-30, -20, -10, 0, 10), cex.axis=0.8, las=2)
mtext(text="n=215", side=1, at=130, cex=1, line=-0.6, font=3)

plot(x = c(0, 260), y=c(-35, 15), type="n", axes=F, xlab="", ylab="", main="(b) Healthy Low Carb")
abline(h=10, col="grey", lwd=0.5)
abline(h=0, lwd=1.5)
abline(h=-10, col="grey", lwd=0.5)
abline(h=-20, col="grey", lwd=0.5)
abline(h=-30, col="grey", lwd=0.5)
barplot(dta.LC2$change, col="grey",ylim=c(-35,15), xlim=c(0,260), xlab="", main="",ylab="12-month weight change (kg)", add=TRUE, axes=FALSE)
axis(2, at=c(-30, -20, -10, 0, 10), cex.axis=0.8, las=2)
mtext(text="n=221", side=1, at=130, cex=1, line=-0.6, font=3)
dev.off()


##eFigure 2a: Weight over time with error bars (95% CIs?) by genotype/diet (all 6? or just 4?)
names(d9) #LC_diet, LC_geno
table(d9$LF_LC_NG2, exclude=NULL)
d9$LF_LC_NG2[is.na(d9$LF_LC_NG2)] = "NG"
table(d9$LF_LC_NG2, exclude=NULL)

d9$IDg = paste(paste(d9$time, d9$diet, sep="_"), d9$LF_LC_NG2, sep="_")
table(d9$IDg)
k1 = aggregate(d9[, names(d9) %in% c("weight_gcrc")], by=list(d9$IDg), function(x) mean(x, na.rm=TRUE))
names(k1)[2] = "mean"
k1a = aggregate(d9[, names(d9) %in% c("weight_gcrc")], by=list(d9$IDg), function(x) length(x[!is.na(x)]))
names(k1a)[2] = "n"
k1b = aggregate(d9[, names(d9) %in% c("weight_gcrc")], by=list(d9$IDg), function(x) sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))
names(k1b)[2] = "sem"
g1 = merge(k1, k1a, by="Group.1", all.x=TRUE)
g2 = merge(g1, k1b, by="Group.1", all.x=TRUE)
g2$time = rep(c(1,2,3,4), each=6)
g2$time = g2$time+2*c(-0.05, -0.03, -0.01, 0.01, 0.03, 0.05)-1

library(Hmisc)
par(mfrow=c(1,1))
par(lwd=1)

pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/eFig2a_2018-01-08_v4.pdf")
plot(x = c(-0.5, 3.5), y=c(82, 104), type="n", axes=F, xlab="Time (months)", ylab="Weight (kg)")
#abline(h=10, col="grey", lwd=0.5)
lines(g2$time[c(1,7,13,19)], g2$mean[c(1,7,13,19)], type="l", lty=1, col="black")
points(g2$time[c(1,7,13,19)], g2$mean[c(1,7,13,19)], pch=18)
with(data=g2[c(1,7,13,19), ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5)) #add error bars

lines(g2$time[c(1,7,13,19)+1], g2$mean[c(1,7,13,19)+1], type="l", lty=2, col="black")
points(g2$time[c(1,7,13,19)+1], g2$mean[c(1,7,13,19)+1], pch=18)
with(data=g2[c(1,7,13,19)+1, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=2)) #add error bars

lines(g2$time[c(1,7,13,19)+2], g2$mean[c(1,7,13,19)+2], type="l", lty=3, col="black")
points(g2$time[c(1,7,13,19)+2], g2$mean[c(1,7,13,19)+2], pch=18)
with(data=g2[c(1,7,13,19)+2, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=3)) #add error bars

lines(g2$time[c(1,7,13,19)+3], g2$mean[c(1,7,13,19)+3], type="l", lty=1, col="grey")
points(g2$time[c(1,7,13,19)+3], g2$mean[c(1,7,13,19)+3], pch=18, col="grey")
with(data=g2[c(1,7,13,19)+3, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="grey")) #add error bars

lines(g2$time[c(1,7,13,19)+4], g2$mean[c(1,7,13,19)+4], type="l", lty=2, col="grey")
points(g2$time[c(1,7,13,19)+4], g2$mean[c(1,7,13,19)+4], pch=18, col="grey")
with(data=g2[c(1,7,13,19)+4, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=2, errbar.col="grey")) #add error bars

lines(g2$time[c(1,7,13,19)+5], g2$mean[c(1,7,13,19)+5], type="l", lty=3, col="grey")
points(g2$time[c(1,7,13,19)+5], g2$mean[c(1,7,13,19)+5], pch=18, col="grey")
with(data=g2[c(1,7,13,19)+5, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=3, errbar.col="grey")) #add error bars

axis(1, at=c(1,2,3,4)-1, c("0", "3", "6", "12"))
axis(2, at=seq(85, 100, 5), las=2)
box(which = "plot", bty = "l")
legend('topright', bty='n', c('HLC diet, LCG', 'HLC diet, LFG', 'HLC diet, NG', 'HLF diet, LCG', 'HLF diet, LFG', 'HLF diet, NG'), col=c(1,1,1,'grey', 'grey', 'grey'), lty=c(1,2,3, 1,2,3))
dev.off()

##95% CIs
w = qnorm(0.975)
pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/eFig2a_2018-01-10_CIs.pdf")
plot(x = c(-0.5, 3.5), y=c(82, 104), type="n", axes=F, xlab="Time (months)", ylab="Weight (kg)")
#abline(h=10, col="grey", lwd=0.5)
lines(g2$time[c(1,7,13,19)], g2$mean[c(1,7,13,19)], type="l", lty=1, col="black")
points(g2$time[c(1,7,13,19)], g2$mean[c(1,7,13,19)], pch=18)
with(data=g2[c(1,7,13,19), ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5)) #add error bars

lines(g2$time[c(1,7,13,19)+1], g2$mean[c(1,7,13,19)+1], type="l", lty=2, col="black")
points(g2$time[c(1,7,13,19)+1], g2$mean[c(1,7,13,19)+1], pch=18)
with(data=g2[c(1,7,13,19)+1, ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5, lty=2)) #add error bars

lines(g2$time[c(1,7,13,19)+2], g2$mean[c(1,7,13,19)+2], type="l", lty=3, col="black")
points(g2$time[c(1,7,13,19)+2], g2$mean[c(1,7,13,19)+2], pch=18)
with(data=g2[c(1,7,13,19)+2, ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5, lty=3)) #add error bars

lines(g2$time[c(1,7,13,19)+3], g2$mean[c(1,7,13,19)+3], type="l", lty=1, col="grey")
points(g2$time[c(1,7,13,19)+3], g2$mean[c(1,7,13,19)+3], pch=18, col="grey")
with(data=g2[c(1,7,13,19)+3, ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="grey")) #add error bars

lines(g2$time[c(1,7,13,19)+4], g2$mean[c(1,7,13,19)+4], type="l", lty=2, col="grey")
points(g2$time[c(1,7,13,19)+4], g2$mean[c(1,7,13,19)+4], pch=18, col="grey")
with(data=g2[c(1,7,13,19)+4, ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5, lty=2, errbar.col="grey")) #add error bars

lines(g2$time[c(1,7,13,19)+5], g2$mean[c(1,7,13,19)+5], type="l", lty=3, col="grey")
points(g2$time[c(1,7,13,19)+5], g2$mean[c(1,7,13,19)+5], pch=18, col="grey")
with(data=g2[c(1,7,13,19)+5, ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5, lty=3, errbar.col="grey")) #add error bars

axis(1, at=c(1,2,3,4)-1, c("0", "3", "6", "12"))
axis(2, at=seq(85, 100, 5), las=2)
box(which = "plot", bty = "l")
legend('topright', bty='n', c('HLC diet, LCG', 'HLC diet, LFG', 'HLC diet, NG', 'HLF diet, LCG', 'HLF diet, LFG', 'HLF diet, NG'), col=c(1,1,1,'grey', 'grey', 'grey'), lty=c(1,2,3, 1,2,3))
dev.off()



##With color
pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/eFig2a_2018-01-08_v4c.pdf")
plot(x = c(-0.5, 3.5), y=c(82, 104), type="n", axes=F, xlab="Time (months)", ylab="Weight (kg)")
#abline(h=10, col="grey", lwd=0.5)
lines(g2$time[c(1,7,13,19)], g2$mean[c(1,7,13,19)], type="l", lty=1, col="royalblue1")
points(g2$time[c(1,7,13,19)], g2$mean[c(1,7,13,19)], pch=18, col="royalblue1")
with(data=g2[c(1,7,13,19), ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, errbar.col="royalblue1")) #add error bars

lines(g2$time[c(1,7,13,19)+1], g2$mean[c(1,7,13,19)+1], type="l", lty=1, col="royalblue2")
points(g2$time[c(1,7,13,19)+1], g2$mean[c(1,7,13,19)+1], pch=18, col="royalblue2")
with(data=g2[c(1,7,13,19)+1, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="royalblue2")) #add error bars

lines(g2$time[c(1,7,13,19)+2], g2$mean[c(1,7,13,19)+2], type="l", lty=1, col="royalblue3")
points(g2$time[c(1,7,13,19)+2], g2$mean[c(1,7,13,19)+2], pch=18, col="royalblue3")
with(data=g2[c(1,7,13,19)+2, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="royalblue3")) #add error bars

lines(g2$time[c(1,7,13,19)+3], g2$mean[c(1,7,13,19)+3], type="l", lty=1, col="indianred1")
points(g2$time[c(1,7,13,19)+3], g2$mean[c(1,7,13,19)+3], pch=18, col="indianred1")
with(data=g2[c(1,7,13,19)+3, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="indianred1")) #add error bars

lines(g2$time[c(1,7,13,19)+4], g2$mean[c(1,7,13,19)+4], type="l", lty=1, col="indianred2")
points(g2$time[c(1,7,13,19)+4], g2$mean[c(1,7,13,19)+4], pch=18, col="indianred2")
with(data=g2[c(1,7,13,19)+4, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="indianred2")) #add error bars

lines(g2$time[c(1,7,13,19)+5], g2$mean[c(1,7,13,19)+5], type="l", lty=1, col="indianred3")
points(g2$time[c(1,7,13,19)+5], g2$mean[c(1,7,13,19)+5], pch=18, col="indianred3")
with(data=g2[c(1,7,13,19)+5, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="indianred3")) #add error bars

axis(1, at=c(1,2,3,4)-1, c("0", "3", "6", "12"))
axis(2, at=seq(85, 100, 5), las=2)
box(which = "plot", bty = "l")
legend('topright', bty='n', c('HLC diet, LCG', 'HLC diet, LFG', 'HLC diet, NG', 'HLF diet, LCG', 'HLF diet, LFG', 'HLF diet, NG'), col=c('royalblue1','royalblue2','royalblue3','indianred1', 'indianred2', 'indianred3'), lty=c(1,1,1,1,1,1))
dev.off()



##eFigure 2b: Weight over time with error bars (95% CIs) by insulin-30 tertile/diet (all 6)
names(d9) #LC_diet, LC_geno
table(d9$ins30, exclude=NULL)
d10 = d9[!is.na(d9$ins30), ]

d10$IDi = paste(paste(d10$time, d10$diet, sep="_"), d10$ins30, sep="_")
table(d10$IDi)
l1 = aggregate(d10[, names(d10) %in% c("weight_gcrc")], by=list(d10$IDi), function(x) mean(x, na.rm=TRUE))
names(l1)[2] = "mean"
l1a = aggregate(d10[, names(d10) %in% c("weight_gcrc")], by=list(d10$IDi), function(x) length(x[!is.na(x)]))
names(l1a)[2] = "n"
l1b = aggregate(d10[, names(d10) %in% c("weight_gcrc")], by=list(d10$IDi), function(x) sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))
names(l1b)[2] = "sem"
h1 = merge(l1, l1a, by="Group.1", all.x=TRUE)
h2 = merge(h1, l1b, by="Group.1", all.x=TRUE)
h2$time = rep(c(1,2,3,4), each=6)
h2$time = h2$time+2*c(-0.05, -0.03, -0.01, 0.01, 0.03, 0.05)-1

par(mfrow=c(1,1))
par(lwd=1)

pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/eFig2b_2018-01-08_v2.pdf")
plot(x = c(-0.5, 3.5), y=c(82, 104), type="n", axes=F, xlab="Time (months)", ylab="Weight (kg)")
#abline(h=10, col="grey", lwd=0.5)
lines(h2$time[c(1,7,13,19)], h2$mean[c(1,7,13,19)], type="l", lty=1, col="black")
points(h2$time[c(1,7,13,19)], h2$mean[c(1,7,13,19)], pch=18)
with(data=h2[c(1,7,13,19), ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5)) #add error bars

lines(h2$time[c(1,7,13,19)+1], h2$mean[c(1,7,13,19)+1], type="l", lty=2, col="black")
points(h2$time[c(1,7,13,19)+1], h2$mean[c(1,7,13,19)+1], pch=18)
with(data=h2[c(1,7,13,19)+1, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=2)) #add error bars

lines(h2$time[c(1,7,13,19)+2], h2$mean[c(1,7,13,19)+2], type="l", lty=3, col="black")
points(h2$time[c(1,7,13,19)+2], h2$mean[c(1,7,13,19)+2], pch=18)
with(data=h2[c(1,7,13,19)+2, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=3)) #add error bars

lines(h2$time[c(1,7,13,19)+3], h2$mean[c(1,7,13,19)+3], type="l", lty=1, col="grey")
points(h2$time[c(1,7,13,19)+3], h2$mean[c(1,7,13,19)+3], pch=18, col="grey")
with(data=h2[c(1,7,13,19)+3, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="grey")) #add error bars

lines(h2$time[c(1,7,13,19)+4], h2$mean[c(1,7,13,19)+4], type="l", lty=2, col="grey")
points(h2$time[c(1,7,13,19)+4], h2$mean[c(1,7,13,19)+4], pch=18, col="grey")
with(data=h2[c(1,7,13,19)+4, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=2, errbar.col="grey")) #add error bars

lines(h2$time[c(1,7,13,19)+5], h2$mean[c(1,7,13,19)+5], type="l", lty=3, col="grey")
points(h2$time[c(1,7,13,19)+5], h2$mean[c(1,7,13,19)+5], pch=18, col="grey")
with(data=h2[c(1,7,13,19)+5, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=3, errbar.col="grey")) #add error bars

axis(1, at=c(1,2,3,4)-1, c("0", "3", "6", "12"))
axis(2, at=seq(85, 100, 5), las=2)
box(which = "plot", bty = "l")
legend('topright', bty='n', c('HLC diet, Low Ins-30 Tertile', 'HLC diet, Mid Ins-30 Tertile', 'HLC diet, High Ins-30 Tertile', 'HLF diet, Low Ins-30 Tertile', 'HLF diet, Mid Ins-30 Tertile', 'HLF diet, High Ins-30 Tertile'), col=c(1,1,1,'grey', 'grey', 'grey'), lty=c(1,2,3, 1,2,3))
dev.off()


##With CIs
pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/eFig2b_2018-01-10_CIs.pdf")
plot(x = c(-0.5, 3.5), y=c(82, 104), type="n", axes=F, xlab="Time (months)", ylab="Weight (kg)")
#abline(h=10, col="grey", lwd=0.5)
lines(h2$time[c(1,7,13,19)], h2$mean[c(1,7,13,19)], type="l", lty=1, col="black")
points(h2$time[c(1,7,13,19)], h2$mean[c(1,7,13,19)], pch=18)
with(data=h2[c(1,7,13,19), ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5)) #add error bars

lines(h2$time[c(1,7,13,19)+1], h2$mean[c(1,7,13,19)+1], type="l", lty=2, col="black")
points(h2$time[c(1,7,13,19)+1], h2$mean[c(1,7,13,19)+1], pch=18)
with(data=h2[c(1,7,13,19)+1, ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5, lty=2)) #add error bars

lines(h2$time[c(1,7,13,19)+2], h2$mean[c(1,7,13,19)+2], type="l", lty=3, col="black")
points(h2$time[c(1,7,13,19)+2], h2$mean[c(1,7,13,19)+2], pch=18)
with(data=h2[c(1,7,13,19)+2, ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5, lty=3)) #add error bars

lines(h2$time[c(1,7,13,19)+3], h2$mean[c(1,7,13,19)+3], type="l", lty=1, col="grey")
points(h2$time[c(1,7,13,19)+3], h2$mean[c(1,7,13,19)+3], pch=18, col="grey")
with(data=h2[c(1,7,13,19)+3, ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="grey")) #add error bars

lines(h2$time[c(1,7,13,19)+4], h2$mean[c(1,7,13,19)+4], type="l", lty=2, col="grey")
points(h2$time[c(1,7,13,19)+4], h2$mean[c(1,7,13,19)+4], pch=18, col="grey")
with(data=h2[c(1,7,13,19)+4, ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5, lty=2, errbar.col="grey")) #add error bars

lines(h2$time[c(1,7,13,19)+5], h2$mean[c(1,7,13,19)+5], type="l", lty=3, col="grey")
points(h2$time[c(1,7,13,19)+5], h2$mean[c(1,7,13,19)+5], pch=18, col="grey")
with(data=h2[c(1,7,13,19)+5, ], expr = errbar(time, mean, mean+w*sem, mean-w*sem, add=T, pch="", cap=0, lwd=1.5, lty=3, errbar.col="grey")) #add error bars

axis(1, at=c(1,2,3,4)-1, c("0", "3", "6", "12"))
axis(2, at=seq(85, 100, 5), las=2)
box(which = "plot", bty = "l")
legend('topright', bty='n', c('HLC diet, Low Ins-30 Tertile', 'HLC diet, Mid Ins-30 Tertile', 'HLC diet, High Ins-30 Tertile', 'HLF diet, Low Ins-30 Tertile', 'HLF diet, Mid Ins-30 Tertile', 'HLF diet, High Ins-30 Tertile'), col=c(1,1,1,'grey', 'grey', 'grey'), lty=c(1,2,3, 1,2,3))
dev.off()



##With colors
pdf("/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Figures/eFig2b_2018-01-08_v2c.pdf")
plot(x = c(-0.5, 3.5), y=c(82, 104), type="n", axes=F, xlab="Time (months)", ylab="Weight (kg)")
#abline(h=10, col="grey", lwd=0.5)
lines(h2$time[c(1,7,13,19)], h2$mean[c(1,7,13,19)], type="l", lty=1, col="royalblue1")
points(h2$time[c(1,7,13,19)], h2$mean[c(1,7,13,19)], pch=18, col="royalblue1")
with(data=h2[c(1,7,13,19), ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, errbar.col="royalblue1")) #add error bars

lines(h2$time[c(1,7,13,19)+1], h2$mean[c(1,7,13,19)+1], type="l", lty=1, col="royalblue2")
points(h2$time[c(1,7,13,19)+1], h2$mean[c(1,7,13,19)+1], pch=18, col="royalblue2")
with(data=h2[c(1,7,13,19)+1, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="royalblue2")) #add error bars

lines(h2$time[c(1,7,13,19)+2], h2$mean[c(1,7,13,19)+2], type="l", lty=1, col="royalblue3")
points(h2$time[c(1,7,13,19)+2], h2$mean[c(1,7,13,19)+2], pch=18, col="royalblue3")
with(data=h2[c(1,7,13,19)+2, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="royalblue3")) #add error bars

lines(h2$time[c(1,7,13,19)+3], h2$mean[c(1,7,13,19)+3], type="l", lty=1, col="indianred1")
points(h2$time[c(1,7,13,19)+3], h2$mean[c(1,7,13,19)+3], pch=18, col="indianred1")
with(data=h2[c(1,7,13,19)+3, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="indianred1")) #add error bars

lines(h2$time[c(1,7,13,19)+4], h2$mean[c(1,7,13,19)+4], type="l", lty=1, col="indianred2")
points(h2$time[c(1,7,13,19)+4], h2$mean[c(1,7,13,19)+4], pch=18, col="indianred2")
with(data=h2[c(1,7,13,19)+4, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="indianred2")) #add error bars

lines(h2$time[c(1,7,13,19)+5], h2$mean[c(1,7,13,19)+5], type="l", lty=1, col="indianred3")
points(h2$time[c(1,7,13,19)+5], h2$mean[c(1,7,13,19)+5], pch=18, col="indianred3")
with(data=h2[c(1,7,13,19)+5, ], expr = errbar(time, mean, mean+sem, mean-sem, add=T, pch="", cap=0, lwd=1.5, lty=1, errbar.col="indianred3")) #add error bars

axis(1, at=c(1,2,3,4)-1, c("0", "3", "6", "12"))
axis(2, at=seq(85, 100, 5), las=2)
box(which = "plot", bty = "l")
legend('topright', bty='n', c('HLC diet, Low Ins-30 Tertile', 'HLC diet, Mid Ins-30 Tertile', 'HLC diet, High Ins-30 Tertile', 'HLF diet, Low Ins-30 Tertile', 'HLF diet, Mid Ins-30 Tertile', 'HLF diet, High Ins-30 Tertile'), col=c('royalblue1','royalblue2','royalblue3','indianred1', 'indianred2', 'indianred3'), lty=c(1,1,1, 1,1,1))
dev.off()


##Save sample sizes for the captions
write.csv(g2, "/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Sample_size_DxG.csv", row.names=TRUE)
write.csv(h2, "/Users/jrigdon/Box Sync/Rigdon/Gardner/JAMA_submission/Revision/Data/Sample_size_DxI.csv", row.names=TRUE)





















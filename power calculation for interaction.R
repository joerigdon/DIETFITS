# Power calculation for interaction

# gene by diet analysis
genotype<-c(rep(0,100),rep(1,100),rep(0,100),rep(1,100))
treat<-c(rep(0,200),rep(1,200))
id<-seq(1:400)
beta0<-2
beta1<-0
beta2<-0
beta3<-7
pvalue<-numeric(0)
for (i in 1:100)
{
weightchange<-beta0 + beta1*treat + beta2*genotype + beta3*treat*genotype + rnorm(400,0,13)

thedata<-cbind.data.frame(id,genotype,treat,weightchange)

themodel<-lm(weightchange~treat+genotype+treat*genotype,data=thedata)
pvalue<-c(pvalue,summary(themodel)$coef[4,4])
print(i)
}

power<-length(pvalue[pvalue < 0.05])

# gene by diet analysis v2 (n=600)
genotype<-c(rep(0,150),rep(1,150),rep(0,150),rep(1,150))
treat<-c(rep(0,300),rep(1,300))
id<-seq(1:600)
beta0<-2
beta1<-0
beta2<-0
beta3<-7
pvalue<-numeric(0)
for (i in 1:100)
{
weightchange<-beta0 + beta1*treat + beta2*genotype + beta3*treat*genotype + rnorm(600,0,13)

thedata<-cbind.data.frame(id,genotype,treat,weightchange)

themodel<-lm(weightchange~treat+genotype+treat*genotype,data=thedata)
pvalue<-c(pvalue,summary(themodel)$coef[4,4])
print(i)
}

power<-length(pvalue[pvalue < 0.05])



# We have 80% power to detect clinically meaningful interaction effects. For example, if the treatment effect only impacts those on the LC genotype correctly matched to dietary group enabling a weight change of 7 lbs at 12 months, we will detect this with 80% power. This assumes that weight change at 12 months is normally distributed with a standard deviation of change of 13 lbs, a type I error rate of 0.05, a two-sided Wald test, and approximately 100 subjects in each of the 4 groups.

# insulin by diet analysis
is<-rnorm(400,10,5)
treat<-c(rep(0,200),rep(1,200))
id<-seq(1:400)
beta0<-2
beta1<-0
beta2<-0
beta3<--.8
pvalue<-numeric(0)
for (i in 1:100)
{
weightchange<-beta0 + beta1*treat + beta2*is + beta3*treat*is + rnorm(400,0,13)

thedata<-cbind.data.frame(id,is,treat,weightchange)

themodel<-lm(weightchange~treat+is+treat*is,data=thedata)
pvalue<-c(pvalue,summary(themodel)$coef[4,4])
print(i)
}

power<-length(pvalue[pvalue < 0.05])

# We have over 80% power to detect clinically meaningful interaction effects between dietary assignment and insulin sensitivity. For example, if folks gain on average 2 pounds at the end of year regardless of treatment assignment, and assume that for every 1-unit increase of insulin sensitivity, subjects on the Low Carb diet lose 8/10ths of a pound more at 12 months, we have over 80% power to detect such differences. This assumes that weight change at 12 months is normally distributed with a standard deviation of change of 13 lbs, a type I error rate of 0.05, a two-sided Wald test, and approximately 200 subjects in the LC and LF groups, respectively.









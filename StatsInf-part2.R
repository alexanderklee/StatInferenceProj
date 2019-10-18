data("ToothGrowth")

names(ToothGrowth)
levels(ToothGrowth$supp)


table(ToothGrowth$dose)
table(ToothGrowth$supp)

summary(ToothGrowth)

cor(ToothGrowth[sapply(ToothGrowth, is.numeric)])

p1 <- ggplot(ToothGrowth, aes(x =factor(dose), y = len, fill = factor(dose)))
p1 + geom_boxplot() + xlab("Dosage") + ylab("Length")

p1
Interval <- (mean(ToothGrowth$len) + c(-1, 1) * qnorm(0.975) * sd(ToothGrowth$len)/sqrt(length(ToothGrowth$len)))

 
t.test(len~supp, data=ToothGrowth)
 t.test(len~supp, mu=0, alt="two.sided", var.eq=F, paired=F, conf=0.95, data=ToothGrowth)


dose0_5 <- subset(ToothGrowth, dose == 0.5)
dose1   <- subset(ToothGrowth, dose == 1)
dose2   <- subset(ToothGrowth, dose == 2)

oj <- subset(ToothGrowth, supp=="OJ")
vc <- subset(ToothGrowth, supp=="VC")
t.test(oj$len, vc$len, var.eq=T)

oj0_5 <- subset(oj, dose==0.5)
oj1 <- subset(oj, dose==1)
oj2 <- subset(oj, dose==2)

vc0_5 <- subset(oj, dose==0.5)
vc1 <- subset(oj, dose==0.5)
vc2 <- subset(oj, dose==0.5)

t.test(oj0_5$len, vc0_5$len, var.eq=T)
t.test(oj1$len, vc1$len, var.eq=T)
t.test(oj2$len, vc2$len, var.eq=T)

t.test(len~supp, mu=0, alt="two.sided", var.eq=F, paired=F, , conf=0.95, data=dose0_5) 
t.test(len~supp, mu=0, alt="two.sided", var.eq=F, paired=F, , conf=0.95,data=dose1) 
t.test(len~supp, mu=0, alt="two.sided", var.eq=F, paired=F, , conf=0.95,data=dose2) 


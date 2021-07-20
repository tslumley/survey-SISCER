## using same design object created in session 1

summary(svyglm(neckpain~age_p, design=nhis, family=quasibinomial))
summary(svyglm(neckpain~sex, design=nhis, family=quasibinomial))
summary(svyglm(neckpain~bmi, design=nhis, family=quasibinomial))
summary(svyglm(neckpain~sickleave, design=nhis, family=quasibinomial))

summary(svyglm(backpain~age_p, design=nhis, family=quasibinomial))
summary(svyglm(backpain~sex, design=nhis, family=quasibinomial))
summary(svyglm(backpain~bmi, design=nhis, family=quasibinomial))
summary(svyglm(backpain~sickleave, design=nhis, family=quasibinomial))

svyboxplot(age_p~interaction(sex,sickleave,backpain), design=nhis)
svyboxplot(age_p~interaction(sex,sickleave,neckpain), design=nhis)

model0<-svyglm(neckpain~age_p+bmi+sex+sickleave, design=nhis, family=quasibinomial)
library(splines)
model1<-svyglm(neckpain~ns(age_p,4)+ns(bmi,4)+sex+sickleave, design=nhis, family=quasibinomial)
anova(model1,model0)

summary(model0)
summary(model1)

byage<-svyby(~neckpain,~age_p, svymean, na.rm=TRUE, design=nhis)
plot(byage$age_p, byage$neckpainyes)

model2<-svyglm(neckpain~pmin(age_p,50)+pmax(age_p,50)+sex+sickleave, design=nhis, family=quasibinomial)

model3<-svyglm(neckpain~(pmin(age_p,50)+pmax(age_p,50))*(sex+sickleave), design=nhis, family=quasibinomial)

summary(model2)
model2a<-svyglm(neckpain~pmin(age_p,50)+pmax(age_p,50)+I(age_p==85)+sex+sickleave, design=nhis, family=quasibinomial)
summary(model2a)

byage2<-svyby(~backpain,~age_p, svymean, na.rm=TRUE, design=nhis)
plot(log(byage2$age_p), byage2$backpainyes)
bmodel2<-svyglm(backpain~log(age_p)+sex+sickleave, design=nhis, family=quasibinomial)
summary(bmodel2)
bmodel3<-svyglm(backpain~log(age_p)*(sex+sickleave), design=nhis, family=quasibinomial)
summary(bmodel3)
bmodel4<-svyglm(backpain~log(age_p)+(sex+sickleave)+I(age_p==85), design=nhis, family=quasibinomial)
summary(bmodel4)

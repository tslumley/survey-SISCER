#asssuming the design object created earlier in session 1

grep("age",names(nhis_data), value=TRUE)
grep("sex",names(nhis_data), value=TRUE)

summary(svyglm(sleep~age_p, design=nhis))
summary(svyglm(sleep~sex, design=nhis))

summary(svyglm(sleep~sex*age_p, design=nhis))

svyplot(sleep~age_p,design=nhis, style="trans", pch=19)

summary(nhis_data$sleep)
with(nhis_data, table(sleep[sleep>24]))


nhis<-update(nhis, sleep=ifelse(sleep>24,NA,sleep))     
svyplot(sleep~age_p,design=nhis, style="trans", pch=19)
svyplot(sleep~age_p,design=nhis, style="hex") #ick

summary(nhis_data$age_p)
table(nhis_data$sex)

svyplot(sleep~age_p,design=nhis, style="trans", pch=19)
age_smth<-svysmooth(sleep~age_p, design=nhis)
lines(age_smth,col="orange",lwd=4)


f_smth<-svysmooth(sleep~age_p, design=subset(nhis,sex=="2 Female"))
m_smth<-svysmooth(sleep~age_p, design=subset(nhis,sex=="1 Male"))
svyplot(sleep~age_p,design=nhis, style="trans", pch=19)
lines(f_smth,col="orange",lwd=4)
lines(m_smth,col="purple",lwd=4)

f_smth<-svysmooth(sleep~age_p, design=subset(nhis,sex=="2 Female"), bandwidth=5)
m_smth<-svysmooth(sleep~age_p, design=subset(nhis,sex=="1 Male"),bandwidth=5)
svyplot(sleep~age_p,design=nhis, style="trans", pch=19)
lines(f_smth,col="orange",lwd=4)
lines(m_smth,col="purple",lwd=4)

nhis<-update(nhis,
             agelo=pmin(30,age_p),
             agemid=pmin(55,pmax(30,age_p)),
             agehi=pmax(55,age_p)
             )
summary(svyglm(sleep~sex*(agelo+agemid+agehi), design=nhis))


svyttest(sleep~sex, design=nhis)
svyranktest(sleep~sex, design=nhis)
svyranktest(sleep~sex, design=nhis,test="median")

svyttest(sleep~backpain, design=nhis)
svyranktest(sleep~backpain, design=nhis)
svyranktest(sleep~backpain, design=nhis,test="median")
svyboxplot(sleep~backpain, design=nhis,all.outliers=TRUE)


svyciprop(~backpain, design=nhis)
svyciprop(~backpain, design=nhis,method="likelihood")
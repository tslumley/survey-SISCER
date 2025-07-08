#install.packages("survey")
library(survey)
setwd("/Users/tlum005/TEACHING/MILANO/SeattleSummer2021/Course Materials")
load("nhis_data.rda")

nhis<-svydesign(id=~psu_p, weight=~wtfa_sa, strata=~strat_p, data=nhis_data)

nhis<-svydesign(id=~psu_p, weight=~wtfa_sa, strata=~strat_p, data=nhis_data,nest=TRUE)


nhis<-update(nhis, sickleave=factor(ifelse(pdsicka=="1 Yes","yes","no")))
nhis<-update(nhis, backpain=factor(ifelse(painlb=="1 Yes","yes","no")))
nhis<-update(nhis, neckpain=factor(ifelse(paineck=="1 Yes","yes","no")))

svytotal(~sickleave, nhis, na.rm=TRUE)
svytotal(~neckpain, nhis)
svytotal(~backpain, nhis)

table(nhis_data$painlb)
table(nhis_data$painneck)

library(XLConnect)
loan<-readWorksheetFromFile('/ADS/Assignment/Assignment02/loan.xlsx', sheet=1)

help("contrasts")
Res_status <- as.factor(loan$Res_status)
Occupation <- as.factor(loan$Occupation)
Job_status <-as.factor(loan$Job_status)
Liab_ref <- as.factor(loan$Liab_ref)
Acc_ref <- as.factor(loan$Acc_ref)
Decision <- as.factor(loan$Decision)



contrasts(Res_status)
contrasts(Occupation)
contrasts(Job_status)
contrasts(Liab_ref)
contrasts(Acc_ref)
contrasts(Decision)

x<- data.frame(Res_status, Occupation , Job_status, Liab_ref , Acc_ref )
loan.fit <- glm(Decision ~ Res_status+ Occupation + Job_status+ Liab_ref + Acc_ref, family = "binomial", data = x )
summary(loan.fit)


newdata1 = data.frame(Res_status="owner", Occupation="creative_" , Job_status = "governmen", Liab_ref="f", Acc_ref = "given")
predict(loan.fit, newdata1, type = "response")

newdata2 = data.frame(Res_status="rent", Occupation="creative_" , Job_status = "governmen", Liab_ref="f", Acc_ref = "given")
predict(loan.fit, newdata2, type = "response")

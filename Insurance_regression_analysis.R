rm(list = ls())
library(rio)
insurance_df=import("insurance.csv")
colnames(insurance_df)=tolower(make.names(colnames(insurance_df)))
attach(insurance_df)


insurance_df_sample = insurance_df[sample(1:nrow(insurance_df),100),]
insurance_df_sample.
attach(insurance_df_sample)

smoker = as.factor(smoker)

df_sample_1.out = lm(charges~age)
summary(df_sample_1.out)

df_sample_2.out = lm(charges~bmi)
summary(df_sample_2.out)

df_sample_3.out = lm(charges~smoker)
summary(df_sample_3.out)

df_sample_4.out = lm(charges~age+bmi)
summary(df_sample_4.out)

df_sample_5.out = lm(charges~bmi+smoker)
summary(df_sample_5.out)


df_sample_6.out = lm(charges~age+smoker)
summary(df_sample_6.out)


df_sample_7.out = lm(charges~bmi+age+I(bmi*age))
summary(df_sample_7.out)

df_sample_8.out = lm(charges~age+I(age*age))
summary(df_sample_8.out)

df_sample_9.out = lm(charges~bmi+I(bmi*bmi))
summary(df_sample_9.out)

df_sample_11.out = lm(charges~bmi+age+smoker)
summary(df_sample_11.out)


df_sub_destination_sample_continious.out = lm(charges~age+bmi+smoker)
summary(df_sub_destination_sample_continious.out)

#best model is df_sub_destination_sample_continious.out

#Line assumptions for best model lm(charges~age+bmi+smoker)

#Linearity
plot(df_sub_destination_sample_continious$charges,df_sub_destination_sample_continious.out$fitted.values,pch=20)
abline(0,1,lwd=3, col="red")

#Normality
qqnorm(df_sub_destination_sample_continious.out$residuals,pch=20)
qqline(df_sub_destination_sample_continious.out$residuals,col="red",lwd=3)

#Equality of variances

plot(df_sub_destination_sample_continious.out$fitted.values,df_sub_destination_sample_continious.out$residuals,pch=20)
abline(0,0,lwd=3,col="red")







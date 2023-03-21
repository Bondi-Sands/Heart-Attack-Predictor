

#Import data to R
library(readr)
library(tidyr)
library(ggplot2)
library(outliers)
library(dplyr)
library(tidymodels)
library(MASS)
library(viridis)
library(gridExtra)


Heart <- read_csv("~/GitHub projects/1st repository power BI heart attack predictor/heart.csv")
o2Saturation <- read_csv("~/GitHub projects/1st repository power BI heart attack predictor/o2Saturation.csv")

head(Heart)

#Data Tidying, Summary and Data Type conversion

HAP_data <-Heart %>%
  rename(Chest_pain = cp,
         Resting_BP_mmHg = trtbps,
         Cholesterol_mg_dl = chol,
         Max_heart_rate_achieved = thalachh,
         Fasting_Blood_sugar = fbs,
         Suspected_left_Main_Coronary_Artery_Disease = slp,
         Cardinal_Anterior_Aspect_Score = caa,
         Resting_ECG = restecg,
         Thallasemia = thall,
         Exercise_Induced_Angina = exng,
         Diagnosis_Heart_Attack = output)%>%
  mutate(sex=recode_factor(sex,`0`="Female",
                           `1` = "Male"),
         Suspected_left_Main_Coronary_Artery_Disease = recode_factor(Suspected_left_Main_Coronary_Artery_Disease, `0` = "No Heart Attack",
                                                                     `1` = "Non-ST elevation Myocardial Infarction (NSTEMI)",
                                                                     `2` = "ST Elevation myocardial Infarction (STEMI)"),
         Cardinal_Anterior_Aspect_Score = recode_factor(Cardinal_Anterior_Aspect_Score, `0` = "No Heart Attack",
                                        `1` = "Mild Heart Attack",
                                        `2` = "Moderate Heart Attack",
                                        `3` =  "Severe Heart Attack",
                                        `4` = "Most Severe Heart Attack"),
         Chest_pain = recode_factor(Chest_pain, `0` = "No Pain", 
                                    `1` = "Typical",
                                    `2` = "Atypical",
                                    `3` = "Non-angina",
                                    `4` = "Asymptomatic"),
         Fasting_Blood_sugar = recode_factor(Fasting_Blood_sugar, `0` = "<=120mg/dl",
                                             `1` = ">120mg/dl"),
         Resting_ECG = recode_factor(Resting_ECG, `0` = "normal",
                                     `1` = "ST-T abnormality",
                                     `2` = "LV hypertrophy"),
         Exercise_Induced_Angina = recode_factor(Exercise_Induced_Angina, `0` = "no",
                                                 `1` = "yes"),
         Thallasemia = recode_factor(Thallasemia, `0` = "Thallasemia_minor",
                                     `1` = "Thallassemia Intermedia",
                                     `2` = "Thallassemia Major",
                                     `3` = "Alpha Thallassemia"),
        Diagnosis_Heart_Attack = recode_factor(Diagnosis_Heart_Attack, `0` = "Low",
                                                `1` = "High",.default = "NA"))
         
HAP_data$ Max_heart_rate_achieved = ifelse(Heart$thalachh < (as.numeric(220) - as.numeric(Heart$age))*0.64, "Low", 
                                 ifelse(Heart$thalachh >= (as.numeric(220) - as.numeric(Heart$age))*0.64 & Heart$thalachh < (as.numeric(220) - as.numeric(Heart$age))*0.76, "Normal", 
                                        "High"))
Heart$MHRA = ifelse(Heart$thalachh < (as.numeric(220) - as.numeric(Heart$age))*0.64, "0", 
                    ifelse(Heart$thalachh >= (as.numeric(220) - as.numeric(Heart$age))*0.64 & Heart$thalachh < (as.numeric(220) - as.numeric(Heart$age))*0.76, "1", 
                           "2"))
head( Max_heart_rate_achieved)

head(Heart)

summary(HAP_data)
class(HAP_data)

Heart_df <- as.data.frame(Heart)
HAP_df <-as.data.frame(HAP_data)
class(HAP_df)
head(HAP_df)

#Check for missing values

HAP_data_mv <- apply(HAP_data, 2, function(x) sum(is.na(x)))
View(HAP_data_mv)


#outliers and exploring relationships

outlier.plot <-ggplot(HAP_df, mapping = aes(x = age, fill = Diagnosis_Heart_Attack)) +
  geom_bar(position = "dodge")+
  labs(title = "Diagnosis of Heart Attack across age", x = "Age", y = "Diagnosis of Heart Attack", fill = "Heart Attack")
plot(outlier.plot)


outlier.oldpeak <- ggplot(data = Heart,
          aes(x=as.factor(output), y = oldpeak, fill = as.factor(output)))+
  geom_boxplot(alpha = 0.5,
               notch = TRUE,notchwidth = 0.5,
               outlier.color = "red",
               outlier.shape = 8,
               outlier.size =3)+
  stat_boxplot(geom = "errorbar", width = 0.3)+
  xlab("Diagnosis of Heart Attack")+
  ylab("OldPeak")
outlier.oldpeak

outlier.Resting_BP <- ggplot(data = Heart,
                          aes(x=as.factor(output), y = trtbps,fill = as.factor(output)))+
  geom_boxplot(alpha = 0.5,
               notch = TRUE,notchwidth = 0.5,
               outlier.color = "Blue",
               outlier.shape = 9,
               outlier.size =3)+
  stat_boxplot(geom = "errorbar", width = 0.3)+
  xlab("Diagnosis of Heart Attack")+
  ylab("Exercise Induced Angina")
outlier.Resting_BP

outlier.cholesterol <- ggplot(data = Heart,
                             aes(x=as.factor(output), y = chol,fill = as.factor(output)))+
  geom_boxplot(alpha = 0.5,
               notch = TRUE,notchwidth = 0.5,
               outlier.color = "Blue",
               outlier.shape = 7,
               outlier.size =3)+
  stat_boxplot(geom = "errorbar", width = 0.3)+
  xlab("Diagnosis of Heart Attack")+
  ylab("Cholesterol(mg/dl)")
outlier.cholesterol

grid.arrange(outlier.oldpeak,outlier.Resting_BP,outlier.cholesterol,ncol=3)


HAP_sig_test <- table(Heart$output,
                   Heart$sex, 
                   Heart$fbs,
                   Heart$restecg,
                   Heart$cp,
                   Heart$thall,
                   Heart$MHRA,
                   Heart$oldpeak,
                   Heart$caa) 

HHAP_chisq <- chisq.out.test(HAP_sig_test)
HHAP_chisq
HHAP_chisq_opp <- chisq.out.test(HAP_sig_test,opposite = TRUE)
HHAP_chisq_opp

Heart_sig_test <-table(Heart$output,
                       Heart$age,
                       Heart$chol,
                       Heart$trtbps,
                       Heart$thalachh)

lower_bound <-quantile(Heart_sig_test,0.25)
  lower_bound

upper_bound <-quantile(Heart_sig_test,0.75)
  upper_bound
  
outlier_test <- which(Heart_sig_test<lower_bound| Heart_sig_test>upper_bound)
HAP_df[HAP_df$PatientID == outlier_test,]
  
#Model
Heart_model <- filter(Heart, !PatientID %in% c(86))
HAP_split <-initial_split(Heart_model, prob = c(0.8,0.2))
HAP_train <- training(HAP_split)
HAP_test <- testing(HAP_split)

head(HAP_train)
head(HAP_test)

linear_regression <-lm(output ~ age + trtbps+ chol+ thalachh + MHRA + thall + cp, data = HAP_train)
linear_regression
summary(linear_regression)

#The adjusted R-squared of the model is 0.3013, indicating that about 30% of the variation in the output can be explained by the model. 
#The F-statistic of 13.13 and the p-value of <2.2e-15 indicate that the model is statistically significant.

logistic_regression <-glm(output ~ sex + cp + oldpeak + caa + chol + exng + thall, data = HAP_train, family = 'binomial')
logistic_regression
summary(logistic_regression)

HAP_model <-stepAIC(logistic_regression)
summary(HAP_model)

HAP_prediction <- predict(HAP_model, newdata = HAP_test, type = "response")
HAP_prediction <- ifelse(HAP_prediction >= 0.5, 1, 0)


accuracy <- mean(HAP_prediction == HAP_test$output)
print(paste("Accuracy:", accuracy))

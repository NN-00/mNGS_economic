### GLM model analysis ###
library(readxl)
library(stats)

df <- read_excel("00-Final-dataset-mNGS_economic.xlsx", sheet = "Sheet1")


df$group <- ifelse(df$label == 0, 1, 0) 
df$sex_code <- ifelse(df$sex == "male", 1, 0)
df$in_ICU <- ifelse(!is.na(df$ICU_date), 1, 0)

model_cost <- glm(cost ~ group + age + sex_code + Hypertension + 
                    Hyperglycemia + Hyperlipidemia + Coronary_artery_disease + 
                    COPD + Tumor + fever + cough + chest_pain, 
                  family = stats::Gamma(link = "log"), 
                  data = df)

print("--- Summary for Cost Model ---")
print(summary(model_cost))

model_los <- glm(hospitalization_length ~ group + age + sex_code + Hypertension + 
                   Hyperglycemia + Hyperlipidemia + Coronary_artery_disease + 
                   COPD + Tumor + fever + cough + chest_pain,  
                 family = stats::Gamma(link = "log"), 
                 data = df)

print("--- Summary for Length of Stay Model ---")
print(summary(model_los))

print("--- Wilcoxon Test for Cost ---")
wilcox.test(cost ~ group, data = df, conf.int = TRUE, conf.level = 0.95)
## DATA MODELING ##

summary(data)

## models using one variable 

# un transformed variables
lm_food = lm(Sub_Prop~Food, data=data)
sqrt(mean(lm_food$resid^2)) # 0.16
BIC(lm_food) 
summary(lm_food)

lm_healthcare = lm(Sub_Prop~Healthcare, data=data)
sqrt(mean(lm_healthcare$resid^2)) # 0.16
BIC(lm_healthcare) 
summary(lm_healthcare)

lm_other = lm(Sub_Prop~Other, data=data)
sqrt(mean(lm_other$resid^2)) # 0.16
BIC(lm_other) 
summary(lm_other)

lm_na = lm(Sub_Prop~N_A, data=data)
sqrt(mean(lm_na$resid^2)) # 0.16
BIC(lm_na) 
summary(lm_na)

lm_work = lm(Sub_Prop~Work, data=data)
sqrt(mean(lm_work$resid^2)) # 0.16
BIC(lm_work) 
summary(lm_work)

lm_education = lm(Sub_Prop~Education, data=data)
sqrt(mean(lm_education$resid^2)) # 0.16
BIC(lm_education) 
summary(lm_education)

lm_retail = lm(Sub_Prop~Retail, data=data)
sqrt(mean(lm_retail$resid^2)) # 0.16
BIC(lm_retail) 
summary(lm_retail)

lm_recreation = lm(Sub_Prop~Recreation, data=data)
sqrt(mean(lm_recreation$resid^2)) # 0.16
BIC(lm_recreation) 
summary(lm_recreation)


# untransformed prop, percent business
lm_pfood = lm(Sub_Prop~p_food, data=data)
sqrt(mean(lm_pfood$resid^2)) # 0.16
BIC(lm_pfood) 
summary(lm_pfood)

lm_phealthcare = lm(Sub_Prop~p_healthcare, data=data)
sqrt(mean(lm_phealthcare$resid^2)) # 0.16
BIC(lm_phealthcare) 
summary(lm_phealthcare)

lm_pother = lm(Sub_Prop~p_other, data=data)
sqrt(mean(lm_pother$resid^2)) # 0.16
BIC(lm_pother) 
summary(lm_pother)

lm_pna = lm(Sub_Prop~p_n_a, data=data)
sqrt(mean(lm_pna$resid^2)) # 0.16
BIC(lm_pna) 
summary(lm_pna)

lm_pwork = lm(Sub_Prop~p_work, data=data)
sqrt(mean(lm_pwork$resid^2)) # 0.16
BIC(lm_pwork) 
summary(lm_pwork)

lm_peducation = lm(Sub_Prop~p_education, data=data)
sqrt(mean(lm_peducation$resid^2)) # 0.16
BIC(lm_peducation) 
summary(lm_peducation)

lm_pretail = lm(Sub_Prop~p_retail, data=data)
sqrt(mean(lm_pretail$resid^2)) # 0.16
BIC(lm_pretail) 
summary(lm_pretail)

lm_precreation = lm(Sub_Prop~p_recreation, data=data)
sqrt(mean(lm_precreation$resid^2)) # 0.16
BIC(lm_precreation) 
summary(lm_precreation)

# logit prop, percent business
lm_pfoodl = lm(Logit_Prop~p_food, data=data)
sqrt(mean(lm_pfoodl$resid^2)) # 0.16
BIC(lm_pfoodl) 
summary(lm_pfoodl)

lm_phealthcarel = lm(Logit_Prop~p_healthcare, data=data)
sqrt(mean(lm_phealthcarel$resid^2)) # 0.16
BIC(lm_phealthcarel) 
summary(lm_phealthcarel)

lm_potherl = lm(Logit_Prop~p_other, data=data)
sqrt(mean(lm_potherl$resid^2)) # 0.16
BIC(lm_potherl) 
summary(lm_potherl)

lm_pnal = lm(Logit_Prop~p_n_a, data=data)
sqrt(mean(lm_pnal$resid^2)) # 0.16
BIC(lm_pnal) 
summary(lm_pnal)

lm_pworkl = lm(Logit_Prop~p_work, data=data)
sqrt(mean(lm_pworkl$resid^2)) # 0.16
BIC(lm_pworkl) 
summary(lm_pworkl)

lm_peducationl = lm(Logit_Prop~p_education, data=data)
sqrt(mean(lm_peducationl$resid^2)) # 0.16
BIC(lm_peducationl) 
summary(lm_peducationl)

lm_pretaill = lm(Logit_Prop~p_retail, data=data)
sqrt(mean(lm_pretaill$resid^2)) # 0.16
BIC(lm_pretaill) 
summary(lm_pretaill)

lm_precreationl = lm(Logit_Prop~p_recreation, data=data)
sqrt(mean(lm_precreationl$resid^2)) # 0.16
BIC(lm_precreationl) 
summary(lm_precreationl)


# logit prop, untransformed business
lm_foodl = lm(Logit_Prop~Food, data=data)
sqrt(mean(lm_foodl$resid^2)) # 0.16
BIC(lm_foodl) 
summary(lm_foodl)

lm_healthcarel = lm(Logit_Prop~Healthcare, data=data)
sqrt(mean(lm_healthcarel$resid^2)) # 0.16
BIC(lm_healthcarel) 
summary(lm_healthcarel)

lm_otherl = lm(Logit_Prop~Other, data=data)
sqrt(mean(lm_otherl$resid^2)) # 0.16
BIC(lm_otherl) 
summary(lm_otherl)

lm_nal = lm(Logit_Prop~N_A, data=data)
sqrt(mean(lm_nal$resid^2)) # 0.16
BIC(lm_nal) 
summary(lm_nal)

lm_workl = lm(Logit_Prop~Work, data=data)
sqrt(mean(lm_workl$resid^2)) # 0.16
BIC(lm_workl) 
summary(lm_workl)

lm_educationl = lm(Logit_Prop~Education, data=data)
sqrt(mean(lm_educationl$resid^2)) # 0.16
BIC(lm_educationl) 
summary(lm_educationl)

lm_retaill = lm(Logit_Prop~Retail, data=data)
sqrt(mean(lm_retaill$resid^2)) # 0.16
BIC(lm_retaill) 
summary(lm_retaill)

lm_recreationl = lm(Logit_Prop~Recreation, data=data)
sqrt(mean(lm_recreationl$resid^2)) # 0.16
BIC(lm_recreationl) 
summary(lm_recreationl)


# untransformed prop, log business
lm_lfood = lm(Sub_Prop~Log_Food, data=data)
sqrt(mean(lm_lfood$resid^2)) # 0.16
BIC(lm_lfood) 
summary(lm_lfood)

lm_lhealthcare = lm(Sub_Prop~Log_Healthcare, data=data)
sqrt(mean(lm_lhealthcare$resid^2)) # 0.16
BIC(lm_lhealthcare) 
summary(lm_lhealthcare)

lm_lother = lm(Sub_Prop~Log_Other, data=data)
sqrt(mean(lm_lother$resid^2)) # 0.16
BIC(lm_lother) 
summary(lm_lother)

lm_lna = lm(Sub_Prop~Log_N_A, data=data)
sqrt(mean(lm_lna$resid^2)) # 0.16
BIC(lm_lna) 
summary(lm_lna)

lm_lwork = lm(Sub_Prop~Log_Work, data=data)
sqrt(mean(lm_lwork$resid^2)) # 0.16
BIC(lm_lwork) 
summary(lm_lwork)

lm_leducation = lm(Sub_Prop~Log_Education, data=data)
sqrt(mean(lm_leducation$resid^2)) # 0.16
BIC(lm_leducation) 
summary(lm_leducation)

lm_lretail = lm(Sub_Prop~Log_Retail, data=data)
sqrt(mean(lm_lretail$resid^2)) # 0.16
BIC(lm_lretail) 
summary(lm_lretail)

lm_lrecreation = lm(Sub_Prop~Log_Recreation, data=data)
sqrt(mean(lm_lrecreation$resid^2)) # 0.16
BIC(lm_lrecreation) 
summary(lm_lrecreation)

 
# both logit and log
lm_lfoodl = lm(Logit_Prop~Log_Food, data=data)
sqrt(mean(lm_lfoodl$resid^2)) # 0.16
BIC(lm_lfoodl) 
summary(lm_lfoodl)

lm_lhealthcarel = lm(Logit_Prop~Log_Healthcare, data=data)
sqrt(mean(lm_lhealthcarel$resid^2)) # 0.16
BIC(lm_lhealthcarel) 
summary(lm_lhealthcarel)

lm_lotherl = lm(Logit_Prop~Log_Other, data=data)
sqrt(mean(lm_lotherl$resid^2)) # 0.16
BIC(lm_lotherl) 
summary(lm_lotherl)

lm_lnal = lm(Logit_Prop~Log_N_A, data=data)
sqrt(mean(lm_lnal$resid^2)) # 0.16
BIC(lm_lnal) 
summary(lm_lnal)

lm_lworkl = lm(Logit_Prop~Log_Work, data=data)
sqrt(mean(lm_lworkl$resid^2)) # 0.16
BIC(lm_lworkl) 
summary(lm_lworkl)

lm_leducationl = lm(Logit_Prop~Log_Education, data=data)
sqrt(mean(lm_leducationl$resid^2)) # 0.16
BIC(lm_leducationl) 
summary(lm_leducationl)

lm_lretaill = lm(Logit_Prop~Log_Retail, data=data)
sqrt(mean(lm_lretaill$resid^2)) # 0.16
BIC(lm_lretaill) 
summary(lm_lretaill)

lm_lrecreationl = lm(Logit_Prop~Log_Recreation, data=data)
sqrt(mean(lm_lrecreationl$resid^2)) # 0.16
BIC(lm_lrecreationl) 
summary(lm_lrecreationl)


## model using all variables
lm_alls = lm(Sub_Prop~Food+Healthcare+Other+N_A+Work+Education+Retail+Recreation + Log_Food+Log_Healthcare+Log_Other+Log_N_A+Log_Work+Log_Education+Log_Retail+Log_Recreation + total + p_food+p_healthcare+p_other+p_n_a+p_work+p_education+p_recreation+p_retail, data=data) 
sqrt(mean(lm_alls$resid^2))
BIC(lm_alls)
summary(lm_alls)

lm_alll = lm(Logit_Prop~Food+Healthcare+Other+N_A+Work+Education+Retail+Recreation + Log_Food+Log_Healthcare+Log_Other+Log_N_A+Log_Work+Log_Education+Log_Retail+Log_Recreation + total + p_food+p_healthcare+p_other+p_n_a+p_work+p_education+p_recreation+p_retail, data=data)
sqrt(mean(lm_alll$resid^2))
BIC(lm_alll)
summary(lm_alll)

## models using multiple variables 

lm_prop_some = lm(Sub_Prop~N_A+Education+Retail+Log_N_A+Log_Work+p_healthcare+p_n_a+p_work+p_education+p_recreation, data=data)
sqrt(mean(lm_prop_some$resid^2))
BIC(lm_prop_some)
summary(lm_prop_some)


lm_logit_some = lm(Logit_Prop~Healthcare+N_A+Education+Retail+Log_Work+p_healthcare+p_n_a+p_work+p_education, data=data)
sqrt(mean(lm_logit_some$resid^2))
BIC(lm_logit_some)
summary(lm_logit_some)


## Model assumptions
data$predicted <- predict(lm_alll, data)

ggplot(data, aes(x = predicted, y = Sub_Prop)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Predicted vs. Observed Values",
    x = "Predicted Values",
    y = "Observed Values"
  )


data$residuals <- residuals(lm_alll)
ggplot(data, aes(x = predicted, y = residuals)) +
  geom_point() +  # Residuals points
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Horizontal zero line
  labs(
    x = "Fitted Values (Predicted)",
    y = "Residuals",
    title = "Residuals vs. Fitted Values"
  )


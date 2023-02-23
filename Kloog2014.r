#Eq(1) 
library(ipw)
ipwmodel <- ipwpoint(exposure = haveAOD, family = "binomial", link = "logit",# haveAOD is 0 or 1, have AOD=1, missingAOD=0 
                     numerator = ~Month, denominator = ~ ele+SLP+TEM, data = data)# I am not sure if this formular is correct. Please refer to the help document of library(ipw)
data$ipw <- temp$ipw.weights
data$adjustAOD=data$AOD*data$ipw

#Eq(2) first stage mixed model 
library(lme4)
Model1 = lmer(PM2.5 ~ adjustAOD +Temperature+ 
                      populationdensity+elevation+trafficdensity+percentlanduse+emission+#X1
                      windspeed+visibility+SLP+RH+#X2
                      NDVI+PBL+#X3
                      (1+AOT+Temperature|day),#random intercepts+random slopes for AOD and Tem
                      data = train)#we calibrate PM2.5 for one city,so do not need the g(reg) and h(reg)




# read data:
load("data_train_DF.RData")
#
library(tidyr)
library(dplyr)
library(plyr)
library(mgcv)


# explore the data:
dim(data_train_DF)
names(data_train_DF)

# Remove NA values from CNT and BA
train_DF <- data_train_DF[!is.na(data_train_DF$CNT),]
train_DF <- train_DF[!is.na(train_DF$BA),]

train_DF<-train_DF%>% dplyr::rename(NSwind=clim1, WEwind=clim2, dew_temperature=clim3, temperature=clim4, 
         potential_evaporation=clim5, solar_radiation= clim6, 
         thermal_radiation=clim7, pressure=clim8, evaporation=clim9, 
         precipitation=clim10)

head(train_DF)

## Simplify some covariates 

dt<- train_DF%>%
  mutate(Wspeed=(sqrt(NSwind^2+WEwind^2)))%>%
  select(-NSwind, -WEwind)%>%
  mutate(trees_typ1=(sum(lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9)))%>%
  select(-lc1,-lc2,-lc3,-lc4,-lc5,-lc6,-lc7,-lc8,-lc9)%>%
  mutate(trees_typ2=(sum(lc10+lc11+lc12+lc13+lc14+lc14)))%>%
  select(-lc10,-lc11,-lc12,-lc13,-lc14,-lc15,-lc16,-lc17,-lc18)%>%
  mutate(RH=100*exp(17.625*(dew_temperature-273.15)/(dew_temperature-39.11))/
           exp(17.625*(temperature-273.15)/(temperature-39.11))  )

head(dt)

# histogram of training data on log-scale for better readability:
hist(log(1+dt$CNT), xlab = "log(1+CNT)", main = "Histogram of log-transformed CNT")
hist(log(1+dt$BA), xlab = "log(1+BA)", main = "Histogram of log-transformed BA")

# Or select datasets for some years 
dt_training<- dt%>%
  filter(year %in% c(1995, 2005, 2015))


# CNT (Poisson regression)
fit = glm(CNT ~., data = dt_training, family = poisson(link = log))
summary(fit)


# CNT (Poisson regression):
fit.gcv <- gam(CNT~  te(lon, lat),data=dt_training,family=poisson())
summary(fit.gcv)







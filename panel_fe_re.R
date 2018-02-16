
#setwd("C:/Users/utilisateur/iCloudDrive/TSE-EEE-S1/Panel/HW1")
library(readstata13)
library(tidyr)
library(dplyr)
library(plyr)

library(plm)


### (1) Converting data into Panel format ####

wide_data = read.dta13("wagedata.dta")

# Get the panel lwage
data_long <- gather(wide_data, year, lwage, lwage1980:lwage1987)
data_long <- data_long[order(data_long$nr, data_long$year), ]
data_long$year = mapvalues(data_long$year, from = unique(data_long$year), to = 1980:1987)
lwage = data_long %>% select(nr, year, lwage)

# Get the panel educ
data_long <- gather(wide_data, year, educ, educ1980:educ1987, factor_key=TRUE)
data_long <- data_long[order(data_long$nr, data_long$year), ]
data_long$year = mapvalues(data_long$year, from = unique(data_long$year), to = 1980:1987)
educ = data_long %>% select(nr, year, educ)

# Get the panel black
data_long <- gather(wide_data, year, black, black1980:black1987, factor_key=TRUE)
data_long <- data_long[order(data_long$nr, data_long$year), ]
data_long$year = mapvalues(data_long$year, from = unique(data_long$year), to = 1980:1987)
black = data_long %>% select(nr, year, black)

# Get the panel hisp
data_long <- gather(wide_data, year, hisp, hisp1980:hisp1987, factor_key=TRUE)
data_long <- data_long[order(data_long$nr, data_long$year), ]
data_long$year = mapvalues(data_long$year, from = unique(data_long$year), to = 1980:1987)
hisp = data_long %>% select(nr, year, hisp)

# Get the panel expersq
data_long <- gather(wide_data, year, expersq, expersq1980:expersq1987, factor_key=TRUE)
data_long <- data_long[order(data_long$nr, data_long$year), ]
data_long$year = mapvalues(data_long$year, from = unique(data_long$year), to = 1980:1987)
expersq = data_long %>% select(nr, year, expersq)

# Get the panel exper
data_long <- gather(wide_data, year, exper, exper1980:exper1987, factor_key=TRUE)
data_long <- data_long[order(data_long$nr, data_long$year), ]
data_long$year = mapvalues(data_long$year, from = unique(data_long$year), to = 1980:1987)
exper = data_long %>% select(nr, year, exper)

# Get the panel married
data_long <- gather(wide_data, year, married, married1980:married1987, factor_key=TRUE)
data_long <- data_long[order(data_long$nr, data_long$year), ]
data_long$year = mapvalues(data_long$year, from = unique(data_long$year), to = 1980:1987)
married = data_long %>% select(nr, year, married)

# Get the panel union
data_long <- gather(wide_data, year, union, union1980:union1987, factor_key=TRUE)
data_long <- data_long[order(data_long$nr, data_long$year), ]
data_long$year = mapvalues(data_long$year, from = unique(data_long$year), to = 1980:1987)
union = data_long %>% select(nr, year, union)

# Combine everything into the panel data
panel = cbind(lwage, educ$educ, black$black,hisp$hisp, exper$exper, expersq$expersq, married$married, union$union)
colnames(panel) = c("nr","year","lwage", "educ", "black","hisp", "exper", "expersq", "married", "union")
remove(black, data_long, educ, exper, expersq, hisp, lwage, married, union, wide_data)

# change years to numeric
panel$year = as.numeric(panel$year)

# check the balance of panel
table(panel$year)
foreign::write.dta(panel, "wage_panel.dta")

# transform data
panel <- plm::pdata.frame(panel, index=c("nr", "year"))

### (2) OLS Regression ####
pm = lm(lwage ~ educ + black + hisp + exper + expersq + married + union + year, data=panel)
summary(pm)

#### OLS Clustering ####
library(lmtest)
pm_cluster = plm(lwage ~ educ + black + hisp + exper + expersq + married + union + year, data=panel, 
                 model="pooling")
# cluster
coeftest(pm_cluster,vcov=vcovHC(pm_cluster,type="HC0", cluster="group")) # cluster = "group" or "time"
summary(pm_cluster)
# the clustering reduce the bias, but the variability is higher (SE is higher)



### (3) RE Model ####
random = plm(lwage ~ educ + black + hisp + exper + expersq + married + union + year, data=panel, index=c("nr", "year")
                ,model="random")
summary(random) 
coeftest(random,vcov=vcovHC(random,type="HC0",cluster="group"))
# the results is quite close to the OLS clustering



### (4) FE Model (with panel-robust se) ####
fixed <- plm(lwage ~ educ + black + hisp + exper + expersq + married + union + year, data=panel, 
             model="within", index = c("nr","year"))
summary(fixed)
coeftest(fixed,vcov=vcovHC(fixed,type="HC0",cluster="group"))

### (5) Hausman ####
form <- lwage ~ educ + black + hisp + exper + expersq + married + union
phtest(form, data=panel, method="aux", vcov = function(x) vcovHC(x, type="HC0", cluster="group"))  # reject the fe is prefered => problem with time-invariant


### (6) Hausman-Taylor ####
ht <- plm(formula = lwage ~ educ + black + hisp + 
            exper + expersq + married + union | black + hisp + married,
          data=panel, model="ht")
summary(ht)
phtest(fixed, ht)


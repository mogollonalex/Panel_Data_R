install.packages(c("gplots","plm", "lmtest", "zoo", "tseries"))

library(gplots)

library(plm)

library(tseries)

library(zoo)

library(lmtest)

path = "https://raw.githubusercontent.com/mogollonalex/Panel_Data_R/main/data.csv"
datos <-read.table(path,  header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
datos

# Heterogeneidad
plotmeans(IDHr ~ COD, main="Heterogeneidad entre países", data=datos)

plotmeans(IDHr ~ YEAR, main="Heterogeneidad a lo largo de los años", data=datos)#Testing for unit roots/stationarity |||| If p-value < 0.05 then no unit roots present
Panel.set <- plm.data(datos, index = c("COD", "YEAR"))
IDHr <-Panel.set$IDHr
adf.test(IDHr, k=2)

#Testing for unit roots/stationarity |||| If p-value < 0.05 then no unit roots present
Panel.set <- plm.data(datos, index = c("COD", "YEAR"))
INBpc <-Panel.set$INBpc
adf.test(INBpc, k=2)

#Testing for unit roots/stationarity |||| If p-value < 0.05 then no unit roots present
Panel.set <- plm.data(datos, index = c("COD", "YEAR"))
IGINICEPAL<-Panel.set$IGINICEPAL
adf.test(IGINICEPAL, k=2)

#Testing for unit roots/stationarity |||| If p-value < 0.05 then no unit roots present
Panel.set <- plm.data(datos, index = c("COD", "YEAR"))
GasSocial <-Panel.set$GasSocial
adf.test(GasSocial, k=2)

#Testing for unit roots/stationarity |||| If p-value < 0.05 then no unit roots present
Panel.set <- plm.data(datos, index = c("COD", "YEAR"))
VOZYREND <-Panel.set$VOZYREND
adf.test(VOZYREND, k=2)

### within
fixed <- plm(IDHr ~ IGINICEPAL+INBpc+GasSocial+VOZYREND,
         data=datos, index=c("COD", "YEAR"), model="within")
summary(fixed)

### random
library(plm)
random <-plm(IDHr ~ IGINICEPAL+INBpc+GasSocial+VOZYREND,
 data=datos, index=c("COD", "YEAR"), model="random")
summary(random)

################# Elegir el modelo
### Fixed or Random: Hausman test |||| If p-value > 0.05, then use random effects
phtest(fixed, random)

################# Robusto
summary(random, robust = TRUE)

# Test de normalidad de residuos
residuos=residuals(random)
residuos
#jarqueberaTest(residuos)
jarque.bera.test(residuos)

# stationarity residuals
#Testing for unit roots/stationarity |||| If p-value < 0.05 then no unit roots present
adf.test(residuos, k=2)

### Testing for serial correlation |||| If p-value > 0.05, No serial correlation
pbgtest(random)

### Testing for heteroskedasticity |||| If p-value > 0.05, No Presence of heteroskedasticity   
# If hetersokedaticity is detected you can use robust covariance matrix to account for it. See the following pages.
library(lmtest)
bptest(IDHr ~ IGINICEPAL+INBpc+GasSocial+VOZYREND + factor(COD), data = datos, studentize=F)

# Heteroskedasticity consistent coefficients (Arellano)   Corrección
coeftest(fixed, vcovHC(fixed, method = "arellano")) 




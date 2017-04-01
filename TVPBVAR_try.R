library(bvarsv)
library(forecastHybrid)
library(vars)

#Load data
data(usmacro.update)

#number of draws (in Gibbs sampler)
nburn.vignette <- 5000
nrep.vignette <- 50000

# variable to be forecast (1 = inf, 2 = une, 3 = tbi)
sel.var <- 2

# number of forecast periods
n.fc <- 5

# Set and define training and testing sample 
data.train <- head(usmacro.update, nrow(usmacro.update) - n.fc)
data.test <- tail(usmacro.update, n.fc)[, sel.var]

#fit the model
set.seed(1)
?bvar.sv.tvp
fit_tvp1 <- bvar.sv.tvp(data.train, p = 4, nburn = nburn.vignette, 
                   nrep = nrep.vignette, nf = 1)
fit_tvp2 <- bvar.sv.tvp(data.train, p = 4, nburn = nburn.vignette, 
                        nrep = nrep.vignette, nf = 2)
fit_tvp3 <- bvar.sv.tvp(data.train, p = 4, nburn = nburn.vignette, 
                        nrep = nrep.vignette, nf = 3)
fit_tvp4 <- bvar.sv.tvp(data.train, p = 4, nburn = nburn.vignette, 
                        nrep = nrep.vignette, nf = 4)
fit_tvp5 <- bvar.sv.tvp(data.train, p = 4, nburn = nburn.vignette, 
                        nrep = nrep.vignette, nf = 5)

#Get the forecast!!
sample_fcst1 <- predictive.draws(fit_tvp1, v = sel.var, h = 1)$y
sample_fcst2 <- predictive.draws(fit_tvp2, v = sel.var, h = 2)$y
sample_fcst3 <- predictive.draws(fit_tvp3, v = sel.var, h = 3)$y
sample_fcst4 <- predictive.draws(fit_tvp4, v = sel.var, h = 4)$y
sample_fcst5 <- predictive.draws(fit_tvp5, v = sel.var, h = 5)$y

fcst_tvp <- c(mean(sample_fcst1), mean(sample_fcst2),mean(sample_fcst3),
              mean(sample_fcst4), mean(sample_fcst5))
#Какой бы горизонт мы не ставили, выдает nrep/thinfac наблюдений #
#Правда ли это прогноз для Y_(T+h), а не все h y-ов???           #



#########################################################
#ETS+auto.arima

fit_hyb <- hybridModel(data.train[ ,sel.var], models = "ae", weights = "equal")
fcst_hyb <- forecast(fit_hyb, h = n.fc)
plot(fcst_hyb)
fcst_hyb$mean

#########################################################
#Simple VAR

VARselect(data.train, lag.max = 15)
fit_var <- VAR(data.train, p = 4)
fcst_var <- predict(fit_var, n.ahead = n.fc)[[1]][[sel.var]]

fcst_var[ , 1]

###########################################################
#Accuracy estimation
accuracy <- data.frame(tvp_bvar = accuracy(fcst_tvp, data.test)[1, ], 
                       hybrid = accuracy(fcst_hyb$mean, data.test)[1, ],
                       var = accuracy(fcst_var[ , 1], data.test)[1, ], 
                       row.names = colnames(accuracy(fcst_hyb$mean, data.test)))



############ Считаем одномерные модели

OneDim <- function(train, h = 24){
  # Функция оценивает 4 одномерные модели
  # ARIMA, ETS, Theta, Hybrid
  # 
  # Return:
  # Таблица с прогнозами каждой из моделей на заданном горизонте

  
  fit.1 <- ets(train)
  fit.2 <- auto.arima(train)
  fit.3 <- thetaf(train, h = h)
  
  #e == ets, a == arima, f ==theta
  horison.names <- paste("h =",h)
  method.names <- c( "random walk", "e", "a", "f", "hybrid")
  
  # Create lists for the output
  fcsts <- data.frame(matrix(rep(NA,5*h), nrow = h, ncol = 5))
  colnames(fcsts) <- method.names
  rmse <- rep(NA,5)

  for (j in 1:3){ # Get the forecasts of 3 models and compute rmse
      fcsts[,j+1] <- forecast(get(paste('fit', j, sep = '.')), h = h)$mean
      rmse[j+1] <- accuracy(get(paste('fit', j, sep = '.')))[2]
  }
  
  #assign(paste('fcst',horison[i], sep = '.'), temp)
  fcsts[,1] <- rwf(train, h = h)$mean
  rmse[1] <- accuracy(rwf(train, h = h))[2]
  
  best <- which(rmse[2:4] < rmse[1])
  if (length(best) > 1){
    model.name <- ''
    for (k in 1:length(best)){
      model.name <- paste(model.name, method.names[best[k]+1], sep='')
    } 
  } else {
    model.name <- 'eaf'
  }
  fit.4 <- hybridModel(train, models = model.name, weights = "cv.errors", 
                       verbose = FALSE, cvHorizon = 1, windowSize = (length(train)-h))
  fcsts[,5] <- forecast(fit.4, h = h)$mean 
  
  #acc <- list(rmse, mape)
  #names(acc) <- c("rmse", "mape")
  #result <- list(fcsts, acc)
  #names(result) <- c("forecasts", "accuracy")
  return(fcsts)
}



#################### Получаем результаты одномерных моделей

macro.fcst <- vector('list', ncol(macro))
names(macro.fcst) <- colnames(macro)
for (i in 1:ncol(macro)){
  macro.fcst[[i]] <- OneDim(train = macro[1:180,i], h = 24)
}


fin.fcst <- vector('list', ncol(stocks))
names(fin.fcst) <- colnames(stocks)
for (i in 1:ncol(stocks)){
  fin.fcst[[i]] <- OneDim(train = stocks[1:555,i], h = 260)
}


################ Считаем профет ###################

ProphetForecast <- function(x, h){
  data <- data.frame(ds = index(x), y = x)
  fit <- prophet(data)
  future <- make_future_dataframe(fit, periods = h)
  fcst <- tail(predict(fit, future)$yhat,h)
  return(fcst)
}

for (i in 1:ncol(stocks)){
  fin.fcst[[i]]$prophet <- ProphetForecast(stocks.zoo[1:555,i], h = 260)
}


# a <- data.frame(ds = index(stocks.zoo), y = stocks[,2])
# p <- prophet(a)
# future <- make_future_dataframe(p, periods = 260)
# 
# fcst <- predict(p, future)
# tail(fcst[c('ds', 'yhat')], 260)
# fcst$yhat
# plot(p, fcst)
# prophet_plot_components(p, forecast)






################## Теперь многомерные 


set.seed(7)
macro.pc <- Rtsne(t(macro), dims = 2, perplexity = 2)


qplot(x = macro.pc$Y[,1], y = macro.pc$Y[,2]) + 
  annotate("text", x = macro.pc$Y[,1], y = macro.pc$Y[,2],  label = m.names) +
  annotate("rect", xmin = -175, xmax = -25, ymin = -75, ymax = 25,
           alpha = .15)+
  annotate("rect", xmin = 50, xmax = 250, ymin = -75, ymax = -15,
           alpha = .15)+
  annotate("rect", xmin = -200, xmax = -125, ymin = 100, ymax = 135,
           alpha = .15)+
  ylab("Компонента 1")+
  xlab("Компонента 1")


# 3 группы - зп+производство+М2       РТС+нефть   CPI+ставка+безработица+курс доллара

set.seed(1)
t.stocks <- as.data.frame(t(stocks))
t.stocks$industry <- c(rep('banks.finance',3), rep('machinary',3), 
                       rep('metal.mining',3), rep('oil.gas',3), 
                       rep('consumer.goods',3), rep('IT',3), rep('chemistry',3),
                       rep('electrical.power',3))

colors = rainbow(length(unique(t.stocks$industry)))
names(colors) = unique(t.stocks$industry)
fin.pc<- Rtsne(t.stocks[,-816], dims = 5, perplexity = 1, theta=0.0,
               pca_center = FALSE)


qplot(x = fin.pc$Y[,1], y = fin.pc$Y[,2]) + 
  geom_text(aes(label = t.stocks$industry))


############# Считаем TVP-BVAR для макры
set.seed(123)

macro.tvp[[1]] <-  bvar.sv.tvp(macro[1:180, 1:7], p = 1, nburn = 10000, 
                               nrep = 16000, nf = 24, pdrift = FALSE, thinfac = 8)

macro.tvp[[2]] <-  bvar.sv.tvp(macro[1:180, c(1,5,7,8,9)], p = 1, nburn = 5000, 
                               nrep = 20000, nf = 24, pdrift = FALSE)

macro.tvp[[3]] <- bvar.sv.tvp(macro[1:180, c(2,3,4,6,8,9)], p = 1, nburn = 5000, 
                              nrep = 20000, nf = 24, pdrift = FALSE)

macro.tvp[[4]] <-  bvar.sv.tvp(macro[1:180, c(1,5,7)], p = 6, nburn = 5000, 
                               nrep = 25000, nf = 24, pdrift = FALSE)
macro.tvp[[5]] <- bvar.sv.tvp(macro[1:180, c(1,5,7)], p = 3, nburn = 5000, 
                              nrep = 20000, nf = 24, pdrift = FALSE)

macro.tvp[[6]] <-  bvar.sv.tvp(macro[1:180, c(8,9)], p = 12, nburn = 5000, 
                               nrep = 25000, nf = 24, pdrift = FALSE)

macro.tvp[[7]] <- bvar.sv.tvp(macro[1:180, c(8,9)], p = 6, nburn = 5000, 
                              nrep = 20000, nf = 24, pdrift = FALSE)
macro.tvp[[8]] <- bvar.sv.tvp(macro[1:180, c(8,9)], p = 3, nburn = 5000, 
                              nrep = 20000, nf = 24, pdrift = FALSE)

macro.tvp[[9]] <- bvar.sv.tvp(macro[1:180, c(2,3,4,6)], p = 3, nburn = 5000, 
                              nrep = 20000, nf = 24, pdrift = FALSE)
macro.tvp[[10]] <- bvar.sv.tvp(macro[1:180, c(2,3,4,6)], p = 1, nburn = 5000, 
                               nrep = 20000, nf = 24, pdrift = FALSE)


############### Считаем TVP-BVAR для акций
set.seed(123)
fin.tvp <- vector('list', 8)

for (i in 0:7){
  fin.tvp[[i+1]] <-  bvar.sv.tvp(stocks[1:555, (i*3+1):(i*3+3)], p = 5, 
                                 nburn = 5000, nrep = 14000, nf = 260, pdrift = FALSE, thinfac = 7)
}



##############################################################################
########################  Проверяем сходимость  ##############################
##############################################################################
chain1 <-as.mcmc(parameter.draws(macro.tvp[[2]], type = 'lag1', row = 1, col = 1)[1,])
chain1 <- as.mcmc(macro.tvp[[2]]$fc.mdraws[1,24,])
#chain2 <-  parameter.draws(test, type = 'lag1', row = 1, col = 1)
test <- bvar.sv.tvp(macro[1:180, 1:7], p = 1, nburn = 5000, 
                    nrep = 5000, nf = 24, thinfac = 5)

geweke.diag(chain1)
autocorr.diag(chain1)
autocorr.plot(chain1)
cumuplot(chain1)
heidel.diag(chain1) #Кайфовый
effectiveSize(chain1)
chain1 <-macro.tvp[[8]]$fc.mdraw[2,1,]
chain2 <-  macro.tvp2[[8]]$fc.mdraw[2,1,]
combinedchains <- mcmc.list(as.mcmc(chain1), as.mcmc(chain2))
plot(combinedchains)
gelman.diag(combinedchains, autoburnin = FALSE)
gelman.plot(combinedchains, autoburnin = FALSE)
?gelman.plot
plot(as.mcmc(chain1))



##############################################################################
######################  Добавим прогнозы в лист ##############################
##############################################################################

h = 24
for (k in 1:h){ 
  for (i in 1:7){ #[[1]]
    macro.fcst[[i]]$big.tvp1[k] <- mean(macro.tvp[[1]]$fc.mdraws[i,k, ])
  }
  n = 1
  for (i in c(1,5,7,8,9)){#[[2]]
    macro.fcst[[i]]$big.tvp2[k] <- mean(macro.tvp[[2]]$fc.mdraws[n,k, ])
    n = n + 1
  }
  n = 1
  for (i in c(2,3,4,6,8,9)){#[[3]]
    
    macro.fcst[[i]]$big.tvp3[k] <- mean(macro.tvp[[3]]$fc.mdraws[n,k, ])
    n = n + 1
  }
  n = 1
  for (i in c(1,5,7)){#[[4]]
    
    macro.fcst[[i]]$small.tvp6[k] <- mean(macro.tvp[[4]]$fc.mdraws[n,k, ])
    n = n + 1
  }
  n = 1
  for (i in c(1,5,7)){#[[5]]
    
    macro.fcst[[i]]$small.tvp3[k] <- mean(macro.tvp[[5]]$fc.mdraws[n,k, ])
    n = n + 1
  }
  n = 1
  for (i in c(8,9)){#[[6]]
    
    macro.fcst[[i]]$small.tvp12[k] <- mean(macro.tvp[[6]]$fc.mdraws[n,k, ])
    n = n + 1
  }
  for (i in c(8,9)){#[[7]]
    n = 1
    macro.fcst[[i]]$small.tvp6[k] <- mean(macro.tvp[[7]]$fc.mdraws[n,k, ])
    n = n + 1
  }
  n = 1
  for (i in c(8,9)){#[[8]]
    
    macro.fcst[[i]]$small.tvp3[k] <- mean(macro.tvp[[8]]$fc.mdraws[n,k, ])
    n = n + 1
  }
  n = 1
  for (i in c(2,3,4,6)){#[[9]]
    
    macro.fcst[[i]]$small.tvp3[k] <- mean(macro.tvp[[9]]$fc.mdraws[n,k, ])
    n = n + 1
  }
  n = 1
  for (i in c(2,3,4,6)){#[[10]]
    
    macro.fcst[[i]]$small.tvp1[k] <- mean(macro.tvp[[10]]$fc.mdraws[n,k, ])
    n = n + 1
  }
}





##############################################################################
##############################  Акции ########################################
##############################################################################

h = 260
for (k in 1:h){ 
  for (i in 1:8){
    n = 1
    for (p in 1:3){
      fin.fcst[[(i-1)*3 + p]]$tvp[k] <- mean(fin.tvp[[i]]$fc.mdraws[n,k, ])
      n = n + 1
    }
  }
}


@
  
  
  
  
  ##############################################################################
########  Теперь применим функцию, которая отменяет стандартизацию ###########
##############################################################################

macro.fcst.raw <- macro.fcst
fin.fcst.raw <- fin.fcst


for (i in 1:ncol(macro)){
  for (j in 1:ncol(macro.fcst[[i]])){
    macro.fcst.raw[[i]][,j] <- ReverseDataTransform(x = macro.fcst[[i]][,j],
                                                    center = attr(macro, 'center')[i], scale = attr(macro, 'scale')[i])
  }
}

for (i in 1:ncol(stocks)){
  for (j in 1:ncol(fin.fcst[[i]])){
    fin.fcst.raw[[i]][,j] <- ReverseDataTransform(x = fin.fcst[[i]][,j],
                                                  center = attr(stocks, 'center')[i], scale = attr(stocks, 'scale')[i])
  }
}




##############################################################################
##################### Считаем RMSE  ##########################################
##############################################################################

macro.rmse <- vector("list", ncol(macro))
h.macro <- c(3, 6, 12, 24)
test.sample.macro <- macro.raw[-1,]
for (i in 1:ncol(macro)){
  if (attr(macro, 'n.dif')[i] == 1){
    test.sample.macro[,i] <- diff(macro.raw[,i], lag = 1, differences = 1)
  } else{
    test.sample.macro[,i] <- macro.raw[1:204,i]
  }
}

for (i in 1:ncol(macro)){
  Mod <- ncol(macro.fcst.raw[[i]])
  macro.rmse[[i]] <- data.frame(matrix(rep(NA, 4*Mod), nrow = 4, ncol = Mod))
  colnames(macro.rmse[[i]]) <- colnames(macro.fcst[[i]])
  rownames(macro.rmse[[i]]) <- paste("h=", h.macro)
  for (m in 1:Mod){
    for (h in 1:4){
      errors <- accuracy(macro.fcst.raw[[i]][1:(h.macro[h]), m], 
                         test.sample.macro[181:(180+h.macro[h]),i])
      macro.rmse[[i]][h,m] <- errors[2]
    }
  }
}


fin.rmse <- vector("list", ncol(stocks))
h.fin <- c(7, 14, 30, 180, 260)
test.sample.fin <- stocks.raw[-1,]
for (i in 1:ncol(stocks)){
  test.sample.fin[,i] <- diff(stocks.raw[,i])
}


for (i in 1:ncol(stocks)){
  Mod <- ncol(fin.fcst.raw[[i]])
  fin.rmse[[i]] <- data.frame(matrix(rep(NA, 5*Mod), nrow = 5, ncol = Mod))
  colnames(fin.rmse[[i]]) <- colnames(fin.fcst[[i]])
  #rownames(fin.rmse[[i]]) <- paste("h=", h.fin)
  for (m in 1:Mod){
    for (h in 1:5){
      errors <- accuracy(fin.fcst.raw[[i]][1:h.fin[h], m], test.sample.fin[556:(555+h.fin[h]), i])
      fin.rmse[[i]][h,m] <- errors[2]
    }
  }
}




##############################################################################
################## Ошибки относительно RW  ###################################
##############################################################################
macro.error <- vector("list", ncol(macro))


for (i in 1:ncol(macro)){
  Mod <- ncol(macro.fcst.raw[[i]]) - 1
  macro.error[[i]] <- data.frame(matrix(rep(NA, 4*Mod), nrow = 4, ncol = Mod))
  colnames(macro.error[[i]]) <- colnames(macro.rmse[[i]])[-1]
  rownames(macro.error[[i]]) <- paste("h=", h.macro)
  for (m in 1:Mod){
    for (h in 1:4){
      error <- macro.rmse[[i]][h,m+1]/macro.rmse[[i]][h,1]
      macro.error[[i]][h,m] <- error
    }
  }
  #macro.rmse[[i]]$horison <- h.macro
}


fin.error <- vector("list", ncol(stocks))


for (i in 1:ncol(stocks)){
  Mod <- ncol(fin.fcst.raw[[i]])-1
  fin.error[[i]] <- data.frame(matrix(rep(NA, 5*Mod), nrow = 5, ncol = Mod))
  colnames(fin.error[[i]]) <- colnames(fin.rmse[[i]])[-1]
  rownames(fin.error[[i]]) <- paste("h=", h.fin)
  for (m in 1:Mod){
    for (h in 1:5){
      error <- fin.rmse[[i]][h,m+1]/fin.rmse[[i]][h,1]
      fin.error[[i]][h,m] <- error
    }
  }
 # fin.error[[i]]$horison <- h.fin
}



##############################################################################
#########  Сохраним результат, чтобы использовать в работе ###################
##############################################################################

save(file = "error.RData", list = c("macro.error", "fin.error"))













##############################################################################
##################       Лишнее      #########################################
##############################################################################

# GetFcstsBvar <- function(x, test, horison = c(3, 6, 12, 24)){
  # Get forecasts' means
  # Computes rmse and mape for each horison and variable
  #
  test = macro[181:204,c(2,3,4,6)]
  vars <- colnames(test)
  N <- length(vars) # количество переменных
  H <- length(horison) # количество горизонтов
  
   # Create lists for the output
  fcsts <- vector("list", H)
  mape <- data.frame(matrix(rep(NA,N*H), nrow = H, ncol = N))
  rmse <- data.frame(matrix(rep(NA,N*H), nrow = H, ncol = N))
  colnames(mape) <- vars
  colnames(rmse) <- vars
  row.names(mape) <- paste("h =",horison)
  row.names(rmse) <- paste("h =",horison)
  
  for (var in 1:N){
    for (j in 1:H){
      h = horison[j]
      fcsts[[j]] <- data.frame(matrix(rep(NA,N*h), nrow = h, ncol = N))
      colnames(fcsts[[j]]) <- vars
      for (k in 1:h){
        fcsts[[j]][k, var] <- mean(x$fc.mdraws[var, k,])
      }
      rmse[j, var] <- accuracy(fcsts[[j]][, var], test[1:h,var])[2]
      mape[j, var] <- accuracy(fcsts[[j]][, var], test[1:h,var])[5]
    }  
  }
  acc <- list(rmse, mape)
  names(acc) <- c("rmse", "mape")
  result <- list(fcsts, acc)
  names(results) <- c("forecasts", "accuracy")
  return(list(fcsts, list(mape, rmse)))

  

  
#GetFcstsBvar(macro.tvp[[1]], test = macro[181:204, 1:7])


  
#SelectParameters <- function(train, window, nburn, nrep, lags){
  # Parameter selection, using algorithm from Banbura,Giannone,Reichlin (2010) 
  #  (minimizing rmse for rolling estimates using given window)
  # Choosing among 18 different parameter spesifications (as in Primiceri 2005)
  # 
  # Fits the TVP BVAR model using optimal parameters
  # 
  # Returns:
  #  Out-of sample forecasts (mean value) and rmse for each variable and horison
  
  T <- length(train[,1])  # Количество наблюдений и тренировочной выборке
  N <- ncol(train)  # Количество переменных
  names <- colnames(train)  # Названия переменных
  kq = c(0.01, 0.05, 0.1)
  ks = c(0.01, 0.1, 1)
  kw = c(0.001, 0.01)
  fcsts.in <- vector("list", 18)  # Таблички с прогнозами по всем переменным
  rmse.in <- rep(NA, 18)  # Вектора со средними RMSE по всем переменным 
  
  spec = 0  # Отвечает за номер "спецификации" модели
  for (q in 1:3){
    for (s  in 1:3){
      for (w in 1:2){
        
        spec = spec +1
        # Табличка с прогнозами для каждой спецификации
        fcsts.in[[spec]] <- matrix(rep(NA, N*in.sample.fits), 
                                     ncol = N, nrow = in.sample.fits)
        rmse.temp <- rep(NA, N) # Вектор из RMSE по каждой переменной
        
        fcsts.in[[spec]] <- 
          rollapply(train[-T,], width = 120, 
                    FUN = function(x){
                            fit.in = bvar.sv.tvp(x, p = lags,
                                                 nburn = nburn, nrep = nrep, nf = 1, 
                                                 k_Q = kq[q], k_S = ks[s], 
                                                 k_W = kw[w])
                            draws = matrix(fit.in$fc.mdraws, nrow = N)
                            return(rowMeans(draws))
                          }, by.column=FALSE)
        for (var in 1:N){ 
          
          rmse.temp[var] <- accuracy(fcsts.in[[spec]][, var], 
                                     train[(window+1):T,var])[,2] 
        }
      rmse.in[spec] <- mean(rmse.temp)  
      }
    } 
  }
  spec.num <- which.min(rmse.in) # Выбираем спецификацию с минимальным rmse
  
  # Зная номер модели, определям значения памараметров
  q <- kq[(spec.num-1) %/% 6 + 1]
  s <- ks[((spec.num+5)%%6 + 1) %/% 2 + 1]
  w <- kw[(spec.num+1) %% 2 + 1]
  
  return(c(q,s,w))
}





# set.seed(123)
# macro.param <- SelectParameters(train <- macro[40:164,1:3], window = 120, 
#                                 nburn = 1000, nrep = 10000, lags = 6)
# 
# 
# macro.param <- SelectParameters(train <- macro[40:164,1:6], window = 120, 
#                                 nburn = 1000, nrep = 10000, lags = 2)
# 
# 
# 
# 
# stocks.param <- SelectParameters(train <- stocks[[1]][1:32,], window = 20, 
#                                  nburn = 1000, nrep = 10000, lags = 6)


# a <- as.data.frame(stocks.raw[[1]])
# a <- scale(a, center = FALSE)
# a <- diff(a)

# autoplot(diff(scale((stocks.raw[[1]]), center = FALSE)))
         
# SelectParameters(train = a[4000:4514,1:5],window = 400, nburn = 1000, nrep = 10000, lags = 2)





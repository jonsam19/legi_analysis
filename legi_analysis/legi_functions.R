## load packages
library(tidyverse)
library(fpp3)
library(gridExtra)

#################### Fourier model ##########################

fourier_model <- function(country){
  ## try model without ARIMA errors for 25 different Ks to find lowest AIC
  test_model <- filter_index(country,.~"2016 W52") %>%
    model('1' = ARIMA(log(cases+1)~fourier(K=1)+pdq(0,0,0)+PDQ(0,0,0)),
          '2' = ARIMA(log(cases+1)~fourier(K=2)+pdq(0,0,0)+PDQ(0,0,0)),
          '3' = ARIMA(log(cases+1)~fourier(K=3)+pdq(0,0,0)+PDQ(0,0,0)),
          '4' = ARIMA(log(cases+1)~fourier(K=4)+pdq(0,0,0)+PDQ(0,0,0)),
          '5' = ARIMA(log(cases+1)~fourier(K=5)+pdq(0,0,0)+PDQ(0,0,0)),
          '6' = ARIMA(log(cases+1)~fourier(K=6)+pdq(0,0,0)+PDQ(0,0,0)),
          '7' = ARIMA(log(cases+1)~fourier(K=7)+pdq(0,0,0)+PDQ(0,0,0)),
          '8' = ARIMA(log(cases+1)~fourier(K=8)+pdq(0,0,0)+PDQ(0,0,0)),
          '9' = ARIMA(log(cases+1)~fourier(K=9)+pdq(0,0,0)+PDQ(0,0,0)),
          '10' = ARIMA(log(cases+1)~fourier(K=10)+pdq(0,0,0)+PDQ(0,0,0)),
          '11' = ARIMA(log(cases+1)~fourier(K=11)+pdq(0,0,0)+PDQ(0,0,0)),
          '12' = ARIMA(log(cases+1)~fourier(K=12)+pdq(0,0,0)+PDQ(0,0,0)),
          '13' = ARIMA(log(cases+1)~fourier(K=13)+pdq(0,0,0)+PDQ(0,0,0)),
          '14' = ARIMA(log(cases+1)~fourier(K=14)+pdq(0,0,0)+PDQ(0,0,0)),
          '15' = ARIMA(log(cases+1)~fourier(K=15)+pdq(0,0,0)+PDQ(0,0,0)),
          '16' = ARIMA(log(cases+1)~fourier(K=16)+pdq(0,0,0)+PDQ(0,0,0)),
          '17' = ARIMA(log(cases+1)~fourier(K=17)+pdq(0,0,0)+PDQ(0,0,0)),
          '18' = ARIMA(log(cases+1)~fourier(K=18)+pdq(0,0,0)+PDQ(0,0,0)),
          '19' = ARIMA(log(cases+1)~fourier(K=19)+pdq(0,0,0)+PDQ(0,0,0)),
          '20' = ARIMA(log(cases+1)~fourier(K=20)+pdq(0,0,0)+PDQ(0,0,0)),
          '21' = ARIMA(log(cases+1)~fourier(K=21)+pdq(0,0,0)+PDQ(0,0,0)),
          '22' = ARIMA(log(cases+1)~fourier(K=22)+pdq(0,0,0)+PDQ(0,0,0)),
          '23' = ARIMA(log(cases+1)~fourier(K=23)+pdq(0,0,0)+PDQ(0,0,0)),
          '24' = ARIMA(log(cases+1)~fourier(K=24)+pdq(0,0,0)+PDQ(0,0,0)),
          '25' = ARIMA(log(cases+1)~fourier(K=25)+pdq(0,0,0)+PDQ(0,0,0)))
  ## find K with the lowest AIC
  glance_model <- test_model %>% glance()
  best_K <- glance_model$.model[glance_model$AIC == min(glance_model$AIC)]
  ## select model with lowest AIC
  best_model <- filter_index(country,.~"2016 W52") %>%
    model(ARIMA(log(cases+1)~fourier(K=as.numeric(best_K))+PDQ(0,0,0)))
  ## return best model
  return(best_model)
}



#################### Fourier 2019 model ##########################

fourier_2019_model <- function(country){
  ## try model without ARIMA errors for 25 different Ks to find lowest AIC
  test_model <- filter_index(country,"2014 W01"~"2018 W52") %>%
    model('1' = ARIMA(log(cases+1)~fourier(K=1)+pdq(0,0,0)+PDQ(0,0,0)),
          '2' = ARIMA(log(cases+1)~fourier(K=2)+pdq(0,0,0)+PDQ(0,0,0)),
          '3' = ARIMA(log(cases+1)~fourier(K=3)+pdq(0,0,0)+PDQ(0,0,0)),
          '4' = ARIMA(log(cases+1)~fourier(K=4)+pdq(0,0,0)+PDQ(0,0,0)),
          '5' = ARIMA(log(cases+1)~fourier(K=5)+pdq(0,0,0)+PDQ(0,0,0)),
          '6' = ARIMA(log(cases+1)~fourier(K=6)+pdq(0,0,0)+PDQ(0,0,0)),
          '7' = ARIMA(log(cases+1)~fourier(K=7)+pdq(0,0,0)+PDQ(0,0,0)),
          '8' = ARIMA(log(cases+1)~fourier(K=8)+pdq(0,0,0)+PDQ(0,0,0)),
          '9' = ARIMA(log(cases+1)~fourier(K=9)+pdq(0,0,0)+PDQ(0,0,0)),
          '10' = ARIMA(log(cases+1)~fourier(K=10)+pdq(0,0,0)+PDQ(0,0,0)),
          '11' = ARIMA(log(cases+1)~fourier(K=11)+pdq(0,0,0)+PDQ(0,0,0)),
          '12' = ARIMA(log(cases+1)~fourier(K=12)+pdq(0,0,0)+PDQ(0,0,0)),
          '13' = ARIMA(log(cases+1)~fourier(K=13)+pdq(0,0,0)+PDQ(0,0,0)),
          '14' = ARIMA(log(cases+1)~fourier(K=14)+pdq(0,0,0)+PDQ(0,0,0)),
          '15' = ARIMA(log(cases+1)~fourier(K=15)+pdq(0,0,0)+PDQ(0,0,0)),
          '16' = ARIMA(log(cases+1)~fourier(K=16)+pdq(0,0,0)+PDQ(0,0,0)),
          '17' = ARIMA(log(cases+1)~fourier(K=17)+pdq(0,0,0)+PDQ(0,0,0)),
          '18' = ARIMA(log(cases+1)~fourier(K=18)+pdq(0,0,0)+PDQ(0,0,0)),
          '19' = ARIMA(log(cases+1)~fourier(K=19)+pdq(0,0,0)+PDQ(0,0,0)),
          '20' = ARIMA(log(cases+1)~fourier(K=20)+pdq(0,0,0)+PDQ(0,0,0)),
          '21' = ARIMA(log(cases+1)~fourier(K=21)+pdq(0,0,0)+PDQ(0,0,0)),
          '22' = ARIMA(log(cases+1)~fourier(K=22)+pdq(0,0,0)+PDQ(0,0,0)),
          '23' = ARIMA(log(cases+1)~fourier(K=23)+pdq(0,0,0)+PDQ(0,0,0)),
          '24' = ARIMA(log(cases+1)~fourier(K=24)+pdq(0,0,0)+PDQ(0,0,0)),
          '25' = ARIMA(log(cases+1)~fourier(K=25)+pdq(0,0,0)+PDQ(0,0,0)))
  ## find K with the lowest AIC
  glance_model <- test_model %>% glance()
  best_K <- glance_model$.model[glance_model$AIC == min(glance_model$AIC)]
  ## select model with lowest AIC
  best_model <- filter_index(country,"2014 W01"~"2018 W52") %>%
    model(ARIMA(log(cases+1)~fourier(K=as.numeric(best_K))+PDQ(0,0,0)))
  ## return best model
  return(best_model)
}

## with Box-Cox transformation instead of log

fourier_model_lambda <- function(country){
  ## find best lambda for Box-Cox transformation
  lambda <- filter_index(country,.~"2017-12-25") %>%
    features(cases, features=guerrero) %>% pull(lambda_guerrero)
  ## try model without ARIMA errors for 25 different Ks to find lowest AIC
  test_model <- filter_index(country,.~"2017-12-25") %>%
    model('1' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=1)+pdq(0,0,0)+PDQ(0,0,0)),
          '2' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=2)+pdq(0,0,0)+PDQ(0,0,0)),
          '3' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=3)+pdq(0,0,0)+PDQ(0,0,0)),
          '4' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=4)+pdq(0,0,0)+PDQ(0,0,0)),
          '5' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=5)+pdq(0,0,0)+PDQ(0,0,0)),
          '6' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=6)+pdq(0,0,0)+PDQ(0,0,0)),
          '7' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=7)+pdq(0,0,0)+PDQ(0,0,0)),
          '8' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=8)+pdq(0,0,0)+PDQ(0,0,0)),
          '9' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=9)+pdq(0,0,0)+PDQ(0,0,0)),
          '10' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=10)+pdq(0,0,0)+PDQ(0,0,0)),
          '11' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=11)+pdq(0,0,0)+PDQ(0,0,0)),
          '12' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=12)+pdq(0,0,0)+PDQ(0,0,0)),
          '13' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=13)+pdq(0,0,0)+PDQ(0,0,0)),
          '14' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=14)+pdq(0,0,0)+PDQ(0,0,0)),
          '15' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=15)+pdq(0,0,0)+PDQ(0,0,0)),
          '16' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=16)+pdq(0,0,0)+PDQ(0,0,0)),
          '17' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=17)+pdq(0,0,0)+PDQ(0,0,0)),
          '18' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=18)+pdq(0,0,0)+PDQ(0,0,0)),
          '19' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=19)+pdq(0,0,0)+PDQ(0,0,0)),
          '20' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=20)+pdq(0,0,0)+PDQ(0,0,0)),
          '21' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=21)+pdq(0,0,0)+PDQ(0,0,0)),
          '22' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=22)+pdq(0,0,0)+PDQ(0,0,0)),
          '23' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=23)+pdq(0,0,0)+PDQ(0,0,0)),
          '24' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=24)+pdq(0,0,0)+PDQ(0,0,0)),
          '25' = ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=25)+pdq(0,0,0)+PDQ(0,0,0)))
  ## find K with the lowest AIC
  glance_model <- test_model %>% glance()
  best_K <- glance_model$.model[glance_model$AIC == min(glance_model$AIC)]
  ## select model with lowest AIC
  best_model <- filter_index(country,.~"2017-12-25") %>%
    model(ARIMA(box_cox(cases+1, lambda=lambda)~fourier(K=as.numeric(best_K))+PDQ(0,0,0)))
  ## return best model
  return(best_model)
}


######################## ECDC plot old #########################

ECDC_plot_old <- function(forecast_model, country){
  
  # FIGTSBREAKS <- pretty(seq(0, max(forecast_model[["upper_95"]]),
  #                          by = max(forecast_model[["upper_95"]])/5))
  
  ggplot() +
    geom_ribbon(forecast_model, mapping=aes(x=as.Date(week), ymin=lower_95, ymax=upper_95, fill='95% PI')) +
    geom_ribbon(forecast_model, mapping=aes(x=as.Date(week), ymin=lower_80, ymax=upper_80, fill='80% PI')) +
    geom_line(country, mapping=aes(x=as.Date(week), y=cases, colour="Observed"), size=1) +
    geom_line(forecast_model, mapping=aes(x=as.Date(week), y=.mean, colour="Predicted"), size=1.5) +
    geom_line(forecast_model, mapping=aes(x=as.Date(week), y=observed, colour="Observed"), size=1) +
    scale_x_date(date_labels = "  %Y", date_breaks= "1 year", expand = c(0, 0),
                 limits=c(as.Date("2012-01-01"),as.Date("2020-01-01"))) + 
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim=c(0, NA)) +
    ylab("Cases") + xlab("Year") +
    scale_colour_manual("lines", values=c("Predicted" = rgb(204,107,33,maxColorValue = 255), 
                                          "Observed" = rgb(101,179,46,maxColorValue = 255))) +
    scale_fill_manual("ribbons", values=c("95% PI" = rgb(225,167,68,maxColorValue = 255),
                                          "80% PI" = rgb(241,214,118,maxColorValue = 255))) +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12)) +    #Axis text style
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) +  #Empty backgroud
    theme(axis.line = element_line(colour = "#767171") ) +
    theme(legend.position = "right", legend.title =element_blank(), legend.text =element_text(size=12),
          legend.key=element_blank(), legend.key.width = unit(0.8, "cm")) +
    guides(fill=guide_legend(reverse=TRUE), colour=guide_legend(reverse=TRUE))
  
}



######################## ECDC plot #########################

ECDC_plot <- function(forecast_model, country){
  
  # FIGTSBREAKS <- pretty(seq(0, max(forecast_model[["upper_95"]]),
  #                          by = max(forecast_model[["upper_95"]])/5))
  
  ggplot() +
    geom_ribbon(forecast_model, 
                mapping=aes(x=as.Date(week), ymin=lower_95, ymax=upper_95, colour='95% PI',
                            fill='95% PI')) +
    geom_ribbon(forecast_model, 
                mapping=aes(x=as.Date(week), ymin=lower_80, ymax=upper_80, colour='80% PI',
                            fill='80% PI')) +
    geom_line(country, 
              mapping=aes(x=as.Date(week), y=cases, colour="Observed", fill="Observed"), size=1) +
    geom_line(forecast_model, 
              mapping=aes(x=as.Date(week), y=.mean, colour="Predicted", fill="Predicted"), size=1) +
    geom_line(forecast_model,
              mapping=aes(x=as.Date(week), y=observed, colour="Observed", fill="Observed"), size=1) +
    scale_x_date(date_labels = "  %Y", date_breaks= "1 year", expand = c(0, 0),
                 limits=c(as.Date("2012-01-01"),as.Date("2020-01-01"))) + 
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim=c(0, NA)) +
    ylab("Cases") + xlab("Year") +
    scale_colour_manual("lines", values=c("Predicted" = rgb(204,107,33,maxColorValue = 255),
                                          "Observed" = rgb(101,179,46,maxColorValue = 255),
                                          "95% PI" = rgb(225,167,68,maxColorValue = 255),
                                          "80% PI" = rgb(241,214,118,maxColorValue = 255))) +
    scale_fill_manual("ribbons", values=c("95% PI" = rgb(225,167,68,maxColorValue = 255),
                                          "80% PI" = rgb(241,214,118,maxColorValue = 255),
                                          "Predicted" = rgb(204,107,33,maxColorValue = 255),
                                          "Observed" = rgb(101,179,46,maxColorValue = 255))) +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12)) +    #Axis text style
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), plot.title = element_text(size=12)) +  #Empty backgroud
    theme(axis.line = element_line(colour = "#767171") ) +
    theme(legend.position = "right", legend.title =element_blank(), legend.text =element_text(size=12),
          legend.key=element_blank(), legend.key.width = unit(0.8, "cm")) +
    guides(fill=guide_legend(reverse=TRUE), colour=guide_legend(reverse=TRUE))
  
}



####################### decomposition function ##############################
dec_fun <- function(europe){
  p <<- europe %>% model(STL(cases, robust=TRUE)) %>% components() %>% 
    autoplot(colour=rgb(101,179,46,maxColorValue = 255)) + ggtitle("STL decomposition") + xlab(NULL) +
    scale_x_yearweek(breaks="1 year", 
                     labels=c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")) +
    theme_bw()
  
  cat("\n")
  }


########################## forecast function ##################################
for_fun <- function(europe, europe1719){
  
  model_europe <<- fourier_model(europe)
  
  ## create forecast for 2018
  forecast_europe <- forecast(model_europe,h=156) %>% 
    ## include 80% and 90% confidence intervals
    hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
    rename(
      upper_95 = '95%_upper',
      upper_80 = '80%_upper',
      lower_95 = '95%_lower',
      lower_80 = '80%_lower')
  
  ## add observed cases for 2018 to forecast
  forecast_europe$observed <- europe1719$cases
  
  forecast_europe <<- forecast_europe
  
  ## plot the foreacst
  p <<- ECDC_plot(forecast_europe, europe)
  cat("\n")
}



################################ excess function ########################################
excess_fun <- function(forecast_europe){
  ## excess cases
  table_europe <- forecast_europe %>% as_tibble() %>% 
    select(week, observed, .mean, upper_95, lower_95) %>% 
    mutate(
      above_pred = observed - .mean,
      above_95 = observed - upper_95,
      under_95 = observed - lower_95,
      year=year(week),
      year=as.character(year),
      week=format(week, format="%W") %>% as.double()) %>% 
    arrange(year,week)
  
  # table_europe$above_pred[table_europe$above_pred<0] <- 0
  table_europe$above_95[table_europe$above_95<0] <- 0
  table_europe$under_95[table_europe$under_95>0] <- 0
  
  table_2017 <- table_europe %>% filter(year==2017) %>% 
    summarise(above_95=sum(above_95+under_95),
              upper_95=sum(upper_95),
              observed=sum(observed),
              pred=sum(.mean),
              diff_pred=observed-pred,
              diff_95=observed-above_95) %>% 
    mutate(change_95=round(100*above_95/upper_95,digits=1),
           change_pred=round(100*diff_pred/pred,digits=1),
           year="2017") %>% 
    select(year,observed,pred,upper_95,diff_pred,change_pred,above_95,change_95)
  
  table_2018 <- table_europe %>% filter(year==2018) %>% 
    summarise(above_95=sum(above_95+under_95),
              upper_95=sum(upper_95),
              observed=sum(observed),
              pred=sum(.mean),
              above_pred=observed-pred,
              diff_pred=observed-pred,
              diff_95=observed-above_95) %>% 
    mutate(change_95=round(100*above_95/upper_95,digits=1),
           change_pred=round(100*diff_pred/pred,digits=1),
           year="2018") %>% 
    select(year,observed,pred,upper_95,diff_pred,change_pred,above_95,change_95)
  
  table_2019 <- table_europe %>% filter(year==2019) %>% 
    summarise(above_95=sum(above_95+under_95),
              upper_95=sum(upper_95),
              observed=sum(observed),
              pred=sum(.mean),
              diff_pred=observed-pred,
              diff_95=observed-above_95) %>% 
    mutate(change_95=round(100*above_95/upper_95,digits=1),
           change_pred=round(100*diff_pred/pred,digits=1),
           year="2019") %>% 
    select(year,observed,pred,upper_95,diff_pred,change_pred,above_95,change_95)
  
  total_table <- table_europe %>% 
    summarise(above_95=sum(above_95+under_95),
              upper_95=sum(upper_95),
              observed=sum(observed),
              pred=sum(.mean),
              diff_pred=observed-pred,
              diff_95=observed-above_95) %>% 
    mutate(change_95=round(100*above_95/upper_95,digits=1),
           change_pred=round(100*diff_pred/pred,digits=1),
           year="Total") %>% 
    select(year,observed,pred,upper_95,diff_pred,change_pred,above_95,change_95)
  
  over_predicted <<- total_table |> select(diff_pred,change_pred,above_95,change_95)
  
  ## table of each week with observed and predicted cases
  t <- rbind(table_2017, table_2018, table_2019, total_table) %>% 
    kable(digits=c(0,0,0,0,0,1,0,1), "html", align="c",
          col.names=c(" ", "Observed cases (O)", "Expected cases (E)", "Cases upper 95% PI", 
                      "O-E", "Percent change (%)", "Number", "Percent change (%)")) %>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>%
    add_header_above(c(" "=4, "Cases above prediction"=2, "Cases above 95% PI"=2)) %>%
    column_spec(2:8, width=2)
  print(t)
}

################################ analysis function ######################################
analysis_fun <- function(model_europe){
  
  ##### The model & residuals
  t <- model_europe %>% coef() %>% as.data.frame() %>% select(-.model) %>%
    kable(col.names=c("Term", "Estimate", "Standard error", "Test statistic", "P-value"), digits=3, caption="Model parameters") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
  
cat("\n")

  p <- model_europe %>% gg_tsresiduals(lag_max=52) + ggtitle("Residuals")
  print(p)
  
  ## Tests
  t <- model_europe %>% augment() %>% features(.resid, ljung_box, lag=104) %>% 
    rename(Test=.model,"Statistic"=lb_stat, "P-value"=lb_pvalue) %>% 
    rbind(model_europe %>% augment() %>% features(.resid, box_pierce, lag=104) %>% 
            rename(Test=.model,"Statistic"=bp_stat, "P-value"=bp_pvalue)) %>%
    mutate(Test=c("Ljung-Box test", "Box-Pierce test")) %>% 
    kable(digits=3) %>% 
    kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
  
}

##################### forecast 2019 function #############################
for19_fun <- function(europe, europe19){
## fit best model
  model_europe <<- europe %>% fourier_2019_model()

## create forecast for 2018
forecast_europe <- forecast(model_europe,h=52) %>% 
  ## include 80% and 90% confidence intervals
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower')

## add observed cases for 2018 to forecast
forecast_europe$observed <- europe19$cases

forecast_europe <<- forecast_europe

## plot the foreacst
p <<- ECDC_plot(forecast_europe, europe)
cat("\n")
}

################################ 2019 analysis function ######################################
analysis19_fun <- function(forecast_europe, model_europe){
  ## excess cases
  table_europe <- forecast_europe %>% as_tibble() %>% 
    select(week, observed, .mean, upper_95, lower_95) %>% 
    mutate(
      above_pred = observed - .mean,
      above_95 = observed - upper_95,
      under_95 = observed - lower_95,
      year=year(week) %>% as.character(),
      week=format(week, format="%W") %>% as.double()) %>% 
    arrange(week)
  
  # table_europe$above_pred[table_europe$above_pred<0] <- 0
  table_europe$above_95[table_europe$above_95<0] <- 0
  table_europe$under_95[table_europe$under_95>0] <- 0
  
  total_table <- table_europe %>% summarise(above_95=sum(above_95+under_95),
                             upper_95=sum(upper_95),
                             observed=sum(observed),
                             pred=sum(.mean),
                             diff_pred=observed-pred,
                             diff_95=observed-above_95) %>% 
    mutate(change_95=round(100*above_95/upper_95,digits=1),
           change_pred=round(100*diff_pred/pred,digits=1),
           year="2019") %>% 
    select(year,observed,pred,upper_95,diff_pred,change_pred,above_95,change_95)
  
  ## table of each week with observed and predicted cases
  t <- total_table %>%  
    kable(digits=c(0,0,0,0,0,1,0,1), "html", align="c", caption="Excess cases",
          col.names=c(" ", "Observed cases (O)", "Expected cases (E)", "Cases upper 95% PI", 
                      "O-E", "Percent change (%)", "Number", "Percent change (%)")) %>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>%
    add_header_above(c(" "=4, "Cases above prediction"=2, "Cases above 95% PI"=2)) %>%
    column_spec(2:8, width=2)
  print(t)
  
  cat(paste("Model: ",paste(coef(model_europe)[1,1]),sep=""))
  
  ##### The model & residuals
  t <- model_europe %>% coef() %>% as.data.frame() %>% select(-.model) %>%
    kable(col.names=c("Term", "Estimate", "Standard error", "Test statistic", "P-value"), digits=3, caption="Model parameters") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
  
  t <- model_europe %>% glance() %>% select(-.model) %>% 
    kable(digits=3) %>% kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
  
  cat("\n")
  
  p <- model_europe %>% gg_tsresiduals(lag_max=52) + ggtitle("Residual analysis")
  print(p)
  
  ## Ljung-Box test
  t <- model_europe %>% augment() %>% features(.resid, ljung_box, lag=104) %>% 
    kable(digits=3, caption="Ljung-Box test") %>% kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
  ## Box-Pierce test
  t <- model_europe %>% augment() %>% features(.resid, box_pierce, lag=104) %>% 
    kable(digits=3, caption="Box-Pierce test") %>% kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
}

############################### ITS function #############################

its_fun <- function(europe){
  
  ## intervention
  europe_dec <- europe %>% model(STL(cases, robust=TRUE)) %>% components() 
  europe$study_period = c(rep(0, 261), rep(1, 156))
  europe$sadjust_cases <- europe_dec$season_adjust
  europe_its <- europe %>% mutate(time=seq.int(nrow(europe)),
                                  time_after=seq(from=-260,to=156, by=1))
  europe_its$time_after[europe_its$time_after<0] <- 0
  
  colors <- c("Observed" = rgb(101,179,46,maxColorValue = 255), 
              "Fitted" = rgb(204,107,33,maxColorValue = 255),
              "Predicted" = rgb(204,107,33,maxColorValue = 255))
  linetypes <- c("Observed" = "solid", 
                 "Fitted" = "solid",
                 "Predicted" = "dashed")
  
  ## full period model
  its_model <- europe_its %>% 
    model(TSLM(sadjust_cases~time+lag(sadjust_cases)+study_period+time_after))

## plot of seasonally adjusted cases
  p <<- ggplot(data=europe_its, mapping=aes(x=as.Date(week), y=sadjust_cases)) +
    geom_line(size=1, mapping=aes(x=as.Date(week), y=cases, color="Observed",lty="Observed")) +
    geom_smooth(data=augment(its_model) %>% filter_index(.~"2016 W52"),
                mapping=aes(x=as.Date(week),y=.fitted,color="Predicted",lty="Predicted"),
                method="lm",se=F,fullrange=T) +
    geom_smooth(data=augment(its_model) %>% filter_index(.~"2016 W52"),
                mapping=aes(x=as.Date(week),y=.fitted,color="Fitted",lty="Fitted"),method="lm",se=F) + 
    geom_smooth(data=augment(its_model) %>% filter_index("2017 W01"~.),
                mapping=aes(x=as.Date(week),y=.fitted,color="Fitted",lty="Fitted"),method="lm",se=F) +
    geom_vline(xintercept=as.numeric(ymd("2017-01-01")), lty="dashed", colour = "#767171") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), legend.key=element_blank(), legend.title=element_blank()) +
    theme(axis.line = element_line(colour = "#767171")) +
    theme(axis.text=element_text(size=10), plot.title = element_text(size=12),
          axis.title=element_text(size=12)) +    #Axis text style
    theme(legend.position = "right", legend.title =element_blank(), legend.text =element_text(size=12),
          legend.key=element_blank(), legend.key.width = unit(0.8, "cm")) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim=c(0, NA)) +
    scale_x_date(date_labels = "  %Y", date_breaks= "1 year", expand = c(0, 0),
                 limits=c(as.Date("2012-01-01"),as.Date("2020-01-01"))) + 
    scale_color_manual(values=colors,name="legend") +
    scale_linetype_manual(values=linetypes,name="legend") +
    ylab("Cases") + xlab("Year")

  its_model <- lm(sadjust_cases~time+lag(sadjust_cases)+study_period+time_after,data=europe_its)
  its_table <- summary(its_model)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% as_tibble() %>% 
    left_join(confint(its_model) %>% as.data.frame() %>% rownames_to_column(var="term") %>% as_tibble(), by="term") %>% 
    mutate(conf_int=paste("(",round(`2.5 %`,digits=3)," - ",round(`97.5 %`,digits=3),")",sep=""),
           pval=format.pval(round(`Pr(>|t|)`, digits=3), eps=.001, digits=3, nsmall=3)) %>% 
    select(term,Estimate,conf_int,pval)
its_table <<- its_table
its_model <<- its_model

}

################################ analysis function rates ######################################
analysis_fun_rates <- function(forecast_europe, model_europe, pop_europe){
  
  ## excess cases
  table_europe <- forecast_europe %>% mutate(year=as.character(year(week))) %>%
    as.data.frame() %>% 
    select(week, observed, .mean, upper_95, lower_95,year) %>% 
    left_join(pop_europe,by="year") %>% 
    mutate(observed=(observed*population)/100000000,
           .mean=(.mean*population)/100000000,
           upper_95=(upper_95*population)/100000000,
           lower_95=(lower_95*population)/100000000) %>% 
    mutate(
      above_pred = observed - .mean,
      above_95 = observed - upper_95,
      under_95 = observed - lower_95,
      year=year(week),
      year=as.character(year),
      week=format(week, format="%W"),
      week=as.double(week))
  
  table_europe$above_pred[table_europe$above_pred<0] <- 0
  table_europe$above_95[table_europe$above_95<0] <- 0
  table_europe$under_95[table_europe$under_95>0] <- 0
  
  table_2017 <- table_europe %>% filter(year==2017) %>% 
    mutate(above_95=above_95-under_95,
           above_95=sum(above_95),
           above_predicted=sum(observed)-sum(.mean),
           observed=sum(observed),
           predicted=sum(.mean),
           upper_95=sum(upper_95),
           change_predicted=100*sum(above_predicted)/sum(predicted),
           change_95=100*sum(above_95)/sum(upper_95)) %>% 
    select(year, observed, predicted, upper_95, above_predicted, change_predicted, above_95, change_95) %>% 
    as_tibble() %>% unique()
  
  table_2018 <- table_europe %>% filter(year==2018) %>% 
    mutate(above_95=above_95-under_95,
           above_95=sum(above_95),
           above_predicted=sum(observed)-sum(.mean),
           observed=sum(observed),
           predicted=sum(.mean),
           upper_95=sum(upper_95),
           change_predicted=100*sum(above_predicted)/sum(predicted),
           change_95=100*sum(above_95)/sum(upper_95)) %>% 
    select(year, observed, predicted, upper_95, above_predicted, change_predicted, above_95, change_95) %>% 
    as_tibble() %>% unique()
  
  table_2019 <- table_europe %>% filter(year==2019) %>% 
    mutate(above_95=above_95-under_95,
           above_95=sum(above_95),
           above_predicted=sum(observed)-sum(.mean),
           observed=sum(observed),
           predicted=sum(.mean),
           upper_95=sum(upper_95),
           change_predicted=100*sum(above_predicted)/sum(predicted),
           change_95=100*sum(above_95)/sum(upper_95)) %>% 
    select(year, observed, predicted, upper_95, above_predicted, change_predicted, above_95, change_95) %>% 
    as_tibble() %>% unique()
  
  total_table <- table_europe %>% select(-year) %>% 
    mutate(above_95=above_95-under_95,
           above_95=sum(above_95),
           above_predicted=sum(observed)-sum(.mean),
           observed=sum(observed),
           predicted=sum(.mean),
           upper_95=sum(upper_95),
           change_predicted=100*sum(above_predicted)/sum(predicted),
           change_95=100*sum(above_95)/sum(upper_95),
           year="Total") %>% 
    select(year, observed, predicted, upper_95, above_predicted, change_predicted, above_95, change_95) %>% 
    as_tibble() %>% unique()
  
  ## table of each week with observed and predicted cases
  t <- rbind(table_2017, table_2018, table_2019, total_table) %>% 
    kable(digits=c(0,0,0,0,0,1,0,1), "html", align="c", caption="Excess cases",
          col.names=c(" ", "Observed cases (O)", "Expected cases (E)", "Cases upper CI 95%", 
                      "O-E", "Percent change (%)", "Number", "Percent change (%)")) %>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>%
    add_header_above(c(" "=4, "Cases above prediction"=2, "Cases above 95% PI"=2)) %>%
    column_spec(2:8, width=2)
  print(t)
  
  ##### The model & residuals
  t <- model_europe %>% coef() %>% as.data.frame() %>% select(-.model) %>%
    kable(col.names=c("Term", "Estimate", "Standard error", "Test statistic", "P-value"), digits=3, caption="Model parameters") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
  
  t <- model_europe %>% glance() %>% select(-.model) %>% 
    kable(digits=3) %>% kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
  
  cat("\n")
  
  p <- model_europe %>% gg_tsresiduals(lag_max=52) + ggtitle("Residual analysis")
  print(p)
  
  ## Ljung-Box test
  t <- model_europe %>% augment() %>% features(.resid, ljung_box, lag=104) %>% 
    kable(digits=3, caption="Ljung-Box test") %>% kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
  ## Box-Pierce test
  t <- model_europe %>% augment() %>% features(.resid, box_pierce, lag=104) %>% 
    kable(digits=3, caption="Box-Pierce test") %>% kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
}

########################## forecast function rates ##################################
for_fun_rate <- function(europe, europe1719){
  
  model_europe <<- fourier_model(europe)
  
  ## create forecast for 2018
  forecast_europe <- forecast(model_europe,h=156) %>% 
    ## include 80% and 90% confidence intervals
    hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
    rename(
      upper_95 = '95%_upper',
      upper_80 = '80%_upper',
      lower_95 = '95%_lower',
      lower_80 = '80%_lower')
  
  ## add observed cases for 2018 to forecast
  forecast_europe$observed <- europe1719$cases
  
  ## cases per 100 thousand
  forecast_europe <- forecast_europe %>%
    mutate(
      across(c(4:9),
             .fns = ~./1000))
  
  europe <- europe %>% 
    mutate(cases=cases/1000)
  
  forecast_europe <<- forecast_europe
  
  ## plot the foreacst
  p <<- ECDC_plot(forecast_europe, europe)
  cat("\n")
}


################################# Plot Shiny function ##################################
shiny_plot <- function(forecast,europe){
ggplot() +
  geom_ribbon(forecast, 
              mapping=aes(x=as.Date(week), ymin=lower_95, ymax=upper_95, colour='95% PI',
                          fill='95% PI')) +
  geom_ribbon(forecast,
              mapping=aes(x=as.Date(week), ymin=lower_80, ymax=upper_80, colour='80% PI',
                          fill='80% PI')) +
  geom_line(europe,
            mapping=aes(x=as.Date(week), y=cases, colour="Observed", fill="Observed"), size=1) +
  geom_line(forecast,
            mapping=aes(x=as.Date(week), y=.mean, colour="Predicted", fill="Predicted"), size=1) +
  geom_line(forecast,
            mapping=aes(x=as.Date(week), y=observed, colour="Observed", fill="Observed"), size=1) +
  scale_x_date(date_labels = "  %Y", date_breaks= "1 year", expand = c(0, 0),
               limits=c(as.Date("2012-01-01"),as.Date("2020-01-01"))) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, NA)) +
  ylab("Cases") + xlab("Year") +
  scale_colour_manual("lines", values=c("Predicted" = rgb(204,107,33,maxColorValue = 255),
                                        "Observed" = rgb(101,179,46,maxColorValue = 255),
                                        "95% PI" = rgb(225,167,68,maxColorValue = 255),
                                        "80% PI" = rgb(241,214,118,maxColorValue = 255))) +
  scale_fill_manual("ribbons", values=c("95% PI" = rgb(225,167,68,maxColorValue = 255),
                                        "80% PI" = rgb(241,214,118,maxColorValue = 255),
                                        "Predicted" = rgb(204,107,33,maxColorValue = 255),
                                        "Observed" = rgb(101,179,46,maxColorValue = 255))) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12)) +    #Axis text style
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), plot.title = element_text(size=12)) +  #Empty backgroud
  theme(axis.line = element_line(colour = "#767171") ) +
    guides(fill=guide_legend(reverse=TRUE), colour=guide_legend(reverse=TRUE)) +
  theme(legend.position = "right", legend.title =element_blank(), legend.text =element_text(size=12),
        legend.key=element_blank(), legend.key.width = unit(0.8, "cm"))
  
  
}



################################ Shiny excess function ########################################
shiny_excess <- function(forecast_europe){
  ## excess cases
  table_europe <- forecast_europe %>% as_tibble() %>% 
    select(week, observed, .mean, upper_95, lower_95) %>% 
    mutate(
      above_pred = observed - .mean,
      above_95 = observed - upper_95,
      under_95 = observed - lower_95,
      year=year(week),
      year=as.character(year),
      week=format(week, format="%W") %>% as.double()) %>% 
    arrange(year,week)
  
  # table_europe$above_pred[table_europe$above_pred<0] <- 0
  table_europe$above_95[table_europe$above_95<0] <- 0
  table_europe$under_95[table_europe$under_95>0] <- 0
  
  table_2017 <<- table_europe %>% filter(year==2017) %>% 
    summarise(above_95=sum(above_95+under_95),
              upper_95=sum(upper_95),
              observed=sum(observed),
              pred=sum(.mean),
              diff_pred=observed-pred,
              diff_95=observed-above_95) %>% 
    mutate(change_95=round(100*above_95/upper_95,digits=1),
           change_pred=round(100*diff_pred/pred,digits=1),
           year="2017") %>% 
    select(year,observed,pred,upper_95,diff_pred,change_pred,above_95,change_95)
  
  table_2018 <<- table_europe %>% filter(year==2018) %>% 
    summarise(above_95=sum(above_95+under_95),
              upper_95=sum(upper_95),
              observed=sum(observed),
              pred=sum(.mean),
              above_pred=observed-pred,
              diff_pred=observed-pred,
              diff_95=observed-above_95) %>% 
    mutate(change_95=round(100*above_95/upper_95,digits=1),
           change_pred=round(100*diff_pred/pred,digits=1),
           year="2018") %>% 
    select(year,observed,pred,upper_95,diff_pred,change_pred,above_95,change_95)
  
  table_2019 <<- table_europe %>% filter(year==2019) %>% 
    summarise(above_95=sum(above_95+under_95),
              upper_95=sum(upper_95),
              observed=sum(observed),
              pred=sum(.mean),
              diff_pred=observed-pred,
              diff_95=observed-above_95) %>% 
    mutate(change_95=round(100*above_95/upper_95,digits=1),
           change_pred=round(100*diff_pred/pred,digits=1),
           year="2019") %>% 
    select(year,observed,pred,upper_95,diff_pred,change_pred,above_95,change_95)
  
  total_table <<- table_europe %>% 
    summarise(above_95=sum(above_95+under_95),
              upper_95=sum(upper_95),
              observed=sum(observed),
              pred=sum(.mean),
              diff_pred=observed-pred,
              diff_95=observed-above_95) %>% 
    mutate(change_95=round(100*above_95/upper_95,digits=1),
           change_pred=round(100*diff_pred/pred,digits=1),
           year="Total") %>% 
    select(year,observed,pred,upper_95,diff_pred,change_pred,above_95,change_95)
  
  over_predicted <<- total_table |> select(diff_pred,change_pred,above_95,change_95)
  
}



################################ Shiny analysis function ######################################
analysis_shiny <- function(model_europe){
  
  ##### The model & residuals
  t <- model_europe %>% coef() %>% as.data.frame() %>% select(-.model) %>%
    kable(col.names=c("Term", "Estimate", "Standard error", "Test statistic", "P-value"), digits=3, caption="Model parameters") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"))
  print(t)
  
  cat("\n")
  
  p <- model_europe %>% gg_tsresiduals(lag_max=52) + ggtitle("Residuals")
  print(p)

}


############################### ITS Shiny #############################


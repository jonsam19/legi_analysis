#################################### All ################################
europe <- legi_tsibble %>% 
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

model_europe <- filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=3)+1+pdq(1,1,1)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_all.rds")

forecast(readRDS("Models and plots/model_all.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_all.rds")



################################# Imported ##############################
europe <- legi_tsibble %>% filter(Imported3=="Imported") |> 
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=7)+0+pdq(0,1,1)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_imported.rds")

forecast(readRDS("Models and plots/model_imported.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_imported.rds")



################################# Not imported ##############################
europe <- legi_tsibble %>% filter(Imported3=="Not imported") |> 
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=3)+1+pdq(1,1,1)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_notimported.rds")

forecast(readRDS("Models and plots/model_notimported.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_notimported.rds")



################################# <40 ##############################
europe <- legi_tsibble %>%
  filter(AgeGroup2=="<40") %>%
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=3)+1+pdq(0,0,0)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_40.rds")

forecast(readRDS("Models and plots/model_40.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_40.rds")



################################# 40-49 ##############################
europe <- legi_tsibble %>%
  filter(AgeGroup2=="40-49") %>%
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=4)+1+pdq(1,0,0)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_40_49.rds")

forecast(readRDS("Models and plots/model_40_49.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_40_49.rds")


################################# 50-59 ##############################
europe <- legi_tsibble %>%
  filter(AgeGroup2=="50-59") %>%
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=4)+1+pdq(1,0,0)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_50_59.rds")

forecast(readRDS("Models and plots/model_50_59.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_50_59.rds")


################################# 60-69 ##############################
europe <- legi_tsibble %>%
  filter(AgeGroup2=="60-69") %>%
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=4)+1+pdq(1,0,0)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_60_69.rds")

forecast(readRDS("Models and plots/model_60_69.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_60_69.rds")


################################# 70-79 ##############################
europe <- legi_tsibble %>%
  filter(AgeGroup2=="70-79") %>%
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=4)+1+pdq(1,0,0)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_70_79.rds")

forecast(readRDS("Models and plots/model_70_79.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_70_79.rds")



################################# 80>= ##############################
europe <- legi_tsibble %>%
  filter(AgeGroup2=="80>=") %>%
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=4)+1+pdq(1,0,0)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_80.rds")

forecast(readRDS("Models and plots/model_80.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_80.rds")


################################# Female ##############################
europe <- legi_tsibble %>%
  filter(Gender=="F") %>%
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=4)+1+pdq(1,0,0)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_female.rds")

forecast(readRDS("Models and plots/model_female.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_female.rds")


################################# Male ##############################
europe <- legi_tsibble %>%
  filter(Gender=="M") %>%
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

filter_index(europe,.~"2016 W52") %>%
  model(ARIMA(log(cases+1)~fourier(K=4)+1+pdq(1,0,0)+PDQ(0,0,0))) |> 
  saveRDS("Models and plots/model_male.rds")

forecast(readRDS("Models and plots/model_male.rds"),h=156) %>% 
  hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
  rename(
    upper_95 = '95%_upper',
    upper_80 = '80%_upper',
    lower_95 = '95%_lower',
    lower_80 = '80%_lower') |> 
  mutate(observed=europe1719$cases) |> 
  saveRDS("Models and plots/forecast_male.rds")


############################### ITS all #####################################
europe <- legi_tsibble %>%
  filter(Imported3=="Imported") |> 
  summarise(cases=sum(nb))
europe1719 <- europe %>%
  filter_index("2017 W01"~.)

europe_dec <- europe %>% model(STL(cases, robust=TRUE)) %>% components() 
europe$study_period = c(rep(0, 261), rep(1, 156))
europe$sadjust_cases <- europe_dec$season_adjust
europe_its <- europe %>% mutate(time=seq.int(nrow(europe)),
                                time_after=seq(from=-260,to=156, by=1))
europe_its$time_after[europe_its$time_after<0] <- 0

its_model <- europe_its %>% 
  model(TSLM(sadjust_cases~time+lag(sadjust_cases)+study_period+time_after))

its_model |> 
  saveRDS("Models and plots/its_model_male.rds")

colors <- c("Observed" = rgb(101,179,46,maxColorValue = 255), 
            "Fitted" = rgb(204,107,33,maxColorValue = 255),
            "Predicted" = rgb(204,107,33,maxColorValue = 255))
linetypes <- c("Observed" = "solid", 
               "Fitted" = "solid",
               "Predicted" = "dashed")

p <- ggplot(data=europe_its, mapping=aes(x=as.Date(week), y=sadjust_cases)) +
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
saveRDS(p,"Models and plots/itsplot_imported.rds")

its_model <- lm(sadjust_cases~time+lag(sadjust_cases)+study_period+time_after,data=europe_its)

checkresiduals(its_model,test=FALSE) |> 
  saveRDS("Models and plots/itsresiduals_all.rds")

summary(its_model)$coefficients %>% as.data.frame() %>% 
  rownames_to_column(var="term") %>% as_tibble() %>% 
  left_join(confint(its_model) %>% as.data.frame() %>% 
              rownames_to_column(var="term") %>% as_tibble(), by="term") %>% 
  mutate(conf_int=paste("(",round(`2.5 %`,digits=3)," - ",round(`97.5 %`,digits=3),")",sep=""),
         pval=format.pval(round(`Pr(>|t|)`, digits=3), eps=.001, digits=3, nsmall=3)) %>% 
  select(term,Estimate,conf_int,pval) |> 
  kable(col.names=c("Term", "Estimate", "95% Confidence interval", "P-value"),digits=3,
        align=c("l",rep('c',times=3))) |>
  kable_styling(bootstrap_options = c("striped", "condensed","hover")) |> 
  saveRDS("Models and plots/itsmodel_imported.rds")
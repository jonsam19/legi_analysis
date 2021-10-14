######################### prepare the data ############################

## read in legi data and create aggregated data
legidata <- read_csv("legi_agg_onset_4.csv",
                     col_type = list(col_character(), col_date(), col_date(), col_date(), 
                                     col_character(), col_double(), col_character(), col_character(), 
                                     col_character(), col_character(), col_character(),
                                     col_character(), col_character(), col_character(), 
                                     col_character(), col_character(), col_double()))

## create date variable
legidata <- legidata %>%
  mutate(date=case_when(is.na(DateOfOnsetISOdate_date)~DateUsedForStatisticsISO,
                        TRUE~DateOfOnsetISOdate_date),
         date=case_when(is.na(date)~DateOfNotificationISOdate,
                        TRUE~date))

completeness_dates <- legidata

## create Imported3
legidata <- legidata %>% 
  mutate(Imported3=case_when(Imported2=="UNK"~Imported,
                             TRUE~Imported2),
         Imported3=case_when(Imported3=="Y"~"Imported",
                             Imported3=="N"~"Not imported",
                             Imported3=="UNK"~"Unknown",
                             is.na(Imported3)~"Unknown"))

## create age groups
legidata$AgeGroup <- NA
legidata$AgeGroup[legidata$Age<20] <- "<20"
legidata$AgeGroup[legidata$Age>=20 & legidata$Age<30] <- "20-29"
legidata$AgeGroup[legidata$Age>=30 & legidata$Age<40] <- "30-39"
legidata$AgeGroup[legidata$Age>=40 & legidata$Age<50] <- "40-49"
legidata$AgeGroup[legidata$Age>=50 & legidata$Age<60] <- "50-59"
legidata$AgeGroup[legidata$Age>=60 & legidata$Age<70] <- "60-69"
legidata$AgeGroup[legidata$Age>=70 & legidata$Age<80] <- "70-79"
legidata$AgeGroup[legidata$Age>=80] <- "80>="
legidata$AgeGroup[is.na(legidata$Age)] <- "Unknown"

legidata$AgeGroup2 <- NA
legidata$AgeGroup2[legidata$Age<40] <- "<40"
legidata$AgeGroup2[legidata$Age>=40 & legidata$Age<50] <- "40-49"
legidata$AgeGroup2[legidata$Age>=50 & legidata$Age<60] <- "50-59"
legidata$AgeGroup2[legidata$Age>=60 & legidata$Age<70] <- "60-69"
legidata$AgeGroup2[legidata$Age>=70 & legidata$Age<80] <- "70-79"
legidata$AgeGroup2[legidata$Age>=80] <- "80>="
legidata$AgeGroup2[is.na(legidata$Age)] <- "Unknown"

legidata$AgeGroup3 <- NA
legidata$AgeGroup3[legidata$Age<65] <- "<65"
legidata$AgeGroup3[legidata$Age>=65] <- "65>="
legidata$AgeGroup3[is.na(legidata$Age)] <- "Unknown"


## add week and month
legidata$week <- yearweek(legidata$date)
legidata$month <- yearmonth(legidata$date)
legidata$date <- NULL

completeness_setting <- legidata

a <- legidata %>% summarise(sum(nb))

## remove dateless cases
legidata <- legidata[!is.na(legidata$week),]
b <- legidata %>% summarise(sum(nb))

## with all countries
legi_all <- legidata

## keeping Germany
legidata_de <- legidata %>% filter(CountryName!="Croatia" & CountryName!="Iceland" & 
                                     CountryName!="Luxembourg" & CountryName!="Bulgaria")

legidata <- legidata %>% filter(CountryName!="Croatia" & CountryName!="Iceland" & 
                                  CountryName!="Germany" & CountryName!="Luxembourg" & CountryName!="Bulgaria")
c <- legidata %>% summarise(sum(nb))

## count cases in week 45 2014 belonging to PT2014 outbreak
legidata %>% filter(week==yearweek("W45 2014")) %>% 
  group_by(ClusterID) %>% summarise(cases=sum(nb)) %>% 
  mutate(percent=100*cases/sum(cases))

## create data without PT2014 outbreak
legidata_nopt <- legidata[legidata$ClusterID!="206-VFX" | is.na(legidata$ClusterID),] ## is.na to include cases after 2014
d <- legidata_nopt %>% summarise(sum(nb))

## data without cluster cases
legidata_noc <- legidata[legidata$Cluster!="Y",] %>% filter(!is.na(nb))
e <- legidata_noc %>% summarise(sum(nb))

## data with only cluster cases
legidata_c <- legidata[legidata$Cluster=="Y",] %>% filter(!is.na(nb))
legidata_c <- legidata_c[legidata_c$ClusterID!="206-VFX" | is.na(legidata_c$ClusterID),] %>% filter(!is.na(nb))
f <- legidata_c %>% summarise(sum(nb))



## aggregate
agg_legidata <- legidata %>% group_by(CountryName, Imported3, Gender, AgeGroup2, week) %>%
  summarise(nb=sum(nb)) %>% ungroup()

agg_legiall <- legi_all %>% group_by(CountryName, week) %>%
  summarise(nb=sum(nb)) %>% ungroup()

agg_legidata_nopt <- legidata_nopt %>% group_by(CountryName, Imported3, Gender, AgeGroup2, week) %>%
  summarise(nb=sum(nb)) %>% ungroup()

agg_legidata_noc <- legidata_noc %>% group_by(week) %>%
  summarise(nb=sum(nb)) %>% ungroup()

agg_legidata_c <- legidata_c %>% group_by(week) %>%
  summarise(nb=sum(nb)) %>% ungroup()

agg_legidata_de <- legidata_de %>% group_by(week) %>%
  summarise(nb=sum(nb)) %>% ungroup()

agg_monthly <- legidata %>% group_by(CountryName, Imported3, Gender, AgeGroup2, month) %>%
  summarise(nb=sum(nb)) %>% ungroup()

agg_newage <- legidata_nopt %>% group_by(Imported3, AgeGroup3, week) %>%
  summarise(nb=sum(nb)) %>% ungroup()



################################################## create Legi tsibbles ###############################################
legi_tsibble <- agg_legidata %>%
  as_tsibble(index=week, 
             key=c(CountryName, Imported3, Gender, AgeGroup2)) %>%
  fill_gaps(.full=TRUE) %>%
  filter_index("2012 W01"~"2019 W52")
rm(agg_legidata) ## remove aggregated data
legi_tsibble$nb[is.na(legi_tsibble$nb)] <- 0 ## fill missing weeks with 0 cases

legi_alltsibble <- agg_legiall %>%
  as_tsibble(index=week, 
             key=c(CountryName)) %>%
  fill_gaps(.full=TRUE) %>%
  filter_index("2012 W01"~"2019 W52")
rm(agg_legiall)
legi_alltsibble$nb[is.na(legi_alltsibble$nb)] <- 0

## without PT outbreak
leginopt_tsibble <- agg_legidata_nopt %>%
  as_tsibble(index=week, 
             key=c(CountryName, Imported3, Gender, AgeGroup2)) %>%
  fill_gaps(.full=TRUE) %>%
  filter_index("2012 W01"~"2019 W52")
rm(agg_legidata_nopt)
leginopt_tsibble$nb[is.na(leginopt_tsibble$nb)] <- 0
g <- leginopt_tsibble %>% as_tibble() %>% summarise(sum(nb))

## without clusters
leginoc_tsibble <- agg_legidata_noc %>%
  as_tsibble(index=week) %>%
  fill_gaps(.full=TRUE) %>%
  filter_index("2012 W01"~"2019 W52")
rm(agg_legidata_noc)
leginoc_tsibble$nb[is.na(leginoc_tsibble$nb)] <- 0

## only clusters
legic_tsibble <- agg_legidata_c %>%
  as_tsibble(index=week) %>%
  fill_gaps(.full=TRUE) %>%
  filter_index("2012 W01"~"2019 W52")
rm(agg_legidata_c)
legic_tsibble$nb[is.na(legic_tsibble$nb)] <- 0

## with Germany
legide_tsibble <- agg_legidata_de %>%
  as_tsibble(index=week) %>%
  fill_gaps(.full=TRUE) %>%
  filter_index("2012 W01"~"2019 W52")
rm(agg_legidata_de)
legide_tsibble$nb[is.na(legide_tsibble$nb)] <- 0

## monthly data
legi_monthly <- agg_monthly %>% 
  as_tsibble(index=month, 
             key=c(CountryName, Imported3, Gender, AgeGroup2)) %>%
  fill_gaps(.full=TRUE) %>%
  filter_index("2012 Jan"~"2019 Dec")
rm(agg_monthly)
legi_monthly$nb[is.na(legi_monthly$nb)] <- 0

## new age group
legi_newage <- agg_newage %>% 
  as_tsibble(index=week, 
             key=c(Imported3, AgeGroup3)) %>%
  fill_gaps(.full=TRUE) %>%
  filter_index("2012 W01"~"2019 W52")
rm(agg_newage)
legi_newage$nb[is.na(legi_newage$nb)] <- 0

## labels
x_month <- list(title = "Month")
x_year <- list(title = "Year")
x_week <- list(title = "Week")
y_cases <- list(title = "Cases")
#y_cases <- list(title = "Cases",font=list(size=1))

## create vectors of stratifications
countries <- as.factor(unique(legi_tsibble$CountryName)[!is.na(unique(legi_tsibble$CountryName))])
imported <- as.factor(unique(legi_tsibble$Imported3)[!is.na(unique(legi_tsibble$Imported3))])
agearoups <- as.factor(unique(legi_tsibble$AgeGroup2)[!is.na(unique(legi_tsibble$AgeGroup2)) &
                                                        unique(legi_tsibble$AgeGroup2)!="Unknown"])
genders <- unique(legi_tsibble$Gender)[!is.na(unique(legi_tsibble$Gender)) &
                                         unique(legi_tsibble$Gender)!="UNK"]

## create forecast



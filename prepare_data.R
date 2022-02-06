## read in legi data and create aggregated data
legidata <- read_csv("legi_agg_onset_4.csv",
                     col_type = list(col_character(), col_date(), col_date(), col_date(), 
                                     col_character(), col_double(), col_character(), col_character(), 
                                     col_character(), col_character(), col_character(),
                                     col_character(), col_character(), col_character(), 
                                     col_character(), col_character(), col_double()))

## create date variable
legidata <- legidata |>
  mutate(date=case_when(is.na(DateOfOnsetISOdate_date)~DateUsedForStatisticsISO,
                        TRUE~DateOfOnsetISOdate_date),
         date=case_when(is.na(date)~DateOfNotificationISOdate,
                        TRUE~date))

completeness_dates <- legidata

## create Imported3
legidata <- legidata |> 
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

## remove dateless cases
legidata <- legidata[!is.na(legidata$week),]

legidata <- legidata |> filter(CountryName!="Croatia" & CountryName!="Iceland" & 
                                 CountryName!="Germany" & CountryName!="Luxembourg" & CountryName!="Bulgaria")

## create data without PT2014 outbreak
legidata_nopt <- legidata[legidata$ClusterID!="206-VFX" | is.na(legidata$ClusterID),] ## is.na to include cases after 2014

## data without cluster cases
legidata_noc <- legidata[legidata$Cluster!="Y",] |> filter(!is.na(nb))

## data with only cluster cases
legidata_c <- legidata[legidata$Cluster=="Y",] |> filter(!is.na(nb))
legidata_c <- legidata_c[legidata_c$ClusterID!="206-VFX" | is.na(legidata_c$ClusterID),] |> filter(!is.na(nb))



## aggregate
agg_legidata <- legidata |> group_by(Imported3, Gender, AgeGroup2, week) |>
  summarise(nb=sum(nb)) |> ungroup()



## make tsibble
legi_tsibble <- agg_legidata |>
  as_tsibble(index=week, 
             key=c(Imported3, Gender, AgeGroup2)) |>
  fill_gaps(.full=TRUE) |>
  filter_index("2012 W01"~"2019 W52")
rm(agg_legidata) ## remove aggregated data
legi_tsibble$nb[is.na(legi_tsibble$nb)] <- 0 ## fill missing weeks with 0 cases

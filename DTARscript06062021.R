#### Set wd ####
setwd("C:/Users/woodj/Dropbox (Personal)/Apps/Overleaf/Taxtreatiesmaster")

#### Base: Packages ####
packages <- c("tidyr", "car", "dplyr", "survival", "ggplot2", "tidyverse",
              "haven", "readxl", "lme4", "reshape", "splitstackshape", 
              "countrycode", "lubridate", "stargazer", "runner", 
              "separationplot", "car", "MASS", "survival", 
              "multiwayvcov", "maxLik", "miscTools", "sampleSelection", "mvtnorm", "censReg", 
              "plotROC", "lmtest", "sandwich", "MVN") ## Vector of needed packages

## Applying a function that installs/loads packages to the vector of packages
sapply(packages, 
       FUN = function(x){
         ## Check if package is installed; if not, install it:
         if(x %in% rownames(installed.packages()) == FALSE){
           install.packages(x)
         }
         ## load the package:
         library(x, character.only = TRUE)
       })

#### ATH: Importing data for ATH ####

#Main unit for ATH index #Leader spells until 2015
archigos <- read_dta("DArchigos_4.1_stata14.dta") 

#Variable: Ancars typology for regime types
load("Danckar.rda")
regimedata<- anckar #Classification of political regimes 1800-2016
table(regimedata$democracy) #11578 non democracies

#Variable: Type of conflict 1-4 until #2020
load("Ducdp-prio-acd-201.RData")
UCDP <- ucdp_prio_acd_201

# Variable: coups until 2020
coupsdata <- read.delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_ccode_year.txt")

#Oilrents <- read_dta("oilReplication data for The Oil Curse - Ross 2012.dta")
#1960-2010 #Was included in Moon and Bak's ATH index, but I will use other data. 

#VDem - Alternative economic data
VDEM <- as.data.frame(readRDS("DV-Dem-CY-Full+Others-v11.rds"))
VDEM <- VDEM %>% 
  dplyr::select(country_id, year, e_migdpgro, e_migdppc, e_total_oil_income_pc, v2clrspct_osp, v2x_polyarchy) #growth, gdp per cap, and oil income + rigourous public administration + demokratiindex
VDEM <- VDEM %>% 
  dplyr::rename(   Vdemgrowth = e_migdpgro,
                       VdemGDP = e_migdppc, #Will also be used to pick who is the host and home in dyads
                       Vdemoil = e_total_oil_income_pc,
                   rigadmin = v2clrspct_osp,
                   demokratiindex = v2x_polyarchy)
VDEM$ccode <- countrycode(VDEM$country_id, origin = 'vdem', destination = 'cown')
VDEM <- VDEM %>% 
  mutate(ccode = ifelse(VDEM$country_id == 198, 340, ccode)) # For Serbia             


# Do not use Banks and Wilson unrest data, as in Moon and Bak. 
# Variable: NAVCO Violent and non-violent campaigns #1890-2019
Navco <- read_excel("DNAVCO 1.3 List.xlsx")

#National capabilities index from the Correlates of War project
cap <- read_dta("DNMC_5_0.dta")%>%
  dplyr::select(ccode, year, cinc)

cap1 <- cap %>%
  dplyr::rename(ccode1 = ccode)

cap2 <- cap %>%
  dplyr::rename(ccode2 = ccode)

#### ATH: Merging for ATH probit ####
## Archigos as base ##

#Make censored variable for leaders still in power at end of observation
archigos <- archigos %>% 
  mutate(stillinpower = ifelse(archigos$exitcode == "Still in Office" , 1, 0))
# 3241 are not censored. 168 are.

#Creat duration (in days) and event (failure) variables
archigos <- archigos %>% 
 mutate(startdate = as.Date(startdate, format = "%Y-%m-%d"), #as.Date makes sure that R reads this as a date.
       enddate = as.Date(enddate, format = "%Y-%m-%d"), 
      tenure = ifelse(stillinpower == 0, 
                       enddate - startdate, #This is to know that the still in power is not right-censored
                       as.Date("2015-12-31", format = "%Y-%m-%d") - startdate), #These are the right censored ones
    failure = ifelse(stillinpower == 0, 1, 0)) # Opposite of the censoring, when the event happens 


archigos$startdate <- as.Date(archigos$startdate) #Make variables date format
archigos$enddate <- as.Date(archigos$enddate)

# Create row for each year a leader was in power
archigos <- archigos %>%
  group_by(rn = row_number()) %>% 
  mutate(year = list(year(startdate):year(enddate))) %>% 
  unnest(cols = year)  %>% 
  ungroup() %>% 
  group_by(obsid) %>% 
  mutate(start = ifelse(year == min(year),
                            startdate, 
                            as.Date(paste(year, "-01-01",sep =""), format = "%Y-%m-%d")),
         stop = ifelse(year == max(year), 
                           enddate, 
                           as.Date(paste(year,"-12-31",sep=""), format = "%Y-%m-%d")),
         start = start - as.numeric(startdate),
         stop = stop - as.numeric(startdate) + start+1,
         failure = ifelse(year == max(year) & stillinpower == 0,1,0)) %>% 
  dplyr::select(obsid, leader, year, ccode, startdate, enddate, tenure, start, stop, failure) # And make dataset with only some of the variables
## Archigos1 with correct failure and "start" og "stop" counts number of days as 
## R wants it for time varying covariates

# Make pastfailure variable
archigos <- archigos %>%
  group_by(ccode) %>% ## using group_by we make sure the lag is within countries
  arrange(ccode, year) %>% ## Sort dataset by ccode and year
  mutate(pastfail = lag(failure), # Leaving first pastfail in every new ccodeobservation be NA 
         #because we don't know what failure was before the dataset begins
         pastfails = cumsum(ifelse(is.na(pastfail),0,pastfail)), #Adds number of failures from beginning of observations
         pastfails10 = runner( x = ifelse(is.na(pastfail),0,pastfail), #number of failure the past ten years
                               f = sum, 
                               k = 10))

## Merge with coup-data ##
coupsdata <- coupsdata %>%  
  mutate(year = as.numeric(year), #Making year and ccode numeric
         ccode = as.numeric(ccode))

#Change coup variable 0-2 (non-attempt, unsucsessful and sucessful) to 0: non-attempt 1:attempt
coupsdata <- coupsdata %>%  
  mutate(coup1 = ifelse(coup1 > 1, 1, coup1), 
         coup2 = ifelse(coup2 > 1, 1, coup2),
         coup3 = ifelse(coup3 > 1, 1, coup3),
         coup4 = ifelse(coup4 > 1, 1, coup4))

# Aggregate attempts per ccode-year in new variable
coupsdata <- coupsdata %>%  
  mutate(coup = coup1 + coup2 + coup3 + coup4)

coupsdata <- coupsdata %>%
  dplyr::select(year, ccode, coup) #New dataset coup = number of coups per year. 
# Have not added variable for if there has been a coup or not at all, but might consider this. 

#Mergeing coup variable into data set
archigos_coups <- left_join(archigos, coupsdata)  %>% #Keeps everything in archigos2 but only relevant rows in coupsdata3
  mutate(coup = ifelse(is.na(coup) & year >= min(coupsdata$year) & ccode %in% coupsdata$ccode, #NAs change to 0 if year is higher than the first year in coupsdata3 and has ccode in coupsdata3
                               0, coup))
table(archigos_coups$coup)  #  0:10601 (before making NAs 0 if in coupsdata3 = 10484) 1:610  2:108 3:21      4 :  3 
table(is.na(archigos_coups$coup)) # 6343 of coups are NA.

## Add NAVCO ##
Navco <- rename(Navco, c(BYEAR="year"))

Navco$ccode <- countrycode(Navco$LOCATION, origin = 'country.name', destination = 'cown') #bruker landnr isteden
Navco <- Navco %>% 
  mutate(ccode = ifelse(Navco$LOCATION == "Ottoman Empire", 640,ccode ), ## Archigos uses Turkey ccode also for the Ottoman empire
         ccode = ifelse(Navco$LOCATION == "Serbia", 340,ccode), ## Archigos uses 340 for Serbia. Modern use is 345. Check later. 
         ccode = ifelse(Navco$LOCATION == "Northern Ireland", 200, ccode)) ## UK, no independent leader.
###Vest-Sahara, Palestina, Tibet, Western Sahara, Natal, Aruba are not added as they do not have independent leaders in Archigos. 

# New variable which is 1 for all Navco events. 
Navco1 <- Navco %>%
  mutate(Navco_event = 1)

#New Navcodataset
Navco2 <- Navco1 %>%
  dplyr::select(year, ccode, Navco_event) 

# Navco has several campaigns per country-year, so we have to aggregate to merge successfully.
Navco3 <- Navco2 %>% 
  dplyr::select(year, ccode, Navco_event) %>%  
  arrange(desc(year, ccode))%>% 
  group_by(year, ccode) %>% 
  summarise(Navco_events = sum(Navco_event), 
            Navco_event = 1, .groups = "keep")# at least 1 event



#Merge, making navco 0 if it covers ccode and year of archigos_coup and NA if they do not overlap. 
archigos_coup_navco <- left_join(archigos_coups, Navco3) %>% 
  mutate(Navco_events = ifelse(is.na(Navco_events) & year >= min(Navco3$year) & ccode %in% Navco3$ccode, #NAs change to 0 if year is higher than the first year in Navco and has ccode in Navco
                               0, Navco_events),
         Navco_event = ifelse(is.na(Navco_event) & year >= min(Navco3$year) & ccode %in% Navco3$ccode, 
                              0, Navco_event)) #Same but for navco_event

## Add UCDP conflict type variable ##
UCDP1 <- cSplit(UCDP, "location", ",", direction = "long") #Split countries where there are several countries on one side of conflict

UCDP1$ccode <- countrycode(UCDP1$location, origin = 'country.name', destination = 'cown') ## Ccodes
UCDP1$ccode <- ifelse(UCDP1$location == "Hyderabad", 750, UCDP1$ccode) ## ccode of India

UCDP2 <- UCDP1 %>% # New dataset
  dplyr::select(year, ccode, type_of_conflict)

UCDP3 <- UCDP2 %>% # Aggration because there are several observations per country-year
  group_by(ccode, year) %>% 
  arrange(desc(ccode,year))%>% 
  summarise(type_of_conflict = toString(unique(type_of_conflict)), .groups = "keep") # can also count each type or make dummy for different types. Concider. 

archigos_coup_navco_ucdp <- left_join(archigos_coup_navco, UCDP3) # Merge
nrow(archigos_coup_navco_ucdp) == nrow(archigos) ## check

#Comparing type of conflict befor and after merge
table(UCDP1$type_of_conflict) #Before: 1: 121  2: 275  3:1850    4:350
table(grepl("1", archigos_coup_navco_ucdp$type_of_conflict)) # 1:32
table(grepl("2", archigos_coup_navco_ucdp$type_of_conflict)) # 2:321
table(grepl("3", archigos_coup_navco_ucdp$type_of_conflict)) # 3:1498
table(grepl("4", archigos_coup_navco_ucdp$type_of_conflict)) # 4:316

# Seperate variables for intrastate and interstate war
archigos_coup_navco_ucdp <- archigos_coup_navco_ucdp %>%
  mutate(interwar = ifelse(grepl("2", type_of_conflict), 1, NA), #interstate war
         intrawar = ifelse(grepl("3", type_of_conflict), 1, NA)) #intrastate war

#  Make all NA = 0 in intrawar and interwar except those that are before the time frame of UCPC 
archigos_coup_navco_ucdp <- archigos_coup_navco_ucdp %>%
  mutate(interwar = ifelse(year >= min(UCDP2$year) & is.na(interwar) & ccode %in% UCDP2$ccode, 0, interwar ), 
         intrawar = ifelse(year >= min(UCDP2$year) & is.na(intrawar) & ccode %in% UCDP2$ccode, 0, intrawar))
nrow(archigos_coup_navco_ucdp) == nrow(archigos) ## check

## Merge in regimetype from Anckar ##
regimedata1 <- regimedata %>% 
  dplyr::rename(ccode = anckar_ccode) %>% 
  dplyr::select(year, ccode, democracy, regimebroadcat, regimenarrowcat)

archigos_coup_navco_ucdp_regime <- left_join(archigos_coup_navco_ucdp, regimedata1)
nrow(archigos_coup_navco_ucdp_regime) == nrow(archigos) # Ckeck

# Make regime type dummies
archigos_coup_navco_ucdp_regime <- archigos_coup_navco_ucdp_regime %>% 
  mutate(Monarchy = ifelse(regimebroadcat == "Absolute monarchy", 1,0),
           Military = ifelse(regimebroadcat == "Military rule", 1,0),
         Party= ifelse(regimebroadcat == "Party-based rule", 1,0),
         Personalist = ifelse(regimebroadcat == "Personalist rule", 1,0),
         Oligarchy = ifelse(regimebroadcat == "Oligarchy", 1,0))

## Add VDEM-data / Growth, GDPpc and oil + rigadmin
archigos_coup_navco_ucdp_regime_Oil_growth_gdp <- left_join(archigos_coup_navco_ucdp_regime, VDEM)

## Create region variable ##
archigos_coup_navco_ucdp_regime_Oil_growth_gdp$region <- countrycode(archigos_coup_navco_ucdp_regime_Oil_growth_gdp$ccode, origin = 'cown', destination = 'region')
# Did not match: 260 (German Federal Republic), 340 (Serbia?), 563, 564, 711, 730 (Korea), 815

## Throw out non-autoritarian regimes ## 
table(archigos_coup_navco_ucdp_regime_Oil_growth_gdp$democracy)
#   0 : 9835 (authoritarian)   1 : 6632 (demo)

ATHdata <- archigos_coup_navco_ucdp_regime_Oil_growth_gdp[archigos_coup_navco_ucdp_regime_Oil_growth_gdp$democracy %in% 0,] 
#9835 observations

##Looking at NAs
sum(is.na(ATHdata$interwar)) #4867
sum(is.na(ATHdata$intrawar)) #4867
sum(is.na(ATHdata$coup)) #4001
sum(is.na(ATHdata$Navco_event)) #2131 #samme på event. 
sum(is.na(ATHdata$Vdemgrowth)) #2549
sum(is.na(ATHdata$VdemGDP)) #2517
sum(is.na(ATHdata$Vdemoil)) #1320

#### ATH: Probit: replication and ATH index seperation plots ####

PrFail <- glm(failure ~ pastfails # As close I get to replicating Moon and Bak (2020). #Aliased coefficients in the model
                + interwar  
                + intrawar  
                + Navco_events 
                + coup 
                + Military 
                + log1p(Vdemoil)
                + poly(start,3) 
                + as.factor(ccode), 
                + as.factor(region),  
              data = filter(ATHdata, year > 1950),
              family = binomial(link = "probit"), 
              y = TRUE)

# My ATH index and adding predictions
modelData <- filter(ATHdata, year > 1950) #So the predictions will match the dataset you make them in
ATHindex <- glm(failure ~ 
                  poly(start, 3) 
                + pastfails10 # Failures the past 10 years
                + interwar  
                + intrawar  
                + Navco_events 
                + coup 
                + Military 
                + Party
                + Personalist
                + Oligarchy #Dummies for regime types. Monarchy is the reference category.
                #+ as.factor(region),
                + poly(start, 3) # tenure to date and not total tenure
                + Vdemgrowth
                + log1p(Vdemoil)
                + log1p(VdemGDP)
                 + as.factor(ccode),
                data = modelData,
                family = binomial(link = "probit"), 
                y = TRUE, na.action = "na.exclude") #The model object keeps the observations not in the model because of missingness. 
modelData$athdatawprob <- predict(ATHindex, type="response") #Predicting  

vif(ATHindex)
#Seperationplot - How well does the model predict - Simple version
P6SeperationATH <- separationplot(pred = fitted(ATHindex), # Have to remove na.exclude in ATHindex probit first for this to work. 
               actual = ATHindex$y)

#### ATH: tables and plots ATH probit (Chapter 6) ##### 

## Descriptive table of ATHdata
desc <- ATHdata %>% 
  dplyr::select(failure, pastfails10, interwar, intrawar, Navco_events, 
         coup, Military, Party, Personalist, Oligarchy, Monarchy, Vdemgrowth, VdemGDP,
        Vdemoil, tenure, ccode) %>% 
  as.data.frame()
stargazer(desc, summary=TRUE, 
          out = "6descriptivestatATH.tex",
          omit.summary.stat = c("p25", "p75"), 
          digits = 1,
          label = "desckvarATH",
          title            = "Descriptive statistics: ATH probit",
          covariate.labels = c("Failure", "Past Failures in last 10 years", "Interwar", "Intrawar", "Campaigns", 
                               "Coup", "Regime type: Military", "Regime type: Party", 
                               "Regime type: Personalist", "Regime type: Oligarchy", 
                               "Regime type: Monarchy (ref)", "GDP growth rate", "GDP per cap",
                              "Oil Production", "Total tenure (days)", "ccode"), #Bruker tenure her, men start i probit
          omit = "ccode")

## Stargazer of ATH probit##
stargazer(ATHindex,
          title="Predicted probability of leadership failure (ATH probit)",
          align=TRUE, 
          type = "latex",
          label = "6ATHprobit",
          out = "6ATHprobit.tex",
          notes.align = "l", 
          notes= c("Standard errors in parentheses"),
          dep.var.labels=c("Autocratic leadership failure"), 
          omit = "ccode", omit.labels = "Country fixed effects",
                    covariate.labels= c("Start", "Start squared", "Start cubed",
                              "Past Failures", "Interwar", 
                              "Intrawar", "Campaign", 
                              "Coup", "Regime type dummy: Military", 
                              "Regime type dummy: Party", 
                              "Regime type dummy: Personalist", 
                              "Regime type dummy: Oligarchy", "GDP Growth", "Log (Oil production)", 
                              "Log (GDP per cap)"), 
          digits = 2, 
          omit.stat=c("f"), 
          no.space=TRUE)


## My themes
My_Theme = theme(axis.title.x = element_text(size = 12), #For all figures
                 axis.title.y = element_text(size = 12),
                 axis.text = element_text(size = 13), 
                 legend.position = "top", 
                 legend.title = element_text(size = 12),
                 legend.text = element_text(size = 13))

My_Theme2 = theme(axis.title.x = element_text(size = 12), #For all figures
                 axis.title.y = element_text(size = 12),
                 axis.text = element_text(size = 11), 
                 legend.title = element_text(size = 12),
                 legend.text = element_text(size = 11))

My_Theme3 = theme(axis.title.x = element_text(size = 12), #For all figures
                  axis.title.y = element_text(size = 12),
                  axis.text = element_text(size = 11), 
                  legend.title = element_blank())

#Predicted probability plot ATH index - 2 countries 
predcountries <- filter(modelData, ccode == "517" | ccode == "700") 
# Rwanda and Afghanisthan 

# Frequency comparing assymetric with NUMBERS \ Original
number_ticks <- function(n) {function(limits) pretty(limits, n)}

predcountries  <- predcountries %>%
  dplyr::rename(Country = ccode) %>%
  mutate(Country = ifelse(Country == "517", "Rwanda", "Afghanistan"))
  
predcountries$Country <- as.character(predcountries$Country)

countriespredplot <- ggplot(predcountries, aes(x=year, y=athdatawprob, group=Country)) +
  geom_line(aes(linetype=Country, color=Country))+
  geom_point(aes(color=Country))+
  xlab("Year") + 
  ylab("Pr(Fail)") +
  theme_bw() + theme(legend.position="top")

countriespredplot + My_Theme +
  ylim(0,1) + xlim(1960, 2010) + scale_x_continuous(breaks=number_ticks(10)) 

#Histogram values athdataprob (in ATH sample)
athdistribution <- ggplot(data=modelData, aes(athdatawprob)) + 
  geom_histogram(binwidth=0.02, fill="#FF6666", alpha=1) + 
  theme_bw() +
  scale_y_continuous(breaks=number_ticks(10)) +
  scale_x_continuous(breaks=number_ticks(10)) +
  scale_fill_manual(values = ("#FF6666"))

athdistribution + My_Theme + xlab("Pr(Fail)") + #These are both in force and not
  ylab("Frequency")

# ath 1950-2005
athtime <- ggplot(modelData, aes(x=year, y=athdatawprob)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) +
  xlab("Year") + #These are both in force and not
  ylab("Pr(Fail)") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

athtime + My_Theme + 
  ylim(0,1)

#timetestsATHvariables
simplertime <- ggplot(modelDatasimple, aes(x=year, y=athdatasimple)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) +
  xlab("Year") + #These are both in force and not
  ylab("Simpler Pr(Fail)") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

simplertime + My_Theme + 
  ylim(0,1) 

#Vdemoil goes to about 2008


#### Treaties: Importing Tax Treaty Explorer Dataset ####

# Importing data 
treatydata <- read.csv("Dtaxtreatydata_downloaded031021.csv")
# SIGNED: 1945-2020

treatydata1 <- treatydata %>% #renaming relevant variables
  dplyr::rename(year = SIGNED, 
                type = TYPE, 
                reference = REFERENCE, 
                treatyID = TREATYID, 
                effective = EFFECTIVE, 
                status = STATUS, 
                source = SOURCE, 
                WHTrates = WHTRATES, 
                Other = OTHER)

#Give treaties 1 of june the year they where signed as date. 
treatydata1 <- treatydata1 %>%
  mutate(signdate = as.Date(paste(year,"-01-01",sep=""), format = "%Y-%m-%d"))

#Make ccodes and make c1ccode og c2ccode
treatydata2 <- treatydata1 %>%
  mutate(ccode1 = countrycode(C1CODE, origin = 'iso3c', destination = 'cown'),
         ccode2 = countrycode(C2CODE, origin = 'iso3c', destination = 'cown'))
# 1: In countrycode(C1CODE, origin = "iso3c", destination = "cown") :
#Some values were not matched unambiguously: , #CUW, FRO (Faroe Islands), HKG (Hong Kong), IMN (Isle of Man), 
#MAC (Macao), PSE (Occupied palistinian territory), UNK + same for C2CODE
treatydata3 <- treatydata2 %>%
  mutate(ccode1 = ifelse(treatydata1$C1CODE == "SRB", 340, ccode1),
         ccode2 = ifelse(treatydata1$C2CODE == "SRB", 340, ccode2))         

## Original treaties ##
#Only analyse using Original, as ammended treaties are added in addition to original, not istead of
treatydataori <- filter(treatydata3, type == "Original")


## Original treaties and pre-independence treaties ##
treatydatapre <- filter(treatydata3, type == "Original" | type == "Pre-independence")

## Percent signed treaties between 1992 and 2012 (To see if it is the same as in Barthel)
library( data.table )
percentyear1 <- treatydatapre %>%
  dplyr::filter(year < 2013)
percentyear <- percentyear1 %>%
  mutate(divide = ifelse(year < 1992, "old", "new"))
setDT( percentyear )[ , 100 * .N / nrow( percentyear ), by = divide ]

## Percent signed since 1990 with the newer years
percentyear <- treatydatapre %>%
  mutate(divide = ifelse(year < 1990, "old", "new"))
setDT( percentyear )[ , 100 * .N / nrow( percentyear ), by = divide ]

## How many have mandatory arbitration (art.25)? 
mandatory <- treatydatapre %>%
  filter(X25i == "YES")

#### Treaties: Plot for all original and pre-independence treaties over time ####
treatydatapreplot <- treatydatapre %>%
  mutate(treaty = 1)

plot <- ggplot(data=treatydatapreplot, aes(year)) + 
  geom_histogram(binwidth=1, fill="red", alpha=0.6) + 
theme_bw() +
  scale_y_continuous(breaks=number_ticks(10)) +
  scale_x_continuous(breaks=number_ticks(10)) +
  scale_fill_manual(values = ("red"))

plot + My_Theme + xlab("Year signed") + #These are both in force and not
  ylab("Frequency")

#### Treaties: Figure showing treaties for different numbers on democracy index (Chapter 4) Original ####

#New dataset so each treaty has two rows
startdoubletreaty <- treatydataori %>%
  dplyr::select(ccode1, year) %>%
  dplyr::rename(ccode = ccode1) 

startdoubletreaty2 <- treatydataori %>%
  dplyr::select(ccode2, year) %>%
  dplyr::rename(ccode = ccode2)

doubletreaty <- rbind(startdoubletreaty, startdoubletreaty2)

table(doubletreaty$ccode) #ccode1 = 38 NA #year = 0

doubletreaty$dups <- ifelse(duplicated(dplyr::select(doubletreaty, ccode, year)),1,0)
check <- dplyr::filter(doubletreaty, dups == 1)

#New dataset for VDEM only with year, ccode and democracy index
demoindexdata <- VDEM %>%
  dplyr::select(ccode, year, demokratiindex)

#figurechapter4 <- left_join(doubletreaty, demoindexdata)
alternativefig4 <- left_join(Original_hostasym2, demoindexdata) #figure with only one side of the treaties

democracyindexplot4 <- ggplot(alternativefig4, aes(x=year, y = demokratiindex)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) +
  xlab("Year") + #These are both in force and not
  ylab("Democracy index") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw()

democracyindexplot4 + My_Theme + scale_x_continuous(breaks=number_ticks(8)) + scale_y_continuous(breaks=number_ticks(5))

#### Original Treaties: Richest Dyad is defined as the home country (Chapter 6) ####

VDEMsmall <- VDEM %>% # New dataset with only year, country and GDP per cap (oil and growth are only used in ATH index)
  dplyr::select(ccode, VdemGDP, year)

## Original treaties (without pre-independence): treatydataori

## Making incomelevels NUMERIC VERSION ##
treatydataori_dyad <- VDEMsmall %>% #Make correct historical GDP levels for C1
  dplyr::rename(ccode1 = ccode,
                GDP1 = VdemGDP) %>%
  dplyr::select(ccode1, year, GDP1) %>%
  right_join(treatydataori)

## Adding cincvalue (for robustness test) 1
treatydataori_dyad <- left_join(treatydataori_dyad, cap1) %>%
  dplyr::rename(cinc1 = cinc)

check <- dplyr::filter(treatydataori_dyad, is.na(ccode2))

treatydataori_dyad1 <- VDEMsmall %>% #Make correct historical GDP levels for C2
  dplyr::rename(ccode2 = ccode,
                GDP2 = VdemGDP) %>%
  dplyr::filter(is.na(ccode2) == FALSE) %>% ## don't want to merge on the NAs
  dplyr::select(ccode2, year, GDP2) %>%
  right_join(treatydataori_dyad)

## Adding cincvalue (for robustness test) 2
treatydataori_dyad1 <- left_join(treatydataori_dyad1, cap2) %>%
  dplyr::rename(cinc2 = cinc)

#### Dataset with highest GDP is home country and the other is host (ccode)
## Original treaties (with GDPlevels): treatydataori_dyad

Original_hosthome2 <- treatydataori_dyad1 %>% #New variables for host and home
  mutate(home = ifelse(GDP1 > GDP2, ccode1, ccode2),
         ccode = ifelse(GDP1 < GDP2, ccode1, ccode2)) #Host has to be ccode to enable merge!

#Make variable which is 1 for all treaties
Original_hostasym1 <- Original_hosthome2 %>%
  mutate(treaty = 1,## When merging all observations with treaty will be 0 and rest will be NA
         cincdif = ifelse(cinc1 > cinc2, cinc1-cinc2, cinc2-cinc1),#For robustnesstest. make cincdif
                          GDPdif = ifelse(GDP1 > GDP2, GDP1-GDP2, GDP2-GDP1)) #For robusthet make GDPdif

#Aggregate data for those ccode-years which have more treaties.
Original_hostasym2 <- Original_hostasym1 %>%
  dplyr::select(year, ccode, treaty, source, home, cincdif, GDPdif, signdate) %>% 
  arrange(desc(year, ccode)) %>% 
  group_by(year, ccode) %>% 
  summarise(treaties = sum(treaty),
            treaty = 1, 
            signdate = mean(signdate),
            source = mean(source, na.rm = TRUE),
            home = list(home),
            cincdif = mean(cincdif, na.rm = TRUE),
            GDPdif = mean(GDPdif, na.rm = TRUE), .groups = "keep") # making a list

table(Original_hostasym2$ccode) #96 different countries
#### Main Model: Source is NA (not 0)! Merge treatydata (Original_hostasym2) with modelData (ATH probit) Add control variables and IV #### 
#Merge this with modelData, where treaties-variable is 0 for the period and countries which are covered by Original_hosthomeasym2
#and NA for what it does not cover. # left_join here to delete observations in the Original_hostasym2 that are not in the modelData
#doubletreaty has all the ccodes in treatydata as ccode
masterdatadyad <- left_join(modelData, Original_hostasym2) %>%
  mutate(treaties = ifelse(is.na(treaties) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                           treaties), # put treaty to 0 if year is bigger than the first year in Original_hosthome2
         treaty = ifelse(is.na(treaty) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                         treaty))
        
masterdatadyad1 <- masterdatadyad  %>%
  mutate(treaties = ifelse(treaties > 0 & enddate > signdate & startdate < signdate, treaties, 0),
         treaty = ifelse(treaty > 0 & enddate > signdate & startdate < signdate, treaty, 0),
         source = ifelse(source > 0 & enddate > signdate & startdate < signdate, source, NA), 
         cincdif = ifelse(cincdif > 0 & enddate > signdate & startdate < signdate, cincdif, 0),
         GDPdif = ifelse(GDPdif > 0 & enddate > signdate & startdate < signdate, GDPdif, 0),
         signdate = ifelse(signdate > 0 & enddate > signdate & startdate < signdate, signdate, NA))


table(masterdatadyad1$treaty)
#619
sum(table(masterdatadyad1$signdate))
# 585 has signing data.
sum(table(masterdatadyad1$source)) #575 has source index values.

##Add control variables and IV ##
## 1. Bureaucratic quality
#rigadmin is already in masterdatadyad1

## Add regional competition measured by number of treaties in region past 10 years (with one year lag)
dyadburcomp <- masterdatadyad1 %>%
  group_by(region) %>% ## using group_by we make sure the lag is within countries
  arrange(region, year) %>%
  mutate(competition = lag(treaties), # Leaving first treaties in every new region-year be NA 
         #because we don't know what failure was before the dataset begins
         competitions = cumsum(ifelse(is.na(competition),0,competition)), #Adds number of treaties from beginning of observations
         competition10 = runner( x = ifelse(is.na(competition),0,competition), #number of treaties past ten years
                                 f = sum, 
                                 k = 10))

dyadburcomp1 <- dyadburcomp %>% #Lag the effect with one year
  mutate(comp10lag = ifelse(year == lag(year), competition10, lag(competition10)))

## Add IV - BIT-signing data 
BITdata <- read_excel("DUNCTAD_IIA3.xlsx")
# Make year-variable 
BITdata$year <- format(as.Date(BITdata$Date.of.signature, format="%d/%m/%Y"),"%Y")
BITdata$year <- as.numeric(BITdata$year)
BITdata$Date.of.signature <- format(as.Date(BITdata$Date.of.signature,format="%d/%m/%Y"), "%Y-%m-%d")

#Make one observation per signing countries - all agreements are counted twice
BITdata1 <- BITdata %>%
  dplyr::select(Parties.1, year, Date.of.signature) %>%
  dplyr::rename(ccode = Parties.1) 

BITdata2 <- BITdata %>%
  dplyr::select(Parties.2, year, Date.of.signature) %>%
  dplyr::rename(ccode = Parties.2)

BITdataboth <-rbind(BITdata1, BITdata2) #bind: double length

#Make correct ccode 
BITdataboth$ccode <- countrycode(BITdataboth$ccode, origin = "country.name", destination = "cown")

sum(is.na(BITdataboth$ccode)) # Only 629 NAs

BITdataboth1 <- BITdataboth %>%
  mutate(BITPTA = 1) ## When merging all observations with BIT will be 1 and rest will be NA

dyadburcitcompbit0 <- left_join(dyadburcomp1, BITdataboth1) 

dyadburcitcompbit00 <- dyadburcitcompbit0  %>%
  mutate(BITPTA = ifelse(BITPTA > 0 & enddate > Date.of.signature & startdate < Date.of.signature, BITPTA, NA))

#new mean variables
dyadburcitcompbit1 <- dyadburcitcompbit00 %>%
  filter(is.na(ccode) == FALSE, 
         is.na(year) == FALSE) %>%
  arrange(desc(leader, year)) %>% 
  group_by(leader, year) %>% 
  mutate(region = toString(unique(region)),
         athdatawprob= mean(athdatawprob), 
         rigadmin = mean(rigadmin),
         Military = mean(Military), 
         Monarchy = mean(Monarchy),
         Party = mean(Party),
         Oligarchy = mean(Oligarchy),
         Personalist = mean(Personalist), 
         VdemGDP = mean(VdemGDP), 
         cincdif = mean(cincdif), 
         ccode = mean(ccode),
         year = mean(year), 
         start = mean(start),
         coup = mean(coup), 
         comp10lag = mean(comp10lag),
         source = mean(source)) 

dyadburcitcompbit12 <- dyadburcitcompbit1 %>%
  dplyr::select(obsid, 
                leader,
                ccode,
                year,
                region,
                athdatawprob, 
                rigadmin,
                Military, 
                Monarchy,
                Party,
                Oligarchy,
                Personalist, 
                VdemGDP, 
                cincdif, 
                coup, 
                comp10lag,
                treaty,
                treaties,
                source,
                BITPTA,
                start
                )

dyadburcitcompbitdist <- distinct(dyadburcitcompbit12, .keep_all = FALSE)

dyadburcitcompbit <- dyadburcitcompbitdist %>%
  group_by(leader)%>%
  arrange(leader, year) %>% 
  mutate(pastbit = lag(BITPTA),
         pastbits = cumsum(ifelse(is.na(pastbit),0,pastbit)))

# Throw out treaties where ccode is not the countries in the Tax Treaties Explorer data
# 1. Make ccode of all countries in sample (118)
taxcountries <-  c("Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", 
                   "Azerbaijan", "Bangladesh", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil",
                   "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Central African Republic", "Chad", "China", 
                   "Colombia", "Comoros", "Côte d'Ivoire", "Democratic People’s Republic of Korea", "Democratic Republic of Congo", 
                   "Djibouti", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini",
                   "Ethiopia", "Federal States of Micronesia", "Gabon", "Georgia", "Ghana", "Guatemala", "Guinea", "Guinea-Bissau", 
                   "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran", "Iraq", "Jordan", "Kenya", "Kiribati", "Kosovo", "Kyrgyz Republic",
                   "Lao PDR", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Maldives", "Mali", "Marshall Islands",
                   "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Nicaragua", 
                   "Niger", "Nigeria", "North Macedonia", "Pakistan", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Republic of Congo", 
                   "Rwanda", "Samoa", "São Tomé and Principe", "Senegal", "Seychelles", "Sierra Leone", "Solomon Islands", "Somalia", "South Africa",
                   "South Sudan", "Sri Lanka", "Sudan", "Syrian Arab Republic", "Tajikistan", "Tanzania", "Thailand", "The Gambia", 
                   "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uganda", "Ukraine", "Uzbekistan", 
                   "Vanuatu", "Venezuela", "Vietnam", "West Bank and Gaza", "Yemen", "Zambia", "Zimbabwe")
taxcountries <- as.data.frame(taxcountries)
#2. making ccode 
taxcountries$ccode <- countrycode(taxcountries$taxcountries, origin = 'country.name', destination = 'cown')
# West bank was not matched

taxcountries$ccodesample <- taxcountries$ccode

#3. throw out if they do not match
dyadburcitcompbitsample <- left_join(dyadburcitcompbit, taxcountries)
mainsample <- dyadburcitcompbitsample %>%
  filter(ccodesample >= 0)

sum(is.na(dyadburcitcompbitsample$ccodesample)) #1362 removed observations

#### Main Model: Heckman maximum likelyhood model ####

set.seed(1)
mlisnasource <- selection(!is.na(source) ~ athdatawprob #Bør det være dette eller treaty? Evt fjerne 0ene for treaty
                + pastbits
                + poly(year, 3)
                + rigadmin
                + Military #All other regime types as ref
                + comp10lag # treaties signed past 10 years in region, lagged by one year
                + log(VdemGDP), 
                source ~ athdatawprob
                + poly(year, 3)
                + rigadmin
                + Military 
                + comp10lag 
                + log(VdemGDP), 
                data = filter(mainsample, VdemGDP > 0), 
                method = "ml")

table(mainsample$treaty) # 3344 0's og 623 1's

#Main model
mltreaty <- selection(treaty ~ athdatawprob 
                          + pastbits
                          + poly(year, 3)
                          + rigadmin
                          + Military #All other regime types as ref
                          + comp10lag # treaties signed past 10 years in region, lagged by one year
                          + log(VdemGDP),  
                          source ~ athdatawprob
                          + poly(year, 3)
                          + rigadmin
                          + Military 
                          + comp10lag 
                          + log(VdemGDP), 
                          data = filter(mainsample, VdemGDP > 0), 
                          method = "ml")
summary(mltreaty, diagnostics = TRUE)

#No instrument
noinstrument <- selection(treaty ~ athdatawprob
                          + poly(year, 3)
                          + rigadmin
                          + Military #All other regime types as ref
                          + comp10lag # treaties signed past 10 years in region, lagged by one year
                          + log(VdemGDP), 
                          source ~ athdatawprob
                          + poly(year, 3)
                          + rigadmin
                          + Military 
                          + comp10lag 
                          + log(VdemGDP), 
                          data = filter(mainsample, VdemGDP > 0), 
                method = "ml")
summary(noinstrument, diagnostics = TRUE)

## Waldtest / stage 1 models with and without instrument. 
test1 <- lm(treaty ~ athdatawprob
+ pastbits
+ poly(year, 3)
+ rigadmin
+ Military 
+ comp10lag 
+ log(VdemGDP),  
data = filter(mainsample, VdemGDP > 0)) 

test2 <- lm(treaty ~ athdatawprob
            + poly(year, 3)
            + rigadmin
            + Military 
            + comp10lag
            + log(VdemGDP),  
            data = filter(mainsample, VdemGDP > 0)) 

waldtest(test1, test2, test = "F")
               
 # The 2-step Heckman
heckvan <- heckit(treaty ~ athdatawprob
                 + pastbits
                 + poly(year, 3)
                 + rigadmin
                 + Military #All other regime types as ref
                 + comp10lag # treaties signed past 10 years in region, lagged by one year
                 + log(VdemGDP),  
                 source ~ athdatawprob
                 + poly(year, 3)
                 + rigadmin
                 + Military 
                 + comp10lag 
                 + log(VdemGDP), 
                 data = filter(mainsample, VdemGDP > 0), 
                 method = "2-step")


#### Clustring standard errors ####

# Main model
modelMatrix <- filter(mainsample, VdemGDP > 0) %>% 
  dplyr::select(treaty, athdatawprob, pastbits, year, rigadmin, Military, comp10lag, 
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  na.omit()

mltreaty$leadercluster <- vcovCL(mltreaty, cluster = modelMatrix$leader)

#cluster on country
mltreaty$countrycluster <- vcovCL(mltreaty, cluster = modelMatrix$ccode)

#cluster on leader noinstrument
noinstrument$leadercluster <- vcovCL(noinstrument, cluster = modelMatrix$leader)

#cluster on leader 2-step
heckvan$leadercluster <- vcovCL(heckvan, cluster = modelMatrix$leader)

#cluster on leader mlisnasource
modelMatrixmlisnasource <- filter(mainsample, VdemGDP > 0) %>% 
  mutate(sourceNotNA = ifelse(is.na(source), 0, 1)) %>% 
  dplyr::select(sourceNotNA, athdatawprob, pastbits, year, rigadmin, Military, comp10lag, 
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(sourceNotNA == 0, 0, source)) %>% 
  na.omit()
mlisnasource$leadercluster <- vcovCL(mlisnasource, cluster = modelMatrixmlisnasource$leader)

#### Main model: Tables and plots ####

## Descriptive table of variables in Main model
desc1 <- mainsample %>% 
  dplyr::select(treaty, source, athdatawprob, 
                BITPTA, pastbits, rigadmin, Military, Party, Personalist,
                Oligarchy, Monarchy, comp10lag, VdemGDP, year, ccode)%>%  
  as.data.frame()
stargazer(desc1, summary=TRUE, 
          out = "6descriptiveheckman.tex",
          omit.summary.stat = c("p25", "p75"), 
          digits = 1,
          label = "descvarheck",
          title            = "Descriptive statistics for variables in main model",
          covariate.labels = c("DTA signing", "Source index", "Pr(Fail)", "BIT signing", "IV: Past BIT signing",
                               "Bureaucratic quality", "Regime type: Military", "Regime type: Party", 
                               "Regime type: Personalist", "Regime type: Oligarchy", 
                               "Regime type: Monarchy (ref)", "Regional competition", "GDP per cap",
                               "Year", "Host countries"), 
          omit = "ccode")

# Print list of countries in main sample
mainsample$countryccode <- countrycode(mainsample$ccode, origin = "cown", destination = "country.name")
countryccode <- unique(mainsample$countryccode) 
writeLines(countryccode, "countrylist.tex") 
dfcountrylist <-  as.data.frame(countryccode)

## BUTON of Heckman - main model 
tobit_2O <- mltreaty
tobit_2O$param$index$betaO <- mltreaty$param$index$betaS
tobit_2O$param$index$betaS <- mltreaty$param$index$betaO

stargazer(mltreaty, tobit_2O,    
          se =list(sqrt(diag(mltreaty$leadercluster))[mltreaty$param$index$betaS],
                   sqrt(diag(mltreaty$leadercluster))[mltreaty$param$index$betaO]), 
          selection.equation = TRUE, 
          out = "6Heckmanmaximum.tex",
          title="Heckman (ML)",
          label = "mainmodel",
          column.labels = c("Selection equation", "Outcome equation"),
          dep.var.labels = c("DTA signing Source index"),# Is adjusted in Latex
          covariate.labels= c("Pr(Fail)", "IV: Past BIT signing", "Year", "Year squared", "Year cubed",
                              "Bureaucratic quality",
                              "Regime type: Military", 
                              "Regional competition", "Log GDP per cap"), 
          digits = 2, 
          omit.stat=c("f"), 
          notes.align = "l", 
          notes= c("Clustered standard errors", "on leader in parentheses"),
          no.space=TRUE ) 
          

## BUTON of Heckman - Clustered standard errors on Country
tobit_2O <- mltreaty
tobit_2O$param$index$betaO <- mltreaty$param$index$betaS
tobit_2O$param$index$betaS <- mltreaty$param$index$betaO

stargazer(mltreaty, tobit_2O, 
              se =list(sqrt(diag(mltreaty$countrycluster))[mltreaty$param$index$betaS],
                   sqrt(diag(mltreaty$countrycluster))[mltreaty$param$index$betaO]), 
          selection.equation = TRUE, 
          out = "6Heckmacountrycluster.tex",
          title="Heckman (ML) Standard errors clustered on country",
          label = "mainmodel",
          column.labels = c("Selection equation", "Outcome equation"),
          dep.var.labels = c("DTA signing Source index"),# Is adjusted in Latex
          covariate.labels= c("Pr(Fail)", "IV: Past BIT signing", "Year", "Year squared", "Year cubed",
                              "Bureaucratic quality",
                              "Regime type: Military", 
                              "Regional competition", "Log GDP per cap"), 
          digits = 2, 
          omit.stat=c("f"), 
          notes.align = "l", 
          notes= c("Clustered standard errors", "on country in parentheses"),
          no.space=TRUE)

## BUTON of Heckman - maximum likelyhood estimation (!is.na(source))
tobit_2Omlisnasource <- mlisnasource
tobit_2Omlisnasource$param$index$betaO <- mlisnasource$param$index$betaS
tobit_2Omlisnasource$param$index$betaS <- mlisnasource$param$index$betaO

stargazer(mlisnasource, tobit_2Omlisnasource, 
          se =list(sqrt(diag(mlisnasource$leadercluster))[mlisnasource$param$index$betaS],
                   sqrt(diag(mlisnasource$leadercluster))[mlisnasource$param$index$betaO]),
          selection.equation = TRUE, 
          title="Heckman (ML) With value on Source index as dependent var",    
          out = "AHeckisna.tex",
          label = "isnasource",
          column.labels = c("Selection equation", "Outcome equation"),
          dep.var.labels = c("Value on Source Index Source index"),# Is adjusted in Latex
          covariate.labels= c("Pr(Fail)", "IV: Past BIT signing", "Year", "Year squared", "Year cubed",
                              "Bureaucratic quality",
                              "Regime type: Military", 
                              "Regional competition", "Log GDP per cap"), 
          digits = 2, 
          omit.stat=c("f"), 
          notes.align = "l", 
          notes= c("Clustered standard errors", "on leader in parentheses"),
          no.space=TRUE)

## BUTON of Heckman - Without instrument
tobit_2Onoinstrument <- noinstrument
tobit_2Onoinstrument$param$index$betaO <- noinstrument$param$index$betaS
tobit_2Onoinstrument$param$index$betaS <- noinstrument$param$index$betaO

stargazer(noinstrument, tobit_2Onoinstrument, 
          se =list(sqrt(diag(noinstrument$leadercluster))[noinstrument$param$index$betaS],
                   sqrt(diag(noinstrument$leadercluster))[noinstrument$param$index$betaO]),
          selection.equation = TRUE, title="Heckman (ML) Without instrument",    
          out = "Anoinstrumentheckman.tex",
          label = "noinstrument",
          column.labels = c("Selection equation", "Outcome equation"),
          dep.var.labels = c("DTA signing Source index"),# Is adjusted in Latex
          covariate.labels= c("Pr(Fail)", "Year", "Year squared", "Year cubed",
                              "Bureaucratic quality",
                              "Regime type: Military", 
                              "Regional competition", "Log GDP per cap"), 
          digits = 2, 
          omit.stat=c("f"), 
          notes.align = "l", 
          notes= c("Clustered standard errors", "on leader in parentheses"),
          no.space=TRUE)

## BUTON of Heckman - 2step 
tobit_2Oheckvan <- heckvan
tobit_2Oheckvan$param$index$betaO <- heckvan$param$index$betaS
tobit_2Oheckvan$param$index$betaS <- heckvan$param$index$betaO

stargazer(heckvan, tobit_2Oheckvan, selection.equation = TRUE, 
          se =list(sqrt(diag(heckvan$leadercluster))[heckvan$param$index$betaS],
                   sqrt(diag(heckvan$leadercluster))[heckvan$param$index$betaO]),
          title="Heckman (2-step)",    
          out = "A2stepheck.tex",
          label = "2step",
          column.labels = c("Selection equation", "Outcome equation"),
          dep.var.labels = c("DTA signing Source index"),# Is adjusted in Latex
          covariate.labels= c("Pr(Fail)", "IV: Past BIT signing", "Year", "Year squared", "Year cubed",
                              "Bureaucratic quality",
                              "Regime type: Military", 
                              "Regional competition", "Log GDP per cap"), 
          digits = 2, 
          omit.stat=c("f"), 
          notes.align = "l", 
          notes= c("Clustered standard errors", "on leader in parentheses"),
          no.space=TRUE)


# Distribution of values Treaty (in main sample)
treatyplot <- mainsample %>% #change with early treaty data if you want hist not in sample
  filter(treaty == 1 | treaty == 0) 

treatydistribution <- ggplot(data=treatyplot, aes(treaty)) + 
  geom_histogram(binwidth=0.5, fill = "#FF6666", alpha = 1) + 
  theme_bw() +
  scale_y_continuous(breaks=number_ticks(10)) +
  scale_x_continuous(breaks=number_ticks(2)) +
  scale_fill_manual(values = ("#FF6666"))

treatydistribution + My_Theme + xlab("DTA signing") + 
  ylab("Frequency")

#Histogram values Source index (in sample)
sourcedistribution <- ggplot(data=mainsample, aes(source)) + 
  geom_histogram(binwidth=0.02, fill = "#FF6666", alpha = 1) + 
  theme_bw() +
  scale_y_continuous(breaks=number_ticks(10)) +
  scale_x_continuous(breaks=number_ticks(10)) +
  scale_fill_manual(values = ("#FF6666"))

sourcedistribution + My_Theme + xlab("Source Index") +
  ylab("Frequency")

#BIT signing frequency
BITdataplot <- mainsample %>% #Change sample with BITdata 
  filter(BITPTA == 1) #if you want figure for whole data and not in main sample

BITdataplotplot <- ggplot(data=BITdataplot, aes(year)) + 
  geom_histogram(binwidth=1, fill="#FF6666", alpha=1) + 
  theme_bw() +
  scale_y_continuous(breaks=number_ticks(10)) +
  scale_fill_manual(values = ("#FF6666"))

BITdataplotplot + My_Theme + xlab("Year signed") + 
  ylab("Frequency") +
  scale_x_continuous(breaks=number_ticks(8))


res <- residuals(heckvan, part = "outcome")
resdata <- as.data.frame(res)

#Histogram residuals second stage heckman
normalresplot <- ggplot(data=resdata, aes(res)) + 
  geom_histogram(binwidth=0.015, fill = "#FF6666", alpha = 1) + 
  theme_bw() +
  scale_y_continuous(breaks=number_ticks(10)) +
  scale_x_continuous(breaks=number_ticks(10)) +
  scale_fill_manual(values = ("#FF6666"))

normalresplot + My_Theme + xlab("Residuals of second stage Heckman") +
  ylab("Frequency")


#### Predicted probabilities scenario plots. Heckman main model  ####

# First a scatterplot with Predicted values and Autocratic time horizon (Loess)
number_ticks <- function(n) {function(limits) pretty(limits, n)}

probitplot <- ggplot(probitdata, aes(y=predprobit, x=athdatawprob)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) +
  ylab("Predicted probability of tax DTA signing") + 
  xlab("Pr(Fail)") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 1) +
  theme_bw() + theme(legend.position="top")

probitplot + My_Theme +
  ylim(0,1) + xlim(0,1)

#Predicted probabilities plot stage 1
firstStageBetas <- coefficients(heckvan)[attr(coefficients(heckvan),"index")$betaS] ## Choosing which coefficients in the 2-step model 

#clusterd standard errors
heckvan$leadercluster <- vcovCL(heckvan, cluster = modelMatrix$leader)
firstStageVCov <- heckvan$leadercluster[attr(coefficients(heckvan),"index")$betaS, 
                                         attr(coefficients(heckvan),"index")$betaS]
  
firstStageSimb <- mvrnorm(n = 1000, 
                          mu = firstStageBetas, 
                          Sigma = firstStageVCov)
firstStageXrange <- seq(from = min(filter(mainsample, VdemGDP > 0)$athdatawprob, na.rm = TRUE), 
                        to = 1,#max(filter(dyadburcitcompbit, VdemGDP > 0)$athdatawprob, na.rm = TRUE), 
                        length.out = 50)
firstStageX <- cbind(1, # Intercept
                     firstStageXrange, #athdatawprob
                     mean(filter(mainsample, VdemGDP > 0)$pastbits, na.rm = TRUE), 
                     mean(poly(filter(mainsample, VdemGDP > 0)$year, 3)[,1]),
                     mean(poly(filter(mainsample, VdemGDP > 0)$year, 3)[,2]),
                     mean(poly(filter(mainsample, VdemGDP > 0)$year, 3)[,3]), 
                     mean(filter(mainsample, VdemGDP > 0)$rigadmin, na.rm = TRUE),
                     0, # Military; 
                     mean(filter(mainsample, VdemGDP > 0)$comp10lag, na.rm = TRUE),
                     mean(log(filter(mainsample, VdemGDP > 0)$VdemGDP), na.rm = TRUE))

firstStageXBetaMatrix <- firstStageX  %*% t(firstStageSimb)
firstStagePredY <- pnorm(firstStageXBetaMatrix) 

firstStagePlotValues <- t(apply(firstStagePredY, MARGIN = 1, FUN = quantile, probs = c(.025,.5,.975)))

firstStagePlotValues <- as.data.frame(firstStagePlotValues) %>% 
  mutate(athdatawprob = firstStageXrange)

ggplot(firstStagePlotValues, 
       aes(x = athdatawprob, 
           y = `50%`, 
           ymin =  `2.5%`, 
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.5, fill = "#999999" )+
  geom_line()+
  ylab("Predicted probability of DTA signing") +
  xlab("Pr(Fail)") + 
  theme_classic() + My_Theme

#Predicted probabilities plot stage 2
secondStageBetas <- coefficients(heckvan)[attr(coefficients(heckvan),"index")$betaO] ## Choosing which coefficients in the 2-step model 

#clusterd standard errors
heckvan$leadercluster <- vcovCL(heckvan, cluster = modelMatrix$leader)
secondStageVCov <- heckvan$leadercluster[attr(coefficients(heckvan),"index")$betaO, 
                                        attr(coefficients(heckvan),"index")$betaO]
secondStageSimb <- mvrnorm(n = 1000, 
                           mu = secondStageBetas, 
                           Sigma = secondStageVCov)
secondStageXrange <- seq(from = min(filter(mainsample, VdemGDP > 0)$athdatawprob, na.rm = TRUE), 
                         to = max(filter(mainsample, VdemGDP > 0)$athdatawprob, na.rm = TRUE), 
                         length.out = 50)
secondStageX <- cbind(1, # Intercept
                      secondStageXrange, #athdatawprob
                      # mean(filter(dyadburcitcompbit, VdemGDP > 0)$pastbits, na.rm = TRUE), #Is not included in stage 2
                      mean(poly(filter(mainsample, VdemGDP > 0)$year, 3)[,1]),
                      mean(poly(filter(mainsample, VdemGDP > 0)$year, 3)[,2]),
                      mean(poly(filter(mainsample, VdemGDP > 0)$year, 3)[,3]), 
                      mean(filter(mainsample, VdemGDP > 0)$rigadmin, na.rm = TRUE),
                      0, # Military; 
                      mean(filter(mainsample, VdemGDP > 0)$comp10lag, na.rm = TRUE),
                      mean(log(filter(mainsample, VdemGDP > 0)$VdemGDP), na.rm = TRUE))

secondStageXBetaMatrix <- secondStageX  %*% t(secondStageSimb)
#firstStagePredY <- pnorm(firstStageXBetaMatrix) 

secondStagePlotValues <- t(apply(secondStageXBetaMatrix, MARGIN = 1, FUN = quantile, probs = c(.025,.5,.975)))

secondStagePlotValues <- as.data.frame(secondStagePlotValues) %>% 
  mutate(source = secondStageXrange)

ggplot(secondStagePlotValues, 
       aes(x = source, 
           y = `50%`, 
           ymin =  `2.5%`, 
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.5, fill = "#999999")+
  geom_line()+
  ylab("Source index") +
  xlab("Pr(Fail)") +
  theme_classic() + My_Theme


## ROC for probit
nonadata <- probitdata %>% 
  drop_na(treaty, leader, ccode, athdatawprob, pastbits, year, rigadmin, Military, Monarchy, Oligarchy, Personalist, Party, comp10lag, VdemGDP)

predicted_data1 <- predict(probit, type = "response")
predandrealdata <- cbind(nonadata, predicted_data1)
# Basic ROC
basicplot <- ggplot(predandrealdata, 
                    aes(d =treaty, 
                        m = predicted_data1)) +
  geom_roc(labelround = 2, colour = "#FF6666") 
# Nicer ROC with AUC
basicplot + 
  style_roc(ylab = "Sensivity") + 
  theme(axis.text = element_text(colour = "#000000")) +
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", 
                         round(calc_auc(basicplot)$AUC, 2))) +
  scale_x_continuous("Specificity", 
                     breaks = seq(0, 1, by = .1)) + My_Theme3

#### Pre and original Treaties: Richest Dyad is defined as the home country - in order to make indecis over time (Chaper 6) ####

VDEMsmallpre <- VDEM %>% # New dataset with only year, country and GDP per cap (oil and growth are only used in ATH index)
  dplyr::select(ccode, VdemGDP, year)

## Making incomelevels NUMERIC VERSION ##
PREtreatydatapre_dyad <- VDEMsmallpre %>% #Make correct historical GDP levels for C1
  dplyr::rename(ccode1 = ccode,
                GDP1 = VdemGDP) %>%
  dplyr::select(ccode1, year, GDP1) %>%
  right_join(treatydatapre)

PREtreatydatapre_dyad1 <- VDEMsmallpre %>% #Make correct historical GDP levels for C2
  dplyr::rename(ccode2 = ccode,
                GDP2 = VdemGDP) %>%
  dplyr::filter(is.na(ccode2) == FALSE) %>% ## don't want to merge on the NAs
  dplyr::select(ccode2, year, GDP2) %>%
  right_join(PREtreatydatapre_dyad)

#### Precolonial and original treaties: make richest country GDP home and the other country host
## Pre and Original treaties (with GDPlevels): treatydatapre_dyad

PREOriginal_hosthome2 <- PREtreatydatapre_dyad1 %>% #New variables for host and home
  mutate(home = ifelse(GDP1 > GDP2, ccode1, ccode2),
         ccode = ifelse(GDP1 < GDP2, ccode1, ccode2)) #Host has to be ccode to enable merge!

#Make variable which is 1 for all treaties
PRE_hostasym1 <- PREOriginal_hosthome %>%
  mutate(treaty = 1)

#### Pre and Original treaties: making indecis over time (Chapter 6 and Appendix) ####

sourcetimepre <- ggplot(PRE_hostasym1, aes(x=year, y=source)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) +
  xlab("Year signed") + #These are both in force and not
  ylab("Source Index") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

sourcetimepre + My_Theme + 
  ylim(0,1)

## WTH index development over time assymetric orginal + precolonial
WHTtimepre <- ggplot(PRE_hostasym1, aes(x=year, y=WHTrates)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) + 
  xlab("Year signed") + #These are both in force and not
  ylab("Witholding Tax Index") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

WHTtimepre + My_Theme + 
  ylim(0,1)

## PE index development over time assymetric orginal + precolonial
PEplotpre <- ggplot(PRE_hostasym1, aes(x=year, y=PE)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) + 
  xlab("Year signed") + #These are both in force and not
  ylab("Permanent Establishment Index") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

PEplotpre + My_Theme + 
  ylim(0,1)

## Other index development over time assymetric orginal + precolonial
Otherplotpre <- ggplot(PRE_hostasym1, aes(x=year, y=Other)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) + 
  xlab("Year signed") + #These are both in force and not
  ylab("Other Provisions Index") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

Otherplotpre + My_Theme + 
  ylim(0,1)

#### 2. Pre and original Treaties: Only with sample countries! Richest Dyad is defined as the home country - in order to make indecis over time (Chaper 6) ####

VDEMsmallpre <- VDEM %>% # New dataset with only year, country and GDP per cap (oil and growth are only used in ATH index)
  dplyr::select(ccode, VdemGDP, year)

## Making incomelevels NUMERIC VERSION ##
PREtreatydatapre_dyad <- VDEMsmallpre %>% #Make correct historical GDP levels for C1
  dplyr::rename(ccode1 = ccode,
                GDP1 = VdemGDP) %>%
  dplyr::select(ccode1, year, GDP1) %>%
  right_join(treatydatapre)

PREtreatydatapre_dyad1 <- VDEMsmallpre %>% #Make correct historical GDP levels for C2
  dplyr::rename(ccode2 = ccode,
                GDP2 = VdemGDP) %>%
  dplyr::filter(is.na(ccode2) == FALSE) %>% ## don't want to merge on the NAs
  dplyr::select(ccode2, year, GDP2) %>%
  right_join(PREtreatydatapre_dyad)

#### Precolonial and original treaties: make richest country GDP home and the other country host
## Pre and Original treaties (with GDPlevels): treatydatapre_dyad

PREOriginal_hosthome2 <- PREtreatydatapre_dyad1 %>% #New variables for host and home
  mutate(home = ifelse(GDP1 > GDP2, ccode1, ccode2),
         ccode = ifelse(GDP1 < GDP2, ccode1, ccode2)) #Host has to be ccode to enable merge!


# Throw out treaties where ccode is not the countries in the Tax Treaties Explorer data
# 1. Make ccode of all countries in sample (118)
PREtaxcountries <-  c("Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", 
                   "Azerbaijan", "Bangladesh", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil",
                   "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Central African Republic", "Chad", "China", 
                   "Colombia", "Comoros", "Côte d'Ivoire", "Democratic People’s Republic of Korea", "Democratic Republic of Congo", 
                   "Djibouti", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini",
                   "Ethiopia", "Federal States of Micronesia", "Gabon", "Georgia", "Ghana", "Guatemala", "Guinea", "Guinea-Bissau", 
                   "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran", "Iraq", "Jordan", "Kenya", "Kiribati", "Kosovo", "Kyrgyz Republic",
                   "Lao PDR", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Maldives", "Mali", "Marshall Islands",
                   "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Nicaragua", 
                   "Niger", "Nigeria", "North Macedonia", "Pakistan", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Republic of Congo", 
                   "Rwanda", "Samoa", "São Tomé and Principe", "Senegal", "Seychelles", "Sierra Leone", "Solomon Islands", "Somalia", "South Africa",
                   "South Sudan", "Sri Lanka", "Sudan", "Syrian Arab Republic", "Tajikistan", "Tanzania", "Thailand", "The Gambia", 
                   "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uganda", "Ukraine", "Uzbekistan", 
                   "Vanuatu", "Venezuela", "Vietnam", "West Bank and Gaza", "Yemen", "Zambia", "Zimbabwe")
PREtaxcountries <- as.data.frame(PREtaxcountries)
#2. making ccode 
PREtaxcountries$ccode <- countrycode(PREtaxcountries$PREtaxcountries, origin = 'country.name', destination = 'cown')
# West bank was not matched

PREtaxcountries$ccodesample <- PREtaxcountries$ccode

#3. throw out if they do not match
PREOriginal_hosthome3 <- left_join(PREOriginal_hosthome2, PREtaxcountries)
PREOriginal_hosthome <- PREOriginal_hosthome3 %>%
  filter(ccodesample >= 0) # Removed about 600 treaties

#Make variable which is 1 for all treaties
PRE_hostasym1 <- PREOriginal_hosthome %>%
  mutate(treaty = 1)

#### 2. Pre and Original treaties: Only with sample countries! making indecis over time (Chapter 6 and Appendix) ####

sourcetimepre <- ggplot(PRE_hostasym1, aes(x=year, y=source)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) +
  xlab("Year signed") + #These are both in force and not
  ylab("Source Index") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

sourcetimepre + My_Theme + 
  ylim(0,1)

## WTH index development over time assymetric orginal + precolonial
WHTtimepre <- ggplot(PRE_hostasym1, aes(x=year, y=WHTrates)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) + 
  xlab("Year signed") + #These are both in force and not
  ylab("Witholding Tax Index") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

WHTtimepre + My_Theme + 
  ylim(0,1)

## PE index development over time assymetric orginal + precolonial
PEplotpre <- ggplot(PRE_hostasym1, aes(x=year, y=PE)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) + 
  xlab("Year signed") + #These are both in force and not
  ylab("Permanent Establishment Index") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

PEplotpre + My_Theme + 
  ylim(0,1)

## Other index development over time assymetric orginal + precolonial
Otherplotpre <- ggplot(PRE_hostasym1, aes(x=year, y=Other)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) + 
  xlab("Year signed") + #These are both in force and not
  ylab("Other Provisions Index") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

Otherplotpre + My_Theme + 
  ylim(0,1)

#### Asymmetric Heckman: Income Levels. ## Prepare income levels data ######

# The World Banks historical data on country income levels. # 1987-2019 
# Will be used to determine which treaties are assymetrical.
WBincome <- read_excel("Dincomelevel.xlsx") 

# Make colums for 1950 - 1986, based on 1987 column. 
WBincome <- WBincome %>% 
  mutate(`1950` = `1987`, `1951` = `1987`, `1952` = `1987`, `1953` = `1987`, `1954` = `1987`, `1955` = `1987`, `1956` = `1987`, `1957` = `1987`, `1958` = `1987`,
         `1959` = `1987`, `1960` = `1987`, `1961` = `1987`, `1962` = `1987`, `1963` = `1987`, `1964` = `1987`, `1965` = `1987`, `1966` = `1987`, `1967` = `1987`, `1968` = `1987`, `1969` = `1987`, `1970` = `1987`,
         `1971` = `1987`, `1972` = `1987`, `1973` = `1987`, `1974` = `1987`, `1975` = `1987`, `1976` = `1987`, `1977` = `1987`, `1978` = `1987`, `1979` = `1987`,
         `1980` = `1987`, `1981` = `1987`, `1982` = `1987`, `1983` = `1987`, `1984` = `1987`, `1985` = `1987`, `1986` = `1987`)

df_pivoted <- pivot_longer(WBincome, cols = -c("Country"), names_to = "year") # Change direction of matrix
Incomeleveldata <- rename(df_pivoted, c(value= "Incomelevel")) 
Incomeleveldata$Incomelevel  <- dplyr::recode(Incomeleveldata$Incomelevel, # Changing names of income levels to match tax treaty datasett.   
                                              L = "Low income", LM = "Lower middle income", 
                                              UM = "Upper middle income", H = "High income") 
Incomeleveldata <- Incomeleveldata %>% # Changing countries to ccode
  mutate(ccode = countrycode(Country, origin = 'country.name', destination = 'cown'),
         ccode = ifelse(Country == "Korea, Dem. Rep.", 731, ccode)) # Making sure North Korea gets its own code
# Where not matched: American Samoa, Aruba, Bermuda, British Virgin Islands, Cayman Islands, Channel Islands, Curaçao, 
#Faeroe Islands, French Polynesia, Gibraltar, Greenland, Guam, Hong Kong SAR, China (710), Isle of Man, Macao SAR, 
#China, New Caledonia, Northern Mariana Islands, Puerto Rico, Serbia, Sint Maarten (Dutch part), 
#St. Martin (French part), Turks and Caicos Islands, Virgin Islands (U.S.), West Bank and Gaza
Incomeleveldata <- Incomeleveldata %>% arrange(Country, year) #sort data
Incomeleveldata$year <-as.numeric(Incomeleveldata$year) #Make year numerical

sum(is.na(Incomeleveldata$ccode)) #1680 NAs
Incomeleveldata <- drop_na(Incomeleveldata) #Removed all NAs from ccode = 1680

modeleveldata <- left_join(modelData, Incomeleveldata) #First merge with incomedata
# loose observations in IncomeLeveldata which are not in modelData
# And get observations with NA for rows in ModelData which are not in IncomelevelData
table(leveldata$Incomelevel) #leveldata 239 NAs of 5778 observations 

modeleveldata1 <- modeleveldata %>% #4027 observations
  filter(str_detect(Incomelevel, "Low income")|
           str_detect(Incomelevel, "Lower middle income")) 

#### Asymmetric Heckman: Income Levels. preparing treatydata ####

## Original treaties: Making incomelevels and assymtetric dataset ##
treatydataori_LEVEL <- Incomeleveldata %>% #Make correct historical income levels for C1
  dplyr::rename(ccode1 = ccode,
                Incomelevel1 = Incomelevel) %>%
  dplyr::select(ccode1, year, Incomelevel1) %>%
  right_join(treatydataori) 

treatydataori_LEVEL1 <- Incomeleveldata %>% #Make correct historical income levels for C2
  dplyr::rename(ccode2 = ccode,
                Incomelevel2 = Incomelevel) %>%
  dplyr::filter(is.na(ccode2) == FALSE) %>% ## don't want to merge on the NAs
  dplyr::select(ccode2, year, Incomelevel2) %>%
  right_join(treatydataori_LEVEL)

# Seperating between assymeteric treaties (across the middle income line) and not.

# This is the dataset I will use for the robustness test later #CATEGORY VERSION
treatydataori_LEVEL2 <- filter(treatydataori_LEVEL1, ((Incomelevel1 == "Low income" | Incomelevel1 == "Lower middle income") 
                                                   & (Incomelevel2 == "High income" | Incomelevel2 == "Upper middle income")) |
                              ((Incomelevel2 == "Low income" | Incomelevel2 == "Lower middle income") 
                               & (Incomelevel1 == "High income" | Incomelevel1 == "Upper middle income")))

#These are the non-asymmetric in the sample
LEVELNO <- filter(treatydataori_LEVEL1, ((Incomelevel1 == "Low income" | Incomelevel1 == "Lower middle income") 
                                                 & (Incomelevel2 == "Low income" | Incomelevel2 == "Lower middle income")) |
                            ((Incomelevel1== "High income" | Incomelevel1 == "Upper middle income") 
                             & (Incomelevel2 == "High income" | Incomelevel2 == "Upper middle income")))

# Since I have removed all non-assymetric treaties I can make a variable ccode which 
# always is the low or lower middle income country in the treaty
treatydataori_LEVEL3 <- treatydataori_LEVEL2 %>%
  mutate(ccode = ifelse(Incomelevel1 == "Low income" | Incomelevel1 == "Lower middle income", ccode1, ccode2)) 

#Make variable which is 1 for all treaties
treatydataori_LEVEL3 <- treatydataori_LEVEL3 %>%
  mutate(treaty = 1) ## When merging all observations with treaty will be 0 and rest will be NA

#Aggregate data for those ccode-years which have more treaties.
treatydataori_LEVEL4 <- treatydataori_LEVEL3 %>%
  dplyr::select(year, ccode, treaty, source, signdate) %>% 
  arrange(desc(year, ccode)) %>% 
  group_by(year, ccode) %>% 
  summarise(treaties = sum(treaty),
            treaty = 1, 
            signdate = mean(signdate),
            source = mean(source, na.rm = TRUE),
            .groups = "keep") 

#Source is NA (not 0). Merge this with modelData, where treaties-variable is 0 for the period and countries which are covered by Original_hosthomeasym2
#and NA for what it does not cover. # left_join here to delete observations in the Original_hostasym2 that are not in the modelData
#doubletreaty has all the ccodes in treatydata as ccode
levelmasterdatadyad <- left_join(modeleveldata1, treatydataori_LEVEL4) %>%
  mutate(treaties = ifelse(is.na(treaties) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                           treaties), # put treaty to 0 if year is bigger than the first year in Original_hosthome2
         treaty = ifelse(is.na(treaty) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                         treaty))

levelmasterdatadyad1 <- levelmasterdatadyad  %>%
  mutate(treaties = ifelse(treaties > 0 & enddate > signdate & startdate < signdate, treaties, 0),
         treaty = ifelse(treaty > 0 & enddate > signdate & startdate < signdate, treaty, 0),
         source = ifelse(source > 0 & enddate > signdate & startdate < signdate, source, NA), # Source is NA
         signdate = ifelse(signdate > 0 & enddate > signdate & startdate < signdate, signdate, NA))


##Add control variables and IV ##
## 1. Bureaucratic quality
#rigadmin is already in masterdatadyad

## Add regional competition measured by number of treaties in region past 10 years (with one year lag)
leveldyadburcomp <- levelmasterdatadyad1 %>%
  group_by(region) %>% ## using group_by we make sure the lag is within countries
  arrange(region, year) %>%
  mutate(competition = lag(treaties), # Leaving first treaties in every new region-year be NA 
         #because we don't know what failure was before the dataset begins
         competitions = cumsum(ifelse(is.na(competition),0,competition)), #Adds number of treaties from beginning of observations
         competition10 = runner( x = ifelse(is.na(competition),0,competition), #number of treaties past ten years
                                 f = sum, 
                                 k = 10))

leveldyadburcomp1 <- leveldyadburcomp %>% #Lag the effect with one year
  mutate(comp10lag = ifelse(year == lag(year), competition10, lag(competition10)))

## Add IV - BIT-signing data 
BITdata <- read_excel("DUNCTAD_IIA3.xlsx")
# Make year-variable 
BITdata$year <- format(as.Date(BITdata$Date.of.signature, format="%d/%m/%Y"),"%Y")
BITdata$year <- as.numeric(BITdata$year)
BITdata$Date.of.signature <- format(as.Date(BITdata$Date.of.signature,format="%d/%m/%Y"), "%Y-%m-%d")


#Make one observation per signing countries - all agreements are counted twice
BITdata1 <- BITdata %>%
  dplyr::select(Parties.1, year, Date.of.signature) %>%
  dplyr::rename(ccode = Parties.1) 

BITdata2 <- BITdata %>%
  dplyr::select(Parties.2, year, Date.of.signature) %>%
  dplyr::rename(ccode = Parties.2)

BITdataboth <-rbind(BITdata1, BITdata2) #bind: double length

#Make correct ccode 
BITdataboth$ccode <- countrycode(BITdataboth$ccode, origin = "country.name", destination = "cown")

sum(is.na(BITdataboth$ccode)) # Only 629 NAs

BITdataboth1 <- BITdataboth %>%
  mutate(BITPTA = 1) ## When merging all observations with BIT will be 1 and rest will be NA

leveldyadburcitcompbit0 <- left_join(leveldyadburcomp1, BITdataboth1) 

leveldyadburcitcompbit00 <- leveldyadburcitcompbit0  %>%
  mutate(BITPTA = ifelse(BITPTA > 0 & enddate > Date.of.signature & startdate < Date.of.signature, BITPTA, NA))

#new mean variables
leveldyadburcitcompbit1 <- leveldyadburcitcompbit00 %>%
  filter(is.na(ccode) == FALSE, 
         is.na(year) == FALSE) %>%
  arrange(desc(leader, year)) %>% 
  group_by(leader, year) %>% 
  mutate(region = toString(unique(region)),
         athdatawprob= mean(athdatawprob), 
         rigadmin = mean(rigadmin),
         Military = mean(Military), 
         Monarchy = mean(Monarchy),
         Party = mean(Party),
         Oligarchy = mean(Oligarchy),
         Personalist = mean(Personalist), 
         VdemGDP = mean(VdemGDP), 
         ccode = mean(ccode),
         year = mean(year), 
         start = mean(start),
         coup = mean(coup), 
         comp10lag = mean(comp10lag),
         source = mean(source)) 

leveldyadburcitcompbit12 <- leveldyadburcitcompbit1 %>%
  dplyr::select(obsid, 
                leader,
                ccode,
                year,
                region,
                athdatawprob, 
                rigadmin,
                Military, 
                Monarchy,
                Party,
                Oligarchy,
                Personalist, 
                VdemGDP, 
                coup, 
                comp10lag,
                treaty,
                source,
                BITPTA
  )

leveldyadburcitcompbitdist <- distinct(leveldyadburcitcompbit12, .keep_all = FALSE)

leveldyadburcitcompbit <- leveldyadburcitcompbitdist %>%
  group_by(leader)%>%
  arrange(leader, year) %>% 
  mutate(pastbit = lag(BITPTA),
         pastbits = cumsum(ifelse(is.na(pastbit),0,pastbit)))

# Throw out treaties where ccode is not the countries in the Tax Treaties Explorer data
# 1. Make ccode of all countries in sample (118)
asymtaxcountries <-  c("Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", 
                   "Azerbaijan", "Bangladesh", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil",
                   "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Central African Republic", "Chad", "China", 
                   "Colombia", "Comoros", "Côte d'Ivoire", "Democratic People’s Republic of Korea", "Democratic Republic of Congo", 
                   "Djibouti", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini",
                   "Ethiopia", "Federal States of Micronesia", "Gabon", "Georgia", "Ghana", "Guatemala", "Guinea", "Guinea-Bissau", 
                   "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran", "Iraq", "Jordan", "Kenya", "Kiribati", "Kosovo", "Kyrgyz Republic",
                   "Lao PDR", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Maldives", "Mali", "Marshall Islands",
                   "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Nicaragua", 
                   "Niger", "Nigeria", "North Macedonia", "Pakistan", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Republic of Congo", 
                   "Rwanda", "Samoa", "São Tomé and Principe", "Senegal", "Seychelles", "Sierra Leone", "Solomon Islands", "Somalia", "South Africa",
                   "South Sudan", "Sri Lanka", "Sudan", "Syrian Arab Republic", "Tajikistan", "Tanzania", "Thailand", "The Gambia", 
                   "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uganda", "Ukraine", "Uzbekistan", 
                   "Vanuatu", "Venezuela", "Vietnam", "West Bank and Gaza", "Yemen", "Zambia", "Zimbabwe")
asymtaxcountries <- as.data.frame(asymtaxcountries)
#2. making ccode 
asymtaxcountries$ccode <- countrycode(asymtaxcountries$asymtaxcountries, origin = 'country.name', destination = 'cown')
# West bank was not matched

asymtaxcountries$ccodesample <- asymtaxcountries$ccode

#3. throw out if they do not match
asymdyadburcitcompbitsample <- left_join(leveldyadburcitcompbit, asymtaxcountries)
asymsample <- asymdyadburcitcompbitsample %>%
  filter(ccodesample >= 0)

sum(is.na(asymdyadburcitcompbitsample$ccodesample)) #214 removed observations

#### Asymmetric: Running Heckman ####

set.seed(1)
robust1levels <- selection(treaty ~ athdatawprob
                + pastbits
                + poly(year,3)
                + rigadmin
                + Military # All other categories as reference 
                + comp10lag # treaties signed past 10 years in region, lagged by one year
                + log(VdemGDP), 
                source ~ athdatawprob
                + poly(year,3)
                +rigadmin
                + Military 
                + comp10lag 
                + log(VdemGDP), 
                data = filter(asymsample, VdemGDP > 0), method = "ml")


summary(robust1levels)

#### Asymmetric: Clustered standard errors ####
modelMatrix <- filter(asymsample, VdemGDP > 0) %>% 
  dplyr::select(treaty, athdatawprob, pastbits, year, rigadmin, Military, comp10lag, 
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  na.omit()

robust1levels$leadercluster <- vcovCL(robust1levels, cluster = modelMatrix$leader)

#### Asymmetric: tables and plots ####

# Print list of asymmetric treaties
asymsample$countryccode <- countrycode(asymsample$ccode, origin = "cown", destination = "country.name")
countryccodeasym <- unique(asymsample$countryccode)  
writeLines(countryccodeasym, "countrylistasym.tex")

# Histogram that compares assymetrical and non-asym Original treaties in the Tax Treaties explorer dataset! 
treatydataori_LEVEL2$veg <- 'asym'
treatydataori_LEVEL1$veg <- 'all'
treatylength <- rbind(treatydataori_LEVEL1, treatydataori_LEVEL2)
bothLEVELS <- ggplot(treatylength, aes(year, fill = veg)) + 
  geom_histogram(alpha = 0.5, aes(), position = 'identity', bins = 70) + 
  theme_bw() +
  scale_y_continuous(breaks=number_ticks(10)) +
  scale_x_continuous(breaks=number_ticks(8)) +
  scale_fill_manual(name = "", values = c("darkgrey", "red"), labels=c("All treaties", "Asymmetric treaties"))

bothLEVELS + My_Theme2 + xlab("Year signed") + #These are both in force and not
  ylab("Frequency")

#Histogram that main and asymetric samples
asyminsample <- asymsample %>%
  filter(treaty == 1)
  
treatyinsample <- mainsample %>%
  filter(treaty == 1)

#Histogram that main and asymetric samples
asyminsample$veg <- 'asym'
treatyinsample$veg <- 'all'
treatylength <- rbind(asyminsample, treatyinsample)
bothLEVELS <- ggplot(treatylength, aes(year, fill = veg)) + 
  geom_histogram(alpha = 0.5, aes(), position = 'identity', bins = 60) + 
  theme_bw() +
  scale_y_continuous(breaks=number_ticks(8)) +
  scale_x_continuous(breaks=number_ticks(8)) +
  scale_fill_manual(name = "", values = c("darkgrey", "red"), labels=c("Main sample", "Asymmetric sample"))

bothLEVELS + My_Theme2 + xlab("Year signed") + #These are both in force and not
  ylab("Frequency")

#mainsample 4439
#asymsample 3830

## Stargazer of asymmetric Heckman ##

## Descriptive table of variables in Main model
desc1 <- asymsample %>% 
  dplyr::select(treaty, source, athdatawprob, 
                BITPTA, pastbits, rigadmin, Military, Party, Personalist,
                Oligarchy, Monarchy, comp10lag, VdemGDP, year, ccode)%>%  
  as.data.frame()
stargazer(desc1, summary=TRUE, 
          out = "descasym.tex",
          omit.summary.stat = c("p25", "p75"), 
          digits = 1,
          label = "descvarheck",
          title            = "Descriptive statistics for variables in asymmetric model",
          covariate.labels = c("DTA signing", "Source index", "Pr(Fail)", "BIT signing", "IV: Past BIT signing",
                               "Bureaucratic quality", "Regime type: Military", "Regime type: Party", 
                               "Regime type: Personalist", "Regime type: Oligarchy", 
                               "Regime type: Monarchy (ref)", "Regional competition", "GDP per cap",
                               "Year", "Host countries"), 
          omit = "ccode")

## BUTON of Heckman - maximum likelyhood estimation 
tobit_2Orobust1levels <- robust1levels
tobit_2Orobust1levels$param$index$betaO <- robust1levels$param$index$betaS
tobit_2Orobust1levels$param$index$betaS <- robust1levels$param$index$betaO

stargazer(robust1levels, tobit_2Orobust1levels, selection.equation = TRUE, title="Heckman (ML) Asymmetric treaties",       
          column.labels = c("Selection equation", "Outcome equation"),
          notes.align = "l", 
          notes= c("Clustered standard errors", "on leader in parentheses"),
          se =list(sqrt(diag(robust1levels$leadercluster))[robust1levels$param$index$betaS],
                   sqrt(diag(robust1levels$leadercluster))[robust1levels$param$index$betaO]),
          out = "ALEVELsheckman.tex",
          label = "LEVEL",
          dep.var.labels = c("DTA signing Source index"),# Is adjusted in Latex
          covariate.labels= c("Pr(Fail)", "IV: Past BIT signing", "Year", "Year squared", "Year cubed",
                              "Bureaucratic quality",
                              "Regime type: Military",
                              "Regional competition", "Log GDP per cap"), 
          digits = 2, 
          omit.stat=c("f"), 
          no.space=TRUE)


#Predicted probabilities asym plot stage 1
firstStageBetas <- coefficients(robust1levels)[attr(coefficients(robust1levels),"index")$betaS] ## Choosing which coefficients in the 2-step model 

#clusterd standard errors
robust1levels$leadercluster <- vcovCL(robust1levels, cluster = modelMatrix$leader)
firstStageVCov <- robust1levels$leadercluster[attr(coefficients(robust1levels),"index")$betaS, 
                                        attr(coefficients(robust1levels),"index")$betaS]

firstStageSimb <- mvrnorm(n = 1000, 
                          mu = firstStageBetas, 
                          Sigma = firstStageVCov)
firstStageXrange <- seq(from = min(filter(asymsample, VdemGDP > 0)$athdatawprob, na.rm = TRUE), 
                        to = 1,#max(filter(dyadburcitcompbit, VdemGDP > 0)$athdatawprob, na.rm = TRUE), 
                        length.out = 50)
firstStageX <- cbind(1, # Intercept
                     firstStageXrange, #athdatawprob
                     mean(filter(asymsample, VdemGDP > 0)$pastbits, na.rm = TRUE), 
                     mean(poly(filter(asymsample, VdemGDP > 0)$year, 3)[,1]),
                     mean(poly(filter(asymsample, VdemGDP > 0)$year, 3)[,2]),
                     mean(poly(filter(asymsample, VdemGDP > 0)$year, 3)[,3]), 
                     mean(filter(asymsample, VdemGDP > 0)$rigadmin, na.rm = TRUE),
                     0, # Military; 
                     mean(filter(mainsample, VdemGDP > 0)$comp10lag, na.rm = TRUE),
                     mean(log(filter(mainsample, VdemGDP > 0)$VdemGDP), na.rm = TRUE))

firstStageXBetaMatrix <- firstStageX  %*% t(firstStageSimb)
firstStagePredY <- pnorm(firstStageXBetaMatrix) 

firstStagePlotValues <- t(apply(firstStagePredY, MARGIN = 1, FUN = quantile, probs = c(.025,.5,.975)))

firstStagePlotValues <- as.data.frame(firstStagePlotValues) %>% 
  mutate(athdatawprob = firstStageXrange)

ggplot(firstStagePlotValues, 
       aes(x = athdatawprob, 
           y = `50%`, 
           ymin =  `2.5%`, 
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.5, fill = "#999999" )+
  geom_line()+
  ylab("Predicted probability of DTA signing") +
  xlab("Pr(Fail)") + 
  theme_classic() + My_Theme

## #Predicted probabilities PE Heckman Stage 2
asymStageBetas <- coefficients(robust1levels)[attr(coefficients(robust1levels),"index")$betaO] ## Choosing which coefficients in the 2-step model 

#clusterd standard errors
robust1levels$leadercluster <- vcovCL(robust1levels, cluster = modelMatrix$leader)
asymStageVCov <- robust1levels$leadercluster[attr(coefficients(robust1levels),"index")$betaO, 
                                       attr(coefficients(robust1levels),"index")$betaO]
asymStageSimb <- mvrnorm(n = 1000, 
                       mu = asymStageBetas, 
                       Sigma = asymStageVCov)
asymStageXrange <- seq(from = min(filter(asymsample, VdemGDP > 0)$athdatawprob, na.rm = TRUE), 
                     to = max(filter(asymsample, VdemGDP > 0)$athdatawprob, na.rm = TRUE), 
                     length.out = 50)
asymStageX <- cbind(1, # Intercept
                  asymStageXrange, #athdatawprob
                  # mean(filter(dyadburcitcompbit, VdemGDP > 0)$pastbits, na.rm = TRUE), #Is not included in stage 2
                  mean(poly(filter(asymsample, VdemGDP > 0)$year, 3)[,1]),
                  mean(poly(filter(asymsample, VdemGDP > 0)$year, 3)[,2]),
                  mean(poly(filter(asymsample, VdemGDP > 0)$year, 3)[,3]), 
                  mean(filter(asymsample, VdemGDP > 0)$rigadmin, na.rm = TRUE),
                  0, # Military; 
                  mean(filter(asymsample, VdemGDP > 0)$comp10lag, na.rm = TRUE),
                  mean(log(filter(asymsample, VdemGDP > 0)$VdemGDP), na.rm = TRUE))

asymStageXBetaMatrix <- asymStageX  %*% t(asymStageSimb)
#firstStagePredY <- pnorm(firstStageXBetaMatrix) #Removed because this is linear

asymStagePlotValues <- t(apply(asymStageXBetaMatrix, MARGIN = 1, FUN = quantile, probs = c(.025,.5,.975)))

asymStagePlotValues <- as.data.frame(asymStagePlotValues) %>% 
  mutate(source = asymStageXrange)

ggplot(asymStagePlotValues, 
       aes(x = source, 
           y = `50%`, 
           ymin =  `2.5%`, 
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.5, fill = "#999999" )+
  geom_line()+
  ylab("Source index") +
  xlab("Pr(Fail)") +
  theme_classic() + My_Theme


#### Further analysis with PE, WHT and Other indices - Preparing data #### 
INDEXOriginal_hosthome <- treatydataori_dyad1 %>% #New variables for host and home
  mutate(home = ifelse(GDP1 > GDP2, ccode1, ccode2),
         ccode = ifelse(GDP1 < GDP2, ccode1, ccode2)) #Host has to be ccode to enable merge!

#Make variable which is 1 for all treaties
INDEXOriginal_hostasym1 <- INDEXOriginal_hosthome %>%
  mutate(treaty = 1)## When merging all observations with treaty will be 0 and rest will be NA

#Aggregate data for those ccode-years which have more treaties.
INDEXOriginal_hostasym2 <- INDEXOriginal_hostasym1 %>%
  dplyr::select(year, ccode, treaty, source, home, Other, PE, WHTrates, source, signdate) %>% 
  arrange(desc(year, ccode)) %>% 
  group_by(year, ccode) %>% 
  summarise(treaties = sum(treaty),
            treaty = 1, 
            signdate = mean(signdate),
            source = mean(source, na.rm = TRUE),
            home = list(home),
            Other = mean(Other),
            PE = mean(PE),
            WHT = mean(WHTrates)
            , .groups = "keep") # making list 

table(INDEXOriginal_hostasym2$ccode) #105 different host countries

#Merge this with modelData, where treaties-variable is 0 for the period and countries which are covered by Original_hosthomeasym2
#and NA for what it does not cover. # left_join here to delete observations in the Original_hostasym2 that are not in the modelData
#doubletreaty has all the ccodes in treatydata as ccode
INDEXmasterdatadyad <- left_join(modelData, INDEXOriginal_hostasym2) %>%
  mutate(treaties = ifelse(is.na(treaties) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                           treaties), # put treaty to 0 if year is bigger than the first year in Original_hosthome2
         treaty = ifelse(is.na(treaty) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                         treaty))


INDEXmasterdatadyad1 <- INDEXmasterdatadyad  %>%
  mutate(treaties = ifelse(treaties > 0 & enddate > signdate & startdate < signdate, treaties, 0),
         treaty = ifelse(treaty > 0 & enddate > signdate & startdate < signdate, treaty, 0),
         Other = ifelse(Other > 0 & enddate > signdate & startdate < signdate, Other, NA), 
         WHT = ifelse(WHT > 0 & enddate > signdate & startdate < signdate, WHT, NA),
         PE = ifelse(PE > 0 & enddate > signdate & startdate < signdate, PE, NA),
         source = ifelse(source > 0 & enddate > signdate & startdate < signdate, source, NA),
         signdate = ifelse(signdate > 0 & enddate > signdate & startdate < signdate, signdate, NA))

##Add control variables and IV ##
## 1. Bureaucratic quality
#rigadmin is already in masterdatadyad

## Add regional competition measured by number of treaties in region past 10 years (with one year lag)
INDEXdyadburcomp <- INDEXmasterdatadyad1 %>%
  group_by(region) %>% ## using group_by we make sure the lag is within countries
  arrange(region, year) %>%
  mutate(competition = lag(treaties), # Leaving first treaties in every new region-year be NA 
         #because we don't know what failure was before the dataset begins
         competitions = cumsum(ifelse(is.na(competition),0,competition)), #Adds number of treaties from beginning of observations
         competition10 = runner( x = ifelse(is.na(competition),0,competition), #number of treaties past ten years
                                 f = sum, 
                                 k = 10))

INDEXdyadburcomp1 <- INDEXdyadburcomp %>% #Lag the effect with one year
  mutate(comp10lag = ifelse(year == lag(year), competition10, lag(competition10)))

## Add IV - BIT-signing data 
INDEXBITdata <- read_excel("DUNCTAD_IIA3.xlsx")
# Make year-variable 
INDEXBITdata$year <- format(as.Date(INDEXBITdata$Date.of.signature, format="%d/%m/%Y"),"%Y")
INDEXBITdata$year <- as.numeric(INDEXBITdata$year)
INDEXBITdata$Date.of.signature <- format(as.Date(INDEXBITdata$Date.of.signature,format="%d/%m/%Y"), "%Y-%m-%d")

#Make one observation per signing countries - all agreements are counted twice
INDEXBITdata1 <- INDEXBITdata %>%
  dplyr::select(Parties.1, year, Date.of.signature) %>%
  dplyr::rename(ccode = Parties.1) 

INDEXBITdata2 <- INDEXBITdata %>%
  dplyr::select(Parties.2, year, Date.of.signature) %>%
  dplyr::rename(ccode = Parties.2)

INDEXBITdataboth <-rbind(INDEXBITdata1, INDEXBITdata2) #bind: double length

#Make correct ccode 
INDEXBITdataboth$ccode <- countrycode(INDEXBITdataboth$ccode, origin = "country.name", destination = "cown")

sum(is.na(INDEXBITdataboth$ccode)) # Only 629 NAs

INDEXBITdataboth1 <- INDEXBITdataboth %>%
  mutate(BITPTA = 1) ## When merging all observations with BIT will be 1 and rest will be NA

INDEXdyadburcitcompbit0 <- left_join(INDEXdyadburcomp1, INDEXBITdataboth1) 

INDEXdyadburcitcompbit00 <- INDEXdyadburcitcompbit0  %>%
  mutate(BITPTA = ifelse(BITPTA > 0 & enddate > Date.of.signature & startdate < Date.of.signature, BITPTA, NA))

#new mean variables
INDEXdyadburcitcompbit1 <- INDEXdyadburcitcompbit00 %>%
  filter(is.na(ccode) == FALSE, 
         is.na(year) == FALSE) %>%
  arrange(desc(leader, year)) %>% 
  group_by(leader, year) %>% 
  mutate(region = toString(unique(region)),
         athdatawprob= mean(athdatawprob), 
         rigadmin = mean(rigadmin),
         Military = mean(Military), 
         Monarchy = mean(Monarchy),
         Party = mean(Party),
         Oligarchy = mean(Oligarchy),
         Personalist = mean(Personalist), 
         VdemGDP = mean(VdemGDP), 
         Other = mean(Other), 
         PE = mean(PE), 
         source = mean(source), 
         WHT = mean(WHT), 
         ccode = mean(ccode),
         year = mean(year), 
         start = mean(start),
         coup = mean(coup), 
         comp10lag = mean(comp10lag)) 

INDEXdyadburcitcompbit12 <- INDEXdyadburcitcompbit1 %>%
  dplyr::select(obsid, 
                leader,
                ccode,
                year,
                region,
                athdatawprob, 
                rigadmin,
                Military, 
                Monarchy,
                Party,
                Oligarchy,
                Personalist, 
                VdemGDP, 
                coup, 
                comp10lag,
                treaty,
                treaties,
                Other,
                PE,
                WHT,
                BITPTA, 
                source
  )

INDEXdyadburcitcompbitdist <- distinct(INDEXdyadburcitcompbit12, .keep_all = FALSE)

INDEXdyadburcitcompbit <- INDEXdyadburcitcompbitdist %>%
  group_by(leader)%>%
  arrange(leader, year) %>% 
  mutate(pastbit = lag(BITPTA),
         pastbits = cumsum(ifelse(is.na(pastbit),0,pastbit)))

# Throw out treaties where ccode is not the countries in the Tax Treaties Explorer data
# 1. Make ccode of all countries in sample (118)
indextaxcountries <-  c("Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", 
                       "Azerbaijan", "Bangladesh", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil",
                       "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Central African Republic", "Chad", "China", 
                       "Colombia", "Comoros", "Côte d'Ivoire", "Democratic People’s Republic of Korea", "Democratic Republic of Congo", 
                       "Djibouti", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini",
                       "Ethiopia", "Federal States of Micronesia", "Gabon", "Georgia", "Ghana", "Guatemala", "Guinea", "Guinea-Bissau", 
                       "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran", "Iraq", "Jordan", "Kenya", "Kiribati", "Kosovo", "Kyrgyz Republic",
                       "Lao PDR", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Maldives", "Mali", "Marshall Islands",
                       "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Nicaragua", 
                       "Niger", "Nigeria", "North Macedonia", "Pakistan", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Republic of Congo", 
                       "Rwanda", "Samoa", "São Tomé and Principe", "Senegal", "Seychelles", "Sierra Leone", "Solomon Islands", "Somalia", "South Africa",
                       "South Sudan", "Sri Lanka", "Sudan", "Syrian Arab Republic", "Tajikistan", "Tanzania", "Thailand", "The Gambia", 
                       "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uganda", "Ukraine", "Uzbekistan", 
                       "Vanuatu", "Venezuela", "Vietnam", "West Bank and Gaza", "Yemen", "Zambia", "Zimbabwe")
indextaxcountries <- as.data.frame(indextaxcountries)
#2. making ccode 
indextaxcountries$ccode <- countrycode(indextaxcountries$indextaxcountries, origin = 'country.name', destination = 'cown')
# West bank was not matched

indextaxcountries$ccodesample <- indextaxcountries$ccode

#3. throw out if they do not match
indexdyadburcitcompbitsample <- left_join(INDEXdyadburcitcompbit, indextaxcountries)
indexsample <- indexdyadburcitcompbitsample %>%
  filter(ccodesample >= 0)

sum(is.na(indexdyadburcitcompbitsample$ccodesample)) #1362 observations deleted

#### Further analysis with PE, WHT and Other indices ####

PEheckman <- selection(treaty ~ athdatawprob 
                       + pastbits
                       + poly(year, 3)
                       + rigadmin
                       + Military #All other regime types as ref
                       + comp10lag # treaties signed past 10 years in region, lagged by one year
                       + log(VdemGDP),  
                       PE ~ athdatawprob
                       + poly(year, 3)
                       + rigadmin
                       + Military # All other regime types as ref
                       + comp10lag # treaties signed past 10 years in region, lagged by one year
                       + log(VdemGDP), 
                       data = filter(indexsample, VdemGDP > 0), 
                       method = "ml")
summary(PEheckman, diagnostics = TRUE)

WHTheckman <- selection(treaty ~ athdatawprob 
                        + pastbits
                        + poly(year, 3)
                        + rigadmin
                        + Military #All other regime types as ref
                        + comp10lag # treaties signed past 10 years in region, lagged by one year
                        + log(VdemGDP), 
                        WHT ~ athdatawprob
                        + poly(year, 3)
                        + rigadmin
                        + Military 
                        + comp10lag 
                        + log(VdemGDP), 
                        data = filter(indexsample, VdemGDP > 0), 
                        method = "ml")
summary(WHTheckman, diagnostics = TRUE)

Otherheckman <- selection(treaty ~ athdatawprob 
                          + pastbits
                          + poly(year, 3)
                          + rigadmin
                          + Military #All other regime types as ref
                          + comp10lag # treaties signed past 10 years in region, lagged by one year
                          + log(VdemGDP), 
                          Other ~ athdatawprob
                          + poly(year, 3)
                          + rigadmin
                          + Military 
                          + comp10lag 
                          + log(VdemGDP), 
                          data = filter(indexsample, VdemGDP > 0), 
                          method = "ml")
summary(Otherheckman, diagnostics = TRUE)

## Descriptive table of variables in INDEX models
descINDEX <- indexsample %>% 
  dplyr::select(PE, WHT, Other)%>%  
  as.data.frame()
stargazer(descINDEX, summary=TRUE, 
          out = "AdescINDEX.tex",
          omit.summary.stat = c("p25", "p75"), 
          digits = 1,
          label = "descINDEX",
          title            = "Descriptive statistics for variables in the PE, WHT and Other indecis models",
          covariate.labels = c("PE index", "WHT index", "Other index"
          ),
                    omit = "ccode")

#### Further analysis with PE, WHT and Other indices: Clustered standard errors on leader ####

#PE
modelMatrixPE <- filter(indexsample, VdemGDP > 0) %>% 
  dplyr::select(treaty, athdatawprob, pastbits, year, rigadmin, Military, 
                comp10lag, PE, 
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  mutate(PE = ifelse(treaty == 0, 0, PE)) %>%      
  na.omit()

PEheckman$leadercluster <- vcovCL(PEheckman, cluster = modelMatrixPE$leader)

#WHT
modelMatrixWHT <- filter(indexsample, VdemGDP > 0) %>% 
  dplyr::select(treaty, athdatawprob, pastbits, year, rigadmin, Military, 
                comp10lag, WHT, 
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  mutate(WHT = ifelse(treaty == 0, 0, WHT)) %>%      
  na.omit()

WHTheckman$leadercluster <- vcovCL(WHTheckman, cluster = modelMatrixWHT$leader)

#Other
modelMatrixOther <- filter(indexsample, VdemGDP > 0) %>% 
  dplyr::select(treaty, athdatawprob, pastbits, year, rigadmin, Military, 
                comp10lag, Other, 
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  mutate(Other = ifelse(treaty == 0, 0, Other)) %>%      
  na.omit()
Otherheckman$leadercluster <- vcovCL(Otherheckman, cluster = modelMatrixOther$leader)

#### Further analysis with PE, WHT and Other indices: tables and plots #### 

## BUTON of Heckman - maximum likelyhood estimation - Sub indices
tobit_PEheckman <- PEheckman
tobit_PEheckman$param$index$betaO <- PEheckman$param$index$betaS
tobit_PEheckman$param$index$betaS <- PEheckman$param$index$betaO

tobit_WHTheckman <- WHTheckman
tobit_WHTheckman$param$index$betaO <- WHTheckman$param$index$betaS
tobit_WHTheckman$param$index$betaS <- WHTheckman$param$index$betaO

tobit_Otherheckman <- Otherheckman
tobit_Otherheckman$param$index$betaO <- Otherheckman$param$index$betaS
tobit_Otherheckman$param$index$betaS <- Otherheckman$param$index$betaO

stargazer(PEheckman, tobit_PEheckman, WHTheckman, tobit_WHTheckman, Otherheckman, tobit_Otherheckman, selection.equation = TRUE, 
          title="Heckman (ML) PE, WHT and Other indices",    
          out = "AheckmanPEWHT.tex",
          label = "PEWHT",
          dep.var.labels = c(""),
          column.labels = c("DTA signing & PE index & DTA signing & WHT index & DTA signing & Other index"),# Is adjusted in Latex
          covariate.labels= c("Pr(Fail)", "IV: Past BIT signing", "Year", "Year squared", "Year cubed",
                              "Bureaucratic quality",
                              "Regime type: Military", 
                              "Regional competition", "Log GDP per cap"), 
          digits = 2, 
          omit.stat=c("f"), 
          notes.align = "l", 
          notes= c("Clustered standard errors", "on leader in parentheses"),
          se =list(sqrt(diag(PEheckman$leadercluster))[PEheckman$param$index$betaS],
                   sqrt(diag(PEheckman$leadercluster))[PEheckman$param$index$betaO],
                   sqrt(diag(WHTheckman$leadercluster))[WHTheckman$param$index$betaS],
                   sqrt(diag(WHTheckman$leadercluster))[WHTheckman$param$index$betaO],
                   sqrt(diag(Otherheckman$leadercluster))[Otherheckman$param$index$betaS],
                   sqrt(diag(Otherheckman$leadercluster))[Otherheckman$param$index$betaO]), 
          no.space=TRUE)

#Histogram values PE index (in sample)
PEdistribution <- ggplot(data=indexsample, aes(PE)) + 
  geom_histogram(binwidth=0.02, fill="red", alpha=0.6) + 
  theme_bw() +
  scale_y_continuous(breaks=number_ticks(10)) +
  scale_x_continuous(breaks=number_ticks(10)) +
  scale_fill_manual(values = ("red"))

PEdistribution + My_Theme + xlab("Value") + #These are both in force and not
  ylab("Frequency")

#Histogram values WHT index (in sample)
WHTdistribution <- ggplot(data=indexsample, aes(WHT)) + 
  geom_histogram(binwidth=0.02, fill="red", alpha=0.6) + 
  theme_bw() +
  scale_y_continuous(breaks=number_ticks(10)) +
  scale_x_continuous(breaks=number_ticks(10)) +
  scale_fill_manual(values = ("red"))

WHTdistribution + My_Theme + xlab("Value") + #These are both in force and not
  ylab("Frequency") # 61 0er

#Histogram values Other index (in sample)
Otherdistribution <- ggplot(data=indexsample, aes(Other)) + 
  geom_histogram(binwidth=0.01, fill="red", alpha=0.6) + 
  theme_bw() +
  scale_y_continuous(breaks=number_ticks(10)) +
  scale_x_continuous(breaks=number_ticks(10)) +
  scale_fill_manual(values = ("red"))

Otherdistribution + My_Theme + xlab("Value") + #These are both in force and not
  ylab("Frequency") # 61 0er

## PE index development over time assymetric orginal 
PEplot <- ggplot(indexsample, aes(x=year, y=PE)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) + 
  xlab("Year signed") + #These are both in force and not
  ylab("Permanent Establishment Index") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

PEplot + My_Theme + 
  ylim(0,1)


## #Predicted probabilities PE Heckman
PEStageBetas <- coefficients(PEheckman)[attr(coefficients(PEheckman),"index")$betaO] ## Choosing which coefficients in the 2-step model 

#clusterd standard errors
PEheckman$leadercluster <- vcovCL(PEheckman, cluster = modelMatrixPE$leader)
PEStageVCov <- PEheckman$leadercluster[attr(coefficients(PEheckman),"index")$betaO, 
                                         attr(coefficients(PEheckman),"index")$betaO]
PEStageSimb <- mvrnorm(n = 1000, 
                       mu = PEStageBetas, 
                       Sigma = PEStageVCov)
PEStageXrange <- seq(from = min(filter(indexsample, VdemGDP > 0)$athdatawprob, na.rm = TRUE), 
                     to = max(filter(indexsample, VdemGDP > 0)$athdatawprob, na.rm = TRUE), 
                     length.out = 50)
PEStageX <- cbind(1, # Intercept
                  PEStageXrange, #athdatawprob
                  # mean(filter(dyadburcitcompbit, VdemGDP > 0)$pastbits, na.rm = TRUE), #Is not included in stage 2
                  mean(poly(filter(indexsample, VdemGDP > 0)$year, 3)[,1]),
                  mean(poly(filter(indexsample, VdemGDP > 0)$year, 3)[,2]),
                  mean(poly(filter(indexsample, VdemGDP > 0)$year, 3)[,3]), 
                  mean(filter(indexsample, VdemGDP > 0)$rigadmin, na.rm = TRUE),
                  0, # Military; 
                  mean(filter(indexsample, VdemGDP > 0)$comp10lag, na.rm = TRUE),
                  mean(log(filter(indexsample, VdemGDP > 0)$VdemGDP), na.rm = TRUE))

PEStageXBetaMatrix <- PEStageX  %*% t(PEStageSimb)
#firstStagePredY <- pnorm(firstStageXBetaMatrix) #Removed because this is linear

PEStagePlotValues <- t(apply(PEStageXBetaMatrix, MARGIN = 1, FUN = quantile, probs = c(.025,.5,.975)))

PEStagePlotValues <- as.data.frame(PEStagePlotValues) %>% 
  mutate(PE = PEStageXrange)

ggplot(PEStagePlotValues, 
       aes(x = PE, 
           y = `50%`, 
           ymin =  `2.5%`, 
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.5, fill = "#999999" )+
  geom_line()+
  ylab("Permanent establishment index") +
  xlab("Pr(Fail)") +
  theme_classic() + My_Theme

#### Robusttest 1: Alternative covariats ####

#1. Corporate tax dependence (with quite a lot of missing)
#2. Learning effect (previously signed treaties). 
#3. Capabilities difference (between signing parties) 
#4. Regime type dummies

#1. Add Add corporate tax income (CIT) as part of total country revenue (Corporate tax dependence)

CITdata <- read_excel("Dcorporatetaxmerge.xlsx")

CITdata1 <- CITdata %>% #Create CIT of total rev-variable
  mutate(CITofRevenue = CIT/Revenue) %>%
  dplyr::rename(year = Year)

CITdata1$year <- as.numeric(CITdata1$year)

CITdata2 <- CITdata1 %>%
  dplyr::select(ISO, year, CITofRevenue) %>%
  mutate(ccode = countrycode(ISO, origin = 'iso3c', destination = 'cown'))
# Not matched ABW, AIA, HKG, KSV, MAC, MSR, SRB, WBG

CITdata3 <- CITdata2 %>%
  dplyr::select(ccode, year, CITofRevenue) 

#Merge with the data used for main model
mainsamplecorr <- left_join(mainsample, CITdata3)
summary(mainsamplecorr$CITofRevenue) #lots of NA

# Add CIT
R4_CIT <- selection(treaty ~ athdatawprob
                    + pastbits
                    + CITofRevenue
                    + Military #All other categories as reference 
                    + poly(year, 3)
                    + rigadmin
                    + comp10lag # treaties signed past 10 years in region, lagged by one year
                    + log(VdemGDP),  
                    source ~ athdatawprob
                    + CITofRevenue
                    + Military 
                    + poly(year, 3)
                    +rigadmin
                    + comp10lag # treaties signed past 10 years in region, lagged by one year
                    + log(VdemGDP), 
                    data = filter(mainsamplecorr, VdemGDP > 0), 
                    method = "ml")

summary(R4_CIT) 

##2.  Learning variable

# Make past treaties signed-variable: 
mainsamplecorr <- mainsamplecorr %>%
  group_by(ccode) %>% ## using group_by we make sure the lag is within countries
  arrange(ccode, year) %>% ## Sort dataset by ccode and year
  mutate(learn = lag(treaties), # Leaving first treaty in every new ccodeobservation be NA 
         #because we don't know what failure was before the dataset begins
         learning = cumsum(ifelse(is.na(learn),0,learn))) #Adds number of treaties from beginning of observations

R4_learning <- selection(treaty ~ athdatawprob
                         + pastbits
                         + learning
                         + Military #Ref: All other categories
                         + poly(year, 3)
                         + rigadmin
                         + comp10lag # treaties signed past 10 years in region, lagged by one year
                         + log(VdemGDP),  
                         source ~ athdatawprob
                         + learning
                         + Military 
                         + poly(year, 3)
                         +rigadmin
                         + comp10lag 
                         + log(VdemGDP), 
                         data = filter(mainsamplecorr, VdemGDP > 0), 
                         method = "ml")

summary(R4_learning) 

# 3. Difference in capabilities (National capability index)

sum(is.na(mainsamplecorr$cincdif)) #3820 NA. Because only cincdif for countries with treaties

R4_capability <- selection(treaty ~ athdatawprob
                           + pastbits
                           + Military #Ref: All other categories
                           + poly(year, 3)
                           + rigadmin
                           + comp10lag # treaties signed past 10 years in region, lagged by one year
                           + log(VdemGDP),
                           source ~ athdatawprob
                           + cincdif 
                           + Military
                           + poly(year, 3)
                           + rigadmin
                           + comp10lag
                           + log(VdemGDP), 
                           data = filter(mainsamplecorr, VdemGDP > 0), 
                           method = "ml")

sum(is.na(mainsample$cincdif))
#579 source values and 619 cincvalues of 623 treaties.
summary(R4_capability) 

## 4. Regime type dummies
R4_regime <- selection(treaty ~ athdatawprob
                       + pastbits
                       + Party
                       + Personalist
                       + Oligarchy 
                       + Military #Ref: All other categories
                       + poly(year, 3) 
                       + rigadmin
                       + comp10lag # treaties signed past 10 years in region, lagged by one year
                       + log(VdemGDP),
                       source ~ athdatawprob
                       + Party
                       + Personalist
                       + Oligarchy
                       + Military 
                       + poly(year, 3)
                       + rigadmin
                       + comp10lag 
                       + log(VdemGDP), 
                       data = filter(mainsamplecorr, VdemGDP > 0), 
                       method = "ml")

summary(R4_regime) 


#### Robusttest 1: Alternative covariats: Clustering standard errors ####

#CIT
CITmodelMatrix <- filter(mainsamplecorr, VdemGDP > 0) %>% 
  dplyr::select(treaty, athdatawprob, pastbits, year, rigadmin, Military, comp10lag, CITofRevenue, 
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  na.omit()

R4_CIT$leadercluster <- vcovCL(R4_CIT, cluster = CITmodelMatrix$leader)

#Learning
learningmodelMatrix <- filter(mainsamplecorr, VdemGDP > 0) %>% 
  dplyr::select(treaty, athdatawprob, pastbits, year, rigadmin, Military, comp10lag, learning, 
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  na.omit()

R4_learning$leadercluster <- vcovCL(R4_learning, cluster = learningmodelMatrix$leader)

#Cincdif
capmodelMatrix <- filter(mainsamplecorr, VdemGDP > 0) %>% 
  dplyr::select(treaty, athdatawprob, pastbits, year, rigadmin, Military, comp10lag, cincdif,
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source),
         cincdif = ifelse(treaty == 0, 0, cincdif)) %>% 
  na.omit()

R4_capability$leadercluster <- vcovCL(R4_capability, cluster = capmodelMatrix$leader)

#regime type dummies
regimemodelMatrix <- filter(mainsamplecorr, VdemGDP > 0) %>% 
  dplyr::select(treaty, athdatawprob, pastbits, year, rigadmin, Military, 
                comp10lag, Party, Personalist, Oligarchy, 
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  na.omit()

R4_regime$leadercluster <- vcovCL(R4_regime, cluster = regimemodelMatrix$leader)

#### Robustness 1: alternative covariats. Tables and plots ####

## Descriptive table of variables in Alternative covariates
desc3 <- mainsamplecorr %>% 
  dplyr::select(CITofRevenue, learning, cincdif, Military, Party, Personalist, Oligarchy, Monarchy)%>%  
  as.data.frame()
stargazer(desc3, summary=TRUE, 
          out = "AAlternativecovariatesDESC.tex",
          omit.summary.stat = c("p25", "p75"), 
          digits = 1,
          label = "descvaralt",
          title            = "Descriptive statistics for alternative covariates",
          covariate.labels = c("Corporate Tax as share of Revenue", "Learning", "Capacity assymetry", 
                               "Regime type: Military", "Regime type: Party", 
                               "Regime type: Personalist", "Regime type: Oligarchy", 
                               "Regime type: Monarchy (ref)", "Region"), 
          omit = "ccode")


## BUTON robusttest3 models Heckman - maximum likelyhood estimation 

#CIT
tobit_R4_CIT <- R4_CIT
tobit_R4_CIT$param$index$betaO <- R4_CIT$param$index$betaS
tobit_R4_CIT$param$index$betaS <- R4_CIT$param$index$betaO

#Learning
tobit_R4_learning <- R4_learning
tobit_R4_learning$param$index$betaO <- R4_learning$param$index$betaS
tobit_R4_learning$param$index$betaS <- R4_learning$param$index$betaO

# Capability dif
tobit_R4_capability <- R4_capability
tobit_R4_capability$param$index$betaO <- R4_capability$param$index$betaS
tobit_R4_capability$param$index$betaS <- R4_capability$param$index$betaO

# Fixed effects regime
tobit_R4_regime <- R4_regime
tobit_R4_regime$param$index$betaO <- R4_regime$param$index$betaS
tobit_R4_regime$param$index$betaS <- R4_regime$param$index$betaO

stargazer(R4_CIT, tobit_R4_CIT, R4_learning, tobit_R4_learning, R4_capability, tobit_R4_capability, R4_regime, tobit_R4_regime, selection.equation = TRUE, 
          title="Heckman (ML) Alternative covariates",       
          column.labels = c("
                            \\\\
                            & Corporate tax & Corporate tax & 
                            Learning & Learning & 
                            Capability & Capability &
                            Regime & Regime &
                             \\\\ &&&&& asymmetry & asymmetry & type & type"),
          out = "Arobustcovariates.tex",
          label = "covars",
          covariate.labels= c("Pr(Fail)", "IV: BIT signing", 
                              "CIT Dependence", "Learning",
                              "Capability asymmetry", 
                              "Regime type: Party",
                              "Regime type: Personalist", "Regime type: Oligarchy", 
                              "Regime type: Military",
                              "Year", "Year squared", "Year cubed",
                              "Bureaucratic quality",
                              "Regional Competition", "GDP per capita (logged)"), 
          dep.var.labels = c("Selection: DTA signing Outcome: Source index"),
          omit = "region", omit.labels = "Region Fixed Effects",
          digits = 2, 
          omit.stat=c("f"), 
          notes.align = "l", 
          notes= c("Clustered standard errors", "on leader in parentheses"),
          se =list(sqrt(diag(R4_CIT$leadercluster))[R4_CIT$param$index$betaS],
                   sqrt(diag(R4_CIT$leadercluster))[R4_CIT$param$index$betaO],
                   sqrt(diag(R4_learning$leadercluster))[R4_learning$param$index$betaS],
                   sqrt(diag(R4_learning$leadercluster))[R4_learning$param$index$betaO],
                   sqrt(diag(R4_capability$leadercluster))[R4_capability$param$index$betaS],
                   sqrt(diag(R4_capability$leadercluster))[R4_capability$param$index$betaO],
                   sqrt(diag(R4_regime$leadercluster))[R4_regime$param$index$betaS],
                   sqrt(diag(R4_regime$leadercluster))[R4_regime$param$index$betaO]),
          no.space=TRUE)

#### Robusttest 2: ATH meaure. Simple index ####

#Simple index
modelDatasimple <- filter(ATHdata, year > 1950) #So the predictions will match the dataset you make them in
ATHsimple <- glm(failure ~ poly(start, 3) 
                 + pastfails10 
                 + interwar  
                 + intrawar  
                 + Navco_events 
                 + coup 
                 + Military 
                 + Party
                 + Personalist
                 + Oligarchy #Dummies for regime types. Monarchy is the reference category.
                 #+ GDPgrowth1
                 #+ log1p(Oilrents)
                 #+ log1p(GDPpercap1)
                 + as.factor(ccode), 
                 #+ as.factor(region), 
                 data = modelDatasimple,
                 family = binomial(link = "probit"), 
                 y = TRUE, na.action = "na.exclude")# The model object keeps the observations not in the model because of missingness. 
modelDatasimple$athdatasimple <- predict(ATHsimple, type="response") #Predicting ## The order is the same in the two, so we don't need left join or merge. 
sum(table(modelDatasimple$athdatasimple)) #4443 predictions of 5778

#Seperationplot - How well does the model predict - Simple version
separationplot(pred = fitted(ATHsimple), # Have to remove na.exclude in ATHsimple probit first for this to work. 
               actual = ATHsimple$y)

simplemasterdatadyad <- left_join(modelDatasimple, Original_hostasym2) %>%
  mutate(treaties = ifelse(is.na(treaties) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                           treaties), # put treaty to 0 if year is bigger than the first year in Original_hosthome2
         treaty = ifelse(is.na(treaty) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                         treaty))

simplemasterdatadyad1 <- simplemasterdatadyad  %>%
  mutate(treaties = ifelse(treaties > 0 & enddate > signdate & startdate < signdate, treaties, 0),
         treaty = ifelse(treaty > 0 & enddate > signdate & startdate < signdate, treaty, 0),
         source = ifelse(source > 0 & enddate > signdate & startdate < signdate, source, NA), #Source is NA
                 signdate = ifelse(signdate > 0 & enddate > signdate & startdate < signdate, signdate, NA))

##Add control variables and IV ##
## 1. Bureaucratic quality
#rigadmin is already in masterdatadyad

## Add regional competition measured by number of treaties in region past 10 years (with one year lag)
simpledyadburcomp <- simplemasterdatadyad1 %>%
  group_by(region) %>% ## using group_by we make sure the lag is within countries
  arrange(region, year) %>%
  mutate(competition = lag(treaties), # Leaving first treaties in every new region-year be NA 
         #because we don't know what failure was before the dataset begins
         competitions = cumsum(ifelse(is.na(competition),0,competition)), #Adds number of treaties from beginning of observations
         competition10 = runner( x = ifelse(is.na(competition),0,competition), #number of treaties past ten years
                                 f = sum, 
                                 k = 10))

simpledyadburcomp1 <- simpledyadburcomp %>% #Lag the effect with one year
  mutate(comp10lag = ifelse(year == lag(year), competition10, lag(competition10)))

## Add IV - BIT-signing data 
BITdata <- read_excel("DUNCTAD_IIA3.xlsx")
# Make year-variable 
BITdata$year <- format(as.Date(BITdata$Date.of.signature, format="%d/%m/%Y"),"%Y")
BITdata$year <- as.numeric(BITdata$year)
BITdata$Date.of.signature <- format(as.Date(BITdata$Date.of.signature,format="%d/%m/%Y"), "%Y-%m-%d")


#Make one observation per signing countries - all agreements are counted twice
BITdata1 <- BITdata %>%
  dplyr::select(Parties.1, year, Date.of.signature) %>%
  dplyr::rename(ccode = Parties.1) 

BITdata2 <- BITdata %>%
  dplyr::select(Parties.2, year, Date.of.signature) %>%
  dplyr::rename(ccode = Parties.2)

BITdataboth <-rbind(BITdata1, BITdata2) #bind: double length

#Make correct ccode 
BITdataboth$ccode <- countrycode(BITdataboth$ccode, origin = "country.name", destination = "cown")

sum(is.na(BITdataboth$ccode)) # Only 629 NAs

BITdataboth1 <- BITdataboth %>%
  mutate(BITPTA = 1) ## When merging all observations with BIT will be 1 and rest will be NA

simpledyadburcitcompbit0 <- left_join(simpledyadburcomp1, BITdataboth1) 

simpledyadburcitcompbit00 <- simpledyadburcitcompbit0  %>%
  mutate(BITPTA = ifelse(BITPTA > 0 & enddate > Date.of.signature & startdate < Date.of.signature, BITPTA, NA))

#new mean variables
simpledyadburcitcompbit1 <- simpledyadburcitcompbit00 %>%
  filter(is.na(ccode) == FALSE, 
         is.na(year) == FALSE) %>%
  arrange(desc(leader, year)) %>% 
  group_by(leader, year) %>% 
  mutate(region = toString(unique(region)),
         athdatasimple= mean(athdatasimple), 
         rigadmin = mean(rigadmin),
         Military = mean(Military), 
         Monarchy = mean(Monarchy),
         Party = mean(Party),
         Oligarchy = mean(Oligarchy),
         Personalist = mean(Personalist), 
         VdemGDP = mean(VdemGDP), 
         cincdif = mean(cincdif), 
         ccode = mean(ccode),
         year = mean(year), 
         start = mean(start),
         coup = mean(coup), 
         comp10lag = mean(comp10lag),
         source = mean(source)) 

simpledyadburcitcompbit12 <- simpledyadburcitcompbit1 %>%
  dplyr::select(obsid, 
                leader,
                ccode,
                year,
                region,
                athdatasimple, 
                rigadmin,
                Military, 
                Monarchy,
                start,
                Party,
                Oligarchy,
                Personalist, 
                VdemGDP, 
                cincdif, 
                coup, 
                comp10lag,
                treaty,
                source,
                BITPTA
  )

simpledyadburcitcompbitdist <- distinct(simpledyadburcitcompbit12, .keep_all = FALSE)

simpledyadburcitcompbit <- simpledyadburcitcompbitdist %>%
  group_by(leader)%>%
  arrange(leader, year) %>% 
  mutate(pastbit = lag(BITPTA),
         pastbits = cumsum(ifelse(is.na(pastbit),0,pastbit)))

# Throw out treaties where ccode is not the countries in the Tax Treaties Explorer data
# 1. Make ccode of all countries in sample (118)
simpletaxcountries <-  c("Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", 
                   "Azerbaijan", "Bangladesh", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil",
                   "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Central African Republic", "Chad", "China", 
                   "Colombia", "Comoros", "Côte d'Ivoire", "Democratic People’s Republic of Korea", "Democratic Republic of Congo", 
                   "Djibouti", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini",
                   "Ethiopia", "Federal States of Micronesia", "Gabon", "Georgia", "Ghana", "Guatemala", "Guinea", "Guinea-Bissau", 
                   "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran", "Iraq", "Jordan", "Kenya", "Kiribati", "Kosovo", "Kyrgyz Republic",
                   "Lao PDR", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Maldives", "Mali", "Marshall Islands",
                   "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Nicaragua", 
                   "Niger", "Nigeria", "North Macedonia", "Pakistan", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Republic of Congo", 
                   "Rwanda", "Samoa", "São Tomé and Principe", "Senegal", "Seychelles", "Sierra Leone", "Solomon Islands", "Somalia", "South Africa",
                   "South Sudan", "Sri Lanka", "Sudan", "Syrian Arab Republic", "Tajikistan", "Tanzania", "Thailand", "The Gambia", 
                   "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uganda", "Ukraine", "Uzbekistan", 
                   "Vanuatu", "Venezuela", "Vietnam", "West Bank and Gaza", "Yemen", "Zambia", "Zimbabwe")
simpletaxcountries <- as.data.frame(simpletaxcountries)
#2. making ccode 
simpletaxcountries$ccode <- countrycode(simpletaxcountries$simpletaxcountries, origin = 'country.name', destination = 'cown')
# West bank was not matched

simpletaxcountries$ccodesample <- simpletaxcountries$ccode

#3. throw out if they do not match
simpledyadburcitcompbitsample <- left_join(simpledyadburcitcompbit, simpletaxcountries)
simplesample <- simpledyadburcitcompbitsample %>%
  filter(ccodesample >= 0)

sum(is.na(simpledyadburcitcompbitsample$ccodesample)) #1362 removed observations

#### Robustness 2: ATH meaure. Running Heckman with Simple Index ####

set.seed(1)
robust2simple <- selection(treaty ~ athdatasimple
                           + pastbits
                           + poly(year, 3)
                           + rigadmin
                           + Military 
                           + comp10lag # treaties signed past 10 years in region, lagged by one year
                           + log(VdemGDP), #Dummier for regime types. Monarchy is the reference category. 
                           source ~ athdatasimple
                           + poly(year, 3)
                           +rigadmin
                           + Military 
                           + comp10lag # treaties signed past 10 years in region, lagged by one year
                           + log(VdemGDP), 
                           data = filter(simplesample, VdemGDP > 0), 
                           method = "ml")

#### Robusttest 2: ATH meaure. Start as proxy ####

# Make length of time in posision as years-variable
simplesample1 <- simplesample %>%
  mutate(yearin_position = ifelse(leader == lag(leader), start/365, 0))
simplesample2 <- simplesample1 %>%
  mutate(yearin_position = as.numeric(substr(yearin_position, 1,1)))

robust2start <- selection(treaty ~ yearin_position
                          + pastbits
                          + poly(year, 3)
                          + rigadmin
                          + Military #All other categories as reference
                          + comp10lag # treaties signed past 10 years in region, lagged by one year
                          + log(VdemGDP),  
                          source ~ yearin_position
                          + poly(year, 3)
                          +rigadmin
                          + Military 
                          + comp10lag # treaties signed past 10 years in region, lagged by one year
                          + log(VdemGDP), 
                          data = filter(simplesample2, VdemGDP > 0), 
                          method = "ml")
summary(robust2start)

#### Robusttest 2: ATH meaure. Coup as proxy ####

robust2coup <- selection(treaty ~ coup
                         + pastbits
                         + poly(year, 3)
                         + rigadmin
                         + Military #All others as reference categories
                         + comp10lag # treaties signed past 10 years in region, lagged by one year
                         + log(VdemGDP),  
                         source ~ coup
                         + poly(year, 3)
                         +rigadmin
                         + Military 
                         + comp10lag # treaties signed past 10 years in region, lagged by one year
                         + log(VdemGDP), 
                         data = filter(simplesample2, VdemGDP > 0), 
                         method = "ml")

summary(robust2coup)
#### Robusttest 2: ATH meaure. Clustring standard errors ####

#Simple
simplemodelMatrix <- filter(simplesample, VdemGDP > 0) %>% 
  dplyr::select(treaty, pastbits, year, rigadmin, athdatasimple,
                Military, comp10lag,  
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  na.omit()

robust2simple$leadercluster <- vcovCL(robust2simple, cluster = simplemodelMatrix$leader)

#start 
startmodelMatrix <- filter(simplesample2, VdemGDP > 0) %>% 
  dplyr::select(treaty, pastbits, year, rigadmin, 
                Military, comp10lag, coup, yearin_position,
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  na.omit()

robust2start$leadercluster <- vcovCL(robust2start, cluster = startmodelMatrix$leader)

#coup
coupmodelMatrix <- filter(simplesample2, VdemGDP > 0) %>% 
  dplyr::select(treaty, pastbits, year, rigadmin, 
                Military, comp10lag, coup,
                VdemGDP, ccode, leader, source) %>% 
  mutate(source = ifelse(treaty == 0, 0, source)) %>% 
  na.omit()

robust2coup$leadercluster <- vcovCL(robust2coup, cluster = coupmodelMatrix$leader)

#### Robusttest 2: ATH meaure. Tables and plots ####

## Stargazer of comparison of the content of the indices 
stargazer(ATHindex, ATHsimple, object.names=FALSE, model.numbers=TRUE,
          title="Simpler ATH probit",
          align=TRUE, 
          dep.var.labels=c("Autocratic leadership failure"), 
          omit = "ccode", omit.labels = "Country Fixed effects",
          out = "AsimpleandATHindexprobits.tex",
          label = "comparesimpleprobit",
          column.labels = c("ATH probit in main model", "Simpler ATH probit"),
                    covariate.labels= c("Start", "Start squared ", "Start cubed",
                              "Past Failures", "Interwar", 
                              "Intrawar", "Campaign", 
                              "Coup", "Regime type dummy: Military", 
                              "Regime type dummy: Party", 
                              "Regime type dummy: Personalist", 
                              "Regime type dummy: Oligarchy", "GDP growth", "Log Oil production", 
                              "Log GDP per cap"), 
          digits = 2, 
          omit.stat=c("f"), 
          notes.align = "l", 
          notes= c("Standard errors in parentheses"),
          no.space=TRUE)

## BUTON robusttest2 models Heckman - maximum likelyhood estimation 

#simple
tobit_simple <- robust2simple
tobit_simple$param$index$betaO <- robust2simple$param$index$betaS
tobit_simple$param$index$betaS <- robust2simple$param$index$betaO

#start
tobit_start <- robust2start
tobit_start$param$index$betaO <- robust2start$param$index$betaS
tobit_start$param$index$betaS <- robust2start$param$index$betaO

#coup
tobit_coup <- robust2coup
tobit_coup$param$index$betaO <- robust2coup$param$index$betaS
tobit_coup$param$index$betaS <- robust2coup$param$index$betaO

stargazer(robust2simple, tobit_simple, robust2start, tobit_start, robust2coup, tobit_coup, selection.equation = TRUE, 
          title="Heckman (ML) Alternative ATH measurements",   
          label = "heckmanATHmeasures",
          column.labels = c("\\\\&  Selection: & Outcome: & 
                              Selection: & Outcome: & 
                              Selection: & Outcome: \\\\ & Simple ATH probit & Simple ATH probit &
                            Years in position & Years in position & Coup & Coup"),
          dep.var.labels = c("Selection: DTA signing Outcome: Source index"),# Is adjusted in Latex
          covariate.labels= c("Simpler ATH probit", "Proxy Years in position", "Proxy Coup", 
                              "IV: Past BIT signing", "Year", "Year squared", "Year cubed", "Bureaucratic quality",
                              "Regime type: Military",
                              "Regional competition", "Log GDP per cap"), 
          out = "Arobust2Heckman.tex",
          digits = 2, 
          omit.stat=c("f"), 
          notes.align = "l", 
          notes= c("Clustered standard errors", "on leader in parentheses"),
          se =list(sqrt(diag(robust2simple$leadercluster))[robust2simple$param$index$betaS],
                    sqrt(diag(robust2simple$leadercluster))[robust2simple$param$index$betaO],
                    sqrt(diag(robust2start$leadercluster))[robust2start$param$index$betaS],
                   sqrt(diag(robust2start$leadercluster))[robust2start$param$index$betaO],
                   sqrt(diag(robust2coup$leadercluster))[robust2coup$param$index$betaS],
                   sqrt(diag(robust2coup$leadercluster))[robust2coup$param$index$betaO]
                  ),
          no.space=TRUE)

#Simpler Pr(Fail) over time
simplertime <- ggplot(modelDatasimple, aes(x=year, y=athdatasimple)) +
  geom_point(size = 1, colour = "#FF6666", alpha = 0.7) +
  xlab("Year") + #These are both in force and not
  ylab("Simpler Pr(Fail)") +
  geom_smooth(method="loess", colour = "black", linetype = 1, size = 0.7) +
  theme_bw() +
  scale_x_continuous(breaks=number_ticks(8))

simplertime + My_Theme + 
  ylim(0,1) 
#### Robusttest 3: Other estimators. Probit logit + tables ####

## Probit 
mainsampleprobit <- mainsample %>%
  dplyr::select(treaty, leader, ccode, athdatawprob, pastbits, year, rigadmin, Military, Monarchy, Oligarchy, Personalist, Party, comp10lag, VdemGDP)

probitdata <- filter(mainsampleprobit, year > 1950) #So the predictions will match the dataset you make them in
probit <- glm(treaty ~ athdatawprob
              + pastbits
              + poly(year,3)
              + rigadmin
              + Military #rest of regime types are ref cat. 
              + comp10lag # treaties signed past 10 years in region, lagged by one year
              + log(VdemGDP),
              data = mainsampleprobit,
              family = binomial(link = "probit"),
              y = TRUE, na.action = "na.exclude")
probitdata$predprobit <- predict(probit, type="response") 

# Logit 
logitdata <- filter(mainsampleprobit, year > 1950) #So the predictions will match the dataset you make them in
Aestimatorlogit <- glm(treaty ~ athdatawprob
              + pastbits
              + poly(year,3)
              + rigadmin
              + Military #rest of regime types are ref cat. 
              + comp10lag # treaties signed past 10 years in region, lagged by one year
              + log(VdemGDP),
              data = mainsample,
              family = binomial(link = "logit"),
              y = TRUE, na.action = "na.exclude")
logitdata$predlogit <- predict(Aestimatorlogit, type="response") 

# Logit with cubed time trend on start
logitdatastart <- filter(mainsampleprobit, year > 1950) #So the predictions will match the dataset you make them in
Aestimatorlogitstart <- glm(treaty ~ athdatawprob
                       + pastbits
                       + poly(start,3)
                       + rigadmin
                       + Military #rest of regime types are ref cat. 
                       + comp10lag # treaties signed past 10 years in region, lagged by one year
                       + log(VdemGDP),
                       data = mainsample,
                       family = binomial(link = "logit"),
                       y = TRUE, na.action = "na.exclude")
logitdatastart$predlogit <- predict(Aestimatorlogitstart, type="response") 

# Clustring standard errors probit and logit

probit$leadercluster <- vcovCL(probit, cluster = mainsampleprobit$leader)
Aestimatorlogit$leadercluster <- vcovCL(Aestimatorlogit, cluster = mainsample$leader)
Aestimatorlogitstart$leadercluster <- vcovCL(Aestimatorlogitstart, cluster = mainsample$leader)

#Logit/probit/Logit other time trend BUTON
stargazer(probit, Aestimatorlogit, Aestimatorlogitstart,
          title="Alternative estimator: Logit",       
          model.numbers=FALSE,
          notes.align = "l", 
          notes= c("Clustered standard errors", "on country in parentheses"),
          se =list(sqrt(diag(probit$leadercluster)),
                        sqrt(diag(Aestimatorlogit$leadercluster)),
                             sqrt(diag(Aestimatorlogitstart$leadercluster))), 
                   covariate.labels= c("Pr(Fail)", 
                              "Past BIT signing",
                              "Year", "Year squared", "Year cubed", "Start", "Start squared", "Start cubed",
                              "Bureaucratic quality", "Regime type: Military", 
                              "Regional Competition", "Log GDP per capita" ), 
          dep.var.labels = c("DTA signing"),
          digits = 2, 
          label = "logit",
          omit.stat=c("f"),
          out = "ArobustestimatorLogit.tex",
          no.space=TRUE)

Probitseperation <- separationplot(pred = fitted(probit), # Have to remove na.exclude in ATHindex probit first for this to work. 
                                  actual = probit$y)
Logitseperation <- separationplot(pred = fitted(Aestimatorlogit), # Have to remove na.exclude in ATHindex probit first for this to work. 
                                  actual = Aestimatorlogit$y)
Logittimeseperation <- separationplot(pred = fitted(Aestimatorlogitstart), # Have to remove na.exclude in ATHindex probit first for this to work. 
                                  actual = Aestimatorlogitstart$y)

#### Robusttest 3: Other estimators. Source is 0 (Not NA)! Merge treatydata (Original_hostasym2) with modelData (ATH Index) Add control variables and IV #### ####

#Merge this with modelData, where treaties-variable is 0 for the period and countries which are covered by Original_hosthomeasym2
#and NA for what it does not cover. # left_join here to delete observations in the Original_hostasym2 that are not in the modelData
#doubletreaty has all the ccodes in treatydata as ccode
masterdatadyad <- left_join(modelData, Original_hostasym2) %>%
  mutate(treaties = ifelse(is.na(treaties) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                           treaties), # put treaty to 0 if year is bigger than the first year in Original_hosthome2
         treaty = ifelse(is.na(treaty) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                         treaty),
         source = ifelse(is.na(source) & year >= min(Original_hostasym2$year) & ccode %in% Original_hostasym2$ccode, 0, 
                         source))

masterdatadyad1 <- masterdatadyad  %>%
  mutate(treaties = ifelse(treaties > 0 & enddate > signdate & startdate < signdate, treaties, 0),
         treaty = ifelse(treaty > 0 & enddate > signdate & startdate < signdate, treaty, 0),
         source = ifelse(source > 0 & enddate > signdate & startdate < signdate, source, 0),
         cincdif = ifelse(cincdif > 0 & enddate > signdate & startdate < signdate, cincdif, 0),
         GDPdif = ifelse(GDPdif > 0 & enddate > signdate & startdate < signdate, GDPdif, 0),
         signdate = ifelse(signdate > 0 & enddate > signdate & startdate < signdate, signdate, NA))

## Add control variables and IV
## 1. Bureaucratic quality
#rigadmin is already in masterdatadyad

dyadburcomp <- masterdatadyad1 %>%
  group_by(region) %>% ## using group_by we make sure the lag is within countries
  arrange(region, year) %>%
  mutate(competition = lag(treaties), # Leaving first treaties in every new region-year be NA 
         #because we don't know what failure was before the dataset begins
         competitions = cumsum(ifelse(is.na(competition),0,competition)), #Adds number of treaties from beginning of observations
         competition10 = runner( x = ifelse(is.na(competition),0,competition), #number of treaties past ten years
                                 f = sum, 
                                 k = 10))

dyadburcomp1 <- dyadburcomp %>% #Lag the effect with one year
  mutate(comp10lag = ifelse(year == lag(year), competition10, lag(competition10)))

## Add IV - BIT-signing data 
BITdata <- read_excel("DUNCTAD_IIA3.xlsx")
# Make year-variable 
BITdata$year <- format(as.Date(BITdata$Date.of.signature, format="%d/%m/%Y"),"%Y")
BITdata$year <- as.numeric(BITdata$year)
BITdata$Date.of.signature <- format(as.Date(BITdata$Date.of.signature,format="%d/%m/%Y"), "%Y-%m-%d")


#Make one observation per signing countries - all agreements are counted twice
BITdata1 <- BITdata %>%
  dplyr::select(Parties.1, year, Date.of.signature) %>%
  dplyr::rename(ccode = Parties.1) 

BITdata2 <- BITdata %>%
  dplyr::select(Parties.2, year, Date.of.signature) %>%
  dplyr::rename(ccode = Parties.2)

BITdataboth <-rbind(BITdata1, BITdata2) #bind: double length

#Make correct ccode 
BITdataboth$ccode <- countrycode(BITdataboth$ccode, origin = "country.name", destination = "cown")

sum(is.na(BITdataboth$ccode)) # Only 629 NAs

BITdataboth1 <- BITdataboth %>%
  mutate(BITPTA = 1) ## When merging all observations with BIT will be 1 and rest will be NA

dyadburcitcompbit0 <- left_join(dyadburcomp1, BITdataboth1) 

dyadburcitcompbit00 <- dyadburcitcompbit0  %>%
  mutate(BITPTA = ifelse(BITPTA > 0 & enddate > Date.of.signature & startdate < Date.of.signature, BITPTA, NA))

#new mean variables
dyadburcitcompbit1 <- dyadburcitcompbit00 %>%
  filter(is.na(ccode) == FALSE, 
         is.na(year) == FALSE) %>%
  arrange(desc(leader, year)) %>% 
  group_by(leader, year) %>% 
  mutate(region = toString(unique(region)),
         athdatawprob= mean(athdatawprob), 
         rigadmin = mean(rigadmin),
         Military = mean(Military), 
         Monarchy = mean(Monarchy),
         Party = mean(Party),
         Oligarchy = mean(Oligarchy),
         Personalist = mean(Personalist), 
         VdemGDP = mean(VdemGDP), 
         cincdif = mean(cincdif), 
         ccode = mean(ccode),
         year = mean(year), 
         start = mean(start),
         coup = mean(coup), 
         comp10lag = mean(comp10lag),
         source = mean(source)) 

dyadburcitcompbit12 <- dyadburcitcompbit1 %>%
  dplyr::select(obsid, 
                leader,
                ccode,
                year,
                region,
                athdatawprob, 
                rigadmin,
                Military, 
                Monarchy,
                Party,
                Oligarchy,
                Personalist, 
                VdemGDP, 
                cincdif, 
                coup, 
                comp10lag,
                treaty,
                treaties,
                source,
                BITPTA
  )

dyadburcitcompbitdist <- distinct(dyadburcitcompbit12, .keep_all = FALSE)

dyadburcitcompbit0 <- dyadburcitcompbitdist %>%
  group_by(leader)%>%
  arrange(leader, year) %>% 
  mutate(pastbit = lag(BITPTA),
         pastbits = cumsum(ifelse(is.na(pastbit),0,pastbit)))

table(dyadburcitcompbit0$source) #3829 0's 1382 NAs 590 sourceverdier.


# Throw out treaties where ccode is not the countries in the Tax Treaties Explorer data
# 1. Make ccode of all countries in sample (118)
taxcountries0 <-  c("Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", 
                   "Azerbaijan", "Bangladesh", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil",
                   "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Central African Republic", "Chad", "China", 
                   "Colombia", "Comoros", "Côte d'Ivoire", "Democratic People’s Republic of Korea", "Democratic Republic of Congo", 
                   "Djibouti", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini",
                   "Ethiopia", "Federal States of Micronesia", "Gabon", "Georgia", "Ghana", "Guatemala", "Guinea", "Guinea-Bissau", 
                   "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran", "Iraq", "Jordan", "Kenya", "Kiribati", "Kosovo", "Kyrgyz Republic",
                   "Lao PDR", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Maldives", "Mali", "Marshall Islands",
                   "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Nicaragua", 
                   "Niger", "Nigeria", "North Macedonia", "Pakistan", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Republic of Congo", 
                   "Rwanda", "Samoa", "São Tomé and Principe", "Senegal", "Seychelles", "Sierra Leone", "Solomon Islands", "Somalia", "South Africa",
                   "South Sudan", "Sri Lanka", "Sudan", "Syrian Arab Republic", "Tajikistan", "Tanzania", "Thailand", "The Gambia", 
                   "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uganda", "Ukraine", "Uzbekistan", 
                   "Vanuatu", "Venezuela", "Vietnam", "West Bank and Gaza", "Yemen", "Zambia", "Zimbabwe")
taxcountries0 <- as.data.frame(taxcountries0)
#2. making ccode 
taxcountries0$ccode <- countrycode(taxcountries0$taxcountries0, origin = 'country.name', destination = 'cown')
# West bank was not matched

taxcountries0$ccodesample <- taxcountries0$ccode

#3. throw out if they do not match
dyadburcitcompbitsample0 <- left_join(dyadburcitcompbit0, taxcountries0)
sample0 <- dyadburcitcompbitsample0 %>%
  filter(ccodesample >= 0)

sum(is.na(dyadburcitcompbitsample0$ccodesample)) #1362 removed observations


#### Robusttest 3: Other estimators. Tobit (with 0 on Source) tables ####
#Lagged source
sample0lag <- sample0 %>% #Lag the effect with one year
  group_by(leader) %>%
  mutate(sourcelag = ifelse(year == lag(year), source, lag(source)))


AestimatorTobit <- censReg(formula = sourcelag ~  # Here, the missing data in source has to be 0 (where the two datasets overlap), not NA as in the main model Heckman.
                           #  (1|ccode) 
                           +  athdatawprob
                           + poly(year, 3)
                           + rigadmin
                           + Military 
                           + comp10lag 
                           + log(VdemGDP), left = 0, 
                           data = filter(sample0lag, VdemGDP > 0))


summary(AestimatorTobit)
summary(margEff(AestimatorTobit)) # Are used for the interpretation

##cluster standard errors
tobitmatrix <- filter(sample0lag, VdemGDP > 0) %>% 
  dplyr::select(pastbits, year, rigadmin, 
                Military, comp10lag, athdatawprob,
                VdemGDP, ccode, leader, sourcelag)%>% 
  na.omit()
 # mutate(source = ifelse(treaty == 0, 0, sourcelag)) %>% 

AestimatorTobit$leadercluster <- vcovCL(AestimatorTobit, cluster = tobitmatrix$leader)

stargazer(AestimatorTobit, 
          title="Alternative estimator: Tobit",       
          model.numbers=FALSE,
          covariate.labels= c("Pr(Fail)", "Year", "Year squared", "Year cubed",
                              "Bureaucratic quality", "Regime type: Military", 
                              "Regional Competition", "Log GDP per capita", "Log Sigma"), 
          dep.var.labels = c("Lagged Source Index"),
          notes.align = "l", 
          notes= c("Clustered standard errors", "on leader in parentheses"),
          se =list(sqrt(diag(AestimatorTobit$leadercluster))),
                    digits = 2, 
          omit.stat=c("f"),
          out = "ArobustestimatorTobit.tex",
          label = "Tobit",
          no.space=TRUE)

#### Robusttest 3: Other estimatiors. OLS tables ####

mainsamplelag <- mainsample %>% #Lag the effect with one year
  group_by(leader) %>%
  mutate(sourcelag = ifelse(year == lag(year), source, lag(source)))

OLS <- lm(sourcelag ~ # NA (so only the observations with source are included)
             + athdatawprob
           + poly(year, 3)
           + rigadmin
           + Military # all other are ref
           + comp10lag 
           + log(VdemGDP),
           data = filter(mainsamplelag, VdemGDP > 0))

OLS0 <- lm(sourcelag ~ # 0 here!
            + athdatawprob
          + poly(year, 3)
          + rigadmin
          + Military # all other are ref
          + comp10lag 
          + log(VdemGDP),
          data = filter(sample0lag, VdemGDP > 0))

OLS1 <- lm(source ~ # 0 here !
             + athdatawprob
           + poly(year, 3)
           + rigadmin
           + Military # all other are ref
           + comp10lag 
           + log(VdemGDP),
           data = filter(sample0, VdemGDP > 0))

##Cluster standard errors

#OLS
OLSmatrix <- filter(mainsamplelag, VdemGDP > 0) %>% 
  dplyr::select(pastbits, year, rigadmin, 
                Military, comp10lag, athdatawprob,
                VdemGDP, ccode, leader, sourcelag)%>% 
  na.omit()
# mutate(source = ifelse(treaty == 0, 0, sourcelag)) %>% 

OLS$leadercluster <- vcovCL(OLS, cluster = OLSmatrix$leader)

## OLS0
OLS0matrix <- filter(sample0lag, VdemGDP > 0) %>% 
  dplyr::select(pastbits, year, rigadmin, 
                Military, comp10lag, athdatawprob,
                VdemGDP, ccode, leader, sourcelag)%>% 
  na.omit()
# mutate(source = ifelse(treaty == 0, 0, sourcelag)) %>% 

OLS0$leadercluster <- vcovCL(OLS0, cluster = OLS0matrix$leader)

## OLS1
OLS1matrix <- filter(sample0, VdemGDP > 0) %>% 
  dplyr::select(pastbits, year, rigadmin, 
                Military, comp10lag, athdatawprob,
                VdemGDP, ccode, leader, source)%>% 
  na.omit()
# mutate(source = ifelse(treaty == 0, 0, sourcelag)) %>%
OLS1$leadercluster <- vcovCL(OLS1, cluster = OLS1matrix$leader)

stargazer(OLS, OLS0, OLS1, 
          title="Alternative estimator: OLS",       
          model.numbers=FALSE,
          column.labels = c("Value on Source index", "Panel", "Panel"),
          covariate.labels= c("Pr(Fail)", "Year", "Year squared", "Year cubed",
                              "Bureaucratic quality", "Regime type: Military", 
                              "Regional Competition", "Log GDP per capita"), 
          dep.var.labels = c("Lagged Source Index", "Source Index"),
          #omit = "year", omit.labels = "Year random effects",
          digits = 2, 
          omit.stat=c("f"),
          notes.align = "l", 
                se =list(sqrt(diag(OLS$leadercluster)),
                   sqrt(diag(OLS0$leadercluster)),
                   sqrt(diag(OLS1$leadercluster))), 
          notes= c("Clustered standard errors", "on leader in parentheses"),
          label = "OLS",
          out = "ApanelOLS.tex",
          no.space=TRUE)


#### Workspace ####

## Lagre workspace
save.image("Julie6June.RData")


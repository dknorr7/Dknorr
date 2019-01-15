library(pacman)
p_load(tidycensus, tidyverse, scales,sf, tigris, viridis, ggthemes, ggplot2,
       stringr, dplyr, leaflet, broom, censusapi, acs, lubridate, reshape)

#tidycensus
census_api_key('e1bf01270616e86ab00da4cd8e15c94be3f13bd3', overwrite = TRUE, install = TRUE)
## censusapi
Sys.setenv(CENSUS_KEY="e1bf01270616e86ab00da4cd8e15c94be3f13bd3", overwrite = TRUE)
# Reload .Renviron

#acs key set
api.key.install("e1bf01270616e86ab00da4cd8e15c94be3f13bd3")

readRenviron("~/.Renviron")


#Create inflation adjustment table
monthly_cpi <-
  read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
             skip = 53, header = TRUE)

monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))

yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2017]


#Load variables from tidycensus
vars2010 <- load_variables(2010, "acs5", cache = TRUE)

#Load variables from censusapi
All_Census_Variables <- listCensusMetadata(name= "2016/acs/acs5/subject", type = "variables")

apis <- listCensusApis()


###Set census call variable years and geography ("tract" or "block group")
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016)
geo = "block group"
State = "TN"
County = "Davidson"
masterPath <- "C://School//Research//Predictive Variables//Master//"

#Median Household Income
foo <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data <- get_acs(geography = geo, table = "B19013", state = State, county = County, 
                      year = loop_year, cache_table = TRUE) %>%
    mutate(year = loop_year)
  foo <- bind_rows(foo, acs_data)
}

#inflation adjustment
MedHHInc_adj <- merge(x = foo, y = yearly_cpi, by.x = "year", by.y = "cpi_year")

MedInc_LT <- MedHHInc_adj %>% mutate(variable = paste("Median HH Income- ", geo), inf_adj_value = estimate/adj_factor) %>%
  select(CensusTractID = GEOID, variable, year, MedIncome = inf_adj_value)

saveRDS(MedInc_LT, paste0(masterPath, "Census_MedIncome", ".rds"))



#Percentage of Non-White population 
foo3 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data3 <- get_acs(geography = geo, table = "B02001", state = State, county = County,
                       year = loop_year, cache_table = TRUE) %>%
    mutate(year = loop_year) %>% select(GEOID, variable, estimate, year) %>% spread(key = variable, value = estimate) %>% 
    mutate(frac_nonwhite = (1  - B02001_002 / B02001_001)*100)
  foo3 <- bind_rows(foo3, acs_data3)
}

NonWhite_f <- foo3 %>% select(CensusTractID = GEOID, year , PctNonWhite = frac_nonwhite) %>% mutate(year = as.integer(as.character(year)), variable = paste("Percentage nonwhite-", geo))

saveRDS(NonWhite_f, paste0(masterPath, "Census_NonWhitePct", ".rds"))



#Percentage of Multi-Unit Households 
foo5 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data5 <- get_acs(geography = geo, table = "B25024", state = State, county = County,
                       year = loop_year, cache_table = TRUE, output = "wide") %>%
    select(GEOID, Total = B25024_001E, single1 = B25024_002E, single2 = B25024_003E) %>%
    mutate(SingleUnit = single1 + single2, Pct_MultiUnit = 100*((Total - SingleUnit)/Total)) %>%
    mutate(year = loop_year, variable = paste("Percent MultiUnit-", geo)) %>%
    select(CensusTractID = GEOID, variable, Pct_MultiUnit, year)
  foo5 <- bind_rows(foo5, acs_data5)
}

saveRDS(foo5, paste0(masterPath, "Census_MultiUnitPct", ".rds"))



#Percentage of Vacant Households 
foo7 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data7 <- get_acs(geography = geo, table = "B25002", state = State, county = County,
                       year = loop_year, cache_table = TRUE, output = "wide") %>%
    select(GEOID, Vacant = B25002_003E, Occupied = B25002_002E, TotalHHs = B25002_001E) %>%
    mutate(Pct_Vacant = 100*(Vacant/TotalHHs),year = loop_year, variable = paste("Percent Vacant Housing-", geo)) %>%
    select(CensusTractID = GEOID, Pct_Vacant, year, variable)
  foo7 <- bind_rows(foo7, acs_data7)
}

saveRDS(foo7, paste0(masterPath, "Census_VacantPct", ".rds"))



#Percentage of Renters
foo9 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data9 <- get_acs(geography = geo, table = "B25033", state = State, county = County,
                       year = loop_year, cahce_table = TRUE, output = "wide") %>%
    select(GEOID, Total = B25033_001E, RenterHH = B25033_008E, OwnerHH = B25033_002E) %>%
    mutate(Pct_Renters = 100*(RenterHH/Total), year = loop_year, variable = paste("Percent Renters-", geo)) %>%
    select(CensusTractID = GEOID, Pct_Renters, variable, year)
  foo9 <- bind_rows(foo9, acs_data9)
}

saveRDS(foo9, paste0(masterPath, "Census_RentersPct", ".rds"))



#Rent Value 
foo11 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data11 <- get_acs(geography = geo, table = "B25064", state = State, county = County,
                        year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year, variable = paste("Median Gross Rent (inf adjusted 2017)-", geo)) %>%
    select(GEOID, MedianRent = B25064_001E, variable, year)
  foo11 <- bind_rows(foo11, acs_data11)
}

#Inflation Adjustment
MedRentVal <- merge(x = foo11, y = yearly_cpi, by.x = "year", by.y = "cpi_year")

MedRentVal_f <- MedRentVal %>% mutate(inf_adj_value = MedianRent/adj_factor) %>%
  select(CensusTractID = GEOID, variable, year, MedRent = inf_adj_value)

saveRDS(MedRentVal_f, paste0(masterPath, "Census_MedianRentValue", ".rds"))



#Home Value
foo12 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data12 <- get_acs(geography = geo, table = "B25077", state = State, county = County,
                        year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year, variable = paste("Median Home Value (2017 Inflation adjusted)-", geo)) %>%
    select(GEOID, MedianHomeVal = B25077_001E, variable, year)
  foo12 <- bind_rows(foo12, acs_data12)
}

MedHomeVal <- merge(x = foo12, y = yearly_cpi, by.x = "year", by.y = "cpi_year")

MedHomeValue_f <- MedHomeVal %>% mutate(inf_adj_value = MedianHomeVal/adj_factor) %>%
  select(CensusTractID = GEOID, variable, year, MedHomevalue = inf_adj_value)

saveRDS(MedHomeValue_f, paste0(masterPath, "Census_MedianHomeValue", ".rds"))




#Travel Times - Tract
foo20 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data20 <- get_acs(geography = geo, table = "B08303", state = State, county = County, 
                        year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year) %>%
    select(GEOID, year, B08303_001E, B08303_002E, B08303_003E, B08303_004E, B08303_005E) %>% 
    mutate(Total = B08303_001E, Less20min = B08303_002E + B08303_003E + B08303_004E + B08303_005E, 
           Pctless20min = 100*(Less20min/Total), variable = "Percent of working population with sub 20 minute commute") %>%
    select(CensusTractID = GEOID, year, variable, PctCommuteless20min = Pctless20min)
  foo20 <- rbind(foo20, acs_data20)
}

saveRDS(foo20, paste0(masterPath, "Census_Sub20minCommute", ".rds"))



#Access to Vehicles - Tract 
foo21 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data21 <- get_acs(geography = "tract", table = "B08141", state = State, county = County,
                        year = loop_year, cache_table = TRUE, output = "wide") %>%
    select(GEOID, TotalCars = B08141_001E, noCars = B08141_002E) %>%
    mutate(value = 100 - (100*((TotalCars - noCars)/TotalCars)), year = loop_year, variable = "Percent with no car available") %>%
    select(CensusTractID = GEOID, year, variable, NoCarPct = value)

  foo21 <- bind_rows(foo21, acs_data21)
}


saveRDS(foo21, paste0(masterPath, "Census_NoCars.rds"))




#Home Age (NOT USED)
foo22 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data22 <-get_acs(geography = geo, table = "B25034", state = State, county = County,
                       year = loop_year, cache_table = TRUE, output = "wide") %>%
    select(GEOID, Total = B25034_001E, built10to13 = B25034_003E, built2014 = B25034_002E, built90to99 = B25034_005E, built00to10 = B25034_004E) %>% 
    mutate(post90 = built10to13 + built2014 + built90to99 + built00to10, Pct_post90 = 100*(post90/Total)) %>%
    mutate(year = loop_year) %>%
    select(GEOID, Pct_post90, year)
  foo22 <- bind_rows(foo22, acs_data22)
}

Pct_post90T <- foo22 %>% mutate(variable = "Percent homes built After 1990") %>% select(CensusTractID = GEOID, Pct_post90, year, variable) 


saveRDS(Pct_post90T, paste0(masterPath, "Census_PercentNewHomes.rds"))

# #Home Age - Block Group
# foo23 <- tibble()
# for (loop_year in c(2009,2010,2011,2012,2013,2014,2015,2016)) {
#   message("Getting data for ", loop_year)
#   acs_data23 <-get_acs(geography = "block group", table = "B25034", state = State, county = County,
#                        year = loop_year, cache_table = TRUE, output = "wide") %>%
#     select(GEOID, Total = B25034_001E, built10to13 = B25034_003E, built2014 = B25034_002E, built90to99 = B25034_005E, built00to10 = B25034_004E) %>% 
#     mutate(post90 = built10to13 + built2014 + built90to99 + built00to10, Pct_post90 = 100*(post90/Total)) %>%
#     mutate(year = loop_year) %>%
#     select(GEOID, Pct_post90, year)
#   foo23 <- bind_rows(foo23, acs_data23)
# }
# 
# Pct_post90B <- foo23 %>% mutate(year = str_c("estimate_", year)) %>% select(GEOID, Pct_post90, year) %>%
#   spread(key = year, value = Pct_post90)


#_______ Using censusapi
#POVERTY Tract Only
Pov <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data21 <- get_acs(geography = "tract", table = "B17001", state = State, county = County,
                        year = loop_year, cache_table = TRUE, output = "wide") %>%
    select(GEOID, Total = B17001_001E, poverty = B17001_002E) %>%
    mutate(value =  100 -(100*((Total - poverty)/Total)), year = loop_year, variable = "Percent in poverty-tract") %>%
    select(CensusTractID = GEOID, year, variable, PovertyPct = value)
  
  Pov <- bind_rows(Pov, acs_data21)
}


saveRDS(Pov, paste0(masterPath, "Census_PovertyPct.rds"))




#Percentage of Unemployment - Tract - 2011-2016 only
foo15 <- tibble()
for (loop_year in c(2011, 2012, 2013, 2014, 2015, 2016)) {
  acs_data15 <- get_acs(geography = geo, table = "B23025", state = State, county = County,
                        year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(unemployment = 100*(B23025_005E/B23025_002E), year = loop_year, variable = paste("Percent over 16 unemployment-", geo)) %>%
    select(GEOID, year, variable, UnemploymentPct = unemployment)
  foo15 <- bind_rows(foo15, acs_data15)
}

geolytics <- read.csv('C:/School/Research/Geolytics/Geolytics_ALL.csv')
Unemployed2010 <- geolytics %>% select(GEO2010, nUnemployed_10 = UNEMPT1AN, nTotal = UNEMPT1AD) %>% mutate(GEOID = as.character(GEO2010),UnemploymentPct = (nUnemployed_10/nTotal) *100, variable = "Percent over 16 unemployment- tract", year = 2010) %>%
  select( -GEO2010, -nUnemployed_10, -nTotal)

Unemployed_all <- bind_rows(foo15,Unemployed2010) %>% select(CensusTractID = GEOID, UnemploymentPct, variable, year)

saveRDS(Unemployed_all, paste0(masterPath, "Census_UnemploymentPct", ".rds"))




#Age demos for 2010

Age_2010 <- geolytics %>% select(CensusTractID = GEO2010,CHILD1, OLD1) %>% 
  mutate(Under18_10 = 100*CHILD1, Over65_10 = 100*OLD1, year = 2010) %>%
  select(-CHILD1, -OLD1)

saveRDS(Age_2010, paste0(masterPath, "Census_Age2010_", geo, ".rds"))

#Non-Family Households - Tract - 2015-2016 B11001
NonFam <- tibble()
for (loop_year in years) {
  acs_datanonfam <- get_acs(geography = geo, table = "B11001", state = State, county = County,
                        year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(NonFamily = 100*(B11001_007E/B11001_001E), year = loop_year, variable = paste("Percent Nonfamily Households-", geo)) %>%
    select(CensusTractID = GEOID, year, variable, NonFamPct = NonFamily)
  NonFam <- bind_rows(NonFam, acs_datanonfam)
}

saveRDS(NonFam, paste0(masterPath, "Census_NonFamilyPct", ".rds"))



#Median Population Age
Age <- tibble()
for (loop_year in years) {
  acs_dataAge <- get_acs(geography = geo, table = "B01002", state = State, county = County,
                            year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year, variable = paste("Median Age-", geo)) %>%
    select(CensusTractID = GEOID, year, variable, MedianPopAge = B01002_001E)
  Age <- bind_rows(Age, acs_dataAge)
}

saveRDS(Age, paste0(masterPath, "Census_MedianPopAge", ".rds"))



#Rent Burdened
RentBurdened <- tibble()
for (loop_year in years) {
  acs_rentburdened <- get_acs(geography = geo, table = "B25070", state = State, county = County,
                         year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year, variable = paste("Rent Burdened-", geo)) %>%
    select(GEOID, year, variable, TotalHH = B25070_001E, rent30_35 = B25070_007E, rent35_40 = B25070_008E , rent40_50 = B25070_009E, rent_50plus = B25070_010E) %>%
    mutate(BurdenCnt = rent30_35 + rent35_40 + rent40_50 + rent_50plus, BurdenPct = 100*(BurdenCnt/TotalHH)) %>% select(CensusTractID = GEOID, year, RentBurdenPct = BurdenPct, variable)
  RentBurdened <- bind_rows(RentBurdened, acs_rentburdened)
}

saveRDS(RentBurdened, paste0(masterPath, "Census_RentBurdened",".rds"))




####Commuting characteristics
PublicCommute <- tibble()
for (loop_year in years) {
  acs_pubcommunte <- get_acs(geography = geo, table = "B08006", state = State, county = County,
                              year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year, variable = paste("Public Commute Pct-", geo)) %>%
    select(GEOID, year, variable, TotalCommuters = B08006_001E, PublicCommute = B08006_008E) %>%
    mutate(PctPubCommute = 100*(PublicCommute/TotalCommuters)) %>%
      select(CensusTractID = GEOID, year, PctPubCommute, variable)
  PublicCommute <- bind_rows(PublicCommute, acs_pubcommunte)
}

saveRDS(PublicCommute, paste0(masterPath, "Census_PublicCommute",".rds"))


#Vehicles Available B08141_005E
Vehicles<- tibble()
for (loop_year in years) {
  acs_vehicles <- get_acs(geography = geo, table = "B08141", state = State, county = County,
                             year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year, variable = paste("Percentage of HH with 3+ vehicles available-", geo)) %>%
    select(GEOID, year, variable, TotalHH = B08141_001E, plus3Vehicles = B08141_005E) %>%
    mutate(Pct3Vehicles = 100*(plus3Vehicles/TotalHH)) %>%
    select(CensusTractID = GEOID, year, Pct3Vehicles, variable)
  Vehicles <- bind_rows(Vehicles, acs_vehicles)
}

saveRDS(Vehicles, paste0(masterPath, "Census_Vehicles",".rds"))



##Year Structure Builts B25034
BuildingAge1<- tibble()
for (loop_year in years) {
  acs_buildingage1 <- get_acs(geography = geo, table = "B25035", state = State, county = County,
                          year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year, variable = paste("Median Building Age", geo)) %>%
    select(CensusTractID = GEOID, year, variable, MedianAgeBuilt = B25035_001E)
  BuildingAge1 <- bind_rows(BuildingAge1, acs_buildingage1)
}

saveRDS(BuildingAge1, paste0(masterPath, "Census_MedianBuildingAge",".rds"))



##Occupations B24011
Occupations<- tibble()
for (loop_year in years) {
  acs_Occupations <- get_acs(geography = geo, table = "B08124", state = State, county = County,
                              year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year, variable = paste("Percent Blue Collar", geo)) %>%
    select(CensusTractID = GEOID, year, variable, Total = B08124_001E, Management = B08124_002E, Sales = B08124_004E) %>% mutate(BlueCollar = Total - Management - Sales, PctBlueCollar = 100*(BlueCollar/Total)) %>% select(CensusTractID, year, PctBlueCollar, variable)
  Occupations <- bind_rows(Occupations, acs_Occupations)
}

saveRDS(Occupations, paste0(masterPath, "Census_PctBlueCollar",".rds"))

##Public Transit Access
require(foreign)
require(tigris)
# The input file geodatabase
path = "C://School//Research//PublicTransitAccessibility//TransitAccess.dbf"
TransitAccessSP_raw <- read.dbf(path)

AllDavidsonTracts <- as.data.frame(tracts("TN", "Davidson")) %>% select(CensusTractID = GEOID)

JoinedAccessSP <- merge(x = AllDavidsonTracts, y = TransitAccessSP_raw, by.x = "CensusTractID", by.y =  "Join_ID2", all.x = TRUE) %>% select(CensusTractID, Transit_PctCoverage = PERCENTAGE) %>% mutate(variable = "Percent Tract Covered by .25 Transit Stop Buffer")

JoinedAccessSP[is.na(JoinedAccessSP)] <- 0

saveRDS(JoinedAccessSP, paste0(masterPath, "Census_TransitCoverage",".rds"))



##Park Coverage 2010
ParkPath10 = "C://School//Research//Predictive Variables//Parks//ParkCoverage.dbf"
ParkCoverage_raw10 <- read.dbf(ParkPath10)
ParkPath16 = "C://School//Research//Predictive Variables//Parks//ParkCoverage2016.dbf"
ParkCoverage_raw16 <- read.dbf(ParkPath16)


JoinedParks <- merge(x = AllDavidsonTracts, y = ParkCoverage_raw10, by.x = "CensusTractID", by.y =  "GEOID", all.x = TRUE) %>% select(CensusTractID, Park_PctCoverage10 = PERCENTAGE) 

JoinedParks10and16 <- merge(x = JoinedParks, ParkCoverage_raw16, by.x = "CensusTractID", by.y =  "GEOID", all.x = TRUE) %>% select(CensusTractID, Park_PctCoverage10, Park_PctCoverage16 = PERCENTAGE) 

JoinedParks10and16[is.na(JoinedParks10and16)] <- 0

saveRDS(JoinedParks10and16, paste0(masterPath, "ParkCoverage",".rds"))

##Age Distribution - under 18 2000 - 2016

all2010 <- read.csv("Age_Sex//ACS_10_5YR_S0101_with_ann.csv", skip = 1) %>% select(CensusTractID = Id2, Over18 = Total..Estimate..SELECTED.AGE.CATEGORIES...18.years.and.over, Over65 = Total..Estimate..SELECTED.AGE.CATEGORIES...65.years.and.over ) %>% mutate(year = 2010, Under18 = 100 - as.numeric(as.character(Over18)))

all2011 <- read.csv("Age_Sex//ACS_11_5YR_S0101_with_ann.csv", skip = 1) %>% mutate(year = 2011) %>% select(CensusTractID = Id2, Over18 = Total..Estimate..SELECTED.AGE.CATEGORIES...18.years.and.over, Over65 = Total..Estimate..SELECTED.AGE.CATEGORIES...65.years.and.over ) %>% mutate(year = 2011, Under18 = 100 - as.numeric(as.character(Over18)))

all2012 <- read.csv("Age_Sex//ACS_12_5YR_S0101_with_ann.csv", skip = 1) %>% mutate(year = 2012)%>% select(CensusTractID = Id2, Over18 = Total..Estimate..SELECTED.AGE.CATEGORIES...18.years.and.over, Over65 = Total..Estimate..SELECTED.AGE.CATEGORIES...65.years.and.over ) %>% mutate(year = 2012, Under18 = 100 - as.numeric(as.character(Over18)))

all2013 <- read.csv("Age_Sex//ACS_13_5YR_S0101_with_ann.csv", skip = 1) %>% mutate(year = 2013) %>% select(CensusTractID = Id2, Over18 = Total..Estimate..SELECTED.AGE.CATEGORIES...18.years.and.over, Over65 = Total..Estimate..SELECTED.AGE.CATEGORIES...65.years.and.over ) %>% mutate(year = 2013, Under18 = 100 - as.numeric(as.character(Over18)))

all2014 <- read.csv("Age_Sex//ACS_14_5YR_S0101_with_ann.csv", skip = 1) %>% mutate(year = 2014) %>% select(CensusTractID = Id2, Over18 = Total..Estimate..SELECTED.AGE.CATEGORIES...18.years.and.over, Over65 = Total..Estimate..SELECTED.AGE.CATEGORIES...65.years.and.over ) %>% mutate(year = 2014, Under18 = 100 - as.numeric(as.character(Over18)))

all2015 <- read.csv("Age_Sex//ACS_15_5YR_S0101_with_ann.csv", skip = 1) %>% mutate(year = 2015) %>% select(CensusTractID = Id2, Over18 = Total..Estimate..SELECTED.AGE.CATEGORIES...18.years.and.over, Over65 = Total..Estimate..SELECTED.AGE.CATEGORIES...65.years.and.over ) %>% mutate(year = 2015, Under18 = 100 - as.numeric(as.character(Over18)))

all2016 <- read.csv("Age_Sex//ACS_16_5YR_S0101_with_ann.csv", skip = 1) %>% mutate(year = as.integer(2016)) %>% select(CensusTractID = Id2, Over18 = Total..Estimate..SELECTED.AGE.CATEGORIES...18.years.and.over, Over65 = Total..Estimate..SELECTED.AGE.CATEGORIES...65.years.and.over ) %>% mutate(year = 2016, Under18 = 100 - as.numeric(as.character(Over18)))

Under18 <- rbind(all2010, all2011, all2012, all2013, all2014, all2015, all2016) %>% mutate(Over65x = as.numeric(as.character(Over65)), CensusTractID = as.character(CensusTractID)) %>% select(-Over18, -Over65, - Over65x)

Over65 <- rbind(all2010, all2011, all2012, all2013, all2014, all2015, all2016) %>% mutate(Over65x = as.numeric(as.character(Over65)), CensusTractID = as.character(CensusTractID)) %>% select(-Over18, -Over65, - Under18)

saveRDS(Under18, paste0(masterPath, "Census_Under18", ".rds"))
saveRDS(Over65, paste0(masterPath, "Census_Over65", ".rds"))
library(readxl)
library(dplyr)
library(lubridate)
library(sf)
library(tidyverse)
library(mapboxapi)
library(censusxy)
library(tidycensus)
library(ggplot2)
library(gridExtra)
library(scales)
library(reshape2)
library(htmlTable)
library(stringr)
library(stringi)


SPSStartTimes <- read_excel("SPSStartTimes.xlsx") %>%
  mutate(Type = case_when(
    Type == 'K-8' ~ 'K-8',
    TRUE ~ paste(Type,"School")),
    CurrentStartTime = as.POSIXct("2022-04-26 00:00:00", tz="GMT") + hours(hour(CurrentStartTime)) + minutes(minute(CurrentStartTime))
    )

OSPI_SPS_Demographics_21_22 <- read_excel("OSPI SPS Demographics 21-22.xlsx")
FRPL <- read_excel("FRPL.xlsx")
Title1<- read_excel("FRPL.xlsx", sheet = "Title1")

SPSStartTimes <- left_join(SPSStartTimes,OSPI_SPS_Demographics_21_22,by=c("Name2"="SchoolName")) %>%
  left_join(.,FRPL, by=c("Name" ="School Name")) %>%
  left_join(.,Title1, by=c("Name" ="School"))

names(SPSStartTimes) <- gsub(" ",".",names(SPSStartTimes))

SPSStartTimes <- SPSStartTimes %>% mutate(Title1 = ifelse(is.na(Title.I.School.Wide)==TRUE,'No',Title.I.School.Wide),
         Type = ordered(Type,levels=c("Elementary School","K-8","Middle School","High School")),
         ProposedStart = case_when(
           Start == "7:40 a.m." ~ as.POSIXct("2022-04-26 07:40:00", tz="GMT"),
           Start == '8:30 a.m.' ~ as.POSIXct("2022-04-26 08:30:00", tz="GMT"),
           TRUE~as.POSIXct("2022-04-26 09:30:00", tz="GMT")
         )) %>%
  mutate(TimeShift = as.numeric(difftime(ProposedStart,CurrentStartTime,units="mins")))

SchoolCounts <- SPSStartTimes %>% group_by(Type,ProposedStart) %>% summarize(nschool=n())

SPSStartTimes$ADR_LTG = NULL
SPSStartTimes$ADR_LTT = NULL
SPSStartTimes$GEOID = NULL
for(i in 1:nrow(SPSStartTimes)){
  temp <- mb_geocode(SPSStartTimes$MailingAddress[i])
  SPSStartTimes$ADR_LGT[i] = temp[1]
  SPSStartTimes$ADR_LTT[i] = temp[2]
  
  temp2 <- cxy_geography(lon = temp[1], lat = temp[2])
  SPSStartTimes$GEOID[i] = substr(temp2$X2020.Census.Blocks.GEOID,1,11)
}


my_acs_2020 = get_acs(geography = "block group", 
                      variables = c(median_hh_income = "B19013_001",
                                    #foreign_born = "B05002_013",
                                    #white_alone = "B02001_002",
                                    #inc_pov_ratio = "B05010_001",
                                    #Travel_time_to_work = "B08303_001",
                                    
                                    Go2Work_total="B08302_001",
                                    Go2Work_0500="B08302_002",
                                    Go2Work_0530="B08302_003",
                                    Go2Work_0600="B08302_004",
                                    Go2Work_0630="B08302_005",
                                    Go2Work_0700="B08302_006",
                                    Go2Work_0730="B08302_007",
                                    Go2Work_0800="B08302_008",
                                    Go2Work_0830="B08302_009",
                                    Go2Work_0900="B08302_010",
                                    Go2Work_1000="B08302_011",
                                    Go2Work_1100="B08302_012",
                                    Go2Work_1200="B08302_013",
                                    Go2Work_1600="B08302_014",
                                    Go2Work_2400="B08302_015"), 
                      state = c("WA"), 
                      county=c("King"),
                      year = 2020,
                      geometry = TRUE)  %>% st_transform(4326)


Elementary  = st_read("SPS_AttendanceAreasAndSchools_Shapefiles_2021_2022/ES/sps_attendance_area_ES_2021_2022.shp") %>% st_transform(4326)
Middle  = st_read("SPS_AttendanceAreasAndSchools_Shapefiles_2021_2022/MS/sps_attendance_area_MS_2021_2022.shp") %>% st_transform(4326)
High  = st_read("SPS_AttendanceAreasAndSchools_Shapefiles_2021_2022/HS/sps_attendance_area_HS_2021_2022.shp") %>% st_transform(4326)
Sites  = st_read("SPS_AttendanceAreasAndSchools_Shapefiles_2021_2022/sites/Sites_2021.shp") %>% st_transform(4326)

sf::sf_use_s2(FALSE)
Elm_inter<- st_intersection(Elementary,my_acs_2020)
Middle_inter<- st_intersection(Middle,my_acs_2020)
High_inter<- st_intersection(High,my_acs_2020)


Elm_summaries <- Elm_inter %>% group_by(ES_ZONE,variable) %>% 
  summarize(estimate.mean= mean(estimate,na.rm=T),estimate.sum = sum(estimate,na.rm=T)) %>%
  mutate(estimate = case_when(
    variable == "median_hh_income" ~ estimate.mean,
    TRUE~estimate.sum
  )) %>%
  select(SchoolName=ES_ZONE,variable,estimate)

Mid_summaries <- Middle_inter %>% group_by(MS_ZONE,variable) %>% 
  summarize(estimate.mean= mean(estimate,na.rm=T),estimate.sum = sum(estimate,na.rm=T)) %>%
  mutate(estimate = case_when(
    variable == "median_hh_income" ~ estimate.mean,
    TRUE~estimate.sum
  )) %>%
  select(SchoolName=MS_ZONE,variable,estimate)


Hi_summaries <- High_inter %>% group_by(HS_ZONE,variable) %>% 
  summarize(estimate.mean= mean(estimate,na.rm=T),estimate.sum = sum(estimate,na.rm=T)) %>%
  mutate(estimate = case_when(
    variable == "median_hh_income" ~ estimate.mean,
    TRUE~estimate.sum
  )) %>%
  select(SchoolName=HS_ZONE,variable,estimate)

CensusData_Non_Option_Schools = rbind(Elm_summaries,Mid_summaries,Hi_summaries) %>%
  mutate(SchoolName2 = case_when(
    SchoolName =='Adams'~ 'Adams Elementary School',
    SchoolName =='Aki Kurose'~ 'Aki Kurose Middle School',
    SchoolName =='Alki'~ 'Alki Elementary School',
    SchoolName =='Arbor Heights'~ 'Arbor Heights Elementary School',
    SchoolName =='Ballard'~ 'Ballard High School',
    SchoolName =="Beacon Hill Int'l" ~ 'Beacon Hill International School',
    SchoolName =='B.F. Day'~ 'B F Day Elementary School',
    SchoolName =='Broadview-Thomson K-8'~ 'Broadview-Thomson K-8 School',
    SchoolName =='Bryant'~ 'Bryant Elementary School',
    SchoolName =='Catharine Blaine K-8'~ 'Catharine Blaine K-8 School',
    SchoolName =="Chief Sealth Int'l"~ 'Chief Sealth International High School',
    SchoolName =='Coe'~ 'Frantz Coe Elementary School',
    SchoolName =="Concord Int'l"~ 'Concord International School',
    SchoolName =='Bagley'~ 'Daniel Bagley Elementary School',
    SchoolName =="Dearborn Park Int'l"~ 'Dearborn Park International School',
    SchoolName =="Denny Int'l"~ 'David T. Denny International Middle School',
    SchoolName =='Dunlap'~ 'Dunlap Elementary School',
    SchoolName =='Robert Eagle Staff'~ 'Robert Eagle Staff Middle School',
    SchoolName =='Eckstein'~ 'Eckstein Middle School',
    SchoolName =='Emerson'~ 'Emerson Elementary School',
    SchoolName =='Fairmount Park'~ 'Fairmount Park Elementary School',
    SchoolName =='Franklin'~ 'Franklin High School',
    SchoolName =='Garfield'~ 'Garfield High School',
    SchoolName =='Gatewood'~ 'Gatewood Elementary School',
    SchoolName =='Gatzert'~ 'Bailey Gatzert Elementary School',
    SchoolName =='Genesee Hill'~ 'Genesee Hill Elementary',
    SchoolName =='Graham Hill'~ 'Graham Hill Elementary School',
    SchoolName =='Green Lake'~ 'Green Lake Elementary School',
    SchoolName =='Greenwood'~ 'Greenwood Elementary School',
    SchoolName =='Nathan Hale'~ 'Nathan Hale High School',
    SchoolName =="Hamilton Int'l"~ 'Hamilton International Middle School',
    SchoolName =='Hawthorne'~ 'Hawthorne Elementary School',
    SchoolName =='Highland Park'~ 'Highland Park Elementary School',
    SchoolName =="Ingraham Int'l"~ 'Ingraham High School',
    SchoolName =='Jane Addams'~ 'Jane Addams Middle School',
    SchoolName =='Hay'~ 'John Hay Elementary School',
    SchoolName =='John Muir'~ 'John Muir Elementary School',
    SchoolName =='John Rogers'~ 'John Rogers Elementary School',
    SchoolName =='Kimball'~ 'Kimball Elementary School',
    SchoolName =='Lafayette'~ 'Lafayette Elementary School',
    SchoolName =='Laurelhurst'~ 'Laurelhurst Elementary School',
    SchoolName =='Lawton'~ 'Lawton Elementary School',
    SchoolName =='Leschi'~ 'Leschi Elementary School',
    SchoolName =='Lincoln'~ 'Lincoln High School',
    SchoolName =='Lowell'~ 'Lowell Elementary School',
    SchoolName =='Loyal Heights'~ 'Loyal Heights Elementary School',
    SchoolName =='Madison'~ 'Madison Middle School',
    SchoolName =='Madrona'~ 'Madrona K-5 School',
    SchoolName =='Magnolia'~ 'Magnolia Elementary School',
    SchoolName =='Maple'~ 'Maple Elementary School',
    SchoolName =='MLK Jr.'~ 'Martin Luther King Jr. Elementary School',
    SchoolName =='McClure'~ 'McClure Middle School',
    SchoolName =='McGilvra'~ 'McGilvra Elementary School',
    SchoolName =='Meany'~ 'Edmonds S. Meany Middle School',
    SchoolName =="Mercer Int'l"~ 'Mercer International Middle School',
    SchoolName =='Montlake'~ 'Montlake Elementary School',
    SchoolName =='North Beach'~ 'North Beach Elementary School',
    SchoolName =='Northgate'~ 'Northgate Elementary School',
    SchoolName =='Olympic Hills'~ 'Olympic Hills Elementary School',
    SchoolName =='Olympic View'~ 'Olympic View Elementary School',
    SchoolName =='Rainier Beach'~ 'Rainier Beach High School',
    SchoolName =='Rainier View'~ 'Rainier View Elementary School',
    SchoolName =='Rising Star'~ 'Rising Star Elementary School',
    SchoolName =='Roosevelt'~ 'Roosevelt High School',
    SchoolName =='Roxhill'~ 'Roxhill Elementary School',
    SchoolName =='Sacajawea'~ 'Sacajawea Elementary School',
    SchoolName =='Sand Point'~ 'Sand Point Elementary',
    SchoolName =='Sanislo'~ 'Sanislo Elementary School',
    SchoolName =='Stevens'~ 'Stevens Elementary School',
    SchoolName =='Thurgood Marshall'~ 'Thurgood Marshall Elementary',
    SchoolName =='View Ridge'~ 'View Ridge Elementary School',
    SchoolName =='Viewlands'~ 'Viewlands Elementary School',
    SchoolName =='Washington'~ 'Washington Middle School',
    SchoolName =='Wedgwood'~ 'Wedgwood Elementary School',
    SchoolName =='West Seattle Elem'~ 'West Seattle Elementary School',
    SchoolName =='West Seattle HS'~ 'West Seattle High School',
    SchoolName =='West Woodland'~ 'West Woodland Elementary School',
    SchoolName =='Whitman'~ 'Whitman Middle School',
    SchoolName =='Whittier'~ 'Whittier Elementary School',
    SchoolName =='Wing Luke'~ 'Wing Luke Elementary School'
  )
  ) %>% data.frame()

OptionsSchools  <- SPSStartTimes %>% filter(!(Name2 %in% CensusData_Non_Option_Schools$SchoolName2)) %>%
  select(Name2,ADR_LGT,ADR_LTT)
projcrs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

OptionSchools_list <- list()

for(i in 1:nrow(OptionsSchools)){
  temp = st_as_sf(x = OptionsSchools[i,],                         
                  coords = c("ADR_LGT", "ADR_LTT"),
                  crs = projcrs)
  temp2 <- mb_isochrone(temp,distance =3219)
  
  temp3 <- st_intersection(temp2,my_acs_2020)
  
  OptionSchools_list[[i]]  <- temp3 %>% group_by(variable) %>% 
    summarize(estimate.mean= mean(estimate,na.rm=T),estimate.sum = sum(estimate,na.rm=T)) %>%
    mutate(
      SchoolName =OptionsSchools$Name2[i],
      estimate = case_when(
        variable == "median_hh_income" ~ estimate.mean,
        TRUE~estimate.sum
      ))
}


OptionSchools_all <- do.call('rbind',OptionSchools_list) %>% data.frame() %>% select(SchoolName2 = "SchoolName","variable","estimate")

CensusData_all <- rbind(CensusData_Non_Option_Schools[,c("SchoolName2","variable","estimate")],
                        OptionSchools_all)

MedianHHIncome = CensusData_all %>% filter(variable == "median_hh_income") %>% select(SchoolName2,median_hhi = estimate)
G2W_total = CensusData_all %>% filter(variable == "Go2Work_total") %>% select(SchoolName2,Total = estimate)

G2W = CensusData_all %>% filter(variable != "median_hh_income" & variable != "Go2Work_total") %>% left_join(.,G2W_total) %>% mutate(Percent = estimate/Total) %>%
  mutate(LeaveTime = case_when(
    variable == "Go2Work_0500" ~ hm("05:00"),
    variable == "Go2Work_0530" ~ hm("05:30"),
    variable == "Go2Work_0600" ~ hm("06:00"),
    variable == "Go2Work_0630" ~ hm("06:30"),
    variable == "Go2Work_0700" ~ hm("07:00"),
    variable == "Go2Work_0730" ~ hm("07:30"),
    variable == "Go2Work_0800" ~ hm("08:00"),
    variable == "Go2Work_0830" ~ hm("08:30"),
    variable == "Go2Work_0900" ~ hm("09:00"),
    variable == "Go2Work_1000" ~ hm("10:00"),
    variable == "Go2Work_1100" ~ hm("11:00"),
    variable == "Go2Work_1200" ~ hm("12:00"),
    variable == "Go2Work_1600" ~ hm("16:00"),
    variable == "Go2Work_2400" ~ hm("24:00")
  ))  %>% arrange(SchoolName2,LeaveTime) %>% group_by(SchoolName2) %>%
  mutate(CumPer = cumsum(Percent),
         LeaveDT = as.POSIXct("2022-04-26 00:00:00", tz="GMT") + LeaveTime)

MyData <- left_join(G2W,SPSStartTimes,by=c("SchoolName2"="Name2")) %>% filter(is.na(Start)==FALSE) %>% left_join(.,MedianHHIncome) %>%
  mutate(
  HHI_group = case_when(
    median_hhi <= 50000 ~ '0-50K',
    median_hhi >50000 & median_hhi <= 100000 ~'50-100K',
    median_hhi >100000 & median_hhi <= 150000 ~'100-150K',
    median_hhi > 150000 ~ '>150K'
  )) %>% ungroup() %>%
  mutate(HHI_group = ordered(HHI_group,levels=c('0-50K','50-100K','100-150K','>150K')),
         NeedToLeave = ProposedStart - minutes(30))

SummaryByType = MyData %>% group_by(Type,LeaveDT) %>% summarize(mycount=sum(estimate),mytotal=sum(Total)) %>% 
  mutate(Percent=mycount/mytotal) %>% arrange(Type,LeaveDT) %>% group_by(Type) %>%
  mutate(CumPer = cumsum(Percent)) %>%
  mutate(CumPer2 = paste0(round(100*CumPer),'%')) 

SummaryByTypeTable <- SummaryByType %>% filter(hour(LeaveDT) %in% c(7,8,9) & minute(LeaveDT)==0) %>%
  dcast(.,Type ~ LeaveDT,value.var='CumPer2') 
names(SummaryByTypeTable) <- c("","7:00","8:00","9:00")

Summary1Table <- htmlTable(SummaryByTypeTable,
          cgroup = c("","Depart for Work Before"),
          n.cgroup=c(1,3),
          rnames=FALSE,
          align='lccc')

SummaryByTypeStart = MyData %>% group_by(Type,ProposedStart,LeaveDT) %>% summarize(mycount=sum(estimate),mytotal=sum(Total)) %>% 
  mutate(Percent=mycount/mytotal) %>% arrange(Type,ProposedStart,LeaveDT) %>% group_by(Type,ProposedStart) %>%
  mutate(CumPer = cumsum(Percent)) %>%
  mutate(CumPer2 = paste0(round(100*CumPer),'%')) 

SummaryByTypeTableStart <- SummaryByTypeStart %>% filter(hour(LeaveDT) %in% c(7,8,9) & minute(LeaveDT)==0) %>%
  dcast(.,Type+ProposedStart ~ LeaveDT,value.var='CumPer2') %>%
  left_join(.,SchoolCounts, by=c("Type"="Type","ProposedStart"="ProposedStart")) %>%
  mutate(ProposedStart = format(ProposedStart,"%l:%M"))

names(SummaryByTypeTableStart)  <- c("","Proposed Start Time","7:00","8:00","9:00","# of<br>Schools")

css.cell.Ref<- matrix("",nrow(SummaryByTypeTableStart[,2:6]), ncol(SummaryByTypeTableStart[,2:6]))
css.cell.Ref[1,2] <- "background-color:rgba(240, 228, 66, 0.5)"
css.cell.Ref[2,3] <- "background-color:rgba(240, 228, 66, 0.5)"
css.cell.Ref[3,4] <- "background-color:rgba(240, 228, 66, 0.5)"
css.cell.Ref[4,4] <- "background-color:rgba(240, 228, 66, 0.5)"
css.cell.Ref[5,3] <- "background-color:rgba(240, 228, 66, 0.5)"
css.cell.Ref[6,4] <- "background-color:rgba(240, 228, 66, 0.5)"
css.cell.Ref[7,3] <- "background-color:rgba(240, 228, 66, 0.5)"

Summary2Table <- htmlTable(SummaryByTypeTableStart[,2:6],
          cgroup = c("","Depart for Work Before",""),
          n.cgroup=c(1,3,1),
         tspanner = c("Elementary School","K-8","Middle School","High School"),
          n.tspanner = c(3,1,2,1),
          rnames=FALSE,
          align=c('cccc'),
         spacer.celltype="double_cell",
          css.cell = css.cell.Ref)

Equity <- MyData %>% filter(ProposedStart - minutes(30) == LeaveDT | ProposedStart - minutes(40) == LeaveDT) %>%
  mutate(HighPoverty = `Preliminary.High.Poverty.Eligibility.for.SY.2020-21`,
         FRPL = `3.Year.Average.for.2020-21.Eligibility`)

AllSchools <- sort(unique(MyData$Name))

save(MyData,AllSchools,Summary1Table,Summary2Table,Equity,file="SPSApp/ForApp.RData")
write.csv2(MyData,file="SchoolsAndCensus.csv")
write.csv2(Equity,file="JustSchools.csv")


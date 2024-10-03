library(dplyr)
library(lubridate)
library(readxl)
#library(ggplot2)

wqp_data<-read.csv('wqp_data.csv')
#lakes data

lake_sites<-wqp_data %>%
  select(SITE_CODE,SITE_NAME,LAT,LON)%>%
  distinct() %>%
  arrange(SITE_NAME) 



saveRDS(lake_sites,'outputs/lake_sites.RDS')


lakes_wq_dat<-wqp_data %>%
  select(SITE_CODE,DateTime=date_time,parameter,value,unit,depth=depth_m,dup,mdl,pql,qualifier) %>%
  mutate(unit=trimws(unit),
         qualifier=trimws(qualifier),
         nonDetectFlag=grepl('U',qualifier)|grepl('<',value),
         value=as.numeric(gsub('<','',value)),
         mdl=ifelse(mdl==0,max(mdl[parameter==parameter]),mdl),
         pql=ifelse(pql<mdl,mdl,pql),
         newResultValue=ifelse(nonDetectFlag,mdl,value),
         newResultValue=ifelse(parameter=='Turbidity'&newResultValue<=0,0.01,newResultValue),
         Year=year(DateTime),
         Month=month(DateTime),
         WaterYear=ifelse(Month>=10,Year+1,Year),
         FakeDate=as.Date(paste(2000,Month,day(DateTime),sep='-')),
         WY_FakeDate=as.Date(if_else(Month>=10,FakeDate-years(1),FakeDate)))

lakes_wq_dat<-lakes_wq_dat %>% 
  bind_rows(.,
    lakes_wq_dat %>%
      group_by(SITE_CODE,DateTime,depth,dup,Year,Month,WaterYear,FakeDate,WY_FakeDate) %>%
      summarise(value=newResultValue[parameter=='Total Nitrogen']/newResultValue[parameter=='Total Phosphorus'],
                newResultValue=value,
                qualifier=paste(unique(c(qualifier[parameter=='Total Nitrogen'],
                                         qualifier[parameter=='Total Phosphorus'])),
                  collapse=' '),
                nonDetectFlag=any(nonDetectFlag[parameter=='Total Nitrogen'],
                                    nonDetectFlag[parameter=='Total Phosphorus']),
                parameter='N:P Ratio'),
    lakes_wq_dat %>%
      group_by(SITE_CODE,DateTime,depth,dup,Year,Month,WaterYear,FakeDate,WY_FakeDate) %>%
      summarise(value=newResultValue[parameter=='Chlorophyll a']/newResultValue[parameter=='Pheophytin a'],
                newResultValue=value,
                qualifier=paste(unique(c(qualifier[parameter=='Chlorophyll a'],
                                         qualifier[parameter=='Pheophytin a'])),
                                collapse=' '),
                nonDetectFlag=any(nonDetectFlag[parameter=='Chlorophyll a'],
                                    nonDetectFlag[parameter=='Pheophytin a']),
                parameter='Chl-a:Pheo-a Ratio')
  ) %>%
  mutate(qualifier=trimws(gsub('NA','',qualifier)))

saveRDS(lakes_wq_dat,'outputs/lakes_wq_dat.RDS')

sites_list<-setNames(lake_sites$SITE_CODE,paste0(lake_sites$SITE_NAME,' (',lake_sites$SITE_CODE,')'))
parm_list<-unique(lakes_wq_dat$parameter)
years_list<-sort(unique(lakes_wq_dat$Year),T)

saveRDS(sites_list,'outputs/sites_list.RDS')
saveRDS(parm_list,'outputs/parm_list.RDS')
saveRDS(years_list,'outputs/years_list.RDS')

unique(lakes_wq_dat$depth) #all 0 or NA
unique(lakes_wq_dat$dup) #there are dups
unique(lakes_wq_dat$qualifier)
#NA    "J"   "UJ"  ""    "J,"  ",J"  ",UJ"
#EST = estimated J
#JG = estimated high
#JG = estimated low
#FE = ????
#FH = ????
#U = nonDetect
#K = ????
#FD = ?????
#EQP = ????
#FA = ????

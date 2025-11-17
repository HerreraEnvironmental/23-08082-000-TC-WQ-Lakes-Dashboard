library(dplyr)
library(ggplot2)
library(readxl)

OLD_HEC_DATA<-read_xlsx('inputs/Herrera Lakes All.xlsx',
                        col_types = c('text','text','numeric','numeric','date','numeric','text','text','text','text',
                                      'numeric','numeric','text','text','text','numeric','numeric')) %>%
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
         WY_FakeDate=as.Date(if_else(Month>=10,FakeDate-years(1),FakeDate))) %>%
  mutate(parameter=ifelse(parameter=='Water transparency','Secchi Depth',parameter))

OLD_HEC_DATA<-OLD_HEC_DATA %>% 
  bind_rows(.,
    OLD_HEC_DATA %>%
      group_by(SITE_CODE,DateTime,depth,dup,Year,Month,WaterYear,FakeDate,WY_FakeDate) %>%
      summarise(value=newResultValue[parameter=='Total Persulfate Nitrogen']/newResultValue[parameter=='Total Phosphorus'],
                newResultValue=value,
                qualifier=paste(unique(c(qualifier[parameter=='Total Persulfate Nitrogen'],
                                         qualifier[parameter=='Total Phosphorus'])),
                  collapse=' '),
                nonDetectFlag=any(nonDetectFlag[parameter=='Total Persulfate Nitrogen'],
                                    nonDetectFlag[parameter=='Total Phosphorus']),
                parameter='N:P Ratio'),
    OLD_HEC_DATA %>%
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

#saveRDS(OLD_HEC_DATA,'outputs/OLD_HEC_DATA.RDS')

wqp_data<-read_parquet('outputs/lakes_wq_dat.parquet')


glimpse(wqp_data)
glimpse(OLD_HEC_DATA)


merged_data<-wqp_data |>
  mutate(Date=as_date(DateTime),
  dup=if_else(grepl('Replicate|Duplicate',dup),1,0)) |>
  full_join(
    OLD_HEC_DATA|>mutate(Date=as_date(DateTime)),
    by=c('SITE_CODE','DateTime','Date','parameter','depth','dup','Year','Month','WaterYear','FakeDate','WY_FakeDate'),
    suffix = c('.WQP','.OLD')
  )
  
merged_data %>%
  filter(is.na(value.WQP))

diff_value<-merged_data |>
  filter(value.WQP!=value.OLD) |>
  select(-(Year:WY_FakeDate)) |>
  select(SITE_CODE,Date,parameter,depth,value.WQP,value.OLD) |>
  mutate(diff=value.WQP-value.OLD,
  relDiff=diff/value.WQP)|>
  filter(relDiff>0.0001)

merged_data %>% 
  inner_join(diff_value |> select(SITE_CODE,Date,depth,parameter,relDiff)) |>
  select(SITE_CODE,parameter,dup,Date,depth,value.WQP,value.OLD,relDiff) |>
  arrange(SITE_CODE,Date,parameter,depth) 




OLD_HEC_DATA %>%
  group_by(SITE_CODE,parameter,Year) %>%
  summarise(n=n(),
            Measured=n>0) %>%
  group_by(SITE_CODE,parameter) %>%
  filter(sum(Measured)>1&
           !(grepl('toxin|cystin|spermopsin',parameter))) %>%
  ggplot(aes(x=Year,y=parameter,col=Measured))+
  geom_point()+
  facet_wrap(~SITE_CODE)+
  theme_bw()+
  scale_x_continuous(breaks=seq(1970,2025,5),minor_breaks = 1970:2025)+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))


 OLD_HEC_DATA %>%
   filter(parameter=="Chlorophyll a") %>%
   summary()

wqp_data %>%
  mutate(Year=year(ActivityStartDate))%>%
  rename(parameter=CharacteristicName,
         unit=ResultMeasure.MeasureUnitCode) %>%
  mutate(parameter=dplyr::case_when(
    parameter %in%c('Total Phosphorus, mixed forms','Phosphorus') ~ 'Total Phosphorus',
    parameter=='Dissolved oxygen (DO)'&unit=='%' ~ 'Dissolved Oxygen (saturation)',
    parameter=='Dissolved oxygen (DO)' ~ 'Dissolved Oxygen',
    parameter=='Escherichia coli' ~ 'E. coli',
    parameter=="Temperature, water" ~ "Water Temperature (°C)",
    parameter=='Chlorophyll' ~ 'Chlorophyll a',
    parameter %in% c("Total Nitrogen, mixed forms","Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)") ~ 'Total Nitrogen',
    parameter %in%c("Secchi, Horizontal Distance","Light attenuation at measurement depth")~'Secchi Depth',
    T ~ parameter
  )) %>%
  group_by(Site=gsub('THURSTONCOUNTY-EH-','',MonitoringLocationIdentifier),parameter,Year) %>%
  summarise(n=n(),
            Measured=n>0) %>%
  group_by(Site,parameter) %>%
  filter(sum(Measured)>1&Year>=2000&
           !(grepl('toxin|cystin|spermopsin',parameter))) %>%
  ggplot(aes(x=Year,y=parameter,col=Measured))+
  geom_point()+
  facet_wrap(~Site)+
  theme_bw()+
  scale_x_continuous(breaks=seq(1970,2025,5),minor_breaks = 1970:2025)+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

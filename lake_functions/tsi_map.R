#map and summary plot of all tsi data

tsi_map<-function(lakes_list,annual_tsi,selectYear,plotParm=c('Chlorophyll a','Total Phosphorus','Secchi Depth')){
  plotParm<-plotParm[1]
  
  pal<-colorFactor(c('green','yellow','blue','grey'),levels=c('Eutrophic',"Mesotrophic",'Oligotrophic',NA))
  ## add for each score
  
  selectIndex<-  annual_tsi %>%
    filter(parameter==plotParm) %>%
    mutate(parameter=factor(parameter,levels=c('Chlorophyll a','Total Phosphorus','Secchi Depth')))
  
  annual_tsi %>%
    tidyr::pivot_wider(id_cols = c(SITE_CODE),names_from=parameter,values_from = TSI,names_expand=T) %>%
    left_join(selectIndex) %>%
    left_join(lakes_list) %>%
    mutate(Category=ifelse(TSI>=50,'Eutrophic',ifelse(TSI>=40,"Mesotrophic",'Oligotrophic'))) %>%
    leaflet() %>%
    addCircleMarkers(fillColor = ~pal(Category),fillOpacity = 0.9,weight=1,
                     color='black',
                     popup=~paste0("<h5>", "<b>", SITE_NAME,'<br>', "</b>","</h5>",
                                   "<hr>",
                                   "For",selectYear,  ", the TSI score was ", "<b>",round(TSI,0),' for ',plotParm,
                                   "</b>",", and is considered ", "<b>", Category, "</b>","."#,"<br>",
                                   # 'Chlorophyll a TSI: ',`Chlorophyll a`,'<br>',
                                   # 'Total Phosphorus TSI: ', `Total Phosphorus`,'<br>',
                                   # 'Secchi Depth TSI:',`Secchi Depth`
                                   
                     ),
                     layerId= ~SITE_CODE,
                     label = ~SITE_CODE) %>%
    addProviderTiles('Esri.NatGeoWorldMap') %>%
     addLegend(pal=pal,values=factor(c('Eutrophic',"Mesotrophic",'Oligotrophic'),levels=c('Eutrophic',"Mesotrophic",'Oligotrophic')),
               title=paste('Trophic State Index for',plotParm))
}

# tsi_map(lakes_list,
#         annual_tsi = WQX_PULL(project='Ambient_Water_Quality_Lakes',startYear=2019,endYear =2019,
#                               waterYear = F) %>%
#           wqx_cleanup() %>%
#           group_by(SITE_CODE) %>%
#           tidyr::nest() %>%
#           mutate(TSI_out=map(.x=data,.f=~tsi_calc(.x))) %>%
#           select(-data) %>%
#           tidyr::unnest(TSI_out),
#           selectYear = 2019,
#         plotParm = 'Total Phosphorus')

tsi_summary_plot<-function(annual_tsi){
  cols <- c("Eutrophic" = "green", "Mesotrophic" = "yellow", "Oligotrophic" = "blue")
  tsi_summary_plot<-annual_tsi %>%
    mutate(Category=factor(ifelse(TSI>=60,'Eutrophic',ifelse(TSI>=40,"Mesotrophic",'Oligotrophic')),
                           levels=c('Eutrophic',"Mesotrophic",'Oligotrophic'))) %>%
    ggplot(aes(x=Category))+
    geom_bar(stat="count", aes(fill = Category))+
    scale_x_discrete(drop=F)+
    ylab("Number of stations")+
    scale_fill_manual(values = cols,drop=F)+
    theme_bw() + 
    facet_wrap(~parameter,ncol=1)
  ggplotly(tsi_summary_plot)
}

#tsi_summary_plot(annual_tsi)

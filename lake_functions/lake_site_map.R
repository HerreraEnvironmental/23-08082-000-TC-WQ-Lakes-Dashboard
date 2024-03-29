#Map of all monitoring sites in leaflet

site_map<-function(site_list){
  
leaflet(site_list) %>%
  addMarkers(popup=~paste0("<h5>", "<b>", SITE_NAME,'<br>', "</b>","</h5>",
                           "<h6>", "<b>", "Click to do the following:", "</b>","<ul>","<br>",
                           "<li>", "<a onclick=","openTab('tsi')>",'View Trophic State Index','</a>', "<br>","</li>",
                           "<li>", "<a onclick=","openTab('all_data')>",'View Water Quality trends and all data for this station','</a>', "<br>","</li>",
                           "<li>", "<a onclick=","openTab('data_download')>",'Download all data for this station', '</a>', "<br>","</li>",
                           "</ul>","</h6>"),
             layerId= ~SITE_CODE,
             label = ~SITE_NAME) %>%
  #addPolygons(data = ThurstonCo_WA,
  #            fillColor = "")%>%
  addProviderTiles('Esri.NatGeoWorldMap')
}

# lakes_list<-wqx_siteInfo(project='Ambient_Water_Quality_Lakes') %>%
#   transmute(
#     SITE_CODE=MonitoringLocationIdentifier,
#     SITE_NAME=MonitoringLocationName,
#     LAT=as.numeric(LatitudeMeasure),
#     LON=as.numeric(LongitudeMeasure)
#   )
# 
# site_map(lakes_list)

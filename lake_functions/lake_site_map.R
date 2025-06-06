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
    leaflet.esri::addEsriFeatureLayer('https://map.co.thurston.wa.us/arcgis/rest/services/Thurston/Thurston_Watersheds/FeatureServer/0',
                                      useServiceSymbology = TRUE,
                                      fillColor='lightblue',
                                      stroke=1,
                                      color='black',
                                      popupProperty = JS(paste0(
                                        "function(feature) {",
                                        " return L.Util.template(",
                                        " \"<b>Watershed: {Watershed}</b>",
                                        " <p>This watershed drains to {Drainage}</p>",
                                        " \",",
                                        " feature.properties",
                                        " );",
                                        "}"
                                      ))) %>%
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

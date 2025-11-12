#deploy_App

source('install_deps.R')
source('sourcing_scripts/data_prep.R')

rsconnect::deployApp(appFiles = c('ui.R',
                                  'server.R',
                                  'lake_functions/',
                                  'outputs/',
                                  'www/'
),
appId=9474652,
account= 'herrerainc',
server= 'shinyapps.io',
appName= 'ThurstonCounty_Lakes_Dashboard_Dev',
appTitle= 'ThurstonCounty_Lakes_Dashboard_Dev')

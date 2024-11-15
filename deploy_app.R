#deploy_App

source('package_check_script.R')
source('data_prep.R')

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

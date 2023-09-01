# 23-08082-000-TC-WQ-Dashboard
Thurston County Dashboard

This dashboard presents water quality monitoring data for Thurston County lakes

https://herrerainc.shinyapps.io/ThurstonCounty_Lakes_Dashboard_Dev/

## Input files
* "Herrera Lakes All.xlsx"
  * This file has two uses:
      1.  it is used to generate a table of SITE_CODE, SITE_NAME, LAT, and LON, which is used for mapping
      2.  it is used as the source of water quality data for plotting and calculating TSI 
  * The following column structure is needed
    * date_time - MM/DD/YY HH:MM in local time (assumed to be text value)
    * SITE_CODE - text value designating the site - note that the SITE_CODE names on WQX appear to be slightly different, often haveing a prefix
    * SITE_NAME - long-form site name
    * LAT - site location latitude numeric
    * LON - site location longitude, numeric
    * parameter - parameter, character - Dashboard was developed for the following distinct parameter names:
      * "Specific Conductivity (at 25 deg C)", "Temperature, water", "pH", "Total Phosphorus", "Light attenuation at measurement depth", "Surface Water Depth", "Dissolved Oxygen", "Nitrate-Nitrite as N", 
"Chlorophyll a", "Pheophytin a", "Total Persulfate Nitrogen", "Water transparency"
      * deviation from these parameter names may result in errors in the calculation of TSI 
    * value - numeric result for specific parameter
    * unit - text value for result units (e.g, mg/L)
    * depth_m - numeric value of sampling depth, typically NA for streams
    * dup - TRUE/FALSE (1/0) value for whether sample is a field duplicate
    * mdl - numeric value of laboratory method detection limit
    * pql - numeric value of laboratory practical quantitation limit (or reporting limit)
    * qualifier - text value for laboratory qualifiers - "U" flags are used to designate non-detects (i.e., <MDL)
 ## How to Update
1. Append or replace "Herrera Lakes All.xlsx" with updated data. **We can change the name for ease of use**
2. Run "deploy_app.R"

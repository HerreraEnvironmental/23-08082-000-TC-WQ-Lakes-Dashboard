---
editor_options: 
  markdown: 
    wrap: 72
---

# Thurston County Lakes Water Quality Dashboard

<img src="www/Herrera_wordmark_vertical_4c.png" width="100"/>

![StaticBadge](https://img.shields.io/badge/water_quality_index-blue)
![StaticBadge](https://img.shields.io/badge/water_quality_criteria-green)
![StaticBadge](https://img.shields.io/badge/water_quality_trends-purple)

------------------------------------------------------------------------

# Project 23-08082-000

## Description

This dashboard presents water quality monitoring data for Thurston
County Lakes.

Access the dashboard here: [Thurston County Lakes
Dashboard](https://herrerainc.shinyapps.io/ThurstonCounty_Lakes_Dashboard_Dev/)

## 📦 Requirements and Dependencies

This Shiny application is hosted on shinyapps.io (see link above).
Should you need to run the app locally, please see the deps.yaml file in
this directory for a complete list of dependencies. The install_deps.R
script will automatically install the necessary packages.

You will need to have R and a compatible IDE (e.g., RStudio or Positron)
installed on your computer.

## :droplet: Data and Analysis

The original, unmodified data used in this project is located in the
inputs/ subdirectory, which is backed up both on this GitHub and in
Herrera's private
[SharePoint](https://herrerainc.sharepoint.com/teams/m-24-08458-000/Internal/Forms/AllItems.aspx?id=%2Fteams%2Fm%2D24%2D08458%2D000%2FInternal%2FProject%2DFiles%2FDashboard%5Fraw%5Fdata&viewid=5a5b1715%2D8cc2%2D40c8%2Da461%2D5a736f61a7ff).

If you do not have access to the data, please contact the contributors
listed at the bottom of the repository.

### :arrows_counterclockwise: Layout of directory, data, and analysis

The ui.R and server.R scripts contain the Shiny application code. There
is a global.R script which contains all library calls.

**General flow of data acquisition + tidying:**

The inputs/ folder contains raw data. The sourcing_scripts/ folder
contains the scripts that will pull in external data from WQP, tidy it,
and produce the data that is used in the app. Within the
sourcing_scripts/ folder:

-   WQP_data_query.R sources data from the remote [Water Quality
    Portal](https://www.waterqualitydata.us.) and tidies the sourced
    data to match with internal formatting. This tidied dataframe is
    saved as wqp_data.parquet and is used to produce the various
    dashboard tables.

-   The data_prep.R script produces multiple RDS and parquet files that
    are used within the dashboard.

Internal, raw data adheres to the following structure:

| Column      | Type / Format                    | Notes / Examples                                                                                                                                                                                                                                                                                                                                                                                 |
|---------|---------|------------------------------------------------------|
| `date_time` | Text in `MM/DD/YY HH:MM` (local) | Example: `09/15/25 13:45`.                                                                                                                                                                                                                                                                                                                                                                       |
| `SITE_CODE` | Text                             | Site code; note WQX site codes may differ slightly (often have a prefix).                                                                                                                                                                                                                                                                                                                        |
| `SITE_NAME` | Text                             | Long-form site name.                                                                                                                                                                                                                                                                                                                                                                             |
| `LAT`       | Numeric                          | Site latitude.                                                                                                                                                                                                                                                                                                                                                                                   |
| `LON`       | Numeric                          | Site longitude.                                                                                                                                                                                                                                                                                                                                                                                  |
| `parameter` | Text                             | Parameter name. Dashboard expects **exactly** one of:<br>"Specific Conductivity (at 25 deg C)", "Temperature, water", "pH", "Total Phosphorus", "Light attenuation at measurement depth", "Surface Water Depth", "Dissolved Oxygen", "Nitrate-Nitrite as N", "Chlorophyll a", "Pheophytin a", "Total Persulfate Nitrogen", "Water transparency".<br>Deviations may cause TSI calculation errors. |
| `value`     | Numeric                          | Result for the specified parameter.                                                                                                                                                                                                                                                                                                                                                              |
| `unit`      | Text                             | Result units (e.g., `mg/L`).                                                                                                                                                                                                                                                                                                                                                                     |
| `depth_m`   | Numeric                          | Sampling depth in meters; typically `NA` for streams.                                                                                                                                                                                                                                                                                                                                            |
| `dup`       | Boolean (`TRUE/FALSE` or `1/0`)  | Indicates field duplicate.                                                                                                                                                                                                                                                                                                                                                                       |
| `mdl`       | Numeric                          | Method Detection Limit.                                                                                                                                                                                                                                                                                                                                                                          |
| `pql`       | Numeric                          | Practical Quantitation (reporting) limit.                                                                                                                                                                                                                                                                                                                                                        |
| `qualifier` | Text                             | Lab qualifier codes; `"U"` designates non-detects (i.e., `< MDL`).                                                                                                                                                                                                                                                                                                                               |

### :signal_strength: How to Update the Shiny Dashboard Data

The app data can be updated by running the sourcing scripts in the
sourcing_scripts/ folder, or simply by running deploy_app.R.

### :exclamation: Helpful Notes

WQP (Water Quality Portal) = NWIS (National Water Information System) +
WQX (WaterQuality Exchange)

------------------------------------------------------------------------

## 🔧 Pull Requests {#pull-requests}

Pull requests are welcome. Do not merge to the main branch without an
approved code review. For major changes, please open an issue first.

## 💬 Contributors + Contact Information

| Role            | Name                                                                         | GitHub                                   |     |
|------------------|-------------------|------------------|------------------|
| Primary Contact | [Tim Clark](https://www.herrerainc.com/team-member/tim-clark/)               | [GitHub](https://github.com/clarkbar88)  |     |
| Contributor     | [Regina Lionheart](https://www.herrerainc.com/team-member/regina-lionheart/) | [GitHub](https://github.com/R-Lionheart) |     |
| Contributor     | [Nikki VandePutte](https://www.herrerainc.com/team-member/nikki-vandeputte/) | [GitHub](https://github.com/nvandeputte) |     |
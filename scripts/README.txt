***************************************************************************************************
README for A Line in the Sand

Baseline folder: 
--- Under Empire/Data and code/papers_jeppe/trade/

Scripts:
--- Under Empire/Data and code/papers_jeppe/trade/scripts/

Data:
--- Under Empire/Data and code/papers_jeppe/trade/data/

Paper
--- Under Empire/Data and code/papers_jeppe/trade/tex/
***************************************************************************************************
=========================================================
99_preamble.R (from Under Empire/Data and code/dofiles)
=========================================================
Always run this at the beginning of each session.
Here, I:
--- set the working directory,
--- define some functions that are needed later
--- load custom fonts for ggplot
--- define some ggplot themes for a unified visual style


***************************************************************************************************
CREATE RAW DATA
***************************************************************************************************
=========================================================
1 create_cities.R
=========================================================
Creates the raw city data for the subsequent data stages

Input:
--- Under Empire/Data and code/2_data in stages/9_anatem.dta

Output:
--- data/df_full/cities_raw.Rdata (full dataset)
--- data/df_balanced/cities_raw.Rdata (balanced dataset)
--- data/df_pure/cities_raw.Rdata (non-interpolated dataset)

***************************************************************************************************
GEODATA
***************************************************************************************************
=========================================================
2 geo_script.R
=========================================================
Here I measure a range of geographic features at the city level
(e.g. access to the atlantic ocean, access to roman roads, ruggedness of terrain etc.)

Input:
--- data/df_full/cities_raw.Rdata
--- 5_centennia/eurasia_map_union
--- Zabel (2014)/rainfed/overall_suitability_1/overall_suitability_1.bil
--- Zabel (2014)/irrigated/overall_suitability_1/overall_suitability_1.bil
--- Zabel (2014)/rainfed_irrigated/overall_cropsuit_i_1961-1990/overall_cropsuit_i_1961-1990.bil
--- NE/ne_10m_geography_marine_polys
--- NE/ne_10m_rivers_lake_centerlines
--- McCormick et al 2013a/dataverse_files : roman_roads_v2008
--- trade routes/traderoutes : Trade Routes 1100, Trade Routes 1300, Trade Routes 1500, Trade Routes 1700
--- Shaver et al 2019/shaver_eurasia/ruggedness.tif

Output: 
--- geo_data/agricultural_suitability.Rdata
--- geo_data/grid_data.Rdata.Rdata
--- geo_data/coast_data.Rdata
--- geo_data/river_data.Rdata
--- geo_data/roman_data.Rdata
--- geo_data/muslimtrade_data.Rdata
--- geo_data/ruggedness_data.Rdata


=========================================================
3 gather_geo.R
=========================================================
Here, I gather all of the different geographical variables created and exported above
I export one file with all of these measures:

Input:
--- geo_data/agricultural_suitability.Rdata
--- geo_data/grid_data.Rdata.Rdata
--- geo_data/coast_data.Rdata
--- geo_data/river_data.Rdata
--- geo_data/roman_data.Rdata
--- geo_data/muslimtrade_data.Rdata
--- geo_data/ruggedness_data.Rdata

Output:
--- geo_data/0_geodata_final.Rdata

***************************************************************************************************
BORDER DATA
***************************************************************************************************

=========================================================
4 dist_all.R
=========================================================
Here, I calculate distance from cities to borders (incl. maritime borders)

Output:
--- data/border_data/border_distAll.Rdata

=========================================================
5 dist_polities.R
=========================================================
Here, I calculate distance from cities to polity borders/inland borders

Output:
--- data/border_data/border_distPolity.Rdata

=========================================================
6 gather_border.R
=========================================================
Here, I combine the two border datasets created above into one file

Output:
--- data/border_data/0_borderdata_final.Rdata


***************************************************************************************************
COMPLETE DATA
***************************************************************************************************
=========================================================
7 make_df_full.R
=========================================================
Calculates Urban Potential
Adds state level data on urban population + number of cities
Merges with the geographic data + the border distance data

Input:
--- data/df_full/cities_raw.Rdata

Output:
--- data/df_full/cities.Rdata

*** NB: the scripts '8 make_df_balanced.R' + '9 make_df_pure.R' contain the same code as 'make_df_full.R' ***
*** they only use different subsets of the data ***
*** they also spit out different versions of the 


***************************************************************************************************
CODE FOR PAPER
***************************************************************************************************
=========================================================
10 paper_script.R
=========================================================
Code for the tables/figures in 'A Line in the Sand'
Also contains code for creating the semicentennial data structure used


=========================================================
11 appendix_script.R
=========================================================
Code for tables/figures in the appendix for 'A Line in the Sand'
Also contains code for creating
--- the semicentennial data structure (balanced dataset)
--- the centennial data structure (non-interpolated dataset)
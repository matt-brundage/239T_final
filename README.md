# 239T_final
Final project for 239T



**Short Description**

a. Goals

- Visualize data on refugee placements in the U.S. by city, state, year, and refugee nationality (e.g., Iran, Syria, etc.)

- Develop a function that takes user input (e.g., year(s) of interest, refugee placements as an absolute figure or as a percentage of the city population, etc.) and produces (1) a corresponding city “bubble” map and (2) a table of the top datapoints

- Data are collected from the Refugee Processing Center (RPC) operated by the U.S Department of State Bureau of Population, Refugees, and Migration. You can explore the raw data here: http://ireports.wrapsnet.org/

b. Overview of steps

- First, data_manipulation.R takes RPC data, cleans it, and merges it with matching city population and location (lat/long) pulled from the U.S. Census. Data are exported to refugee_data_clean.csv.

- Second, data_mapping.R creates three core functions: (1) function for creating any subset of interest; (2) function for mapping the data; (3) function for printing a table of the data. These allow the user to simply input any variables of interest (e.g., year = 2002, nationality = Iran, etc.) and have the functions output the corresponding map and table.

- Third, refugee_visualization.Rmd creates a slidy presentation. The presentation includes (1) a discussion of the nature of the data in more detail, and (2) several example maps and tables from different user inputs.




**Dependencies**

R, version 3.5.3




**Files**

a. Code

- data_manipulation.R: described above
- data_mapping.R: described above
- refugee_visualization.Rmd: described above

b. Data

- nst-est2018-alldata.csv: 2010-2018 population of U.S. states (U.S. Census)
- st-est00int-01.csv: 2000-2010 population of U.S. states (U.S. Census)
- PEP_2017_PEPANNRES_with_ann.csv: 2010-2017 population of U.S. "places" (as defined by U.S. Census)
- uscitiesv1.5.csv: latitude and longitude of U.S. "places" (as defined by U.S. Census)

- city_nationality_2002_2003.csv: 2002-2003 refugee placement data from RPC
- city_nationality_2004_2005.csv: 2004-2005 refugee placement data from RPC
- city_nationality_2006_2007.csv: 2006-2007 refugee placement data from RPC
- city_nationality_2008_2009.csv: 2008-2009 refugee placement data from RPC
- city_nationality_2010_2011.csv: 2010-2011 refugee placement data from RPC
- city_nationality_2012_2013.csv: 2012-2013 refugee placement data from RPC
- city_nationality_2014_2015.csv: 2014-2015 refugee placement data from RPC
- city_nationality_2016_2017.csv: 2016-2017 refugee placement data from RPC
- city_nationality_2018_2019.csv: 2018-2019 refugee placement data from RPC

c. Results

- refugee_visualization.html: slidy presentation with several examples of maps and tables

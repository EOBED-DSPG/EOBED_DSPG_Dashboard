# EOBED_DSPG_Dashboard
Repository for DSPG summer 2020.


Download R 4.0.2 or later using CRAN distribution.
ex. http://lib.stat.cmu.edu/R/CRAN/

Install R Studio

Clone repository from github by 
1 Opening R studio
2 File tab
3 New Project
4 Version Control 
5 Repository URL: https://github.com/agnizab1776/OSU_DSPG_Team_Delta_2020.git

Install packages by typing these commands into the console (bottom left)
> install.packages('shinydashboard')
> install.packages('shiny')
> install.packages('ggplot2')
> install.packages('dplyr')
> install.packages('conflicted')
> install.packages('tidyverse')
> install.packages('plotly')
> install.packages('shinythemes')
> install.packages('tigris')
> install.packages('sf')
> install.packages('treemap')
> install.packages('readxml')
> install.packages('rsconnect')
> install.packages('leaflet')
> install.packages('plyr')

To run dashboard locally, open file (domain area)Dashboard.R
i.e. generalDashboard.R, housingDashboard.R, or qolDashboard.R (files are located bottom right)

Then click Run App (near the top middle with a green arrow)

**NOTE**
If the console prints an error about not being able to open shape file
1. Session tab
2. Clear workspace
3. Close R studio and do not save workspace
4. Reload R studio and run project again
********

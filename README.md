## MEDINFO 2015 Tutorial on Visualizing Operational Informatics Data Using R

**Authors:** Leslie McIntosh, PhD; Kanstantsyia (Connie) Zabarouskaya, MBA; 

**ATTRIBUTIONS**

We would like to thank developers of the following products for the opportunity to use their great tools: 
- RStudio team - Shiny and shinydashboard packages
- Ramnath Vaidyanathan - rCharts and Slidify package 
- Highsoft - Highcharts noncommercial license as part of rCharts (https://github.com/ramnathv/rCharts/blob/master/inst/libraries/highcharts/LICENSE.MD)
- Simon Muller - BulletGraphSipemu.txt
- Hadley Wickham - plyr package

All licenses are included inside the Licenses folder.

This repository contains R scripts, exercise solutions, demo dashboard files and PDF version of the slides used in the tutorial at MEDINFO 2015 in São Paulo, Brazil on Thursday, August 20, 2015. All the files will be uploaded by 8/20/2015. If you have any questions please contact one of the presenters (Kanstantsyia(at)pathology.wustl.edu).

The R scripts and demo dashboard can be run inside RStudio, however you must have all the necessary R packages installed. To run the demo dashboard, download the dashboard folder with all the included files, and run any of the R files inside RStudio.

### Tutorial Prerequisites
While getting ready for the tutorial is optional, it will allow you to follow the instructors during the tutorial and perform the hands-on exercises. Come a little early if you have issues with installing the tools, so we have time to help you. The installation steps below will save you time during the tutorial if you have any problems or issues with the installation please let us know.
- Download and Install R: http://cran.wustl.edu/. It may be best to have the latest version of R (or at least 3.0.3), for all libraries to work properly
- Download and install R-Studio from this web link: https://www.rstudio.com/ide/
- Install shiny and shinydashboard packages from within R (or R-Studio)
- Install rCharts from github using the devtools package (see more info here: http://ramnathv.github.io/rCharts/)


You can use the following code:

```
install.packages('shiny')
install.packages('shinydashboard')
install.packages('devtools')
require(devtools)
install_github('rCharts', 'ramnathv')
```




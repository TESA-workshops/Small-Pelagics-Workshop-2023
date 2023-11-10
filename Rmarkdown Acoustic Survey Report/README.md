# Rmarkdown Reporting Code
This subdirectory is used to provide example code of reporting from Acoustic Surveys for Small Pelagics. 

You can view an example of [2022 Acoustic Surveys Report HERE](https://htmlpreview.github.io/?https://github.com/TESA-workshops/Small-Pelagics-Workshop-2023/blob/main/Rmarkdown%20Acoustic%20Survey%20Report/AcousticSurveyReport_FY2022_230302.html)

The integretations occured in Echoview and the relies on [EchoviewR](https://github.com/AustralianAntarcticDivision/EchoviewR#readme) to provide some automation. While this code can be run without integretations for those who are interested and have Echoview, please follow the README below to get [EchoviewR](https://github.com/AustralianAntarcticDivision/EchoviewR#readme) to work with the most recent versions of R and Echoview.

This code is open for community development and we encourage users to extend the code as they need. We are not liable for any losses or misinterptation of statistics derived from this code. 

If using EchoviewR, please cite as:

```{r citation}
citation('EchoviewR')
```

If installing EchoviewR please try the following steps first:

### FIRST Install latest version of Rtools for your R version. 

### SECOND DOWNLOAD THIS FOLDER in advance of Day 2.


### Installing EchoviewR (optional)
You can install all these packages with the following  ```R``` code:

```{r dependPacks,eval=FALSE}
install.packages(c('sp','geosphere','lubridate', 'devtools'))
install.packages("https://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-8.tar.gz", repo=NULL, type="source")
devtools::install_github("omegahat/RDCOMClient")
devtools::install_github("AustralianAntarcticDivision/EchoviewR", build_opts = c("--no-resave-data", "--no-manual"))
```

You should then be ready to work with EchoviewR:
```{r startEVR, eval=FALSE}
library(EchoviewR)
```

### Downloading RAW data. (Optional)
If you are interested in the integration and RAW data from the example please log into the DFO network. Map the following network drive.

\\AcousticsSABS\Small Pelagics Workshop Data\

Contact Allan Debertin @dfo email for Username and password.

Put the data and place in the local folder under "Purse Seine/Hydro Data"


### Installing Dependencies for Building the RMarkdown Report Coding.

```{r dependPacks2,eval=FALSE}
install.packages(c('ggplot2','grid','grDevices', 'gridExtra', 'taRifx', 'MASS', 'sp',
'RODBC', 'sqldf', 'ggpubr', 'xlsx', 'dplyr', 'data.table', 'ggthemes', 'plotly', 'scales', 'lubridate', 'rmarkdown'))
```

### Preview of Product

When going through the coding steps in the next folder. 

We should produce a report for [German Bank and Seal Island HERE](https://htmlpreview.github.io/?https://github.com/TESA-workshops/Small-Pelagics-Workshop-2023/blob/main/Rmarkdown%20Acoustic%20Survey%20Report/Purse%20Seine/AcousticSurveyReport_GermanBank.html)


### References

Hadley Wickham and Winston Chang (2017). devtools: Tools to Make Developing R Packages Easier. R package version 1.34.4.
  https://CRAN.R-project.org/package=devtools


- a free interface between Echoview (R) and R using COM scripting.

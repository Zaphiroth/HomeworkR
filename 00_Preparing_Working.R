# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Homework of old aunt
# Purpose:      May you be happy
# programmer:   Zhe Liu
# Date:         21-01-2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


options(java.parameters = "-Xmx2048m",
        stringsAsFactors = FALSE)

##---- loading the required packages ----
suppressPackageStartupMessages({
  require(openxlsx)
  require(readxl)
  require(plyr)
  require(stringi)
  require(feather)
  require(RODBC)
  require(MASS)
  require(car)
  require(data.table)
  require(tidyverse)
  require(Rwordseg)
  require(tmcn)
  require(wordcloud2)
})

##---- setup the directories ----
# system("mkdir 01_Background 02_Inputs 03_Outputs 04_Codes 05_Internal_Review 06_Deliveries")

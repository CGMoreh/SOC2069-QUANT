#############################################################################################################################################################
#####  Data preparation for SOC2069 workshops and assignments ###############################################################################################
#####  Prepared by: Dr. Chris Moreh                           ###############################################################################################
#####  Date: October 2024                                     ###############################################################################################
#############################################################################################################################################################


## Install and load packages ################################################################################################################################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, ggrepel, easystats, sjlabelled, sjmisc, archive, fs, DT
)

### ESS10 data preparation ##################################################################################################################################

## Select variables for each assignment question

ess_vars <- ess10 |> 



## Download Version 3.2 of Integrated file of Round 10 (2020-22) of the European Social Survey: 
## https://ess.sikt.no/en/datafile/f37d014a-6958-42d4-b03b-17c29e481d3d/263?tab=0

## Name of the downloaded raw .zip folder and the SPSS dataset name
zipfile <- "ESS10.zip"
datafile <- "ESS10.sav"

## Find the path to the raw file on the PC
zipfile_path <- fs::dir_ls(glob = paste0("*", zipfile), 
                           recurse = TRUE, 
                           fail = FALSE)

## Check contents of the .zip file
archive::archive(zipfile_path)                                         

## Create connection link with the data file in the .zip
filelink <- archive::archive_read(zipfile_path,                        
                                  file = datafile)

## Import the SPSS file to R
ess10 <- sjlabelled::read_spss(filelink)

## Checks
dim(ess10)  # [1] 37611   618



## Recode country
country_names <- get_labels(ess10$cntry, drop.unused = TRUE)
country_codes <- levels(ess10$cntry)

ess10 <- ess10 |> 
        mutate(country = labels_to_levels(cntry),
               country_iso2 = as.character(cntry),
               country_iso3 = countrycode::countrycode(country_iso2, origin = "iso2c", destination = "iso3c")
               )

## Select out a limited number of variables
ess10_small <- ess10 |> 
  select(
    ### Are religious people more satisfied with life?
      stflife,
      rlgblg, rlgdgr, rlgatnd, pray,
    
    ### Are older people more likely to see the death penalty as justifiable?
      # N/A ## No death penalty variable
    
    ### What factors are associated with opinions about future European Union enlargement among Europeans?
      euftf, 
    
    ### Is higher internet use associated with stronger anti-immigrant sentiments?
      imsmetn, imdfetn, impcntr, imueclt, imwbcnt,
      netusoft, netustm,
    
    ### How does victimisation relate to trust in the police?
      trstplc,
      crmvct,  
    
    ### What factors are associated with belief in life after death?
      # N/A ## No afterlife variables
    
    ### Are government/public sector employees more inclined to perceive higher levels of corruption than those working in the private sector?
      # N/A ## No corruption in Europe
    
    ### Various covariates
      lrscale, health, inprdsc, atchctr, atcherp, dscrgrp, wrclmch, gndr, agea, maritalb, chldhhe, domicil, 
      eduyrs, tporgwk, hinctnta, hincfel, 
    
    ### Administrative
    country, country_iso2, country_iso3 # country to be created
    ) |> 
  relocate(starts_with("country"))


country_codes <- get_labels(ess10_small$country_iso3, drop.unused = TRUE)
country_names <- get_labels(ess10_small$country,      drop.unused = TRUE)

## Save the dataset
ess10_small |> 
  sjlabelled::write_spss("Data/assignment_data/ESS10/ess10.sav")

## Break down and export dataset by country
for (c in country_codes) {
  ess10_small |> 
    filter(country_iso3 == c) |> 
    sjlabelled::write_spss(paste0("Data/assignment_data/ESS10/ess10_", c, ".sav"))
}

## Make HTML code for the download links on the website
for (i in 1:length(country_codes)) {
  cat(
    paste0('<li><a class="dropdown-item" href="/Data/assignment_data/ESS10/ess10_', country_codes[i], '.sav">', country_names[i], '</a></li> \n')
  )
}


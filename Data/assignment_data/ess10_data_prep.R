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

ess_vars <- c(
          ### Are religious people more satisfied with life?
          ### Are older people more likely to see the death penalty as justifiable?
          ### What factors are associated with opinions about future European Union enlargement among Europeans?
          ### Is higher internet use associated with stronger anti-immigrant sentiments?
          ### How does victimisation relate to trust in the police?
          ### What factors are associated with belief in life after death?
          ### Are government/public sector employees more inclined to perceive higher levels of corruption than those working in the private sector?
          ### Various covariates
          ### Administrative
          "country", "country_iso2", "country_iso3" # country to be created

)



## Download the : 
## 

## Name of the downloaded raw .zip folder and the SPSS dataset name
zipfile <- ""
datafile <- ""

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
dim(ess10)  # [1] 









## Recode country
country_names <- get_labels(wvs7$B_COUNTRY,       drop.unused = TRUE)
country_codes <- get_labels(wvs7$B_COUNTRY_ALPHA, drop.unused = TRUE)

wvs7 <- wvs7 |> 
        mutate(country = labels_to_levels(B_COUNTRY),
               country = case_match(country, 
                                    "Great Britain" ~ "United Kingdom",
                                    "Northern Ireland" ~ "United Kingdom",
                                    .default = country),
               country_iso3 = case_match(B_COUNTRY_ALPHA, 
                                    "GBR" ~ "GBR",
                                    "NIR" ~ "GBR",
                                    .default = B_COUNTRY_ALPHA),
               country_iso2 = countrycode::countrycode(country_iso3, origin = "iso3c", destination = "iso2c")
               ) 

## Select out a limited number of variables
wvs7_small <- wvs7 |> 
  select(all_of(wvs_vars)) |> 
  relocate(starts_with("country"))

country_codes <- get_labels(wvs7_small$country_iso3, drop.unused = TRUE)
country_names <- get_labels(wvs7_small$country,      drop.unused = TRUE)

## Save the dataset
wvs7_small |> 
  sjlabelled::write_spss("Data/assignment_data/WVS7/wvs7.sav")

## Break down and export dataset by country
for (c in country_codes) {
  wvs7_small |> 
    filter(country_iso3 == c) |> 
    sjlabelled::write_spss(paste0("Data/assignment_data/WVS7/wvs7_", c, ".sav"))
}

## Make HTML code for the download links on the website
for (i in 1:length(country_codes)) {
  cat(
    paste0('<li><a class="dropdown-item" href="/Data/assignment_data/WVS7/wvs7_', country_codes[i], '.sav">', country_names[i], '</a></li> \n')
  )
}


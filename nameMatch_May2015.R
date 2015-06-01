
library(ggplot2)
library(stringr)
library(lubridate)
library("SciencesPo") # for psum()
library(stringdist)
library(plyr)
library(dplyr)

# library("devtools")
# devtools::install_github("hadley/readxl")
library(readxl)

after <- ymd("2015MAR1")
dir = 'May2015/'  # folder, relative to this project, with spreadsheets

# "Base de donnees des contacts_RATOMA_ 24 MAI 2015.xls"
# "Base_Contacts_parprefecture_version travail 19 mai_2015  (1).xls"
# "BASE SUIVI CONTACTS DIXINN 25 05 2015_jp.xlxs" # removed first 128 rows (early 2014) because of bad date formats
# "Base contact Dubreka au 29 mai 2015 pour envoi niveau    national (1).xlsx"

contact.file.name = "BASE SUIVI CONTACTS DIXINN 25 05 2015"
# contact.file.name = "Base de donnees des contacts_RATOMA_ 24 MAI 2015"

contact.sheet.name = "Liste_de_contacts"
contact.sheet.type = "xlsx"

case.file.name = "cases_may20"

# CASE DATA ####

# file.csv = paste0(dir, case.file.name , ".csv")
file.xls = paste0(dir, case.file.name , ".xls")
sheet = case.file.name  # export file from VHF has same sheet name and file name

cases = read_excel(file.xls, sheet)
str(cases)

# reduce
cases = cases[1:61] %>%
  filter( EpiCaseDef %in% 1:3 ) %>%
  filter(DateOnset >= after) %>%
  mutate(Age = ifelse(AgeUnit %in% "Mois", floor(Age/12), Age)
  ) 


# if surname is empty, split OtherName, or enter 'XXXXXX'
noSurname = which(cases$Surname %in% '')
if (length( noSurname ) > 0) { 
  name = strsplit(cases[ noSurname, 'OtherNames'], ' ')
  newSurname = rapply(name, function(x) ifelse(length(x) ==0, 'xxx', head( x, 1) ) ) 
  cases[noSurname, 'Surname'] = newSurname
}

# if ohtername is empty, split OtherName, or enter 'XXXXXX'
noOtherNames = which(cases$OtherNames %in% '')
if (length( noOtherNames ) > 0) { 
  name = strsplit(cases[ noOtherNames, 'Surname'], ' ')
  newOtherNames = rapply(name, function(x) ifelse(length(x) ==0, 'xxx', head( x, 1) ) ) 
  cases[noOtherNames, 'OtherNames'] = newOtherNames
}

# encoding check
save(cases, file = paste0(dir, case.file.name , ".rda"))

cases %>% count(Surname)
cases %>% count(OtherNames)
cases %>% count(toupper(DistrictRes))
cases %>% count(toupper(ParishRes))
cases %>% count(toupper(VillageRes))
cases %>% count(AgeUnit)
cases %>% count(Age)
cases %>% count(DateOnset) %>% arrange(desc(DateOnset))

# CONTACT DATA  ####

file.xls = paste0(dir, contact.file.name, ".", contact.sheet.type)

contacts = read_excel(file.xls, contact.sheet.name)
str(contacts)

# check data format
if (!(is.POSIXct(contacts$`Date du dernier contact avec cas source`))) {
  contacts$`Date du dernier contact avec cas source` = 
    mdy(contacts$`Date du dernier contact avec cas source`[2:220])
}

contacts = contacts[,1:21] %>% 
  rename( ID =  `N ° du contact`,
          Surname = Nom,
          OtherNames = Prenom,
          Nom.du.cas.source = `Nom du cas source`,
          Prenom.du.cas.source = `Prenom du cas source`,
          DateLastContact = `Date du dernier contact avec cas source`,
          Gender = Sexe,
          District = Prefecture,
          subCounty = `Sous-prefecture/Commune`,
          Village = `Village/quartier`,
          link = `Lien avec cas source`,
          case_age = `Age du cas source`,
          case_District = `Prefecture du cas source`,
          case_subCounty = `Sous-prefecture/commune de cas source`,
          case_Village = `Village/quartier du cas source`
          )  %>%
  filter( DateLastContact >= after ) %>%
  mutate(
    SourceCase = paste( Prenom.du.cas.source, Nom.du.cas.source)
  ) 
str(contacts)

# if surname is empty, split OtherName, or enter 'XXXXXX'
noSurname = which(contacts$Surname %in% '')
if (length(noSurname) > 0){
  name = strsplit(contacts[ noSurname, 'OtherNames'], ' ')
  newSurname = rapply(name, function(x) ifelse(length(x) ==0, 'xxx', head( x, 1) ) ) 
  contacts[noSurname, 'Surname'] = newSurname
}

# save
save(contacts, file = paste0(dir, contact.file.name, ".rda"))

contacts %>% count(Surname)
contacts %>% count(District)
contacts %>% count(subCounty)
contacts %>% count(Village)
contacts %>% count(case_District)
contacts %>% count(case_District)
contacts %>% count(case_District)
contacts %>% count(DateLastContact) %>% arrange(desc(DateLastContact))

# NAME MATCH ####

# clean memory 
rm(list = ls()); gc()

case.file.name = "cases_may20"
contact.file.name = "Base de donnees des contacts_RATOMA_ 24 MAI 2015"
dir = 'May2015/' 

load(paste0(dir, case.file.name, ".rda"))
load(paste0(dir, contact.file.name, ".rda"))

# setup matrix of values to test
# m = matrix( nrow = nrow(cases) , ncol = nrow(contacts), 
#             dimnames = list(cases$ID, contacts$ID))
# dim(m)
# 
# # index all of the combinations between data_a and data_b
# index = as.data.frame(which(is.na(m), arr.ind =  TRUE))
# a_index = index[, 'row']
# b_index = index[, 'col']
# 
# # get vectors of names
# a_first_name = tolower( cases[a_index, "OtherNames"] )
# a_last_name = tolower( cases[a_index, "Surname"] )
# b_last_name = tolower( contacts[b_index, "Surname"] )
# b_first_name = tolower( contacts[b_index, "OtherNames"] )

a_first_name = tolower( cases$OtherNames )
a_last_name = tolower( cases$Surname)
b_last_name = tolower( contacts$Surname )
b_first_name = tolower( contacts$OtherNames)

## first name match ####
  # vector-base matches
  first_name_match_dist = stringdistmatrix( a_first_name, b_first_name) 
  
  # first_name_match_jw = stringdistmatrix( a_first_name, b_first_name, method = 'jw') 
  
  first_name_match_soundex =  # stridist-soundex is 0 when perfect match, 1 for no match
    stringdistmatrix( a_first_name, b_first_name, method = 'soundex') 

## last name match  ####
  # vector-base matches
  last_name_match_dist = stringdistmatrix( a_last_name, b_last_name) 
  
  # first_name_match_jw = stringdistmatrix( a_first_name, b_first_name, method = 'jw') 
  
  last_name_match_soundex = # stridist-soundex is 0 when perfect match, 1 for no match
    stringdistmatrix( a_last_name, b_last_name, method = 'soundex')

## first-last name match  ####
  # vector-base matches
  first_name_match_distx = stringdistmatrix( a_first_name, b_last_name) 
  
  # first_name_match_jw = stringdistmatrix( a_first_name, b_first_name, method = 'jw') 
  
  first_name_match_soundexx =  # stridist-soundex is 0 when perfect match, 1 for no match
    stringdistmatrix( a_first_name, b_last_name, method = 'soundex')
  

## last-first name match  ####
  # vector-base matches
  last_name_match_distx = stringdistmatrix( a_last_name, b_first_name) 
  
  # first_name_match_jw = stringdistmatrix( a_first_name, b_first_name, method = 'jw') 
  
  last_name_match_soundexx =  # stridist-soundex is 0 when perfect match, 1 for no match
    stringdistmatrix( a_last_name, b_first_name, method = 'soundex')

## combine results #### 
  
# combine with original, and take best fit
#   fn_dist =  pmin( first_name_match_dist, first_name_match_distx , na.rm=TRUE)
#   # fn_dist0 =  pmax( first_name_match_dist0 , first_name_match_dist0X, na.rm=TRUE )
#   # fn_jw =  pmin( first_name_match_jw, first_name_match_jwX , na.rm=TRUE)
#   fn_soundex =  pmax( first_name_match_soundex , first_name_match_soundexx , na.rm=TRUE )
#   
# # combine with original, and take best fit
#   ln_dist =  pmin( last_name_match_dist, last_name_match_distx, na.rm=TRUE )
#   # last_name_match_dist0 =  pmax( last_name_match_dist0 , last_name_match_dist0X, na.rm=TRUE )
#   # ln_jw =  pmin( last_name_match_jw, last_name_match_jwX, na.rm=TRUE )
#   ln_soundex =  pmax( last_name_match_soundex , last_name_match_soundexx , na.rm=TRUE)


# sum first and last surname-surname, nom-nom 
  n_dist = list(first_name_match_dist, last_name_match_dist) 
  n_dist_sum = Reduce("+", n_dist)
  
  n_soundex = list(first_name_match_soundex, last_name_match_soundex) 
  n_soundex_sum = Reduce("+", n_soundex)
  
# sum first and last nom-surname, surname-nom 
  n_distx = list(first_name_match_distx, last_name_match_distx) 
  n_distx_sum = Reduce("+", n_distx) 
  
  n_soundexx = list(first_name_match_soundexx, last_name_match_soundexx) 
  n_soundexx_sum = Reduce("+", n_soundexx) 
  
# choose best fit
 n_dist_best = pmin( n_dist_sum, n_distx_sum , na.rm=TRUE)
 n_soundex_best = pmin( n_soundex_sum, n_soundexx_sum , na.rm=TRUE)
 

## summary of distance match ####
  low_dist = n_dist_best<3
  zero_dist = n_dist_best == 0
  zero_dist[is.na(zero_dist)] = 0 # set NA to 0
  str(zero_dist)
  
  hist(n_dist_best[ low_dist ])
  paste( 'total number of exact pariwise name matches is,' , 
         sum(zero_dist, na.rm = TRUE))
  
  ### how many contacts (columns) with dist == 0?
  n_contacts = colSums(zero_dist, na.rm = TRUE)
  paste( 'total number of contacts that have at least one case with same name is,' , 
         sum( n_contacts>0 , na.rm = TRUE) )
  qplot(n_contacts[n_contacts>0], binwidth = 1, main = "Distance: cases / contacts" )

  ### how many cases (rows) with dist == 0?
  n_cases = rowSums(zero_dist, na.rm = TRUE)
  paste( 'total number of cases that have at least one contact with same name is,' , 
         sum( n_cases>0 , na.rm = TRUE) )
  qplot(n_cases[n_cases>0], binwidth = 1, main = "Distance: contacts / case" )
  
  ### example contacts taht may have become case...
  i = 5
  contacts[ which(n_contacts>0)[i], c("ID", "Age du contact", "Surname", "OtherNames", "DateLastContact", "District", "subCounty", "Village")]
  # cases for first contact..
  cases[ which(n_dist_sum[ , which(n_contacts>0)[i] ]==0), c("ID", "Age", "Surname", "OtherNames", "DateOnset", "DistrictRes", "ParishRes", "VillageRes")]
  
 

## summary of soundex match ####
  hist(n_soundex_best)
  zero_soundex = n_soundex_best == 0
  zero_soundex[is.na(zero_soundex)] = 0 # set NA to 0
  str(zero_soundex)
  
  paste( 'total number of exact pariwise name matches is,' , 
         sum(zero_soundex, na.rm = TRUE))
  
  # how many contacts (columns) with soundex == 0?
  n_contacts = colSums(zero_soundex, na.rm = TRUE)
  paste( 'total number of contacts that have a case with same (soundex) name is,' , 
         sum( n_contacts>0 , na.rm = TRUE) )
  qplot(n_contacts[n_contacts>0], binwidth = 1, main = "Soundex: cases / contacts" )
  
  ### how many cases (rows) with soundex == 0?
  n_cases = rowSums(zero_soundex, na.rm = TRUE)
  paste( 'total number of cases that have a contact with same (soundex) name is,' , 
         sum( n_cases>0 , na.rm = TRUE) )
  qplot(n_cases[n_cases>0], binwidth = 1, main = "Soundex: contacts / case"  )
  
  ### example contacts that may have become case...
  i = 2
  contacts[ which(n_contacts>0)[i], 
            c("ID", "Age du contact", "Surname", "OtherNames", "DateLastContact", "District", "subCounty", "Village")]
  # cases for first contact..
  cases[ which(n_soundex_sum[ , which(n_contacts>0)[i] ]==0), 
         c("ID", "Age", "Surname", "OtherNames", "DateOnset", "DistrictRes", "ParishRes", "VillageRes")]
  
  

# Age #####  

case_age = cases$Age 
contact_age = contacts$`Age du contact`

## age difference in years 
  # age_diff_years = abs( case_age - contact_age) 
  
## age difference in character distance 
  age_diff_dist = stringdistmatrix( case_age, contact_age)
  same_age = age_diff_dist == 0
  same_age[is.na(same_age)] = 0 # set NA to 0
  sum( same_age , na.rm = TRUE)
 

# District #####  

  case_district = tolower(cases$DistrictRes) 
  contact_district = tolower(contacts$District)

  district_dist = stringdistmatrix( case_district, contact_district, method = 'soundex')
  same_district = district_dist == 0
  same_district[is.na(same_district)] = 0 # set NA to 0
  sum( same_district , na.rm = TRUE) 
  

# Prioritize Matches ####
  total = zero_dist + zero_soundex + same_age + same_district
  qplot(as.integer(total[total>1]), binwidth = 1L)
    
  ### example contacts that may have become case...
  match_level = 3  # criteria  
  n_contacts = which( colSums(total>= match_level, na.rm = TRUE)>0)
  
  order_total = rev(order(apply(total[, n_contacts], 2, max)))
  
  n_contacts = n_contacts[ order_total ]
  
  n_cases = which( rowSums(total>= match_level, na.rm = TRUE)>0)
  n = sum(total>= match_level)
  paste( "the total number of cases with match level", match_level, "or greater is", n)
  paste( "the frequency by strength of match is ")
         table(total)
  
# manual review #####
  
if ( readline("press y if you want to review matches") == "y" ){ 
  
  match_review = data.frame(
    contact_file = rep(contact.file.name, n) , case_file = rep(case.file.name, n),
    contact_id = rep(NA, n), case_id = rep(NA, n)
  )
  
  for (i in 1:n){
  match.tmp = NA
  
  print(
    contacts[ n_contacts[i] , 
            c("ID", "Age du contact", "Surname", "OtherNames", "DateLastContact", "District", "subCounty", "Village")]
  )

  print(  
  cases[ which(total[ , n_contacts[i] ]>= match_level), 
         c("ID", "Age", "Surname", "OtherNames", "DateOnset", "DistrictRes", "ParishRes", "VillageRes")]
  )
  
  is.match = readline("Is this a match? print row number of cases that matches (0 if no match, 00 to exit)")
  if (is.match == "00") break
  
  if ( !is.null( unlist(
      cases[ which(total[ , n_contacts[i] ]>= match_level), "ID"][as.numeric(is.match)] 
  ))   ){
    
  match.tmp = data_frame(
    contact_file = contact.file.name , case_file = case.file.name,
    contact_id = as.integer(contacts[ n_contacts[i], "ID"]), 
    case_id = as.character(
      cases[ which(total[ , n_contacts[i] ]>= match_level), "ID"][as.numeric(is.match)] )
  )
  
  match_review = rbind(match_review, match.tmp)
  }
  
  }
  
  save(match_review, file=paste0( quote(match_review), ".rda"))
  
}
  
View(match_review)


# find cases that were known .contacts:

# run vhd first to load packages and datasets

# A) match surname to surname and othernames to othernames.  Find any where both are true
# match surname-surname

# load data from original source -- see dashboard.Rmd or vhf.Rmd

load('cases.guinea.rda') # see vhf.rmd; data has had locations put in consistent style to match regions.rda
cases = cases.guinea

# if surname is empty, split OtherName, or enter 'XXXXXX'
noSurname = which(cases$Surname %in% '')
name = strsplit(cases[ noSurname, 'OtherNames'], ' ')
newSurname = rapply(name, function(x) ifelse(length(x) ==0, 'xxx', head( x, 1) ) ) 
cases[noSurname, 'Surname'] = newSurname

# if surname is empty, split OtherName, or enter 'XXXXXX'
noOtherNames = which(cases$OtherNames %in% '')
name = strsplit(cases[ noOtherNames, 'Surname'], ' ')
newOtherNames = rapply(name, function(x) ifelse(length(x) ==0, 'xxx', head( x, 1) ) ) 
cases[noOtherNames, 'OtherNames'] = newOtherNames

# encoding check
table(cases$Surname)
table(cases$DistrictRes)

# load('contacts.guinea.rda')
file.name = "contacts"
dir = 'fieldData/'  # folder, relative to this project, with spreadsheets
file = paste0(dir, file.name, '.csv') 

# Maenner
x = readLines(file, skipNul=TRUE, encoding="latin1", n=-1L )

# convert encoding from windows to mac
writeLines(iconv(x, from = "WINDOWS-1252" , to = "UTF-8"), "fieldData/tmp.csv")
# x. = readLines("fieldData/tmp.csv", skipNul=TRUE, encoding="UTF-8", n=-1L)
x. = readLines("fieldData/tmp.csv", skipNul=TRUE, n=-1L)

# first line contains worthless string that must by skipped ("\xff\xfesep=,")
cd = read.csv(textConnection(x.[-1]), header = TRUE, stringsAsFactors = FALSE)

cd$Surname = str_trim(cd$Surname)
cd$OtherNames = str_trim(cd$OtherNames)

# convert character data to dates
contacts = cd %>% 
  mutate(
    DateLastContact = parse_date_time(DateLastContact, 'dmy') ,
    DateOfLastFollowUp = parse_date_time(DateOfLastFollowUp, 'dmy')
  )

district = 'Conakry'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE, max=4), 'District'] = district

district = 'Guéckédou'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE, max=4), 'District'] = district

district = 'Nzérékoré'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE, max=4), 'District'] = district

district = 'Télimélé'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE, max=3), 'District'] = district

district = 'Kérouané'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE, max=4), 'District'] = district

district = 'Boké'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE), 'District'] = district

district = 'Labé'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE), 'District'] = district

district = 'Lélouma'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE), 'District'] = district

district = 'Tougué'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE), 'District'] = district

district = 'Forécariah'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE, max=3), 'District'] = district

district = 'Dubréka'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE), 'District'] = district

district = 'Yomou'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE), 'District'] = district

district = 'Siguiri'
contacts[ agrepl(district, contacts$District, ignore.case=TRUE, max=4), 'District'] = district


# if surname is empty, split OtherName, or enter 'XXXXXX'
noSurname = which(contacts$Surname %in% '')
name = strsplit(contacts[ noSurname, 'OtherNames'], ' ')
newSurname = rapply(name, function(x) ifelse(length(x) ==0, 'xxx', head( x, 1) ) ) 
contacts[noSurname, 'Surname'] = newSurname

save(contacts, file = 'contacts.rda')

table(contacts$Surname)
table(contacts$District)
table(week(contacts$DateLastContact))



# Test data
# needle = c('a', 'b','c','d','e')
# haystack = c('c', 'd','e','f','g')          
# maply(needle, grepl, haystack )


nameMatch = function( .needles = needles,
                        .haystack = haystack,
                        FUN = agrepl,
                        .ignore.case=TRUE,
                        ...
                        ){
  library( plyr )
  library(dplyr)
  
  # remove surnames that are empty.
  # match surname-surname:  result is matrix with columns = cases; rows = contacts
  m <- maply( .needles, FUN , 
                        .haystack,
                        .progress = "text",
                        ignore.case = .ignore.case)
  return(m)
}

### Perform name match... nameMatch( needle, haystack, agrepl)  ####

sur.sur = nameMatch(cases$Surname, contacts$Surname, max.distance = 4)
sur.other = nameMatch(cases$Surname, contacts$OtherNames, max.distance = 4)
other.sur = nameMatch(cases$OtherNames, contacts$Surname, max.distance = 4)
other.other = nameMatch(cases$OtherNames, contacts$OtherNames, max.distance = 4)
   
# both true when surname match with surname, or both true when surname match with OtherNames
match =  (sur.sur * other.other) + (other.sur * sur.other)
  

### inspect matches with other attributes ####
case.match =  unique(which(match>=1, arr.ind=TRUE)[, 1])
contact.match = unique(which(match>=1, arr.ind=TRUE)[, 2]) 

cat(length(case.match), 'cases matched at least one contact;', 
        length(contact.match), 'contacts matched at least one case.')

# setup match fields
cases$WasContact = 0L
contacts$BecameCase = 0L
contacts$caseID = NA
contacts$score = NA

# select columns
case.cols = c('ID', 'Surname','OtherNames', 'Age', 'Gender' , 'DistrictRes', 'SCRes', 'VillageRes', 
              'DateOnset', 'dateOnset',
              'ContactName1', 'ContactName2')

contact.cols = c('ID', 'Surname','OtherNames', 'Age', 'Gender' , 'District', 'SubCounty', 'Village', 'DateLastContact', 'SourceCase', 'SourceCaseID')

# print all combos
library(knitr)

j = 0; k = 0

for (i in 1:length(case.match)){
 
  case.index = case.match[i]
  potential.case.match = cases[ case.index , case.cols]

  contact.index = which( match[ case.match[i], ] > 0 )
#   contact.index = which( match[ 808, ] > 0 )
  
  potential.contact.matches = contacts[ contact.index , contact.cols] %>%
    mutate(  
            incubation = as.Date(potential.case.match$dateOnset) - as.Date(DateLastContact)  ,  
            incubation.dist = ifelse(is.na( incubation), TRUE, 
                  ifelse( incubation >= -10 & incubation <= 31, TRUE, FALSE)) , 
            age.dist = ifelse( is.na(potential.case.match$Age) | 
                                 potential.case.match$Age %in% '' |
                                 is.na(Age), TRUE, 
                               agrepl(potential.case.match$Age, Age) ),
            district.dist = ifelse( is.na(potential.case.match$DistrictRes) | 
                                      potential.case.match$DistrictRes %in% '' |
                                      is.na(District), TRUE, 
                                    agrepl(potential.case.match$DistrictRes, District, ignore.case = TRUE)) ,
            SC.dist = ifelse( is.na(potential.case.match$SCRes) | 
                                potential.case.match$SCRes %in% '' |
                                is.na(SubCounty) 
                              , TRUE, 
                              agrepl(potential.case.match$SCRes, SubCounty, ignore.case =  TRUE) ) ,
            score = incubation.dist + age.dist + district.dist + SC.dist
            ) %>%
    arrange( -score ) %>% as.data.frame()
  

# ASSEMMENT
  j = j + 1
  if (nrow(potential.contact.matches) >=1 ){

    # write out all potential matches to manually reveiw
#     j = j + 1
#     cat('\n\n **Match number',j, '**\n')
#     print ( kable( potential.case.match ) )
#     cat('contact:\n')
#     print( kable( potential.contact.matches ) )
   
    # tag each case record that meets the match criterea (only one)
    max.score = max(potential.contact.matches$score, na.rm = TRUE)
    
    potential.contact.matches = potential.contact.matches %>% 
      filter( district.dist ) %>%
      filter( incubation.dist ) %>%
      filter( score >= 3 & score == max.score) %>%
      filter( !is.na(incubation) )
    
    if ( nrow(potential.contact.matches) >=1 ){
      k = k + 1
      
        cases[ cases$ID %in% potential.case.match$ID , 'WasContact'] = 1
        # tag each contact record that meets match criteria (may be >1 if there were >1 source cases)

        for( ii in (1: nrow(potential.contact.matches) ) ){
              contacts[ contacts$ID %in% potential.contact.matches$ID , 'BecameCase'] = 1
              contacts[ contacts$ID %in% potential.contact.matches$ID , 'score'] = potential.contact.matches$score
              contacts[ contacts$ID %in% potential.contact.matches$ID , 'caseID'] = potential.case.match$ID
      }
      
      # write out HIGH PROBABILITY potential matches to manually reveiw
      cat('\n\n **Match number', k , '/', j, '**\n')
      print ( kable( potential.case.match ) )
      cat('contact:\n')
      print( kable( potential.contact.matches ) )
    }
  }
}

#### keys  ####
keys = data.frame(
  caseID = contacts[ contacts$BecameCase == 1, 'caseID'] ,
  contactID = contacts[ contacts$BecameCase == 1, 'ID'] ,
  score = contacts[ contacts$BecameCase == 1, 'score']
) %>% arrange(caseID, contactID, score)

# manual de-match: no .contacts made match to this case
# notMatch = keys[,'case'] %in% c('GUI-CKY-14-1590', 'GUI-CKY-14-1408', 'GUI-CKY-14-3751',
#                                 'GUI-CKY-14-3755', 'GUI-CKY-14-3841')
# keys$match = 1
# keys[ notMatch, 'match' ] = 0

save(keys, file='keys.rda')

```

# find cases that were known .contacts:


# A) match surname to surname and othernames to othernames.  Find any where both are true
# match surname-surname

# load data from original source -- see dashboard.Rmd or vhf.Rmd

# load('cases.guinea.rda') # see vhf.rmd; data has had locations put in consistent style to match regions.rda
# cases = cases.guinea
load('cases.rda')
# encoding check
table(cases$Surname)
table(cases$DistrictRes)

# load('contacts.guinea.rda')
load('contacts.rda')
contacts = contacts.guinea
table(contacts$Surname)
table(contacts$District)

# clean district names with fuzz matching (agrepl)
Origine = 'Télimélé'
contacts[ agrepl(Origine, contacts$District, ignore.case = T, max.distance =4), 'District' ] = Origine
Origine = 'Conakry'
contacts[ agrepl(Origine, contacts$District, ignore.case = T, max.distance =4), 'District' ] = Origine
Origine = 'Forécariah'
contacts[ agrepl(Origine, contacts$District, ignore.case = T, max.distance =4), 'District' ] = Origine
Origine = 'Kérouané'
contacts[ agrepl(Origine, contacts$District, ignore.case = T, max.distance =4), 'District' ] = Origine
Origine = 'Guéckédou'
contacts[ agrepl(Origine, contacts$District, ignore.case = T, max.distance =4), 'District' ] = Origine
table(contacts$District)


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
            age.dist = ifelse( is.na(potential.case.match$Age) | is.na(Age), TRUE, 
                               agrepl(potential.case.match$Age, Age) ),
            district.dist = ifelse( is.na(potential.case.match$DistrictRes) | is.na(District), TRUE, 
                                    agrepl(potential.case.match$DistrictRes, District, ignore.case = TRUE)) ,
            SC.dist = ifelse( is.na(potential.case.match$SCRes) | is.na(SubCounty), TRUE, 
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
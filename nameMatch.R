
# find cases that were known .contacts:


# A) match surname to surname and othernames to othernames.  Find any where both are true
# match surname-surname

# load data from original source -- see dashboard.Rmd or vhf.Rmd
load('contacts.rda')
load('cases.rda')

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
  # match surname-surname:  result is matrix with columns = cases; rows = contacts
  m <- maply( .needles, FUN ,
                        .haystack,
                        .progress = "text",
                        ignore.case = .ignore.case)
  return(m)
}

# nameMatch( needle, haystack, agrepl)

sur.sur = nameMatch(cases$Surname, contacts$Surname)
sur.other = nameMatch(cases$Surname, contacts$OtherNames)
other.sur = nameMatch(cases$OtherNames, contacts$Surname)
other.other = nameMatch(cases$OtherNames, contacts$OtherNames)
   
# both true when surname match with surname, or both true when surname match with OtherNames
match =  (sur.sur * other.other) + (other.sur * sur.other)
  
# inspect matches
case.match =  unique(which(match>=1, arr.ind=TRUE)[, 1])
contact.match = unique(which(match>=1, arr.ind=TRUE)[, 2]) 

cat(length(case.match), 'cases matched at least one contact;', 
        length(contact.match), 'contacts matched at least one case.')

# setup match fields
cases$WasContact = 0L
contacts$BecameCase = 0L
contacts$caseID = NA

# select columns
case.cols = c('ID', 'Surname','OtherNames', 'Age', 'Gender' , 'DistrictRes', 'SCRes', 'VillageRes', 
              'DateOnset', 'dateOnset',
              'ContactName1', 'ContactName2')

contact.cols = c('ID', 'Surname','OtherNames', 'Age', 'Gender' , 'District', 'SubCounty', 'Village', 'DateLastContact', 'SourceCase', 'SourceCaseID')

# print all combos
library(knitr)
j = 0
for (i in 1:length(case.match)){
 
  case.index = case.match[i]
  potential.case.match = cases[ case.index , case.cols]

  contact.index = which( match[ case.match[i], ] > 0 )
#   contact.index = which( match[ 808, ] > 0 )
  
  potential.contact.matches = contacts[ contact.index , contact.cols] %>%
    mutate( incubation = potential.case.match$dateOnset - DateLastContact ,  
            incubation.dist = ifelse( incubation >= -7 & incubation <= 31, TRUE, FALSE), 
            age.dist = ifelse( is.na(potential.case.match$Age) | is.na(Age), TRUE, 
                               agrepl(potential.case.match$Age, Age)),
            district.dist = ifelse( is.na(potential.case.match$DistrictRes) | is.na(District), TRUE, 
                                    agrepl(potential.case.match$DistrictRes, District)) ,
            SC.dist = ifelse( is.na(potential.case.match$SCRes) | is.na(SubCounty), TRUE, 
                              agrepl(potential.case.match$SCRes, SubCounty) )
            ) %>%
            group_by(ID) %>%
            mutate(
              score = sum( c(incubation.dist, age.dist, district.dist, SC.dist), na.rm=TRUE)
            ) %>%
    arrange( -score ) %>% as.data.frame()
  
  # write out all potential matches to manually reveiw
  if (nrow(potential.contact.matches) >=1 ){
#     j = j + 1
#     cat('\n\n **Match number',j, '**\n')
#     print ( kable( potential.case.match ) )
#     cat('contact:\n')
#     print( kable( potential.contact.matches ) )

  # tag contact as case is sufficienly close
      potential.contact.matches %>%
        filter( score >=3 )
      if (nrow(potential.contact.matches) >=1 ){
        # tag each case record that meets the match criterea (only one)
        cases[ cases$ID %in% potential.case.match$ID , 'WasContact'] = 1]
        # tag each contact record that meets match criteria (may be >1 if there were >1 source cases)
        for( ii in (1: nrow(potential.contact.matches) ) ){
              contacts[ contacts$ID %in% potential.contact.matches$ID , 'BecameCase'] = 1
              contacts[ contacts$ID %in% potential.contact.matches$ID , 'caseID'] = potential.case.match$ID
      }
    }
  }
}

#### keys
keys = data.frame(
  #   contact = .contacts$ID[key.pairs[,1]],
  #   case = .cases$ID[key.pairs[,2]]
  contact = .contacts[.contacts$BecameCase == 1, 'ID'],
  case = .contacts[.contacts$BecameCase == 1, 'caseID']
)

# manual de-match: no .contacts made match to this case
# notMatch = keys[,'case'] %in% c('GUI-CKY-14-1590', 'GUI-CKY-14-1408', 'GUI-CKY-14-3751',
#                                 'GUI-CKY-14-3755', 'GUI-CKY-14-3841')
# keys$match = 1
# keys[ notMatch, 'match' ] = 0

keys = keys %>% 
  #   filter( match == 1) %>%
  inner_join(cases, by = c('case' = 'ID')) %>%
  inner_join(.contacts[, c('ID', 'SourceCaseID', 'SourceCase')], by = c('contact' = 'ID')) 

# Case-ContactName should refer to same person as contact's SourceCase
# keys %>% select(case, contact, ContactName1, ContactName2 , SourceCase) %>% arrange(case)

# edge list
edges = keys %>%
  mutate( source = SourceCaseID ) %>%
  select (source, case)

nodes = unique( c(edges[,1], edges[,2]))
node.data = cases[ cases$ID %in% nodes, ] %>% select (ID, dateOnset, Age, DistrictRes, SCRes) %>%
  mutate(week = week(dateOnset),
         day = as.numeric(strftime(dateOnset, format = "%j"))
  )

# replace missing age with 35
node.data[ is.na(node.data$Age), 'Age'] = 35

# limit edges to thosse with node data (why some missing?  not confirmed source cases?)
edges = edges %>% inner_join(node.data, by = c('source' = 'ID'))

cky.keys = keys
cky.edges = edges
save(cky.keys, cky.edges, file='cky.keys.rda')

```
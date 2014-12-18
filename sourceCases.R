substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

load('keys.rda')
load('cases.guinea.rda')
load('contacts.guinea.rda')
cases = cases.guinea
contacts = contacts.guinea

# select columns that will be used for display
case.cols = c('ID', 'Surname','OtherNames', 'Age', 'Gender' , 
              'DistrictRes', 'SCRes', 'VillageRes', 
              'DateOnset', 'dateOnset', 'ContactName1', 'ContactName2')

contact.cols = c('ID', 'DateLastContact', 'SourceCase', 'SourceCaseID', 
                 'HeadHousehold', 'RelationshipToCase')

edges = keys %>% 
  inner_join(cases[, case.cols], by = c('caseID' = 'ID')) %>%  # excludes non-cases
  inner_join(contacts[, contact.cols], by = c('contactID' = 'ID')) %>% 
  merge(Region, all.x = TRUE, by ='DistrictRes' ) %>%
  rename( caseRegion = Region,
          caseDistrict = DistrictRes,
          caseSC = SCRes ,
          case = caseID , 
          source = SourceCaseID ,
          RelationshipToSource = RelationshipToCase 
  ) %>% 
  select( source, case , score, Age, Gender, 
          caseDistrict, caseSC, VillageRes, 
          dateOnset, DateOnset, HeadHousehold, RelationshipToSource) %>%
  unique()  # may have multiple edeges because of duplicate contacts

# optional, filter to a  source case
# edges = edges %>% filter(source %in% sourceID) %>% arrange(case, score)}

sourceCases = edges %>% 
  select(source) %>% unique() %>% 
  left_join( cases , by = c('source' = 'ID' )) %>%
  merge(Region, all.x = TRUE, by ='DistrictRes' ) %>%
  rename(sourceDistrict = DistrictRes,
         sourceRegion = Region,
         sourceSCRes = SCRes )
#   mutate( sourceSCRes = toupper(sourceSCRes) )%>%
#   select( source, sourceSCRes)  

sourceCases[is.na(sourceCases$sourceSCRes) | 
              sourceCases$sourceSCRes %in% '', 'sourceSCRes'] = 'Unknown'

sourceCases[is.na(sourceCases$sourceDistrict) | 
              sourceCases$sourceDistrict %in% '', 'sourceDistrict'] = 'Unknown'

conakry.districts = c('DIXINN','KALOUM' ,'MATAM', 'MATOTO', 'RATOMA')
notConakry = !( toupper(sourceCases$sourceSCRes) ) %in% conakry.districts
notMissing = !( sourceCases$sourceSCRes %in% 'Unknown')
sourceCases[ notConakry &  notMissing, 'sourceSCRes'] = ' OUTSIDE CONAKRY'

sourceCases %>% count(sourceSCRes)

d = data.cky %>% 
  left_join( edges %>% select(case, source), by = c('ID'= 'case')) %>%
  left_join(sourceCases[, c('source', 'sourceDistrict', 'sourceSCRes', 'sourceRegion')], 
            by = 'source') %>%
  mutate( idnum = substrRight(source, 4),
          period = as.Date(floor_date(dateOnset, "week"))
          #           SCRes = ifelse(is.na(SCRes) | SCRes %in% '', 
          #                          "Unkown", toupper(SCRes)) , 
          #           sourceSCRes = ifelse(is.na(sourceSCRes) | sourceSCRes %in% '', 
          #                                " Unkown", toupper(sourceSCRes))
  ) %>% 
  group_by( period, DistrictRes, ID ) %>%
  summarize( freq = 1 ,
             idnum = idnum,
             sourceDistrict = sourceDistrict ,
             sourceRegion = sourceRegion, 
             sourceSCRes = sourceSCRes) %>%
  mutate( cumsum = cumsum(freq),
          midpoint = (cumsum(freq) - 0.5 * freq)) %>%
  arrange(DistrictRes, period, sourceDistrict)

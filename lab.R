setwd("//cdc/locker/2014_Ebola_Epi_VHF/Lab data/SL/processed")
source("Separate labs.R")
source("Combined by lab.R")

######  Combine apply  ######

#install.packages(c("xlsx","plyr","stringr","lubridate"))
library(xlsx)
library(plyr)
library(stringr)
library(lubridate)

# CDC

labFileList <- list.files("//cdc/locker/2014_Ebola_Epi_VHF/Lab data/SL/processed/CDC")
cdc_labs<-list()

for (j in 1:length(labFileList)){
  dailyLab<-labFileList[j]
  lab<-read.csv(paste0("CDC/",dailyLab), stringsAsFactors=FALSE,header=TRUE)
  cdc_labs[[j]]<-lab
}
cdc_labs<-ldply(cdc_labs, data.frame)

###Fix date columns
cdc_labs$Date.of.Symptom.Onset <- as.Date(as.numeric(cdc_labs$Date.of.Symptom.Onset), origin = "1899-12-30")
cdc_labs$Date.of.Specimen.Collection <- as.Date(as.numeric(cdc_labs$Date.of.Specimen.Collection), origin = "1899-12-30")
cdc_labs$Date.Specimen.Received.at.Lab <- as.Date(as.numeric(cdc_labs$Date.Specimen.Received.at.Lab), origin = "1899-12-30")
cdc_labs$Date.Tested <- as.Date(as.numeric(cdc_labs$Date.Tested), origin = "1899-12-30")

#write new file:
write.csv(cdc_labs,"recombined/cdclabs.csv",row.names=FALSE)


# Canadian
labFileList <- list.files("//cdc/locker/2014_Ebola_Epi_VHF/Lab data/SL/processed/CANADIAN")
canadian_labs<-list()
for (j in 1:length(labFileList)){ 
  dailyLab<-labFileList[j]
  lab<-read.csv(paste0("CANADIAN/",dailyLab), stringsAsFactors=FALSE,header=TRUE)
  canadian_labs[[j]]<-lab
}
canadian_labs<-ldply(canadian_labs, data.frame)

###Fix date columns
canadian_labs$Date.received<-as.Date(as.numeric(canadian_labs$Date.received), origin = "1899-12-30")
canadian_labs$Date.tested<-as.Date(as.numeric(canadian_labs$Date.tested), origin = "1899-12-30")


#write new file:
write.csv(canadian_labs,"recombined/canadianlabs.csv",row.names=FALSE)

# Chinese
labFileList <- list.files("//cdc/locker/2014_Ebola_Epi_VHF/Lab data/SL/processed/CHINESE")
chinese_labs<-list()
for (j in 1:length(labFileList)){
  dailyLab<-labFileList[j]
  lab<-read.csv(paste0("CHINESE/",dailyLab), stringsAsFactors=FALSE,header=TRUE)
  chinese_labs[[j]]<-lab
}

chinese_labs<-ldply(chinese_labs, data.frame)
###Fix date columns
chinese_labs$Date.of.Symptom.Onset <- as.Date(as.numeric(chinese_labs$Date.of.Symptom.Onset), origin = "1899-12-30")
chinese_labs$Date.of.Specimen.Collection <- as.Date(as.numeric(chinese_labs$Date.of.Specimen.Collection), origin = "1899-12-30")
chinese_labs$Date.Specimen.Received.at.Lab <- as.Date(as.numeric(chinese_labs$Date.Specimen.Received.at.Lab), origin = "1899-12-30")
chinese_labs$Date.Tested <- as.Date(as.numeric(chinese_labs$Date.Tested), origin = "1899-12-30")
chinese_labs$Date.of.Hospitalization <- as.Date(as.numeric(chinese_labs$Date.of.Hospitalization), origin = "1899-12-30")



#write new file:
write.csv(chinese_labs,"recombined/chineselabs.csv",row.names=FALSE)


# NICD South African
labFileList <- list.files("//cdc/locker/2014_Ebola_Epi_VHF/Lab data/SL/processed/NICD")
nicd_labs<-list()
for (j in 1:length(labFileList)){
  dailyLab<-labFileList[j]
  lab<-read.csv(paste0("NICD/",dailyLab), stringsAsFactors=FALSE,header=TRUE)
  nicd_labs[[j]]<-lab
}
nicd_labs<-ldply(nicd_labs, data.frame)

###Fix date columns
nicd_labs$Date.of.Symptom.Onset <- as.Date(as.numeric(nicd_labs$Date.of.Symptom.Onset), origin = "1899-12-30")
nicd_labs$Date.of.Specimen.Collection <- as.Date(as.numeric(nicd_labs$Date.of.Specimen.Collection), origin = "1899-12-30")
nicd_labs$Date.Specimen.Received.at.Lab <- as.Date(as.numeric(nicd_labs$Date.Specimen.Received.at.Lab), origin = "1899-12-30")
nicd_labs$Date.Tested <- as.Date(as.numeric(nicd_labs$Date.Tested), origin = "1899-12-30")
nicd_labs$Date.of.Hospitalization <- as.Date(as.numeric(nicd_labs$Date.of.Hospitalization), origin = "1899-12-30")

#write new file:
write.csv(nicd_labs,"recombined/nicdlabs.csv",row.names=FALSE)


# Kerrytown
labFileList <- list.files("//cdc/locker/2014_Ebola_Epi_VHF/Lab data/SL/processed/KERRY")
kerry_labs<-list()
for (j in 1:length(labFileList)){
  dailyLab<-labFileList[j]
  lab<-read.csv(paste0("KERRY/",dailyLab), stringsAsFactors=FALSE,header=TRUE)
  kerry_labs[[j]]<-lab
}
kerry_labs<-ldply(kerry_labs, data.frame)

###Fix date columns
kerry_labs$BatchDateTime <- as.Date(as.numeric(kerry_labs$BatchDateTime), origin = "1899-12-30")
kerry_labs$SpecimenDate <- as.Date(as.numeric(kerry_labs$SpecimenDate), origin = "1899-12-30")
brdt<-as.Date(as.numeric(kerry_labs$BatchReportDateTime), origin="1899-12-30")
brdt_l <- dmy_hms(kerry_labs$BatchReportDateTime)
kerry_labs$BatchReportDateTime<-ifelse(is.na(brdt_l), as.character(brdt), as.character(brdt_l))
kerry_labs$ReportDateTime<-dmy_hms(kerry_labs$ReportDateTime)


#write new file:
write.csv(kerry_labs,"recombined/kerrylabs.csv",row.names=FALSE)

######  R summary  #####
setwd("C:/Users/bzp3/dropbox/_Ebola/VHF")

library(dplyr)

# cdc
cdc = read.csv('cdclabs.csv', stringsAsFactors = FALSE) %>% 
  select(Patient.District.MSF.ID..,
         Specimen.Type, Laboratory.ID, New.Repeat.Sample, District.of.Origin, 
               Date.of.Specimen.Collection, Confirmed.Ebola..yes.no.) %>%
  filter( Confirmed.Ebola..yes.no. %in% c('no', 'yes')) %>%
  group_by(Patient.District.MSF.ID..) %>%
  summarize( case = max(ifelse(Confirmed.Ebola..yes.no. %in% 'yes', 1, 0)),
          district = District.of.Origin,
          specimen = max(Specimen.Type),
          date = min(Date.of.Specimen.Collection)
          )

cdc[agrepl('blood', cdc$specimen, ignore.case = T), 'specimen'] = 'blood'
cdc[agrepl('swab', cdc$specimen, ignore.case = T), 'specimen'] = 'swab'

cdc. = cdc %>% select(date, district, specimen, case)

# cdc %>% count(case)
# cdc %>% count(District.of.Origin)
# cdc %>% count(Specimen.Type)
# cdc %>% count(specimen)

# canadian
canada = read.csv('canadianlabs.csv', stringsAsFactors = FALSE) %>% 
  select(MSF.number, New.or.Repeat.Sample, District, 
         Date.of.Onset , Lab.confirmed, Sample.type ) %>%
  mutate( Lab.confirmed = tolower(Lab.confirmed)) %>%
  filter( Lab.confirmed %in% c('no', 'yes')) %>%
  group_by(MSF.number) %>%
  summarize( 
          case = max(ifelse( Lab.confirmed %in% 'yes', 1, 0)),
          district = District,
          specimen = Sample.type,
          date = min(Date.of.Onset)
  )

canada[agrepl('blood', canada$specimen, ignore.case = T), 'specimen'] = 'blood'
canada[agrepl('swab', canada$specimen, ignore.case = T), 'specimen'] = 'swab'

canada. = canada %>% select(date, district, specimen, case)

# canada %>% count(case)
# canada %>% count(District)
# canada %>% count(specimen)

# chinese
chinese = read.csv('chineselabs.csv', stringsAsFactors = FALSE)  %>%
  select(Patient.District.MSF.ID.., Specimen.Type , District.of.Origin, 
         Date.of.Specimen.Collection, New.Repeat.Sample, Confirmed.Ebola..yes.no.) %>%
  mutate( Confirmed.Ebola..yes.no. = tolower(Confirmed.Ebola..yes.no.)) %>%
  filter( Confirmed.Ebola..yes.no. %in% c('no', 'yes')) %>%
  group_by(Patient.District.MSF.ID..) %>%
  summarize( 
    case = max(ifelse( Confirmed.Ebola..yes.no.  %in% 'yes', 1, 0)),
          district = min(District.of.Origin),
          specimen = max(Specimen.Type),
          date = min(Date.of.Specimen.Collection)
  )

chinese[agrepl('blood', chinese$specimen, ignore.case = T), 'specimen'] = 'blood'
chinese[agrepl('swab', chinese$specimen, ignore.case = T), 'specimen'] = 'swab'

chinese. = chinese %>% select(date, district, specimen, case)

# chinese %>% count(case)
# chinese %>% count(District.of.Origin)
# chinese %>% count(specimen)

# kerrytown
kerry = read.csv('kerrylabs.csv', stringsAsFactors = FALSE)  %>%
  select(ETAID, SpecimenType , District, 
         SpecimenDate, EbolaPCRResult) %>%
  filter( EbolaPCRResult%in% c('Negative', 'POSITIVE')) %>%
  group_by(ETAID) %>%
  summarize( 
    case = max(ifelse( EbolaPCRResult %in% 'POSITIVE', 1, 0)),
    district = min(District),
    specimen = max(SpecimenType),
    date = min(SpecimenDate)
  )
 
kerry[agrepl('blood', kerry$specimen, ignore.case = T), 'specimen'] = 'blood'
kerry[agrepl('swab', kerry$specimen, ignore.case = T), 'specimen'] = 'swab'

kerry %>% count(case)
kerry %>% count(District)
kerry %>% count(specimen)

kerry. = kerry %>% select(date, district, specimen, case)

# southafrica
# nicd = read.csv('nicdlabs.csv', stringsAsFactors = FALSE)  %>%
#   select(Laboratory.ID, Specimen.Type , District.of.Origin, 
#          Date.of.Specimen.Collection, New.Follow.up.sample, Confirmed.Ebola..yes.no. ) %>%
# # no positives!!!!!
#   filter( Confirmed.Ebola..yes.no.  %in% c('no', 'yes')) %>%
#   mutate( case = ifelse(Confirmed.Ebola..yes.no.  %in% 'yes', 1, 0) )

# nicd %>% count(case)
# nicd %>% count(District.of.Origin)
# nicd %>% count(Specimen.Type)
# 

# combine data
library(lubridate)
data =  rbind(cdc., canada., chinese., kerry.) 

# fix district names
district = 'Western Urban'
data[ agrepl(district, data$district, ignore.case = T), 'district' ] = district 
district = 'Western Rural'
data[ agrepl(district, data$district, ignore.case = T), 'district' ] = district 
district = 'Port Loko'
data[ agrepl(district, data$district, ignore.case = T), 'district' ] = district 
district = 'Kambia'
data[ agrepl(district, data$district, ignore.case = T), 'district' ] = district 
data = data %>% filter( !(district %in% '?') )

data = data  %>%
  filter( !is.na(date) & !is.na(district) & !is.na(specimen)) %>%
  filter(!(specimen %in% 'EDTA')) %>%
  mutate( date = ymd(date),
          period = as.Date(floor_date(date, "week")) ) %>%
  group_by( period, specimen) %>%
  summarize( n = n(),
             pos = sum(case),
             percentage = pos/n )

# data %>% count(district)

g =
  data %>% 
#   filter( district %in% c('Bo', 'Bombali', 'Kenema', 'Western Urban')) %>% 
  ggplot( aes(period, percentage)) +
  geom_line( ) +
  geom_point( aes(size = n), stat = 'identity') +
  scale_x_date('Week beginning', labels = date_format("%b-%d"),
               breaks = date_breaks("week")) +
  scale_y_continuous( labels = percent, limits = c(0,1) ) +
  ylab('% Positive') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 2),
        legend.position = 'right' ) +
  facet_grid( specimen ~ .)
g

###### REad excel summary  ####


library(XLConnect)
setwd("C:/Users/bzp3/dropbox/_Ebola/VHF")
dir = ''
file.name = "Lab data visualization"
excel.file = paste0(dir, file.name, '.xlsx') 
wb = loadWorkbook(excel.file)
lab =   readWorksheet(wb, 'summary')

data = lab %>%
  group_by( period = as.Date(floor_date(date, "week")) ) %>%
  summarize( n = sum(Total),
             pos = sum(Pos),
             percentage = pos/n) %>%
  as.data.frame()

g = 
  ggplot(data , aes( period, percentage) ) +
  scale_x_date('\nWeek beginning', labels = date_format("%b-%d"),
               breaks = date_breaks("week")) +
  scale_y_continuous('% Samples Confirmed\n', labels = percent, limits = c(0,1)) +
  geom_bar( stat = 'identity', fill = 'blue') + 
  geom_text(color = 'white', size =6,
            aes(label = n ), vjust = 1)  +
  theme(axis.text= element_text(angle = 90, color = 'black', size=20) ,
        axis.title= element_text(face = 'bold', color = 'black', size=32),
        strip.text = element_text(size=20, face="bold")
  ) 
g

file.remove('dash.perchosp.guinea.facet.png')
png(file='dash.perpos.SL.png', width = 24, height = 11, units = 'in', 
    res = 72, bg = "transparent")
g
dev.off()

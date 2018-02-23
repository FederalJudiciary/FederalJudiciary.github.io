setwd("C:/cygwin64/home/659163/GitHub/FederalJudiciary.github.io")
library(lubridate)
library(ggmap) 
library(rvest)
library(XML)
library(dplyr)
library(foreach)
library(plyr)
library(gender)
library(stringr)
library(janitor)
library(tidyr)
library(dplyr)
library(data.table)
presadentz <- c("Barack_Obama","Donald_Trump" , "George_W._Bush","Bill_Clinton",
          "George_H._W._Bush",
          "Ronald_Reagan",
          "Jimmy_Carter",
          "Gerald_Ford",
          "Richard_Nixon",
          "Lyndon_B._Johnson",
          "John_F._Kennedy",
          "Dwight_D._Eisenhower",
          "Harry_S._Truman",
          "Franklin_D._Roosevelt")
prez_start_year <- c(2009, 2017, 2001, 1993, 1989,1981, 1977 ,1974,1969,1964,1961,1953,1945,1933)
cbind(presadentz,prez_start_year)

links <-paste0("https://en.wikipedia.org/wiki/List_of_federal_judges_appointed_by_" ,presadentz)


 ## New Tables
pres_appeals0 <- build_curveclimb(3,c(1:5,7:8,10:14))
#pres_appeals0 <- build_curveclimb(3,c(1:5))
  str(pres_appeals0)
  table(pres_appeals0$circuit)
  pres_appeals0$court_note_1 <- NULL
  colnames(pres_appeals0)  
  str(pres_appeals0)
pres_appeals1 <- build_curveclimb(3,c(6,9))
  names(pres_appeals1)  
  pres_appeals1$confirmation_vote <- NA 
  pres_appeals1 <- pres_appeals1[,c(1:5,11,6:10)]
  names(pres_appeals1)  
  str(pres_appeals1)
  
 
  
   

pres_appeals <- rbind(pres_appeals0,pres_appeals1)             
head(pres_appeals)
colnames(pres_appeals)

pres_appeals$start_active_date <- as.Date(substr((sapply(strsplit(pres_appeals$began_active_service,"-0000"), `[`, 1)),9,19))
pres_appeals$end_active_date <- gsub("Incumbent",paste0("00000000",Sys.Date()),pres_appeals$ended_active_service)
pres_appeals$end_active_date <- substr((sapply(strsplit(pres_appeals$end_active_date,"-0000"), `[`, 1)),9,19)
pres_appeals$end_active_date <- as.Date(substr(pres_appeals$end_active_date,1,10))
pres_appeals$end_senior_date <- gsub("Incumbent",paste0("00000000",Sys.Date()),pres_appeals$ended_senior_status)
pres_appeals$end_senior_date <- gsub('0 !',"000000004712-12-31",pres_appeals$end_senior_date)
pres_appeals$end_senior_date <- substr((sapply(strsplit(pres_appeals$end_senior_date,"-0000"), `[`, 1)),9,19)
pres_appeals$end_senior_date <- as.Date(substr(pres_appeals$end_senior_date,1,10))
pres_appeals$nomination_date <- gsub("Incumbent",paste0("00000000",Sys.Date()),pres_appeals$nomination_date)
pres_appeals$nomination_date <- gsub('0 !',"000000004712-12-31",pres_appeals$nomination_date)
pres_appeals$nomination_date <- substr((sapply(strsplit(pres_appeals$nomination_date,"-0000"), `[`, 1)),9,19)
pres_appeals$nomination_date <- as.Date(substr(pres_appeals$nomination_date,1,10))
pres_appeals$confirmation_date <- gsub("Incumbent",paste0("00000000",Sys.Date()),pres_appeals$confirmation_date)
pres_appeals$confirmation_date <- gsub('0 !',"000000004712-12-31",pres_appeals$confirmation_date)
pres_appeals$confirmation_date <- substr((sapply(strsplit(pres_appeals$confirmation_date,"-0000"), `[`, 1)),9,19)
pres_appeals$confirmation_date <- as.Date(substr(pres_appeals$confirmation_date,1,10))
pres_appeals$judge <- substr(pres_appeals[,c("judge")],1,nchar(pres_appeals[,c("judge")])/2+1)
pres_appeals$confirmation_vote <- (sapply(strsplit(pres_appeals$confirmation_vote,"!"), `[`, 2))
pres_appeals$court <- paste0(gsub(" Cir.","",(sapply(strsplit(pres_appeals$circuit,"!"), `[`, 2)))," Circuit")
pres_appeals$Judgenum <- pres_appeals$'Judge#'
pres_appeals$Judgenum <- as.numeric(ifelse(is.na(sapply(strsplit(pres_appeals$Judgenum,"♠"), `[`, 2)),
                        pres_appeals$Judgenum,sapply(strsplit(pres_appeals$Judgenum,"♠"), `[`, 2)))
pres_appeals$court_type <- "Appeals"
pres_appeals$court_note_1 <- NULL
pres_appeals$months_from_inauguration <- (interval(pres_appeals$pres_start_date,pres_appeals$start_active_date ) %/% months(1))
str(pres_appeals$Judgenum)
  head(pres_appeals)
  colnames(pres_appeals)
  

appeals_appointments <- pres_appeals[,c(16,2,4:6,10:15,17:18)]

colnames(appeals_appointments)
  head(appeals_appointments)
  nrow(appeals_appointments)
  unique(pres_appeals$appointed_by)
  
write.csv(appeals_appointments, file= "./appeals_appointments_current.csv" , row.names=FALSE)  
appeals_appointments_current_read <- read.csv("./appeals_appointments_current.csv",stringsAsFactors = FALSE)
str(appeals_appointments_current_read)


historical <- read.csv("./appeals_appointments.csv",stringsAsFactors = FALSE)
historical_no_current <- historical[!historical$appointed_by=='Donald Trump',]
colnames(historical_no_current)
str(historical_no_current)

combined_appeals_appointments <- rbind(appeals_appointments_current_read,historical_no_current)
colnames(combined_appeals_appointments)
unique(combined_appeals_appointments$start_active_date)
write.csv(combined_appeals_appointments, file= "./appeals_appointments.csv" , row.names=FALSE)


### Write all appointments
all_appointments0 <- rbind(final_district_appointments,combined_appeals_appointments)
str(final_district_appointments)
final_district_appointments %>% dplyr::filter(appointed_by %like% 'Barack') %>%
  group_by(appointed_by,court_type,start_active_date) %>%
  dplyr::summarise(n = n())

colnames(appeals_appointments)
colnames(final_district_appointments)




all_appointments1 <- arrange(all_appointments0 ,appointed_by,start_active_date)



#all_appointments1 %>% dplyr::filter(appointed_by %like% 'Barack')  -> all_appointments1


all_appointments1 <- ddply(all_appointments1,c("appointed_by","court_type"),transform,court_judges_count=seq(from=1,by=1,length.out=length(appointed_by)))
all_appointments1 <- ddply(all_appointments1,c("appointed_by"),transform,all_judges_count=seq(from=1,by=1,length.out=length(appointed_by)))
colnames(all_appointments1)

head(arrange(all_appointments1 ,appointed_by,start_active_date))
tail(arrange(all_appointments1 ,appointed_by,start_active_date))



all_appointments <- all_appointments1
colnames(all_appointments)
table(all_appointments$court_type,all_appointments$appointed_by)

write.csv(all_appointments, file= "./all_appointments.csv" , row.names=FALSE)
historical_all_appst <- read.csv("./all_appointments.csv",stringsAsFactors = FALSE)
colnames(historical_all_appst)
# Functions ----
build_curveclimb <- function (xpathtable,range) {
    curveclimb <-bind_rows(lapply (range, function(i){
    cat (i)
    url <- links[i]
    xpath <- paste0('//*[@id="mw-content-text"]/div/table[' , xpathtable, ']')
    cat(url,"\n")
    read_html(url) %>%
      html_nodes(xpath=xpath) %>%
      html_table(fill = TRUE) %>% as.data.frame() %>% mutate(appointed_by = gsub("_"," ",presadentz[i]),
                                                             pres_start_date = as.Date(ISOdate(prez_start_year[i], 1, 1))) %>%
      clean_names() %>% mutate() %>%
      rename(c('active_service'='active','chief_judge'='chief','senior_status'='senior','x'='Judge#')) 
  }) 
  )
    curveclimb <- rename(curveclimb, c("X."="Judge#","Term.of.service"="Active","Term.of.service.1"="Chief","Term.of.service.2"="Senior"))
  return (curveclimb)
}








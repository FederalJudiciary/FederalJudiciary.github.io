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


# Links Setup -------------------------------------------------------------

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


 ## New Tables  Rebuild -------

#pres0 <- build_curveclimb(4,c(1:2))

## Get Latest Current -------

pres0 <- build_curveclimb(4,c(2))
  str(pres0)        
  names(pres0)
# pres1 <- build_curveclimb(3,c(3:6))  
#   names(pres1)

  
  ## Get Carter -------
  
  ###  Old Tables
  ### carter has different format than the rest
# pres20 <- build_curveclimb(2,c(7,10))  
# colnames(pres20)
#pres21 <- cbind("Judgenum" = with(pres20, ave(as.numeric(rownames(pres20)),appointed_by, FUN = seq_along))  , pres20)  
# 
# pres2 <-  cbind(pres20[,1:5],
#               # nomination_date=NA,
#               # confirmation_date=NA,
#                confirmation_vote=NA,
#                 pres20[,6:10]
#                 )

#colnames(pres2)[3] <- "court_note_1"
#  tail(pres2) 
  # names(pres2) 
  # nrow(pres2)


  
  ## Get Other Old Guys -------
  ### These are all the old guys
# pres31 <- build_curveclimb(3,c(8:9,11:14)) 
# colnames(pres31)
#pres31 <- cbind("Judge#" = with(pres30, ave(as.numeric(rownames(pres30)),appointed_by, FUN = seq_along))  , pres30)  
#colnames(pres31)
# pres3 <- cbind(pres31[,1:5],
#             #nomination_date=NA,
#             #confirmation_date=NA,
#             confirmation_vote=NA,
#             pres31[,6:10])
# tail(pres3)
# names(pres3)



# Begin Processing Current or rebuils -------------------------------------
names(pres0)
# names(pres1)
# names(pres2)
# names(pres3)

pres_district <- pres0
#pres_district<- rbind(pres0,pres1 ,pres2,pres3)
unique(pres_district$appointed_by)          
head(pres_district)
colnames(pres_district)

pres_district$start_active_date <- as.Date(substr((sapply(strsplit(pres_district$began_active_service,"-0000"), `[`, 1)),9,19))
pres_district$end_active_date <- gsub("Incumbent",paste0("00000000",Sys.Date()),pres_district$ended_active_service)
pres_district$end_active_date <- substr((sapply(strsplit(pres_district$end_active_date,"-0000"), `[`, 1)),9,19)
pres_district$end_active_date <- as.Date(substr(pres_district$end_active_date,1,10))
pres_district$end_senior_date <- gsub("Incumbent",paste0("00000000",Sys.Date()),pres_district$ended_senior_status)
pres_district$end_senior_date <- gsub('0 !',"000000004712-12-31",pres_district$end_senior_date)
pres_district$end_senior_date <- substr((sapply(strsplit(pres_district$end_senior_date,"-0000"), `[`, 1)),9,19)
pres_district$end_senior_date <- as.Date(substr(pres_district$end_senior_date,1,10))
pres_district$nomination_date <- gsub("Incumbent",paste0("00000000",Sys.Date()),pres_district$nomination_date)
pres_district$nomination_date <- gsub('0 !',"000000004712-12-31",pres_district$nomination_date)
pres_district$nomination_date <- substr((sapply(strsplit(pres_district$nomination_date,"-0000"), `[`, 1)),9,19)
pres_district$nomination_date <- as.Date(substr(pres_district$nomination_date,1,10))
pres_district$confirmation_date <- gsub("Incumbent",paste0("00000000",Sys.Date()),pres_district$confirmation_date)
pres_district$confirmation_date <- gsub('0 !',"000000004712-12-31",pres_district$confirmation_date)
pres_district$confirmation_date <- substr((sapply(strsplit(pres_district$confirmation_date,"-0000"), `[`, 1)),9,19)
pres_district$confirmation_date <- as.Date(substr(pres_district$confirmation_date,1,10))
pres_district$judge <- substr(pres_district[,c("judge")],1,nchar(pres_district[,c("judge")])/2+1)
pres_district$confirmation_vote <- (sapply(strsplit(pres_district$confirmation_vote,"!"), `[`, 2))
pres_district$court <- (sapply(strsplit(pres_district$court_note_1,"!"), `[`, 2))
pres_district$Judgenum <- as.numeric(ifelse(is.na(sapply(strsplit(pres_district$"Judge#","♠"), `[`, 2)),
                        pres_district$"Judge#",sapply(strsplit(pres_district$"Judge#","♠"), `[`, 2)))
pres_district$court_type <- "District"
pres_district$months_from_inauguration <- (interval(pres_district$pres_start_date,pres_district$start_active_date ) %/% months(1))
pres_district$'Judge#' <- NULL

head(pres_district)
colnames(pres_district)

# Produce Current matching historical  ------------------------------------

current_district_appointments <- pres_district[,c(15,1,3:5,9:14,16:17)]
colnames(current_district_appointments)
head(current_district_appointments)
nrow(current_district_appointments)
unique(current_district_appointments$appointed_by)


# write rebuild -----------------------------------------------------------

#write.csv(district_appointments, file= "./district_appointments.csv" , row.names=FALSE)


# Read historical ---------------------------------------------------------

hst_district_appointments <- read.csv("./district_appointments.csv",stringsAsFactors = FALSE)
colnames(hst_district_appointments)
# Read filter current from historical ---------------------------------------------------------
hst_district_appointments <- hst_district_appointments[!hst_district_appointments$appointed_by=='Donald Trump',]
unique(hst_district_appointments$appointed_by)

###  Combine historical and current district appointments  -------------------
final_district_appointments <- rbind(hst_district_appointments,current_district_appointments)
unique(final_district_appointments$appointed_by)

# Write current + historical ---------------------------------------------------------

write.csv(final_district_appointments, file= "./district_appointments.csv" , row.names=FALSE)


# Functions ---------------------------------------------------------------


#i<-10
#xpathtable <-3 
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
      clean_names() %>% 
      rename(c('active_service'='active','chief_judge'='chief','senior_status'='senior','x'='Judgenum','court_note_1'='circuit')) 
  }) 
  )
    curveclimb <- rename(curveclimb, c("X."="Judgenum","Term.of.service"="Active","Term.of.service.1"="Chief","Term.of.service.2"="Senior"))
  return (curveclimb)
}






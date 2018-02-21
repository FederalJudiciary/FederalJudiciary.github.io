register_google(key = 'AIzaSyAjgxr9cFSYAcfSsC9JoySyPxrpuPpkccg')
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

setwd("C:/cygwin64/home/659163/GitHub/FederalJudiciary.github.io")

#  Get list of district courts --------------------------------------------

# districtcourtsxpath <- '//*[@id="mw-content-text"]/div/table[2]'
# url <- 'https://en.wikipedia.org/wiki/List_of_United_States_district_and_territorial_courts'
# cat(url,"\n")
# dcourts <-
#   read_html(url) %>%
#     html_nodes(xpath=districtcourtsxpath) %>%
#     html_table(fill = TRUE) %>% as.data.frame()
# 
# 
#  head(dcourts)
# 
# dcourts_wrk <- as.data.frame(matrix(unlist(strsplit(dcourts$Region,"\\!")), ncol=2, byrow=TRUE))
# dcourts_list <-unique(gsub("\\s","_",dcourts_wrk$V2 )  )
# head(dcourts_list)

# Build Links -------------------------------------------------------------


#links <- c(paste0('https://en.wikipedia.org/wiki/United_States_District_Court_for_the_',dcourts_list))

#dcourts<- c(dcourts)


# Table 2 xpath -----------------------------------------------------------


# dcircut_judges1 <- build_dcourt(3,c(13,25,27,46,53,66,68,72))

 str(dcircut_judges1)
# Table 3 xpath -----------------------------------------------------------
 
# dcircut_judges2 <- build_dcourt(2,c(1:12,14:21,23:24,26,28:45,47:52,54:61,63:65,67,69:71,73:84,86:94))
 str(dcircut_judges2)


# Combine xpat varietals --------------------------------------------------
 # dcircuit <- rbind(dcircut_judges2,dcircut_judges1)
 # dcircuit <- dcircuit[dcircuit$title !='Title',]
 # str(dcircuit)



# Retired ---------------------------------------------------------------
###########  RETIRED JUDGES
# 
# xpath_tbl_ret <- c(3,4,4,4,4,4,4,4,5,4,4,3,4)
# xpath_ret <- paste0("//*[@id=\"mw-content-text\"]/div/table[",xpath_tbl_ret,"]")
# circuit_ret1 <-''
# circuit_ret1 <-bind_rows(
#   wigga <-  lapply (c(1:13), function(i){
#     cat (i)
#     url <- links[i]
#     cat(url,"\n")
#     read_html(url) %>%
#       html_nodes(xpath=xpath_ret[i]) %>%
#       html_table(fill = TRUE) %>% as.data.frame() %>% mutate(circuit=paste0(dcourts[i]," Circuit"),"X."=as.character(.$X.)) %>%
#       filter(State != 'State') %>%
#       clean_names() %>%
#       rename(c('active_service'='active','chief_judge'='chief','senior_status'='senior','x'='Judge#')) 
#   })
# )
# circuit_ret1$judge <- substr(circuit_ret1[,c("judge")],1,nchar(circuit_ret1[,c("judge")])/2+1)
# 
# pos <- ifelse(regexpr('\\,', circuit_ret1$appointed_by)==-1,
#               nchar(circuit_ret1$appointed_by),
#               regexpr('\\,', circuit_ret1$appointed_by)-1)
# 
# circuit_ret1$appointed_by <- substr(circuit_ret1[,c("appointed_by")],1,pos)
# head(circuit_ret1)
# 
# unique(circuit_ret1[,c("circuit","appointed_by")])
# 
# 
# 
# Vacancies ---------------------------------------------------------------
# #no vacancies on the Federal Court
# #
# circuit_vac1
# 
# circuit_vac2 <-bind_rows(
#   wigga <-  lapply (c(3,4,5,6,11), function(i){
#     cat (i)
#     url <- links[i]
#     cat(url,"\n")
#     read_html(url) %>%
#       html_nodes(xpath=xpath_vac[i]) %>%
#       html_table(fill = TRUE) %>% as.data.frame() %>% mutate(circuit=paste0(dcourts[i]," Circuit"))
#   })
# )
# circuit_vac2
# 
# names(circuit_vac1) <- tolower(names(circuit_vac1))
# names(circuit_vac2) <- tolower(names(circuit_vac2))
# 
# vacancies_lkup <- rbind(circuit_vac2,circuit_vac1)


# Begin Cleanse -----------------------------------------------------------




dcircuit <- rename(dcircuit, c("term_of_service"="Active",
                               "term_of_service_1"="Chief",
                               "term_of_service_2"="Senior"))


#all <- dcircuit[dcircuit$state == 'ND',]
#sort(unique(dcircuit$duty_station))
 

# Start Processing with dcircuit -----------------------------------------
 
all <- dcircuit
colnames(all)

# Cleanse longitude and latitude ----------------------------------------------

#all <- dcircuit
#all %>% filter(state %like% "D.C." ) -> all
all$judge <- iconv(all$judge, "latin1", "ASCII", sub=NA)
all$judge <- ifelse(is.na(all$judge),'vacant',all$judge)
all$duty_station1 <- iconv(all$duty_station, "latin1", "ASCII", sub=NA)
all$duty_station1 <- gsub("\\[[^\\]]*\\]|Texas|CT|none|\\,|Connecticut","",all$duty_station1,perl=T)
all$duty_station1 <- gsub("Louis IL","Louis",all$duty_station1,perl=T)
all$duty_station1 <- gsub("Benton IL","Louis",all$duty_station1,perl=T)
all$duty_station1 <- gsub("Bowling GreenOwensboro","Owensboro",all$duty_station1,perl=T)
all$duty_station1 <- gsub("Bowling GreenLouisvillePaducah","Louisville",all$duty_station1,perl=T)
all$duty_station1 <- gsub("Alexandria /","",all$duty_station1,perl=T)
all$duty_station1 <- gsub("/Victoria","",all$duty_station1,perl=T)
all$duty_station1 <- gsub("/Brooklyn","",all$duty_station1,perl=T)
all$duty_station1 <- gsub("/Houston","",all$duty_station1,perl=T)
all$duty_station1 <-  ifelse(all$state=='ND','Bismarck',all$duty_station1)
all$duty_station1 <-  ifelse(all$judge=='David Alan Ezra','Honolulu',all$duty_station1)
all$duty_station1 <-  ifelse(all$judge %like% 'Sippel' & all$title == 'District Judge','Kansas City',all$duty_station1)
all$duty_station1 <-  ifelse(all$judge %like% 'Brian C. Wimes','St. Louis',all$duty_station1)
all$duty_station1 <-  ifelse(all$judge %like% 'Nanette K. Laughrey','St. Louis',all$duty_station1)
all$duty_station1 <-  ifelse(all$judge %like% 'James H. Payne','Oklahoma City',all$duty_station1)
all$duty_station1 <- gsub("San Antonio/","",all$duty_station1,perl=T)
all$duty_station1 <- gsub("inactive",NA,trimws(all$duty_station1),perl=T)
all$state         <- ifelse(all$duty_station1 %like% 'Washington D.C.','D.C.',all$state)
all$state         <- ifelse(all$duty_station1 %like% 'Medford','Oregon',all$state)
all$duty_station1 <- ifelse(is.na(all$duty_station1),NA,trimws(paste0(all$duty_station1," ",all$state," USA")))
sort(unique(all$duty_station1))
duty_station1 <-  sort(unique(all$duty_station1))



# Geocode Longlat ---------------------------------------------------------

# longlat <- geocode(duty_station1)
# longlat <-cbind(duty_station1,longlat)
# names(longlat) <- c("duty_station1","lon","lat")
# head(longlat)
# write.csv(longlat,"./longlat.csv")


# Read longlat ------------------------------------------------------------
# 
# longlat <- read.csv("longlat.csv",stringsAsFactors = FALSE)
# head(longlat)
# longlat[order(-longlat$lon),]
# longlat[order(longlat$lat),]


# Final Join with summaries and geocode -----------------------------------

all %>% 
# filter(state=="OR" ) %>% 
  mutate(active_flg = ifelse(!judge %like% 'vacant' & (title %like% 'Chief' | title %like% 'District'),1,0))  %>%
  mutate(senior_flg = ifelse(title %like% 'Senior',1,0))  %>%
  mutate(vacant_flg = ifelse(judge %like% 'vacant' | is.na(judge),1,0))  %>%
  mutate(senior_inactive_flg = ifelse(judge %like% 'Senior' & duty_station %like% 'inactive',1,0)) %>%
  left_join(longlat, by = "duty_station1") %>%
  tidyr::fill(duty_station1,lon,lat) -> all

head(data.frame(all),50)
colnames(all)


# Fix Appointed by --------------------------------------------------------

pos <- ifelse(regexpr('\\,', all$appointed_by)==-1,
              nchar(all$appointed_by),
              regexpr('\\,', all$appointed_by)-1)

all$appointed_by <- substr(all[,c("appointed_by")],1,pos)
all$appointed_by <- gsub('G\\.H\\.W\\.Bush','G\\.H\\.W\\. Bush',all$appointed_by)
all$appointed_by <- gsub('G\\. W\\. Bush','G\\.W\\. Bush',all$appointed_by)
all$appointed_by <- gsub('^Johnson$','L\\. Johnson',all$appointed_by)
table(all$appointed_by)
all$appointed_by <- ifelse ( substr(all$Active,1,4)==1992 & all$appointed_by=="Bush",'G.H.W. Bush',all$appointed_by)
table(all$appointed_by)
all$appointed_by <- gsub('^Bush$','G\\.W\\. Bush',all$appointed_by)

head(all)

sort(all$judge)

unique(all[,c("appointed_by")])
unique(all[,c("circuit","appointed_by")])

head(all)


# Fix Judge Name ----------------------------------------------------------

listofdistricts <- c('Northern_District_of_Ohio',
  'Southern_District_of_Ohio',
  'Eastern_District_of_Tennessee',
  'Northern_District_of_Illinois',
  'Eastern_District_of_Michigan',
  'Eastern_District_of_California')


all$judge <- ifelse( all$circuit %in% listofdistricts & all$judge != 'vacant' & regexpr('\\,', all$judge)>0,
        substr(newly$judge,nchar(newly$judge)/2+2,nchar(newly$judge)),
        all$judge)

   

# Determine Gender --------------------------------------------------------


stigga <- strsplit(all$judge," ")
str(stigga)
gendertest <- lapply(1:length(stigga), function(i){
  ifelse(nchar(unlist(stigga[i])[[1]]) < 3,name <- unlist(stigga[i])[2],name <- unlist(stigga[i])[1])
})

gt <- data.frame(name = unlist(gendertest))
all <- cbind(all,gt)
str(all)

gender <- gender(unlist(gendertest), years = c(1925,1970), method = c("ssa", "ipums", "napp",
                                                                      "kantrowitz", "genderize", "demo"), countries = c("United States"))

gender %>% distinct(name, gender) %>% arrange(name, gender) -> gender_lkup

all %>% left_join(.,gender_lkup,by="name")   -> all


all[is.na(all$gender) & all$name != 'vacant',c("name","circuit","judge")]

malenames <-c("Haldane","Ketanji","Diarmuid","Lavenski","Amul","Stephanos","José","Raner",
              "André","Ortrie","Halil","Avern","Duggen","Gershwin","Ketanji","Legrome","Ancer","S.")
femalenames <- c("Amalya","Rogeriee","Landya","Rya","Petrese") 

all$gender[all$name %in% malenames] <- "male"
all$gender[all$name %in% femalenames] <- "female"
all[is.na(all$name) ,]$name <- "vacant"

colnames(all)

# Get Active LOS ---------------------------------
present <- c("present","present[5]")

active_dates <- data.frame(str_split_fixed(all$Active, "[[:punct:]]", 2),stringsAsFactors = FALSE)
colnames(active_dates) <- c("ActiveStartYear","ActiveEndYear")
active_dates$currActive [active_dates$ActiveEndYear %in% present ] <- "Y"
active_dates$currActive [!active_dates$ActiveEndYear %in% present ] <- "N"
active_dates$currActive [active_dates$ActiveEndYear=='' ] <- "V"
#active_dates$currActive <- ifelse(active_dates$judge=='vacant','V',active_dates$currActive)
active_dates$ActiveEndYear[active_dates$ActiveEndYear %in% present ] <- as.numeric(format(Sys.Date(), "%Y"))
active_dates$yrsActive <- (as.numeric(active_dates$ActiveEndYear)-as.numeric(active_dates$ActiveStartYear))
active_dates$yrsActive <- ifelse(!is.na(active_dates$yrsActive)==TRUE,active_dates$yrsActive,0)
active_dates

all <- cbind(all,active_dates)

# Get Chief LOS position ----------------------------------
chief_dates <- data.frame(str_split_fixed(all$Chief, "[[:punct:]]", 2),stringsAsFactors = FALSE)
colnames(chief_dates) <- c("ChiefStartYear","ChiefEndYear")
chief_dates$currChief [chief_dates$ChiefEndYear %in% present ] <- "Y"
chief_dates$currChief [!chief_dates$ChiefEndYear %in% present ] <- "N"
chief_dates$ChiefEndYear[chief_dates$ChiefEndYear %in% present ] <- as.character(format(Sys.Date(), "%Y"))
chief_dates$yrsChief <- (as.numeric(chief_dates$ChiefEndYear)-as.numeric(chief_dates$ChiefStartYear))
chief_dates$yrsChief <- ifelse(!is.na(chief_dates$yrsChief)==TRUE,chief_dates$yrsChief,0)
all <- cbind(all,chief_dates)

# Get Senior LOS ----------------------------------
senior_dates <- data.frame(str_split_fixed(all$Senior, "[[:punct:]]", 2),stringsAsFactors = FALSE)
colnames(senior_dates) <- c("SeniorStartYear","SeniorEndYear")
senior_dates$currSenior [senior_dates$SeniorEndYear %in% present ] <- "Y"
senior_dates$currSenior [!senior_dates$SeniorEndYear %in% present ] <- "N"
senior_dates$SeniorEndYear[senior_dates$SeniorEndYear %in% present ] <- as.character(format(Sys.Date(), "%Y"))
senior_dates$yrsSenior <- (as.numeric(senior_dates$SeniorEndYear)-as.numeric(senior_dates$SeniorStartYear))
senior_dates$yrsSenior <- ifelse(!is.na(senior_dates$yrsSenior)==TRUE,senior_dates$yrsSenior,0)
all <- cbind(all,senior_dates)


# Presidential Appointment ------------------------------------------------

pres_start_year <- c(0,1977,1993,1974,1989,2001,1961,1965,1969,2009,1981,2017)

appointed_by <- sort(unique(all$appointed_by))

pres_start_lkup <- data.frame(cbind(pres_start_year,appointed_by), stringsAsFactors = FALSE )

all %>%   left_join(.,pres_start_lkup,by="appointed_by")  %>%
  mutate(yrOfPresidency = as.numeric(.$ActiveStartYear)-as.numeric(.$pres_start_year)+1) %>%
  dplyr::rename(Appointed.By = appointed_by  ) -> all

colnames(all)
nrow(all)

# Appeals Court -----------

appealsLkp <-read.csv("./district_appeals_supreme.csv", stringsAsFactors = FALSE)
colnames(appealsLkp)
all %>%   left_join(.,clean_names(appealsLkp),by="state")  -> all


# Write Final File --------------------------------------------------------


write.csv(all, file= "./districtjudges.csv" , row.names=FALSE)

colnames(all)

##################  FUNCTIONS  ########

build_dcourt <- function (xpathtable,range) {
  #some states have more than one court
  state  <- c(rep('AL',3),"AK","AZ",rep('AR',2),rep('CA',4),"CO","CT","DE","D.C.",rep("FL",3),rep("GA",3),
              "GUAM","HI","ID",rep("IL",3),rep("IN",2),rep("IA",2),"KS","KY","KY",rep("LA",3),"ME","MD","MA",
              "MI","MI","MN","MS","MS","MO","MO","MT","NE","NV","NH", "NJ","NM",rep("NY",4),rep("NC",3),"ND","MARIANA","OH","OH",
              rep("OK",3),"OR",rep("PA",3),"PR","RI","SC","SD",rep("TN",3),rep("TX",4),"UT","VT","VI","VA","VA","WA","WA","WV","WV",
              "WI","WI","WY")
  
  dcircuit <-bind_rows(lapply (range, function(i){
    cat (i)
    url <- links[i]
    xpath <- paste0('//*[@id="mw-content-text"]/div/table[' , xpathtable, ']')
    cat(url,"\n")
    read_html(url) %>%
      html_nodes(xpath=xpath) %>%
      html_table(fill = TRUE) %>% as.data.frame() %>% mutate(circuit=paste0(dcourts_list[i]),state=state[i]) %>%
      clean_names() %>%
      plyr::rename(c(active_service='active',chief_judge='chief','senior_status'='senior','x'='Judge#')) 
  }) 
  )
  dcircuit <- plyr::rename(dcircuit, c("X."="Judge#","Term.of.service"="Active","Term.of.service.1"="Chief","Term.of.service.2"="Senior"))
  return (dcircuit)
}




  
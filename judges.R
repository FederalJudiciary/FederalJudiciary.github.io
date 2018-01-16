

library(rvest)
library(XML)
library(dplyr)
library(foreach)
library(plyr)
library(gender)
library(stringr)
library(janitor)
library(lubridate)




pres_start_year <- c(0,0,1977,1977,1993,1993,1974,1974,1989,2001,1969,2009,1981,2017)

Appointed.by <- sort(unique(all$Appointed.by))

pres_start_lkup <- data.frame(cbind(pres_start_year,Appointed.by), stringsAsFactors = FALSE )

courts <-c("First","Second","Third","Fourth","Fifth","Sixth","Seventh","Eighth","Ninth", "Tenth","Eleventh","Federal")

links <- c(paste0('https://en.wikipedia.org/wiki/United_States_Court_of_Appeals_for_the_',courts,'_Circuit'))
courts<- c(courts,"DC")
links <- c(links,"https://en.wikipedia.org/wiki/United_States_District_Court_for_the_District_of_Columbia")

xpath_tbl <- c(2,2,2,2,2,2,2,2,3,2,2,2,2)
xpath <- paste0("//*[@id=\"mw-content-text\"]/div/table[",xpath_tbl,"]")



current_population <-''
population <- data.frame()
circuit <-bind_rows(lapply (1:length(courts), function(i){
  cat (i)
  url <- links[i]
  cat(url,"\n")
  read_html(url) %>%
    html_nodes(xpath=xpath[i]) %>%
    html_table(fill = TRUE) %>% as.data.frame() %>% mutate(circuit=paste0(courts[i]," Circuit")) %>%
    filter(Title != 'Title') 
}) 
)
circuit <- rename(circuit, c("X."="Judge#","Term.of.service"="Active","Term.of.service.1"="Chief","Term.of.service.2"="Senior"))
str(circuit)
i<-4


###########  RETIRED JUDGES

xpath_tbl_ret <- c(3,4,4,4,4,4,4,4,5,4,4,3,4)
xpath_ret <- paste0("//*[@id=\"mw-content-text\"]/div/table[",xpath_tbl_ret,"]")
circuit_ret1 <-''
circuit_ret1 <-bind_rows(
    wigga <-  lapply (c(1:13), function(i){
        cat (i)
        url <- links[i]
        cat(url,"\n")
        read_html(url) %>%
          html_nodes(xpath=xpath_ret[i]) %>%
          html_table(fill = TRUE) %>% as.data.frame() %>% mutate(circuit=paste0(courts[i]," Circuit"),"X."=as.character(.$X.)) %>%
          filter(State != 'State') %>%
          clean_names() %>%
          rename(c('active_service'='active','chief_judge'='chief','senior_status'='senior','x'='Judge#')) 
      })
  )
circuit_ret1$judge <- substr(circuit_ret1[,c("judge")],1,nchar(circuit_ret1[,c("judge")])/2+1)

pos <- ifelse(regexpr('\\,', circuit_ret1$appointed_by)==-1,
              nchar(circuit_ret1$appointed_by),
              regexpr('\\,', circuit_ret1$appointed_by)-1)

circuit_ret1$appointed_by <- substr(circuit_ret1[,c("appointed_by")],1,pos)
head(circuit_ret1)

unique(circuit_ret1[,c("circuit","appointed_by")])



#####Vacancies
#no vacancies on the Federal Court
#
xpath_tbl_vac <- c(3,3,3,3,3,3,3,3,4,3,3,0,3)
xpath_vac <- paste0("//*[@id=\"mw-content-text\"]/div/table[",xpath_tbl_vac,"]")
circuit_vac <-''
i<-2
circuit_vac1 <-bind_rows(
  wigga <-  lapply (c(2,3,4,5,6,7,8,9,10,11,13), function(i){
    cat (i)
    url <- links[i]
    cat(url,"\n")
    read_html(url) %>%
      html_nodes(xpath=xpath_vac[i]) %>%
      html_table(fill = TRUE) %>% as.data.frame() %>% mutate(circuit=paste0(courts[i]," Circuit")) %>%
      clean_names() 
  })
)
circuit_vac1

circuit_vac2 <-bind_rows(
  wigga <-  lapply (c(3,4,5,6,11), function(i){
    cat (i)
    url <- links[i]
    cat(url,"\n")
    read_html(url) %>%
      html_nodes(xpath=xpath_vac[i]) %>%
      html_table(fill = TRUE) %>% as.data.frame() %>% mutate(circuit=paste0(courts[i]," Circuit"))
  })
)
circuit_vac2

names(circuit_vac1) <- tolower(names(circuit_vac1))
names(circuit_vac2) <- tolower(names(circuit_vac2))

vacancies_lkup <- rbind(circuit_vac2,circuit_vac1)



######################



all <- circuit


########   Determine Gender
stigga <-strsplit(all$Judge," ")
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
str(all)

all[is.na(all$gender),]

malenames <-c("Haldane","Ketanji","Diarmuid","Lavenski","Amul","Stephanos","José")
femalenames <- c("Amalya","Rogeriee") 

all$gender[all$name %in% malenames] <- "male"
all$gender[all$name %in% femalenames] <- "female"

all[is.na(all$gender),]
###########
######  Get Length of Service in each position

present <- c("present","present[5]")

active_dates <- data.frame(str_split_fixed(all$Active, "[[:punct:]]", 2),stringsAsFactors = FALSE)
colnames(active_dates) <- c("ActiveStartYear","ActiveEndYear")
active_dates$currActive [active_dates$ActiveEndYear %in% present ] <- "Y"
active_dates$currActive [!active_dates$ActiveEndYear %in% present ] <- "N"
active_dates$ActiveEndYear[active_dates$ActiveEndYear %in% present ] <- as.character(format(Sys.Date(), "%Y"))
active_dates$yrsActive <- (as.numeric(active_dates$ActiveEndYear)-as.numeric(active_dates$ActiveStartYear))
active_dates$yrsActive <- ifelse(!is.na(active_dates$yrsActive)==TRUE,active_dates$yrsActive,0)
active_dates

all <- cbind(all,active_dates)


chief_dates <- data.frame(str_split_fixed(all$Chief, "[[:punct:]]", 2),stringsAsFactors = FALSE)
colnames(chief_dates) <- c("ChiefStartYear","ChiefEndYear")
chief_dates$currChief [chief_dates$ChiefEndYear %in% present ] <- "Y"
chief_dates$currChief [!chief_dates$ChiefEndYear %in% present ] <- "N"
chief_dates$ChiefEndYear[chief_dates$ChiefEndYear %in% present ] <- as.character(format(Sys.Date(), "%Y"))
chief_dates$yrsChief <- (as.numeric(chief_dates$ChiefEndYear)-as.numeric(chief_dates$ChiefStartYear))
chief_dates$yrsChief <- ifelse(!is.na(chief_dates$yrsChief)==TRUE,chief_dates$yrsChief,0)
all <- cbind(all,chief_dates)


senior_dates <- data.frame(str_split_fixed(all$Senior, "[[:punct:]]", 2),stringsAsFactors = FALSE)
colnames(senior_dates) <- c("SeniorStartYear","SeniorEndYear")
senior_dates$currSenior [senior_dates$SeniorEndYear %in% present ] <- "Y"
senior_dates$currSenior [!senior_dates$SeniorEndYear %in% present ] <- "N"
senior_dates$SeniorEndYear[senior_dates$SeniorEndYear %in% present ] <- as.character(format(Sys.Date(), "%Y"))
senior_dates$yrsSenior <- (as.numeric(senior_dates$SeniorEndYear)-as.numeric(senior_dates$SeniorStartYear))
senior_dates$yrsSenior <- ifelse(!is.na(senior_dates$yrsSenior)==TRUE,senior_dates$yrsSenior,0)
all <- cbind(all,senior_dates)


all %>% 
  left_join(.,pres_start_lkup,by="Appointed.by")  %>%
  mutate(yrOfPresidency = as.numeric(.$ActiveStartYear)-as.numeric(.$pres_start_year)) -> all

write.csv(all, file= "C:/Users/659163/Desktop/judges.csv" , row.names=FALSE)






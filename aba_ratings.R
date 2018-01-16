
library(ggmap) 
library(rvest)
library(XML)
library(dplyr)
library(foreach)
library(plyr)
library(gender)
library(stringr)
library(janitor)
library(pdftools)
library(pdftables)
library(jsonlite)
library(splitstackshape)
library(ghit)
library(rJava)
library(tabulizer)
library(tidyr)
Sys.setenv(JAVA_HOME='C:\\Program Files\\DbVisualizer\\jre')

prefix <- 'https://www.americanbar.org/content/dam/aba/'

report <- c(
  "migrated/2011_build/federal_judiciary/ratings101.authcheckdam.pdf",
  "migrated/2011_build/federal_judiciary/ratings102.authcheckdam.pdf",
  "migrated/2011_build/federal_judiciary/ratings103.authcheckdam.pdf",
  "migrated/2011_build/federal_judiciary/ratings104.authcheckdam.pdf",
  "migrated/2011_build/federal_judiciary/ratings105.authcheckdam.pdf",
  "migrated/2011_build/federal_judiciary/ratings106.authcheckdam.pdf",
  "migrated/2011_build/federal_judiciary/ratings107.authcheckdam.pdf",
  "migrated/2011_build/federal_judiciary/ratings108.authcheckdam.pdf",
  "migrated/2011_build/federal_judiciary/ratings109.authcheckdam.pdf",
  "migrated/2011_build/federal_judiciary/ratings110.authcheckdam.pdf",
  "migrated/2011_build/federal_judiciary/ratings111.authcheckdam.pdf",
  "uncategorized/2011/ratings112.authcheckdam.pdf",
  "uncategorized/GAO/WebRatingChart.authcheckdam.pdf",
  "uncategorized/GAO/WebRatingChart114.authcheckdam.pdf",
  "uncategorized/GAO/Web%20rating%20Chart%20Trump%20115.authcheckdam.pdf")

file <-paste0(prefix,report)
congress <- c(101:115)            
presidents <- c("H.W. Bush","H.W. Bush","Clinton","Clinton","Clinton","Clinton","W. Bush","W. Bush","W. Bush","W. Bush"
                ,"Obama","Obama","Obama","Obama","Trump")
range <- c(10:15)
range <- c(9)
z<-9

wehaveafile <- buildRatings(file,presidents,congress,c(1:8,10:11,13:15))

summary(wehaveafile)



buildRatings <- function(file,presidents,congress,range){
  ratings <-bind_rows(lapply (range, function(z){
    lst <- extract_tables(file[z]) 
    print(length(lst))
    print(z)
    range1 <- c(1:length(lst))
    lst <- lapply(range1,function(i){
              width <- dim(lst[[i]])[2]
              lst[[i]] <- if(dim(lst[[i]])[2] == 6) lst[[i]][,c(1,2,4,5)] else lst[[i]]
              lst[[i]] <- if( dim(lst[[i]])[2] == 5) lst[[i]][, -5] else lst[[i]]
              return(lst[[i]])
              })
    table <- do.call(rbind, lst)
    #tail(table)
    table1 <- as.data.frame(table,stringsAsFactors=FALSE) 
    table1$President <- presidents[z]
    table1$Congress <- congress[z]
    colnames(table1) <- c('Date_Nominated','Name_of_Nominee','Court_to_Which_Nominated','ABA_Rating',"President","Congress")
    table1

  }))
  return(ratings)
}
aba_ratings <-wehaveafile %>% 
  filter(!Date_Nominated %in% c('DATE','NOMINATED')) %>% 
  filter(!ABA_Rating %in% c(''))  %>% 
  mutate(.,Date_Nominated = ifelse(Date_Nominated=='',NA,.$Date_Nominated),
         Name_of_Nominee = ifelse(Name_of_Nominee=='',NA,.$Name_of_Nominee),
         Court_to_Which_Nominated = ifelse(Court_to_Which_Nominated=='',NA,.$Court_to_Which_Nominated)) %>%  
           fill(Date_Nominated,.direction="up")  %>% 
           fill(Name_of_Nominee,.direction="up")  %>% 
           fill(Court_to_Which_Nominated,.direction="up")  %>%
           group_by_at(vars(Date_Nominated,Name_of_Nominee,Court_to_Which_Nominated,President,Congress)) %>%
           summarize_all(paste, collapse=" ")  %>%  
           mutate(ABA_Rating = gsub('\\r','/',ABA_Rating)) %>%
           mutate(ABA_Rating = gsub(filters,'',ABA_Rating,ignore.case=TRUE)) %>%
           mutate(ABA_Rating = gsub(' or ','/',ABA_Rating)) %>%
           mutate(ABA_Rating = gsub('See rating 4/9/03','WQ',ABA_Rating))  %>%
           mutate(ABA_Rating = gsub('See rating 1/7/03','WQ',ABA_Rating))  %>%
           mutate(ABA_Rating = gsub('mmin/Q/NQ','Qm/NQmin',ABA_Rating))  %>%
           mutate(ABA_Rating = gsub('mmin/Q/WQ','Qm/WQmin',ABA_Rating))  %>%  
           mutate(ABA_Rating = gsub('mmin/WQ/Q','WQm/Qmin',ABA_Rating))  %>% 
          mutate(ABA_Rating = gsub('sm/min/Q/NQ','Qsm/NQmin',ABA_Rating))  %>% 
          mutate(ABA_Rating = gsub('sm/Q/NQ','Qsm/NQ',ABA_Rating))  %>%   
          mutate(ABA_Rating = gsub('sQm/NQmin','Qsm/NQmin',ABA_Rating))  %>%  
          mutate(ABA_Rating = gsub('sQm/WQmin','Qsm/WQmin',ABA_Rating))  %>%  
          mutate(ABA_Rating = gsub('sWQm/Qmin','WQsm/Qmin',ABA_Rating))  %>% 
          mutate(ABA_Rating = gsub('WQ  NQsm/Qmin','WQ/NQsm/Qmin',ABA_Rating))  %>%
          mutate(ABA_Rating = gsub('Qsm/','Qsm',ABA_Rating))  %>%
          mutate(ABA_Rating = gsub('\\(\\)','',ABA_Rating))  %>%
          mutate(ABA_Rating = gsub(' ','',ABA_Rating)) 
    sort(unique(aba_ratings$ABA_Rating))
         colnames(aba_ratings)
 
        listoffilters <-  list("/\\(1 recusal\\)",
                            "/\\(1 recusal plus/chair recused\\)",
                            "1 recusal",
                            " \\(1 abstention\\)",
                            " \\(  1 abstention\\)",
                            "minority abstained",
                            "2 recusals",
                            "/\\(Chair recused\\)",
                            "/\\(1 Recusal\\)",
                            "1 abstention",
                            "1abstention",
                            "abstainmin",
                            "\\(1NV\\)",
                            "\\(Chair Recused\\)",
                            "/\\(1 abstention\\)",
                            "\\(1 abstention\\)",
                            "Nom Withdrawn 1/23/08 NO RATING",
                            "2 abstentions",
                            "Chair recusal",
                            "/ minabstain",
                            "\\(3 abstention\\)",
                            "\\(\\)"
                          
                            
        )
           filters <-  paste(listoffilters,collapse='|')               
                        
          sort( unique(gsub(filters,'',aba_ratings$ABA_Rating,ignore.case=TRUE)))
            
    write.csv(aba_ratings, file= "C:/Users/659163/Desktop/master_aba_ratings.csv" , row.names=FALSE)

    
    
    
   







## Get list of district courts

districtcourtsxpath <- '//*[@id="mw-content-text"]/div/table[2]'
url <- 'https://en.wikipedia.org/wiki/List_of_United_States_district_and_territorial_courts'
cat(url,"\n")
dcourts <- 
  read_html(url) %>%
  html_nodes(xpath=districtcourtsxpath) %>%
  html_table(fill = TRUE) %>% as.data.frame() 


head(dcourts)

dcourts_wrk <- as.data.frame(matrix(unlist(strsplit(dcourts$Region,"\\!")), ncol=2, byrow=TRUE))
dcourts_list <-unique(gsub("\\s","_",dcourts_wrk$V2 )  )
head(dcourts_list)

links <- c(paste0('https://en.wikipedia.org/wiki/United_States_District_Court_for_the_',dcourts_list))

dcourts<- c(dcourts)

##Table 3 xpath
dcircut_judges1 <- build_dcourt(3,c(13,25,27,46,53,66,68,72))
str(dcircut_judges1)
##Table 2 xpath
dcircut_judges2 <- build_dcourt(2,c(1:12,14:21,23:24,26,28:45,47:52,54:61,63:65,67,69:71,73:84,86:94))
str(dcircut_judges2)

dcircuit <- rbind(dcircut_judges2,dcircut_judges1)
dcircuit <- dcircuit[dcircuit$title !='Title',]
str(dcircuit)




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
# #####Vacancies
# #no vacancies on the Federal Court
# #
# xpath_tbl_vac <- c(3,3,3,3,3,3,3,3,4,3,3,0,3)
# xpath_vac <- paste0("//*[@id=\"mw-content-text\"]/div/table[",xpath_tbl_vac,"]")
# circuit_vac <-''
# i<-2
# circuit_vac1 <-bind_rows(
#   wigga <-  lapply (c(2,3,4,5,6,7,8,9,10,11,13), function(i){
#     cat (i)
#     url <- links[i]
#     cat(url,"\n")
#     read_html(url) %>%
#       html_nodes(xpath=xpath_vac[i]) %>%
#       html_table(fill = TRUE) %>% as.data.frame() %>% mutate(circuit=paste0(dcourts[i]," Circuit")) %>%
#       clean_names() 
#   })
# )
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



######################

dcircuit <- rbind(dcircut_judges2,dcircut_judges1)
dcircuit <- dcircuit[dcircuit$title !='Title',]
str(dcircuit)

dcircuit <- rename(dcircuit, c("term_of_service"="Active",
                               "term_of_service_1"="Chief",
                               "term_of_service_2"="Senior"))


all <- dcircuit
colnames(all)

######  summarize districts

all %>% 
  group_by(circuit)  %>% 
  summarise(newvar = count(circuit)) %>%
  ungroup() -> circuit_total 

circuit_total<-circuit_total$newvar
names(circuit_total) <- c("circuit","district_count")


#### Get long and lat

duty_station <- unique(paste0(dcircuit$duty_station," ",dcircuit$state," USA"))
duty_station <- gsub("inactive|-","",duty_station) 


#longlat <- geocode(duty_station)
#longlat <-cbind(dcircuit$duty_station,longlat)
head(longlat,50)

names(longlat) <- c("duty_station1","lon","lat")
all %>% mutate(duty_station1 = paste0(dcircuit$duty_station," ",dcircuit$state," USA"),
               duty_station1 = gsub("inactive|-","",duty_station1)) %>%
  left_join(longlat, by = "duty_station1" ) %>%
  left_join(circuit_total, by = "circuit")-> all
head(data.frame(all),50)

#### FIX Appointed by

pos <- ifelse(regexpr('\\,', all$appointed_by)==-1,
              nchar(all$appointed_by),
              regexpr('\\,', all$appointed_by)-1)

all$appointed_by <- substr(all[,c("appointed_by")],1,pos)
all$appointed_by <- gsub('G\\.H\\.W\\.Bush','G\\.H\\.W\\. Bush',all$appointed_by)
all$appointed_by <- gsub('G\\. W\\. Bush','G\\.W\\. Bush',all$appointed_by)
table(all$appointed_by)
all$appointed_by <- ifelse ( substr(all$Active,1,4)==1992 & all$appointed_by=="Bush",'G.H.W. Bush',all$appointed_by)
table(all$appointed_by)
all$appointed_by <- gsub('^Bush$','G\\.W\\. Bush',all$appointed_by)
all[all$appointed_by=="Bush",]
all

sort(all$judge)

unique(all[,c("appointed_by")])
unique(all[,c("circuit","appointed_by")])
all[all$appointed_by=="Bush",]


####  FIX JUDGE NAME
listofdistricts <- c('Northern_District_of_Ohio',
                     'Southern_District_of_Ohio',
                     'Eastern_District_of_Tennessee',
                     'Northern_District_of_Illinois',
                     'Eastern_District_of_Michigan',
                     'Eastern_District_of_California')


all$judge <- ifelse( all$circuit %in% listofdistricts & all$judge != 'vacant' & regexpr('\\,', all$judge)>0,
                     substr(newly$judge,nchar(newly$judge)/2+2,nchar(newly$judge)),
                     all$judge)


########   Determine Gender
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

###########
######  Get Length of Service in each position

present <- c("present","present[5]")

active_dates <- data.frame(str_split_fixed(all$Active, "[[:punct:]]", 2),stringsAsFactors = FALSE)
colnames(active_dates) <- c("ActiveStartYear","ActiveEndYear")
active_dates$currActive [active_dates$ActiveEndYear %in% present ] <- "Y"
active_dates$currActive [!active_dates$ActiveEndYear %in% present ] <- "N"
active_dates$currActive [active_dates$ActiveEndYear=='' ] <- "V"
active_dates$currActive <- ifelse(active_dates$judge=='vacant','V',active_dates$currActive)
active_dates$ActiveEndYear[active_dates$ActiveEndYear %in% present ] <- as.numeric(format(Sys.Date(), "%Y"))
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

pres_start_year <- c(0,1977,1993,1974,1989,2001,1961,1965,1969,2009,1981,2017)

appointed_by <- sort(unique(all$appointed_by))

pres_start_lkup <- data.frame(cbind(pres_start_year,appointed_by), stringsAsFactors = FALSE )

all %>% 
  left_join(.,pres_start_lkup,by="appointed_by")  %>%
  mutate(yrOfPresidency = as.numeric(.$ActiveStartYear)-as.numeric(.$pres_start_year)+1) -> all

############3


write.csv(all, file= "C:/Users/659163/Desktop/districtjudges.csv" , row.names=FALSE)

colnames(all)

##################  FUNCTIONS  ########

build_dcourt <- function (xpathtable,range) {
  
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
      rename(c('active_service'='active','chief_judge'='chief','senior_status'='senior','x'='Judge#')) 
  }) 
  )
  dcircuit <- rename(dcircuit, c("X."="Judge#","Term.of.service"="Active","Term.of.service.1"="Chief","Term.of.service.2"="Senior"))
  return (dcircuit)
}






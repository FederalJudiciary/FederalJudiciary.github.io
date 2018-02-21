
my_sociomatrix <- matrix(round(runif(num_nodes*num_nodes)), # edge values
                         nrow = num_nodes, #nrow must be same as ncol
                         ncol = num_nodes)


install.packages("sna") -----------
install.packages("network")
trace(utils:::unpackPkgZip, edit=TRUE)
install.packages("GGally")
library(GGally)
devtools::install_github("briatte/ggnet")
library(ggnet)
library(network)
library(sna)
library(ggplot2)

?read.paj

bigga <- source("https://goo.gl/q1JFih")
x = cut_number(as.integer(net %v% "year"), 4)
col = c("#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2")
names(col) = levels(x)


ggnet2(net, color = x, color.legend = "period", palette = col,
       edge.alpha = 1/4, edge.size = "weight",
       size = "outdegree", max_size = 4, size.cut = 3,
       legend.size = 12, legend.position = "bottom") +
  coord_equal()


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
Sys.setenv(JAVA_HOME='C:\\Program Files\\DbVisualizer\\jre')
library(rJava)
library(tabulizer)
library(tidyr)
library(lubridate)
setwd("C:/cygwin64/home/659163/GitHub/FederalJudiciary.github.io")

buildRatings <- function(file,presidents,congress,range){
  ratings <-bind_rows(lapply (range, function(z){
    
    z <- c(9)
    lst <- extract_tables(file[z]) 
    print(length(lst))
    print(z)
    range1 <- c(1:length(lst))
    lst <- lapply(range1,function(i){
      width <- dim(lst[[i]])[2]
      lst[[i]] <- if(dim(lst[[i]])[2] == 6) lst[[i]][,c(1,2,4,5)] else lst[[i]]
      lst[[i]] <- if( dim(lst[[i]])[2] == 5) lst[[i]][, -5] else lst[[i]]
      lst[[i]] <- if( dim(lst[[i]])[2] == 3) lst[[i]][, -5] else lst[[i]]
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




ratings_hst <- buildRatings(file,presidents,congress,c(1:8,10:11,13:14))
unique(ratings_hst$ABA_Rating)
write.csv(ratings_hst, file= "./master_aba_ratings_raw_hst.csv" , row.names=FALSE)
ratings_hst <- read.csv( file= "./master_aba_ratings_raw_hst.csv" ,stringsAsFactors = FALSE)
head(ratings_hst)
unique(ratings_hst$ABA_Rating)

####  Current Version

ratings_curr <- buildRatings(file,presidents,congress,c(15))
head(ratings_curr)

combinedRatings <- rbind (ratings_hst,ratings_curr,congress109special_raw,congress112special_raw)

head(combinedRatings)
sort(unique(combinedRatings$ABA_Rating))

combinedRatings$date_prob_ind <- sapply(strsplit(combinedRatings$Date_Nominated,"\n"), `[`, 2)

aba_ratings <- combinedRatings %>% 
            filter(!trimws(Date_Nominated) %in% c('DATE','NOMINATED')) %>% 
            filter(!ABA_Rating %in% c(''))  %>% 
            mutate(asterisk_ind = grepl('\\*',Date_Nominated))  %>% 
            mutate(Date_Nominated = ifelse(!is.na(date_prob_ind),date_prob_ind,Date_Nominated)) %>%
            mutate(Date_Nominated = gsub('\\*','',Date_Nominated)) %>%
            mutate(Date_Nominated = gsub('\\(|\\)','',Date_Nominated)) %>%
            mutate(Date_Nominated = gsub('4/28/03','4/28/2003',Date_Nominated)) %>%
            mutate(Date_Nominated = gsub('\\?','',Date_Nominated)) %>%
            mutate(.,Date_Nominated = ifelse(Date_Nominated=='',NA,.$Date_Nominated),
            Name_of_Nominee = ifelse(Name_of_Nominee=='',NA,.$Name_of_Nominee),
            Court_to_Which_Nominated = ifelse(Court_to_Which_Nominated=='',NA,.$Court_to_Which_Nominated)) %>%  
            fill(Date_Nominated,.direction="up")  %>% 
            fill(Name_of_Nominee,.direction="up")  %>% 
            fill(Court_to_Which_Nominated,.direction="up")  %>%
            group_by_at(vars(Date_Nominated,Name_of_Nominee,Court_to_Which_Nominated,President,Congress)) %>%
            summarize_all(paste, collapse=" ")  %>%  
            mutate(ABA_Rating = gsub('\\n','/',ABA_Rating)) %>%
            mutate(ABA_Rating = gsub('\\*','/',ABA_Rating)) %>%
            mutate(ABA_Rating = gsub(filters,'',ABA_Rating,ignore.case=TRUE)) %>%
            mutate(ABA_Rating = gsub(' or ','/',ABA_Rating)) %>%
            mutate(ABA_Rating = gsub('See rating 4/9/03','WQ',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('See rating 1/7/03','WQ',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('mmin/Q/NQ','Qm/NQmin',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub(' ','',ABA_Rating)) %>%
            mutate(ABA_Rating = gsub('mminQ/WQmminQ/WQ','Qm/WQmin',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('mminQ/NQ','Qm/NQmin',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('mminQ/WQ','Qm/WQmin',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('mminWQ/Q','WQm/Qmin',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('mmin/Q/WQ','Qm/WQmin',ABA_Rating))  %>%  
            mutate(ABA_Rating = gsub('mmin/WQ/Q','WQm/Qmin',ABA_Rating))  %>% 
            mutate(ABA_Rating = gsub('sm/min/Q/NQ','Qsm/NQmin',ABA_Rating))  %>% 
            mutate(ABA_Rating = gsub('sm/Q/NQ','Qsm/NQ',ABA_Rating))  %>%   
            mutate(ABA_Rating = gsub('sQm/NQmin','Qsm/NQmin',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('sm min WQ  /Q','WQsm/Qmin',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('sQm/WQmin','Qsm/WQmin',ABA_Rating))  %>%  
            mutate(ABA_Rating = gsub('sWQm/Qmin','WQsm/Qmin',ABA_Rating))  %>% 
            mutate(ABA_Rating = gsub('WQ  NQsm/Qmin','WQ/NQsm/Qmin',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('^Qsm/$','Qsm',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('^sm$','Qsm/NQmin ',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub('\\(\\)','',ABA_Rating))  %>%
            mutate(ABA_Rating = gsub(' ','',ABA_Rating)) %>%
            mutate(ABA_Rating = gsub('^\\/+','',ABA_Rating)) %>% 
            mutate(ABA_Rating = gsub('WQsm/QminWQ','WQ',ABA_Rating)) %>%
            select("Date_Nominated",Name_of_Nominee,Court_to_Which_Nominated,President,Congress,ABA_Rating)
                        
    sort( unique(aba_ratings$ABA_Rating))
            
    write.csv(aba_ratings, file= "./master_aba_ratings.csv" , row.names=FALSE)
    
    
    filters <-  paste(listoffilters,collapse='|') 
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
    





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

    
    
    
   




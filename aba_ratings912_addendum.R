
    z <- 9
    lst_arch <- extract_tables(file[z]) 
    lst <- lst_arch
    print(length(lst))
    print(z)
    range1 <- c(3:6)
    lst1 <- lapply(range1,function(i){
      width <- dim(lst[[i]])[2]
      lst[[i]] <- if(dim(lst[[i]])[2] == 6) lst[[i]][,c(1,2,4,5)] else lst[[i]]
      lst[[i]] <- if( dim(lst[[i]])[2] == 5) lst[[i]][, -5] else lst[[i]]
      #      lst[[i]] <- if( dim(lst[[i]])[2] == 3) lst[[i]]$dummy <- NA
      return(lst[[i]])
    })
    table <- do.call(rbind, lst1)
    #tail(table)
    table1 <- as.data.frame(table,stringsAsFactors=FALSE) 
    table1$President <- presidents[z]
    table1$Congress <- congress[z]
    colnames(table1) <- c('Date_Nominated','Name_of_Nominee','Court_to_Which_Nominated','ABA_Rating',"President","Congress")

    congress109special_raw <- table1
    sort(unique(congress109special_raw$ABA_Rating))
    
    
    z <- 12
    lst_arch <- extract_tables(file[z]) 
    lst <- lst_arch
    print(length(lst))
    print(z)
    range1 <- c(1:4,6)
    lst1 <- lapply(range1,function(i){
      width <- dim(lst[[i]])[2]
      lst[[i]] <- if(dim(lst[[i]])[2] == 6) lst[[i]][,c(1,2,4,5)] else lst[[i]]
      lst[[i]] <- if( dim(lst[[i]])[2] == 5) lst[[i]][, -5] else lst[[i]]
      #      lst[[i]] <- if( dim(lst[[i]])[2] == 3) lst[[i]]$dummy <- NA
      return(lst[[i]])
    })
    table <- do.call(rbind, lst1)
    #tail(table)
    table1 <- as.data.frame(table,stringsAsFactors=FALSE) 
    table1$President <- presidents[z]
    table1$Congress <- congress[z]
    colnames(table1) <- c('Date_Nominated','Name_of_Nominee','Court_to_Which_Nominated','ABA_Rating',"President","Congress")
    table1
    sort(unique(table1$ABA_Rating))
    congress112special_raw <- table1
    
    
    
    library(pivottabler)
    qhpvt(bhmtrains, "TOC", "TrainCategory", "n()") 








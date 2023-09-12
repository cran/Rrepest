################# Test for over in Frequencies #################
# 1 May 2023
# Rodolfo Ilizaliturri
#############################################################


over.test.freq <- function(data, over) {
  # Goal: Create a dataframe only with by.var and statistic variable but with difference by over variables
  # ------ INPUTS ------.
  # data : (dataframe) df to analize
  # over : (vector string) columns over which to do analysis
  
  # Create by.var so not to interfere with other functions
  by.var <- colnames(data)[1 : which(colnames(data) == over[1])-1]
  
  # Group by all over and by variables and drop nas for over
  res.data <- data %>% 
    drop_na(all_of(over)) 
  
  
  
  # Pivot frequencie column to column names (freq.category1, freq.category2, ...)
  res.data <- res.data %>% 
    pivot_wider(values_from = "freq", names_from = over[length(over)])
  
  # Keep over as over.old
  over.old <- over
  # Create over variable without last entry (frequency variable name)
  over <- over[-length(over)]
  
  # Exctract colnames of numeric variables
  num.var <- colnames(res.data)[(which(colnames(res.data) == over[length(over)])+1) : ncol(res.data)]
  # Create a list of as many data frames as numeric variables with by.var and over in them
  l.dfs <- lapply(num.var, function(n.i){
    df.i <- res.data %>% select(all_of(c(by.var,over,n.i)))
    return(df.i)
  })
  
  
  # Assumption that all the combinations of "over" categories exist in database
  # for multiple variables when doing difference. IOW: That a difference can be made
  # for the last variable in over
  
  #Apply method to show test on list of l.dfs
  l.dfs <- lapply(l.dfs, function(res.d){
    
    if (length(over) > 1) {
      # If there is more than 1 over variable get how many values per group for the last
      # variable are there (Sprint 2.1.2)
      res.d <- res.d %>% 
        unite("over-l", all_of(over[-length(over)]),sep="|")
      
      n.diff <- res.d$"over-l" %>% unique() %>% length() - 1
      
      res.d <- res.d %>% 
        unite("over", 
              all_of(c(over[length(over)],"over-l")),
              sep = "|")
    }else{
      #If less or equal to 1 variable just rename
      n.diff <- 0
      res.d <- res.d %>% 
        rename("over"=over)
    }
    
    #Merge all by.var variables, do wider with over, and sort all of over alphabetically
    res.d <- res.d %>% 
      unite("by.group", all_of(by.var), sep = "|") %>%
      pivot_wider(names_from = "over", values_from = colnames(.)[length(.)]) %>%
      select("by.group",sort(colnames(.)[-1]))
    
    #Get difference for the first and last categories of over
    diffs <- res.d[2:(2+n.diff)]-res.d[(length(res.d)-n.diff):length(res.d)]
    
    #Get colnames for first and last categories of over
    c.names.a <- colnames(res.d)[2:(2+n.diff)]
    c.names.b <- colnames(res.d)[(length(res.d)-n.diff):length(res.d)]
    
    #Create names for difference columns on the format:
    #(1st - last)|rest
    c.names.diffs <- c()
    for (i in seq_along(c.names.a)) {
      if(n.diff > 0) {
        c.n <- paste0("(",sub("\\|.*$","",c.names.a[i]),"-",sub("\\|.*$","",c.names.b[i]),")",
                      "|",sub(".*?\\|","",c.names.a[i]))
        c.names.diffs <- c(c.names.diffs,c.n)
      }else{
        c.n <- paste0("(",sub("\\|.*$","",c.names.a[i]),"-",sub("\\|.*$","",c.names.b[i]),")")
        c.names.diffs <- c(c.names.diffs,c.n)
      }
    } 
    colnames(diffs) <- c.names.diffs
    res.d <- cbind(res.d,diffs)
    
    #Get long data frame with only by.var  column already with differences
    res.d <- res.d %>% 
      pivot_longer(cols = colnames(res.d)[-1] ,names_to = "over") %>% 
      unite("by.group",all_of(c("by.group","over")),sep = "|")
    
    #Send the value corresponding to last variable in over to the end of the name
    res.d[["by.group"]] <- apply(res.d,1, function(x) {
      c(strsplit(x,"\\|")[[1]],
        strsplit(x,"\\|")[[1]][(length(by.var)+1)])[-(length(by.var)+1)] %>% 
        paste(collapse = "|")
    })
    
    return(res.d)
    
  })  %>% 
    # Merge all dataframes by by.group
    reduce(full_join, by = "by.group")
  
  # Rename numeric columns
  colnames(l.dfs)[-1] <- num.var
  
  #Pivot longer each category from the frequency variable
  l.dfs <- l.dfs %>% 
    pivot_longer(cols = num.var,names_to = over.old[length(over.old)],values_to = "freq") %>% 
    unite(col = "by.group", c("by.group",over.old[length(over.old)]), sep = "|")
  
  return(l.dfs)
}

#' Rrepest
#' @description 
#' Estimates statistics using replicate weights (Balanced Repeated Replication (BRR) weights,
#' Jackknife replicate weights,...), thus accounting for complex survey
#' designs in the estimation of sampling variances. It is specially designed
#' to be used with the data sets produced by the Organization for Economic Cooperation and Development (OECD),
#' some of which include the Programme for International Student Assessment (PISA) and 
#' Teaching and Learning International Survey (TALIS) data sets, but works for all International Large Scale Assessments 
#' that use replicated weights.  It also allows for
#' analyses with multiply imputed variables (plausible values); where
#' plausible values are included in a pvvarlist, the average estimator
#' across plausible values is reported and the imputation error is added to
#' the variance estimator.

#' @param data (dataframe) df to analyze
#' @param svy (string) Possible projects to analyse.must be equal to ALL, IALS, 
#' IELS, PIAAC, PISA, PISA2015, PISAOOS, TALISSCH, TALISTCH .  
#' @param est (est function) that takes arguments stimate, target variable, regressor (optional for linear regressions)
#' @param by (string vector) column in which we'll break down results
#' @param over (vector string) columns over which to do analysis
#' @param test (bool) TRUE: will calculate the difference between over variables
#' @param user_na (bool) TRUE: show nature of user defined missing values for by.var
#' @param show_na (bool) TRUE: include na in frequencies of x
#' @param flag (bool) TRUE: Show NaN when there is not enough cases (or schools)
#' @param fast (bool) TRUE: Only do 6 replicated weights
#' @param average (grp function) that takes arguments group.name, column, cases to create averages at the end of df
#' @param group (grp function) that takes arguments group.name, column, cases to create groups at the end of df
#' @param tabl (bool) TRUE: Creates a flextable with all examples
#' @param ... Optional filtering parameters: i.e.:
#' isced = 2, 
#' n.pvs = 5, 
#' cm.weights = c("finw",paste0("repw",1:22))
#' var.factor = 1/(0.5^2)
#' z.score = qnorm(1-0.05/2)
#' 
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import labelled
#' @import parallel
#' @import foreach
#' @import doParallel
#' @import flextable
#' @import stringr
#' @import officer
#' @importFrom stats as.formula coefficients lm.wfit na.omit qnorm resid weighted.mean setNames
#' @importFrom utils combn head
#' @rawNamespace import(data.table, except = c(first,last,between,transpose))
#' @rawNamespace import(purrr, except = c(when,compose,accumulate))
#' @rawNamespace import(magrittr, except = c(set_names,extract))
#' 
#'
#' @return Dataframe containing estimation "b." and standard error "se." of desired processes
#' @export
#'
#' @examples
#' data(df_pisa18)
#' 
#' Rrepest(data = df_pisa18,
#' svy = "PISA2015",
#' est = est("mean","AGE"),
#' by = c("CNT"))
#' 
#' 
#' 
Rrepest <- function(data, svy, est, by = NULL, over = NULL,
                    test = FALSE, user_na= FALSE, show_na = FALSE, flag = FALSE, fast = FALSE,
                    tabl = FALSE, average = NULL, group = NULL, ...) {
  # Goal: Dataframe with statistics for TALIS
  # ------ INPUTS ------.
  ######### WHO ######### 
  # data : (dataframe) df to analyze
  # svy : List of possible projects to analyse TALISSCH and TALISTCH
  ######### WHAT ######### 
  # est : (est function) that takes arguments what = estimate, tgt = target, rgr = regressor
      # what : (string vector) accepts "mean", "mean pct","var","std", "quant", "iqr", "freq", "lm", "corr", "cov", "gen"
      # tgt : (string vector) variable from where to get frequencies or R script if selected "gen"
      # rgr : (string vector) independant variable for regression (1+)
  ######### WHERE ######### 
  # by.var : (string vector) column in which we'll break down results
  # over : (vector string) columns over which to do analysis
  ######### HOW ######### 
  # test : (bool) If TRUE will calculate the difference between over variables
  # user_na : (Bool) TRUE → show nature of user defined missing values for by.var
  # show_na : (Bool) TRUE → include na in frequencies of x 
  # flag : (Bool) TRUE → Show NaN when there is not enough cases (or schools)
  # fast : (Bool) TRUE → Only do 6 replicated weights\
  # group : (grp function) that takes arguments group.name, column, cases
  # table : (Bool) TRUE → Creates a flextable with all examples
  # ...
  # isced : (number) isced level to analyze
  # na_to_zero : (Bool) TRUE → will take NA as zero for the simple average calculation
  
  # Create a list of dfs for ewach conserning group
  if (!is.null(group)) {
    res.l <- lapply(names(group), function(g) {
      # Assign name of group to all the column value
      data %>% 
        # Filter only the desired cases
        filter(get(group[[g]][["column"]]) %in% group[[g]][["cases"]]) %>% 
        # Rename column values to that of the desired group
        mutate(!!group[[g]][['column']] := g) %>% 
        return()
    })
  } else 
    {res.l <- NULL}

  # All dataframes together (original + groups)
  res.l <- append(list(data),res.l)
  

# Apply Rrepest_core to each of those dataframes --------------------------
  res.l <- lapply(res.l, function(df.i){
    # Original Rrepest
    rrepest_base(data = df.i, 
                 svy = svy,
                 est = est,
                 by = by ,
                 over = over,
                 test = test,
                 user_na= user_na,
                 show_na= show_na, 
                 flag = flag,
                 fast = fast,
                 tabl = tabl, 
                 ...)
  }) %>% reduce(rbind)
  
  # If there is no by.var have an empty string in columns
  if (is.null(by)) by <-  "."
  # establish by.var as the corresponding the variable names in final results table
  
  res.l <- res.l %>% 
    separate(col = "by.var",into = by ,sep = "\\|") 
  
  # AVERAGES of results -----------------------------------------------------
  if (!is.null(average)) {
    res.l <- average_groups(res = res.l, group = average, by = by, ...)
  }
  
  #-------------- PRETTY TABLES --------------.
  if (tabl) {
    # Invert column names
    col_n <- names(res.l)
    col_n <- sapply(USE.NAMES = F, col_n, function(n_i) {
      # Replace first two instances of . with ..
      n_i <- n_i %>% 
        # First dot
        str_replace(pattern = "(?<!\\.)\\.(?!\\.)",
                    replacement =  "..") %>% 
        # Second dot
        str_replace(pattern = "(?<!\\.)\\.(?!\\.)",
                    replacement =  "..")
      
      # Split by .. 
      n_i <- n_i %>% 
        str_split(pattern = "\\..") %>% 
        unlist()
      
      # Replace b with B and se with SE
      if (n_i[1] == "b") {n_i[1] <- "B"}
      else if (n_i[1] == "se") {n_i[1] <- "SE"}
      
      # Invert order and paste back by sep = ..
      n_i <- n_i %>% 
        rev() %>% 
        paste0(collapse = "..")
      
      return(n_i)
    })
    names(res.l) <- col_n
    
    # Create flextable
    res.l <- res.l %>% 
      flextable()
    
    # Significant differences in tests -----------------------------------------
    # Check for z.score in arguments ...
    if (!is.null(list(...)$z.score)) {z <- list(...)$z.score}
    else {z <- qnorm(1 - .05/2)}
    
    res.l <- ft_significance(ft_i = res.l, z = z, statistic = est$what)

    # Combine columns of by vars ----------------------------------------------
    res.l <- res.l %>% 
      theme_vanilla() %>% 
      # Merge all adjacent except for the last one in by.var
      merge_v(j = as.formula(paste0(" ~ ",
                                    paste0(by[1:max(1,length(by)-1)], collapse = "+") ))) %>% 
      colformat_double(na_str = "..", nan_str = "a",
                       digits = 1) %>% 
      hline_bottom(part = "body")
    

    # Separate column names into multiple lines of header--------------------------------------------------
    res.l <- res.l %>% 
      separate_header(split = "\\..") %>%  
      align(align = "center", part = "header") %>% 
      autofit() %>% 
      bold(bold = TRUE, part = "header") %>% 
      vline(border = fp_border(color="gray", width=.5),
            part = "header") %>% 
      bg(bg = "#D9E1F2", part = "header")
    
  }
  
  #---------------------------------------.
  # Remove transitory Data Bases
  gc()
  return(res.l)
}


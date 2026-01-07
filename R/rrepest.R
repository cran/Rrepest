#' Estimation using replicate weights
#' 
#' @description 
#' Estimates statistics using replicate weights (Balanced Repeated Replication (BRR) weights,
#' Jackknife replicate weights,...), thus accounting for complex survey designs in the estimation
#' of sampling variances. It is designed specifically to be used with the data sets produced by
#' the Organization for Economic Cooperation and Development (OECD), some of which include the
#' Programme for the International Assessment of Adult Competencies (PIAAC), Programme for
#' International Student Assessment (PISA) and Teaching and Learning International Survey (TALIS)
#' data sets, but works for any educational large-scale assessment and survey that uses replicated
#' weights. It also allows for analyses with multiply imputed variables (plausible values);
#' where plausible values are used, average estimator across plausible values is reported and
#' the imputation error is added to the variance estimator.
#' 
#' @param data (data frame) Data to analyse
#' @param svy (string) Declares the survey settings. It must be equal to one of the following: ALL, IALS, ICCS, ICILS, IELS,
#' PBTS, PIAAC, PIRLS, PISA, PISAOOS, PISA2015, SSES, SSES2023, SVY, TALISSCH, TALISTCH, TALISEC_LEADER, TALISEC_STAFF, TIMSS.
#' @param est (est function) Specifies the estimates of interest. It has three arguments: statistics type, target variable and an (optional) regressor list in case of a linear regression.
#' @param by (string vector) Produces separate estimates by levels of the variable(s) specified by the string vector. 
#' @param over (string vector) Requests estimates to be obtained separately for each level of categorical variable(s) identified by the string vector.
#' @param test (bool) if TRUE: Computes the difference between estimates obtained for the lowest and highest values of the 'over' variable(s).
#' (See 'over' option above.) It is useful to test for differences between dependent samples (e.g. female-male).
#' @param invert_tests (bool) Invert test columns from Rrepest test = TRUE by name on "b." and "se." in the column name and by sign (*-1) on "b."
#' @param user_na (bool) if TRUE: Shows the nature of user defined missing values.
#' @param show_na (bool) if TRUE: Includes missing values (i.e. NAs) when estimating frequencies for the variable of interest.
#' @param flag (bool) if TRUE: Replaces estimation results that are based on fewer observations than required for reporting with NaN.
#' When used with the PIAAC survey settings, it checks if each estimation result is based on at least 30 observations.
#' When used with the PISA, PISAOOS, PISA2015 survey settings, it checks if each estimation result is based on at least 30 observations and 5 schools.  
#' When used with the TALISSCH survey settings, it checks if each estimation result is based on at least 10 schools.  
#' When used with the TALISTCH survey settings, it checks if each estimation result is based on at least 30 observations and 10 schools.
#' @param fast (bool) if TRUE: Computes estimates by using only 6 replicated weights.
#' @param average (grp function) Computes an arithmetic average (or weighted average). It has three arguments: name of the average, column/variable used for computing the average, rows/observations included in the average.
#' It has three arguments: name of the group, column/variable used for computing the group, rows/observations included in the group.
#' @param total (grp function) Computes an average weighted by the estimated size of the target population covered.
#' @param tabl (bool) if TRUE: Creates customisable and transferable tables using the flextable R package.
#' @param coverage (bool/numeric) TRUE: shows column next to se. Numeric: Shows NaN if bellow the set coverage.
#' @param se_zero2na (bool) TRUE: Masks SE of 0 as NA from a mean, meanpct, or freq with 1, 100, or 0 
#' @param save_arg (bool) TRUE: returns a named list with the estimation data frame and all arguments used in Rrepest.
#' @param cores (numeric) NULL: Will recruit max-1 cores when doing PVs. Else, will recruit the specified number of cores for PVs
#' @param ... Other optional parameters include:
#' isced = Filters the data used for analysis by ISCED level (e.g. isced = 2), 
#' n.pvs = Customizes the number of plausible values used in the estimation (e.g. n.pvs = 5),
#' cm.weights = Customizes the weights used in the estimation (e.g. cm.weights = c("finw",paste0("repw",1:22))),
#' var.factor = Customizes the variance factor used in the estimation (e.g. var.factor = 1/(0.5^2)),
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
#' @importFrom stats as.formula coefficients lm.wfit glm.fit na.omit qnorm resid weighted.mean setNames aggregate quasibinomial runif
#' @importFrom utils combn head
#' @rawNamespace import(data.table, except = c(first,last,between,transpose))
#' @rawNamespace import(purrr, except = c(when,compose,accumulate))
#' @rawNamespace import(magrittr, except = c(set_names,extract))
#' @rawNamespace import(rlang, except = c("flatten_lgl","splice","flatten_chr","flatten_raw","flatten","flatten_dbl","invoke","flatten_int",":=","%@%"))
#' 
#'
#' @return Data frame containing estimation "b." and standard error "se.".
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
                    tabl = FALSE, average = NULL, total = NULL, coverage = FALSE, invert_tests = FALSE,
                    save_arg = FALSE, cores = NULL, se_zero2na = FALSE,...) { #######group to total
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
  # user_na : (Bool) TRUE: show nature of user defined missing values for by.var
  # show_na : (Bool) TRUE: include na in frequencies of x 
  # flag : (Bool) TRUE: Show NaN when there is not enough cases (or schools)
  # fast : (Bool) TRUE: Only do 6 replicated weights\
  # table : (Bool) TRUE: Creates a flextable with all examples
  # coverage : (Numeric / Bool) Numeric [0,100]: If less than this percentage it will be marked as not having enough coverage
  #                             TRUE: Shows an extra cvg column with the coverage for that estimate
  # ...
  # isced : (number) isced level to analyze
  # na_to_zero : (Bool) TRUE: will take NA as zero for the simple average calculation
  
  # Turn data into tibble
  data <- data %>% as_tibble()
  
  # Save all arguments to list
  if (save_arg){
    argg <- c(as.list(environment()), list(...))
    argg[["data"]] <- NULL
  }
  
  # NAs in over variables are uninteresting as they will create a column of NAs, so must be removed
  for(over_i in over){
    if(!grepl("@",over_i)){ # Skip of @ in over
      data <- data %>% 
        filter(!is.na(get(over_i)))
    }
  }
  
  # Patch for TALISSCH: At least 50 that start with srwgt (2018 at. av)
  if(sum(grepl("^crwgt",names(data))) > 50){
    names(data)[names(data) == "schwgtc"] <- "schwgt"
    names(data)[names(data) %>% startsWith("crwgt")] <- paste0("s",substr(names(data)[names(data) %>% startsWith("crwgt")],2,10))
  }
  
  # Get na_to_zero from ...
  arguments <- list(...)

  # Create a list of dfs for each concerning group
  if (!is.null(total)) {
    res.l <- lapply(names(total), function(g) {
      # Assign name of group to all the column value
      data %>% 
        # Filter only the desired cases
        filter(get(total[[g]][["column"]]) %in% total[[g]][["cases"]]) %>% 
        # Rename column values to that of the desired group
        mutate(!!total[[g]][['column']] := g) %>% 
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
                 cores = cores,
                 ...)
  }) %>% reduce(rbind)
  
  
  
  # If there is no by.var have an empty string in columns
  if (is.null(by)) by <-  "."
  
  # establish by.var as the corresponding the variable names in final results table
  res.l <- res.l %>% 
    separate(col = "by.var",into = by ,sep = "\\|") 
  


  # Force NaN ---------------------------------------------------------------
  # Force NaNs from se. generated by the flag onto b.
  if(flag){
    nans_from_se2b(res.l)
  }
  

  # Coverage ----------------------------------------------------------------
  
  if (coverage != FALSE){
    # Get estimation specs
    what <- est$what
    tgt <- est$tgt
    rgr <- est$rgr
    
    # Patch for quantile table
    if ("quantiletable" %in% what) {
      # Create variable for flags and coverage "var1__var2"
      if(grepl("__",tgt)){
        # If found separete element into "y for order" and "y for mean""
        y_vars <- strsplit(tgt,"__")[[1]]
        y_4order <- y_vars[1]
        y_4mean <- y_vars[2]
        # Get the name of y for columns
        y_name <- paste0(y_4order,"__",y_4mean)
        tgt.i <- y_name
        # y_name will be multiplication of both to get the proper ammount of NAs
        data[[y_name]] <- data[[y_4order]] * data[[y_4mean]]
      }
    }
    # Plausible Values DIGITS--------------------------------------------------
    # If tgt or rgr has an @ then get digits of PVs
    if (any(grepl("@", c(tgt,rgr,by,over)))) {
      # Get all PV digits
      pv.digits <- digits.pvs(svy = svy, ...)
    } else {
      # if no @ the just iterate over 1 value
      pv.digits <- "@"
    }
    
    # Formatting depending on continuous or categorical tgt (assuming categorical target for all)
    df_covg <- format_data_repest(df = data,
                                  svy = svy,
                                  x = NULL, 
                                  # all arguments with an @ get converted into pvs
                                  by.over = get.pv.arguments(pv.digits, tolower(c(by,over, tgt))),
                                  user_na = user_na, 
                                  isced = is.there(arguments$isced),
                                  ...)
  }
  
  
  # If there is a number, show NaNs for where there is coverage problem
  if (is.numeric(coverage)){
    res.l <- coverage_column(data = df_covg, res = res.l, by = by, over = over,
                    est = est, svy = svy, pct = coverage)
  } else if (coverage){
    res.l <- coverage_column(data = df_covg, res = res.l, by = by, over = over,
                    est = est, svy = svy)
  }

  # AVERAGES of results -----------------------------------------------------
  if (!is.null(average)) {
    res.l <- average_groups(data = data, res = res.l, group = average, by = by,
                            over = over, est = est, svy = svy, user_na = user_na, ...)
  }

  # INVERT ALL TESTS --------------------------------------------------------
  if (invert_tests){
    # Test must exist to apply invert_tests
    if(test){
      # Iterate through all accurances of "b.est.(cat1-cat2)" name structure
      for(t_i in grep("^b\\..*\\(.*\\-.*\\)",names(res.l))){
        res.l <- inv_test(data = res.l, name_index = t_i)
      }
    }
  }

  # 0 SE for mean and freq --------------------------------------------------
  # SE of 0 get marked as an NA if meanpct/freq is 0 or 100 but zero is still used for averages
  if(se_zero2na){
    if("freq" %in% est$what){
      res.l <- se_to_na(res.l, mean_stat = FALSE)
    } else if (any(c("mean","means","meanpct","meanspct") %in% est$what)){
      res.l <- se_to_na(res.l, mean_stat = TRUE)
    }
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
  # If wanted to return arguments all will be put into a list with the resulting dataframe first
  if(save_arg){
    return(c(result=list(res.l),argg))
  } else {
    return(res.l)
  }
}


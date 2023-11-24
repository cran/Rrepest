ft_significance <- function(ft_i, z = qnorm(1 - .05/2), statistic){
  # ft_i (flextable) : flextable with b and se
  # z (numeric) : z-score for significance level
  # statistic (string) : statistic required from Rrepest (if "lm" do sig. diffs.)
  
  col_n <- ft_i$col_keys
  
  # Iterate over the indexes of col_n
  for (n_i in seq_along(col_n)) {
    if(any(c("gen","lm") %in% statistic)) { #If "lm" everything gets sig. diff. except R^2 and Intercept
      if (!grepl("Rsqr", col_n[n_i]) & # No R^2
          !grepl("Intercept", col_n[n_i]) & # No Intercept
          grepl(".*SE$", col_n[n_i])) {
        # Evaluate Statistically Significant rows
        sig_rows <- abs(ft_i$body$dataset[n_i-1]/ft_i$body$dataset[n_i])
        # Change flextable according to Statistical Significance
        ft_i <- ft_i %>% 
          color( i = sig_rows > z,
                 color = "blue",
                 j = n_i-1) %>% 
          bold ( i = sig_rows > z,
                 bold = TRUE,
                 j = n_i-1)
      }
    } else { #If no "lm" only tests get sig. diff.
      # If have structure _(_-_)_ and start with se (NOTE: the middle hyphen common hyphen)
      if (grepl(".*\\(.*\\-.*\\).*", col_n[n_i]) &
          grepl(".*SE$", col_n[n_i])) {
        # Evaluate Statistically Significant rows
        sig_rows <- abs(ft_i$body$dataset[n_i-1]/ft_i$body$dataset[n_i])
        # Change flextable according to Statistical Significance
        ft_i <- ft_i %>% 
          color( i = sig_rows > z,
                 color = "blue",
                 j = n_i-1) %>% 
          bold ( i = sig_rows > z,
                 bold = TRUE,
                 j = n_i-1)
      } 
    }
  }
  return(ft_i)
}

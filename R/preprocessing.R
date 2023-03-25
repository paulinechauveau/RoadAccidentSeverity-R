
set_numericals <- function(data, list_var){
  require(dplyr, install.packages('dplyr'))
  list_var <- unlist(list_var)
  data <- data %>% mutate_at(vars(matches(list_var)), as.integer)
  return(data)
}

set_factors <- function(data, list_var){
  require(dplyr, install.packages('dplyr'))
  list_var <- unlist(list_var)
  data <- data %>% mutate_at(vars(matches(list_var)), as.factor)
  return(data)
}

set_strings <- function(data, list_var){
  require(dplyr, install.packages('dplyr'))
  list_var <- unlist(list_var)
  data <- data %>% mutate_at(vars(matches(list_var)), as.character)
  return(data)
}

set_data <- function(data, var_numericals, var_factors, var_strings){
  if(length(var_numericals)!=0)
    data <- set_numericals(data, var_numericals)
  if(length(var_factors)!=0)
    data <- set_factors(data, var_factors)
  if(length(var_strings)!=0)
    data <- set_strings(data, var_strings)
  return(data)
}

del_vars <- function(data, vars){
  vars <- unlist(vars)
  data <- data %>% select(-vars)
  return(data)
}

get_missing <- function(df){ 
  variables <- names(df) 
  dtypes <- sapply(df, class)
  count <- sapply(df, length) 
  unique <- sapply(df, function(x) length(unique(na.omit(x)))) 
  missing <- sapply(df, function(x) sum(is.na(x))) 
  missing_prop <- round(missing/count*100, 2) 
  output <- data.frame(variable=variables, 
                       dtype=dtypes, 
                       count=count, 
                       unique=unique, 
                       missing=missing, 
                       missing_prop=missing_prop) 
  return(output) 
}

get_dtypes <- function(df){
  variables <- names(df) 
  dtypes <- sapply(df, class)
  return(data.frame(variable=variables, dtype=dtypes)) 
}

set_dtypes_df <- function(df){
  rownames(df) <- df$variable
  df <- df %>% select(-variable)
  df <- df %>% t()
  return(df)
}

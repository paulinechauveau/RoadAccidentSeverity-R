get_countplot <- function(data, categorical_var) {
  #' Plots a countplot
  #'
  #' @param data A data.frame object
  #' @param categorical_var A character object, colname of data

  ggplot(
    data,
    aes(x = data[, categorical_var], fill = data[, categorical_var])
  ) +
    geom_bar() +
    labs(x = categorical_var, fill = categorical_var)
}

get_boxplot <- function(data, categorical_var, numerical_var) {
  #' Plots a boxplot
  #'
  #' @param data A data.frame object
  #' @param categorical_var A character object, colname of data
  #' @param numerical_var A character object, colname of data
  
  ggplot(
    data,
    aes(
      x = data[, categorical_var],
      y = data[, numerical_var],
      colour = data[, categorical_var],
      fill = data[, categorical_var]
    )
  ) +
    geom_boxplot(
      alpha = 0.5,
      outlier.alpha = 0
    ) +
    geom_jitter(
      width = 0.25
    ) +
    stat_summary(
      fun = mean,
      colour = "black",
      geom = "point",
      shape = 18,
      size = 2
    ) +
    labs(
      x = categorical_var, y = numerical_var,
      colour = categorical_var, fill = categorical_var, 
      title = paste(categorical_var, "vs", numerical_var)
    )
}

get_multivar_plot <- function(data, target, var_quant, var_qual) {
  #' Multivariate plot
  #'
  #' @param data A data.frame object
  #' @param target A character object, colname of data
  #' @param var_quant A character object, colname of data
  #' @param var_qual A character object, colname of data
  
  ggplot(
    data,
    aes_string(
      x = var_quant,
      y = target, color = var_qual
    )
  ) +
    geom_point() +
    geom_smooth(method = "lm")
}

get_hist_and_density <- function(data, var, binwidth = 10) {
  #' Plots histogram and density
  #'
  #' @param data A data.frame object
  #' @param var A character object, colname of data
  #' @param binwidth
  
  ggplot(
    data,
    aes(x = data[, var])
  ) +
    geom_histogram(
      aes(y = ..density..),
      colour = 1,
      fill = "white",
      binwidth = binwidth
    ) +
    geom_density(alpha = .2, fill = "#FF6666") +
    labs(x = var, title = paste(var, "distribution"))
}

plot_cat_vs_cat <- function(data, x, y) {
  #' Jitterplot
  #'
  #' @param data A data.frame object
  #' @param x A character object, colname of data
  #' @param y A character object, colname of data

  ggplot(
    data,
    aes(
      x = data[, x],
      y = data[, y]
    )
  ) +
    geom_jitter(
      aes(color = data[, x]),
      width = 0.25
    ) +
    labs(
      x = x, y = y,
      colour = x, fill = x,
      title = paste(x, "vs", y)
    )
}
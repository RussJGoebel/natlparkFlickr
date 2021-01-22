#' Generate a parameter dataframe
#'
#' For a given dataframe of parks and popularity proxy, generates a data frame of parameters.
#'
#' @param parkdata A data frame of park visitation data in with columns as in the park_visitation dataset.
#' @param popularity_proxy A time series object representing the popularity of social media used for PUD.
#' @param use_ref_series A boolean specifying whether nps data is to be used for parameter estimates.
#' @param ... Additional arguments to be passed onto the visitation_model function.
#'
#' @export
#'

generate_parameter_df <- function(parkdata,popularity_proxy, use_ref_series = FALSE, ...){
  ###generates a dataframe with parameter estimates for different parks.

  parkname_vector <- unique(parkdata$park)
  nparks <- length(parkname_vector)

  beta <- numeric(nparks)
  lag_MSE <- numeric(nparks)
  lag_Rank <- numeric(nparks)
  lag_cc <- numeric(nparks)
  constants <- numeric(nparks)

  for(i in 1:nparks){


    park <- parkname_vector[i]
    pud_ts <- ts(parkdata[parkdata$park == park,]$pud, start = 2005, frequency = 12)
    pud_ts <- log(pud_ts)

    ref_series <- NULL
    if(use_ref_series){
      nps_ts <- ts(parkdata[parkdata$park == park,]$nps, start = 2005, frequency = 12)
      nps_ts <- log(nps_ts)
      ref_series <- nps_ts
    }

    vm_MSE <- visitation_model(pud_ts,popularity_proxy, criterion = "polynomial (MSE)", ref_series = ref_series, ...)
    vm_rank <- visitation_model(pud_ts,popularity_proxy, criterion = "polynomial (rank)", ref_series = ref_series, ...)
    vm_cc <- visitation_model(pud_ts,popularity_proxy, criterion ="cross-correlation", ref_series = ref_series, ...)

    beta[i] <- vm_cc$beta

    lag_MSE[i] <- vm_MSE$lag_estimate$lag
    lag_Rank[i] <- vm_rank$lag_estimate$lag
    lag_cc[i] <- vm_cc$lag_estimate$lag
    constants[i] <- vm_cc$constant

  }

  parameter_df <- data.frame(park = parkname_vector, lag_cc = lag_cc, lag_MSE = lag_MSE, lag_Rank = lag_Rank, beta = beta, constant = constants)

  parameter_df

}

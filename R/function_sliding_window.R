#' Sliding Window Results
#'
#' Sliding window used for model evaluation in *Modeling and Forecasting Percent Changes in National ParkVisitation Using Social Media*.
#'
#' @import VisitorCounts
#' @importFrom stats median predict ts window
#'
#' @param parkdata A dataframe matching the format of `park_visitation` from the VisitorCounts package.
#' @param popularity_proxy A time series matching the format of `flickr_userdays` from the VisitorCounts package.
#' @param num_windows  The number of windows to use in the sliding window analysis.
#' @param n_ahead An integer describing the number of forecasts to make in each window.
#' @param suspected_periods A vector which stores the suspected periods in the descending order of importance. The default option is c(12,6,4,3), corresponding to 12, 6, 4, and 3 months.
#' @param max_proportion_of_variance A numeric specifying the proportion of total variance explained using the method specified in proportion_of_variance_type. The default option is 0.995.
#' @param proportion_of_variance_type  A character string specifying the option for choosing the maximum number of eigenvalues based on the proportion of total variance explained. If "leave_out_first" is chosen, then the contribution made by the first eigenvector is ignored; otherwise, if "total" is chosen, then the contribution made by all the eigenvectors is considered.
#' @param log_ratio_cutoff A numeric specifying the threshold for the deviation between the estimated period and candidate periods in suspected_periods. THe default option is 0.2, which means that if the absolute log ratio between the estimated and candidate period is within 0.2 (approximately a 20 percent difference), then the estimated period is deemed equal to the candidate period.
#' @param model A character string specifiying the option for which model to assess. One of "pud_only" (uses only the pud data), "nps_assisted" (uses nps data for parameter estimates), "nps_only" (forecasts using SSA using just the nps data)
#' @param window_length A character string or positive integer specifying the window length for the SSA estimation. If "auto" is chosen, then the algorithm automatically selects the window length by taking a multiple of 12 which does not exceed half the length of time_series. The default option is "auto".
#' @param num_trend_components A positive integer specifying the number of eigenvectors to be chosen for describing the trend in SSA. The default option is 2.
#' @param trace A boolean specifying whether to print out an index whenever the sliding window is completed for a given park.
#' @param ... Additional arguments to be passed onto the visitation_model function.
#'
#' @return
#' \item{park_MAE_diff_mat}{Mean Absolute Error for forecasted differences in sliding window}
#' \item{park_RMSE_diff_mat}{Root-Mean-Square Error for forecasted differences in sliding window}
#' @export
#'



sliding_window_VisitorCounts <- function(parkdata,
                           popularity_proxy,
                           model = c("pud_only","nps_assisted","nps_only"),
                           lag_method = "cross-correlation",
                           num_windows = 48,
                           n_ahead = 12,
                           suspected_periods = c(12, 6, 4, 3),
                           proportion_of_variance_type = c("leave_out_first", "total"),
                           max_proportion_of_variance = 0.995,
                           log_ratio_cutoff = 0.2,
                           window_length = "auto",
                           num_trend_components = 2,
                           trace = TRUE,
                           ...){


  model <- match.arg(model)
  proportion_of_variacne_type <- match.arg(proportion_of_variance_type)
  lag_method <- match.arg(lag_method)


  popular_parks = unique(parkdata$park)
  trend_proxy <- popularity_proxy
  #Sliding window approach for model evaluation. The evaluation is done by comparing the performances of Model1 and Model2.
  #Model1: The model that does not use any of the NPS data at all.
  #Model2: The model that uses the NPS data to estimate the beta and constant terms.
  #The evaluations are done using RMSE and MAE of the differenced series (the pecentage change in the series).
  #For our reference, the r-squared values of the forecasts were also calculated.
  #In addition, for Model 2, visitation counts were also forecasted and compared.
  #In the code, whenever you see "mat1", that is for Model1. Similarly, for Model2, "mat2" is part of the variable name.
  #Also, "val" is for the visitation counts while "diff" is for the change in the visitation counts.
  #There is no "val" for Model 1 as the constant is assumed to be unknown.

  #par(mfrow = c(1,1))



  park_RMSE_diff_mat <- matrix(nrow = length(popular_parks),ncol = n_ahead)
  row.names(park_RMSE_diff_mat) <- popular_parks
  park_MAE_diff_mat <- matrix(nrow = length(popular_parks),ncol = n_ahead)
  row.names(park_MAE_diff_mat) <- popular_parks


  for(j in 1:length(popular_parks)){
    #for(j in 1:1){

    if(trace) print(j)

    park <- popular_parks[j]
    pud_ts <- ts(parkdata[parkdata$park == park,]$pud, start = 2005, frequency = 12)
    pud_ts <- log(pud_ts)

    nps_ts <- ts(parkdata[parkdata$park == park,]$nps, start = 2005, frequency = 12)
    nps_ts <- log(nps_ts)

    fordiffmat1 <- matrix(ncol = n_ahead, nrow = num_windows)
    absdiffmat1 <- matrix(ncol = n_ahead, nrow = num_windows)

    truediffmat <- matrix(ncol = n_ahead, nrow = num_windows)

    begin_year <- 2005
    for(i in 1:num_windows){
      pud_window <- window(pud_ts, start = begin_year+(i-1)/12,end = begin_year+(156-n_ahead-num_windows+i-1)/12)
      nps_window <- window(nps_ts, start = begin_year+(i-1)/12,end = begin_year+(156-n_ahead-num_windows+i-1)/12)
      nps_true_values <- window(nps_ts, start = begin_year+(156-n_ahead-num_windows+i-1)/12, end = begin_year+(156-n_ahead-num_windows+i+11)/12)
      trend_proxy_window <- window(trend_proxy, start = begin_year+(i-1)/12,end = begin_year+(156-n_ahead-num_windows+i-1)/12)
      true_differences <- diff(nps_true_values)

      truediffmat[i,] <- true_differences

      if(identical(model,"pud_only")){

      model1_fit <- visitation_model(pud_window,
                                     trend_proxy_window,
                                     num_trend_components = num_trend_components,
                                     criterion = lag_method,
                                     ...)

      forecasted_differences1 <- predict(model1_fit,n_ahead = 12, difference = TRUE, only_new = TRUE)$forecasts

      }

      if(identical(model,"nps_assisted")){

      model1_fit <- visitation_model(pud_window, trend_proxy_window,
                                     ref_series = nps_window,
                                     num_trend_components = num_trend_components,
                                     criterion = lag_method,
                                     ...)
      forecasted_differences1 <- predict(model1_fit,n_ahead = 12, difference = TRUE, only_new = T)$forecasts

      }




      if(identical(model,"nps_only")){

      ### Forecasting NPS using NPS:
      # SSA is used on the nps_window time series to generate n_ahead forecasts.
      # Groups are determined using the method in decomposition_function.


      nps_window_length <-  length(nps_window)
      ssa_window_parameter <-  (nps_window_length/2) %/% max(suspected_periods) * max(suspected_periods)


      #we use the same grouping as our automatic decomposition:
      grouping = auto_decompose(time_series=nps_window,
                                        suspected_periods=suspected_periods,
                                        proportion_of_variance_type=proportion_of_variance_type,
                                        max_proportion_of_variance=max_proportion_of_variance,
                                        log_ratio_cutoff=log_ratio_cutoff,
                                        window_length=ssa_window_parameter, #this should be the same as "auto".
                                        num_trend_components=num_trend_components)$grouping

      groups = list()
      groups[[1]] = c(1)
      for(period in 2:(length(suspected_periods)+1)){
        groups[[period]] =  which(grouping[,period-1] == 1)
      }

      nps_model <- Rssa::ssa(nps_window,L = ssa_window_parameter)


      groupwise_nps_model_forecasts <- Rssa::rforecast(nps_model, groups = groups, len = 12)

      nps_model_forecasts <- 0

      for(group in 1:length(groupwise_nps_model_forecasts)){
        nps_model_forecasts <- nps_model_forecasts+groupwise_nps_model_forecasts[[group]]
      }

      nps_model_forecasts_and_current <- c(nps_true_values[1],nps_model_forecasts)

      forecasted_differences1 <- diff(nps_model_forecasts_and_current)

      }

     #
      absdiff1 <- abs(forecasted_differences1-true_differences)

      fordiffmat1[i,] <- forecasted_differences1
      absdiffmat1[i,] <- absdiff1

      #plot(forecasted_differences1, col = "red")
      #lines(true_differences)
      #fordiffmat3[i,] <- diff(nps_model_forecasts_and_current)
      #forvalmat3[i,] <- nps_model_forecasts
      #absdiffmat3[i,] <-  abs(diff(nps_model_forecasts_and_current)-true_differences)


    }


    truefordiffmat1 <- rbind(truediffmat, fordiffmat1)


    park_RMSE_diff_mat[j,] <-sqrt(colMeans(absdiffmat1^2))


    park_MAE_diff_mat[j,] <-apply(absdiffmat1, 2, median)



  }

  # Results from the Sliding Window:

  list(park_MAE_diff_mat  =  park_MAE_diff_mat,
       park_RMSE_diff_mat =  park_RMSE_diff_mat)



}





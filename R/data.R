#' Popularity of Flickr, in user-days
#'
#' A time series representing the popularity of flickr in the United States, as measured in user-days. Here, user-days count the number of unique users posting on flickr on a given day.
#'
#' @format A time series object with 156 observations.
#' @source Flickr. (2019). Retrieved October, 2019, fromhttps://flickr.com/
#'
#'
#'
#'
"flickr_userdays"


#' National park visitation counts and associated Photo-User-Days data
#'
#' A data frame storing monthtly flickr photo-user-days (pud) for 20 popular US national parks.
#' Here, photo-user-days count the number of unique users posting a photo on flickr on a given day from within the boundaries of a given national park.
#'
#'
#' @format A data frame with 3276 rows and 3 variables.
#'
#' \describe{
#'   \item{date}{date of monthly observation, in year-month-day format.}
#'   \item{park}{national park alpha code identifying a national park.}
#'   \item{pud}{Flickr photo-user-days (pud).  Here, photo-user-days count the number of unique users posting a photo on flickr on a given day from within the boundaries of a given national park. }
#' }
#'
#' @source Flickr. (2019). Retrieved October, 2019, fromhttps://flickr.com/


"park_visitation"

#' natlparkFlickr: Datasets for national and park-level Flickr usage
#'
#' This package contains two datasets: flickr_userdays stores a time series of national level user-days for Flickr between 2005 and 2018, and park_visitation stores Flickr photo-user-days associated with twenty popular national parks in the United States. Here, user-days counts the number of unique Flickr users in a day, and photo-user-days counts the number of unique users who post a photo on Flickr from within the boundary of a given park.
#'
#' @section natlparkFlickr Datasets:
#'  \itemize
#'  \item{\code{flickr_userdays}}: A time series of national level user-days for Flickr between 2005 and 2018.
#'  \item{\code{park_visitation}}: A data frame storing Flickr photo-user-days associated with twenty popular national parks in the United States.
#'
#' @docType package
#' @name natlparkFlickr
NULL
#> NULL

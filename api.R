# api.R

#–– load required packages
library(httr)
library(jsonlite)
library(tibble)

#’ @title   Get all breeds
#’ @return  tibble with columns: breed (chr), sub_breeds (list)
get_all_breeds <- function() {
  res <- GET("https://dog.ceo/api/breeds/list/all")
  stop_for_status(res)
  parsed <- content(res, as = "parsed", simplifyVector = TRUE)
  tibble(
    breed      = names(parsed$message),
    sub_breeds = unname(parsed$message)
  )
}

#’ @title   Get sub-breeds for one breed
#’ @param   breed  Character, e.g. "hound"
#’ @return  character vector of sub-breeds (length zero if none)
get_sub_breeds <- function(breed) {
  all_b  <- get_all_breeds()
  sb_list <- all_b$sub_breeds[all_b$breed == breed]
  if (length(sb_list) == 0) return(character(0))
  sb_list[[1]]
}

#’ @title   Fetch one random image for a breed
#’ @param   breed  Character
#’ @return  tibble(url = character(1))
get_random_image_by_breed <- function(breed) {
  url <- sprintf("https://dog.ceo/api/breed/%s/images/random", breed)
  res <- GET(url)
  stop_for_status(res)
  msg <- content(res, as = "parsed", simplifyVector = TRUE)$message
  tibble(url = msg)
}

#’ @title   Fetch one random image for a sub-breed
#’ @param   breed     Character
#’ @param   sub_breed Character
#’ @return  tibble(url = character(1))
get_random_image_by_sub_breed <- function(breed, sub_breed) {
  url <- sprintf("https://dog.ceo/api/breed/%s/%s/images/random", breed, sub_breed)
  res <- GET(url)
  stop_for_status(res)
  msg <- content(res, as = "parsed", simplifyVector = TRUE)$message
  tibble(url = msg)
}

#’ @title   Fetch n random images for a breed
#’ @param   breed  Character
#’ @param   n      Integer
#’ @return  tibble(url = chr(n))
get_n_images_by_breed <- function(breed, n) {
  url <- sprintf("https://dog.ceo/api/breed/%s/images/random/%d", breed, n)
  res <- GET(url)
  stop_for_status(res)
  msgs <- content(res, as = "parsed", simplifyVector = TRUE)$message
  tibble(url = unlist(msgs))
}

#’ @title   Fetch n totally random images
#’ @param   n  Integer
#’ @return   tibble(url = chr(n))
get_n_random_images <- function(n) {
  url <- sprintf("https://dog.ceo/api/breeds/image/random/%d", n)
  res <- GET(url)
  stop_for_status(res)
  msgs <- content(res, as = "parsed", simplifyVector = TRUE)$message
  tibble(url = unlist(msgs))
}

#' @noRd
#' @import httr
accuranker_fetch <- function(request_string, api_version = "v4", simple_token = NULL){
  if(is.null(simple_token)){
    token_header <- paste("Bearer", AccurankerAuth$public_fields$token$credentials$access_token)
  } else {
    token_header <- paste("Token", simple_token)
  }

  data <- GET(
    url = paste0("http://app.accuranker.com/api/",api_version,"/", request_string),
    add_headers(
      .headers = c(
        "Authorization" = token_header
      )
    )
  )

  if (data$status_code == 500){
    stop("[500] Internal server error.")
  } else if(data$status_code != 200){
    stop("Failed to make request to Accuranker. Check your token and/or parameters.", call. = FALSE)
  }

  data <- content(data)

  return(data)
}


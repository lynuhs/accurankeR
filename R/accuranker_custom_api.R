#' Collect custom data from the Accuranker API
#'
#' This function will import data for the chosen parameters.
#' @param request A string that contains the custom API request for Accuranker.
#' @param simple_token Can be used to insert a custom API token instead of using oauth2.
#'
#' @export
#' @examples
#' accuranker_custom_api(request, simple_token = NULL)
accuranker_custom_api <- function(request, simple_token = NULL) {
  if(is.null(simple_token)){
    check <- check_existing_token()
    if(!check$is_valid){
      stop(check$message, call. = FALSE)
    }
  }

  data <- accuranker_fetch(request, simple_token = simple_token)

  return(data)
}

#' Get a list of all domains in the authorized Accuranker account
#'
#' @param simple_token Can be used to insert a custom API token instead of using oauth2.
#' @export
#' @examples
#' accuranker_domains(simple_token = NULL)
accuranker_domains <- function(simple_token = NULL) {
  if(is.null(simple_token)){
    check <- check_existing_token()
    if(!check$is_valid){
      stop(check$message, call. = FALSE)
    }
  }

  data <- accuranker_fetch("domains?fields=domain,id", simple_token = simple_token)
  data <- matrix(unlist(data), ncol = 2, byrow = TRUE)

  data <- data.frame(
    domainId = data[, 1],
    domainName = data[, 2],
    stringsAsFactors = FALSE
  )
  return(data)
}

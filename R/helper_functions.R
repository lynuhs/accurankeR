# CONVERT A LIST INTO A DATA FRAME
listToDataFrame <- function(list, prefix = NULL){
  data <- NULL

  # CHECK IF THE LIST IS NESTED
  if(is.list(list[[1]])){
    for(i in 1:(length(list))){
      unlisted <- unlist(list[[i]])
      colnames <- names(unlisted)
      data <- plyr::rbind.fill(
        data,
        data.frame(
          matrix(unlisted, ncol = length(colnames), byrow = TRUE, dimnames = list(NULL, colnames)),
          stringsAsFactors = FALSE
        )
      )
    }
  } else {
    # CHECK IF THE LIST CONTAINS MULTIPLE ITEMS
    if(length(list) > 1){
      unlisted <- unlist(list)
      colnames <- names(unlisted)

      if(is.null(unlisted)){
        colnames <- names(list)
        data <- data.frame(
          matrix(rep(NA, length(colnames)), ncol = length(colnames), byrow = TRUE, dimnames = list(NULL, colnames)),
          stringsAsFactors = FALSE
        )
      } else {
        data <- data.frame(
          matrix(unlisted, ncol = length(colnames), byrow = TRUE, dimnames = list(NULL, colnames)),
          stringsAsFactors = FALSE
        )
      }
    } else {
      data <- data.frame(
        unlist(list),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      colnames(data) <- names(list)
    }
  }

  if(!is.null(prefix)){
    colnames(data) <- paste0(prefix, "_", colnames(data))
  }

  return(data)
}


is.date <- function(date){
  tryCatch({
    as.Date(date)
    return(TRUE)
  }, error = function(e){
    return(FALSE)
  })
}

valid_token <- function(){
  token <- AccurankerAuth$public_fields$token
  if(!is.null(token)){
    status <- GET(
      url = paste0("http://app.accuranker.com/api/v4/domains"),
      add_headers(
        .headers = c(
          "Authorization" = paste0("Token ", token)
        )
      )
    )$status_code

    if(status == 200){
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Get the keyword rankings of your competitors for a specific domain
#'
#' @param domain_id A specific domain id
#' @param date_range The time period for which to get rankings from
#' @param silent If TRUE, nothing will be printed to the console
#' @param simple_token Can be used to insert a custom API token instead of using oauth2.
#'
#' @export
#' @examples
#' accuranker_keywords(domain_name, date_range = "TODAY", silent = FALSE, simple_token = NULL)
accuranker_competitor_ranks <- function(domain_name, date_range = "TODAY", silent = FALSE, simple_token = NULL) {
  if(is.null(simple_token)){
    check <- check_existing_token()
    if(!check$is_valid){
      stop(check$message, call. = FALSE)
    }
  }

  domain_list <- accuranker_domains(simple_token = simple_token)

  if (!(domain_name %in% domain_list$domainName)) {
    stop(paste0(
      domain_name,
      " does not exist in your authorized Accuranker account!"
    ))
  }

  if (is.character(date_range)) {
    if (toupper(date_range) == "TODAY") {
      date_range <- c(Sys.Date(), Sys.Date())
    } else if (toupper(date_range) == "YESTERDAY") {
      date_range <- c(Sys.Date() - 1, Sys.Date() - 1)
    } else {
      stop(
        "date_range must contain two dates in the following format: c(YYYY-MM-DD, YYYY-MM-DD) or one of c('TODAY', 'YESTERDAY')"
      )
    }
  } else if (length(date_range) == 2) {
    for (i in 1:2) {
      if (!(is.date(date_range[i]))) {
        stop(
          "date_range must contain two dates in the following format: c(YYYY-MM-DD, YYYY-MM-DD) or one of c('TODAY', 'YESTERDAY')"
        )
      }
    }
  } else {
    stop(
      "date_range must contain two dates in the following format: c(YYYY-MM-DD, YYYY-MM-DD)"
    )
  }


  dates <-
    seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), 1)

  loops <- ceiling(length(dates) / 30)

  if (loops > 1 & !silent) {
    cat(crayon::red(paste0(
      "Splitting API calls into ", loops, " fetches\n"
    )))
  }



  tryCatch({
    for (l in 1:loops) {
      startDate <- dates[1 + (30 * (l - 1))]
      if (l == loops) {
        endDate <- dates[length(dates)]
      } else {
        endDate <- dates[30 * l]
      }

      if(!silent){
        if (loops > 1) {
          cat(crayon::red(
            paste0(
              "Fetching data from Accuranker for API call ",
              l,
              " of ",
              loops,
              "\n"
            )
          ))
        } else {
          cat(crayon::red(paste0("Fetching data from Accuranker\n")))
        }
      }

      request <- paste0(
        "domains/",
        domain_list[which(domain_list$domainName == domain_name), 'domainId'],
        "/keywords/?fields=",
        "id,",
        "competitor_ranks.rank,",
        "&period_from=",
        startDate,
        "&period_to=",
        endDate
      )

      data <- accuranker_fetch(request, simple_token = simple_token)

      rankings <- NULL

      for (k in 1:(length(data))) {
        single_rank <- NULL
        length <- length(data[[k]]$competitor_ranks)

        if(length > 0){
          for(r in 1:length){
            single_rank <- plyr::rbind.fill(
              single_rank,
              listToDataFrame(data[[k]]$competitor_ranks[[r]])
            )
          }

          rankings <- plyr::rbind.fill(
            rankings,
            cbind(
              data.frame(
                keyword_id = data[[k]]$id,
                stringsAsFactors = FALSE
              ),
              single_rank
            )
          )
        }
      }

    }

    if(!silent){cat(crayon::green("Competitor keyword rankings downloaded successfully!\n"))}
    return(rankings)
  }, error = function(e) {
    cat(crayon::red(paste0(e, "\n")))
  })
}

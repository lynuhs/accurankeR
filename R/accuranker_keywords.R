#' Get the keywords for a specific domain
#'
#' @param domain_id A specific domain id
#' @param silent If TRUE, nothing will be printed to the console
#' @param simple_token Can be used to insert a custom API token instead of using oauth2.
#'
#' @export
#' @examples
#' accuranker_keywords(domain_name, silent = FALSE, simple_token = NULL)
accuranker_keywords <- function(domain_name, silent = FALSE, simple_token = NULL) {
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
      " does not exist in the authorized Accuranker account!"
    ))
  }


  tryCatch({
    keywords <- NULL

    if(!silent){cat(crayon::red("Fetching keywords from Accuranker\n"))}

    request <- paste0(
      "domains/",
      domain_list[which(domain_list$domainName == domain_name), 'domainId'],
      "/keywords/?fields=",
      "id,",
      "keyword,",
      "tags,",
      "search_type,",
      "starred,",
      "search_locale.country_code,",
      "search_locale.region,",
      "search_locale.locale,",
      "search_locale.locale_short,",
      "search_location,",
      "search_engine.name,",
      "search_volume.search_volume,",
      "search_volume.avg_cost_per_click,",
      "search_volume.competition"
    )

    data <- accuranker_fetch(request, simple_token = simple_token)

    for (k in 1:(length(data))) {
      keywords <- plyr::rbind.fill(
        keywords,
        cbind(
          data.frame(
            keyword_id = data[[k]]$id,
            keyword = data[[k]]$keyword,
            is_starred = data[[k]]$starred,
            search_type = ifelse(data[[k]]$search_type == 1, "Desktop", "Mobile"),
            tags = as.character(ifelse(
              is.null(data[[k]]$tags),
              NA,
              paste(data[[k]]$tags, collapse = ", ")
            )),
            search_location = as.character(ifelse(
              is.null(data[[k]]$search_location),
              NA,
              data[[k]]$search_location
            )),
            stringsAsFactors = FALSE
          ),
          listToDataFrame(data[[k]]$search_locale),
          listToDataFrame(data[[k]]$search_engine, "search_engine"),
          listToDataFrame(data[[k]]$search_volume)
        )
      )
    }

    if(!silent){cat(crayon::green("Keywords downloaded successfully!\n"))}
    return(keywords)
  }, error = function(e) {
    cat(crayon::red(paste0(e, "\n")))
  })
}

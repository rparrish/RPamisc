#' redcap_write_df
#'
#' writes a dataframe to a REDCap project
#' @param ds dataframe to be written
#' @param redcap_uri URL to the REDCap API folder
#' @param token REDCap token (must have Import Data enabled)
#' @param verbose (default = TRUE)
#' @param token
#' @param cert_location
#' @keywords redcap
#' @return ??
#' @export
#' @examples
#'mydata <- data.frame(study_id = c(1,2,3),
#'                     name = c("apple", "banana", "pear")
#'                     )
#'
#'uri <- "https://ampa.org/redcap/api/"
#'token <- "088E8B8639976E90837B34769C4978E5"
#'
#'test <- redcap_write_df(mydata, uri, token)


redcap_write_df <- function( ds, redcap_uri, token, verbose=TRUE, cert_location=NULL ) {
    #TODO: automatically convert boolean/logical class to integer/bit class
    start_time <- Sys.time()
    csvElements <- NULL #This prevents the R CHECK NOTE: 'No visible binding for global variable Note in R CMD check';  Also see  if( getRversion() >= "2.15.1" )    utils::globalVariables(names=c("csvElements")) #http://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check; http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

    if( missing(redcap_uri) )
        stop("The required parameter `redcap_uri` was missing from the call to `redcap_write_oneshot()`.")

    if( missing(token) )
        stop("The required parameter `token` was missing from the call to `redcap_write_oneshot()`.")

    if( missing( cert_location ) | is.null(cert_location) )
        cert_location <- file.path(devtools::inst("REDCapR"), "ssl_certs/mozilla_2014_04_22.crt")
    #     curl_options <- RCurl::curlOptions(ssl.verifypeer=FALSE)

    if( !base::file.exists(cert_location) )
        stop(paste0("The file specified by `cert_location`, (", cert_location, ") could not be found."))

    curl_options <- RCurl::curlOptions(cainfo=cert_location, sslversion=3)

    con <-  base::textConnection(object='csvElements', open='w', local=TRUE)
    write.csv(ds, con, row.names = FALSE, na="")
    close(con)

    csv <- paste(csvElements, collapse="\n")
    rm(csvElements, con)

    # returnContent <- RCurl::postForm(
    #   uri = redcap_uri,
    #   token = token,
    #   content = 'record',
    #   format = 'csv',
    #   type = 'flat',
    #   returnContent = "ids",
    #   overwriteBehavior = 'overwrite', #overwriteBehavior: *normal* - blank/empty values will be ignored [default]; *overwrite* - blank/empty values are valid and will overwrite data
    #   data = csv,
    #   .opts = curl_options
    # )
    post_body <- list(
        token = token,
        content = 'record',
        format = 'csv',
        type = 'flat',

        #These next values separate the import from the export API call
        data = csv,
        overwriteBehavior = 'overwrite', #overwriteBehavior: *normal* - blank/empty values will be ignored [default]; *overwrite* - blank/empty values are valid and will overwrite data
        returnContent = 'ids',
        returnFormat = 'csv'
    )

    result <- httr::POST(
        url = redcap_uri,
        body = post_body,
        config = curl_options #RCurl::curlOptions(ssl.verifypeer=FALSE)
    )

    status_code <- result$status_code
    status_message <- result$headers$statusmessage
    raw_text <- httr::content(result, type="text")
    elapsed_seconds <- as.numeric(difftime( Sys.time(), start_time,units="secs"))

    #isValidIDList <- grepl(pattern="^id\\n.{1,}", x=raw_text, perl=TRUE) #example: x="id\n5835\n5836\n5837\n5838\n5839"
    success <- (status_code == 200L)

    if( success ) {
        elements <- unlist(strsplit(raw_text, split="\\n"))
        affectedIDs <- elements[-1]
        recordsAffectedCount <- length(affectedIDs)
        outcome_message <- paste0(format(recordsAffectedCount, big.mark = ",", scientific = FALSE, trim = TRUE),
                                  " records were written to REDCap in ",
                                  round(elapsed_seconds, 2),
                                  " seconds.")
    }
    else { #If the returned content wasn't recognized as valid IDs, then
        affectedIDs <- numeric() #Pass an empty array
        recordsAffectedCount <- NA
        outcome_message <- "The content returned during the write operation was not recognized.  Please see the `returnContent` element for more information."
    }
    if( verbose )
        message(outcome_message)

    #   browser()

    return( list(
        success = success,
        status_code = status_code,
        status_message = status_message,
        outcome_message = outcome_message,
        records_affected_count = recordsAffectedCount,
        affected_ids = affectedIDs,
        elapsed_seconds = elapsed_seconds,
        raw_text = raw_text
    ))
}


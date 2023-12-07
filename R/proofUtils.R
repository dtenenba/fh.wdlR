#' Get header for PROOF API calls
#'
#' Utility method to get header for PROOF API calls
#'
#' @param token PROOF API token
#' @return A header that can be passed to GET(), POST(), etc.
#' @author Amy Paguirigan
#' @details
#' @export
getProofHeader <- function(token) {
    add_headers(Authorization = paste0("Bearer ", token))
}


#' Authenticate with PROOF API 
#'
#' Authenticates with HutchNet credentials, returns PROOF API token
#'
#' @param username HutchNet username
#' @param password HutchNet password
#' @return A token for bearer authentication with the PROOF API
#' @author Amy Paguirigan
#' @details
#' @export
authenticate <- function(username, password) {
    url <- "https://proof-api.fredhutch.org/authenticate"
    response <-  POST(url, body = list(username = username, 
        password=password), encode = "json")
    if (response$status_code != 200) {
        return(NULL)
    }
    jsonRespParsed<-content(response,as="parsed")
    return(jsonRespParsed$token)
}


#' Get PROOF API job status
#'
#' Get PROOF API job status - is job running, what's its URL...
#'
#' @param token PROOF API token
#' @return A list with fields canJobStart, jobStatus, and cromwellUrl
#' @author Amy Paguirigan
#' @details
#' @export
getProofJobStatus <- function(token) {
    url <- "https://proof-api.fredhutch.org/cromwell-server"
    response <- GET(url, getProofHeader(token))
    if (response$status_code != 200) {
        return(NULL)
    }
    jsonRespParsed<-content(response,as="parsed")
    return(jsonRespParsed)
}

#' Start PROOF Cromwell server
#'
#' See https://github.com/FredHutch/proof-api#post-cromwell-server
#'
#' @param token PROOF API token
#' @param pi_name PI name in the form last_f; only needed if user is in more than one SLURM account
#' @return A list with fields job_id and info
#' @author Amy Paguirigan
#' @details
#' Does not return PROOF/Cromwell server URL, for that you have to periodically
#' call \link{getProofJobStatus}, or wait for the email from the PROOF API.
#' @export
startProofCromwellJob <- function(token, pi_name=NULL) {
    url <- "https://proof-api.fredhutch.org/cromwell-server"
    response <- POST(url, getProofHeader(token),
        body = list(pi_name = pi_name), encode = "json")
    # TODO better error handling - surface error messages
    if (response$status_code != 200) {
        return(NULL)
    }
    jsonRespParsed<-content(response,as="parsed")
    return(jsonRespParsed)
}


cancelCromwellJob <- function(token) {
    url <- "https://proof-api.fredhutch.org/cromwell-server"
    response <- DELETE(url, getProofHeader(token))
    # TODO better error handling - surface error messages
    if (response$status_code != 200) {
        return(NULL)
    }
    jsonRespParsed<-content(response,as="parsed")
    return(jsonRespParsed)
}

# Now all calls to the cromwell server in this package
# should accept a URL and token (or token header) as parameters.

# Example - here is a modified version of the cromwellVersion function
# called cromwellVersion2.

#' Get the version of a Cromwell server
#'
#' @param cromURL The full string of the Cromwell URL to query  (e.g. http://gizmog10:8000). (Optional)
#' @param token PROOF API token
#' @return Cromwell version
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment or specify "cromURL".
#' @examples
#' TBD
#' @export
cromwellVersion2 <- function(cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL"), token=NULL) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } 
  if (is.null(token)) {
    stop("token is required")
  }
  message("Getting timing diagram from Cromwell.") 
  httr::content(httr::GET(paste0(
    cromURL,
    "engine/v1/version" # note that I took off the leading slash
                        # because PROOF returns URLs with a slash on the end
                        # That could be changed....
  ), getProofHeader(token)))
}

#' @title Sreg Class
#'
#' @description
#' Get data from Sudski registar: https://sudreg-data.gov.hr/ords/r/srn_rep/vanjski-srn-rep/home
#'
#' @export
Sreg = R6::R6Class(
  "Sreg",
  public = list(
    #' @field token_url Token url
    token_url = "https://sudreg-data.gov.hr/api/oauth/token",

    #' @field url Base url for endpoints
    url = "https://sudreg-data.gov.hr/api/javni/",

    #' @field user_name User name
    user_name = NULL,

    #' @field pass Password
    pass = NULL,

    #' @field token token
    token = NULL,

    #' @field docs Documentation
    docs = read_json("https://sudreg-data.gov.hr/api/javni/dokumentacija/open_api"),

    #' @description
    #' Create a new Sreg object.
    #'
    #' @param user_name User name. Recommended to sava in .Renviron as USER.
    #'     Otherwise provide string.
    #' @param pass Password. Recommended to save in .Renviron as PASS.
    #'
    #' @return A new `Sreg` object.
    initialize = function(user_name = Sys.getenv("USER"),
                          pass = Sys.getenv("PASS")) {
      self$user_name = user_name
      self$pass = pass
      self$token = self$get_token()
    },

    #' @description Get token.
    #'
    #' @return String - token.
    get_token = function() {
      token = POST(
        self$token_url,
        authenticate(self$user_name, self$pass),
        body = list(grant_type = "client_credentials"),
        encode = "form",
        config(ssl_verifypeer = FALSE)
      )
      token = content(token)
      token$time = Sys.time()
      return(token)
    },

    #' @description Get new token.
    #'
    #' @return String - token.
    check_token_expiry = function() {
      # If token is created 5 hours ago, get new token
      if (difftime(Sys.time(), self$token$time, units = "hours") > 5) {
        print("Get new token after 5 hours")
        self$token = self$get_token()
      }
    },

    #' @description Get data from Sudski registar.
    #'
    #' @param url URL to get data from.
    #'
    #' @return Data from Sudski registar.
    get = function(url) {
      self$check_token_expiry()
      res = GET(url,
                add_headers(
                  "Authorization" = paste0("Bearer ", self$token$access_token),
                  "Content-Type" = "application/json"
                ))
      return(content(res))
    },

    #' @description Get data from Sudski registar.
    #'
    #' @param tag Tag to get data from.
    #' @param q Query parameters.
    #' @param clean Clean data.
    #'
    #' @return Data from Sudski registar.
    get_wrap = function(tag,
                        q = list(limit = 100, only_active = TRUE),
                        clean = TRUE) {
      # Create url
      url = modify_url(paste0(self$url, tag), query = q)
      # Make GET request
      cont = self$get(url)
      # Clean if needed
      if (clean == TRUE) {
        return(rbindlist(cont, fill = TRUE))
      } else {
        return(cont)
      }
    },

    #' @description Get data from Sudski registar.
    #'
    #' @param tag Tag to get data from.
    #' @param q Query parameters.
    #' @param by Number of rows to get.
    #'
    #' @return Data from Sudski registar.
    get_loop = function(tag, q = list(), by = 1000) {
      # Define offsets
      offset_seq = seq(0, 500000, by = by)
      offset_seq = format(offset_seq, scientific = FALSE)
      offset_seq = gsub("\\s+", "", offset_seq)
      # Define urls
      urls = lapply(offset_seq, function(x) {
        modify_url(paste0(self$url, tag), query = c(q, list(
          offset = x,
          limit = format(by, scientific = FALSE)
        )))
      })
      res_l = lapply(urls, self$get)
      res_l = res_l[!sapply(res_l, function(x)
        length(x) == 0)]
      res_l = lapply(res_l, function(l)
        rbindlist(lapply(l, as.data.table), fill = TRUE))
      rbindlist(res_l, fill = TRUE)
    },

    #' @description List of all endpoints.
    #'
    #' @return List of all endpoints.
    endpoints = function() {
      names(self$docs[["paths"]])
    },

    #' @description Get endpoint meta.
    #'
    #' @param endpoint Endpoint.
    #'
    #' @return Endpoint meta.
    endpoint_meta = function(endpoint) {
      self$docs[["paths"]][[endpoint]][["get"]]
    },

    #' @description Get endpoint parameters.
    #'
    #' @param endpoint Endpoint.
    #'
    #' @return Endpoint parameters.
    endpoint_parameters = function(endpoint) {
      rbindlist(self$endpoint_meta(endpoint)[["parameters"]], fill = TRUE)
    }
  )
)

# # TEST
# library(httr)
# library(data.table)
# library(jsonlite)
# sreg = Sreg$new()
# sreg$user_name
# sreg$pass
# sreg$url
# sreg$token_url
# sreg$token
# sreg$endpoints()
# sreg$endpoint_meta("/subjekti")
# sreg$endpoint_meta("/email_adrese")
# sreg$endpoint_parameters("/email_adrese")
# sreg$endpoint_meta("/subjekti")[["get"]][["parameters"]]
# sreg$endpoint_meta("/email_adrese")


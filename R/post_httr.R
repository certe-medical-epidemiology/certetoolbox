# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' POST Data (Encrypted)
#' 
#' Securely transfer any R object with full attribute preservation and cross-platform JSON compatibility. This uses the same approach as the [`mmbi.epi` package of the UMCG](https://github.com/umcg-mmbi-epidemiology/mmbi.epi/blob/1ed61034ad0c68fd0e78843aa1ff253d141ce6d8/R/serialise-encrypt-post.R).
#' @param object Any object of any size, preferably a data set
#' @param compress Should the object be compressed/decompressed? At least allowed: `"gzip"` (or `TRUE`), `"bzip2"`, `"xz"`, see [base::memCompress()]. Use `FALSE` to not compress/decompress.
#' @param encrypt Should the object be encrypted/decrypted? This applies AES-GCM via [openssl::aes_gcm_encrypt()], providing authenticated encryption. This guarantees both confidentiality and integrity: the file cannot be read without the correct `key`, and any tampering will be detected automatically during decryption. The initialization vector (iv) will be a length-12 random [raw] vector.
#' @param key A character to be used as the encryption key. Internally, this is converted using [openssl::sha256()] to ensure a raw high-entropy key of length `32`, suitable for AES-GCM encryption. The default is the [system environment variable][Sys.getenv()]: `mmbi_epi_encryption_key`.
#' @param url A character string specifying the target URL for the HTTP POST request. Must include the full scheme (e.g., `"https://"` or `"http://"`), hostname, and path.
#' @param authorization_header A character string specifying the value of the `Authorization` header to include in the POST request, e.g. `"Bearer <token>"`. Use `NULL` to omit the header.
#' @importFrom httr POST add_headers stop_for_status
#' @rdname post-httr
#' @export
#' @examples
#' \dontrun{
#'
#' post_data(iris,
#'           url = "https://some-server:8000/post",
#'           compress = TRUE,
#'           encrypt = TRUE)
#' }
#'
#' # MANUAL WAY -----------------------------------------------------------
#' 
#' # use create_json_body() to make a(n encrypted) JSON of an object
#' 
#' iris_json <- iris |> create_json_body(compress = TRUE, encrypt = TRUE)
#' 
#' # curl -X POST https://some-server:8000/post
#' #      -H "Content-Type: application/json"
#' #      -d '...'
#'
#' # replace the "..." with the outcome of create_json_body():
#' iris_json
post_data <- function(object,
                      url,
                      authorization_header = NULL,
                      compress = TRUE,
                      encrypt = TRUE,
                      key = read_secret("tools.encryption_password")) {
  
  json_body <- create_json_body(object = object,
                                compress = compress,
                                encrypt = encrypt)
  
  resp <- POST(url = url,
               body = json_body,
               encode = "json",
               config = add_headers(Authorization = authorization_header))
  stop_for_status(resp)
  invisible(resp)
}

#' @importFrom jsonlite toJSON
#' @rdname post-httr
#' @export
create_json_body <- function(object,
                             compress = TRUE,
                             encrypt = TRUE,
                             key = read_secret("tools.encryption_password")) {
  
  payload <- serialise_json(object, compress = compress)
  
  if (isTRUE(encrypt)) {
    payload <- encrypt_object(payload, key = key, serialise = FALSE, compress = FALSE)
    iv <- attr(payload, "iv", exact = TRUE)
  } else {
    iv <- NULL
  }
  
  if (is.null(iv)) {
    toJSON(x = list(data = payload), auto_unbox = TRUE)
  } else {
    toJSON(x = list(data = payload, iv = iv), auto_unbox = TRUE)
  }
}

serialise_json <- function(object, compress = TRUE) {
  # Extract class and levels info if object is a data.frame
  meta <- list()
  meta$obj_attributes <- attributes(object)
  if (is.data.frame(object)) {
    meta$col_attributes <- lapply(object, attributes)
  }
  
  # Wrap object with metadata
  wrapped <- list(
    object = object,
    meta = meta
  )
  
  out <- wrapped |>
    toJSON(dataframe = ifelse(is.data.frame(object), "rows", "columns"),
           auto_unbox = TRUE,
           null = "null",
           na = "null",
           POSIXt = "ISO8601",
           digits = NA,
           force = TRUE) |>
    charToRaw()
  if (isTRUE(compress)) {
    compress <- "gzip"
  }
  if (!isFALSE(compress)) {
    out <- memCompress(out, type = compress)
  }
  out
}

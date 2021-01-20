#' extract recipient details from invoice
#'
#' @description extract recipient details from invoice
#'
#' @param path \code{string} path of invoice pdf
#'
#' @importFrom pdftools pdf_text
#' @importFrom magrittr set_colnames
#' @importFrom stringr str_match
extract_recipient_details <- function(path, regex = regex_pattern()) {
  txt <- pdf_text(path)
  # https://regex101.com/r/wHv4Nk/1
  match <- stringr::str_match(txt, regex)
  recipient <- as_tibble(match, .name_repair = "unique") %>%
    magrittr::set_colnames(c("match", "name", "street", "postal_code", "city", "email", "phone")) %>%
    select(-match)
  return(recipient)
}

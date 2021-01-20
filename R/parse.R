#' extract recipient details from invoice
#'
#' @description extract recipient details from invoice
#'
#' @param path \code{string} path of invoice pdf
#'
#' @importFrom pdftools pdf_text
#' @importFrom magrittr set_colnames
#' @importFrom stringr str_match
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @export
extract_recipient_details <- function(path, regex = regex_pattern()) {
  txt <- pdf_text(path)
  # https://regex101.com/r/wHv4Nk/1
  match <- stringr::str_match(txt, regex)
  recipient <- as_tibble(match, .name_repair = "unique") %>%
    magrittr::set_colnames(c("match", "name", "street", "postal_code", "city", "email", "phone")) %>%
    select(-match)
  return(recipient)
}


#' convert invoice folder to adress book
#'
#' @description crawl through invoice folder, parse invoices,
#'     extract recipient and combine to collect address book
#'
#' @param dir \code{string} path of invoice folder
#'
#' @importFrom tibble enframe
#' @importFrom purrr map
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @export
invoice_folder_to_adress_book <- function(dir) {

  files <- paste0(dir, "/", list.files(dir, pattern = "*.pdf"))
  invoices <- enframe(files, name = "index", value = "invoice")
  adress_book <- invoices %>%
    mutate(
      recipient = map(invoice, extract_recipient_details)
    ) %>%
    unnest(recipient)
  return(adress_book)
}


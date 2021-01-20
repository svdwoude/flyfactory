#' extract recipient details from invoice
#'
#' @description extract recipient details from invoice
regex_pattern <- function() {
  r <- list(
    spacer = " +",
    newline = "\\n",
    BTW = "BE \\d+\\.\\d+\\.\\d+",
    account = "IBAN \\: BE\\d+ \\d+ \\d+ \\d+",
    SWIFT = "SWIFT\\: BBRU BE BB",
    bank = "ING bank",
    name = "([\\w -\\.]+)",
    btw = "(?: +([\\w\\d \\.]+)\\n)?",
    street = "([\\w -]+)",
    postal_code = "(\\d+)",
    city = "([\\w -]+)",
    email = "([^@ \\t\\r\\n]+@[^@ \\t\\r\\n]+\\.[^@ \\t\\r\\n]+)",
    phone = "([\\d\\+]+)"
  )

  regex <- paste0(
    r$BTW, r$spacer, r$name, r$newline,
    r$btw,
    r$spacer, r$street, r$newline,
    r$bank, r$spacer, r$postal_code, " ", r$city, r$newline,
    r$account, r$spacer, r$email, r$newline,
    r$SWIFT, r$spacer, r$phone
  )

  return(regex)
}

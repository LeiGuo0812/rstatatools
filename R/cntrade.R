#' @title Download stock data
#'
#' @description Download historical Market Quotations for a list of stock codes or index codes from Net Ease (a web site providing financial information in China, http://money.163.com/).
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom readr locale
#'
#' @param ticker Ticker is a stock code or index code to be downloaded from Net. In China, stocks are identified by a six digit numbers, not tickers as in the United States. Examples of codes and the names are as following:
#' Stock Codes and Stock Names:
#'   000001 Pingan Bank
#'   000002 Vank Real Estate Co. Ltd.
#'   600000 Pudong Development Bank
#'   600005 Wuhan Steel Co. Ltd.
#'   900901 INESA Electron Co.,Ltd.

#'   Index Codes and Index Names:
#'   000001 The Shanghai Composite Index.
#'   000300 CSI 300 Index.
#'   399001 Shenzhen Component Index.
#' The leading zeros in each code can be omitted.
#' @param type Specify that the code is a stock code or index code, 'stock' is the default choice.
#'
#' @examples
#' cntrade(1, type = "index")
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @export
cntrade <- function(ticker = "",
                    type = "stock") {
  ticker = as.character(ticker)
  stopifnot(type %in% c("stock", "index"))
  address = "http://quotes.money.163.com/service/chddata.html"
  if(type == "index") {
    field = "TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;TURNOVER;VOTURNOVER;VATURNOVER;TCAP;MCAP"
  }
  if(type == "stock") {
    field = "TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER"
  }
  start = "19900101"
  end = stringr::str_remove_all(as.character(Sys.Date()), "-")
  stopifnot(stringr::str_length(ticker) <= 6)
  while (stringr::str_length(ticker) < 6) {
    ticker = paste0("0", ticker)
  }
  if(type == "stock") {
    if(as.numeric(ticker) >= 600000) {
      url = paste0(address, "?code=0", ticker,
                   "&start=", start, "&end=", "&fields=", field)
    }
    if(as.numeric(ticker) < 600000) {
      url = paste0(address, "?code=1", ticker,
                   "&start=", start, "&end=", "&fields=", field)
    }
  }

  if(type == "index") {
    if(as.numeric(ticker) <= 1000) {
      url = paste0(address, "?code=0", ticker,
                   "&start=", start, "&end=", "&fields=", field)
    }
    if(as.numeric(ticker) > 1000) {
      url = paste0(address, "?code=1", ticker,
                   "&start=", start, "&end=", "&fields=", field)
    }
  }

  suppressWarnings({
    if(type == "index"){
      suppressMessages({
        readr::read_csv(url, locale = locale(encoding = "GBK")) %>%
          purrr::set_names(c("date", "code", "name", "close", "high", "low", "open", "yesterday close", "change amount", "change percent", "turnover", "transaction", "volume", "total market capitalisation", "traded market value")) -> df
      })
      df$code <- stringr::str_remove(df$code, "'")
    }
    if(type == "stock"){
      suppressMessages({
        readr::read_csv(url, locale = locale(encoding = "GBK")) %>%
          purrr::set_names(c("date", "code", "name", "close", "high", "low", "open", "yesterday close", "change amount", "change percent", "volume", "transaction")) -> df
      })
      df$code <- stringr::str_remove(df$code, "'")
    }
  })
  stopifnot(nrow(df) != 0)

  return(df)
}

#' @title Stock chart
#'
#' @description Plot a interactive chart to visualize stock date.
#' @importFrom magrittr %>%
#' @import dplyr
#' @import highcharter
#'
#' @param ticker Ticker is a stock code or index code to be downloaded from Net. In China, stocks are identified by a six digit numbers, not tickers as in the United States. Examples of codes and the names are as following:
#' Stock Codes and Stock Names:
#'   000001 Pingan Bank
#'   000002 Vank Real Estate Co. Ltd.
#'   600000 Pudong Development Bank
#'   600005 Wuhan Steel Co. Ltd.
#'   900901 INESA Electron Co.,Ltd.

#'   Index Codes and Index Names:
#'   000001 The Shanghai Composite Index.
#'   000300 CSI 300 Index.
#'   399001 Shenzhen Component Index.
#' The leading zeros in each code can be omitted.
#' @param type Specify that the code is a stock code or index code, 'stock' is the default choice.
#'
#' @examples
#' plotstock(1, type = "index")
#'
#' @export
plotstock <- function(ticker = "1", type = "stock"){
  cntrade(ticker, type) %>%
    arrange(date) -> df

  df %>%
    select("date", "open", "high", "low", "close") %>%
    mutate(date = datetime_to_timestamp(date)) %>%
    list_parse2() -> dflist1

  df %>%
    select("date", "volume") %>%
    mutate(date = datetime_to_timestamp(date)) %>%
    list_parse2() -> dflist2

  highchart(type = "stock") %>%
    hc_add_series(type = 'candlestick',
                  name = df$name[nrow(df)],
                  data = dflist1,
                  color = 'green',
                  lineColor = 'green',
                  upColor = 'red',
                  upLineColor = 'red',
                  showInLegend = F,
                  tooltip = list(pointFormat = "<b>{series.name}</b><br> Open: {point.open} <br>High: {point.high}<br>Low: {point.low} <br>Close: {point.close}",
                                 borderRadius = 5)) %>%
    hc_add_series(type = 'column',
                  name = 'Volume',
                  data = dflist2,
                  yAxis = 1,
                  showInLegend = F,
                  tooltip = list(headerFormat = "",
                                 pointFormat = "<b>{series.name}</b>: {point.x}",
                                 borderRadius = 5)) %>%
    hc_add_dependency('modules/drag-panes.js') %>%
    hc_yAxis_multiples(
      list(labels = list(align = 'left'),
           height = '80%',
           title = list(text = "Price", offset = 20),
           resize = list(enabled = T),
           opposite = FALSE,
           offset = 20),
      list(labels = list(align = "right"),
           top = '80%',
           title = list(text = "Volume", offset = 20),
           height = '30%',
           opposite = TRUE,
           offset = 20)
    ) %>%
    hc_title(text = paste0(df$name[nrow(df)], " Historical Transaction Data")) %>%
    hc_subtitle(text = "Data Source: Net ease") %>%
    hc_add_theme(hc_theme_darkunica()) %>%
    hc_rangeSelector(enabled = TRUE, selected = 1)
}

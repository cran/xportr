## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = " "
)

library(DT)

options(cli.num_colors = 1)

## ----include=FALSE----------------------------------------
options(width = 60)
local({
  hook_output <- knitr::knit_hooks$get("output")
  knitr::knit_hooks$set(output = function(x, options) {
    if (!is.null(options$max.height)) {
      options$attr.output <- c(
        options$attr.output,
        sprintf('style="max-height: %s;"', options$max.height)
      )
    }
    hook_output(x, options)
  })
})

## ----include=FALSE----------------------------------------
knitr::knit_hooks$set(output = function(x, options) {
  if (!is.null(options$max_height)) {
    paste('<pre style = "max-height:',
      options$max_height,
      '; float: left; width: 775px; overflow-y: auto;">',
      x, "</pre>",
      sep = ""
    )
  } else {
    x
  }
})

## ----include=FALSE----------------------------------------
datatable_template <- function(input_data) {
  datatable(
    input_data,
    rownames = FALSE,
    options = list(
      autoWidth = FALSE,
      scrollX = TRUE,
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20)
    )
  ) %>%
    formatStyle(
      0,
      target = "row",
      color = "black",
      backgroundColor = "white",
      fontWeight = "500",
      lineHeight = "85%",
      fontSize = ".875em" # same as code
    )
}

## ----eval = TRUE, message = FALSE, warning = FALSE--------
# Loading packages
library(dplyr)
library(labelled)
library(xportr)
library(readxl)

# Loading in our example data
data("adsl_xportr", package = "xportr")

## ----echo = FALSE-----------------------------------------
datatable_template(adsl_xportr)

## ---------------------------------------------------------
var_spec <- read_xlsx(
  system.file(file.path("specs/", "ADaM_spec.xlsx"), package = "xportr"),
  sheet = "Variables"
) %>%
  rename(type = "Data Type") %>%
  rename_with(tolower)

## ----echo = FALSE, eval = TRUE----------------------------
var_spec_view <- var_spec %>%
  filter(dataset == "ADSL")

datatable_template(var_spec_view)

## ----max_height = "200px", echo = FALSE-------------------
str(adsl_xportr)

## ----echo = TRUE------------------------------------------
adsl_type <- xportr_type(adsl_xportr, var_spec, domain = "ADSL", verbose = "message")

## ----max_height = "200px", echo = FALSE-------------------
str(adsl_type)

## ----max_height = "200px", echo = FALSE-------------------
str(adsl_xportr)

## ---------------------------------------------------------
adsl_length <- adsl_xportr %>% xportr_length(var_spec, domain = "ADSL", verbose = "message")

## ----max_height = "200px", echo = FALSE-------------------
str(adsl_length)

## ----echo = TRUE------------------------------------------
adsl_order <- xportr_order(adsl_xportr, var_spec, domain = "ADSL", verbose = "message")

## ----echo = FALSE-----------------------------------------
datatable_template(adsl_order)

## ----max_height = "200px", echo = FALSE-------------------
adsl_fmt_pre <- adsl_xportr %>%
  select(TRTSDT, TRTEDT, TRTSDTM, TRTEDTM)

tribble(
  ~Variable, ~Format,
  "TRTSDT", attr(adsl_fmt_pre$TRTSDT, which = "format"),
  "TRTEDT", attr(adsl_fmt_pre$TRTEDT, which = "format"),
  "TRTSDTM", attr(adsl_fmt_pre$TRTSDTM, which = "format"),
  "TRTEDTM", attr(adsl_fmt_pre$TRTEDTM, which = "format")
)

## ---------------------------------------------------------
adsl_fmt <- adsl_xportr %>% xportr_format(var_spec, domain = "ADSL")

## ----max_height = "200px", echo = FALSE-------------------
adsl_fmt_post <- adsl_fmt %>%
  select(TRTSDT, TRTEDT, TRTSDTM, TRTEDTM)

tribble(
  ~Variable, ~Format,
  "TRTSDT", attr(adsl_fmt_post$TRTSDT, which = "format"),
  "TRTEDT", attr(adsl_fmt_post$TRTEDT, which = "format"),
  "TRTSDTM", attr(adsl_fmt_post$TRTSDTM, which = "format"),
  "TRTEDTM", attr(adsl_fmt_post$TRTEDTM, which = "format")
)

## ----max_height = "200px", echo = FALSE-------------------
adsl_no_lbls <- haven::zap_label(adsl_xportr)

str(adsl_no_lbls)

## ---------------------------------------------------------
adsl_lbl <- adsl_xportr %>% xportr_label(var_spec, domain = "ADSL", "message")

## ----max_height = "200px"---------------------------------
str(adsl_lbl)

## ---------------------------------------------------------
adsl_xportr %>%
  xportr_type(var_spec, "ADSL", "message") %>%
  xportr_length(var_spec, "ADSL", verbose = "message") %>%
  xportr_label(var_spec, "ADSL", "message") %>%
  xportr_order(var_spec, "ADSL", "message") %>%
  xportr_format(var_spec, "ADSL") %>%
  xportr_write("adsl.xpt")


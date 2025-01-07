## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = " "
)

options(cli.num_colors = 1)

library(DT)

## ----include=FALSE------------------------------------------------------------
# Used to control str() output later on

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

## ----message = FALSE----------------------------------------------------------
library(xportr)
library(dplyr)
library(haven)

data("adsl_xportr", "var_spec", "dataset_spec", package = "xportr")
colnames(var_spec)
ADSL <- adsl_xportr

## ----eval = FALSE-------------------------------------------------------------
#  xportr_options(
#    xportr.variable_name = "Variable",
#    xportr.label = "Label",
#    xportr.type_name = "Data Type",
#    xportr.format = "Format",
#    xportr.length = "Length",
#    xportr.order_name = "Order"
#  )
#  
#  # Or alternatively
#  options(
#    xportr.variable_name = "Variable",
#    xportr.label = "Label",
#    xportr.type_name = "Data Type",
#    xportr.format = "Format",
#    xportr.length = "Length",
#    xportr.order_name = "Order"
#  )

## ----eval = FALSE-------------------------------------------------------------
#  # Default verbose is set to `none`
#  xportr_options(
#    xportr.format_verbose = "none",
#    xportr.label_verbose = "none",
#    xportr.length_verbose = "none",
#    xportr.type_verbose = "none"
#  )
#  
#  xportr_options(
#    xportr.format_verbose = "none", # Disables any messaging, keeping the console output clean
#    xportr.label_verbose = "message", # Sends a standard message to the console
#    xportr.length_verbose = "warn", # Sends a warning message to the console
#    xportr.type_verbose = "stop" # Stops execution and sends an error message to the console
#  )

## ----eval = FALSE-------------------------------------------------------------
#  ADSL %>%
#    xportr_type(var_spec, "ADSL", "message") %>%
#    xportr_length(var_spec, "ADSL", verbose = "message") %>%
#    xportr_label(var_spec, "ADSL", "message") %>%
#    xportr_order(var_spec, "ADSL", "message") %>%
#    xportr_format(var_spec, "ADSL") %>%
#    xportr_df_label(dataset_spec, "ADSL") %>%
#    xportr_write("adsl.xpt")

## ----eval = FALSE-------------------------------------------------------------
#  ADSL %>%
#    xportr_metadata(var_spec, "ADSL") %>%
#    xportr_type() %>%
#    xportr_length(length_source = "metadata") %>%
#    xportr_label() %>%
#    xportr_order() %>%
#    xportr_format() %>%
#    xportr_df_label(dataset_spec) %>%
#    xportr_write("adsl.xpt")

## -----------------------------------------------------------------------------
var_spec <- var_spec %>%
  rename(type = "Data Type") %>%
  rename_with(tolower)

dataset_spec <- dataset_spec %>%
  rename(label = "Description") %>%
  rename_with(tolower)

## ----echo = FALSE-------------------------------------------------------------
columns2hide <- c(
  "significant digits", "mandatory", "assigned value", "codelist", "common",
  "origin", "pages", "method", "predecessor", "role", "comment",
  "developer notes"
)

datatable(
  var_spec %>% select(-all_of(columns2hide)),
  rownames = FALSE,
  filter = "top",
  options = list(
    dom = "Bfrtip",
    columnDefs = list(
      list(
        width = "10px",
        targets = c("order", "length", "format", "type", "dataset", "variable")
      ),
      list(
        className = "text-left",
        targets = c("label")
      )
    ),
    searching = FALSE,
    autoWidth = TRUE
  )
) %>%
  formatStyle(0,
    target = "row",
    color = "black",
    backgroundColor = "white",
    fontWeight = "500",
    lineHeight = "85%",
    textAlign = "center",
    fontSize = ".875em" # same as code
  )

## -----------------------------------------------------------------------------
adsl_fct <- ADSL %>%
  mutate(STUDYID = as_factor(STUDYID))

## ----echo = FALSE-------------------------------------------------------------
adsl_glimpse <- adsl_fct %>%
  select(STUDYID, TRTSDT, TRTEDT, SCRFDT, EOSDT, FRVDT, RANDDT, DTHDT, LSTALVDT)

## ----echo = FALSE-------------------------------------------------------------
glimpse(adsl_glimpse)

## ----echo = TRUE--------------------------------------------------------------
adsl_type <- xportr_type(.df = adsl_fct, metadata = var_spec, domain = "ADSL", verbose = "warn")

## ----echo = FALSE-------------------------------------------------------------
adsl_type_glimpse <- adsl_type %>%
  select(STUDYID, TRTSDT, TRTEDT, SCRFDT, EOSDT, FRVDT, RANDDT, DTHDT, LSTALVDT)

## ----echo = TRUE--------------------------------------------------------------
glimpse(adsl_type_glimpse)

## ----echo = TRUE, error = TRUE------------------------------------------------
adsl_type <- xportr_type(.df = adsl_fct, metadata = var_spec, domain = "ADSL", verbose = "stop")

## ----max.height='300px', attr.output='.numberLines', echo = FALSE-------------
str(ADSL)

## ----echo = TRUE--------------------------------------------------------------
adsl_length <- xportr_length(
  .df = ADSL,
  metadata = var_spec,
  domain = "ADSL",
  verbose = "warn",
  length_source = "metadata"
)

## ----max.height='300px', attr.output='.numberLines', echo = FALSE-------------
str(adsl_length)

## ----echo = TRUE, error = TRUE------------------------------------------------
adsl_length <- xportr_length(
  .df = ADSL,
  metadata = var_spec,
  domain = "ADSL",
  verbose = "stop",
  length_source = "metadata"
)

## ----echo = TRUE--------------------------------------------------------------
var_spec_lbl <- var_spec %>%
  mutate(label = if_else(variable == "TRTSDT",
    "Length of variable label must be 40 characters or less", label
  ))

adsl_lbl <- ADSL

adsl_lbl <- haven::zap_label(ADSL)

## ----max.height='300px', attr.output='.numberLines', echo = FALSE-------------
str(adsl_lbl)

## -----------------------------------------------------------------------------
adsl_lbl <- xportr_label(.df = adsl_lbl, metadata = var_spec_lbl, domain = "ADSL", verbose = "warn")

## ----max.height='300px', attr.output='.numberLines', echo = FALSE-------------
str(adsl_lbl)

## ----echo = TRUE, error = TRUE------------------------------------------------
adsl_label <- xportr_label(.df = adsl_lbl, metadata = var_spec_lbl, domain = "ADSL", verbose = "stop")

## -----------------------------------------------------------------------------
adsl_ord <- xportr_order(.df = ADSL, metadata = var_spec, domain = "ADSL", verbose = "warn")

## ----echo = TRUE, error = TRUE------------------------------------------------
adsl_ord <- xportr_order(.df = ADSL, metadata = var_spec, domain = "ADSL", verbose = "stop")

## ----echo = TRUE--------------------------------------------------------------
adsl_fmt <- ADSL %>%
  xportr_type(metadata = var_spec, domain = "ADSL", verbose = "warn") %>%
  xportr_format(metadata = var_spec, domain = "ADSL")

## ----max.height='300px', attr.output='.numberLines', echo = FALSE-------------
str(adsl_fmt)

## ----echo = TRUE, error = TRUE------------------------------------------------
ADSL %>%
  xportr_metadata(var_spec, "ADSL") %>%
  xportr_type() %>%
  xportr_length(length_source = "metadata") %>%
  xportr_label() %>%
  xportr_order() %>%
  xportr_format() %>%
  xportr_df_label(dataset_spec) %>%
  xportr_write(path = "adsl.xpt", strict_checks = FALSE)

## ----echo = TRUE, error = TRUE------------------------------------------------
ADSL %>%
  xportr_write(path = "adsl.xpt", metadata = dataset_spec, domain = "ADSL", strict_checks = TRUE)

## ----echo = TRUE, error = TRUE------------------------------------------------
var_spec_lbl <- var_spec %>%
  mutate(label = if_else(variable == "TRTSDT",
    "Length of variable label must be 40 characters or less", label
  ))

ADSL %>%
  xportr_metadata(var_spec_lbl, "ADSL") %>%
  xportr_label() %>%
  xportr_type() %>%
  xportr_format() %>%
  xportr_df_label(dataset_spec) %>%
  xportr_write(path = "adsl.xpt", strict_checks = TRUE)

## ----echo = TRUE, error = TRUE------------------------------------------------
xportr(
  ADSL,
  var_metadata = var_spec,
  df_metadata = dataset_spec,
  domain = "ADSL",
  verbose = "none",
  path = "adsl.xpt"
)

## ----echo = TRUE, error = TRUE------------------------------------------------
ADSL %>%
  xportr_metadata(var_spec, "ADSL") %>%
  xportr_type() %>%
  xportr_length(length_source = "metadata") %>%
  xportr_label() %>%
  xportr_order() %>%
  xportr_format() %>%
  xportr_df_label(dataset_spec) %>%
  xportr_write(path = "adsl.xpt", strict_checks = FALSE)


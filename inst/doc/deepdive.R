## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = " "
)

options(cli.num_colors = 1)

library(DT)

## ---- include=FALSE-----------------------------------------------------------
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

## ---- message = FALSE---------------------------------------------------------
library(rlang)
library(xportr)
library(dplyr)
library(haven)

colnames(var_spec)

## ---- eval = FALSE------------------------------------------------------------
#  options(
#    xportr.variable_name = "Variable",
#    xportr.label = "Label",
#    xportr.type_name = "Data Type",
#    xportr.format = "Format",
#    xportr.length = "Length",
#    xportr.order_name = "Order"
#  )

## ---- eval = FALSE------------------------------------------------------------
#  # Default
#  options(
#    xportr.format_verbose = "none",
#    xportr.label_verbose = "none",
#    xportr.length_verbose = "none",
#    xportr.type_verbose = "none",
#  )
#  
#  # Will send Warning Message to Console
#  options(
#    xportr.format_verbose = "warn",
#    xportr.label_verbose = "warn",
#    xportr.length_verbose = "warn",
#    xportr.type_verbose = "warn",
#  )

## ---- eval = FALSE------------------------------------------------------------
#  adsl %>%
#    xportr_type(var_spec, "ADSL", "message") %>%
#    xportr_length(var_spec, "ADSL", "message") %>%
#    xportr_label(var_spec, "ADSL", "message") %>%
#    xportr_order(var_spec, "ADSL", "message") %>%
#    xportr_format(var_spec, "ADSL") %>%
#    xportr_write("adsl.xpt", label = "Subject-Level Analysis Dataset")

## ---- eval = FALSE------------------------------------------------------------
#  adsl %>%
#    xportr_metadata(var_spec, "ADSL") %>%
#    xportr_type() %>%
#    xportr_length() %>%
#    xportr_label() %>%
#    xportr_order() %>%
#    xportr_format() %>%
#    xportr_write("adsl.xpt", label = "Subject-Level Analysis Dataset")

## -----------------------------------------------------------------------------
var_spec <- var_spec %>%
  rename(type = "Data Type") %>%
  set_names(tolower)

## ---- echo = FALSE------------------------------------------------------------
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
adsl_fct <- adsl %>%
  mutate(STUDYID = as_factor(STUDYID))

## ---- echo = FALSE------------------------------------------------------------
adsl_glimpse <- adsl_fct %>%
  select(STUDYID, TRTSDT, TRTEDT, DISONSDT, VISIT1DT, RFENDT)

## ---- echo = FALSE------------------------------------------------------------
glimpse(adsl_glimpse)

## ---- echo = TRUE-------------------------------------------------------------
adsl_type <- xportr_type(.df = adsl_fct, metadata = var_spec, domain = "ADSL", verbose = "warn")

## ---- echo = FALSE------------------------------------------------------------
adsl_type_glimpse <- adsl_type %>%
  select(STUDYID, TRTSDT, TRTEDT, DISONSDT, VISIT1DT, RFENDT)

## ---- echo = TRUE-------------------------------------------------------------
glimpse(adsl_type_glimpse)

## ---- echo = TRUE, error = TRUE-----------------------------------------------
adsl_type <- xportr_type(.df = adsl, metadata = var_spec, domain = "ADSL", verbose = "stop")

## ---- max.height='300px', attr.output='.numberLines', echo = FALSE------------
str(adsl)

## ---- echo = TRUE-------------------------------------------------------------
adsl_length <- xportr_length(.df = adsl, metadata = var_spec, domain = "ADSL", verbose = "warn")

## ---- max.height='300px', attr.output='.numberLines', echo = FALSE------------
str(adsl_length)

## ---- echo = TRUE, error = TRUE-----------------------------------------------
adsl_length <- xportr_length(.df = adsl, metadata = var_spec, domain = "ADSL", verbose = "stop")

## ---- echo = TRUE-------------------------------------------------------------
var_spec_lbl <- var_spec %>%
  mutate(label = if_else(variable == "TRTSDT",
    "Length of variable label must be 40 characters or less", label
  ))

adsl_lbl <- adsl

adsl_lbl <- haven::zap_label(adsl)

## ---- max.height='300px', attr.output='.numberLines', echo = FALSE------------
str(adsl_lbl)

## -----------------------------------------------------------------------------
adsl_lbl <- xportr_label(.df = adsl_lbl, metadata = var_spec_lbl, domain = "ADSL", verbose = "warn")

## ---- max.height='300px', attr.output='.numberLines', echo = FALSE------------
str(adsl_lbl)

## ---- echo = TRUE, error = TRUE-----------------------------------------------
adsl_label <- xportr_label(.df = adsl_lbl, metadata = var_spec_lbl, domain = "ADSL", verbose = "stop")

## -----------------------------------------------------------------------------
adsl_ord <- xportr_order(.df = adsl, metadata = var_spec, domain = "ADSL", verbose = "warn")

## ---- echo = TRUE, error = TRUE-----------------------------------------------
adsl_ord <- xportr_order(.df = adsl, metadata = var_spec, domain = "ADSL", verbose = "stop")

## ---- echo = TRUE-------------------------------------------------------------
adsl_fmt <- adsl %>%
  xportr_type(metadata = var_spec, domain = "ADSL", verbose = "warn") %>%
  xportr_format(metadata = var_spec, domain = "ADSL")

## ---- max.height='300px', attr.output='.numberLines', echo = FALSE------------
str(adsl_fmt)

## ---- echo = TRUE, error = TRUE-----------------------------------------------
adsl %>%
  xportr_metadata(var_spec, "ADSL") %>%
  xportr_type() %>%
  xportr_length() %>%
  xportr_label() %>%
  xportr_order() %>%
  xportr_format() %>%
  xportr_write(path = "adsl.xpt", label = "Subject-Level Analysis Dataset", strict_checks = FALSE)

## ---- echo = TRUE, error = TRUE-----------------------------------------------
adsl %>%
  xportr_write(path = "adsl.xpt", label = "Subject-Level Analysis Dataset", strict_checks = TRUE)

## ---- echo = TRUE, error = TRUE-----------------------------------------------
var_spec_lbl <- var_spec %>%
  mutate(label = if_else(variable == "TRTSDT",
    "Length of variable label must be 40 characters or less", label
  ))


adsl %>%
  xportr_metadata(var_spec_lbl, "ADSL") %>%
  xportr_label() %>%
  xportr_type() %>%
  xportr_format() %>%
  xportr_write(path = "adsl.xpt", label = "Subject-Level Analysis Dataset", strict_checks = TRUE)


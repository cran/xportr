## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = " "
)

options(cli.num_colors = 1)

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

## -----------------------------------------------------------------------------
library(xportr)

## -----------------------------------------------------------------------------
data("adsl_xportr", "var_spec", "dataset_spec", package = "xportr")
colnames(var_spec)

## ----eval = FALSE-------------------------------------------------------------
# xportr_options(
#   xportr.variable_name = "Variable",
#   xportr.label = "Label",
#   xportr.type_name = "Data Type",
#   xportr.format_name = "Format",
#   xportr.length = "Length",
#   xportr.order_name = "Order"
# )
# 
# # Or alternatively
# options(
#   xportr.variable_name = "Variable",
#   xportr.label = "Label",
#   xportr.type_name = "Data Type",
#   xportr.format_name = "Format",
#   xportr.length = "Length",
#   xportr.order_name = "Order"
# )

## ----eval = FALSE-------------------------------------------------------------
# # Default verbose is set to `none`
# xportr_options(
#   xportr.type_verbose = "none",
#   xportr.label_verbose = "none",
#   xportr.length_verbose = "none",
#   xportr.order_verbose = "none",
#   xportr.format_verbose = "none"
# )
# 
# xportr_options(
#   xportr.type_verbose = "message", # Sends a standard message to the console
#   xportr.label_verbose = "message",
#   xportr.length_verbose = "warn", # Sends a warning message to the console
#   xportr.order_verbose = "warn",
#   xportr.format_verbose = "stop" # Stops execution and sends an error message to the console
# )

## ----eval = FALSE-------------------------------------------------------------
# # Tell xportr that "INT" in your spec means integer/numeric
# xportr_options(
#   xportr.numeric_metadata_types = c("integer", "numeric", "num", "float", "INT")
# )

## ----eval = FALSE-------------------------------------------------------------
# library(xportr)
# 
# xportr_options(
#   # Column name mapping for our spec file
#   xportr.variable_name = "Variable",
#   xportr.label = "Label",
#   xportr.type_name = "Data Type",
#   xportr.format_name = "Format",
#   xportr.length = "Length",
#   xportr.order_name = "Order",
#   # Messaging preferences
#   xportr.type_verbose = "message",
#   xportr.label_verbose = "message",
#   xportr.length_verbose = "warn",
#   xportr.order_verbose = "warn",
#   xportr.format_verbose = "none"
# )
# 
# ADSL |>
#   xportr_metadata(var_spec, "ADSL") |>
#   xportr_type() |>
#   xportr_length(length_source = "metadata") |>
#   xportr_label() |>
#   xportr_order() |>
#   xportr_format() |>
#   xportr_df_label(dataset_spec) |>
#   xportr_write("adsl.xpt")

## -----------------------------------------------------------------------------
xportr_options()
getOption("xportr.label")
getOption("xportr.type_verbose")

## ----eval = FALSE-------------------------------------------------------------
# options(xportr.label = "label")
# 
# # Or equivalently
# xportr_options(xportr.label = "label")


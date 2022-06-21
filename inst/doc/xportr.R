## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = " "
)

library(DT)

## ---- include=FALSE-----------------------------------------------------------
local({
  hook_output <- knitr::knit_hooks$get('output')
  knitr::knit_hooks$set(output = function(x, options) {
    if (!is.null(options$max.height)) options$attr.output <- c(
      options$attr.output,
      sprintf('style="max-height: %s;"', options$max.height)
    )
    hook_output(x, options)
  })
})

## ---- eval = TRUE, message = FALSE, warning = FALSE---------------------------
# Loading packages
library(dplyr)
library(labelled)
library(xportr)
library(admiral)

# Loading in our example data
adsl <- admiral::admiral_adsl

## ---- echo = FALSE------------------------------------------------------------
DT::datatable(adsl, options = list(
  autoWidth = FALSE, scrollX = TRUE, pageLength = 5,
  lengthMenu = c(5, 10, 15, 20)
))

## -----------------------------------------------------------------------------
var_spec <- readxl::read_xlsx(
  system.file(paste0("specs/", "ADaM_admiral_spec.xlsx"), package = "xportr"), sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) 
  

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
var_spec_view <- var_spec %>% filter(dataset == "ADSL")

DT::datatable(var_spec_view, options = list(
  autoWidth = FALSE, scrollX = TRUE, pageLength = 5,
  lengthMenu = c(5, 10, 15, 20)
))

## ---- eval = TRUE-------------------------------------------------------------
look_for(adsl, details = TRUE)

## ---- warning=FALSE, message=FALSE, echo = TRUE, results='hide'---------------
adsl_type <- xportr_type(adsl, var_spec, domain = "ADSL", verbose = "message") 

## ---- eval = TRUE-------------------------------------------------------------
look_for(adsl_type, details = TRUE)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  str(adsl)

## ---- max.height='300px', attr.output='.numberLines', echo = FALSE------------
str(adsl)

## -----------------------------------------------------------------------------
adsl_length <- adsl %>% xportr_length(var_spec, domain = "ADSL", "message")

## ---- max.height='300px', attr.output='.numberLines', echo = TRUE-------------
str(adsl_length)

## ---- warning=FALSE, message=FALSE, echo = TRUE, results='hide'---------------
adsl_order <- xportr_order(adsl,var_spec, domain = "ADSL", verbose = "message") 

## ---- echo = FALSE------------------------------------------------------------
DT::datatable(adsl_order, options = list(
  autoWidth = FALSE, scrollX = TRUE, pageLength = 5,
  lengthMenu = c(5, 10, 15, 20)
))

## -----------------------------------------------------------------------------
attr(adsl$TRTSDT, "format.sas")
attr(adsl$TRTEDT, "format.sas")
attr(adsl$TRTSDTM, "format.sas")
attr(adsl$TRTEDTM, "format.sas")

## -----------------------------------------------------------------------------
adsl_fmt <- adsl %>% xportr_format(var_spec, domain = "ADSL", "message")

## -----------------------------------------------------------------------------
attr(adsl_fmt$TRTSDT, "format.sas")
attr(adsl_fmt$TRTEDT, "format.sas")
attr(adsl_fmt$TRTSDTM, "format.sas")
attr(adsl_fmt$TRTEDTM, "format.sas")

## ---- eval = TRUE-------------------------------------------------------------
look_for(adsl, details = FALSE)

## -----------------------------------------------------------------------------
adsl_update <- adsl %>% xportr_label(var_spec, domain = "ADSL", "message")

## -----------------------------------------------------------------------------
look_for(adsl_update, details = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  adsl %>%
#    xportr_type(var_spec, "ADSL", "message") %>%
#    xportr_length(var_spec, "ADSL", "message") %>%
#    xportr_label(var_spec, "ADSL", "message") %>%
#    xportr_order(var_spec, "ADSL", "message") %>%
#    xportr_format(var_spec, "ADSL", "message") %>%
#    xportr_write("adsl.xpt", label = "Subject-Level Analysis Dataset")


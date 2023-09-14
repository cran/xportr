test_that("xportr_df_label: error when metadata is not set", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    SITEID = c(001, 002, 003),
    AGE = c(63, 35, 27),
    SEX = c("M", "F", "M")
  )


  expect_error(
    xportr_df_label(adsl),
    regexp = "Metadata must be set with `metadata` or `xportr_metadata\\(\\)`"
  )
})

test_that("xportr_df_label: use .df name if no 'domain' argument passed", {

  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    SITEID = c(001, 002, 003),
    AGE = c(63, 35, 27),
    SEX = c("M", "F", "M")
  )

  meta <- data.frame(
    dataset = c("df", "ADSL"),
    label = c("foo", "Subject Level")
  )

  meta_var <- data.frame(
    dataset = c("df", "df", "adsl", "adsl"),
    variable = c("USUBJID", "SITEID", "USUBJID", "SITEID"),
    label = c("Subject", "Site", "Subject", "Site")
  )

  adsl_pipe <- adsl %>%
    xportr_df_label(meta)
  adsl_raw <- adsl %>%
    xportr_df_label(meta, domain = "adsl")

  expect_equal(attr(adsl_pipe, "label"), attr(adsl_raw, "label"))


  adsl_pipe2 <- adsl %>%
    xportr_label(meta_var) %>%
    xportr_df_label(meta)

})


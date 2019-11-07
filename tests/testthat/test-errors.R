context("success_fun and error_fun in errors.R")

success_result <- function(verb, the_call, columns, row_redux_call, description) {
  row_redux_message <- ""
  if (!is.na(row_redux_call))
    row_redux_message <- paste0(" on ", row_redux_call, " row reduction")
  msg <- paste0("verification [", the_call, "]", row_redux_message, " passed!")
  success <- list(
    verb = verb,
    message = msg,
    call = the_call,
    columns = columns,
    row_redux_call = row_redux_call,
    description = description
  )
  class(success) <- c("assertr_success", "success", "condition")
  list(success)
}

get_assertr_success <- function(assertion) {
  attr(assertion, "assertr_success")
}

test_that("success_append appends varification result to data", {
  expect_equal(
    get_assertr_success(success_append(mtcars, "method", "rule", "column", "redux", NA)),
    success_result("method", "rule", "column", "redux", NA)
  )
})

test_that("success_report works fine with verification methods", {
  # single assert rule outside chain
  expect_output(
    assert(mtcars, in_set(0, 1), am, success_fun=success_report),
    "assert: verification \\[in_set\\(0, 1\\)\\] passed! Verified columns: am "
  )

  # single assert rule inside chain
  expect_output(
    mtcars %>% chain_start(store_success=TRUE) %>% assert(in_set(0, 1), am) %>% chain_end(success_report),
    "1 result verified: \\nassert: verification \\[in_set\\(0, 1\\)\\] passed! Verified columns: am "
  )

  # single assert rule inside chain without store_success
  expect_output(
    mtcars %>% chain_start %>% assert(in_set(0, 1), am) %>% chain_end(success_report),
    "No success results stored."
  )

  # two assert rules outside chain
  expect_output(
    assert(mtcars, in_set(0, 1), am, success_fun=success_report) %>%
      assert(in_set(0, 1), vs, success_fun=success_report),
    paste0(
      "assert: verification \\[in_set\\(0, 1\\)\\] passed! Verified columns: am \\nassert: verification ",
      "\\[in_set\\(0, 1\\)\\] passed! Verified columns: vs "
    )
  )

  # two assert rules inside chain
  expect_output(
    mtcars %>% chain_start(store_success=TRUE) %>%
      assert(in_set(0, 1), am) %>%
      assert(in_set(0, 1), vs) %>%
      chain_end(success_report),
    paste0(
      "2 results verified: \\nassert: verification \\[in_set\\(0, 1\\)\\] passed! Verified columns: am \\nassert: ",
      "verification \\[in_set\\(0, 1\\)\\] passed! Verified columns: vs "
    )
  )

  # two assert rules inside chain without store_success=TRUE
  expect_output(
    mtcars %>% chain_start() %>%
      assert(in_set(0, 1), am) %>%
      assert(in_set(0, 1), vs) %>%
      chain_end(success_report),
    "No success results stored."
  )

  # single verify rule outside chain
  expect_output(
    mtcars %>% verify(drat > 2, success_fun=success_report),
    "verify: verification \\[drat > 2\\] passed!"
  )

  # single verify rule inside chain
  expect_output(
    mtcars %>% chain_start(store_success=TRUE) %>% verify(drat > 2) %>% chain_end(success_report),
    "1 result verified: \\nverify: verification \\[drat > 2\\] passed!"
  )

  # single verify rule inside chain without store_success
  expect_output(
    mtcars %>% chain_start %>% verify(drat > 2) %>% chain_end(success_report),
    "No success results stored."
  )

  # two verify rules outside chain
  expect_output(
    mtcars %>%
    verify(drat > 2, success_fun=success_report) %>%
      verify(am %in% c(0, 1), success_fun=success_report),
    "verify: verification \\[drat > 2\\] passed!\\nverify: verification \\[am %in% c\\(0, 1\\)\\] passed!"
  )

  # two verify rules inside chain
  expect_output(
    mtcars %>% chain_start(store_success=TRUE) %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(success_report),
    "2 results verified: \\nverify: verification \\[drat > 2\\] passed!\\nverify: verification \\[am %in% c\\(0, 1\\)\\] passed!"
  )

  # two verify rules inside chain without store_success=TRUE
  expect_output(
    mtcars %>% chain_start() %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(success_report),
    "No success results stored."
  )

  # single assert_rows rule outside chain
  expect_output(
    assert_rows(mtcars, rowSums, within_bounds(0,2), vs, am, success_fun=success_report),
    "assert_rows: verification \\[within_bounds\\(0, 2\\)\\] on rowSums row reduction passed! Verified columns: vs am "
  )

  # single assert_rows rule inside chain
  expect_output(
    mtcars %>% chain_start(store_success=TRUE) %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am, success_fun=success_report) %>%
      chain_end(success_report),
    paste0(
      "1 result verified: \\nassert_rows: verification \\[within_bounds\\(0, 2\\)\\] on rowSums row reduction passed! ",
      "Verified columns: vs am ")
  )

  # single assert_rows rule inside chain without store_success
  expect_output(
    mtcars %>% chain_start %>% assert_rows(rowSums, within_bounds(0,2), vs, am) %>% chain_end(success_report),
    "No success results stored."
  )

  # two assert_rows rules outside chain
  expect_output(
    mtcars %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am, success_fun=success_report) %>%
      assert_rows(num_row_NAs, within_bounds(0,.1), vs, am, success_fun=success_report),
    paste0(
      "assert_rows: verification \\[within_bounds\\(0, 2\\)\\] on rowSums row reduction passed! Verified columns: vs am ",
      "\\nassert_rows: verification \\[within_bounds\\(0, 0.1\\)\\] on num_row_NAs row reduction passed! Verified columns: vs am "
    )

  )

  # two assert_rows rules inside chain
  expect_output(
    mtcars %>% chain_start(store_success=TRUE) %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am, success_fun=success_report) %>%
      assert_rows(num_row_NAs, within_bounds(0,.1), vs, am, success_fun=success_report) %>%
      chain_end(success_report),
    paste0(
      "2 results verified: \\nassert_rows: verification \\[within_bounds\\(0, 2\\)\\] on rowSums row reduction passed! ",
      "Verified columns: vs am \\nassert_rows: verification \\[within_bounds\\(0, 0.1\\)\\] on num_row_NAs row reduction ",
      "passed! Verified columns: vs am "
    )
  )

  # two assert_rows rules inside chain without store_success=TRUE
  expect_output(
    mtcars %>% chain_start() %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(success_report),
    "No success results stored."
  )

  # single insist rule outside chain
  expect_output(
    insist(mtcars, within_n_sds(5), vs, success_fun=success_report),
    "insist: verification \\[within_n_sds\\(5\\)\\] passed! Verified columns: vs "
  )

  # single insist rule inside chain
  expect_output(
    mtcars %>% chain_start(store_success=TRUE) %>%
      insist(within_n_sds(5), vs, success_fun=success_report) %>%
      chain_end(success_report),
    "1 result verified: \\ninsist: verification \\[within_n_sds\\(5\\)\\] passed! Verified columns: vs "
  )

  # single insist rule inside chain without store_success
  expect_output(
    mtcars %>% chain_start %>% insist(within_n_sds(5), vs) %>% chain_end(success_report),
    "No success results stored."
  )

  # two insist rules outside chain
  expect_output(
    mtcars %>%
      insist(within_n_sds(5), vs, success_fun=success_report) %>%
      insist(within_n_sds(5), am, success_fun=success_report),
    paste0(
      "insist: verification \\[within_n_sds\\(5\\)\\] passed! Verified columns: vs \\n",
      "insist: verification \\[within_n_sds\\(5\\)\\] passed! Verified columns: am "
    )
  )

  # two insist rules inside chain
  expect_output(
    mtcars %>% chain_start(store_success=TRUE) %>%
      insist(within_n_sds(5), vs, success_fun=success_report) %>%
      insist(within_n_sds(5), am, success_fun=success_report) %>%
      chain_end(success_report),
    paste0(
      "2 results verified: \\n",
      "insist: verification \\[within_n_sds\\(5\\)\\] passed! Verified columns: vs \\n",
      "insist: verification \\[within_n_sds\\(5\\)\\] passed! Verified columns: am "
    )
  )

  # two insist rules inside chain without store_success=TRUE
  expect_output(
    mtcars %>% chain_start() %>%
      insist(within_n_sds(5), vs) %>%
      insist(within_n_sds(5), am) %>%
      chain_end(success_report),
    "No success results stored."
  )

  # single insist_rows rule outside chain
  expect_output(
    insist_rows(iris, maha_dist, within_n_sds(6), Sepal.Length:Petal.Length, success_fun=success_report),
    "insist_rows: verification \\[within_n_sds\\(6\\)\\] on maha_dist row reduction passed! Verified columns: Sepal.Length Sepal.Width Petal.Length "
  )

  # single insist_rows rule inside chain
  expect_output(
    iris %>% chain_start(store_success=TRUE) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      chain_end(success_report),
    paste0(
      "1 result verified: \\ninsist_rows: verification \\[within_n_sds\\(6\\)\\] on maha_dist row reduction passed! ",
      "Verified columns: Sepal.Length Sepal.Width Petal.Length "
    )
  )

  # single insist_rows rule inside chain without store_success
  expect_output(
    iris %>% chain_start %>% insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>% chain_end(success_report),
    "No success results stored."
  )

  # two insist_rows rules outside chain
  expect_output(
    iris %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length, success_fun=success_report) %>%
      insist_rows(maha_dist, within_n_sds(7), Sepal.Length:Petal.Length, success_fun=success_report),
    paste0(
      "insist_rows: verification \\[within_n_sds\\(6\\)\\] on maha_dist row reduction passed! ",
      "Verified columns: Sepal.Length Sepal.Width Petal.Length \\n",
      "insist_rows: verification \\[within_n_sds\\(7\\)\\] on maha_dist row reduction passed! ",
      "Verified columns: Sepal.Length Sepal.Width Petal.Length "
    )
  )

  # two insist_rows rules inside chain
  expect_output(
    iris %>% chain_start(store_success=TRUE) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      insist_rows(maha_dist, within_n_sds(7), Sepal.Length:Petal.Length) %>%
      chain_end(success_report),
    paste0(
      "2 results verified: \\n",
      "insist_rows: verification \\[within_n_sds\\(6\\)\\] on maha_dist row reduction passed! ",
      "Verified columns: Sepal.Length Sepal.Width Petal.Length \\n",
      "insist_rows: verification \\[within_n_sds\\(7\\)\\] on maha_dist row reduction passed! ",
      "Verified columns: Sepal.Length Sepal.Width Petal.Length "
    )
  )

  # two insist_rows rules inside chain without store_success=TRUE
  expect_output(
    iris %>% chain_start() %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      insist_rows(maha_dist, within_n_sds(7), Sepal.Length:Petal.Length) %>%
      chain_end(success_report),
    "No success results stored."
  )

})

success_df <- function(verb, message, call, columns, row_redux_call, description) {
  data.frame(
    verb = verb,
    message = message,
    call = call,
    columns = columns,
    row_redux_call = row_redux_call,
    description = description,
    stringsAsFactors = FALSE
  )
}

test_that("success_df_return works fine with verification methods", {
  # single assert rule outside chain
  expect_equal(
    assert(mtcars, in_set(0, 1), am, success_fun=success_df_return),
    success_df("assert", "verification [in_set(0, 1)] passed!", "in_set(0, 1)", "am", NA, NA)
  )

  # single assert rule inside chain
  expect_equal(
    mtcars %>% chain_start(store_success=TRUE) %>% assert(in_set(0, 1), am) %>% chain_end(success_df_return),
    success_df("assert", "verification [in_set(0, 1)] passed!", "in_set(0, 1)", "am", NA, NA)
  )

  # single assert rule inside chain without store_success
  expect_error(
    mtcars %>% chain_start %>% assert(in_set(0, 1), am) %>% chain_end(success_df_return),
    "No success results stored."
  )

  # two assert rules inside chain
  expect_equal(
    mtcars %>% chain_start(store_success=TRUE) %>%
      assert(in_set(0, 1), am) %>%
      assert(in_set(0, 1), vs) %>%
      chain_end(success_df_return),
    success_df(
      rep("assert", 2), rep("verification [in_set(0, 1)] passed!", 2),
      rep("in_set(0, 1)", 2), c("am", "vs"), rep(NA, 2), rep(NA, 2))
  )

  # two assert rules inside chain without store_success=TRUE
  expect_error(
    mtcars %>% chain_start() %>%
      assert(in_set(0, 1), am) %>%
      assert(in_set(0, 1), vs) %>%
      chain_end(success_df_return),
    "No success results stored."
  )

  # single verify rule outside chain
  expect_equal(
    mtcars %>% verify(drat > 2, success_fun=success_df_return),
    success_df("verify", "verification [drat > 2] passed!", "drat > 2", NA, NA, NA)
  )

  # single verify rule inside chain
  expect_equal(
    mtcars %>% chain_start(store_success=TRUE) %>% verify(drat > 2) %>% chain_end(success_df_return),
    success_df("verify", "verification [drat > 2] passed!", "drat > 2", NA, NA, NA)
  )

  # single verify rule inside chain without store_success
  expect_error(
    mtcars %>% chain_start %>% verify(drat > 2) %>% chain_end(success_df_return),
    "No success results stored."
  )

  # two verify rules inside chain
  expect_equal(
    mtcars %>% chain_start(store_success=TRUE) %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(success_df_return),
    success_df(
      rep("verify", 2), c("verification [drat > 2] passed!", "verification [am %in% c(0, 1)] passed!"),
      c("drat > 2", "am %in% c(0, 1)"), rep(NA, 2), rep(NA, 2), rep(NA, 2))
  )

  # two verify rules inside chain without store_success=TRUE
  expect_error(
    mtcars %>% chain_start() %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(success_df_return),
    "No success results stored."
  )

  # single assert_rows rule outside chain
  expect_equal(
    assert_rows(mtcars, rowSums, within_bounds(0,2), vs, am, success_fun=success_df_return),
    success_df(
      "assert_rows", "verification [within_bounds(0, 2)] on rowSums row reduction passed!",
      "within_bounds(0, 2)", "vs, am", "rowSums", NA)
  )

  # single assert_rows rule inside chain
  expect_equal(
    mtcars %>% chain_start(store_success=TRUE) %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am) %>%
      chain_end(success_df_return),
    success_df(
      "assert_rows", "verification [within_bounds(0, 2)] on rowSums row reduction passed!",
      "within_bounds(0, 2)", "vs, am", "rowSums", NA)
  )

  # single assert_rows rule inside chain without store_success
  expect_error(
    mtcars %>% chain_start %>% assert_rows(rowSums, within_bounds(0,2), vs, am) %>% chain_end(success_df_return),
    "No success results stored."
  )

  # two assert_rows rules inside chain
  expect_equal(
    mtcars %>% chain_start(store_success=TRUE) %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am) %>%
      assert_rows(num_row_NAs, within_bounds(0,.1), vs, am) %>%
      chain_end(success_df_return),
    success_df(
      rep("assert_rows", 2),
      c("verification [within_bounds(0, 2)] on rowSums row reduction passed!", "verification [within_bounds(0, 0.1)] on num_row_NAs row reduction passed!"),
      c("within_bounds(0, 2)", "within_bounds(0, 0.1)"), rep("vs, am", 2), c("rowSums", "num_row_NAs"), rep(NA, 2)
    )
  )

  # two assert_rows rules inside chain without store_success=TRUE
  expect_error(
    mtcars %>% chain_start() %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(success_df_return),
    "No success results stored."
  )

  # single insist rule outside chain
  expect_equal(
    insist(mtcars, within_n_sds(5), vs, success_fun=success_df_return),
    success_df("insist", "verification [within_n_sds(5)] passed!", "within_n_sds(5)", "vs", NA, NA)
  )

  # single insist rule inside chain
  expect_equal(
    mtcars %>% chain_start(store_success=TRUE) %>%
      insist(within_n_sds(5), vs) %>%
      chain_end(success_df_return),
    success_df("insist", "verification [within_n_sds(5)] passed!", "within_n_sds(5)", "vs", NA, NA)
  )

  # single insist rule inside chain without store_success
  expect_error(
    mtcars %>% chain_start %>% insist(within_n_sds(5), vs) %>% chain_end(success_df_return),
    "No success results stored."
  )

  # two insist rules inside chain
  expect_equal(
    mtcars %>% chain_start(store_success=TRUE) %>%
      insist(within_n_sds(5), vs) %>%
      insist(within_n_sds(5), am) %>%
      chain_end(success_df_return),
    success_df(
      rep("insist", 2), rep("verification [within_n_sds(5)] passed!", 2), rep("within_n_sds(5)", 2), c("vs", "am"), rep(NA, 2), rep(NA, 2)
    )
  )

  # two insist rules inside chain without store_success=TRUE
  expect_error(
    mtcars %>% chain_start() %>%
      insist(within_n_sds(5), vs) %>%
      insist(within_n_sds(5), am) %>%
      chain_end(success_df_return),
    "No success results stored."
  )

  # single insist_rows rule outside chain
  expect_equal(
    insist_rows(iris, maha_dist, within_n_sds(6), Sepal.Length:Petal.Length, success_fun=success_df_return),
    success_df("insist_rows", "verification [within_n_sds(6)] on maha_dist row reduction passed!", "within_n_sds(6)",
               "Sepal.Length, Sepal.Width, Petal.Length", "maha_dist", NA)
  )

  # single insist_rows rule inside chain
  expect_equal(
    iris %>% chain_start(store_success=TRUE) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      chain_end(success_df_return),
    success_df("insist_rows", "verification [within_n_sds(6)] on maha_dist row reduction passed!", "within_n_sds(6)",
               "Sepal.Length, Sepal.Width, Petal.Length", "maha_dist", NA)
  )

  # single insist_rows rule inside chain without store_success
  expect_error(
    iris %>% chain_start %>% insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>% chain_end(success_df_return),
    "No success results stored."
  )

  # two insist_rows rules inside chain
  expect_equal(
    iris %>% chain_start(store_success=TRUE) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      insist_rows(maha_dist, within_n_sds(7), Sepal.Length:Petal.Length) %>%
      chain_end(success_df_return),
    rbind(
      success_df("insist_rows", "verification [within_n_sds(6)] on maha_dist row reduction passed!", "within_n_sds(6)",
                 "Sepal.Length, Sepal.Width, Petal.Length", "maha_dist", NA),
      success_df("insist_rows", "verification [within_n_sds(7)] on maha_dist row reduction passed!", "within_n_sds(7)",
                 "Sepal.Length, Sepal.Width, Petal.Length", "maha_dist", NA)
    )
  )

  # two insist_rows rules inside chain without store_success=TRUE
  expect_error(
    iris %>% chain_start %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      insist_rows(maha_dist, within_n_sds(7), Sepal.Length:Petal.Length) %>%
      chain_end(success_df_return),
    "No success results stored."
  )

})


# defects

defect_result <- function(verb, the_call, columns, row_redux_call, description) {
  row_redux_message <- ""
  if (!is.na(row_redux_call))
    row_redux_message <- paste0(" on ", row_redux_call, " row reduction")
  msg <- paste0("verification [", the_call, "]", row_redux_message, " omitted due to data defect!")
  defect <- list(
    verb = verb,
    message = msg,
    call = the_call,
    columns = columns,
    row_redux_call = row_redux_call,
    description = description
  )
  class(defect) <- c("assertr_defect", "defect", "condition")
  list(defect)
}

get_assertr_defect <- function(assertion) {
  attr(assertion, "assertr_defect")
}

test_that("success_defect appends omitted verification due to defect of data", {
  expect_equal(
    get_assertr_defect(defect_append(NULL, mtcars, "method", "rule", "column", "redux", NA)),
    defect_result("method", "rule", "column", "redux", NA)
  )
})

test_that("defect_report works fine with verification methods", {
  defective_data <- mtcars %>%
    assert(in_set(0, 2), vs, obligatory=TRUE, error_fun=error_append)
  defective_data_in_chain <- mtcars %>% chain_start() %>%
    assert(in_set(0, 2), vs, obligatory=TRUE)
  not_defective_data <- mtcars %>%
    assert(in_set(0, 2), vs, error_fun=error_append)
  not_defective_data_in_chain <- mtcars %>% chain_start() %>%
    assert(in_set(0, 2), vs)

  # single assert rule on defective data outside chain
  expect_output(
    defective_data %>%
      assert(in_set(0, 1), am, defect_fun=defect_report),
    "assert: verification \\[in_set\\(0, 1\\)\\] omitted due to data defect! Columns passed to assertion: am "
  )

  # single assert rule on defective data inside chain
  expect_output(
    defective_data_in_chain %>%
      assert(in_set(0, 1), am) %>%
      chain_end(error_fun=defect_report),
    "1 assertion omitted: \\nassert: verification \\[in_set\\(0, 1\\)\\] omitted due to data defect! Columns passed to assertion: am "
  )

  # single assert rule inside chain without marking rule as obligatory
  expect_output(
    not_defective_data_in_chain %>% chain_end(error_fun=defect_report),
    "No rules run on defective data."
  )

  # two assert rules on defective data outside chain
  expect_output(
    defective_data %>%
      assert(in_set(0, 1), am, defect_fun=defect_report) %>%
      assert(in_set(0, 1), vs, defect_fun=defect_report),
    paste0(
      "assert: verification \\[in_set\\(0, 1\\)\\] omitted due to data defect! Columns passed to assertion: am \\n",
      "assert: verification \\[in_set\\(0, 1\\)\\] omitted due to data defect! Columns passed to assertion: vs "
    )
  )

  # two assert rule on defective data inside chain
  expect_output(
    defective_data_in_chain %>%
      assert(in_set(0, 1), am) %>%
      assert(in_set(0, 1), vs) %>%
      chain_end(error_fun=defect_report),
    paste0(
      "2 assertions omitted: \\n",
      "assert: verification \\[in_set\\(0, 1\\)\\] omitted due to data defect! Columns passed to assertion: am \\n",
      "assert: verification \\[in_set\\(0, 1\\)\\] omitted due to data defect! Columns passed to assertion: vs "
    )
  )

  # two assert rule on defective data inside chain without store_success=TRUE
  expect_output(
    not_defective_data_in_chain %>%
      assert(in_set(0, 1), vs) %>%
      chain_end(error_fun=defect_report),
    "No rules run on defective data."
  )

  # single verify rule on defective data outside chain
  expect_output(
    defective_data %>%
      verify(drat > 2, defect_fun=defect_report),
    "verify: verification \\[drat > 2\\] omitted due to data defect!"
  )

  # single verify rule on defective data inside chain
  expect_output(
    defective_data_in_chain %>% verify(drat > 2) %>% chain_end(error_fun=defect_report),
    "1 assertion omitted: \\nverify: verification \\[drat > 2\\] omitted due to data defect!"
  )

  # single verify rule on not defective data inside chain
  expect_output(
    not_defective_data_in_chain %>% verify(drat > 2) %>% chain_end(error_fun=defect_report),
    "No rules run on defective data."
  )

  # two verify rules on defective data outside chain
  expect_output(
    defective_data %>%
      verify(drat > 2, defect_fun=defect_report) %>%
      verify(am %in% c(0, 1), defect_fun=defect_report),
    "verify: verification \\[drat > 2\\] omitted due to data defect!\\nverify: verification \\[am %in% c\\(0, 1\\)\\] omitted due to data defect!"
  )

  # two verify rules on defective data inside chain
  expect_output(
    defective_data_in_chain %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(error_fun=defect_report),
    paste0(
      "2 assertions omitted: \\n",
      "verify: verification \\[drat > 2\\] omitted due to data defect!\\n",
      "verify: verification \\[am %in% c\\(0, 1\\)\\] omitted due to data defect!"
    )
  )

  # two verify rules on not defective data inside chain
  expect_output(
    not_defective_data_in_chain %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(error_fun=defect_report),
    "No rules run on defective data."
  )

  # single assert_rows defective rule outside chain
  expect_output(
    defective_data %>%
    assert_rows(rowSums, within_bounds(0,2), vs, am, defect_fun=defect_report),
    "assert_rows: verification \\[within_bounds\\(0, 2\\)\\] on rowSums row reduction omitted due to data defect! Columns passed to assertion: vs am "
  )

  # single assert_rows defective rule inside chain
  expect_output(
    defective_data_in_chain %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am) %>%
      chain_end(error_fun=defect_report),
    paste0(
      "1 assertion omitted: \\nassert_rows: verification \\[within_bounds\\(0, 2\\)\\] on rowSums row reduction omitted due to data defect! ",
      "Columns passed to assertion: vs am ")
  )

  # single assert_rows on not defective data inside chain
  expect_output(
    not_defective_data_in_chain %>% assert_rows(rowSums, within_bounds(0,2), vs, am) %>% chain_end(error_fun=defect_report),
    "No rules run on defective data."
  )

  # two assert_rows rule on defective data outside chain
  expect_output(
    defective_data %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am, defect_fun=defect_report) %>%
      assert_rows(num_row_NAs, within_bounds(0,.1), vs, am, defect_fun=defect_report),
    paste0(
      "assert_rows: verification \\[within_bounds\\(0, 2\\)\\] on rowSums row reduction omitted due to data defect! Columns passed to assertion: vs am ",
      "\\nassert_rows: verification \\[within_bounds\\(0, 0.1\\)\\] on num_row_NAs row reduction omitted due to data defect! Columns passed to assertion: vs am "
    )
  )

  # two assert_rows rules on defective data inside chain
  expect_output(
    defective_data_in_chain %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am) %>%
      assert_rows(num_row_NAs, within_bounds(0,.1), vs, am) %>%
      chain_end(error_fun=defect_report),
    paste0(
      "2 assertions omitted: \\nassert_rows: verification \\[within_bounds\\(0, 2\\)\\] on rowSums row reduction omitted due to data defect! ",
      "Columns passed to assertion: vs am \\nassert_rows: verification \\[within_bounds\\(0, 0.1\\)\\] on num_row_NAs row reduction ",
      "omitted due to data defect! Columns passed to assertion: vs am "
    )
  )

  # two assert_rows rule on not defective data inside chain
  expect_output(
    not_defective_data_in_chain %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am) %>%
      assert_rows(num_row_NAs, within_bounds(0,.1), vs, am) %>%
      chain_end(error_fun=defect_report),
    "No rules run on defective data."
  )

  # single insist rule on defective data outside chain
  expect_output(
    defective_data %>%
      insist(within_n_sds(5), vs, defect_fun=defect_report),
    "insist: verification \\[within_n_sds\\(5\\)\\] omitted due to data defect! Columns passed to assertion: vs "
  )

  # single insist rule on defective data inside chain
  expect_output(
    defective_data_in_chain %>%
      insist(within_n_sds(5), vs) %>%
      chain_end(error_fun=defect_report),
    "1 assertion omitted: \\ninsist: verification \\[within_n_sds\\(5\\)\\] omitted due to data defect! Columns passed to assertion: vs "
  )

  # single insist rule on non defective data inside chain
  expect_output(
    not_defective_data_in_chain %>% insist(within_n_sds(5), vs) %>% chain_end(error_fun=defect_report),
    "No rules run on defective data."
  )

  # two insist rule on defective data outside chain
  expect_output(
    defective_data %>%
      insist(within_n_sds(5), vs, defect_fun=defect_report) %>%
      insist(within_n_sds(5), am, defect_fun=defect_report),
    paste0(
      "insist: verification \\[within_n_sds\\(5\\)\\] omitted due to data defect! Columns passed to assertion: vs \\n",
      "insist: verification \\[within_n_sds\\(5\\)\\] omitted due to data defect! Columns passed to assertion: am "
    )
  )

  # two insist rule on defective data inside chain
  expect_output(
    defective_data_in_chain %>%
      insist(within_n_sds(5), vs) %>%
      insist(within_n_sds(5), am) %>%
      chain_end(error_fun=defect_report),
    paste0(
      "2 assertions omitted: \\n",
      "insist: verification \\[within_n_sds\\(5\\)\\] omitted due to data defect! Columns passed to assertion: vs \\n",
      "insist: verification \\[within_n_sds\\(5\\)\\] omitted due to data defect! Columns passed to assertion: am "
    )
  )

  # two insist rule on non defective data inside chain
  expect_output(
    not_defective_data_in_chain %>%
      insist(within_n_sds(5), vs) %>%
      insist(within_n_sds(5), am) %>%
      chain_end(error_fun=defect_report),
    "No rules run on defective data."
  )

  # single insist_rows on defective data outside chain
  expect_output(
    iris %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, obligatory=TRUE, error_fun=error_append) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length, defect_fun=defect_report),
    "insist_rows: verification \\[within_n_sds\\(6\\)\\] on maha_dist row reduction omitted due to data defect! Columns passed to assertion: Sepal.Length:Petal.Length "
  )

  # single insist_rows on defective data inside chain
  expect_output(
    iris %>% chain_start %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, obligatory=TRUE) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      chain_end(error_fun=defect_report),
    paste0(
      "1 assertion omitted: \\ninsist_rows: verification \\[within_n_sds\\(6\\)\\] on maha_dist row reduction omitted due to data defect! ",
      "Columns passed to assertion: Sepal.Length:Petal.Length "
    )
  )

  # single insist_rows on not defective data inside chain
  expect_output(
    iris %>% chain_start %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, error_fun=error_append) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>% chain_end(error_fun=defect_report),
    "No rules run on defective data."
  )

  # two insist_rows rule on not defective data outside chain
  expect_silent(
    iris %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, error_fun=error_append) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length, defect_fun=defect_report) %>%
      insist_rows(maha_dist, within_n_sds(7), Sepal.Length:Petal.Length, defect_fun=defect_report)
  )

  # two insist_rows rule on defective data inside chain
  expect_output(
    iris %>% chain_start %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, obligatory=TRUE, error_fun=error_append) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      insist_rows(maha_dist, within_n_sds(7), Sepal.Length:Petal.Length) %>%
      chain_end(error_fun=defect_report),
    paste0(
      "2 assertions omitted: \\n",
      "insist_rows: verification \\[within_n_sds\\(6\\)\\] on maha_dist row reduction omitted due to data defect! ",
      "Columns passed to assertion: Sepal.Length:Petal.Length \\n",
      "insist_rows: verification \\[within_n_sds\\(7\\)\\] on maha_dist row reduction omitted due to data defect! ",
      "Columns passed to assertion: Sepal.Length:Petal.Length "
    )
  )

  # two insist_rows rule on non defective data inside chain
  expect_output(
    iris %>% chain_start %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, error_fun=error_append) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      insist_rows(maha_dist, within_n_sds(7), Sepal.Length:Petal.Length) %>%
      chain_end(error_fun=defect_report),
    "No rules run on defective data."
  )

})

defect_df <- function(verb, message, call, columns, row_redux_call, description) {
  data.frame(
    verb = verb,
    message = message,
    call = call,
    columns = columns,
    row_redux_call = row_redux_call,
    description = description,
    stringsAsFactors = FALSE
  )
}

test_that("defect_df_return works fine with verification methods", {
  defective_data <- mtcars %>%
    assert(in_set(0, 2), vs, obligatory=TRUE, error_fun=error_append)
  defective_data_in_chain <- mtcars %>% chain_start() %>%
    assert(in_set(0, 2), vs, obligatory=TRUE)
  not_defective_data <- mtcars %>%
    assert(in_set(0, 2), vs, error_fun=error_append)
  not_defective_data_in_chain <- mtcars %>% chain_start() %>%
    assert(in_set(0, 2), vs)

  # single assert rule outside chain
  expect_equal(
    defective_data %>%
    assert(in_set(0, 1), am, defect_fun=defect_df_return),
    defect_df("assert", "verification [in_set(0, 1)] omitted due to data defect!", "in_set(0, 1)", "am", NA, NA)
  )

  # single assert rule inside chain
  expect_equal(
    defective_data_in_chain %>% assert(in_set(0, 1), am) %>% chain_end(error_fun=defect_df_return),
    defect_df("assert", "verification [in_set(0, 1)] omitted due to data defect!", "in_set(0, 1)", "am", NA, NA)
  )

  # single assert rule on not defective data inside chain
  expect_error(
    not_defective_data_in_chain %>% assert(in_set(0, 1), am) %>% chain_end(error_fun=defect_df_return),
    "No rules run on defective data."
  )

  # two assert rules on defective data inside chain
  expect_equal(
    defective_data_in_chain %>%
      assert(in_set(0, 1), am) %>%
      assert(in_set(0, 1), vs) %>%
      chain_end(error_fun=defect_df_return),
    defect_df(
      rep("assert", 2), rep("verification [in_set(0, 1)] omitted due to data defect!", 2),
      rep("in_set(0, 1)", 2), c("am", "vs"), rep(NA, 2), rep(NA, 2))
  )

  # two assert rules on not defective data inside chain
  expect_error(
    not_defective_data_in_chain %>%
      assert(in_set(0, 1), am) %>%
      assert(in_set(0, 1), vs) %>%
      chain_end(error_fun=defect_df_return),
    "No rules run on defective data."
  )

  # single verify rule on defective data outside chain
  expect_equal(
    defective_data %>% verify(drat > 2, defect_fun=defect_df_return),
    defect_df("verify", "verification [drat > 2] omitted due to data defect!", "drat > 2", NA, NA, NA)
  )

  # single verify rule on defective data inside chain
  expect_equal(
    defective_data_in_chain %>% verify(drat > 2) %>% chain_end(error_fun=defect_df_return),
    defect_df("verify", "verification [drat > 2] omitted due to data defect!", "drat > 2", NA, NA, NA)
  )

  # single verify rule on not defective data inside chain without store_defect
  expect_error(
    not_defective_data_in_chain %>% verify(drat > 2) %>% chain_end(error_fun=defect_df_return),
    "No rules run on defective data."
  )

  # two verify rules on defective data inside chain
  expect_equal(
    defective_data_in_chain %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(error_fun=defect_df_return),
    defect_df(
      rep("verify", 2), c("verification [drat > 2] omitted due to data defect!", "verification [am %in% c(0, 1)] omitted due to data defect!"),
      c("drat > 2", "am %in% c(0, 1)"), rep(NA, 2), rep(NA, 2), rep(NA, 2))
  )

  # two verify rules on not defective data inside
  expect_error(
    not_defective_data_in_chain %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(error_fun=defect_df_return),
    "No rules run on defective data."
  )

  # single assert_rows rule on defective data outside chain
  expect_equal(
    defective_data %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am, defect_fun=defect_df_return),
    defect_df("assert_rows", "verification [within_bounds(0, 2)] on rowSums row reduction omitted due to data defect!",
              "within_bounds(0, 2)", "vs, am", "rowSums", NA)
  )

  # single assert_rows rule on defective data inside chain
  expect_equal(
    defective_data_in_chain %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am) %>%
      chain_end(error_fun=defect_df_return),
    defect_df("assert_rows", "verification [within_bounds(0, 2)] on rowSums row reduction omitted due to data defect!",
              "within_bounds(0, 2)", "vs, am", "rowSums", NA)
  )

  # single assert_rows rule on defective data inside chain without store_defect
  expect_error(
    not_defective_data_in_chain %>% assert_rows(rowSums, within_bounds(0,2), vs, am) %>% chain_end(error_fun=defect_df_return),
    "No rules run on defective data."
  )

  # two assert_rows rules on defective data inside chain
  expect_equal(
    defective_data_in_chain %>%
      assert_rows(rowSums, within_bounds(0,2), vs, am) %>%
      assert_rows(num_row_NAs, within_bounds(0,.1), vs, am) %>%
      chain_end(error_fun=defect_df_return),
    defect_df(
      rep("assert_rows", 2),
      c("verification [within_bounds(0, 2)] on rowSums row reduction omitted due to data defect!", "verification [within_bounds(0, 0.1)] on num_row_NAs row reduction omitted due to data defect!"),
      c("within_bounds(0, 2)", "within_bounds(0, 0.1)"), rep("vs, am", 2), c("rowSums", "num_row_NAs"), rep(NA, 2)
    )
  )

  # two assert_rows rules on defective data inside chain without store_defect=TRUE
  expect_error(
    not_defective_data_in_chain %>%
      verify(drat > 2) %>%
      verify(am %in% c(0, 1)) %>%
      chain_end(error_fun=defect_df_return),
    "No rules run on defective data."
  )

  # single insist rule on defective data outside chain
  expect_equal(
    defective_data %>%
      insist(within_n_sds(5), vs, defect_fun=defect_df_return),
    defect_df("insist", "verification [within_n_sds(5)] omitted due to data defect!", "within_n_sds(5)", "vs", NA, NA)
  )

  # single insist rule on defective data inside chain
  expect_equal(
    defective_data_in_chain %>%
      insist(within_n_sds(5), vs) %>%
      chain_end(error_fun=defect_df_return),
    defect_df("insist", "verification [within_n_sds(5)] omitted due to data defect!", "within_n_sds(5)", "vs", NA, NA)
  )

  # single insist rule on defective data inside chain without store_defect
  expect_error(
    not_defective_data_in_chain %>% insist(within_n_sds(5), vs) %>% chain_end(error_fun=defect_df_return),
    "No rules run on defective data."
  )

  # two insist rules on defective data inside chain
  expect_equal(
    defective_data_in_chain %>%
      insist(within_n_sds(5), vs) %>%
      insist(within_n_sds(5), am) %>%
      chain_end(error_fun=defect_df_return),
    defect_df(
      rep("insist", 2), rep("verification [within_n_sds(5)] omitted due to data defect!", 2), rep("within_n_sds(5)", 2), c("vs", "am"), rep(NA, 2), rep(NA, 2)
    )
  )

  # two insist rules on defective data inside chain without store_defect=TRUE
  expect_error(
    not_defective_data_in_chain %>%
      insist(within_n_sds(5), vs) %>%
      insist(within_n_sds(5), am) %>%
      chain_end(error_fun=defect_df_return),
    "No rules run on defective data."
  )

  # single insist_rows rule on defective data outside chain
  expect_equal(
    iris %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, obligatory=TRUE, error_fun=error_append) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length, defect_fun=defect_df_return),
    defect_df("insist_rows", "verification [within_n_sds(6)] on maha_dist row reduction omitted due to data defect!", "within_n_sds(6)",
               "Sepal.Length:Petal.Length", "maha_dist", NA)
  )

  # single insist_rows rule on defective data inside chain
  expect_equal(
    iris %>% chain_start %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, obligatory=TRUE, error_fun=error_append) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      chain_end(error_fun=defect_df_return),
    defect_df("insist_rows", "verification [within_n_sds(6)] on maha_dist row reduction omitted due to data defect!", "within_n_sds(6)",
               "Sepal.Length:Petal.Length", "maha_dist", NA)
  )

  # single insist_rows rule on defective data inside chain without store_defect
  expect_error(
    iris %>% chain_start %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, error_fun=error_append) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>% chain_end(error_fun=defect_df_return),
    "No rules run on defective data."
  )

  # two insist_rows rules on defective data inside chain
  expect_equal(
    iris %>% chain_start %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, obligatory=TRUE, error_fun=error_append) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      insist_rows(maha_dist, within_n_sds(7), Sepal.Length:Petal.Length) %>%
      chain_end(error_fun=defect_df_return),
    rbind(
      defect_df("insist_rows", "verification [within_n_sds(6)] on maha_dist row reduction omitted due to data defect!", "within_n_sds(6)",
                 "Sepal.Length:Petal.Length", "maha_dist", NA),
      defect_df("insist_rows", "verification [within_n_sds(7)] on maha_dist row reduction omitted due to data defect!", "within_n_sds(7)",
                 "Sepal.Length:Petal.Length", "maha_dist", NA)
    )
  )

  # two insist_rows rules on defective data inside chain without store_defect=TRUE
  expect_error(
    iris %>% chain_start %>%
      insist_rows(maha_dist, within_n_sds(3), Sepal.Length:Petal.Length, error_fun=error_append) %>%
      insist_rows(maha_dist, within_n_sds(6), Sepal.Length:Petal.Length) %>%
      insist_rows(maha_dist, within_n_sds(7), Sepal.Length:Petal.Length) %>%
      chain_end(error_fun=defect_df_return),
    "No rules run on defective data."
  )

})

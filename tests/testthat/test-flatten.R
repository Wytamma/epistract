test_that("flatten_epi_columns flattens nested objects with dot notation", {
  nested <- tibble::tibble(
    source_name = "WHO",
    outbreak = tibble::tibble(
      event = list(list(disease = "cholera")),
      counts = list(list(confirmed = 14L, deaths = 2L))
    )
  )

  out <- flatten_epi_columns(nested)

  expect_named(
    out,
    c(
      "source_name",
      "outbreak.event.disease",
      "outbreak.counts.confirmed",
      "outbreak.counts.deaths"
    )
  )
  expect_equal(out$outbreak.event.disease[[1]], "cholera")
  expect_equal(out$outbreak.counts.confirmed[[1]], 14L)
  expect_equal(out$outbreak.counts.deaths[[1]], 2L)
})

test_that("flatten_epi_columns preserves vector leaves as list-columns", {
  nested <- tibble::tibble(
    outbreak = list(list(interventions = c("isolation", "vaccination")))
  )

  out <- flatten_epi_columns(nested)

  expect_true(is.list(out$outbreak.interventions))
  expect_equal(out$outbreak.interventions[[1]], c("isolation", "vaccination"))
})

test_that("flatten_epi_columns supports case-report style nested fields", {
  nested <- tibble::tibble(
    case = list(list(
      patient = list(full_name = "Priya Sharma"),
      patient_residence = NULL
    )),
    notification = list(list(
      disease = "Salmonella",
      test_result = "positive",
      laboratory = "Dorevitch Pathology"
    )),
    illness = list(list(
      onset_date = "2024-03-12",
      onset_time_text = "after dinner, maybe 8 or 9 pm",
      symptoms = c("diarrhoea", "abdominal cramping", "mild fever")
    )),
    exposures = list(tibble::tibble(
      exposure_type = c("takeaway", "barbecue"),
      venue_name = c("Cluck & Go", "Fawkner Park barbecue")
    ))
  )

  out <- flatten_epi_columns(nested)

  expect_equal(out$case.patient.full_name[[1]], "Priya Sharma")
  expect_equal(out$notification.disease[[1]], "Salmonella")
  expect_equal(out$notification.test_result[[1]], "positive")
  expect_equal(out$illness.onset_date[[1]], "2024-03-12")
  expect_equal(
    out$illness.symptoms[[1]],
    c("diarrhoea", "abdominal cramping", "mild fever")
  )
  expect_equal(out$exposures.exposure_type[[1]], c("takeaway", "barbecue"))
  expect_equal(out$exposures.venue_name[[1]], c("Cluck & Go", "Fawkner Park barbecue"))
})

test_that("epi_default_prompt emphasizes case investigation extraction", {
  prompt <- epi_default_prompt("Positive Salmonella case with uncertain exposure history.")

  expect_match(prompt, "case investigation record")
  expect_match(prompt, "symptom onset")
  expect_match(prompt, "travel")
  expect_match(prompt, "contacts")
})

test_that("build_prompts returns a list for function prompts", {
  prompts <- epistract:::build_prompts(
    c("case one", "case two"),
    epi_default_prompt
  )

  expect_true(is.list(prompts))
  expect_length(prompts, 2)
  expect_true(all(vapply(prompts, is.character, logical(1))))
  expect_match(prompts[[1]], "case one")
})

test_that("build_prompts returns a list for template prompts", {
  prompts <- epistract:::build_prompts(
    c("alpha", "beta"),
    "Extract this:\n%s"
  )

  expect_true(is.list(prompts))
  expect_identical(prompts[[1]], "Extract this:\nalpha")
  expect_identical(prompts[[2]], "Extract this:\nbeta")
})

test_that("flatten_epi_columns preserves empty repeated nested fields", {
  nested <- tibble::tibble(
    contacts = list(tibble::tibble(
      relationship = character(),
      symptoms_reported = character(),
      tested = character(),
      confirmed = character(),
      notes = character()
    ))
  )

  out <- flatten_epi_columns(nested)

  expect_true(is.list(out$contacts.relationship))
  expect_identical(out$contacts.relationship[[1]], character())
  expect_identical(out$contacts.symptoms_reported[[1]], character())
  expect_identical(out$contacts.tested[[1]], character())
  expect_identical(out$contacts.confirmed[[1]], character())
  expect_identical(out$contacts.notes[[1]], character())
})

test_that("prepare_epi_export converts list-columns to character columns", {
  nested <- tibble::tibble(
    case_id = 1L,
    illness.symptoms = list(c("diarrhoea", "fever")),
    contacts = list(tibble::tibble(
      relationship = c("partner", "housemate"),
      tested = c("no", "unknown")
    ))
  )

  out <- prepare_epi_export(nested)

  expect_true(is.character(out$illness.symptoms))
  expect_true(is.character(out$contacts))
  expect_identical(out$illness.symptoms[[1]], "diarrhoea; fever")
  expect_match(out$contacts[[1]], "relationship = partner")
  expect_match(out$contacts[[1]], "tested = no")
})

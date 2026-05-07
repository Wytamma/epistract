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

test_that("flatten_epi_columns keeps repeated df columns consistent across rows", {
  nested <- tibble::tibble(
    contacts = list(
      tibble::tibble(
        relationship = "partner",
        symptoms_reported = "yes",
        tested = "no",
        confirmed = "unknown",
        notes = "mild gastrointestinal symptoms"
      ),
      tibble::tibble(
        relationship = character(),
        symptoms_reported = character(),
        tested = character(),
        confirmed = character(),
        notes = character()
      )
    )
  )

  out <- flatten_epi_columns(nested)

  expect_true(is.list(out$contacts.relationship))
  expect_identical(out$contacts.relationship[[1]], "partner")
  expect_identical(out$contacts.relationship[[2]], character())
  expect_identical(out$contacts.tested[[1]], "no")
  expect_identical(out$contacts.tested[[2]], character())
})

test_that("repeated_prefixes_from_type identifies array-backed paths", {
  prefixes <- epistract:::repeated_prefixes_from_type(type_epi_case_report())

  expect_true("exposures" %in% prefixes)
  expect_true("contacts" %in% prefixes)
  expect_false("case" %in% prefixes)
  expect_false("notification" %in% prefixes)
})

test_that("simplify_scalar_list_columns simplifies scalar fields only", {
  nested <- tibble::tibble(
    case.patient.full_name = list("Priya Sharma", "Liam OConnor"),
    illness.onset_date = list("2024-03-12", character()),
    exposures.venue_name = list(c("Cluck & Go", "Fawkner Park"), "Rusty Bean"),
    contacts.relationship = list("partner", character())
  )

  out <- epistract:::simplify_scalar_list_columns(
    nested,
    repeated_prefixes = c("exposures", "contacts")
  )

  expect_true(is.character(out$case.patient.full_name))
  expect_identical(out$case.patient.full_name, c("Priya Sharma", "Liam OConnor"))
  expect_true(is.character(out$illness.onset_date))
  expect_identical(out$illness.onset_date, c("2024-03-12", NA_character_))
  expect_true(is.list(out$exposures.venue_name))
  expect_true(is.list(out$contacts.relationship))
})

test_that("default case-report schema uses partial-date fields", {
  schema <- type_epi_case_report()
  props <- attributes(schema)$properties

  expect_true(inherits(props$notification@properties$notification_date, "ellmer::TypeObject"))
  expect_true(inherits(props$interview@properties$interview_date, "ellmer::TypeObject"))
  expect_true(inherits(props$illness@properties$onset_date, "ellmer::TypeObject"))

  notification_parts <- names(attributes(props$notification@properties$notification_date)$properties)
  expect_true(all(c("year", "month", "day", "date_text") %in% notification_parts))
})

test_that("collect_type_postprocessors still finds explicit strict date processors", {
  schema <- ellmer::type_object(
    "schema",
    notification = ellmer::type_object(
      "notification",
      notification_date = type_epi_date(required = FALSE),
      .required = FALSE
    ),
    .required = TRUE
  )
  processors <- epistract:::collect_type_postprocessors(schema)

  expect_true(is.function(processors$notification.notification_date))
})

test_that("type_epi_partial_date defines year month and day fields", {
  x <- type_epi_partial_date()
  props <- attributes(x)$properties

  expect_true(inherits(x, "ellmer::TypeObject"))
  expect_true(all(c("year", "month", "day", "date_text") %in% names(props)))
})

test_that("rebuild_epi_partial_date uses placeholders for missing pieces", {
  out <- rebuild_epi_partial_date(
    year = c(NA, 2024),
    month = c(3, NA),
    day = c(16, 7)
  )

  expect_identical(out, c("XXXX-03-16", "2024-XX-07"))
})

test_that("rebuild_epi_partial_dates reconstructs flattened partial date columns", {
  nested <- tibble::tibble(
    notification.notification_date.year = c(NA_integer_, 2024L),
    notification.notification_date.month = c(3L, 3L),
    notification.notification_date.day = c(16L, 99L)
  )

  out <- rebuild_epi_partial_dates(nested)

  expect_identical(
    out$notification.notification_date,
    c("XXXX-03-16", "2024-03-99")
  )
  errs <- partial_date_errors(out)
  expect_equal(nrow(errs), 1)
  expect_identical(errs$row[[1]], 2L)
  expect_identical(errs$column[[1]], "notification.notification_date")
  expect_match(errs$error[[1]], "Day outside 1-31")
})

test_that("apply_postprocessors converts valid date values", {
  nested <- tibble::tibble(
    notification.notification_date = c("2024-03-14", NA_character_),
    case.patient.full_name = c("Priya Sharma", "Liam OConnor")
  )

  out <- epistract:::apply_postprocessors(
    nested,
    postprocess = list(
      notification.notification_date = epistract:::postprocess_date_strict
    )
  )

  expect_s3_class(out$data$notification.notification_date, "Date")
  expect_identical(as.character(out$data$notification.notification_date), c("2024-03-14", NA_character_))
  expect_equal(nrow(out$errors), 0)
})

test_that("apply_postprocessors keeps original values and records parse errors", {
  nested <- tibble::tibble(
    notification.notification_date = c("2024-03-14", "16 March 2024", NA_character_)
  )

  out <- epistract:::apply_postprocessors(
    nested,
    postprocess = list(
      notification.notification_date = epistract:::postprocess_date_strict
    )
  )

  expect_identical(
    out$data$notification.notification_date,
    c("2024-03-14", "16 March 2024", NA_character_)
  )
  expect_equal(nrow(out$errors), 1)
  expect_identical(out$errors$row[[1]], 2L)
  expect_identical(out$errors$column[[1]], "notification.notification_date")
  expect_match(out$errors$error[[1]], "Could not parse date value")
})

test_that("postprocess_errors returns attached extraction errors", {
  x <- tibble::tibble(a = 1)
  attr(x, "epistract_postprocess_errors") <- tibble::tibble(
    row = 1L,
    column = "notification.notification_date",
    value = "16 March 2024",
    error = "Could not parse"
  )

  out <- postprocess_errors(x)

  expect_identical(out$column[[1]], "notification.notification_date")
  expect_identical(out$value[[1]], "16 March 2024")
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

test_that("prepare_epi_export normalizes blank strings to missing values", {
  nested <- tibble::tibble(
    notification.laboratory = c("", "Dorevitch Pathology"),
    interview.interview_status = c("   ", "completed"),
    contacts.relationship = list("", "partner")
  )

  out <- prepare_epi_export(nested)

  expect_identical(out$notification.laboratory, c("", "Dorevitch Pathology"))
  expect_identical(out$interview.interview_status, c("", "completed"))
  expect_identical(out$contacts.relationship, c("", "partner"))
})

test_that("prepare_epi_export drops blank nested list values when collapsing", {
  nested <- tibble::tibble(
    contacts.notes = list(list("", "partner symptomatic"))
  )

  out <- prepare_epi_export(nested)

  expect_identical(out$contacts.notes, "partner symptomatic")
})

test_that("write_epi_delim writes missing values as blank cells by default", {
  nested <- tibble::tibble(
    case_id = c(1L, NA_integer_),
    notification.laboratory = c(NA_character_, "Dorevitch Pathology"),
    interview.interview_status = c("", "completed")
  )

  path <- tempfile(fileext = ".tsv")
  write_epi_delim(nested, path, sep = "\t")

  lines <- readLines(path, warn = FALSE)

  expect_false(any(grepl("\\bNA\\b", lines)))
})

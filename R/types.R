#' Epidemiology-specific tri-state field
#'
#' @param description Field description supplied to the model.
#' @param required Should the field be required?
#'
#' @return An `ellmer` enum type.
#' @export
type_epi_yes_no_unknown <- function(description = "One of yes, no, or unknown.",
                                    required = FALSE) {
  ellmer::type_enum(c("yes", "no", "unknown"), description = description, required = required)
}

#' Epidemiology-specific case status field
#'
#' @param description Field description supplied to the model.
#' @param required Should the field be required?
#'
#' @return An `ellmer` enum type.
#' @export
type_epi_case_status <- function(description = "Case status classification.",
                                 required = FALSE) {
  ellmer::type_enum(
    c("suspected", "probable", "confirmed", "discarded", "unknown"),
    description = description,
    required = required
  )
}

#' Epidemiology-specific outcome field
#'
#' @param description Field description supplied to the model.
#' @param required Should the field be required?
#'
#' @return An `ellmer` enum type.
#' @export
type_epi_outcome <- function(description = "Clinical outcome.",
                             required = FALSE) {
  ellmer::type_enum(
    c("recovered", "deceased", "ongoing", "unknown"),
    description = description,
    required = required
  )
}

#' Epidemiology-specific test result field
#'
#' @param description Field description supplied to the model.
#' @param required Should the field be required?
#'
#' @return An `ellmer` enum type.
#' @export
type_epi_test_result <- function(description = "Laboratory test result.",
                                 required = FALSE) {
  ellmer::type_enum(
    c("positive", "negative", "pending", "inconclusive", "unknown"),
    description = description,
    required = required
  )
}

#' Epidemiology-specific certainty field
#'
#' @param description Field description supplied to the model.
#' @param required Should the field be required?
#'
#' @return An `ellmer` enum type.
#' @export
type_epi_certainty <- function(description = "How certain the reported detail is.",
                               required = FALSE) {
  ellmer::type_enum(
    c("reported", "probable", "possible", "uncertain", "unknown"),
    description = description,
    required = required
  )
}

#' Epidemiology date field with strict post-processing
#'
#' Creates a string field for dates that also carries a strict post-processing
#' parser. The parser expects `YYYY-MM-DD` input, returns `Date` values when
#' parsing succeeds, and records an error during extraction when parsing fails.
#'
#' @param description Field description supplied to the model.
#' @param required Should the field be required?
#' @param format Date format expected during post-processing.
#'
#' @return An `ellmer` string type with attached post-processing metadata.
#' @export
#'
#' @examples
#' x <- type_epi_date("Notification date in YYYY-MM-DD format when possible.")
type_epi_date <- function(description = "Date in YYYY-MM-DD format.",
                          required = FALSE,
                          format = "%Y-%m-%d") {
  type <- ellmer::type_string(description, required = required)
  type_with_postprocess(
    type,
    function(value) postprocess_date_strict(value, format = format)
  )
}

#' Epidemiology partial date field
#'
#' Creates a date-like object split into `year`, `month`, and `day` components
#' so incomplete dates can be captured without forcing a full ISO date. This is
#' useful when reports mention only part of a date, such as a day and month but
#' no year.
#'
#' @param description Field description supplied to the model.
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
#'
#' @examples
#' x <- type_epi_partial_date("Notification date split into year, month, and day.")
type_epi_partial_date <- function(description = "Date split into year, month, and day components.",
                                  required = FALSE) {
  ellmer::type_object(
    description,
    year = ellmer::type_integer("Four-digit year if reported; leave missing if unknown.", required = FALSE),
    month = ellmer::type_integer("Month number from 1 to 12 if reported.", required = FALSE),
    day = ellmer::type_integer("Day of month from 1 to 31 if reported.", required = FALSE),
    date_text = ellmer::type_string(
      "Original date wording when the full date cannot be normalized confidently.",
      required = FALSE
    ),
    .required = required
  )
}

#' Geographic location type for epidemiological extraction
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_location <- function(required = FALSE) {
  ellmer::type_object(
    "Geographic information for the event or case.",
    location_name = ellmer::type_string("Named location exactly or nearly exactly as reported.", required = FALSE),
    admin1 = ellmer::type_string("First administrative division such as state or province.", required = FALSE),
    admin2 = ellmer::type_string("Second administrative division such as district or county.", required = FALSE),
    country = ellmer::type_string("Country name.", required = FALSE),
    latitude = ellmer::type_number("Latitude in decimal degrees if explicitly stated.", required = FALSE),
    longitude = ellmer::type_number("Longitude in decimal degrees if explicitly stated.", required = FALSE),
    .required = required
  )
}

#' Demographic attributes for an individual or group
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_demographics <- function(required = FALSE) {
  ellmer::type_object(
    "Demographic information relevant to epidemiology.",
    age_years = ellmer::type_number("Age in years when reported or directly inferable from the text.", required = FALSE),
    age_group = ellmer::type_string("Age group such as child, adult, or elderly.", required = FALSE),
    sex = ellmer::type_enum(c("female", "male", "intersex", "unknown"), "Reported sex.", required = FALSE),
    pregnancy_status = type_epi_yes_no_unknown("Pregnancy status when relevant.", required = FALSE),
    .required = required
  )
}

#' Person-level epidemiological attributes
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_person <- function(required = FALSE) {
  ellmer::type_object(
    "Information about an affected person or case.",
    full_name = ellmer::type_string("Case name exactly as reported.", required = FALSE),
    demographics = type_epi_demographics(required = FALSE),
    occupation = ellmer::type_string("Occupation or role such as healthcare worker.", required = FALSE),
    healthcare_worker = type_epi_yes_no_unknown("Whether the person is a healthcare worker.", required = FALSE),
    residence = type_epi_location(required = FALSE),
    .required = required
  )
}

#' Laboratory and notification details for a case
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_notification <- function(required = FALSE) {
  ellmer::type_object(
    "Laboratory result and notification details for the case.",
    disease = ellmer::type_string("Disease reported by the test result.", required = FALSE),
    pathogen = ellmer::type_string("Pathogen or agent named in the report.", required = FALSE),
    test_result = type_epi_test_result("Reported laboratory test result.", required = FALSE),
    specimen = ellmer::type_string("Specimen type such as stool, blood, or swab.", required = FALSE),
    notification_source = ellmer::type_string("How the case was notified, such as laboratory report.", required = FALSE),
    laboratory = ellmer::type_string("Laboratory or provider name.", required = FALSE),
    notification_date = type_epi_partial_date(
      "Notification date split into year, month, and day when possible.",
      required = FALSE
    ),
    confirmation_date = type_epi_partial_date(
      "Date the positive or confirming test was reported, split into year, month, and day when possible.",
      required = FALSE
    ),
    .required = required
  )
}

#' Case interview metadata
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_interview <- function(required = FALSE) {
  ellmer::type_object(
    "Case interview details.",
    interview_date = type_epi_partial_date("Interview date split into year, month, and day when possible.", required = FALSE),
    interview_status = ellmer::type_string("Interview completion status such as completed or delayed.", required = FALSE),
    interview_delay_reason = ellmer::type_string("Reason for delayed or incomplete interview.", required = FALSE),
    .required = required
  )
}

#' Symptom and illness details for an individual case
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_illness <- function(required = FALSE) {
  ellmer::type_object(
    "Symptom onset and illness details for an individual case.",
    onset_date = type_epi_partial_date("Symptom onset date split into year, month, and day.", required = FALSE),
    onset_time_text = ellmer::type_string("Free-text onset timing exactly or nearly exactly as reported.", required = FALSE),
    onset_precision = ellmer::type_enum(
      c("exact", "approximate", "unclear", "unknown"),
      "Precision of the symptom onset timing.",
      required = TRUE
    ),
    symptoms = ellmer::type_array(
      ellmer::type_string("One symptom or sign."),
      description = "Brief list of reported symptoms or signs.",
      required = TRUE
    ),
    fever_measured = type_epi_yes_no_unknown("Whether fever was measured with a recorded temperature.", required = FALSE),
    hospitalized = type_epi_yes_no_unknown("Whether the case was hospitalized.", required = TRUE),
    outcome = type_epi_outcome(required = TRUE),
    .required = required
  )
}

#' Exposure event relevant to a case interview
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_exposure <- function(required = FALSE) {
  ellmer::type_object(
    "A reported potential exposure for the case.",
    exposure_type = ellmer::type_enum(
      c("takeaway", "restaurant", "cafe", "barbecue", "market", "grocery", "household", "travel", "other"),
      "Type of exposure event.",
      required = FALSE
    ),
    exposure_date = type_epi_partial_date("Exposure date split into year, month, and day when possible.", required = FALSE),
    exposure_date_text = ellmer::type_string("Free-text exposure date or timing if exact date is unclear.", required = FALSE),
    venue_name = ellmer::type_string("Venue, store, stall, or event name.", required = FALSE),
    location = type_epi_location(required = FALSE),
    food_items = ellmer::type_array(
      ellmer::type_string("One food item or meal component."),
      description = "Reported food or drink items linked to the exposure.",
      required = FALSE
    ),
    certainty = type_epi_certainty("How confidently the exposure details were recalled or reported.", required = FALSE),
    notes = ellmer::type_string("Other relevant exposure details or uncertainty.", required = FALSE),
    .required = required
  )
}

#' Contact associated with a case
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_contact <- function(required = FALSE) {
  ellmer::type_object(
    "A household, workplace, or close contact mentioned in the case report.",
    relationship = ellmer::type_string("Relationship to the case such as partner, housemate, or coworker.", required = FALSE),
    symptoms_reported = type_epi_yes_no_unknown("Whether symptoms were reported for the contact.", required = FALSE),
    tested = type_epi_yes_no_unknown("Whether the contact was tested.", required = FALSE),
    confirmed = type_epi_yes_no_unknown("Whether infection was confirmed in the contact.", required = FALSE),
    notes = ellmer::type_string("Other contact details.", required = FALSE),
    .required = required
  )
}

#' Case-level epidemiological record
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_case <- function(required = FALSE) {
  ellmer::type_object(
    "Case-level epidemiological details.",
    case_status = type_epi_case_status(required = FALSE),
    patient = type_epi_person(required = FALSE),
    onset_date = type_epi_partial_date("Symptom onset date split into year, month, and day when possible.", required = FALSE),
    report_date = type_epi_partial_date("Date the case was reported, split into year, month, and day when possible.", required = FALSE),
    confirmation_date = type_epi_partial_date(
      "Laboratory confirmation date split into year, month, and day when possible.",
      required = FALSE
    ),
    outcome = type_epi_outcome(required = FALSE),
    hospitalized = type_epi_yes_no_unknown("Whether the case was hospitalized.", required = FALSE),
    symptoms = ellmer::type_array(
      ellmer::type_string("One symptom or sign."),
      description = "Brief list of symptoms or signs.",
      required = FALSE
    ),
    exposures = ellmer::type_array(
      ellmer::type_string("One reported exposure."),
      description = "Brief list of exposures or risk factors.",
      required = FALSE
    ),
    travel_history = type_epi_yes_no_unknown("Whether recent travel is reported.", required = FALSE),
    .required = required
  )
}

#' Event-level epidemiological attributes
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_event <- function(required = FALSE) {
  ellmer::type_object(
    "Event-level outbreak or incident attributes.",
    disease = ellmer::type_string("Disease or syndrome name.", required = FALSE),
    pathogen = ellmer::type_string("Pathogen or agent if reported.", required = FALSE),
    transmission_mode = ellmer::type_array(
      ellmer::type_string("One reported transmission mode."),
      description = "Transmission modes such as airborne, vector-borne, foodborne, or direct contact.",
      required = FALSE
    ),
    setting = ellmer::type_string("Setting such as hospital, school, prison, or community.", required = FALSE),
    .required = required
  )
}

#' Aggregate epidemiological counts
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_counts <- function(required = FALSE) {
  ellmer::type_object(
    "Aggregate counts and rates extracted from the report.",
    suspected = ellmer::type_integer("Number of suspected cases.", required = FALSE),
    probable = ellmer::type_integer("Number of probable cases.", required = FALSE),
    confirmed = ellmer::type_integer("Number of confirmed cases.", required = FALSE),
    total_cases = ellmer::type_integer("Total number of cases.", required = FALSE),
    deaths = ellmer::type_integer("Number of deaths.", required = FALSE),
    hospitalized = ellmer::type_integer("Number hospitalized.", required = FALSE),
    recovered = ellmer::type_integer("Number recovered.", required = FALSE),
    case_fatality_ratio = ellmer::type_number("Case fatality ratio as a proportion from 0 to 1 when available.", required = FALSE),
    attack_rate = ellmer::type_number("Attack rate as a proportion from 0 to 1 when available.", required = FALSE),
    .required = required
  )
}

#' Outbreak-level extraction schema
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_outbreak <- function(required = TRUE) {
  ellmer::type_object(
    "Structured outbreak summary extracted from a report.",
    event = type_epi_event(required = FALSE),
    location = type_epi_location(required = FALSE),
    counts = type_epi_counts(required = FALSE),
    first_case_date = type_epi_partial_date("Earliest case date split into year, month, and day when possible.", required = FALSE),
    latest_case_date = type_epi_partial_date("Latest case date split into year, month, and day when possible.", required = FALSE),
    interventions = ellmer::type_array(
      ellmer::type_string("One public health measure."),
      description = "Short list of interventions or control measures.",
      required = FALSE
    ),
    affected_population = ellmer::type_string("Description of the affected population.", required = FALSE),
    summary = ellmer::type_string("Short epidemiological summary.", required = FALSE),
    .required = required
  )
}

#' Report-level extraction schema
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_case_report <- function(required = TRUE) {
  ellmer::type_object(
    "Structured representation of a single case investigation report, especially enteric disease case interviews with lab results, symptoms, exposures, and contacts.",
    case = type_epi_case(required = FALSE),
    notification = type_epi_notification(required = FALSE),
    interview = type_epi_interview(required = FALSE),
    illness = type_epi_illness(required = FALSE),
    exposures = ellmer::type_array(
      type_epi_exposure(required = FALSE),
      description = "Potential exposure events or meals relevant to the case.",
      required = FALSE
    ),
    travel = ellmer::type_object(
      "Travel history relevant to the case.",
      travelled = type_epi_yes_no_unknown("Whether any travel was reported.", required = FALSE),
      travel_details = ellmer::type_string("Travel details if any were reported.", required = FALSE),
      .required = FALSE
    ),
    contacts = ellmer::type_array(
      type_epi_contact(required = FALSE),
      description = "Sick or relevant contacts mentioned in the case report.",
      required = FALSE
    ),
    investigation_notes = ellmer::type_string("Other salient details not captured elsewhere.", required = FALSE),
    .required = required
  )
}

#' Report-level extraction schema
#'
#' This is currently optimized for individual case investigation reports.
#'
#' @param required Should the object be required?
#'
#' @return An `ellmer` object type.
#' @export
type_epi_report <- function(required = TRUE) {
  type_epi_case_report(required = required)
}

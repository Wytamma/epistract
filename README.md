# epistract

`epistract` is an R package for extracting epidemiological data from unstructured text with local Ollama models.

It is now primarily tuned for single case investigation reports like foodborne or enteric disease notifications that include lab results, interview notes, symptom onset, uncertain exposure histories, travel, and household contacts.

## What it does

- uses `ellmer` structured outputs for schema-constrained extraction
- uses `ollamar` to check local Ollama connectivity and optionally pull models
- provides epidemiology-oriented type helpers such as `type_epi_case_report()`
- extracts from a data frame column and flattens nested results into dot-notated columns

## Install from GitHub

```r
install.packages("remotes")
remotes::install_github("wytamma/epistract", dependencies = TRUE)
```


## Example

```r
library(epistract)

reports <- tibble::tibble(
  case_id = 1:3,
  text = c(
    paste(
      "Case 1: Priya Sharma.",
      "Test Result: Positive for Salmonella.",
      "Case notified via laboratory report (Dorevitch Pathology, Heidelberg) on 14 March 2024.",
      "Initial interview conducted 15 March.",
      "The case reported onset of diarrhoea and abdominal cramping late evening of 12 March.",
      "Takeaway chicken wrap with mayonnaise from Cluck & Go on Swan Street, Richmond on 11 March.",
      "Also attended a family barbecue on 10 March at Fawkner Park, South Yarra.",
      "No overseas travel reported.",
      "Household contact (partner) reported mild gastrointestinal symptoms but was not tested."
    ),
    paste(
      "Case 2: Liam O'Connor.",
      "Test Result: Positive for Salmonella.",
      "Notified 16 March 2024 following positive stool culture.",
      "Interview completed same day.",
      "The case reported eating at The Rusty Bean on Sydney Road, Brunswick on 13 March.",
      "Also consumed pre-packaged spinach and feta salad from Woolworths Barkly Square earlier that week.",
      "No known sick contacts.",
      "Works in an office setting at TechHub CBD."
    ),
    paste(
      "Case 3: Mei Lin Chen.",
      "Test Result: Positive for Salmonella.",
      "Lab notification received 13 March.",
      "Case interview delayed until 17 March due to inability to contact.",
      "Symptoms included diarrhoea, fatigue, and abdominal discomfort.",
      "Exposure history included attendance at the Coburg Night Market on 9 March.",
      "One housemate reportedly unwell but not confirmed."
    )
  )
)

out <- extract_epi_data(
  reports,
  input_col = text,
  type = type_epi_case_report(),
  llm = llm(model = "gemma3:4b")
)

names(out)
# [1] "case_id" "text" "case.case_status" "case.patient.full_name"
# [5] "notification.disease" "notification.test_result"
# [7] "illness.onset_date" "illness.onset_time_text" ...

write_epi_delim(out, "epistract.tsv", sep = "\t")
```

## Intended workflow

Use `type_epi_case_report()` for case interview narratives and `extract_epi_data()` to turn each free-text record into analysis-ready columns. Nested objects flatten with dot notation, while repeated sections such as `exposures` and `contacts` remain list-columns.

For CSV or TSV export, first convert list-columns with `prepare_epi_export()` or use `write_epi_delim()`.

## Case-focused fields

The default schema is optimized for:

- `notification.*`: disease, pathogen, lab, specimen, result, and notification timing
- `interview.*`: interview date, completion status, and delay reason
- `illness.*`: symptom onset date, vague onset wording, symptoms, and outcome
- `exposures`: repeated meals, venues, markets, or other possible exposures
- `travel.*`: whether travel occurred and any travel details
- `contacts`: repeated household or close contacts with symptoms or testing status

## Pixi development environment

This repository can be developed inside a local Pixi environment.

```sh
pixi install
pixi run docs
pixi run dev-install
pixi run test
```

Notes:

- Pixi installs `R` and most package dependencies from `conda-forge`
- `ollamar` is installed into the Pixi-scoped `R_LIBS_USER` library by the `install-ollamar` task
- `pixi run docs` generates `.Rd` help pages from roxygen comments
- use `pixi run R` to start an `R` session inside the development environment


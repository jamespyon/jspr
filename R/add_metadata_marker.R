#' Insert Metadata Marker into README.md File
#'
#' @description Add metadata marker based on SHARE IT Act of 2024 to be read by OCIO AI scanner. To be used as is and not in a script (e.g. in the console of an RStudio workstation).
#'
#' @param contact_email Email for metadata or repository inquiries. CDC ID will also work.
#' @param org Name of CDC division, center, or office.
#' @param version Current release version of the software.
#' @param status Project development status (e.g., Maintained, Deprecated).
#' @param keywords Comma-separated list of technologies, domains, or topics.
#' @param labor_hours Estimated total person-hours for development.
#' @param contract Contract number associated with the project (if applicable).
#' @param exemption One of the five allowed values for restricted access.
#' @param exemption_justification Required explanation if an exemption is claimed. Use "ei" for EI group specific justification.
#'
#' @details
#'
#' | Exemption                | Description                                               |
#' |--------------------------|-----------------------------------------------------------|
#' | exemptByLaw              | Code is exempt from sharing due to legal restrictions     |
#' | exemptByNationalSecurity | Code is exempt due to national security concerns          |
#' | exemptByAgencySystem     | Code is exempt as it supports internal agency systems     |
#' | exemptByMissionSystem    | Code is exempt as it supports critical mission operations |
#' | exemptByCIO              | Code has a special exemption approved by the CDC CIO      |
#'
#' @returns An edit to the README.md file of the repository of the project you are in.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' add_metadata_marker("zzz0", exemption = "exemptByCIO", exemption_justification = "ei")
#' }

add_metadata_marker <- function(contact_email, org = "ATSDR", version = NA, status = c("Maintained", "Deprecated"), keywords = NA, labor_hours = NA, contract = NA, exemption = NA, exemption_justification = NA) {

  #pre-processing
  status <- match.arg(status)
  readme_path <- "README.md"
  keywords <- paste(keywords, collapse = ", ")

  if(grepl("^[A-z]{3}[0-9]$", contact_email)) {contact_email <- paste0(contact_email, "@cdc.gov")}
  if(!grepl("^[A-z]{3}[0-9]@cdc\\.gov", contact_email)) {stop("Contact Email is not valid.")}
  if(keywords == "NA") {keywords <- NA}
  if(!is.na(exemption)) {exemption <- match.arg(exemption, c("exemptByLaw", "exemptByNationalSecurity", "exemptByAgencySystem", "exemptByMissionSystem", "exemptByCIO", "exemptNonCode"))}
  if(grepl("[Ee][Ii]", exemption_justification)) {exemption_justification <- "This repository contains R scripts used solely to analyze and visualize site-specific environmental sampling data for a single ATSDR Public Health Assessment, Health Consultation, or Exposure Investigation. The code is not a reusable tool or generalizable module and is tightly coupled to a specific report deliverable."}

  #nullifying vector
  na_vx <- c(
    org,
    contact_email,
    version,
    status,
    keywords,
    labor_hours,
    contract,
    exemption,
    exemption_justification
  )


  #read README.md lines
  if (file.exists(readme_path)) {
    readme_content <- readLines(readme_path)
  } else {
    stop("README.md not found.")
  }

  #create section
  new_section <- c(paste("Org:", org),
                   paste("Contact Email:", contact_email),
                   paste("Version:", version),
                   paste("Status:", status),
                   paste("Keywords:", keywords),
                   paste("Labor Hours:", labor_hours),
                   paste("Contract#:", contract),
                   paste("Exemption:", exemption),
                   paste("Exemption Justification:", exemption_justification))[!is.na(na_vx)]

  #create code block
  new_block <- c("```", new_section, "```")


  if(grepl("# ", readme_content[1])) {
    new_block <- c(readme_content[1], "", new_block)
    readme_content <- readme_content[-1]
  }

  updated_content <- c(new_block, readme_content)

  writeLines(updated_content, readme_path)

}

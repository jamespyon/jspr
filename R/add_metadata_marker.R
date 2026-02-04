#' Insert Metadata Marker into README.md File
#'
#' @description Add metadata marker based on SHARE IT Act of 2024 to be read by OCIO AI scanner. To be used as is and not in a script (e.g. in the console of an RStudio workstation).
#'
#' @param contact_email Email for metadata or repository inquiries (REQUIRED). CDC ID will also work. If NULL, it will coerce one from your system.
#' @param org Name of CDC division, center, or office (REQUIRED). Default is "ATSDR".
#' @param keywords Comma-separated list of technologies, domains, or topics (REQUIRED). Must match with valid Mission Area keywords.
#' @param status Project development status (e.g., Maintained, Deprecated) (if applicable).
#' @param version Current release version of the software(if applicable).
#' @param labor_hours Estimated total person-hours for development(if applicable).
#' @param contract Contract number associated with the project (if applicable).
#' @param exemption One of the five allowed values for restricted access (if applicable).
#' @param exemption_justification Required explanation if an exemption is claimed (use when exemption is used). Use "ei" for Exposure Investigation group specific justification.
#' @param file The filepath for the README.md file, if your README file is not in the same directory as your working directory.
#' @param overwrite Do you want to overwrite the metadata marker?
#'
#' @details
#' The minimum necessary metadata marker includes `contact_email`, `org`, and `keywords`. You must include these arguments at the minimum.
#'
#' @details
#' Here is a list of all Exemptions and their descriptions.
#'
#' | Exemption                  | Description                                               |
#' |----------------------------|-----------------------------------------------------------|
#' | `exemptByLaw`              | Code is exempt from sharing due to legal restrictions     |
#' | `exemptByNationalSecurity` | Code is exempt due to national security concerns          |
#' | `exemptByAgencySystem`     | Code is exempt as it supports internal agency systems     |
#' | `exemptByMissionSystem`    | Code is exempt as it supports critical mission operations |
#' | `exemptByCIO`              | Code has a special exemption approved by the CDC CIO      |
#'
#' @returns An edit to the README.md file of the repository of the project you are in.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' add_metadata_marker("your@email.here", org = "YourOrg", keywords = c("your", "keywords", "here"))
#' }
#'
add_metadata_marker <- function(contact_email = NULL, org = "ATSDR", keywords, status = c("Maintained", "Deprecated"), version = NULL, labor_hours = NULL, contract = NULL, exemption = c("exemptByLaw", "exemptByNationalSecurity", "exemptByAgencySystem", "exemptByMissionSystem", "exemptByCIO", "exemptNonCode"), exemption_justification = NULL, file = NULL, overwrite = FALSE) {

  # check keywords
  if(!is.null(keywords)) {keywords <- paste(keywords, collapse = ", ")}

  # check email
  if(is.null(contact_email)) {contact_email = user_id()}
  if(grepl("^[A-z]{3}[0-9]$", contact_email)) {contact_email <- paste0(contact_email, "@cdc.gov")}
  if(!grepl("^[A-z]{3}[0-9]@cdc\\.gov", contact_email)) {stop("Contact Email is not valid.")}

  # possible for no status
  if(identical(status, c("Maintained", "Deprecated"))) {
    status <- NULL
  } else {
    status <- match.arg(status)
  }

  # possible for no exemption
  if(identical(exemption, c("exemptByLaw", "exemptByNationalSecurity", "exemptByAgencySystem", "exemptByMissionSystem", "exemptByCIO", "exemptNonCode"))) {
    exemption <- NULL
  } else {
    exemption <- match.arg(exemption)
  }

  # check exemption justification
  if(!is.null(exemption) & is.null(exemption_justification)) {
    stop("Exemption Justification is needed when Exemption is selected.")
  } else

  if(!is.null(exemption)) {
    if(grepl("[Ee][Ii]", exemption_justification)) {exemption_justification <- "This repository contains R scripts used solely to analyze and visualize site-specific environmental sampling data for a single ATSDR Public Health Assessment, Health Consultation, or Exposure Investigation. The code is not a reusable tool or generalizable module and is tightly coupled to a specific report deliverable."}
  }

  # check file path
  if(!is.null(file)) {

    if(file.exists(file)) {
      readme_path <- file
    } else {stop("README.md not found.")}

  } else if(file.exists("README.md")) {

    readme_path <- "README.md"

  } else {stop("README.md not found.")}

  # check keywords
  if(any(is.na(match(keywords, jspr::mission_area$keyword)))) {
    stop("One or more keywords are not valid. Please check mission_area dataset for a list of valid Mission Area keywords.")
  }

  # check if required exists
  if(is.null(org) | is.null(contact_email) | is.null(keywords)) {stop("Arguments org, contact_email, and keywords are required.")}

  #nullifying vector and create block
  marker_list <- list("Organization" = org,
                      "Contact email" = contact_email,
                      "Keywords" = keywords,
                      "Status" = status,
                      "Version" = version,
                      "Labor Hours" = labor_hours,
                      "Contract#" = contract,
                      "Exemption" = exemption,
                      "Exemption Justification" = exemption_justification)

  select_marker <- as.list(unlist(marker_list))
  new_section <- c(paste(names(select_marker), select_marker, sep = ": "))
  section_length <- length(new_section)

  section_index <- 1
  final_section <- c()
  for(i in 1:(2*section_length-1)) {

    if(i %% 2 == 1) {

      final_section <- c(final_section, new_section[section_index])
      section_index <- section_index + 1

    } else {

      final_section <- c(final_section, "")

    }

  }

  new_block <- c("```", final_section, "```")

  #read README.md lines
  readme_content <- readLines(readme_path)
  marker_exists <- any(grepl("^[Oo][Rr][Gg](anization)?: ", readme_content)) | any(grepl("[Cc]ontact [Ee]mail: ", readme_content))

  if(marker_exists) {

    if(overwrite) {

      # find word to center scraping
      word_position <- grep("^[Oo][Rr][Gg](anization)?: ", readme_content)

      # edit top chunk
      top_chunk <- readme_content[1:word_position]
      top_index <- max(grep("```", top_chunk))
      top_chunk <- top_chunk[1:(top_index - 1)]

      # edit bottom chunk
      bottom_chunk <- readme_content[word_position:length(readme_content)]
      bottom_index <- min(grep("```", bottom_chunk))
      bottom_chunk <- bottom_chunk[(bottom_index+1):length(bottom_chunk)]

      # stitch up blocks and write
      updated_content <- c(top_chunk, new_block, bottom_chunk)
      writeLines(updated_content, readme_path)

      message("Metadata marker found. Overwrite has occurred.")

    } else {

      invisible("Metadata marker found, but not replaced. Use `overwrite. = TRUE` to overwrite.")
      message("Metadata marker found, but not replaced. Use `overwrite. = TRUE` to overwrite.")

    }

  } else {

    # edit new block if there's a title
    if(grepl("# ", readme_content[1])) {
      new_block <- c(readme_content[1], "", new_block)
      readme_content <- readme_content[-1]
    }

    # stitch up blocks and write
    updated_content <- c(new_block, readme_content)
    writeLines(updated_content, readme_path)

  }

}

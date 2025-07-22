# ------------------------
# Libraries
# ------------------------
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)

# ------------------------
# 1. Settings
# ------------------------
file_path <- "data/Elicit - screen-results-review-c3938d82-d58d-4371-9a8c-7616936df36c.csv"

# Contact email (required by Unpaywall API)
contact_email <- "davidjpmoore@arizona.edu"

# Your university proxy base URL
# Replace 'libproxy.arizona.edu' with your institution's proxy if different
proxy_base <- "https://libproxy.arizona.edu/login?url=https://doi.org/"

# Create a folder for downloads
if (!dir.exists("downloads")) dir.create("downloads")

# ------------------------
# 2. Load DOIs
# ------------------------
df <- read_csv(file_path)
dois <- df$DOI

# ------------------------
# 3. Function: Query Unpaywall and Download OA PDFs
# ------------------------
check_and_download_pdf <- function(doi, title) {
  if (is.na(doi) || doi == "") return(list(OA = NA, Downloaded = FALSE, Source = NA))
  
  url <- paste0("https://api.unpaywall.org/v2/", doi, "?email=", contact_email)
  response <- try(GET(url), silent = TRUE)
  
  if (inherits(response, "try-error") || status_code(response) != 200) {
    return(list(OA = NA, Downloaded = FALSE, Source = NA))
  }
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  # If Open Access and PDF link available, download
  if (isTRUE(data$is_oa) && !is.null(data$best_oa_location$url_for_pdf)) {
    pdf_url <- data$best_oa_location$url_for_pdf
    safe_title <- gsub("[^A-Za-z0-9_ -]", "_", substr(title, 1, 100))
    file_name <- paste0("downloads/", safe_title, ".pdf")
    
    try(download.file(pdf_url, file_name, mode = "wb"))
    return(list(OA = TRUE, Downloaded = TRUE, Source = "Open Access"))
  }
  
  return(list(OA = FALSE, Downloaded = FALSE, Source = NA))
}

# ------------------------
# 4. Function: Try Library Proxy Download
# ------------------------
download_via_proxy <- function(doi, title) {
  safe_title <- gsub("[^A-Za-z0-9_ -]", "_", substr(title, 1, 100))
  file_name <- paste0("downloads/", safe_title, ".pdf")
  
  doi_url <- paste0(proxy_base, doi)
  
  # Attempt direct download (may not work if authentication is needed)
  result <- try(download.file(doi_url, file_name, mode = "wb"), silent = TRUE)
  
  if (!inherits(result, "try-error")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# ------------------------
# 5. Apply to All Papers
# ------------------------
df$FullTextAvailable <- NA
df$PDF_Downloaded <- FALSE
df$Download_Source <- NA

for (i in seq_along(dois)) {
  doi <- dois[i]
  title <- df$Title[i]
  
  cat("Processing:", title, "\n")
  
  res <- check_and_download_pdf(doi, title)
  df$FullTextAvailable[i] <- res$OA
  df$PDF_Downloaded[i] <- res$Downloaded
  df$Download_Source[i] <- res$Source
  
  # If not OA, try proxy
  if (!isTRUE(res$Downloaded) && !is.na(doi)) {
    proxy_success <- download_via_proxy(doi, title)
    if (proxy_success) {
      df$PDF_Downloaded[i] <- TRUE
      df$Download_Source[i] <- "Proxy"
    }
  }
}

# ------------------------
# 6. Save Results
# ------------------------
write_csv(df, "output/papers_with_download_status.csv")

cat("Process completed! Check 'downloads' folder and 'papers_with_download_status.csv'.\n")

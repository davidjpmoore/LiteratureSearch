library(readr)
library(dplyr)
library(officer)  # for Word output
library(stringr)

# Load your file
df <- read_csv("data/Elicit - screen-results-review-c3938d82-d58d-4371-9a8c-7616936df36c.csv")

# Replace missing data
df <- df %>%
  mutate(
    Authors = ifelse(is.na(Authors), "", Authors),
    Year = ifelse(is.na(Year), "n.d.", as.character(Year)),
    Title = ifelse(is.na(Title), "", str_trim(Title)),
    Venue = ifelse(Venue == "-" | is.na(Venue), "n.p.", Venue),
    DOI_link = ifelse(is.na(`DOI link`), "", `DOI link`)
  )

# Format APA citations
df <- df %>%
  mutate(APA = paste0(
    Authors, " (", Year, "). ",
    "*", Title, ".* ",
    Venue, ". ",
    DOI_link
  ))

# Option A: Save as text
write_lines(df$APA, "APA_Citations.txt")

# Option B: Save as Word document
doc <- read_docx()
for (citation in df$APA) {
  doc <- body_add_par(doc, citation, style = "Normal")
}
print(doc, target = "APA_Citations.docx")

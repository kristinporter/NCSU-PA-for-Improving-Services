
# function to extract code chunks from notebooks
extract_chunk <- function(file, label) {
  lines <- readLines(file)
  inside_chunk <- FALSE
  chunk_lines <- c()

  for (line in lines) {
    if (grepl(sprintf("```\\{r %s", label), line)) {
      inside_chunk <- TRUE
      next
    }

    if (inside_chunk && grepl("```", line)) {
      break
    }

    if (inside_chunk) {
      chunk_lines <- c(chunk_lines, line)
    }
  }

  # You can also print the lines to the console instead of writing to a file
  writeLines(chunk_lines, sprintf("%s.R", label))
}

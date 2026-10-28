# Runs every chapter in order. Usage (from the repo root):
#   Rscript run_all.R            # all chapters
#   Rscript run_all.R 01 04      # only chapters whose file name starts with 01 or 04
#
# Output: HTML if pandoc is available, otherwise Markdown, in results/reports/.
# Tables go to results/*.csv.

args <- commandArgs(trailingOnly = TRUE)
chapters <- sort(list.files(pattern = "^[0-9]{2}_.*\\.Rmd$"))
if (length(args)) chapters <- chapters[substr(chapters, 1, 2) %in% args]

dir.create("results/reports", recursive = TRUE, showWarnings = FALSE)
have_pandoc <- rmarkdown::pandoc_available()

for (f in chapters) {
  t0 <- Sys.time()
  message("== ", f)
  if (have_pandoc) {
    rmarkdown::render(f, output_dir = "results/reports", quiet = TRUE, envir = new.env())
  } else {
    knitr::opts_knit$set(base.dir = file.path(getwd(), "results/reports"))
    knitr::opts_chunk$set(fig.path = paste0(tools::file_path_sans_ext(f), "_files/"))
    knitr::knit(f, output = file.path("results/reports", sub("\\.Rmd$", ".md", f)),
                quiet = TRUE, envir = new.env())
  }
  message(sprintf("   done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

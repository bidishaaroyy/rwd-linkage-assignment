# run_all.R
set.seed(1)                 # no random steps are used; seed recorded for completeness
source("tests.R")           # automated checks first; stops if any fail
source("analysis.R")        # all tables, plots and the issue log
rmarkdown::render("report.Rmd", output_file = "report.html", output_dir = "output", quiet = TRUE)
writeLines(capture.output(sessionInfo()), "output/session_info.txt")
message("Done. Open output/report.html")

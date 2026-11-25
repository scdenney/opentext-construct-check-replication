# session_info.R -- record the software the reporting stage ran with (analysis/output/revision/session_info.txt).
source("analysis/revision/rv_config.R")
suppressMessages({ library(stm); library(quanteda); library(dplyr); library(ggplot2); library(patchwork) })
oc <- "opencheck"  # the copy bundled with this package
git <- function(...) suppressWarnings(tryCatch(system2("git", c("-C", oc, ...), stdout = TRUE, stderr = FALSE), error = function(e) character()))
commit <- git("rev-parse", "HEAD"); commit <- if (length(commit) && is.null(attr(commit, "status"))) commit else "not under version control"
src <- sort(list.files(oc, pattern = "\\.(R|py|md|yaml|sh)$|^DESCRIPTION$", recursive = TRUE, full.names = TRUE))
digest <- unname(tools::md5sum({ f <- tempfile(); writeLines(paste(basename(src), tools::md5sum(src)), f); f }))
out <- c(format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "",
         paste("opencheck source:", oc), paste("opencheck version:", as.character(utils::packageVersion("opencheck"))),
         paste("opencheck commit:", commit), paste0("opencheck source digest (md5 over ", length(src), " file hashes): ", digest), "",
         capture.output(print(sessionInfo())))
writeLines(out, file.path(OUT, "session_info.txt")); cat(out[1:6], sep = "\n")

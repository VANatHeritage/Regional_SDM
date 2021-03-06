if (!"git2r" %in% names(installed.packages()[,1])) stop ("Need to install 'git2r' package first.")
library(git2r)

ld <- list.dirs(loc_scripts, recursive = FALSE, full.names = FALSE)
suppressWarnings(ld <- max(ld[grepl("Regional_SDM_[0-9]{4}-[0-9]{2}-[0-9]{2}$", ld)]))
if (is.na(ld)) script_store <- paste0(loc_scripts, "/Regional_SDM_", Sys.Date()) else script_store <- paste0(loc_scripts, "/", ld)
if (!dir.exists(script_store)) {
  try(suppressMessages(git_repo <- git2r::clone("https://github.com/VANatHeritage/Regional_SDM.git",
                                                branch = branch, local_path = script_store)), silent = TRUE)
  if (exists("git_repo")) {
    message("Scripts downloaded. Ready to run.")
  } else {
    dir.create(script_store)
    message(paste0("Couldn't download latest scripts. \nNew folder '",
                   script_store, "' created. \nManually place latest scripts in there."))
  }
} else {
  try({
    git_repo <- git2r::repository(script_store)
    git_chk <- git2r::checkout(git_repo, branch = branch, force = FALSE)
    git_pull <- git2r::pull(git_repo)
  })
  if (exists("git_pull") && any(git_pull$up_to_date, git_pull$fast_forward)) {
    message("Scripts up-to-date. Ready to run.")
    message(paste0("Set 'loc_scripts' to '", script_store ,"'."))
  } else {
    message(paste0("Couldn't download latest scripts. \nYou can manually place latest scripts in folder '",
                   script_store, "' and set 'loc_scripts' to that path."))
    if (exists("git_repo") & exists("git_chk")) {
      message("Reason for error (git status) is: ") 
      print(status(git_repo))
    }
  }
}
suppressWarnings(rm(git_repo, git_pull, git_chk))
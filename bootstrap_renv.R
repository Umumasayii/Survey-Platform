# Generate a project-local renv.lock from an actual R runtime.
# This file is intentionally executable rather than shipping a fabricated lock.
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::init(bare = TRUE, restart = FALSE)
source("INSTALL_DEPENDENCIES.R", local = new.env(parent = globalenv()))
renv::snapshot(prompt = FALSE)
cat("Created renv.lock from the installed runtime. Commit the lock after tests pass.\n")

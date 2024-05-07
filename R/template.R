#' Project Template function
#'
#'
#' @export
mmp_project_template <- function(name, folder = getwd(), readme=TRUE, git=TRUE, packrat=TRUE) {

  if (missing(name)) stop("name is required")
  if (!is.character(name)) stop("name has to be a character")
  if (nchar(name) < 2) stop("name needs to have at least two characters")

  root.dir <- file.path(normalizePath(folder), name)
  # current_project <- usethis::proj_get()

  tryCatch({
    # ensure path exists
    dir.create(root.dir, recursive = TRUE, showWarnings = FALSE)

    # create data dir
    data.dir <- file.path(root.dir, "data")

    dir.create(data.dir)
    dir.create(file.path(data.dir, "raw"))
    dir.create(file.path(data.dir, "processed"))

    # create figures dir
    dir.create(file.path(root.dir, "figures"))

    # create reports dir
    dir.create(file.path(root.dir, "reports"))

    # create scripts
    dir.create(file.path(root.dir, "scripts"))

    # use readme
    if (readme) {
      usethis::ui_done("Generate README File")

      contents <- paste(
        "# Title of this project",
        "",
        "Some information about this project",
        sep="\n"
      )

      writeLines(contents, con = file.path(root.dir, "README.md"))
    }

    # use git
    if (git) {
      usethis::ui_done("Create GIT Repository")

      git2r::init(root.dir)
    }

    # use packrate
    if (packrat) {
      usethis::ui_done("Initialize Packrat Repository")

      packrat::init(root.dir, enter=FALSE, restart=FALSE)
    }

    # settings.R
    contents <- paste(
      "# General settings",
      "",
      "## General directories",
      "root.dir <- here::here()",
      "data.dir <- file.path(root.dir, \"data\")",
      "data.raw.dir  <- file.path(data.dir, \"raw\")",
      "data.processed.dir  <- file.path(data.dir, \"processed\")",
      "",
      "functions.dir <- file.path(root.dir, \"functions\")",

      sep="\n"
    )

    writeLines(contents, con = file.path(root.dir, "settings.R"))

  },
  error = function(e) {
    message(paste("Error:", e$message))
    e
    # delete folder created earlier
    unlink(root.dir, recursive = TRUE)
    message(sprintf("Oops! An error was found and the `%s` directory was deleted", name)) # nolint
  })

  # usethis::proj_set(current_project, force = TRUE)

  invisible(TRUE)
}

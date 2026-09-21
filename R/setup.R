#' A MATLAB session: an open R.matlab client plus a running step log
#'
#' Every `matlab_*()` step function below takes a `matlab_session` as its
#' first argument and returns one (invisibly), so they can be chained with
#' the native pipe:
#'
#' \preformatted{
#' session <- matlab_start_server() |>
#'   matlab_setup_spm(spm_path, script_path) |>
#'   matlab_coregister(ref_image, source_image) |>
#'   matlab_close_server()
#'
#' matlab_session_log(session)
#' }
new_matlab_session <- function(client) {
  structure(list(client = client, log = list()), class = "matlab_session")
}

#' Start a MATLAB server and return a connected client
#'
#' @param matlab_exe Path to the MATLAB executable.
#' @param port TCP port for the R <-> MATLAB bridge.
#' @return An open `R.matlab::Matlab` client.

matlab_start_server <- function(
  matlab_exe = "/Applications/MATLAB_R2026a.app/bin/matlab",
  port = 9999
) {
  MatlabClass <- get("Matlab", envir = asNamespace("R.matlab"))
  MatlabClass$startServer(matlab = matlab_exe, port = port)

  client <- R.matlab::Matlab(host = "localhost", port = port)

  if (!R.matlab:::open.Matlab(client)) {
    cli::cli_abort("Matlab server could not be opened with {.pkg R.matlab}")
  }

  new_matlab_session(client)
}

#' Add SPM/script paths and initialize SPM on an already-open MATLAB session
#'
#' Call this once per session, right after `matlab_start_server()`
#'
#' @param session An open `R.matlab::Matlab` client.
#' @param spm_path Path to the SPM toolbox directory.
#' @param script_path Path to `coregister.m` (its directory is added to the
#'   MATLAB path).
matlab_setup_spm <- function(session, spm_path) {
  scripts_path <- system.file("matlab", package = "kuadrc.pet")

  matlab_step(
    session,
    "setup_spm",
    sprintf(
      "  addpath('%s');\n  addpath('%s');\n  spm('defaults', 'pet');\n  spm_jobman('initcfg');",
      scripts_path,
      spm_path
    )
  )
}

#' Close the MATLAB server, ending the pipe
#'
#' @param session A `matlab_session`.
#' @return The `session`, invisibly — the log survives after closing, so
#'   you can still inspect it: `session <- ... |> matlab_close_server()`.
#' @export
matlab_close_server <- function(session) {
  stopifnot(inherits(session, "matlab_session"))
  R.matlab:::close.Matlab(session$client)
  invisible(session)
}

#' Pull a session's step log as a data frame
#'
#' @param session A `matlab_session`.
#' @return A data frame with one row per step: `step`, `status`, `message`.
#' @export
matlab_session_log <- function(session) {
  stopifnot(inherits(session, "matlab_session"))
  do.call(rbind, lapply(session$log, as.data.frame, stringsAsFactors = FALSE))
}

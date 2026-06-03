#' Resolve Parallel Worker Count
#'
#' Determines how many workers should be used for an optional parallel task.
#'
#' @author Vivek Gajadhar
#' @param parallel Logical; whether parallel execution is requested.
#' @param n_tasks Number of independent tasks.
#' @param n_cores Optional internal worker count override.
#' @return Integer worker count.
#' @keywords internal
#' @noRd
resolve_parallel_cores <- function(parallel, n_tasks, n_cores = NULL) {
  if (!isTRUE(parallel) || n_tasks <= 1) {
    return(1L)
  }

  if (!is.null(n_cores)) {
    if (!is.numeric(n_cores) || length(n_cores) != 1 || is.na(n_cores) || n_cores < 1) {
      stop("'n_cores' must be a positive number.")
    }

    return(max(1L, min(as.integer(n_cores), n_tasks)))
  }

  detected_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(detected_cores) || detected_cores < 2) {
    detected_cores <- parallel::detectCores(logical = TRUE)
  }
  if (is.na(detected_cores) || detected_cores < 2) {
    return(1L)
  }

  max(1L, min(4L, as.integer(detected_cores - 1L), n_tasks))
}

#' Run Optional Parallel Tasks
#'
#' Runs independent tasks sequentially by default, or in parallel when requested.
#' If parallel execution fails, the tasks are rerun sequentially.
#'
#' @author Vivek Gajadhar
#' @param tasks List or vector of independent tasks.
#' @param task_function Function called once per task.
#' @param parallel Logical; whether parallel execution is requested.
#' @param n_cores Optional internal worker count override.
#' @param fallback_message Warning prefix used when falling back to sequential execution.
#' @return List of task results.
#' @keywords internal
#' @noRd
run_parallel_tasks <- function(tasks,
                               task_function,
                               parallel = FALSE,
                               n_cores = NULL,
                               fallback_message = "Parallel calculation failed; falling back to sequential calculation.") {
  n_cores <- resolve_parallel_cores(
    parallel = parallel,
    n_tasks = length(tasks),
    n_cores = n_cores
  )

  if (n_cores == 1L) {
    return(lapply(tasks, task_function))
  }

  results <- tryCatch(
    run_parallel_tasks_unchecked(
      tasks = tasks,
      task_function = task_function,
      n_cores = n_cores
    ),
    error = function(error) {
      warning(
        fallback_message,
        " Error: ",
        conditionMessage(error),
        call. = FALSE
      )
      NULL
    }
  )

  if (is.null(results)) {
    return(lapply(tasks, task_function))
  }

  results
}

#' Run Parallel Tasks Without Fallback
#'
#' Platform-specific implementation for optional parallel tasks.
#'
#' @author Vivek Gajadhar
#' @inheritParams run_parallel_tasks
#' @return List of task results.
#' @keywords internal
#' @noRd
run_parallel_tasks_unchecked <- function(tasks, task_function, n_cores) {
  if (.Platform$OS.type != "windows") {
    return(parallel::mclapply(tasks, task_function, mc.cores = n_cores))
  }

  cluster <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cluster), add = TRUE)

  parallel::clusterEvalQ(cluster, {
    if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
      pkgload::load_all(getwd(), export_all = TRUE, quiet = TRUE)
    } else if (requireNamespace("REPS", quietly = TRUE)) {
      library(REPS)
    }
    NULL
  })

  package_env <- parent.env(environment())
  package_objects <- mget(ls(package_env, all.names = TRUE), envir = package_env, inherits = FALSE)
  package_functions <- names(package_objects)[vapply(package_objects, is.function, logical(1))]
  parallel::clusterExport(cluster, varlist = package_functions, envir = package_env)

  task_env <- list2env(list(task_function = task_function), parent = .GlobalEnv)
  parallel::clusterExport(cluster, varlist = "task_function", envir = task_env)

  worker_function <- function(task) {
    task_function(task)
  }
  environment(worker_function) <- .GlobalEnv

  parallel::parLapply(cluster, tasks, worker_function)
}

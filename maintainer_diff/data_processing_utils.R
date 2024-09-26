box::use(
  lubridate[interval, month],
  dplyr[filter, pull, select],
  purrr[map],
  stringi[stri_trans_general],
  utils[head],
)
box::use(maintainer_diff/repo_data_utils[...])

#' get_contributors_details
#'
#' Retrieves details of contributors for a list of packages.
#'
#' @param package_list A list of package details including repository paths.
#' @param package_names A character vector of package names.
#' @param api_key An optional API key for authentication.
#'
#' @return A list of contributor details including contributor IDs and names.
#'
#' @examples
#' get_contributors_details(package_list, package_names, "API_KEY")
#' @export
get_contributors_details <- function(
    package_list,
    package_names,
    api_key = NULL) {
  # get contributors' id list
  all_repo_contributors_id <- map(package_list, .f = function(package) {
    get_repo_contributors(
      repo_path = package[["repo"]],
      api_key
    )
  }) |>
    map(.f = function(repo) {
      map(repo, .f = function(contributor) {
        contributor[["login"]]
      }) |>
        unlist()
    }) |>
    set_repo_name(repo_names = package_names)
  # get contributors' name list
  all_repo_contributors_names <- all_repo_contributors_id |>
    map(function(repo) {
      map(repo, function(id) {
        stri_trans_general(get_name_from_id(id, api_key), "Latin-ASCII")
      })
    }) |>
    map(function(repo) unlist(repo, use.names = FALSE))
  # combine contributors' name and id
  combine_id_name <- function(id, name) list(id = id, name = name)
  Map(
    combine_id_name,
    all_repo_contributors_id,
    all_repo_contributors_names
  )
}

#' get_maintainers_details
#'
#' Retrieves details of maintainers for a list of packages.
#'
#' @param package_list A list of package details including repository paths.
#' @param package_names A character vector of package names.
#' @param contributors_details A list of contributor details including
#' contributor IDs and names.
#' @param api_key An optional API key for authentication.
#'
#' @return A list of maintainer details including maintainer IDs and names.
#'
#' @examples
#' get_maintainers_details(package_list, package_names, contributors_details)
#' @export
get_maintainers_details <- function(
    package_list,
    package_names,
    contributors_details,
    api_key = NULL) {
  # get all repo maintainers
  all_repo_maintainer_names <- map(package_list, .f = function(package) {
    tryCatch({
      get_package_maintainers(
        repo_path = package[["repo"]],
        api_key
      )
    }, error = function(e) {
      "DESCRIPTION file not found"
    })
  }) |>
    set_repo_name(repo_names = package_names) |>
    map(.f = function(name) {
      strsplit(name, split = " <")[[1]][1] |>
        stri_trans_general("Latin-ASCII")
    })
  # get repo maintainer id
  all_repo_maintainers_id <- Map(function(contrib_detail, maintainer_name) {
    if (maintainer_name %in% contrib_detail[["name"]])
      contrib_detail[["id"]][which(contrib_detail[["name"]] == maintainer_name)]
  }, contributors_details, all_repo_maintainer_names)
  all_repo_maintainers_id <- Map(
    function(maintainer_id, maintainer_name, contributor) {
      if (is.null(maintainer_id)) {
        name_split <- strsplit(maintainer_name, split = " ")
        first_name <- name_split[[1]][1]
        grep(first_name, contributor[["id"]], value = TRUE, ignore.case = TRUE)
      } else {
        maintainer_id
      }
    },
    all_repo_maintainers_id,
    all_repo_maintainer_names,
    contributors_details
  )
  all_repo_maintainers_id <- Map(
    function(maintainer_id, maintainer_name, contributor) {
      if (length(maintainer_id) == 0) {
        name_split <- strsplit(maintainer_name, split = " ")
        last_name <- name_split[[1]][length(name_split[[1]])]
        grep(last_name, contributor[["id"]], value = TRUE, ignore.case = TRUE)
      } else {
        maintainer_id
      }
    },
    all_repo_maintainers_id,
    all_repo_maintainer_names,
    contributors_details
  )
  all_repo_maintainers_id <- all_repo_maintainers_id |>
    map(function(repo) {
      if (length(repo) == 0) {
        NULL
      } else {
        repo
      }
    })
  # combine maintainer id and name
  combine_id_name <- function(id, name) list(id = id, name = name)
  Map(
    combine_id_name,
    all_repo_maintainers_id,
    all_repo_maintainer_names
  )
}

#' get_maintainers_events
#'
#' Retrieves events for maintainers of a list of packages.
#'
#' @param package_list A list of package details including repository paths.
#' @param package_names A character vector of package names.
#' @param maintainers_details A list of maintainer details including maintainer
#' IDs and names.
#' @param api_key An optional API key for authentication.
#'
#' @return A list of events for the maintainers.
#'
#' @examples
#' get_maintainers_events(package_list, package_names, maintainers_details)
#' @export
get_maintainers_events <- function(
    package_list,
    package_names,
    maintainers_details,
    api_key = NULL) {
  # get repo events
  all_repo_events <- map(package_list, .f = function(package) {
    get_repo_events(
      repo_path = package[["repo"]],
      api_key
    )
  }) |>
    set_repo_name(repo_names = package_names)
  # get repo maintainer events
  Map(function(events, maintainer) {
    Filter(events, f = function(event) {
      event[["actor"]][["login"]] %in% maintainer[["id"]]
    }) |>
      head(1)
  },
  all_repo_events,
  maintainers_details
  ) |>
    map(function(events) {
      map(events, function(event) {
        event[["created_at"]] <- event[["created_at"]] |>
          as.POSIXct(format = "%Y-%m-%dT%H:%M:%SZ")
        event
      })
    })
}

#' get_maintainers_commits
#'
#' Retrieves commits for maintainers of a list of packages.
#'
#' @param package_list A list of package details including repository paths.
#' @param maintainers_details A list of maintainer details including maintainer
#' IDs and names.
#' @param api_key An optional API key for authentication.
#'
#' @return A list of commits for the maintainers.
#'
#' @examples
#' get_maintainers_commits(package_list, maintainers_details, "API_KEY")
#' @export
get_maintainers_commits <- function(
    package_list,
    maintainers_details,
    api_key = NULL) {
  Map(f = function(repo, package) {
    map(repo[["id"]], function(id) {
      get_repo_commits(
        repo_path = package[["repo"]],
        user_id = id,
        api_key
      ) |>
        head(1) |>
        unlist(recursive = FALSE)
    })
  },
  maintainers_details,
  package_list
  ) |>
    map(.f = function(repo) {
      map(repo, function(commit) {
        commit[["commit"]][["author"]][["date"]] <-
          as.POSIXct(
            commit[["commit"]][["author"]][["date"]],
            format = "%Y-%m-%dT%H:%M:%SZ"
          )
        commit
      })
    })
}

#' get_latest_activity
#'
#' Prepares a list of latest activity by maintainers
#'
#' @param maintainer_commits A list of latest commits for the maintainers.
#' @param maintainer_events A list of latest events for the maintainers.
#' @param maintainers A list of maintainer details with maintainer id and names.
#' @param packages A list of package details including repository paths.
#'
#' @return A list of latest activity by maintainers.
#' @export
get_latest_activity <- function(
    maintainer_commits,
    maintainer_events,
    maintainers,
    packages) {
  Map(
    function(commit, event, maintainer, package) {
      if (length(commit) == 0 & length(event) == 0)
        NULL
      else if (length(commit) == 0) {
        list(
          package = package[["name"]],
          name = maintainer[["name"]],
          id = event[[1]][["actor"]][["login"]],
          type = event[[1]][["type"]],
          timestamp = event[[1]][["created_at"]]
        )
      } else if (length(event) == 0) {
        list(
          package = package[["name"]],
          name = maintainer[["name"]],
          id = commit[[1]][["author"]][["login"]],
          type = "CommitEvent",
          timestamp = commit[[1]][["commit"]][["author"]][["date"]]
        )
      } else {
        if (
          commit[[1]][["commit"]][["committer"]][["date"]] >
            event[[1]][["created_at"]]
        ) {
          list(
            package = package[["name"]],
            name = maintainer[["name"]],
            id = commit[[1]][["author"]][["login"]],
            type = "CommitEvent",
            timestamp = commit[[1]][["commit"]][["author"]][["date"]]
          )
        } else {
          list(
            package = package[["name"]],
            name = maintainer[["name"]],
            id = event[[1]][["actor"]][["login"]],
            type = event[[1]][["type"]],
            timestamp = event[[1]][["created_at"]]
          )
        }
      }
    },
    maintainer_commits,
    maintainer_events,
    maintainers,
    packages
  )
}

#' recency_in_months
#'
#' Calculates the recency of an activity in months.
#'
#' @param activity A list of activity details including timestamp.
#'
#' @return The recency of the activity in months.
#' @export
get_recency_in_months <- function(activity) {
  interval(activity[["timestamp"]], Sys.Date()) / months(1)
}

#' get_slack_report
#'
#' Prepares a report for a Slack channel.
#'
#' @param activities A list of latest activity by maintainers.
#'
#' @return A list of activities within a month, within six months,
#' and more than six months.
#' @export
get_slack_report <- function(activities) {
  report <- list()
  report[["within_a_month"]] <- activities |>
    filter(activity_recency == "within a month") |>
    pull(package)
  report[["within_six_months"]] <- activities |>
    filter(activity_recency == "within six months") |>
    pull(package)
  report[["more_than_six_months"]] <- activities |>
    filter(activity_recency == "more than six months") |>
    pull(package)
  return(report)
}

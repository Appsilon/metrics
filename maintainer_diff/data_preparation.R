box::use(
  data.table[rbindlist],
  lubridate[interval, month],
  purrr[detect, map],
  yaml[read_yaml]
)
box::use(
  maintainer_diff / data_processing_utils[
    get_contributors_details,
    get_latest_activity,
    get_maintainers_details,
    get_maintainers_events,
    get_maintainers_commits,
    get_recency_in_months,
  ]
)
box::use(
  maintainer_diff / repo_data_utils[
    get_name_from_id,
    get_pr,
    get_pr_reviewers
  ]
)
pharma_dir <- "data/packages/"
nonpharma_dir <- "data/nonpharma/"
pharma_file_paths <- paste0(pharma_dir, list.files(pharma_dir))
nonpharma_file_paths <- paste0(nonpharma_dir, list.files(nonpharma_dir))

package_list <- map(pharma_file_paths, \(path) {
  file_content <- read_yaml(path)
  list(name = file_content$name, repo = file_content$repo)
})
package_names <- map(package_list, \(package) package[["name"]]) |>
  unlist()
api_key <- Sys.getenv("GH_API_KEY")

#----Get contributors id(s) and name(s) for each repository----
all_repo_contributors_details <- get_contributors_details(
  package_list,
  package_names,
  api_key
)

#----Get maintainers id(s) and name(s) for each repository----
all_repo_maintainers_details <- get_maintainers_details(
  package_list,
  package_names,
  all_repo_contributors_details,
  api_key
)

#----Get maintainers events for each repository----
all_repo_maintainer_events <- get_maintainers_events(
  package_list,
  package_names,
  all_repo_maintainers_details,
  api_key
)

#----Get maintainers commits for each repository----
all_repo_maintainer_commits <- get_maintainers_commits(
  package_list,
  all_repo_maintainers_details,
  api_key
) |>
  map(function(repo) {
    Filter(function(commit) length(commit) != 1, repo)
  })

#----Get list of maintainers' latest activity----
maintainers_latest_activity <- get_latest_activity(
  all_repo_maintainer_commits,
  all_repo_maintainer_events,
  all_repo_maintainers_details,
  package_list
)

#----This section is temporary and will be removed once the DESCRIPTION file
# is added to the validatoR package. The maintainer info is fetched from the
# requested requested reviewer info in the latest PR (open/closed)----
open_pr <- get_pr(
  repo_path = "insightsengineering/thevalidatoR",
  pr_state = "open",
  api_key
)
closed_pr <- get_pr(
  repo_path = "insightsengineering/thevalidatoR",
  pr_state = "closed",
  api_key
)

open_pr_reviewers <- get_pr_reviewers(open_pr)
closed_pr_reviewers <- get_pr_reviewers(closed_pr)

closed_pr_latest_reviewer <- closed_pr_reviewers |>
  detect(function(reviewer) length(reviewer[["reviewers"]]) != 0)
open_pr_latest_reviewer <- open_pr_reviewers |>
  detect(function(reviewer) length(reviewer[["reviewers"]]) != 0)

maintainer_detail <-
  if (is.null(open_pr_latest_reviewer)) {
    closed_pr_latest_reviewer
  } else if (is.null(closed_pr_latest_reviewer)) {
    open_pr_latest_reviewer
  } else {
    if (open_pr_latest_reviewer[["created_at"]] >
          closed_pr_latest_reviewer[["created_at"]]) {
      open_pr_latest_reviewer
    } else {
      closed_pr_latest_reviewer
    }
  }
maintainer_detail <- list(
  id = maintainer_detail[["reviewers"]][[1]][["login"]],
  name = get_name_from_id(
    maintainer_detail[["reviewers"]][[1]][["login"]],
    api_key
  )
)

maintainer_event <- get_maintainers_events(
  maintainers_details = list(maintainer_detail),
  package_list[30],
  package_names[30],
  api_key
)
maintainer_commit <- get_maintainers_commits(
  maintainers_details = list(maintainer_detail),
  package_list[30],
  api_key
)

#----Add validatoR package maintainer info to the
# maintainer_latest_activity list----
maintainers_latest_activity[["thevalidatoR"]] <- get_latest_activity(
  maintainer_commit,
  maintainer_event,
  list(maintainer_detail),
  package_list[30]
) |>
  unlist(recursive = FALSE)

maintainers_latest_activity <- maintainers_latest_activity |>
  map(\(activity) {
    if (get_recency_in_months(activity) <= 1) {
      activity[["activity_recency"]] <- "within a month"
    } else if (get_recency_in_months(activity) <= 6) {
      activity[["activity_recency"]] <- "within six months"
    } else {
      activity[["activity_recency"]] <- "more than six months"
    }
    activity
  }) |>
  rbindlist()

box::use(
  desc[description],
  glue[glue],
  httr2[
    req_headers,
    req_perform,
    req_url_query,
    request,
    resp_body_json,
  ],
  jsonlite[base64_dec],
  lintr[lint_dir],
  lubridate[month],
  purrr[map],
  yaml[read_yaml],
)

#' get_pr
#'
#' Retrieves pull requests from a GitHub repository based on the provided state.
#'
#' @param repo_path The GitHub repository path in the format "owner/repo".
#' @param pr_state The state of the pull requests
#' to retrieve ("open" or "closed").
#' @param api_key An optional API key for authentication.
#'
#' @return A list of pull request details
#'
#' @examples
#' get_pr("owner/repo", "open", "API_KEY")
#' @export
get_pr <- function(repo_path, pr_state, api_key = NULL) {
  if (!is.null(api_key))
    api_key <- paste("Bearer", api_key)
  request(glue("https://api.github.com/repos/{repo_path}/pulls")) |>
    req_url_query(state = pr_state) |>
    req_headers(
      Accept = "application/vnd.github+json",
      "X-GitHub-Api-Version" =  "2022-11-28",
      Authorization = api_key
    ) |>
    req_perform() |>
    resp_body_json()
}

#' get_pr_reviewers
#'
#' Retrieves the details of reviewers for a list of pull requests.
#'
#' @param pr_list A list of pull request details.
#'
#' @return A list of reviewer details including closed_at,
#' updated_at, and reviewers.
#'
#' @examples
#' get_pr_reviewers(pr_list)
#' @export
get_pr_reviewers <- function(pr_list) {
  reviewer_details <- pr_list |>
    map(.f = function(pr) {
      list(
        closed_at = as.POSIXct(
          pr[["closed_at"]],
          format = "%Y-%m-%dT%H:%M:%SZ"
        ),
        updated_at = as.POSIXct(
          pr[["updated_at"]],
          format = "%Y-%m-%dT%H:%M:%SZ"
        ),
        reviewers = pr[["requested_reviewers"]]
      )
    })
  return(reviewer_details)
}

#' get_name_from_id
#'
#' Retrieves the name of a GitHub user based on their user ID.
#'
#' @param user_id The user ID of the GitHub user.
#' @param api_key An optional API key for authentication.
#'
#' @return The name of the GitHub user.
#'
#' @examples
#' get_name_from_id("user_id", "API_KEY")
#' @export
get_name_from_id <- function(user_id, api_key = NULL) {
  if (!is.null(api_key))
    api_key <- paste("Bearer", api_key)
  user_data <- request(paste0("https://api.github.com/users/", user_id)) |>
    req_headers(
      Accept = "application/vnd.github+json",
      "X-GitHub-Api-Version" =  "2022-11-28",
      Authorization = api_key
    ) |>
    req_perform() |>
    resp_body_json()
  name <-
    if (is.null(user_data[["name"]]))
      NA
    else
      user_data[["name"]]
  return(name)
}

#' get_repo_events
#'
#' Retrieves events for a GitHub repository.
#'
#' @param repo_path The path of the GitHub repository in the
#' format "owner/repo".
#' @param api_key An optional API key for authentication.
#' @param page The page number of the results to retrieve.
#' @param per_page The number of results per page.
#'
#' @return A list of events for the repository.
#'
#' @examples
#' get_repo_events("owner/repo", "API_KEY", 1, 100)
#' @export
get_repo_events <- function(
    repo_path,
    api_key = NULL,
    page = 1,
    per_page = 100) {
  if (!is.null(api_key))
    api_key <- paste("Bearer", api_key)
  request(glue("https://api.github.com/repos/{repo_path}/events")) |>
    req_headers(
      Accept = "application/vnd.github+json",
      "X-GitHub-Api-Version" =  "2022-11-28",
      Authorization = api_key
    ) |>
    req_url_query(page = page, per_page = per_page) |>
    req_perform() |>
    resp_body_json()
}

#' get_repo_commits
#'
#' Retrieves commits for a GitHub repository by a specific user.
#'
#' @param repo_path The path of the GitHub repository in the
#' format "owner/repo".
#' @param user_id The user ID of the GitHub user.
#' @param api_key An optional API key for authentication.
#'
#' @return A list of commits for the repository by the specified user.
#'
#' @examples
#' get_repo_commits("owner/repo", "user_id", "API_KEY")
#' @export
get_repo_commits <- function(repo_path, user_id, api_key = NULL) {
  if (!is.null(api_key))
    api_key <- paste("Bearer", api_key)
  request(glue("https://api.github.com/repos/{repo_path}/commits")) |>
    req_headers(
      Accept = "application/vnd.github+json",
      "X-GitHub-Api-Version" =  "2022-11-28",
      Authorization = api_key
    ) |>
    req_url_query(author = user_id) |>
    req_perform() |>
    resp_body_json()
}

#' get_repo_contributors
#'
#' Retrieves contributors for a GitHub repository.
#'
#' @param repo_path The path of the GitHub repository in the
#' format "owner/repo".
#' @param api_key An optional API key for authentication.
#'
#' @return A list of contributors for the repository.
#'
#' @examples
#' get_repo_contributors("owner/repo", "API_KEY")
#' @export
get_repo_contributors <- function(repo_path, api_key = NULL) {
  if (!is.null(api_key))
    api_key <- paste("Bearer", api_key)
  request(glue("https://api.github.com/repos/{repo_path}/contributors")) |>
    req_headers(
      Accept = "application/vnd.github+json",
      "X-GitHub-Api-Version" =  "2022-11-28",
      Authorization = api_key
    ) |>
    req_perform() |>
    resp_body_json()
}

#' get_package_maintainers
#'
#' Retrieves the maintainers of a package in a GitHub repository.
#'
#' @param repo_path The path of the GitHub repository in the
#' format "owner/repo".
#' @param api_key An optional API key for authentication.
#'
#' @return A list of maintainers for the package.
#'
#' @examples
#' get_package_maintainers("owner/repo", "API_KEY")
#' @export
get_package_maintainers <- function(repo_path, api_key = NULL) {
  if (!is.null(api_key))
    api_key <- paste("Bearer", api_key)
  desc_data <- request(
    glue("https://api.github.com/repos/{repo_path}/contents/DESCRIPTION")
  ) |>
    req_headers(
      Accept = "application/vnd.github+json",
      "X-GitHub-Api-Version" =  "2022-11-28",
      Authorization = api_key
    ) |>
    req_perform() |>
    resp_body_json()
  desc_data <- rawToChar(base64_dec(desc_data$content))
  desc_data <- description$new(text = desc_data)
  desc_data$get_maintainer()
}

#' set_repo_name
#'
#' Sets the names of repositories in a list.
#'
#' @param repo_list A list of repositories.
#' @param repo_names A character vector of repository names.
#'
#' @return The updated list of repositories with names set.
#'
#' @examples
#' set_repo_name(repo_list, repo_names)
#' @export
set_repo_name <- function(repo_list, repo_names) {
  names(repo_list) <- repo_names
  return(repo_list)
}

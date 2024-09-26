box::use(testthat[test_that, expect_is, expect_equal])
box::use(maintainer_diff/repo_data_utils[...])

describe("get_pr", {
  it("return a list of pull requests", {
    # Arrange
    repo_path <- "appsilon/rhino"
    pr_state <- "closed"
    # Act
    pr_list <- get_pr(repo_path, pr_state)
    # Assert
    expect_is(pr_list, "list")
    expect_is(pr_list[[1]], "list")
  })
})
describe("get_pr_reviewers", {
  it("should return list of PR requested reviewers", {
    # Arrange
    repo_path <- "appsilon/shiny.fluent"
    pr_state <- "closed"
    # Act
    pr_list <- get_pr(repo_path, pr_state)
    reviewers_list <- get_pr_reviewers(pr_list)
    # Assert
    expect_is(reviewers_list, "list")
    expect_equal(length(reviewers_list[[1]]), 3)
  })
})
describe("get_name_from_id", {
  it("should return the name of the user", {
    # Arrange
    user_id <- "sankhadeepdutta"
    # Act
    name <- get_name_from_id(user_id)
    # Assert
    expect_is(name, "character")
  })
})
describe("get_repo_events", {
  it("should return a list of repository events", {
    # Arrange
    repo_path <- "appsilon/rhino"
    # Act
    events_list <- get_repo_events(repo_path)
    # Assert
    expect_is(events_list, "list")
    expect_is(events_list[[1]], "list")
  })
})
describe("get_repo_commits", {
  it("should return a list of commits done by the user", {
    # Arrange
    repo_path <- "appsilon/rhino"
    user_id <- "kamilzyla"
    # Act
    commits_list <- get_repo_commits(repo_path, user_id)
    # Assert
    expect_is(commits_list, "list")
  })
})
describe("get_repo_contributors", {
  it("should return a list of repository contributors", {
    # Arrange
    repo_path <- "appsilon/rhino"
    # Act
    contributors <- get_repo_contributors(repo_path)
    # Assert
    expect_is(contributors, "list")
  })
})
describe("get_package_maintainers", {
  it("should return the repository maintainer name", {
    # Arrange
    repo_path <- "appsilon/shiny.telemetry"
    # Act
    maintainers <- get_package_maintainers(repo_path)
    # Assert
    expect_is(maintainers, "character")
  })
})

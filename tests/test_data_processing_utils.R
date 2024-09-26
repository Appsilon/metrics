box::use(
  lubridate[days],
  testthat[describe, expect_equal, expect_is, it],
)
box::use(maintainer_diff/data_processing_utils[...])

describe("get_contributors_details", {
  it("should return a list of contributor details", {
    # Arrange
    package_list <- list(
      list(
        name = "modulecreateR",
        repo = "sankhadeepdutta/modulecreateR"
      )
    )
    package_names <- "modulecreateR"
    # Act
    contributors_details <- get_contributors_details(
      package_list,
      package_names
    )
    # Assert
    expect_is(contributors_details, "list")
  })
})
describe("get_maintainers_details", {
  it("should return a list of maintainer details", {
    # Arrange
    package_list <- list(
      list(
        name = "modulecreateR",
        repo = "sankhadeepdutta/modulecreateR"
      )
    )
    package_names <- "modulecreateR"
    # Act
    contributors_details <- contributors_details <- get_contributors_details(
      package_list,
      package_names
    )
    maintainers_details <- get_maintainers_details(
      package_list,
      package_names,
      contributors_details
    )
    # Assert
    expect_is(maintainers_details, "list")
  })
})
describe("get_maintainers_events", {
  it("should return a list of maintainer events", {
    # Arrange
    package_list <- list(list(name = "rhino", repo = "appsilon/rhino"))
    package_names <- "rhino"
    maintainers_details <- list(list(id = "kamilzyla", name = "Kamil Zyla"))
    # Act
    maintainers_events <- get_maintainers_events(
      package_list,
      package_names,
      maintainers_details
    )
    # Assert
    expect_is(maintainers_events, "list")
  })
})
describe("get_maintainers_commits", {
  it("should return a list of maintainer commits", {
    # Arrange
    package_list <- list(list(name = "rhino", repo = "appsilon/rhino"))
    maintainers_details <- list(list(id = "kamilzyla", name = "Kamil Zyla"))
    # Act
    maintainers_commits <- get_maintainers_commits(
      package_list,
      maintainers_details
    )
    # Assert
    expect_is(maintainers_commits, "list")
  })
})
describe("get_latest_activity", {
  # Arrange
  maintainer_commits <- list(
    list(
      list(
        author = list(login = "maintainer1"),
        commit = list(
          author = list(date = as.POSIXct("2023-01-01T12:00:00Z")),
          committer = list(date = as.POSIXct("2023-01-01T12:00:00Z"))
        )
      )
    ),
    list()
  )
  maintainer_events <- list(
    list(
      list(
        actor = list(login = "maintainer1"),
        type = "PushEvent",
        created_at = as.POSIXct("2023-01-01T10:00:00Z")
      )
    ),
    list(
      list(
        actor = list(login = "maintainer2"),
        type = "IssueCommentEvent",
        created_at = as.POSIXct("2023-01-02T12:00:00Z")
      )
    )
  )
  maintainers <- list(
    list(id = "maintainer1", name = "Maintainer One"),
    list(id = "maintainer2", name = "Maintainer Two")
  )
  packages <- list(
    list(name = "package1", repo = "repo1"),
    list(name = "package2", repo = "repo2")
  )
  expected <- list(
    list(
      package = "package1",
      name = "Maintainer One",
      id = "maintainer1",
      type = "PushEvent",
      timestamp = as.POSIXct("2023-01-01T12:00:00Z")
    ),
    list(
      package = "package2",
      name = "Maintainer Two",
      id = "maintainer2",
      type = "IssueCommentEvent",
      timestamp = as.POSIXct("2023-01-02T12:00:00Z")
    )
  )
  # Act
  result <- get_latest_activity(
    maintainer_commits,
    maintainer_events,
    maintainers,
    packages
  )
  # Assert
  expect_equal(result, expected)
})
describe("get_recency_in_months", {
  it("should return the time difference in months", {
    # Arrange
    activity <- list(timestamp = Sys.Date() - months(3))
    expected <- 3
    # Act
    result <- get_recency_in_months(activity)
    # Assert
    expect_equal(result, expected)
  })
})
describe("get_slack_report", {
  it("should return a list of packages classified by activity recency", {
    # Arrange
    activities <- data.frame(
      package = c("package1", "package2", "package3"),
      activity_recency = c(
        "within a month",
        "within six months",
        "more than six months"
      )
    )
    expected <- list(
      within_a_month = c("package1"),
      within_six_months = c("package2"),
      more_than_six_months = c("package3")
    )
    # Act
    result <- get_slack_report(activities)
    # Assert
    expect_equal(result, expected)
  })
})

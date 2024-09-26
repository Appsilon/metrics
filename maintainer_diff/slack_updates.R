box::use(
  dplyr[select],
  httr2[request, req_headers, req_body_json, req_perform]
)
box::use(maintainer_diff/data_processing_utils[get_slack_report])

#----Get the slack report
report <- maintainers_latest_activity |>
  select(package, activity_recency) |>
  get_slack_report()

#----Prepare the slack message
slack_msg <- paste(
  "*:rotating_light: Packages with no maintainers' activity in the last 6 months*", #nolint
  paste(":point_right:", paste(report$more_than_six_months, collapse = ", ")),
  "\n\n*:warning: Packages with maintainers' activity in the last 6 months*",
  paste(":point_right:", paste(report$within_six_months, collapse = ", ")),
  "\n\n*:white_check_mark: Packages with maintainers' activity within a month*",
  paste(":point_right:", paste(report$within_a_month, collapse = ", ")),
  "\n\n*Checkout the detailed report at 👇*",
  "https://connect.appsilon.com/pharmaverse-packages-status/",
  sep = "\n\n"
)

#----Send the slack message
request(Sys.getenv("INCOMING_WEBHOOK_URL")) |>
  req_headers(
    "Content-type" = "application/json"
  ) |>
  req_body_json(
    list(
      text = slack_msg
    )
  ) |>
  req_perform()

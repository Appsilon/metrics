box::use(
  reactable[reactableTheme],
)

box::use(
  styles / colors[navy, ice],
  styles / utils[add_opacity],
)

theme <- reactableTheme(
  color = ice[4],
  backgroundColor = navy[8],
  stripedColor = add_opacity(navy[7], 70),
  borderColor = navy[5],
  cellPadding = "6px 20px",
  searchInputStyle = list(
    backgroundColor = navy[6],
    padding = "10px 20px",
    width = "100%"
  ),
  selectStyle = list(backgroundColor = navy[6]),
  highlightColor = navy[6],
  pageButtonHoverStyle = list(backgroundColor = navy[6]),
  pageButtonActiveStyle = list(backgroundColor = navy[6])
)

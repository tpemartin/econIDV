# clipr::read_clip() |> xfun::write_utf8(
#   "support/figCSS_page.txt"
# )
xfun::read_utf8(
  "support/figCSS_page.txt"
) |>
  clipr::write_clip(
  )
fig <- econWeb::Fig("/Users/martinl/Github/econIDV/assets/img")
fig$split_css$`buttonHolder-icon`$remain$background <- NULL
fig$split_css$`buttonHolder-text`$remain$`text-align` = "center"
fig$update_css()
fig$export("assets/css/page")

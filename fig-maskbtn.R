library(econWeb)
fig <- Fig()
fig$split_css$`btnRound-maskGraph-graph`$remain$display = "none"
fig$split_css$`btnRound-maskGraph-color`$remain$`-webkit-mask` <-
  "no-repeat center url(/lib/attachment-1/graph.svg)"
fig$split_css$`btnRound-maskGraph-color`$remain$mask <-
  "no-repeat center url(/lib/attachment-1/graph.svg)"
fig$update_css()
fig$export("assets/css/maskbtn")

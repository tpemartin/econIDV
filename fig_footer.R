fig <- Fig()
fig$split_css$`footer-buttonHolder-Text`$inside_autoLayout$`text-align`="center"
fig$split_css$btnRound$auto_layout$padding="7px"
fig$split_css$`btnRound-maskGraph`$remain$height <-
  fig$split_css$`btnRound-maskGraph`$remain$width <- "25px"
fig$split_css$`btnRound-maskGraph`$remain$position="relative"
fig$split_css$`btnRound-maskGraph`$remain$left <-
  fig$split_css$`btnRound-maskGraph`$remain$top <- "0"
fig$update_css()
fig$export("assets/css/footer")

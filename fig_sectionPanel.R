# clipr::read_clip() |>
#   xfun::write_utf8("support/figcss_sectionPanel.txt")
xfun::read_utf8("support/figcss_sectionPanel.txt") |>
  clipr::write_clip()
fig <- econWeb::Fig()

fig$split_css$`sectionPanel-section-sectionTitle-logo`$remain$background="no-repeat center url(/lib/img-1/ntpuecon.svg)"
fig$split_css$`sectionPanel-section-graph`$remain$background="no-repeat center url(/lib/img-1/graphdemo.png)"
fig$update_css()
fig$export("assets/css/sectionPanel")

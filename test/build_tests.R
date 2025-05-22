source("src/build_themes.R")


ttt <- read_tmtheme("extras/textmate/Skeletor Syntax.tmTheme")

ttt |>
  filter(!is.na(scope)) |>
  group_by(scope) |>
  count(sort = TRUE) |>
  arrange(desc(n))


ttt |>
  filter(!is.na(scope)) |>
  pull(name) |>
  unique() |>
  sort()

ttt |>
  filter(!is.na(scope)) |>
  pull(scope) |>
  unique() |>
  sort() |>
  clipr::write_clip()



# theme <- "skeletor.scss"
#
# file.path("extras", "hljs", theme) |>
#   readLines() |>
#   sass::sass(cache = FALSE, output = "test/test_hljs.css")
#
# file.path("extras", "prismjs", theme) |>
#   readLines() |>
#   sass::sass(cache = FALSE, output = "test/test_prismjs.css")
#
# file.path("extras", "pygments", theme) |>
#   readLines() |>
#   sass::sass(cache = FALSE, output = "test/test_pygments.css")

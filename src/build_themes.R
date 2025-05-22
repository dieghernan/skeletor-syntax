# Create vscode (json) and RStudio (rstheme) variants using tmTheme as base

library(tidyverse)
tminput <- "./dist/tmtheme/Skeletor Syntax.tmTheme"

# Beautify tmTheme
xml2::read_xml(tminput) %>%
  xml2::write_xml(tminput)

source("src/functions.R")

# VScode -----
output <- basename(tminput) %>%
  str_replace_all(".tmTheme", "-color-theme.json") %>%
  str_replace_all(" ", "-") %>%
  file.path("dist", "vscode", "themes", .) |>
  tolower()

output

tmtheme2vscode(tminput, output)

# Prettify output
read_json(output) |>
  write_json(path = output, auto_unbox = TRUE, pretty = TRUE)

# And get type of theme here
them_type <- read_json(output)$type

message(basename(tminput), " is ", them_type)

# RStudio Theme ----

outdir <- "./dist/rstudio"
rtheme_out <- tools::file_path_sans_ext(tminput) |>
  basename() |>
  paste0(".rstheme") %>%
  file.path(outdir, .)


tmtheme2rstheme(tminput, rtheme_out)

# Skeletor Markdown here:
readLines(rtheme_out) %>%
  c(
    ".ace_markup.ace_heading {color: #DCE7FD;}",
    ".ace_heading {color: #BD93F9;}"
  ) |>
  # Compile and write
  sass::sass(output = rtheme_out, cache = FALSE)


# Apply the new theme
rstudioapi::addTheme(rtheme_out, apply = TRUE, force = TRUE)


#  Register themes ----
library(jsonlite)
library(tidyverse)

# Read produced vscode themes
myvs <- list.files("./dist/vscode/themes", full.names = TRUE)

the_df <- lapply(myvs, function(x) {
  js <- read_json(x)
  tibble::tibble(
    label = js$name,
    uiTheme = case_when(
      str_detect(js$type, "hc") ~ js$type,
      js$type == "light" ~ "vs",
      TRUE ~ "vs-dark"
    ),
    path = file.path(".", "themes", basename(x))
  )
}) %>%
  bind_rows() %>%
  mutate(ord = toupper(label)) |>
  arrange(ord) |>
  select(-ord)

tm <- list()

nm <- names(the_df)

for (i in seq_len(nrow(the_df))) {
  specs <- as.character(the_df[i, ])
  names(specs) <- nm

  tm[[i]] <- as.list(specs)
}

toJSON(tm, pretty = TRUE)




# Package json
thepak <- "dist/vscode/package.json"

pk <- read_json(thepak)
pk$contributes$themes <- tm

write_json(pk, thepak, pretty = TRUE, auto_unbox = TRUE)

file.copy("CHANGELOG.md", "dist/vscode", overwrite = TRUE)
file.copy("LICENSE", "dist/vscode", overwrite = TRUE)
file.copy("assets/icon.png", "dist/vscode", overwrite = TRUE)

# Build css/scss distros ----

library(tidyverse)


# List files

allcss <- list.files("src/themes", pattern = ".scss$", full.names = TRUE)


## Pygments (also used for compiling colors) ----

all_pygments <- allcss[grepl("_pygments", allcss)]
f <- all_pygments[1]
for (f in all_pygments) {
  out_sass <- basename(f) %>%
    gsub("_|pygments", "", .) %>%
    file.path("./dist", "pygments", .)

  out_css <- basename(out_sass) %>%
    gsub("_", "", .) |>
    tools::file_path_sans_ext() |>
    paste0(".css") %>%
    file.path("./dist", "pygments", .)

  out_css_min <- basename(out_sass) %>%
    tools::file_path_sans_ext() |>
    paste0(".min.css") %>%
    file.path("./dist", "pygments", .)
  in_f <- readLines(f)


  comp <- sass::sass(in_f,
    output = out_sass,
    cache = FALSE,
    options = sass::sass_options(output_style = "compact")
  )

  comp <- sass::sass(in_f,
    output = out_css,
    cache = FALSE,
    options = sass::sass_options(output_style = "compact")
  )
  comp <- sass::sass(in_f,
    output = out_css_min,
    cache = FALSE,
    options = sass::sass_options(output_style = "compressed")
  )
}



## Prismjs ----

all_prism <- allcss[grepl("_prismjs", allcss)]
f <- all_prism[1]
for (f in all_prism) {
  out_sass <- basename(f) %>%
    gsub("_|prismjs", "", .) %>%
    file.path("./dist", "prismjs", .)

  out_css <- basename(out_sass) %>%
    gsub("_", "", .) |>
    tools::file_path_sans_ext() |>
    paste0(".css") %>%
    file.path("./dist", "prismjs", .)

  out_css_min <- basename(out_sass) %>%
    tools::file_path_sans_ext() |>
    paste0(".min.css") %>%
    file.path("./dist", "prismjs", .)
  in_f <- readLines(f)


  comp <- sass::sass(in_f,
    output = out_sass,
    cache = FALSE,
    options = sass::sass_options(output_style = "compact")
  )

  comp <- sass::sass(in_f,
    output = out_css,
    cache = FALSE,
    options = sass::sass_options(output_style = "compact")
  )
  comp <- sass::sass(in_f,
    output = out_css_min,
    cache = FALSE,
    options = sass::sass_options(output_style = "compressed")
  )
}

## HighlightJS ----

all_hljs <- allcss[grepl("_hljs", allcss)]
f <- all_hljs[1]
for (f in all_hljs) {
  out_sass <- basename(f) %>%
    gsub("_|hljs", "", .) %>%
    file.path("./dist", "hljs", .)

  out_css <- basename(out_sass) %>%
    gsub("_", "", .) |>
    tools::file_path_sans_ext() |>
    paste0(".css") %>%
    file.path("./dist", "hljs", .)

  out_css_min <- basename(out_sass) %>%
    tools::file_path_sans_ext() |>
    paste0(".min.css") %>%
    file.path("./dist", "hljs", .)
  in_f <- readLines(f)


  comp <- sass::sass(in_f,
    output = out_sass,
    cache = FALSE,
    options = sass::sass_options(output_style = "compact")
  )

  comp <- sass::sass(in_f,
    output = out_css,
    cache = FALSE,
    options = sass::sass_options(output_style = "compact")
  )
  comp <- sass::sass(in_f,
    output = out_css_min,
    cache = FALSE,
    options = sass::sass_options(output_style = "compressed")
  )
}

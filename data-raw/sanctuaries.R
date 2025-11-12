librarian::shelf(
  dplyr, fs, glue, here, purrr, readr, sf, stringr, rvest, tidyr)

# variables
url     <- "https://sanctuaries.noaa.gov/library/imast_gis.html"
dir_raw <- here("data-raw/sanctuary_polygons")
dir_tmp <- tempdir()

dir.create(dir_raw, showWarnings = F)

# web scrape table with sanctuary names and shapefile zip links
rows = read_html(url) |>
  html_nodes(xpath = "/html/body/div/div/article/section/div/table") |>
  html_nodes(xpath = "tbody/tr")

d <- tibble(
  i         = seq_along(rows),
  sanctuary = map_chr(
    i, \(i)
    html_nodes(rows[[i]], xpath = "td")[[1]] |>
      html_text() |>
      str_replace(" Boundary Polygon", "") |>
      str_replace(" Polygon", "") |>
      str_replace("Updated [0-9/]+", "")),
  url_zip   = map_chr(
    i, \(i){
      z <- html_nodes(rows[[i]], xpath = "td")[[5]] |>
        html_nodes("a") |>
        html_attr("href")
      ifelse(
        str_detect(z, "^/"),
        glue("https://sanctuaries.noaa.gov{z}"),
        glue("{dirname(url)}/{z}") )}),
  nms       = map_chr(
    url_zip, \(url_zip)
    ifelse(
      str_detect(url_zip, "wisconsin-shipwreck"),
      "WSCNMS",
      str_replace(url_zip, ".*[imast|gis]/(.*)_py.*\\.zip", "\\1") |>
        toupper() )),
  geojson    = glue("{dir_raw}/{nms}.geojson"),
  url_to_geo = map2_lgl(
    url_zip, geojson, \(url_zip, ply_json){
      if (file_exists(ply_json))
        return(T)

      message("Downloading ", basename(url_zip), " to ", basename(ply_json))
      zip <- file.path(dir_tmp, basename(url_zip))
      download.file(url_zip, zip)
      f_shp <- unzip(zip, list = T) |>
        filter(str_detect(Name, "shp$")) |>
        filter(str_detect(Name, "^__", negate=T)) |>
        filter(str_detect(Name, "Albers", negate=T)) |> # PMNM_py_Albers.shp
        pull(Name)
      unzip(zip, exdir = dir_tmp)

      file.path(dir_tmp, f_shp) |>
        read_sf() |>
        st_transform(4326) |>
        write_sf(ply_json)

      unlink(dirname(f_shp))
      unlink(zip)

      return(T)
    }),
  sf      = map(geojson, read_sf),
  sf_rows = map_int(sf, \(x) nrow(x)),
  geom    = map(sf, \(x) st_union(x) |>  st_cast("MULTIPOLYGON"))) |>
  select(-i, -url_to_geo, -geojson) |>
  relocate(nms) |>
  arrange(nms)

# d |> filter(sf_rows > 1) |> select(nms, sanctuary, sf_rows)
# A tibble: 4 × 3
#   nms     sanctuary                       sf_rows
#   <chr>   <chr>                             <int>
# 1 CINMS   Channel Islands                       2
# 2 FGBNMS  Flower Garden Banks                  19
# 3 HIHWNMS Hawaiian Islands Humpback Whale       5
# 4 NMSAS   American Samoa                       14
# TODO: consider adding a sanctuaries_parts data object with these individual polygons

# setup sanctuaries
sanctuaries <- d |>
  select(nms, sanctuary, url_zip, geom) |>
  unnest(geom) |>
  st_as_sf(crs=4326)

# write table
sanctuaries |>
  st_drop_geometry() |>
  write_csv(here("data-raw/sanctuaries.csv"))

usethis::use_data(sanctuaries, overwrite = T)

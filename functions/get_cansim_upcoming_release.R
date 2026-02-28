
get_cansim_upcoming_release <- function() {
  
  release_schedule <- cansim::get_cansim_key_release_schedule()
  
  upcoming_releases <- release_schedule |>
    filter(date >= Sys.Date()) |>
    select(-url)
  
  assign("cansim_upcoming_releases", upcoming_releases, envir = globalenv())
  return(upcoming_releases)
}

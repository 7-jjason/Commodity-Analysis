# define url
url_main <- "https://www.naesb.org/members/urls_of_pipelines.htm"
# download main page content
webp_main <- read_html(url_main)
# get all urls
ip_urls <- webp_main |>
  html_elements(xpath = "//text()[contains(., 'Informational Postings')]/following::a[1]") %>%
  html_attr("href") |>
  # get unique urls
  unique()

for (i in seq_along(ip_urls)) {
  
}
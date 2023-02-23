##################################################
######## Author: Yan Han 
######## Date: Feb 22, 2023

# OpenAlex R
# OpenAlex Documentation: https://docs.openalex.org/
# Documentation: https://github.com/massimoaria/openalexR/tree/b3541e4f2695da771f15d0d92a7a050757e67e9c 


# Install the developer version of openalexR from GitHub
install.packages("remotes")
remotes::install_github("massimoaria/openalexR")

# Install the released version of openalexR from CRAN 
install.packages("openalexR")

# Before we go any further, we highly recommend you set openalexR.mailto option so that your requests go to the polite pool for faster response times: 
# yhan: No documentation link to the polite pool. So I am not sure if an email will get you a higher priority to get response??? 
options (openalexR.mailto="yhan@arizona.edu")

# common libraries to add
library(openalexR)
library(dplyr)
library(ggplot2)

############################# DOI as filter ########################################
# Getting record using DOIs
works_from_dois <- oa_fetch( 
  doi = c("https://doi.org/10.6017/ital.v40i1.12553", "10.1016/j.joi.2017.08.007"),
  entity = "works",
  verbose = TRUE
  ) 

# show the records
str(works_from_dois, max.level=2) 
head(works_from_dois)
show_works(works_from_dois)

works_from_dois |> 
  show_works() |>
  knitr::kable()

#> Requesting url: https://api.openalex.org/works?filter=doi%3A10.1016%2Fj.joi.2017.08.007%7Chttps%3A%2F%2Fdoi.org%2F10.1093%2Fbioinformatics%2Fbtab727


############################## ORCID as filter ####################################
# Download all works published by a set of authors using ORCIDs
# use author.orcid as a filter
works_from_dois <- oa_fetch(
  entity = "works",
  #author.orcid = c("0000-0001-9518-2684"),
  author.orcid = c("0000-0001-9518-2684", "0000-0002-8517-9411"),
  verbose = TRUE
) 

works_from_dois |>
  show_works()  |>
  knitr::kable()
#> Requesting url: https://api.openalex.org/works?filter=author.orcid%3A0000-0001-9518-2684%7C0000-0002-8517-9411
# > output to a file 


############################ Authors ################################################33
###### Getting all works published by a set of authors using ORCIDs
works_from_orcids <- oa_fetch(
  entity = "works",
  author.orcid = c("0000-0001-9518-2684"), # Only 1 record. not sure why? 
  #author.orcid = c("0000-0001-9518-2684", "0000-0003-1613-5981"),
  verbose = TRUE
)  

#> Requesting url: https://api.openalex.org/works?filter=author.orcid%3A0000-0001-6187-6610%7C0000-0002-8517-9411
#> Getting 2 pages of results with a total of 211 records...

works_from_orcids |> 
  show_works() |> 
  knitr::kable()

##### Getting authors' info using their ORCIDs
authors_from_orcids <- oa_fetch(
  entity = "authors",
  #orcid = c("0000-0001-9518-2684"),
  #orcid = c("0000-0001-6187-6610", "0000-0002-8517-9411", "0000-0001-9518-2684") # Han record not right. not sure why
  orcid =  c("0000-0001-6187-6610", "0000-0002-8517-9411", "0000-0003-1613-5981") # working right
)

authors_from_orcids |> 
 show_authors() |>
 knitr::kable()

  
###################### Author name ####################################
###  use disp_name
authors_from_names <- oa_fetch(
  entity = "authors",
  display_name = c("Massimo Aria", "Jason Priem", "Heather Piwowar"),
  # display_name = c("Yan Han", "Trang T. Le"),   ### "Yan Han" does not work. 
  has_orcid = TRUE
) 
authors_from_names |> 
  show_authors() |>
  knitr::kable()

#################### University (institution.id) as filter ######################
# OpenAlex ID: https://explore.openalex.org/
# OpenAlex ID for UA: https://explore.openalex.org/institutions/I138006243
# Download all authors' records (>1 publications) who currently work at the University of Arizona (OpenAlex ID: I138006243. looks like this is the "I"+"mag") 
# id":"https://openalex.org/I138006243","ror":"https://ror.org/03m2x1q45","display_name":"University of Arizona","relevance_score":82564.97,"country_code":"US","type":"education","homepage_url":"http://www.arizona.edu/",

org_args <- list(
  entity = "authors",
  last_known_institution.id = "I138006243",  # University of Arizona OpenAlex ID
  # last_known_institution.id = "I71267560", # I71267560 is University of Naples Federio II OpenAlex ID
  works_count = ">0"
)

# If works_count > 499, result is 129
# if works_count > 0, result is 36,889


do.call(oa_fetch, c(org_args, list(count_only = TRUE)))

# Download the list
top_authors <- do.call(oa_fetch, org_args) |>
  show_authors() |>
  knitr::kable()

# see the head of the authors' list
install.packages("listviewer")
library(listviewer)
jsonedit(top_authors)
head(top_authors)
###############


###################### Rank institutions by the number of citations ############### 
# U.S. institutions (country_code:us) are classified as educational (type:education)

country_insts <- oa_fetch(
  entity = "institutions",
  country_code = "us",
  type = "education",
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/institutions?filter=country_code%3Aus%2Ctype%3Aeducation
# 22 pages of results with a total of 4334 records... 

country_insts |>
  slice_max(cited_by_count, n = 40) |>    # U Arizona is ranked as 35
  mutate(display_name = forcats::fct_reorder(display_name, cited_by_count)) |>
  ggplot() +
  aes(x = cited_by_count, y = display_name, fill = display_name) +
  geom_col() +
  scale_fill_viridis_d(option = "E") +
  guides(fill = "none") +
  labs(
    x = "Total citations", y = NULL,
    title = "USA Citation Ranking"
  ) 
  coord_cartesian(expand = FALSE)

  
  ## +++++++++++ Concept cloud 
  concept_cloud <- country_insts %>%
    select(inst_id = id, x_concepts) %>%
    tidyr::unnest(x_concepts) %>%
    filter(level == 1) %>%
    select(display_name, score) %>%
    group_by(display_name) %>%
    summarise(score = sum(score))
  
  install.packages("wordcloud")
  library("wordcloud")
  pal <- c("black", scales::brewer_pal(palette = "Set1")(5))
  set.seed(1)
  wordcloud::wordcloud(
    concept_cloud$display_name,
    concept_cloud$score,
    scale = c(2, .4),
    colors = pal
  )
  
  
  
  
############################ Filters: # of citations && keyword in the title && sorted #################################
# Get works cited > 50 tiles published 2020-2021, and include the string "bibliometrics analysis" or "AI" in the title.
# Sort the results by total citations in a descending order

oa_fetch(
  entity = "works",
  title.search = c("bibliometric analysis", "AI"),
  cited_by_count = ">50",
  from_publication_date = "2020-01-01",
  to_publication_date = "2021-12-31",
  sort = "cited_by_count:desc",
  verbose = TRUE
) %>%
  show_works() %>%
  knitr::kable()
#> Requesting url: https://api.openalex.org/works?filter=title.search%3Abibliometric%20analysis%7CAI%2Ccited_by_count%3A%3E50%2Cfrom_publication_date%3A2020-01-01%2Cto_publication_date%3A2021-12-31&sort=cited_by_count%3Adesc


#################### Concepts as filter #######################33
install.packages("gghighlight")
library(gghighlight)
concept_df <- oa_fetch(
  entity = "concepts",
  level = 1,
  ancestors.id = "https://openalex.org/C86803240", # Biology
  works_count = ">1000000"
)

concept_df %>%
  select(display_name, counts_by_year) %>%
  tidyr::unnest(counts_by_year) %>%
  filter(year < 2022) %>%
  ggplot() +
  aes(x = year, y = works_count, color = display_name) +
  facet_wrap(~display_name) +
  geom_line(linewidth = 0.7) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = NULL, y = "Works count",
    title = "Virology spiked in 2020."
  ) +
  guides(color = "none") +
  gghighlight(
    max(works_count) > 244000, 
    label_params = list(nudge_y = 10^5, segment.color = NA)
  )
#> label_key: display_name



############################ Visualize big journals' topics ########################3
jours <- oa_fetch(
  entity = "venues",
  works_count = ">500000",
  verbose = TRUE
) %>%
  filter(publisher != "Elsevier"|is.na(publisher)) %>%
  distinct(display_name, .keep_all = TRUE) %>%
  select(jour = display_name, x_concepts) %>%
  tidyr::unnest(x_concepts) %>%
  filter(level == 0) %>%
  left_join(concept_abbrev) %>%
  mutate(abbreviation = gsub(" ", "<br>", abbreviation)) %>%
  tidyr::complete(jour, abbreviation, fill = list(score = 0)) %>%
  group_by(jour) %>%
  mutate(
    color = if_else(score > 10, "#1A1A1A", "#D9D9D9"), # CCCCCC
    label = paste0("<span style='color:", color, "'>", abbreviation, "</span>")
  )

install.packages("ggtext")
library("ggtext")
jours %>%
  ggplot() +
  aes(fill = jour, y = score, x = abbreviation, group = jour) +
  facet_wrap(~jour) +
  geom_hline(yintercept = c(45, 90), colour = "grey90", linewidth = 0.2) +
  geom_segment(
    aes(x = abbreviation, xend = abbreviation, y = 0, yend = 100),
    color = "grey95"
  ) +
  geom_col(color = "grey20") +
  coord_polar(clip = "off") +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtext::geom_richtext(
    aes(y = 120, label = label),
    fill = NA, label.color = NA, size = 3
  ) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = "none") +
  labs(y = NULL, x = NULL, title = "Journal clocks")



######## Snowball #####################


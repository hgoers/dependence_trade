# This script sources bilateral trade data from UN COMTRADE.

library(tidyverse)
library(jsonlite)
library(glue)

# Get available data ------------------------------------------------------

country_id <- fromJSON("https://comtrade.un.org/api/refs/da/view?freq=A&ps=2019&px=HS") |>
  select(r, rDesc)

# Get trade data ----------------------------------------------------------

comtrade_pull <- function(r, cmd, dir, yr) {

  query <- glue("https://comtrade.un.org/api/get?r={r}&cc={cmd}&rg={dir}&ps={yr}&fmt=json&freq=A&head=H")

  result <- tryCatch(

    fromJSON(query),

    error = function(err) {

      print(glue("Limit reached at {lubridate::now()}, wait one hour"))
      Sys.sleep(3600)
      return(fromJSON(query))

    }

  )

  trade_df <- fromJSON(query)$dataset |>
    as_tibble()

  if (length(trade_df) != 0) {

    print(glue("Pulling data for {r}"))
    return (trade_df)

  } else {

    print(glue("No data for {r}"))
    return(NULL)

  }

}

safely_comtrade_pull <- safely(comtrade_pull)

trade_raw <- map_dfr(country_id$r, ~ safely_comtrade_pull(.x, 50, 2, 2019))

trade_df <- trade_raw$result |>
  janitor::clean_names() |>
  select(
    yr, direction = rg_desc, reporter = rt_title, partner = pt_title, cmd_code, trade_value
  ) |>
  filter(partner != "World", trade_value > 1000)

reporter <- distinct(trade_df, entity = reporter)
partners <- distinct(trade_df, entity = partner)

nodes <- bind_rows(reporter, partners)

edges <- select(trade_df, reporter, partner, trade_value)

trade_network <- tbl_graph(
  nodes = nodes,
  edges = edges,
  directed = T
)

ggraph(trade_network) +
  geom_edge_link(aes(width = trade_value), alpha = 0.8, arrow = arrow(length = unit(2, 'mm'))) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_point() +
  geom_node_text(aes(label = entity), repel = TRUE) +
  theme_graph()

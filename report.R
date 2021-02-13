# INTERACTIVE PLOTS ----

# GOAL: DEVELOP INTERACTIVE PLOTS FOR A SALES REPORT

# LIBRARIES & DATA ----

install.packages("flexdashboard")
# Main
library(tidyverse)
library(lubridate)

# Visualization
library(plotly)

bikes_tbl      <- readRDS("C:/Users/sagor/Documents/GitHub/data_products-ttsagor/bikes_tbl.rds")
bikeshops_tbl  <- readRDS("C:/Users/sagor/Documents/GitHub/data_products-ttsagor/bikeshops_tbl.rds")
orderlines_tbl <- readRDS("C:/Users/sagor/Documents/GitHub/data_products-ttsagor/orderlines_tbl.rds")

bike_orderlines_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl,     by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer_id" = "bikeshop_id")) %>%
  
  # Add the total price
  mutate(total_price = price_euro * quantity)

format_to_euro <- function(x, suffix = " €") {
  
  scales::dollar(x,
                 suffix       = suffix,
                 prefix       = "",
                 big.mark     = ".",
                 decimal.mark = ",")
}

euro_format <- function(scale        = 1,
                        prefix       = "",
                        suffix       = " €",
                        big.mark     = ".",
                        decimal.mark = ",") {
  
  scales::dollar_format(suffix       = suffix,
                        prefix       = prefix,
                        big.mark     = big.mark,
                        decimal.mark = decimal.mark,
                        scale        = scale)
  
}

# 1.0 TOTAL SALES BY MONTH ----

# 1.1 Preparing Time Series Data ----
# Monthly

total_sales_m_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(date_rounded = floor_date(order_date, unit = "month")) %>%
  
  group_by(date_rounded) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

total_sales_m_tbl


# 1.2 Interactive Plot ----

# Step 1: Create ggplot with text feature

g1 <- total_sales_m_tbl %>%
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

g1

# Step 2: Use ggplotly()
ggplotly(g1)

ggplotly(g1, tooltip = "text")


# 1.3 Plot Total Sales Function ----

plot_total_sales <- function(unit = "month", date_format = "%B %Y", interactive = TRUE) {
  
  # Handle Data
  data_tbl <- bike_orderlines_tbl %>%
    
    select(order_date, total_price) %>%
    
    mutate(date_rounded = floor_date(order_date, unit = unit)) %>%
    
    group_by(date_rounded) %>%
    summarise(total_sales = sum(total_price)) %>%
    ungroup() %>%
    
    mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format(date_format)}"))
  
  # Make Plot
  g1 <- data_tbl %>%
    ggplot(aes(x = date_rounded, y = total_sales)) +
    
    # Geoms
    geom_point(aes(text = label_text), color = "#2C3E50") +
    geom_smooth(method = "loess", span = 0.2) +
    
    # Formatting
    scale_y_continuous(labels = euro_format()) +
    expand_limits(y = 0) +
    labs(
      title = "Total Sales",
      y = "Revenue (Euro)",
      x = ""
    )
  
  # Static vs Interactive Logic
  if (interactive) {
    return(ggplotly(g1, tooltip = "text"))
  } else {
    return(g1)
  }
  
}

# 1.4 Test Our Function ----

plot_total_sales(unit = "weekly", date_format = "%B %d, %Y", interactive = TRUE)

plot_total_sales(unit = "monthly", date_format = "%B %Y", interactive = TRUE)


# 2.0 CATEGORY 2 SALES BY MONTH ----

# 2.1 Preparing Time Series Data ----

category_2_sales_m_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_1, category_2, total_price) %>%
  mutate(date_rounded = floor_date(order_date, unit = "month")) %>%
  
  group_by(date_rounded, category_1, category_2) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}")) %>%
  
  mutate(category_2 = as_factor(category_2) %>%
           fct_reorder2(date_rounded, total_sales))

# 2.2 Interactive Plot ----

# Step 1: Create ggplot
g2 <- category_2_sales_m_tbl %>%
  ggplot(aes(x = date_rounded, y = total_sales, color = category_2)) +
  
  # Geoms
  geom_point(aes(text = label_text)) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~ category_2, scales = "free_y", ncol = 3) +
  
  # Formatting
  expand_limits(y = 0) +
  theme(legend.position = "none",
        # Change the height so the text looks less squished
        strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = "pt"))) +
  scale_y_continuous(labels = euro_format(scale = 1e-3, suffix = "K €")) +
  labs(
    title = "Sales By Category 2",
    y = "", x = ""
  )

# Step 2: Use ggplotly()
ggplotly(g2, tooltip = "text")







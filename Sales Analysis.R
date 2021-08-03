library(tidyverse)
library(lubridate)
library(tidyquant)
library(readxl)
library(writexl)
bikes_tbl <- read_excel(path = "data/bike_sales/data_raw/bikes.xlsx")

bikeshops_tbl <- read_excel(path = "data/bike_sales/data_raw/bikeshops.xlsx")

orderlines_tbl <- read_excel(path = "data/bike_sales/data_raw/orderlines.xlsx")

glimpse(bikes_tbl)

bikeshops_tbl

orderlines_tbl

bikes_tbl

left_join(orderlines_tbl, bikes_tbl, by = c ("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c ("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c ("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl

bike_orderlines_joined_tbl %>% glimpse()
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%

separate(description,
         into = c ("category.1" , "category.2" , "frame.material"),
         sep = " - ", 
         remove = TRUE) %>%
    separate(location,
             into = c ("city", "state"),
             sep = ", ",
             remove = FALSE) %>%
    mutate(total.price = price * quantity) %>%
    select(-...1, -location) %>%
  
    select(-ends_with(".id")) %>%
    
    bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
    
    select(contains("date"), contains("id"), contains("order"),
           quantity, price, total.price,
           everything ()) %>%
    rename(order_date = order.date) %>%
    
    set_names(names(.) %>% str_replace_all("\\.", "_")) 

bike_orderlines_wrangled_tbl %>% glimpse()
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>% 
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    group_by(year) %>% 
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    mutate(sales_text = scales::dollar(sales))

sales_by_year_tbl 

sales_by_year_tbl%>%  
    ggplot(aes(x = year, y = sales)) + 
    geom_col(fill = "#2C3E50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) +    
![](https://github.com/MaheenAbbas/Sales-Analysis-with-R/blob/main/images/Screenshot1ggplot.png?raw=true)
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year",
        subtitle = "Upward trend",
        x = "",
        y = "Revenue"
    )
sales_by_year_cat_2_tbl <-bike_orderlines_wrangled_tbl %>%
    select(order_date, total_price, category_2) %>%
    mutate(year = year(order_date)) %>%
    group_by(year, category_2) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    mutate(sale_text = scales::dollar(sales))


sales_by_year_cat_2_tbl %>%
    ggplot(aes(x = year, y = sales, fill = category_2)) +
    geom_col() + 
    geom_smooth(method = "lm", se = FALSE) + 
    facet_wrap(~category_2, ncol = 3, scales = "free_y") +
    theme_tq() + 
    scale_fill_tq() + 
    scale_y_continuous(labels = scales::dollar) + 
    labs(
        title = "Revenue by Year and Category 2",
        subtitle = "Ech product category has an upward trend", 
        x = "", 
        y = "Revenue", 
        fill = "Product Secondary Catergory"
        
    )

fs::dir_create("00_data/bike_sales/data_wrangled_student")
bike_orderlines_wrangled_tbl %>% 
    write_xlsx("00_data/bike_sales/data_wrangled_student/bike_orderlines.xlsx")
bike_orderlines_wrangled_tbl %>%
    write_csv("00_data/bike_sales/data_wrangled_student/bike_orderlines.csv")
bike_orderlines_wrangled_tbl %>%
    write_rds("00_data/bike_sales/data_wrangled_student/bike_orderlines.rds")




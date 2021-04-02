# Sales-Analysis-with-R


```{r}

#R FOR BUSINESS ANALYSIS
#JUMPSTART 

# 1.0 Load the following libraries 

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)

# 2.0 Import the files
# ? and some function can tell you the description about that specific function
?read_excel()

bikes_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx")

bikeshops_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikeshops.xlsx")

orderlines_tbl <- read_excel(path = "00_data/bike_sales/data_raw/orderlines.xlsx")

# 3.0 Examining the data from the files
 # view using glimpse function
glimpse(bikes_tbl)

bikeshops_tbl

orderlines_tbl

# 4.0 Joining Data ----
?left_join

orderlines_tbl

bikes_tbl

left_join(orderlines_tbl, bikes_tbl, by = c ("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c ("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c ("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
#separate description into category.1, category.2 and frame.material

separate(description,
         into = c ("category.1" , "category.2" , "frame.material"),
         sep = " - ", 
         remove = TRUE) %>%
#SEPEATE LOCATION INTO THE CITY AND STATE
    separate(location,
             into = c ("city", "state"),
             sep = ", ",
             remove = FALSE) %>%
# price extended/ mutate function
    mutate(total.price = price * quantity) %>%
# %>% means to pipe into- connect 
    
#reorganize- delete a column by using select (-) 
    select(-...1, -location) %>%
  
    select(-ends_with(".id")) %>%
    
    bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
    
#reorder cols- put date first 
    
    select(contains("date"), contains("id"), contains("order"),
           quantity, price, total.price,
           everything ()) %>%
    
#rename cols
    rename(order_date = order.date) %>%
    
    set_names(names(.) %>% str_replace_all("\\.", "_")) 
#regular expression- taking a string operation 
    #toupper ()) %>%
   # names() %>%
    
    #view and glimpse are the same
    #glimpse()
  
bike_orderlines_wrangled_tbl %>% glimpse()

# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>% 
    #selecting columns to focus on
    select(order_date, total_price) %>%
    #adding a year col 
    mutate(year = year(order_date)) %>%
    #new func- group by and summarizing sales
    group_by(year) %>% 
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    
    #$ format text 
    mutate(sales_text = scales::dollar(sales))

sales_by_year_tbl 

sales_by_year_tbl%>%  
    #setup the canvas with year on x-axis and sales on y-axis
    ggplot(aes(x = year, y = sales)) + 
    #geometries- bars- color- labels
    geom_col(fill = "#2C3E50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) +
    #formatting 

'''


![](https://
hub.com/MaheenAbbas/Sales-Analysis-with-R/blob/main/images/Screenshot1ggplot.png?raw=true)
    
# Step 2 - Visualize
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year",
        subtitle = "Upward trend",
        x = "",
        y = "Revenue"
    )


# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate
sales_by_year_cat_2_tbl <-bike_orderlines_wrangled_tbl %>%
    #selecting the col to focus on and year col 
    select(order_date, total_price, category_2) %>%
    mutate(year = year(order_date)) %>%
    
    #groupby and summarize on year and catergory 2
    group_by(year, category_2) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    
    #format $ Text 
    mutate(sale_text = scales::dollar(sales))




# Step 2 - Visualize
sales_by_year_cat_2_tbl %>%
    ggplot(aes(x = year, y = sales, fill = category_2)) +
    #geometries 
    geom_col() + 
    geom_smooth(method = "lm", se = FALSE) + 
    # facet
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




7.0 Writing Files ----

fs::dir_create("00_data/bike_sales/data_wrangled_student")
7.1 Excel ----
bike_orderlines_wrangled_tbl %>% 
    write_xlsx("00_data/bike_sales/data_wrangled_student/bike_orderlines.xlsx")
7.2 CSV 
bike_orderlines_wrangled_tbl %>%
    write_csv("00_data/bike_sales/data_wrangled_student/bike_orderlines.csv")
# 7.3 RDS ----

bike_orderlines_wrangled_tbl %>%
    write_rds("00_data/bike_sales/data_wrangled_student/bike_orderlines.rds")

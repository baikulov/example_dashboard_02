cohorts <- function(x){
  
  # подключаем библиотеку
  library('dplyr')
  library('tidyr')
  library('lubridate')
    
  #читаем файл c данными
  orders <- read.csv(x)
  
  # меняем формат даты
  orders$order_date <- as.Date(orders$order_date)
  
  # считаем дату первого заказа для каждого клиента
  first_date <- orders %>%
                          subset(order_date >= '2016-01-01') %>%
                          group_by(customer_id) %>%
                          arrange(order_date) %>%
                          mutate(first_date = min(order_date))
  
  # функция для высчитывания первого дня месяца
  monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
  }
  
  # считаем начало месяца
  first_date$first_month <- monthStart(first_date$first_date)
  
  # функция для высчитывания разницы между месяцами
  elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
  }
  
  # добавляем колонку с номером месяца возврата
  first_date$month_returned <- elapsed_months(
    start_date = first_date$first_date,
    end_date = first_date$order_date)
  
  # устанавлием порог по сроку заказа не позднее 365 дней с момента первого заказа
  first_date$limit <- first_date$first_date + days(365)
  
  # считаем когорты? кол-во клиентов и отношение к месяцу первого заказа
  cohorts <- first_date %>%
    subset(order_date <= limit)  %>%
    group_by(first_month, month_returned) %>%
    summarise(n = n_distinct(customer_id))  %>%
    group_by(first_month) %>%
    arrange(first_month, month_returned) %>%
    mutate(n2 = round(n / first(n, 1, month_returned), 3)) %>%
    ungroup()
  
  
  # финализируем таблицу с разбивкой по месяцам
  final <- cohorts  %>%
    pivot_wider(
      id_cols = 'first_month',
      names_from = 'month_returned',
      names_sort = TRUE,
      values_from = 'n2'
      )

}

system.time(
  # используем функцию и считаем когорту
  final <- cohorts('example_02.csv')
)

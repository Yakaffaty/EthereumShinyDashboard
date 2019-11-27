server <- function(input, output) {
    output$menu <- renderMenu({
        sidebarMenu(
            menuItem("Menu item", icon = icon("calendar"))
        )
    })
    
    RV <- reactiveValues()
    
    observeEvent(list(input$menu, input$rango_fechas) ,{
        projectid = "gold-rope-260104"
        ################################################################################################
        #------------------------ Inscico A Hash Rates Active Accounts ---------------------------------
        ################################################################################################
        #bq_auth(path="My First Project-3530235bacf9.json")
        
        queryHash = "
                WITH block_rows AS (
                  SELECT *, ROW_NUMBER() OVER (ORDER BY timestamp) AS rn
                  FROM `bigquery-public-data.crypto_ethereum.blocks`
                ),
                delta_time AS (
                  SELECT
                  mp.timestamp AS block_time,
                  mp.difficulty AS difficulty,
                  TIMESTAMP_DIFF(mp.timestamp, mc.timestamp, SECOND) AS delta_block_time
                  FROM block_rows mc
                  JOIN block_rows mp
                  ON mc.rn = mp.rn - 1
                ),
                hashrate_book AS (
                  SELECT TIMESTAMP_TRUNC(block_time, DAY) AS block_day,
                  AVG(delta_block_time) as daily_avg_block_time,
                  AVG(difficulty) as daily_avg_difficulty
                  FROM delta_time
                  GROUP BY TIMESTAMP_TRUNC(block_time, DAY)
                )
                SELECT block_day,
                (daily_avg_difficulty/daily_avg_block_time)/1000000000 as hashrate
                FROM hashrate_book
                ORDER BY block_day ASC
            "
        
        queryActive = "with double_entry_book as (
                -- debits
                select to_address as address, value as value, block_timestamp
                from `bigquery-public-data.crypto_ethereum.traces`
                where to_address is not null
                and status = 1
                and (call_type not in ('delegatecall', 'callcode', 'staticcall') or call_type is null)
                union all
                -- credits
                select from_address as address, -value as value, block_timestamp
                from `bigquery-public-data.crypto_ethereum.traces`
                where from_address is not null
                and status = 1
                and (call_type not in ('delegatecall', 'callcode', 'staticcall') or call_type is null)
                union all
                -- transaction fees debits
                select miner as address, sum(cast(receipt_gas_used as numeric) * cast(gas_price as numeric)) as value, block_timestamp
                from `bigquery-public-data.crypto_ethereum.transactions` as transactions
                join `bigquery-public-data.crypto_ethereum.blocks` as blocks on blocks.number = transactions.block_number
                group by blocks.miner, block_timestamp
                union all
                -- transaction fees credits
                select from_address as address, -(cast(receipt_gas_used as numeric) * cast(gas_price as numeric)) as value, block_timestamp
                from `bigquery-public-data.crypto_ethereum.transactions`
            ),
            double_entry_book_grouped_by_date as (
                select address, sum(value) as balance_increment, date(block_timestamp) as date
                from double_entry_book
                group by address, date
            ),
            daily_balances_with_gaps as (
                select address, date, sum(balance_increment) over (partition by address order by date) as balance,
                lead(date, 1, current_date()) over (partition by address order by date) as next_date
                from double_entry_book_grouped_by_date
            ),
            calendar AS (
                select date from unnest(generate_date_array('2018-08-01', current_date())) as date
            ),
            daily_balances as (
                select address, calendar.date, balance
                from daily_balances_with_gaps
                join calendar on daily_balances_with_gaps.date <= calendar.date and calendar.date < daily_balances_with_gaps.next_date
            )
            select date, count(*) as address_count
            from daily_balances
            where balance > 0
            group by date
            ORDER BY date"
        #hashRate <- query_exec(queryHash, projectid, use_legacy_sql = FALSE)
        #activeAccounts <- query_exec(queryActive, projectid, use_legacy_sql = FALSE)
        
        
        activeHash <- merge(hashRate, activeAccounts, by.x = "block_day", by.y = "date")
        activeHash$hashrate <-  (activeHash$hashrate * 1000000000)
        
        promSemanal <-  activeHash %>%
            filter(activeHash$block_day >=  as.Date(Sys.Date(), format="%Y/%m/%d")-7)%>% 
            summarise(hashSemanal = mean(activeHash$hashrate))
        
        promSemanalDir <-  activeHash %>%
            filter(activeHash$block_day >=  as.Date(Sys.Date(), format="%Y/%m/%d")-7) %>% 
            summarise(hashSemanal = mean(activeHash$address_count))
        
        lagSemanalHashrate <- activeHash %>%
            filter(activeHash$block_day >=  as.Date(Sys.Date(), format="%Y/%m/%d")-7) %>%
            mutate(pct_change = (hashrate/lag(hashrate) - 1) * 100) 
        lagSemanalHashrate <- lagSemanalHashrate$pct_change[length(lagSemanalHashrate)]
        
        activeHash <- subset(activeHash,activeHash$block_day > as.Date(input$rango_fechas[1]))
        activeHash <- subset(activeHash,activeHash$block_day < as.Date(input$rango_fechas[2]))
        
        RV$activeHash <- activeHash
        
        
        output$hashRate <- renderHighchart ({
            activeHash <- RV$activeHash 
            highchart() %>%
                hc_xAxis(categories = activeHash$block_day) %>%
                hc_title(text = "Active Accounts vs HashRate") %>% 
                hc_subtitle(text = "Hashes in T/H") %>% 
                hc_add_series(name = "HashRate", data = activeHash$hashrate) %>%
                hc_add_series(name = "Active Accounts", data = activeHash$address_count) %>% hc_add_theme(hc_theme_darkunica())
        })
        
        output$semanalHashRate <- renderValueBox({
            promSemanal %>%
                valueBox(subtitle = "Avr. Hashrate (TH/s) For The Last Week", icon = icon("far fa-hashtag"),color = "teal")
        })
        
        output$semanalDirreciones <- renderValueBox({
            round(promSemanalDir, digits = 2) %>%
                valueBox(subtitle = "Avr. Active Addresses For The Last Week", icon = icon("far fa-address-card"),color = "olive")
        })
        
        output$lagSemanalHashRateOut <- renderValueBox({
            paste0(round(lagSemanalHashrate, digits = 2), "%") %>%
                valueBox(subtitle = "Percentage Change For The Last Week", icon = icon("far fa-percentage"),color = "navy")
        })
        
        ################################################################################################
        #---------------- Incisco D Ganancia de Mineros Sobre el Tiempo---------------------------------
        ################################################################################################
        
        query_gini = "
                   with 
                  double_entry_book as (
                      -- debits
                      select to_address as address, value as value, block_timestamp
                      from `bigquery-public-data.crypto_ethereum.traces`
                      where to_address is not null
                      and status = 1
                      and (call_type not in ('delegatecall', 'callcode', 'staticcall') or call_type is null)
                      union all
                      -- credits
                      select from_address as address, -value as value, block_timestamp
                      from `bigquery-public-data.crypto_ethereum.traces`
                      where from_address is not null
                      and status = 1
                      and (call_type not in ('delegatecall', 'callcode', 'staticcall') or call_type is null)
                      union all
                      -- transaction fees debits
                      select miner as address, sum(cast(receipt_gas_used as numeric) * cast(gas_price as numeric)) as value, block_timestamp
                      from `bigquery-public-data.crypto_ethereum.transactions` as transactions
                      join `bigquery-public-data.crypto_ethereum.blocks` as blocks on blocks.number = transactions.block_number
                      group by blocks.miner, block_timestamp
                      union all
                      -- transaction fees credits
                      select from_address as address, -(cast(receipt_gas_used as numeric) * cast(gas_price as numeric)) as value, block_timestamp
                      from `bigquery-public-data.crypto_ethereum.transactions`
                  ),
                  double_entry_book_by_date as (
                      select 
                          date(block_timestamp) as date, 
                          address, 
                          sum(value / POWER(10,0)) as value
                      from double_entry_book
                      group by address, date
                  ),
                  daily_balances_with_gaps as (
                      select 
                          address, 
                          date,
                          sum(value) over (partition by address order by date) as balance,
                          lead(date, 1, current_date()) over (partition by address order by date) as next_date
                          from double_entry_book_by_date
                  ),
                  calendar as (
                      select date from unnest(generate_date_array('2015-07-30', current_date())) as date
                  ),
                  daily_balances as (
                      select address, calendar.date, balance
                      from daily_balances_with_gaps
                      join calendar on daily_balances_with_gaps.date <= calendar.date and calendar.date < daily_balances_with_gaps.next_date
                  ),
                   supply as (
                      select
                          date,
                          sum(balance) as daily_supply
                      from daily_balances
                      group by date
                  ),
                  ranked_daily_balances as (
                      select 
                          daily_balances.date,
                          balance,
                          row_number() over (partition by daily_balances.date order by balance desc) as rank
                      from daily_balances
                      join supply on daily_balances.date = supply.date
                      where safe_divide(balance, daily_supply) >= 0.0001
                      ORDER BY safe_divide(balance, daily_supply) DESC
                  ), 
                  gini_daily as (
                     select
                      date,
                      -- (1 ??? 2B) https://en.wikipedia.org/wiki/Gini_coefficient
                      1 - 2 * sum((balance * (rank - 1) + balance / 2)) / count(*) / sum(balance) as gini
                    from ranked_daily_balances
                    group by date
                  )
                  select date,
                      gini,
                      avg(gini) over (order by date asc rows 7 preceding) as gini_sma7,
                      avg(gini) over (order by date asc rows 30 preceding) as gini_sma30
                  from gini_daily
                  order by date asc"
        
        #miners_gini <- query_exec(query_gini, projectid, use_legacy_sql = FALSE)
        
        query_rich = "with double_entry_book as (
                      -- debits
                      select to_address as address, value as value
                      from `bigquery-public-data.crypto_ethereum.traces`
                      where to_address is not null
                      and status = 1
                      and (call_type not in ('delegatecall', 'callcode', 'staticcall') or call_type is null)
                      union all
                      -- credits
                      select from_address as address, -value as value
                      from `bigquery-public-data.crypto_ethereum.traces`
                      where from_address is not null
                      and status = 1
                      and (call_type not in ('delegatecall', 'callcode', 'staticcall') or call_type is null)
                      union all
                      -- transaction fees debits
                      select miner as address, sum(cast(receipt_gas_used as numeric) * cast(gas_price as numeric)) as value
                      from `bigquery-public-data.crypto_ethereum.transactions` as transactions
                      join `bigquery-public-data.crypto_ethereum.blocks` as blocks on blocks.number = transactions.block_number
                      group by blocks.miner
                      union all
                      -- transaction fees credits
                      select from_address as address, -(cast(receipt_gas_used as numeric) * cast(gas_price as numeric)) as value
                      from `bigquery-public-data.crypto_ethereum.transactions`
                  )
                  select address, 
                  sum(value) / 1000000000 as balance
                  from double_entry_book
                  group by address
                  order by balance desc
                  limit 20"
        miner_rich = read.csv("export_dataframe.csv")  
        miner_rich <- head(miner_rich,10)
        
        miners_gini <- subset(miners_gini,miners_gini$date >as.Date(input$rango_fechas[1]))
        miners_gini <- subset(miners_gini,miners_gini$date < as.Date(input$rango_fechas[2]))
        
        RV$miner_rich <- miner_rich
        RV$miners_gini <- miners_gini
        
        output$minersGini <- renderHighchart ({
            miners_gini <- RV$miners_gini
            highchart() %>%
                hc_xAxis(categories = miners_gini$date) %>%
                hc_title(text = "Miner Gini Coefficient") %>% 
                hc_subtitle(text = "Hashes in T/H") %>% 
                hc_add_series(name = "Gini Coefficient", data = miners_gini$gini) %>%
                hc_add_series(name = "Gini 30", data = miners_gini$gini_sma30) %>%
                hc_add_series(name = "Gini 7", data = miners_gini$gini_sma7) %>% hc_add_theme(hc_theme_darkunica())
        })
        
        output$minersRich <- renderHighchart ({
            miner_rich <- RV$miner_rich
            highchart() %>%
                hc_add_series_labels_values((miner_rich$balance), miner_rich$address, name = "Pie",colorByPoint = TRUE, type = "pie")  %>%
                hc_title(text = "Top 10 Miners") %>% 
                hc_subtitle(text = "Quantity") %>% hc_add_theme(hc_theme_darkunica())
        })
        
        
        
        ################################################################################################
        #---------------- Transacciones y Valores en Tiempo ---------------------------------
        ################################################################################################
        
        
        
        query_transact = "with daily_transactions as (
                          select date(block_timestamp) as date, count(*) as count
                          from `bigquery-public-data.crypto_bitcoin.transactions`
                          group by date
                      )
                      select date_trunc(date, WEEK) as week, cast(avg(count) as INT64) as count
                      from daily_transactions
                      group by week
                      order by week"
        
        query_values = "select date_trunc(date(block_timestamp), MONTH) as month, cast(max(value / 1000000000000000000) as INT64) as MaxVal, cast(avg(value / 1000000000000000000) as INT64) as AvgVal
                      from `bigquery-public-data.crypto_ethereum_classic.transactions` 
                      group by month
                      order by month"
        
        #transactions <- query_exec(query_transact, projectid, use_legacy_sql = FALSE)
        #transaction_values <- query_exec(query_values, projectid, use_legacy_sql = FALSE)
        
        transactions <- subset(transactions,transactions$week > as.Date(input$rango_fechas[1]))
        transactions <- subset(transactions,transactions$week < as.Date(input$rango_fechas[2]))
        
        transaction_values <- subset(transaction_values,transaction_values$month >as.Date(input$rango_fechas[1]))
        transaction_values <- subset(transaction_values,transaction_values$month < as.Date(input$rango_fechas[2]))
        
        RV$transaction <- transactions
        RV$transaction_values <- transaction_values
        
        #_---------------------------------------------------------------------------------------------------
        promSemanalTransac <-  transactions %>%
            filter(transactions$week >=  as.Date(Sys.Date(), format="%Y/%m/%d")-7) %>% 
            summarise(transSemanal = mean(transactions$count))
        
        lagSemanalTransac <- transactions %>%
            filter(transactions$week >=  as.Date(Sys.Date(), format="%Y/%m/%d")-21) %>%
            mutate(pct_change = (count/lag(count) - 1) * 100) 

        lagSemanalTransac <- lagSemanalTransac$pct_change[nrow(lagSemanalTransac)]
        
        output$lagSemanalTransactions <- renderValueBox({
            paste0(round(lagSemanalTransac, digits = 2), "%") %>%
                valueBox(subtitle = "Percentage Change For The Last Week", icon = icon("far fa-percentage"),color = "navy")
        })
        
        output$semanalTransacs <- renderValueBox({
            round(promSemanalTransac, digits = 0) %>%
                valueBox(subtitle = "Avr. Transactions For The Last Week", icon = icon("far fa-address-card"),color = "olive")
        })
        #_---------------------------------------------------------------------------------------------------
        
        
        
        #_---------------------------------------------------------------------------------------------------
        promSemanalTransacMax <-  transaction_values %>%
            filter(transaction_values$month >=  as.Date(Sys.Date(), format="%Y/%m/%d")-7) %>% 
            summarise(transSemanalMax = mean(transaction_values$MaxVal))
        
        lagSemanalTransacMax <- transaction_values %>%
            mutate(pct_change = (MaxVal/lag(MaxVal) - 1) * 100) 
        
        lagSemanalTransacMax <- lagSemanalTransacMax$pct_change[nrow(lagSemanalTransacMax)]
        
        output$lagSemanalTransactionsMax <- renderValueBox({ 
            paste0(round(lagSemanalTransacMax, digits = 2), "%") %>%
                valueBox(subtitle = "Percentage Change For The Last Week", icon = icon("far fa-percentage"),color = "navy")
        })
        
        output$semanalTransacsMax <- renderValueBox({
            round(promSemanalTransacMax, digits = 0) %>%
                valueBox(subtitle = "Avr. Transactions For The Last Week", icon = icon("far fa-address-card"),color = "olive")
        })
        #_---------------------------------------------------------------------------------------------------
        
        output$transactions <- renderHighchart ({
            transactions <- RV$transaction
            hchart(transactions, "bar", hcaes(x = week, y = count)) %>% 
                hc_title(text = "Transactions Throughout Time") %>% 
                hc_yAxis(reversed = TRUE) %>%
                hc_subtitle(text = "") %>% hc_add_theme(hc_theme_darkunica())
        })
        
        output$transactions_max <- renderHighchart ({
            transaction_values <- RV$transaction_values
            hchart(transaction_values, "bar", hcaes(x = month, y = MaxVal))  %>% 
                hc_title(text = "Max Amount of Ethers Sent per Month") %>% 
                hc_subtitle(text = "") %>% hc_add_theme(hc_theme_darkunica())
        })
        
        ################################################################################################
        #----------------------------------- Difficultad Media ---------------------------------
        ################################################################################################
        
        
        difficultad_media = read.csv("export-BlockDifficulty.csv")  
        colnames(difficultad_media) <- c("Date","unixTS","Value")
        difficultad_media$unixTS <- median(difficultad_media$Value)
        difficultad_media$valProm <- mean(difficultad_media$Value)
        difficultad_media$Date <- as.Date(difficultad_media$Date, format = "%m/%d/%Y")
        
        difficultad_media <- subset(difficultad_media,difficultad_media$Date > as.Date(input$rango_fechas[1]))
        difficultad_media <- subset(difficultad_media,difficultad_media$Date < as.Date(input$rango_fechas[2]))
        
        
        RV$difficultad_media <- difficultad_media
        
        
        
        output$difficultadMedia <- renderHighchart ({
            difficultad_media <- RV$difficultad_media
            highchart() %>%
                hc_xAxis(categories = difficultad_media$Date) %>%
                hc_title(text = "Difficulty") %>% 
                hc_subtitle(text = "Difficulty Over Time") %>% 
                hc_add_series(name = "Difficulty", data = difficultad_media$Value) %>%
                hc_add_series(name = "Mean Difficulty", data = difficultad_media$valProm) %>% 
                hc_add_series(name = "Median Difficulty", data = difficultad_media$unixTS) %>% hc_add_theme(hc_theme_darkunica())
        })
        
        
        ################################################################################################
        #----------------------------------- Dirreciones totales---------------------------------
        ################################################################################################
        
        
        
        query_direcciones = "with double_entry_book as (
                              -- debits
                              select to_address as address, value as value
                              from `bigquery-public-data.crypto_ethereum.traces`
                              where to_address is not null
                              and status = 1
                              and (call_type not in ('delegatecall', 'callcode', 'staticcall') or call_type is null)
                              union all
                              -- credits
                              select from_address as address, -value as value
                              from `bigquery-public-data.crypto_ethereum.traces`
                              where from_address is not null
                              and status = 1
                              and (call_type not in ('delegatecall', 'callcode', 'staticcall') or call_type is null)
                              union all
                              -- transaction fees debits
                              select miner as address, sum(cast(receipt_gas_used as numeric) * cast(gas_price as numeric)) as value
                              from `bigquery-public-data.crypto_ethereum.transactions` as transactions
                              join `bigquery-public-data.crypto_ethereum.blocks` as blocks on blocks.number = transactions.block_number
                              group by blocks.miner
                              union all
                              -- transaction fees credits
                              select from_address as address, -(cast(receipt_gas_used as numeric) * cast(gas_price as numeric)) as value
                              from `bigquery-public-data.crypto_ethereum.transactions`
                          )
                          select address, sum(value) as balance
                          from double_entry_book
                          group by address
                          order by balance desc"
        
        query_crecimientoCuentas = "with double_entry_book as (
                                  -- debits
                                  select to_address as address, value as value, block_timestamp
                                  from `bigquery-public-data.crypto_ethereum.traces`
                                  where to_address is not null
                                  and status = 1
                                  and (call_type not in ('delegatecall', 'callcode', 'staticcall') or call_type is null)
                                  union all
                                  -- credits
                                  select from_address as address, -value as value, block_timestamp
                                  from `bigquery-public-data.crypto_ethereum.traces`
                                  where from_address is not null
                                  and status = 1
                                  and (call_type not in ('delegatecall', 'callcode', 'staticcall') or call_type is null)
                                  union all
                                  -- transaction fees debits
                                  select miner as address, sum(cast(receipt_gas_used as numeric) * cast(gas_price as numeric)) as value, block_timestamp
                                  from `bigquery-public-data.crypto_ethereum.transactions` as transactions
                                  join `bigquery-public-data.crypto_ethereum.blocks` as blocks on blocks.number = transactions.block_number
                                  group by blocks.miner, block_timestamp
                                  union all
                                  -- transaction fees credits
                                  select from_address as address, -(cast(receipt_gas_used as numeric) * cast(gas_price as numeric)) as value, block_timestamp
                                  from `bigquery-public-data.crypto_ethereum.transactions`
                              ),
                              double_entry_book_grouped_by_date as (
                                  select address, sum(value) as balance_increment, date(block_timestamp) as date
                                  from double_entry_book
                                  group by address, date
                              ),
                              daily_balances_with_gaps as (
                                  select address, date, sum(balance_increment) over (partition by address order by date) as balance,
                                  lead(date, 1, current_date()) over (partition by address order by date) as next_date
                                  from double_entry_book_grouped_by_date
                              ),
                              calendar AS (
                                  select date from unnest(generate_date_array('2015-07-30', current_date())) as date
                              ),
                              daily_balances as (
                                  select address, calendar.date, balance
                                  from daily_balances_with_gaps
                                  join calendar on daily_balances_with_gaps.date <= calendar.date and calendar.date < daily_balances_with_gaps.next_date
                              )
                              select date, count(*) as address_count
                              from daily_balances
                              where balance > 0
                              group by date"
        
        #cuentas <- query_exec(query_crecimientoCuentas, projectid, use_legacy_sql = FALSE)
        
        cuentas <- cuentas[order(as.Date(cuentas$date, format="%Y/%m/%d")),]
        cuentas_incVacias <- cuentas_incVacias[order(as.Date(cuentas_incVacias$date, format="%Y/%m/%d")),]
        
        cuentas <- subset(cuentas,cuentas$date > as.Date(input$rango_fechas[1]))
        cuentas <- subset(cuentas,cuentas$date < as.Date(input$rango_fechas[2]))
        
        cuentas_incVacias <- subset(cuentas_incVacias,cuentas_incVacias$date > as.Date(input$rango_fechas[1]))
        cuentas_incVacias <- subset(cuentas_incVacias,cuentas_incVacias$date < as.Date(input$rango_fechas[2]))
        
        RV$cuentas <- cuentas 
        RV$cuentas_incVacias <- cuentas_incVacias
        
        #_---------------------------------------------------------------------------------------------------
        promSemanalC <-  mean(cuentas$address_count)
        
        lagSemanalC <- cuentas %>%
            filter(cuentas$date >=  as.Date(Sys.Date(), format="%Y/%m/%d")-7) %>%
            mutate(pct_change = (address_count/lag(address_count) - 1) * 100) 
        
        lagSemanalC <- lagSemanalC$pct_change[nrow(lagSemanalC)]
        
        output$lagSemanalC <- renderValueBox({
            paste0(round(lagSemanalC, digits = 2), "%") %>%
                valueBox(subtitle = "Percentage Change For The Last Week", icon = icon("far fa-percentage"),color = "navy")
        })
        
        output$semanalC <- renderValueBox({
            round(promSemanalC, digits = 0) %>%
                valueBox(subtitle = "Avr. Transactions For The Last Week", icon = icon("far fa-address-card"),color = "olive")
        })
        #_---------------------------------------------------------------------------------------------------
        
        
        
        output$direccionesTotales <- renderHighchart ({
            cuentas <- RV$cuentas 
            cuentas_incVacias <- RV$cuentas_incVacias
            highchart() %>%
                hc_xAxis(categories = cuentas$date) %>%
                hc_title(text = "Total Addresses") %>% 
                hc_subtitle(text = "Total Addreses vs Empty Addresses") %>% 
                hc_add_series(name = "Real Total Adresses", data = cuentas_incVacias$address_count, type="area") %>% 
                hc_add_series(name = "Total Addresses(Exc. Empty)", data = cuentas$address_count, type="area") %>%
                hc_add_theme(hc_theme_darkunica())
        })
        
        
        ################################################################################################
        #----------------------------------- Inflacion  ---------------------------------
        ################################################################################################
        inflacion = read.csv("eth.csv")  
        inflacion <- select(inflacion, c("date", "PriceUSD", "SplyCur"))
        
        inflacion <- inflacion %>%
            mutate(pct_change = (PriceUSD/lag(PriceUSD) - 1) * 100) 
        
        inflacion$date <- as.Date(inflacion$date, format = "%Y-%m-%d")
        
        inflacion <- subset(inflacion,inflacion$date > as.Date(input$rango_fechas[1]))
        inflacion <- subset(inflacion,inflacion$date < as.Date(input$rango_fechas[2]))

        RV$inflacion <- inflacion 
        
        
        output$inflacion <- renderHighchart ({
            inflacion <- RV$inflacion 
            highchart() %>%
                hc_xAxis(categories = inflacion$date) %>%
                hc_title(text = "Inflation") %>% 
                hc_subtitle(text = "Inflation Over Time") %>% 
                hc_add_series(name = "Inflation", data = inflacion$PriceUSD) %>% hc_add_theme(hc_theme_darkunica())
        })
        ################################################################################################
        #----------------------------------- Tether Supply  ---------------------------------
        ################################################################################################
        tether = read.csv("ercoct2018.csv")  
        tether = select(tether, c("DateTime", "Quantity"))
        tether$DateTime <- as.Date(tether$DateTime , format = "%m/%d/%Y")
        
        tether <- subset(tether,tether$DateTime > as.Date(input$rango_fechas[1]))
        tether <- subset(tether,tether$DateTime < as.Date(input$rango_fechas[2]))
        
        tether <- tether %>% group_by(month=floor_date(DateTime, "month")) %>%
            summarize(Quantity=sum(Quantity))
        RV$tether <- tether
        
        output$tether <- renderHighchart ({
            tether <- RV$tether
            hchart(tether, "bar", hcaes(x = month, y = Quantity)) %>% 
                hc_title(text = "Tether Supply") %>% 
                hc_subtitle(text = "") %>% hc_add_theme(hc_theme_darkunica())
        })
        

    })
    
}
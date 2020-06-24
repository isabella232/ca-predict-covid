# Developed by California COVID Modeling Team
#
# John Pugliese, PhD.
# California Department of Public Health
#
# Jason Vargo, PhD.
# California Department of Public Health
#
# Beta Version : Released 6/22/2020
#

library(shiny)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

#### Carousel Navigation ####
    # 
    # shinyjs::onclick("nowcast_img",  updateTabsetPanel(session, inputId="navbar", selected= "Nowcasts"))
    # shinyjs::onclick("forecast_img",  updateTabsetPanel(session, inputId="navbar", selected= "Forecasts"))
    # shinyjs::onclick("epimodel_img",  updateTabsetPanel(session, inputId="navbar", selected= "Scenarios"))
    # 

#### File Uploads ####
    
    output$c_template <- downloadHandler(

        filename = function() { paste("county_upload_template.csv", sep='') },
        content = function(file) {
            dlm <- data.frame( date = seq(as.Date(min(covid$Most.Recent.Date)), as.Date(Sys.Date() + 30), "day"),
                               county = input$county_upload,
                               c_reff = NA,
                               c_hosp = NA,
                               c_cumdeaths = NA)
            write.table(dlm, file, row.names = F, col.names = T, quote = F, na= "NA", sep = ",")
        })
    
    options(shiny.maxRequestSize = 9*1024^2)

    content_file <- reactive({
            inFile <- input$file1
            if (is.null(inFile)) {
               df <- data.frame(Result = c("No data has been uploaded"))
            } else {
              df <- tryCatch({ readr::read_csv(inFile$datapath) },
                               error = function(e) { sendSweetAlert(session, title = "Whoa!", text = "The uploaded file does not look like the template.", type = "error") },
                               finally = { data.frame(Result = c("No data has been uploaded"))  }                             )
            }
    return(df)
    })
    #names(df) <- c("date","county","c_reff","c_hosp","c_cumdeaths")   
    #
    # Here, we are going to store the entire data.frame in a reactiveValue
    rV <- reactiveValues( content_file = NULL, 
                          approve = NULL,
                          reff    = NULL,
                          hosp    = NULL,
                          death   = NULL,
                          county  = NULL)
    observe({ rV$content_file <- content_file()
              rV$approve <- FALSE
              rV$reff    <- FALSE
              rV$hosp    <- FALSE
              rV$death   <- FALSE
              rV$county  <- "California"
            })
    
    observe({
        df <- rV$content_file
        if (ncol(df) > 1 ) {
            data_types <- c()
            #Check for only one geography
            if ( length(unique(df$county)) == 1) {
                geo_set <- TRUE
                rV$county <- paste0(unique(df$county))
            } else { geo_set <- FALSE}
            #Check data types
            if (length(na.omit(df[,1])) > 0) {
                date_result <-tryCatch({ c(data_types,sapply(na.omit(df[1]), function(x) !all(is.na(as.Date(as.character(x),tryFormats = c("%Y-%m-%d", "%m/%d/%Y") )))) ) },
                                           error = function(e) { sendSweetAlert(session, title = "Whoa!", text = "Check your date column, I'm having trouble reading it.", type = "error") },
                                           finally = { FALSE })
                data_types <- c(data_types,date_result)
                if (TRUE %in% date_result) {
                    df$date <- as.Date(as.character(df$date),tryFormats = c("%Y-%m-%d", "%m/%d/%Y") )
                    rV$content_file <- df
                }
            } 
            if (length(na.omit(df[,2])) > 0) {
                data_types <- c(data_types, TRUE %in% c( sapply(na.omit(df[2]), function(x) !all(is.na(as.character(x))) ) ) )
            } 
            if (length(na.omit(df[,3])) > 0) {
                data_types <- c(data_types, TRUE %in% c( sapply(na.omit(df[3]), function(x) !all(is.na(as.numeric(x))) ) ) )
                rV$reff <- TRUE
            } 
            if (length(na.omit(df[,4])) > 0) {
                data_types <- c(data_types, TRUE %in% c( sapply(na.omit(df[4]), function(x) !all(is.na(as.numeric(x))) ) ) )
                rV$hosp <- TRUE
            } 
            if (length(na.omit(df[,5])) > 0) {
                data_types <- c(data_types, TRUE %in% c( sapply(na.omit(df[5]), function(x) !all(is.na(as.numeric(x))) ) ) )
                rV$death <- TRUE
            } 
            #Check if the number of data values is at least as long as the number of values
            date_cover <- c()
            date_num <- length(na.omit(df[,1]) )
            if (length(na.omit(df[,2])) > 0) {
                date_cover <- c(date_cover, date_num >= length(na.omit(df[,2])) ) 
            } 
            if (length(na.omit(df[,3])) > 0) {
                date_cover <- c(date_cover, date_num >= length(na.omit(df[,3])) ) 
            } 
            if (length(na.omit(df[,4])) > 0) {
                date_cover <- c(date_cover, date_num >= length(na.omit(df[,4])) ) 
            } 
            if (length(na.omit(df[,5])) > 0) {
                date_cover <- c(date_cover, date_num >= length(na.omit(df[,5])) ) 
            } 
        }
        
        if (ncol(content_file()) > 1 ) {
            
            if (geo_set == FALSE) {
                sendSweetAlert(session, title = "Oops!", text = "CalCAT can only accept one geography at this time.", type = "error")
            }
           
            if (TRUE %in% all(data_types) & TRUE %in% all(date_cover) ) { 
                rV$approve <- TRUE
                return(NULL)
            } else {
                if (FALSE %in% all(data_types)) {
                    sendSweetAlert(session, title = "Oops!", text = "One of your data columns is not the correct type.", type = "error")
                    rV$approve <- FALSE
                } else {
                    sendSweetAlert(session, title = "Oops!", text = "You have more outputs than dates.", type = "error")
                    rV$approve <- FALSE
                }
            }
        }
    })
    
    output$datacheck <- reactive({ rV$approve })
    outputOptions(output, 'datacheck', suspendWhenHidden = FALSE)

    output$contents <- renderDataTable({
        df <- rV$content_file
        DT::datatable(df,
                      rownames = FALSE,
                      colnames = switch(ncol(df) > 1, c("Date","Geo","Reff","Hosp.","Cum. Deaths"),"" ),
                      class = 'cell-border stripe',
                      options = list(pageLength = 15,
                                     searching = FALSE, 
                                     lengthChange = FALSE) 
                      )

    })
    
### Nowcasts of R Effective ####

    #Data Prep
    rt.ts <- reactive({
        
        icl_rt_f <- icl %>% select(date, constant_mobility_mean_time_varying_reproduction_number_R.t.) %>% rename(mean_rt = constant_mobility_mean_time_varying_reproduction_number_R.t.)
        icl_rt <- icl_model %>% select(date, mean_time_varying_reproduction_number_R.t.) %>% rename(mean_rt = mean_time_varying_reproduction_number_R.t.) 
        icl_rt <- rbind(icl_rt, icl_rt_f)
        
        fu <- filter(yu, !is.na(r_values_mean))
        
        rt.rt.xts <- xts(rt_live[,4], rt_live$date)
        
        can.rt.xts <- xts(can.state.observed[,8],can.state.observed$date)
        epifc.rt.xts <- xts(epi_forecast[which(epi_forecast$type == "nowcast"),4], 
                            epi_forecast[which(epi_forecast$type == "nowcast"),]$date)
        yu.xts <- xts(fu[,19],fu$date)
        ucla.rt.xts <- xts(ucla_state[,2],ucla_state$date)
        ucla.rt.xts <- ucla.rt.xts[paste0("/",Sys.Date()-1)]
        icl.rt.xts <- xts(icl_rt[,2], icl_rt$date) 
        if ( input$approve_upload == TRUE & rV$county == "California" & rV$reff == TRUE) { 
            u.df <- as.data.table(rV$content_file)
            u.df <- u.df %>% select(date, c_reff) %>% filter(date <= Sys.Date())
            u.rt.xts <- xts(u.df[,2], u.df$date) 
            colnames(u.rt.xts) <- c("Upload.Reff")
            df <- merge(rt.rt.xts, can.rt.xts,epifc.rt.xts, yu.xts, ucla.rt.xts, icl.rt.xts, u.rt.xts)
            if (input$include_ensemble == TRUE) {
                df$mean.rt <- rowMeans(df[,c(1:4,6,7)], na.rm = TRUE)
            } else {
                df$mean.rt <- rowMeans(df[,c(1:4,6)], na.rm = TRUE)
            }
            df[is.nan(as.numeric(df))] <- NA_character_
            df <- as.data.table(df) %>% as.data.frame()
            df[,2:9] <- sapply(df[,2:9], function(x) as.numeric(as.character(x)) )
        } else { 
            df <- merge(rt.rt.xts, can.rt.xts,epifc.rt.xts, yu.xts, ucla.rt.xts, icl.rt.xts) 
            df$mean.rt <- rowMeans(df[,c(1:4,6)], na.rm = TRUE)
            df[is.nan(as.numeric(df))] <- NA_character_
            df <- as.data.table(df) %>% as.data.frame()
            df[,2:8] <- sapply(df[,2:8], function(x) as.numeric(as.character(x)) )
        }
        return(df)
        
    })
    
    #Value Boxes
    output$mean.rt.box <- renderValueBox({
        cdt <- Sys.Date()-2
        current.rt <- round(rt.ts()[which(rt.ts()$index == cdt),"mean.rt"], digits = 2)
        valueBox(current.rt, subtitle = paste0(ifelse(current.rt >= 1.4,
                                                "Spread of COVID-19 is very likely increasing",
                                                ifelse(current.rt < 1.4 & current.rt >= 1.1,
                                                       "Spread of COVID-19 may be increasing",
                                                       ifelse(current.rt < 1.1 & current.rt >= 0.9,
                                                              "Spread of COVID-19 is likely stable",
                                                              "Spread of COVID-19 is likely decreasing"
                                                              )
                                                       )
                                                    )
                                              ), 
                                color = paste0(ifelse(current.rt >= 1.3,
                                                      "red",
                                                      ifelse(current.rt < 1.3 & current.rt >= 1.2,
                                                             "orange",
                                                             ifelse(current.rt < 1.2 & current.rt >= 1,
                                                                    "yellow",
                                                                    "green"
                                                             )
                                                      )
                                        )
                                )
                 ) #End valuBox
    })
    
    observeEvent(input$Rt_explain, {
        sendSweetAlert(
            session = session,
            title = "What does a Reff of this size mean?",
            text = HTML("<p>If the R effective is greater than 1, COVID-19 will spread <b>exponentially</b>. If R effective is less than 1, COVID-19 
                         will spread more slowly and cases will decline. The higher the value of R effective, the faster an epidemic will progress.
                         The following graph illustrates the change in growth as R effective increases.</p>
                         <img src='reff_cuml_infection.jpg' alt='Infections increase faster with larger values of R effective' width='450px'/>
                         <p><a href='https://www.cebm.net/covid-19/when-will-it-be-over-an-introduction-to-viral-reproduction-numbers-r0-and-re/' target='_blank'>Source: CEBM</a></p>"
                         ),
            html = TRUE,
            type = NULL
        )
    })

    output$hilo_rt.box <- renderUI({
        
        df <- rt.ts()
        df <- df %>% filter(index < Sys.Date()-1) %>% slice(n())
        
        rt.min <- as.numeric( apply(df[,c(2:5,7)], 1, function(i) min(i, na.rm = TRUE)) )
        rt.max <- as.numeric( apply(df[,c(2:5,7)], 1, function(i) max(i, na.rm = TRUE)) )
        
        name.min <- switch(as.character(colnames(df)[match(apply(df[,c(2:5,7)], 1, function(i) min(i, na.rm = TRUE)),df)]),
                         "rt.rt.xts" = "rt.live",
                         "can.rt.xts" = "COVIDActNow",
                         "epifc.rt.xts" = "EpiForecasts",
                         "yu.xts" = "covid19-projections.com",
                         "ucla.rt.xts" = "UCLA",
                         "icl.rt.xts" = "ICL")
        
        name.max<- switch(as.character(colnames(df)[match(apply(df[,c(2:5,7)], 1, function(i) max(i, na.rm = TRUE)),df)]),
                           "rt.rt.xts" = "rt.live",
                           "can.rt.xts" = "COVIDActNow",
                           "epifc.rt.xts" = "EpiForecasts",
                           "yu.xts" = "covid19-projections.com",
                           "ucla.rt.xts" = "UCLA",
                           "icl.rt.xts" = "ICL")
        
        tagList(valueBox( paste0( round(rt.min,digits = 2)," - ", round(rt.max,digits = 2)) , paste0(name.min," - ",name.max), color = "navy", width = 12) )
        
    })
    
    #Graph
    output$rt.plot <- renderPlotly({
       
        df <- rt.ts() %>% filter(index < Sys.Date()-1 & index > Sys.Date() -80)
        
        p <-  plot_ly(df,
                      hoverinfo = 'text') %>%
            add_trace(x = df[[1]], 
                      y = df[[2]], 
                      name = "rt.live",
                      type = 'scatter',
                      mode = "lines",
                      line = list(color="orange", dash = 'dot', opacity = 0.5),
                      text = paste0(df[[1]],
                                    "<br>",
                                    "rt.live estimated Reff: ", round(df[[2]], digits=2)
                                    )
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[[3]], 
                      name = "COVIDActNow",
                      type = 'scatter',
                      mode = "lines", 
                      line = list(color="blue", dash = 'dot', opacity = 0.5),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "COVIDActNow estimated Reff: ", round(df[[3]], digits=2) )
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[[4]], 
                      name = "EpiForecasts",
                      type = 'scatter',
                      mode = "lines", 
                      line = list(color="purple", dash = 'dot', opacity = 0.5),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "EpiForecasts estimated Reff: ", round(df[[4]], digits=2) )
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[[5]], 
                      name = "covid19-projections.com",
                      type = 'scatter',
                      mode = "lines", 
                      line = list(color="red", dash = 'dot', opacity = 0.5),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "covid19-projections.com estimated Reff: ", round(df[[5]], digits=2) )
            ) %>%
            # add_trace(x = df[[1]], 
            #           y = df[[6]], 
            #           name = "UCLA",
            #           type = 'scatter',
            #           mode = "lines", 
            #           line = list(color="grey", dash = 'dot', opacity = 0.5),
            #           hoverinfo = 'text',
            #           text = paste0("UCLA estimated Reff: ", round(df[[6]], digits=2) )
            #           #marker = list(color = "blue",  symbol= "circle")
            # ) %>%
            add_trace(x = df[[1]],
                      y = df[[7]],
                      name = "ICL",
                      type = 'scatter',
                      mode = "lines",
                      line = list(color="grey", dash = 'dot', opacity = 0.5),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "Imperial College London estimated Reff: ", round(df[[7]], digits=2) )
            )
        if ( input$approve_upload == TRUE & rV$county == "California" & rV$reff == TRUE) { 
            p <- p %>% add_trace(x = df[[1]],
                                 y = df[["Upload.Reff"]],
                                 name = "User Uploaded Reff",
                                 type = 'scatter',
                                 mode = "lines",
                                 line = list(color="brown", dash = 'dot', opacity = 0.5, width = 5),
                                 hoverinfo = 'text',
                                 text = paste0(df[[1]],
                                               "<br>",
                                               "Uploaded estimated Reff: ", round(df[["Upload.Reff"]], digits=2) )
                )
            
        }
        p <- p %>% add_trace(x = df[[1]], 
                      y = df[["mean.rt"]], 
                      name = "Mean Reff",
                      type = 'scatter',
                      mode = "lines", 
                      hoverinfo = 'text',
                      line = list(color = '#2b8cbe', width = 5),
                      text = paste0(df[[1]],
                                    "<br>",
                                    "Mean estimated Reff: ", round(df[["mean.rt"]], digits=2), 
                                    "<br>",
                                    ifelse(round(df[["mean.rt"]], digits=2) >= 1.4,
                                           "Spread of COVID-19 is very likely increasing",
                                           ifelse(round(df[["mean.rt"]], digits=2) < 1.4 & round(df[["mean.rt"]], digits=2) >= 1.1,
                                                  "Spread of COVID-19 may be increasing",
                                                  ifelse(round(df[["mean.rt"]], digits=2) < 1.1 & round(df[["mean.rt"]], digits=2) >= 0.9,
                                                         "Spread of COVID-19 is likely stable",
                                                         "Spread of COVID-19 is likely decreasing"
                                                  )
                                           )
                                        )
                                    )
                      
                      
            ) %>%
            layout(
                title = NULL,
                xaxis = list(title = NULL, showgrid = FALSE, zeroline = FALSE ),
                yaxis = list(title = "R-Eff", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
                margin = list(l = 100),
                showlegend = TRUE, 
                shapes = list(
                    type = "line", 
                    x0 = 0, 
                    x1 = 1, 
                    xref = "paper",
                    y0 = 1, 
                    y1 = 1, 
                    yref = "y",
                    line = list(color = "gray50", dash= "dash", opacity = 0.3))
            ) 
        
        return(p)
        
    })
    
   
    #Downloadable file of Statewide Reff Values
    output$dlRt <- downloadHandler(
        
        filename = function() { paste("R_eff_Nowcasts_",Sys.Date(),'.csv', sep='') },
        
        content = function(file) {
           
            # Title
            t <- c(paste("R-Effective Model and Ensemble Time Series", sep = ""),rep("",ncol(rt.ts())-1))
            #Subtitle
            tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(rt.ts())-1))
            #Column labels
            if ( input$approve_upload == TRUE & rV$county == "California" & rV$reff == TRUE) { 
                l <- c("Date","rt.live","COVIDActNow","EpiForecasts","covid19-projections.com","ICL","Upload","Mean Reff")
            } else { 
                l <- c("Date","rt.live","COVIDActNow","EpiForecasts","covid19-projections.com","ICL","Mean Reff")
            }
            
            df <- rt.ts() %>% select(-6,) %>% filter(index < Sys.Date() & index > Sys.Date() -80)
            #df <- rt.ts()[,c(1:5,7,8)] %>% filter(index < Sys.Date() & index > Sys.Date() -80)
            
            df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2))
            # df[is.na(df)] <- 0
            df[] <- lapply(df, as.character)
            
            #Source
            s <- c("Please see the Technical Notes tab of the application for data sources.",rep("",ncol(rt.ts())-1))
            p <- c("Prepared by: California Department of Public Health - COVID Modeling Team",rep("",ncol(rt.ts())-1))
            
            dlm <- rbind(t, tt, l, df, s, p)
            write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
        })   
    
    
#### County Rt Nowcasts ####
    
    #Data Prep
    county.rt <- reactive({
        # progress <- Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        # on.exit(progress$close())
        # progress$set(message = "Gathering R Effective Nowcasts", value = 0)
        
        c <- names(canfipslist[match(input$select.county.rt,canfipslist)])
        cnty <- input$select.county.rt
        
        # progress$inc(3/4)
        # out <- lapply(cnty[1], function(x) get_can_cnty(x))
        out <- filter(can.county.observed, fips ==  cnty)
        
        # cnty.rt <- do.call("rbind",out)
        cnty.rt <- out %>% select(date,RtIndicator) %>% as.data.frame() #,RtIndicatorCI90
        cnty.rt$date <- as.Date(cnty.rt$date)
        # progress$inc(1/4)
        
        df <- xts(cnty.rt[,-1],cnty.rt$date)
        if (c %in% unique(yu.cnty$subregion)   ) { cnty.yu <- yu.cnty %>% filter(subregion == c) %>% select(date, r_values_mean) 
                                                   yu.xts <- xts(cnty.yu[,-1],cnty.yu$date)
                                                   names(yu.xts) <- c("yu.xts")
                                                   df <- merge(df,yu.xts)
                                                  }
        if (c %in% unique(ucla_cnty_rt$county) ) { cnty.ucla <- ucla_cnty_rt %>% filter(county == c) %>% select(date, Rt)       
                                                   ucla.xts <- xts(cnty.ucla[,-1],cnty.ucla$date)
                                                   names(ucla.xts) <- c("ucla.xts")
                                                   df <- merge(df,ucla.xts)
                                                  }
        
        if ( input$approve_upload == TRUE & rV$county == c & rV$reff == TRUE) { 
            u.df <- as.data.table(rV$content_file)
            u.df <- u.df %>% select(date, c_reff) %>% filter(date <= Sys.Date())
            u.rt.xts <- xts(u.df[,2], u.df$date) 
            colnames(u.rt.xts) <- c("Upload.Reff")
            df <- merge(df, u.rt.xts)
            
            if (input$include_ensemble == TRUE) {
                if (ncol(df) > 1) {df$mean.proj <- rowMeans(df[,1:ncol(df)], na.rm = TRUE)} 
            } else {
                if ( (ncol(df)-1) > 1) {df$mean.proj <- rowMeans(df[,1:ncol(df)-1], na.rm = TRUE)} 
            }
        } else { 
           if (ncol(df) > 1) {df$mean.proj <- rowMeans(df[,1:ncol(df)], na.rm = TRUE)} 
        }
        df <- as.data.table(df) %>% as.data.frame() %>% filter(index < Sys.Date()-1)
        return(df)
        
    })
    
    #Value Boxes
    output$cnty.mean.rt.box <- renderValueBox({
        if (ncol(county.rt()) > 2) { cdt <- Sys.Date()-4 } else { cdt <- Sys.Date()-5}
        if (ncol(county.rt()) > 2) { current.rt <- round( county.rt()[which( county.rt()$index == cdt),"mean.proj"], digits = 2)
        } else { current.rt <- round( county.rt()[which( county.rt()$index == cdt),2], digits = 2) }
        
        valueBox(current.rt, subtitle = paste0(ifelse(current.rt >= 1.4,
                                                      "Spread of COVID-19 is very likely increasing",
                                                      ifelse(current.rt < 1.4 & current.rt >= 1.1,
                                                             "Spread of COVID-19 may be increasing",
                                                             ifelse(current.rt < 1.1 & current.rt >= 0.9,
                                                                    "Spread of COVID-19 is likely stable",
                                                                    "Spread of COVID-19 is likely decreasing"
                                                             )
                                                      )
                                                )
                                            ), 
                                            color = paste0(ifelse(current.rt >= 1.3,
                                                                  "red",
                                                                  ifelse(current.rt < 1.3 & current.rt >= 1.2,
                                                                         "orange",
                                                                         ifelse(current.rt < 1.2 & current.rt >= 1,
                                                                                "yellow",
                                                                                "green"
                                                                         )
                                                                  )
                                                                )
                                                            )
                                            ) #End valueBox
    })
    
    #Graph
    output$county.rt.plot <- renderPlotly({
       
        df <- county.rt()
        
        c <- names(canfipslist[match(input$select.county.rt,canfipslist)])
        
        #df$ymin <- df$RtIndicator - (df$RtIndicatorCI90)
        #df$ymax <- df$RtIndicator + (df$RtIndicatorCI90)
        
        p <-  plot_ly(df,
                      x = df[[1]], 
                      y = df[[2]], 
                      name = "COVIDActNow",
                      type = 'scatter',
                      mode = "lines", 
                      line = list(color="blue", dash = 'dot', opacity = 0.5),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "COVIDActNow estimated Reff: ", round(df[[2]], digits=2) )
            )
        if (c %in% unique(yu.cnty$subregion)   ) {p <- p %>% add_trace(x = df[[1]], 
                                                                  y = df[["yu.xts"]], 
                                                                  name = "covid19-projections.com",
                                                                  type = 'scatter',
                                                                  mode = "lines", 
                                                                  line = list(color="red", dash = 'dot', opacity = 0.5),
                                                                  hoverinfo = 'text',
                                                                  text = paste0(df[[1]],
                                                                                "<br>",
                                                                                "covid19-projections.com estimated Reff: ", round(df[["yu.xts"]], digits=2) )
                                                        ) 
        }
        if (c %in% unique(ucla_cnty_rt$county) ) {p <- p %>% add_trace(x = df[[1]], 
                                                              y = df[["ucla.xts"]], 
                                                              name = "UCLA",
                                                              type = 'scatter',
                                                              mode = "lines", 
                                                              line = list(color="grey", dash = 'dot', opacity = 0.5),
                                                              hoverinfo = 'text',
                                                              text = paste0(df[[1]],
                                                                            "<br>",
                                                                            "UCLA estimated Reff: ", round(df[["ucla.xts"]], digits=2) )
                                                    )
        }
        if ( input$approve_upload == TRUE & rV$county == c & rV$reff == TRUE) { 
            p <- p %>% add_trace(x = df[[1]],
                                 y = df[["Upload.Reff"]],
                                 name = "User Uploaded Reff",
                                 type = 'scatter',
                                 mode = "lines",
                                 line = list(color="brown", dash = 'dot', opacity = 0.5, width = 5),
                                 hoverinfo = 'text',
                                 text = paste0(df[[1]],
                                               "<br>",
                                               "Uploaded estimated Reff: ", round(df[["Upload.Reff"]], digits=2) )
            )
            
        }
        if (ncol(df) > 2) {p <- p %>% add_trace(x = df[[1]], 
                                      y = df[["mean.proj"]], 
                                      name = "Mean Reff",
                                      type = 'scatter',
                                      mode = "lines", 
                                      hoverinfo = 'text',
                                      text = paste0(df[[1]],
                                                    "<br>",
                                                    "Mean estimated Reff: ", round(df[["mean.proj"]], digits=2),
                                                    "<br>",
                                                    ifelse(round(df[["mean.proj"]], digits=2) >= 1.4,
                                                           "Spread of COVID-19 is very likely increasing",
                                                           ifelse(round(df[["mean.proj"]], digits=2) < 1.4 & round(df[["mean.proj"]], digits=2) >= 1.1,
                                                                  "Spread of COVID-19 may be increasing",
                                                                  ifelse(round(df[["mean.proj"]], digits=2) < 1.1 & round(df[["mean.proj"]], digits=2) >= 0.9,
                                                                         "Spread of COVID-19 is likely stable",
                                                                         "Spread of COVID-19 is likely decreasing"
                                                                  )
                                                           )
                                                        )
                                                    ),
                                      inherit = FALSE,
                                      line = list(color = '#2b8cbe', width = 5),
                                      linetype = "solid"
                            )
        }
            # add_ribbons(x = df[[1]],
            #             ymax =  df[[5]],
            #             ymin =  df[[4]],
            #             opacity = 0.5,
            #             inherit = TRUE,
            #             line = list(color = '#2b8cbe' ),
            #             fillcolor = '#2b8cbe',
            #             name = '90% CI'
            # ) %>%
        p <- p %>% layout(  #legend = list(orientation = 'h'),
                            title = as.character(counties[match(input$select.county.rt, counties$fips),1]),
                            xaxis = list(title = NULL, showgrid = FALSE, zeroline = FALSE ),
                            yaxis = list(title = "R-Eff", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
                            margin = list(l = 100),
                            showlegend = TRUE, 
                            shapes = list(
                                type = "line", 
                                x0 = 0, 
                                x1 = 1, 
                                xref = "paper",
                                y0 = 1, 
                                y1 = 1, 
                                yref = "y",
                                line = list(color = "gray50", dash= "dash", opacity = 0.3)
                            )
                        ) 
        
        return(p)
        
    })
    
    #Download file of individual COUNTY Reff Values
    output$dlRt.indv.cnty <- downloadHandler(
        
        filename = function() { paste("Rt_Nowcasts_",names(canfipslist[match(input$select.county.rt,canfipslist)]),"_",Sys.Date(),'.csv', sep='') },
        
        content = function(file) {
            
            c <- names(canfipslist[match(input$select.county.rt,canfipslist)])
            # Title
            t <- c(paste("R-Effective County Model Time Series for ",c, sep = ""),rep("",ncol(county.rt())))
            #Subtitle
            tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(county.rt())))
           
            df <- county.rt() %>% as.data.frame()
            if ( ncol(df) > 2 ) { df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2)) } else { df[,2] <- round(df[,2],2)  } 
            df[is.na(df)] <- 0
            df[] <- lapply(df, as.character)
            
            #Column labels
            
            l <- c("Date","COVIDActNow")
            
            if ( c %in% unique(yu.cnty$subregion) ) {  l <- c(l, c("covid19-projections.com")) }
            if ( c %in% unique(ucla_cnty_rt$county) ) {  l <- c(l, c("UCLA")) }
            if ( input$approve_upload == TRUE & rV$county == c & rV$reff == TRUE) { l <- c(l, c("Upload")) }
            if ( length(l) > 2 ) { l <- c(l, c("Mean Reff") ) }
           
            #Source
            s <- c("Please see the Technical Notes tab of the application for data sources.",rep("",ncol(county.rt())))
            p <- c("Prepared by: California Department of Public Health - COVID Modeling Team",rep("",ncol(county.rt())))
            
            dlm <- rbind(t, tt, l, df, s, p)
            write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
        })   

#### Rt Dot Plot ####
    
    #Data Prep
    cnty.7.day.rt <- data.table(can.county.observed) %>%
        .[, max_date := max(date, na.rm = T), by = .(county)] %>%
        .[date > Sys.Date()-7, .(Rt.m = mean(RtIndicator, na.rm = T),
                                 ll = mean(RtIndicator - RtIndicatorCI90, na.rm=T),
                                 ul = mean(RtIndicator + RtIndicatorCI90, na.rm=T)), by = .(county)] %>% na.omit()

    # cnty.7.day.rt <- reactive({
    #     
    #                 cnty.can <- can.county.observed %>% filter(date <= Sys.Date()-1,
    #                                                       date > Sys.Date()-8) %>% 
    #                     select(county, date, RtIndicator) %>% 
    #                     mutate(date = as.Date(date)) %>% 
    #                     rename(Rt = RtIndicator) %>%
    #                     as.data.frame()
    #                 cnty.yu <- yu.cnty %>% filter(date <= Sys.Date()-1,
    #                                               date >  Sys.Date()-8) %>% 
    #                                         select(subregion, date, r_values_mean) %>% 
    #                                         rename(county = subregion,
    #                                                Rt = r_values_mean )
    #                 cnty.ucla <- ucla_cnty_rt %>% filter(date <= Sys.Date()-1,
    #                                                      date >  Sys.Date()-8) %>% 
    #                                               select(county, date, Rt) 
    #                 
    #                 df <- rbind(cnty.can,cnty.yu,cnty.ucla) %>% 
    #                         arrange(county,date) %>%
    #                         group_by(county) %>%
    #                         summarise(Rt.m = mean(Rt, na.rm = T),
    #                                   Rt.sd = sd(Rt, na.rm = T) ) %>% 
    #                         na.omit() %>%
    #                         mutate(ll = Rt.m  - 1.95*Rt.sd,
    #                                ul = Rt.m  + 1.95*Rt.sd)
    #                 return(df)
    # 
    # })
    # 
    #Graph
    output$rt.dot.plot <- renderPlotly({
       
        df <- cnty.7.day.rt
        
        p <-  plot_ly(df,
                      x = ~ reorder(df$county, desc(df$Rt.m)), 
                      y = ~ df$Rt.m, 
                      name = "R effective",
                      type = 'scatter',
                      mode = "markers", 
                      marker = list(color = '#2b8cbe'),
                      hoverinfo = 'text',
                      text = ~paste0(df$county, " County<br>","7-day Average Reff: ", round(df$Rt.m, digits=2), 
                                     "<br>",
                                     ifelse(df$Rt.m >= 1.4,
                                            "Spread of COVID-19 is very likely increasing",
                                            ifelse(df$Rt.m < 1.4 & df$Rt.m >= 1.1,
                                                   "Spread of COVID-19 may be increasing",
                                                   ifelse(df$Rt.m < 1.1 & df$Rt.m >= 0.9,
                                                          "Spread of COVID-19 is likely stable",
                                                          "Spread of COVID-19 is likely decreasing"
                                                           )
                                                    )
                                             )
                                     )
                     ) %>%
            add_segments(x =~ reorder(df$county, df$Rt.m), 
                         xend = ~ reorder(df$county, df$Rt.m), 
                         y = df$ll, 
                         yend = df$ul, 
                         type = "scatter",
                         mode = "lines",
                         opacity = .5,
                         line = list(color='#2b8cbe', width = 6),
                         showlegend = FALSE
                         ) %>%
            layout(
                xaxis = list(title = "", tickangle = -30, showgrid = FALSE, zeroline = FALSE ),
                yaxis = list(title = "R-Eff", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
                margin = list(l = 100),
                showlegend = FALSE, 
                shapes = list(
                    type = "line", 
                    x0 = 0, 
                    x1 = 1, 
                    xref = "paper",
                    y0 = 1, 
                    y1 = 1, 
                    yref = "y",
                    line = list(color = "gray50", dash= "dash", opacity = 0.3)
                )
            ) 
        return(p)
        
        
    })
    
    #Download file of ALL COUNTY 7-day average Reff Values
    output$dlRt.cnty <- downloadHandler(
        
        filename = function() { paste("Rt_Nowcasts_7DayAvg_Counties",Sys.Date(),'.csv', sep='') },
        
        content = function(file) {
           
            # Title
            t <- c(paste("R-Effective 7 Day Averages for Counties", sep = ""),"","","","")
            #Subtitle
            tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","")
            
            df <- cnty.7.day.rt %>% as.data.frame()
            if ( ncol(df) > 2 ) { df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2)) } else { df[,2] <- round(df[,2],2)  } 
            df[is.na(df)] <- 0
            df[] <- lapply(df, as.character)
            
            #Column labels
            
            l <- c("County","COVIDActNow - 7 Day Avg", "LL", "UL")
            
            #Source
            s <- c("Please see the Technical Notes tab of the application.","","","","")
            p <- c("Prepared by: California Department of Public Health - COVID Modeling Team","","","","")
            u <- c("Source: COVIDActNow - https://blog.covidactnow.org/modeling-metrics-critical-to-reopen-safely/","","","","")
            
            dlm <- rbind(t, tt, l, df, s, p, u)
            write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
        })   
    
    
#### Reff County Map ####
    
   
    cnty.map.rt <- reactive({
        can.rt <- can.county.observed %>% select(date, county, RtIndicator) %>% mutate(date = as.Date(date)) %>% as.data.frame()
        yu.rt <- yu.cnty %>% select(date, subregion, r_values_mean) %>% rename(county = subregion)
        ucla.rt <- ucla_cnty_rt %>% select(date, county, Rt)     
        
        df <- full_join(can.rt, yu.rt, by = c("county" = "county", "date" = "date"))
        df <- full_join(df, ucla.rt, by = c("county" = "county", "date" = "date"))
        
        if ( input$approve_upload == TRUE & input$include_ensemble == TRUE & 
             rV$county %in% names(fipslist) & rV$reff == TRUE) { 
            u.df <- as.data.table(rV$content_file)
            u.df <- u.df %>% select(date, county, c_reff) %>% filter(date <= Sys.Date())
            df <- full_join(df, u.df, by = c("county" = "county", "date" = "date"))
        }
        
        df$mean.proj <- rowMeans(df[,3:ncol(df)], na.rm = TRUE)
        df <- df %>% filter(date < Sys.Date() & date > Sys.Date()-7)
        df <- df %>% group_by(county) %>% summarise(mean.proj = mean(mean.proj, na.rm = T))
        df <- df %>% select(county, mean.proj) %>% mutate(mean.proj = round(mean.proj,digits=2))
        return(df)
    })
    
    output$reff_county_map <- renderggiraph({

        df <- cnty.map.rt()

        map.data <- left_join(counties_sp, df)
        
        map.data <- map.data %>% mutate(color = paste0(ifelse(mean.proj >= 1.3,
                                                                       "red",
                                                                       ifelse(mean.proj < 1.3 & mean.proj >= 1.2,
                                                                              "orange",
                                                                              ifelse(mean.proj < 1.2 & mean.proj >= 1,
                                                                                     "yellow",
                                                                                     "green"
                                                                                    )
                                                                             )
                                                                        )
                                                                )
                        )
        
        map.data <- map.data %>% mutate(color = ifelse(is.na(mean.proj),"gray90",color))
        

        g <- ggplot(data = map.data) +
            geom_sf() +
            geom_sf_interactive(aes(
                fill = color,
                tooltip = paste0(county," County <br> R-eff: ", round(mean.proj,1)),
                data_id = county
            ),
            size = 0.5) +
            scale_fill_manual(values = c("red"="#dd4b39","orange" = "#ff851b","yellow"="#f39c12", "green"="#00a65a", "gray90" ="gray90")) + 
            # scale_fill_gradientn(
            #     low = "white" ,
            #     mid = '#2b8cbe',
            #     high = "navy",
            #     midpoint = 1.0,
            #     na.value = "gray90"
            # ) +
            theme(
                axis.line = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.position = "none",
                panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid.major = element_line(colour = "transparent"),
                panel.grid.minor = element_blank(),
                plot.background = element_blank()
            ) 

        ggiraph(code = print(g),
                selection_type = "single",
                hover_css = "cursor:pointer; stroke:black;")


    })
    
    
    
    #Download file of ALL COUNTY 7-day average Reff Values
    output$dlRt.cnty.map <- downloadHandler(
        
        filename = function() { paste("Rt_Nowcasts_County_Map",Sys.Date(),'.csv', sep='') },
        
        content = function(file) {
            df <- cnty.map.rt()
            # Title
            t <- c(paste("Latest R-Effective for Counties", sep = ""),rep("",ncol(df)))
            #Subtitle
            tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(df)))
            
            if ( ncol(df) > 2 ) { df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2)) } else { df[,2] <- round(df[,2],2)  } 
            df[is.na(df)] <- 0
            df[] <- lapply(df, as.character)
            
            #Column labels
            
            l <- c("County","R-Effective - Latest Ensemble")
            
            #Source
            s <- c("Please see the Technical Notes tab of the application.",rep("",ncol(df)))
            p <- c("Prepared by: California Department of Public Health - COVID Modeling Team",rep("",ncol(df)))
            u <- c("Source: COVIDActNow - https://blog.covidactnow.org/modeling-metrics-critical-to-reopen-safely/",rep("",ncol(df)))
            v <- c("Source: UCLA ML Lab - https://covid19.uclaml.org/",rep("",ncol(df)))
            w <- c("Source: COVID-19 Projections - https://covid19-projections.com/",rep("",ncol(df)))
            
            dlm <- rbind(t, tt, l, df, s, p, u, v, w)
            write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
        })   
    

#### Hospitalization Projections ####

   #Data Prep
   hosp.proj.ts <- reactive({
        
        min_hosp <- min(covid$Most.Recent.Date)
        hosp <- covid %>% select(Most.Recent.Date,COVID.19.Positive.Patients) %>% filter(covid$County.Name == "California") %>% as.data.frame()
        can.hosp.proj <- can.state.observed %>% select(date, hospitalBedsRequired) %>% filter(min_hosp <= date & date <= JHU_inf_end_dt)
        IHME.hosp.proj <- IHME %>% select(date, allbed_mean) %>% filter(min_hosp <= date & date <= JHU_inf_end_dt)
        mobs.hosp.proj <- mobs %>% select(date,hospitalBedsRequired) %>% filter(min_hosp <= date & date <= JHU_inf_end_dt)
        mit.hosp.proj <- mit %>% select(11,7) %>% filter(min_hosp <= date & date <= JHU_inf_end_dt)
        jhu.hosp.proj <- JHU_aws %>% filter(intervention == "Inference" & county == "California") %>% select("date","hosp_occup_mean") %>% filter(min_hosp <= date & date <= JHU_inf_end_dt)
        
        covid.xts <- xts(hosp[,-1],hosp$Most.Recent.Date)
        can.proj.xts <- xts(can.hosp.proj[,-1],can.hosp.proj$date)
        ihme.proj.xts <- xts(IHME.hosp.proj[,-1],IHME.hosp.proj$date)
        mobs.proj.xts <- xts(mobs.hosp.proj[,-1],mobs.hosp.proj$date)
        mit.proj.xts <- xts(mit.hosp.proj[,-1],mit.hosp.proj$date)
        jhu.proj.xts <- xts(jhu.hosp.proj[,-1],jhu.hosp.proj$date)
        
        if ( input$approve_upload == TRUE & rV$county == "California" & rV$hosp == TRUE) { 
            u.df <- as.data.table(rV$content_file)
            u.df <- u.df %>% select(date, c_hosp) %>% filter(min_hosp <= date & date <= JHU_inf_end_dt)
            u.rt.xts <- xts(u.df[,2], u.df$date) 
            colnames(u.rt.xts) <- c("Upload.Hosp")
            df <- merge(covid.xts,can.proj.xts,ihme.proj.xts,mobs.proj.xts,mit.proj.xts,jhu.proj.xts,u.rt.xts)
            
            if (input$include_ensemble == TRUE) {
                df$mean.proj <- rowMeans(df[,2:7], na.rm = TRUE)
            } else {
                df$mean.proj <- rowMeans(df[,2:6], na.rm = TRUE)
            }
           
        } else { 
            df <- merge(covid.xts,can.proj.xts,ihme.proj.xts,mobs.proj.xts,mit.proj.xts,jhu.proj.xts)
            df$mean.proj <- rowMeans(df[,2:6], na.rm = TRUE)
        }
        
        df$mean.proj <- ifelse(!is.na(df$covid.xts), NA, df$mean.proj)
        df <- as.data.table(df) %>% as.data.frame()
       
        df$period <- ifelse(!is.na(df$covid.xts), "solid", "dash")
        df$type <- ifelse(!is.na(df$covid.xts), "Est.", "Proj.")
        return(df)
        
    })
    
    #Value Boxes
    output$actual.hosp.box <- renderValueBox({
        
        cdt <- max(covid$Most.Recent.Date)
        current.hosp <- as.character(covid[which(covid$Most.Recent.Date == cdt & covid$County.Name == "California"),5])
        valueBox( format(as.numeric(current.hosp), big.mark = ","), paste0("Actuals: ",cdt), color = "black")
        
    })
    
    output$mean.proj.hosp.box <- renderUI({ 
        df <- hosp.proj.ts()
        cdt <- max( df[which(df$index <= Sys.Date() + 30),]$index  )
        mean.proj <-  hosp.proj.ts() %>% slice(n()) %>% select("mean.proj")
        valueBox( format(round(mean.proj, digits = 0), big.mark = ","), paste0("Mean Forecast through ", cdt), color = "blue", width = 12)
        
    })
    
    #Graphs
    output$hosp.proj.plot <- renderPlotly({
        
        df <- hosp.proj.ts()  
       
        cdt <- max(df[which(!is.na(df$covid.xts)),1])
            
        p <-  plot_ly(df,
                      hoverinfo = 'text') %>%
            add_trace(x = df[[1]], 
                      y = df[[2]], 
                      name = "Actuals",
                      type = 'scatter',
                      mode = "lines+markers", 
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "Actual Hospitalization: ", format(round(df[[2]],0), big.mark = ",") ),
                      line = list(color = "black"),
                      marker = list(color = "black",  symbol= "circle")
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[[3]], 
                      name = ~I(paste0("COVIDActNow - ",df$type)),
                      type = 'scatter',
                      mode = "lines", 
                      inherit = TRUE,
                      line = list(color="orange"),
                      linetype = ~I(period),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "COVIDActNow Estimate: ", format(round(df[[3]],0), big.mark = ",") )
                      
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[[4]], 
                      name = ~I(paste0("IHME - ",df$type)),
                      type = 'scatter',
                      mode = "lines", 
                      inherit = TRUE,
                      line = list(color="navy"),
                      linetype = ~I(period),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "IHME Estimate: ", format(round(df[[4]],0), big.mark = ",") )
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[[5]], 
                      name = ~I(paste0("MOBS - ",df$type)),
                      type = 'scatter',
                      mode = "lines", 
                      inherit = TRUE,
                      line = list(color="red"),
                      linetype = ~I(period),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "MOBS Estimate: ", format(round(df[[5]],0), big.mark = ",") )
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[[6]], 
                      name = ~I(paste0("MIT - ",df$type)),
                      type = 'scatter',
                      mode = "lines", 
                      inherit = TRUE,
                      line = list(color="green"),
                      linetype = ~I(period),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "MIT Estimate: ", format(round(df[[6]],0), big.mark = ",") )
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[[7]], 
                      name = ~I(paste0("JHU - ",df$type)),
                      type = 'scatter',
                      mode = "lines", 
                      inherit = TRUE,
                      line = list(color="firebrick"),
                      linetype = ~I(period),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "JHU Estimate: ", format(round(df[[7]],0), big.mark = ",") )
            ) 
            if ( input$approve_upload == TRUE & rV$county == "California" & rV$hosp == TRUE) { 
              
                p <- p %>% add_trace(x = df[[1]], 
                                     y = df[["Upload.Hosp"]], 
                                     name = ~I(paste0("User Uploaded - ",df$type)),
                                     type = 'scatter',
                                     mode = "lines", 
                                     #inherit = TRUE,
                                     linetype = ~I(period),
                                     hoverinfo = 'text',
                                     text = paste0(df[[1]],
                                                   "<br>",
                                                   "Uploaded Estimate: ", format(round(df[["Upload.Hosp"]],0), big.mark = ",") ),
                                     line = list(color="blue")
                )  
                
            }
            p <- p %>% add_trace( x = df[[1]], 
                                  y = df[["mean.proj"]], 
                                  name = "Mean Proj.",
                                  type = 'scatter',
                                  mode = "lines", 
                                  hoverinfo = 'text',
                                  text = paste0(df[[1]],
                                                "<br>",
                                                "Mean Projection: ", format(round(df[["mean.proj"]],0), big.mark = ",") ),
                                  line = list(color = '#2b8cbe', width = 5)
            ) %>%
            layout(
                title = NULL,
                xaxis = list(title = NULL, showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
                yaxis = list(title = "Hospitalizations", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
                margin = list(l = 100),
                showlegend = TRUE,
                shapes = list(type = "line", 
                              y0 = 0, 
                              y1 = 1, 
                              yref = "paper",
                              x0 = cdt, 
                              x1 = cdt, 
                              line = list(color = "black", dash = 'dash')
                             )
            ) 
        return(p)
        
    })
    
    #Download file of Statewide Hospitalization Forecasts
    output$dlhosp <- downloadHandler(
        
        filename = function() { paste("Hospital_Forecasts_",Sys.Date(),'.csv', sep='') },
        
        content = function(file) {
            df <- hosp.proj.ts() %>% select(-type, -period) %>% as.data.frame()
            # Title
            t <- c(paste("Statewide Hospitalization Forecasts", sep = ""),rep("",ncol(df)) )
            #Subtitle
            tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(df)))
            #Column labels
            if ( input$approve_upload == TRUE & rV$county == "California" & rV$hosp == TRUE) { 
                l <- c("Date","Actuals", "COVIDActNow","IHME","MOBS","MIT","JHU","Upload", "Mean")
            } else { 
                l <- c("Date","Actuals", "COVIDActNow","IHME","MOBS","MIT","JHU","Mean")
            }
            
            df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2))
            df[is.na(df)] <- 0
            df[] <- lapply(df, as.character)
            
            #Source
            s <- c("Please see the Technical Notes tab of the application for data sources.",rep("",ncol(df)))
            p <- c("Prepared by: California Department of Public Health - COVID Modeling Team",rep("",ncol(df)))
            
            dlm <- rbind(t, tt, l, df, s, p)
            write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
        })   
    
    
    
#### County Hospitalization Projections ####
    
    fc.cnty.beds <- reactive({
        c <- names(fipslist[match(input$select.county.hosp,fipslist)])
        
        if (c %in% cnty.beds[,1] == TRUE) {
            beds <- c(cnty.beds[which(cnty.beds$COUNTY == c),9])
        } else {
            beds <- c(NA)
        }
    })
    
    
    hosp.proj.cnty.ts <- reactive({
        
        # progress <- Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        # on.exit(progress$close())
        # progress$set(message = "Gathering Hospitalization Forecasts", value = 0)
        
        cnty <- input$select.county.hosp
        #c <- names(canfipslist[match(input$select.county.hosp,canfipslist)])
        c <- names(fipslist[match(input$select.county.hosp,fipslist)])
        
        min_hosp <- min(covid$Most.Recent.Date)
        hosp <- covid %>% select(Most.Recent.Date,COVID.19.Positive.Patients) %>% filter(covid$County.Name == c) %>% as.data.frame()
        
        max_ucla <- ucla_cnty %>% filter(output == "hosp",type %in% c("history"), county == c) %>% select(date) %>% summarize(max(date)) %>% c()
        ucla.hist <- ucla_cnty %>% filter(output == "hosp",
                                          type %in% c("history"),
                                          county == c,
                                          min_hosp <= date) %>% 
                                          select(date,value) %>% as.data.frame()
        ucla.pred <- ucla_cnty %>% filter(output == "hosp",
                                          type %in% c("pred"),
                                          county == c,
                                          max_ucla < date, #filter out overlapping dates from forecasts
                                          date <= JHU_inf_end_dt) %>% 
                                          select(date,value) %>% as.data.frame()
        ucla.hosp <- rbind(ucla.hist,ucla.pred)
        jhu.hosp.proj <- JHU_aws %>% filter(intervention == "Inference" & county == c) %>% select("date","hosp_occup_mean") %>% filter(min_hosp <= date & date <= JHU_inf_end_dt)
        
        covid.xts <- xts(hosp[,-1],hosp$Most.Recent.Date)
        ucla.proj.xts <- xts(ucla.hosp[,-1],ucla.hosp$date)   
        jhu.proj.xts <- xts(jhu.hosp.proj[,-1],jhu.hosp.proj$date)
        df <- merge(covid.xts,ucla.proj.xts,jhu.proj.xts )
        
        if (input$select.county.hosp %in% canfipslist) {
            # progress$inc(3/4)
            out <- filter(can.county.observed, fips ==  cnty)
            cnty.hosp <- out %>% select(date,hospitalBedsRequired) %>% as.data.frame()
            
            # progress$inc(1/4)
            can.hosp.proj <- cnty.hosp %>% select(date, hospitalBedsRequired) %>% filter(min_hosp <= date & date <= JHU_inf_end_dt)
            can.proj.xts <- xts(can.hosp.proj[,-1],can.hosp.proj$date)
            df <- merge(df,can.proj.xts)
        } 
        
        ##User Uploaded Data
        if ( input$approve_upload == TRUE & rV$county == c & rV$hosp == TRUE) { 
            u.df <- as.data.table(rV$content_file)
            u.df <- u.df %>% select(date, c_hosp) %>% filter(min_hosp <= date & date <= JHU_inf_end_dt)
            u.rt.xts <- xts(u.df[,2], u.df$date) 
            colnames(u.rt.xts) <- c("Upload.Hosp")
            df <- merge(df,u.rt.xts)
            
            if (input$include_ensemble == TRUE) {
                df$mean.proj <- rowMeans(df[,2:ncol(df)], na.rm = TRUE)
            } else {
                df$mean.proj <- rowMeans(df[,2:ncol(df)-1], na.rm = TRUE)
            }
            
        } else { 
            df$mean.proj <- rowMeans(df[,2:ncol(df)], na.rm = TRUE)
        }
        ##
        
        df$mean.proj <- ifelse(!is.na(df$covid.xts), NA, df$mean.proj)
        df <- as.data.table(df) %>% as.data.frame()
        
        df$period <- ifelse(!is.na(df$covid.xts), "solid", "dash")
        df$type <- ifelse(!is.na(df$covid.xts), "Est.", "Proj.")
        
        return(df)
        
    })
    
    #Value Boxes
    output$actual.cnty.hosp.box <- renderValueBox({
        c <- names(fipslist[match(input$select.county.hosp,fipslist)])
        cdt <- max(covid$Most.Recent.Date)
        current.hosp <- as.character(covid[which(covid$Most.Recent.Date == cdt & covid$County.Name == c),5])
        valueBox( paste0(format(as.numeric(current.hosp), big.mark = ","),"/",
                         format(as.numeric(fc.cnty.beds()), big.mark = ",") ), 
                  paste0("Actuals / Total Beds : ",cdt), 
                  color = "black")
    })
    
    output$mean.cnty.proj.hosp.box <- renderValueBox({ 
        df <- hosp.proj.cnty.ts()
        cdt <- max( df[which(df$index <= Sys.Date() + 30),]$index  )
        mean.proj <-  hosp.proj.cnty.ts() %>% slice(n()) %>% select("mean.proj")
        valueBox( format(round(mean.proj, digits = 0), big.mark = ","), 
                  paste0("Mean Forecast through ", cdt), color = "blue")
        
    })
    
    #Graph
    output$county.hosp.plot <- renderPlotly({
        
        c <- names(fipslist[match(input$select.county.hosp,fipslist)])
        
        df <- hosp.proj.cnty.ts()  
        
        cdt <- max(df[which(!is.na(df$covid.xts)),1])
        
        today <- list(type = "line", 
                      y0 = 0, 
                      y1 = 1, 
                      yref = "paper",
                      x0 = cdt, 
                      x1 = cdt, 
                      line = list(color = "black", dash = 'dash') )
        
        p <-  plot_ly(df,
                      hoverinfo = 'text') %>%
            add_trace(x = df[[1]], 
                      y = df[[2]], 
                      name = "Actuals",
                      type = 'scatter',
                      mode = "lines+markers", 
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "Actual Hospitalization: ", format(df[[2]], big.mark = ",") ),
                      line = list(color = "black"),
                      marker = list(color = "black",  symbol= "circle")
            )
        if (input$select.county.hosp %in% canfipslist) {
            p <- p %>% add_trace(x = df[[1]], 
                          y = df[["can.proj.xts"]], 
                          name = ~I(paste0("COVIDActNow - ",df$type)),
                          type = 'scatter',
                          mode = "lines", 
                          inherit = TRUE,
                          line = list(color="orange"),
                          linetype = ~I(period),
                          hoverinfo = 'text',
                          text = paste0(df[[1]],
                                        "<br>",
                                        "COVIDActNow Estimate: ", format(df[["can.proj.xts"]], big.mark = ",") )
                )
        }
        if ( input$approve_upload == TRUE & rV$county == c & rV$hosp == TRUE) { 
            
            p <- p %>% add_trace(x = df[[1]], 
                                 y = df[["Upload.Hosp"]], 
                                 name = ~I(paste0("User Uploaded - ",df$type)),
                                 type = 'scatter',
                                 mode = "lines", 
                                 #inherit = TRUE,
                                 linetype = ~I(period),
                                 hoverinfo = 'text',
                                 text = paste0(df[[1]],
                                               "<br>",
                                               "Uploaded Estimate: ", format(round(df[["Upload.Hosp"]],0), big.mark = ",") ),
                                 line = list(color="green")
            )  
            
        }
        p <-  p %>% add_trace(x = df[[1]], 
                      y = df[["ucla.proj.xts"]], 
                      name = ~I(paste0("UCLA - ",df$type)),
                      type = 'scatter',
                      mode = "lines", 
                      inherit = TRUE,
                      line = list(color="blue"),
                      linetype = ~I(period),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "UCLA Estimate: ", format(round(df[["ucla.proj.xts"]],0), big.mark = ",") )
                      
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[["jhu.proj.xts"]], 
                      name = ~I(paste0("JHU - ",df$type)),
                      type = 'scatter',
                      mode = "lines", 
                      inherit = TRUE,
                      line = list(color="firebrick"),
                      linetype = ~I(period),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "JHU Estimate: ", format(round(df[["jhu.proj.xts"]],0), big.mark = ",") )
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[["mean.proj"]], 
                      name = "Mean Proj.",
                      type = 'scatter',
                      mode = "lines", 
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "Mean Projection: ", format(round(df[["mean.proj"]],0), big.mark = ",") ),
                      line = list(color = '#2b8cbe', width = 5)
            ) %>%
            layout(
                title = as.character(counties[match(input$select.county.hosp, counties$fips),1]),
                xaxis = list(title = NULL, showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
                yaxis = list(title = "Hospitalziations", showline = TRUE, showgrid = FALSE, zeroline = FALSE),
                margin = list(l = 100),
                showlegend = TRUE, 
                shapes = list(today)
            ) 
        return(p)
        
    })
    
    #Download file of COUNTY Hospitalization Forecasts
    output$dlhosp.cnty <- downloadHandler(
        
        filename = function() { paste("Hospital_Forecasts_for_",names(fipslist[match(input$select.county.hosp,fipslist)]),Sys.Date(),'.csv', sep='') },
        
        content = function(file) {
            
            c <- names(canfipslist[match(input$select.county.hosp,canfipslist)])
            # Title
            t <- c(paste("Hospitalization Forecasts for ",c, sep = ""),rep("",ncol(hosp.proj.cnty.ts())-1))
            #Subtitle
            tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(hosp.proj.cnty.ts())-1))
            
            #Data Prep
            df <- hosp.proj.cnty.ts() %>% select(-c(period, type)) %>% rename(date = index) %>% as.data.frame()
            df[is.na(df)] <- 0
            df[] <- lapply(df, as.character)
            
            #Column labels
            l <- c("Date","Hospitalizations")
            
            if ( "ucla.proj.xts" %in% names(hosp.proj.cnty.ts()) ) {  l <- c(l, c("UCLA")) }
            if ( "jhu.proj.xts" %in% names(hosp.proj.cnty.ts()) ) {  l <- c(l, c("JHU")) }
            if ( "can.proj.xts" %in% names(hosp.proj.cnty.ts()) ) {  l <- c(l, c("COVIDActNow")) }
            if ( input$approve_upload == TRUE & rV$county == c & rV$hosp == TRUE) { l <- c(l, c("Uploaded")) }
            if ( length(l) > 2 ) { l <- c(l, c("Mean") ) }
            
            #Source
            s <- c("Please see the Technical Notes tab of the application for data sources.",rep("",ncol(hosp.proj.cnty.ts())-1))
            p <- c("Prepared by: California Department of Public Health - COVID Modeling Team",rep("",ncol(hosp.proj.cnty.ts())-1))
            
            dlm <- rbind(t, tt, l, df, s, p)
            write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
        })   
    

#### Statewide Cumulative Deaths Projections ####
    
    #Data Prep
    cdeath.ca <- reactive({
        
        reich_test <- reich_lab %>% unique() %>% as.data.frame()
        
        cdeaths_test <- covid %>% select(Most.Recent.Date,Total.Count.Deaths) %>% 
            filter(covid$County.Name == "California") %>% 
            mutate(model_team = 'Actuals') %>%
            rename(model_team = model_team,
                   target_end_date = Most.Recent.Date,
                   pointNA = Total.Count.Deaths
            ) %>%
            select(model_team, pointNA, target_end_date) %>%
            as.data.frame()
        
        reich_test <- rbind(reich_test,cdeaths_test) 
       
        reich_test <-  reich_test %>% distinct(model_team, target_end_date, .keep_all = TRUE)  %>%  spread(model_team, pointNA)
        
    })
    
    #Value Boxes
    output$actual.cdeath.box <- renderValueBox({
        
        cdt <- max(covid$Most.Recent.Date)
        current.deaths <- as.character(covid[which(covid$Most.Recent.Date == cdt & covid$County.Name == "California"),4])
        valueBox( format(as.numeric(current.deaths), big.mark = ","), paste0("Actuals: ",cdt), color = "black")
        
    })
    
    output$mean.proj.cdeaths.box <- renderUI({ 
        
        ensemble <- cdeath.ca() %>% select(target_end_date,COVIDhub.ensemble) %>% filter(!is.na(COVIDhub.ensemble))
        cdt.ens <- max(ensemble$target_end_date)
        mean.proj <- ensemble %>% slice(n()) %>% select(2)
        valueBox( format(round(mean.proj, digits = 0), big.mark = ","), paste0("COVIDhub Ensemble Forecast through ", cdt.ens), color = "blue", width = 12)
        
    })
    
    #Graphs
    output$cdeath.proj.plot <- renderPlotly({
        
        df <- cdeath.ca()
        
        #Need to filter out Reich Lab models that represent scenarios rather than forecasts of current conditions
        models <- names(df)
        models <- setdiff(models, c("target_end_date", "CU.nointerv", "CU.60contact","CU.70contact",
                                    "CU.80contact","CU.80contact1x10p","CU.80contact1x5p","CU.80contactw10p",
                                    "CU.80contactw5p","COVIDhub.ensemble", "Actuals" ) )
        models <-  models %>% c("Actuals","COVIDhub.ensemble")
        
        p <- plot_ly(data=df, type = "scatter", mode = "lines")
        
        for(trace in models){
            if (trace == "Actuals") {
                
                p <- p %>% plotly::add_trace(x = ~target_end_date, 
                                             y = as.formula(paste0("~`", trace, "`")), 
                                             name = trace, 
                                             type = 'scatter',
                                             mode = "lines+markers", 
                                             line = list(color ="black"),
                                             marker = list(color = "black",  symbol= "circle"),
                                             hoverinfo = 'text',
                                             text = paste0(df[[1]],
                                                           "<br>",
                                                           "Actual Total Deaths: ", format(df$Actuals, big.mark = ","))
                                             )
            } else {
                
                if (trace == "COVIDhub.ensemble") {
                    p <- p %>% add_trace(x = ~target_end_date,
                                         y = as.formula(paste0("~`", trace, "`")), 
                                         inherit = FALSE,
                                         name = trace, 
                                         line = list(shape = "spline", color = '#2b8cbe'),
                                         marker = list(color = '#2b8cbe',  symbol= "circle"),
                                         hoverinfo = 'text',
                                         text = paste0(df[[1]],
                                                       "<br>",
                                                       "COVIDhub Ensemble Forecast: ", format(df$COVIDhub.ensemble, big.mark = ","))
                                         ) 
                } else {
                    p <- p %>% plotly::add_trace(x = ~target_end_date, 
                                                 y = as.formula(paste0("~`", trace, "`")), 
                                                 name = trace, 
                                                 type = 'scatter',
                                                 mode = "lines", 
                                                 line = list(color ="lightgray"),
                                                 hoverinfo = 'text+y',
                                                 text = paste0(df[[1]],
                                                               "<br>",
                                                               trace," Forecast")
                                                 )
                }
            }
        }
        p %>% 
            layout(title = NULL,
                   xaxis = list(title = " ", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
                   yaxis = list(title = "Total Deaths", showline = TRUE, showgrid = FALSE, zeroline = FALSE, hoverformat = ',.2r' ),
                   margin = list(l = 100),
                   legend = list(traceorder = "reversed"),
                   showlegend = TRUE)
        
    })
    
    #Download file of Statewide Cumulative Deaths Forecasts
    output$dlDeath <- downloadHandler(
        
        filename = function() { paste("Cumulative_Deaths_Forecasts_",Sys.Date(),'.csv', sep='') },
        
        content = function(file) {
            
            # Title
            t <- c(paste("Statewide Cumulative Deaths Forecasts", sep = ""),rep("",ncol(cdeath.ca())-1) )
            #Subtitle
            tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(cdeath.ca())-1))
            #Column labels
            l <- names(cdeath.ca())
            
            df <- cdeath.ca() %>% as.data.frame()
            #df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2))
            df[is.na(df)] <- 0
            df[] <- lapply(df, as.character)
            
            #Source
            s <- c("Please see the Technical Notes tab of the application for data sources.",rep("",ncol(cdeath.ca())-1))
            p <- c("Prepared by: California Department of Public Health - COVID Modeling Team",rep("",ncol(cdeath.ca())-1))
            
            dlm <- rbind(t, tt, l, df, s, p)
            write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
        })   
    
    
#### County Cumulative Death Projections ####
    
    #Data prep
    county.deaths <- reactive({
        
            # progress <- Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            # on.exit(progress$close())
            # progress$set(message = "Gathering Death Forecast Data", value = 0)
            
            fips <- input$select.county.death
            cnty <- names(fipslist[match(input$select.county.death,fipslist)])
            
            #Used to filter model estimates that occur prior to actuals
            min_death <- min(covid$Most.Recent.Date)
            
            #UCLA Time series overalp in terms of date; therefore we need to find the max date for the "history" of estimates of actuals
            max_ucla <- ucla_cnty %>% filter(output == "deaths",type %in% c("history"), county == cnty) %>% select(date) %>% summarize(max(date)) %>% c()
            
            death <- covid %>% select(Most.Recent.Date,Total.Count.Deaths) %>% filter(covid$County.Name == cnty) %>% as.data.frame()
            
            ucla.hist <- ucla_cnty %>% filter(output == "deaths",
                                              type %in% c("history"),
                                              county == cnty,
                                              min_death <= date) %>% 
                                        select(date,value) %>% as.data.frame()
            ucla.pred <- ucla_cnty %>% filter(output == "deaths",
                                              type %in% c("pred"),
                                              county == cnty,
                                              max_ucla < date, #filter out overlapping dates from forecasts
                                              date <= Sys.Date() + 30) %>% 
                                        select(date,value) %>% as.data.frame()
            ucla.death <- rbind(ucla.hist,ucla.pred)
            jhu.death <- JHU_aws %>% filter(intervention == "Inference" & county == cnty) %>% select("date","cum_deaths_mean") %>% filter(min_death <= date & date <= Sys.Date() + 30)
            
            covid.xts <- xts(death[,-1],death$Most.Recent.Date)
            ucla.proj.xts <- xts(ucla.death[,-1],ucla.death$date)
            jhu.proj.xts <- xts(jhu.death[,-1],jhu.death$date)
            df <- merge(covid.xts,ucla.proj.xts,jhu.proj.xts )
            
            if (input$select.county.death %in% canfipslist) {
                # progress$inc(3/4)
                out <- filter(can.county.observed, county == cnty)
                can.death <- out %>% select(date,cumulativeDeaths) %>% 
                    filter(min_death <= date & date <= Sys.Date() + 30) %>% 
                    rename(CovidActNow = cumulativeDeaths) %>% as.data.frame()
                
                # progress$inc(1/4)
                can.proj.xts <- xts(can.death[,-1],can.death$date)
                
                df <- merge(df,can.proj.xts)
            }
            
            ##User Uploaded Data
            if ( input$approve_upload == TRUE & rV$county == cnty & rV$death == TRUE) { 
                u.df <- as.data.table(rV$content_file)
                u.df <- u.df %>% select(date, c_cumdeaths) %>% filter(min_hosp <= date & date <= JHU_inf_end_dt)
                u.rt.xts <- xts(u.df[,2], u.df$date) 
                colnames(u.rt.xts) <- c("Upload.Death")
                df <- merge(df,u.rt.xts)
                
                if (input$include_ensemble == TRUE) {
                    df$mean.proj <- rowMeans(df[,2:ncol(df)], na.rm = TRUE)
                } else {
                    df$mean.proj <- rowMeans(df[,2:ncol(df)-1], na.rm = TRUE)
                }
                
            } else { 
                df$mean.proj <- rowMeans(df[,2:ncol(df)], na.rm = TRUE)
            }
            ##
            df$mean.proj <- ifelse(!is.na(df$covid.xts), NA, df$mean.proj)
            df <- as.data.table(df) %>% as.data.frame()
            
            df$period <- ifelse(!is.na(df$covid.xts), "solid", "dash")
            df$type <- ifelse(!is.na(df$covid.xts), "Est.", "Proj.")
            
            return(df)
        
    })
    
    #Value Boxes
    output$actual.cnty.death.box <- renderValueBox({
        c <- names(fipslist[match(input$select.county.death,fipslist)])
        cdt <- max(covid$Most.Recent.Date)
        current.deaths <- as.character(covid[which(covid$Most.Recent.Date == cdt & covid$County.Name == c),4])
        valueBox( paste0(format(as.numeric(current.deaths), big.mark = ",") ), 
                  paste0("Actual Deaths : ",cdt), 
                  color = "black")
    })
    
    output$mean.cnty.proj.death.box <- renderValueBox({ 
        df <- county.deaths()
        cdt <- max( df$index )
        mean.proj <-  df %>% slice(n()) %>% select("mean.proj")
        valueBox( format(round(mean.proj, digits = 0), big.mark = ","), 
                  paste0("30-Day Forecast through ", cdt), color = "blue")
        
    })
    
    #Graph
    output$county.death.plot <- renderPlotly({
        
        c <- names(fipslist[match(input$select.county.death,fipslist)])
        
        df <- county.deaths()  
        
        cdt <- max(df[which(!is.na(df$covid.xts)),1])
        
        today <- list(type = "line", 
                      y0 = 0, 
                      y1 = 1, 
                      yref = "paper",
                      x0 = cdt, 
                      x1 = cdt, 
                      line = list(color = "black", dash = 'dash') )
        
        p <-  plot_ly(df,
                      hoverinfo = 'text') %>%
            add_trace(x = df[[1]], 
                      y = df[[2]], 
                      name = "Actuals",
                      type = 'scatter',
                      mode = "lines+markers", 
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "Actual Deaths: ", format(df[[2]], big.mark = ",") ),
                      line = list(color = "black"),
                      marker = list(color = "black",  symbol= "circle")
            )
        if (input$select.county.death %in% canfipslist) {
            p <- p %>% add_trace(x = df[[1]], 
                                 y = df[["can.proj.xts"]], 
                                 name = ~I(paste0("COVIDActNow - ",df$type)),
                                 type = 'scatter',
                                 mode = "lines", 
                                 inherit = TRUE,
                                 line = list(color="orange"),
                                 linetype = ~I(period),
                                 hoverinfo = 'text',
                                 text = paste0(df[[1]],
                                               "<br>",
                                               "COVIDActNow Estimate: ", format(df[["can.proj.xts"]], big.mark = ",") )
            )
        }
        if ( input$approve_upload == TRUE & rV$county == c & rV$death == TRUE) { 
            
            p <- p %>% add_trace(x = df[[1]], 
                                 y = df[["Upload.Death"]], 
                                 name = ~I(paste0("User Uploaded - ",df$type)),
                                 type = 'scatter',
                                 mode = "lines", 
                                 #inherit = TRUE,
                                 linetype = ~I(period),
                                 hoverinfo = 'text',
                                 text = paste0(df[[1]],
                                               "<br>",
                                               "Uploaded Estimate: ", format(round(df[["Upload.Death"]],0), big.mark = ",") ),
                                 line = list(color="green")
            )  
            
        }
        p <-  p %>% add_trace(x = df[[1]], 
                      y = df[["ucla.proj.xts"]], 
                      name = ~I(paste0("UCLA - ",df$type)),
                      type = 'scatter',
                      mode = "lines", 
                      inherit = TRUE,
                      line = list(color="blue"),
                      linetype = ~I(period),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "UCLA Estimate: ", format(df[["ucla.proj.xts"]], big.mark = ",") )
                      
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[["jhu.proj.xts"]], 
                      name = ~I(paste0("JHU - ",df$type)),
                      type = 'scatter',
                      mode = "lines", 
                      inherit = TRUE,
                      line = list(color="firebrick"),
                      linetype = ~I(period),
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "JHU Estimate: ", format(df[["jhu.proj.xts"]], big.mark = ",") )
                      
            ) %>%
            add_trace(x = df[[1]], 
                      y = df[["mean.proj"]], 
                      name = "Mean Proj.",
                      type = 'scatter',
                      mode = "lines", 
                      hoverinfo = 'text',
                      text = paste0(df[[1]],
                                    "<br>",
                                    "Mean Projection: ", format(round(df[["mean.proj"]],0), big.mark = ",") ),
                      line = list(color = '#2b8cbe', width = 5)
            ) %>%
            layout(
                title = as.character(counties[match(input$select.county.death, counties$fips),1]),
                xaxis = list(title = NULL, showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
                yaxis = list(title = "Total Deaths", showline = TRUE, showgrid = FALSE, zeroline = FALSE),
                margin = list(l = 100),
                showlegend = TRUE, 
                shapes = list(today)
            ) 
        return(p)
        
    })
    
    #Download file of COUNTY Total Death Forecasts
    output$dlDeath.cnty <- downloadHandler(
        
        filename = function() { paste("Cumulative_Death_Forecasts_for_",names(canfipslist[match(input$select.county.death,canfipslist)]),Sys.Date(),'.csv', sep='') },
        
        content = function(file) {
            
            c <- names(canfipslist[match(input$select.county.death,canfipslist)])
            # Title
            t <- c(paste("Cumulative Death Forecasts for ",c, sep = ""),rep("",ncol(county.deaths())-1))
            #Subtitle
            tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(county.deaths())-1))
           
            df <- county.deaths() %>% select(-c(period, type)) %>% rename(date = index) %>% as.data.frame()
            df[is.na(df)] <- 0
            df[] <- lapply(df, as.character)
            
            #Column labels
            l <- c("Date","Total Deaths")
            
            
            if ( "ucla.proj.xts" %in% names(county.deaths()) ) {  l <- c(l, c("UCLA")) }
            if ( "jhu.proj.xts" %in% names(county.deaths()) ) {  l <- c(l, c("JHU")) }
            if ( "can.proj.xts" %in% names(county.deaths()) ) {  l <- c(l, c("COVIDActNow")) }
            if ( input$approve_upload == TRUE & rV$county == c & rV$death == TRUE) { l <- c(l, c("Uploaded")) }
            if ( length(l) > 2 ) { l <- c(l, c("Mean") ) }
            
            #Source
            s <- c("Please see the Technical Notes tab of the application for data sources.",rep("",ncol(county.deaths())-1))
            p <- c("Prepared by: California Department of Public Health - COVID Modeling Team",rep("",ncol(county.deaths())-1))
            
            dlm <- rbind(t, tt, l, df, s, p)
            write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
        })   
    
#### Long-term Epi-Models ####
    
    output$model.descrip.ts <- renderUI({
        
        UKKC <- as.character(input$include_JHU_UKKC)
        model_descrip_list <- lapply(UKKC, function(i) { HTML(paste("<p>",as.character(scenarios[match(i, scenarios$colvar),2]),": ",
                                                                            as.character(scenarios[match(i, scenarios$colvar),4]),"</p>")) })
        do.call(tagList, model_descrip_list)
        
    })
    
#### Daily Estimates #####
    
    output$physical.select <- renderUI({
        
        pickerInput(
            inputId = "include_JHU_UKKC", "Select Scenario", 
            choices = switch(input$county_ts,
                             "California" = list("5/28/2020" = modellist[c(8:11)],
                                                 "4/23/2020" = modellist[c(4:7)],
                                                 "Real-time" = randlist,
                                                 "Real-time" = otherlist[1:2],
                                                 "Real-time" = otherlist[3]      ),
                             list("5/28/2020" = modellist[c(8:11)],
                                  "Real-time" = modellist[c(4:7)],
                                  "Real-time" = otherlist[1:2]    )
            ),
            selected = c("strictDistancingNow", 
                         "weakDistancingNow",
                         "IHME_sts",
                         "rand.3.1", 
                         "rand.2.2", 
                         "rand.1.3"),
            options = list(`actions-box` = TRUE, noneSelectedText = "Select Scenario"), 
            #inline = TRUE,
            multiple = TRUE,
            choicesOpt = list( style = rep(("color: black; background: white; font-weight: bold;"),14)) 
        )
        
    })
    
    output$epi_covid_select <- renderUI({
        selectInput("select_COVID",
                    "Select Actuals:",
                    COVIDvar,
                    selected = switch(input$selected_crosswalk,
                                      "1" = "COVID.19.Positive.Patients",
                                      "2" = "ICU.COVID.19.Positive.Patients",
                                      "3" = "Total.Count.Deaths")
                    )
    })
    
    state.model.xts <- reactive({
      state_model_xts_memoized(input$county_ts, input$selected_crosswalk)
    })
    
    total.cnty.beds <- reactive({
        
        c <- input$county_ts 
        
        if (c %in% cnty.beds[,1] == TRUE) {
            beds <- c(cnty.beds[which(cnty.beds$COUNTY == c),9])
        } else {
            beds <- c(NA)
        }
        
    })
    
    #Regex patterns for JHU scenarios
    
    jhu.no <- "UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}"
    jhu.M <- "UK.\\w+.\\d+_\\d+.M|.\\w+_\\w{4,}.M"
    jhu.lh <- "UK.\\w+.\\d[w].\\w+.[LH]|.\\w+_\\w{4,}.[LH]"
    jhu.lh.b <- "UK.\\w+.\\d+_\\d+.[LH]|.\\w+_\\w{4,}.[LH]"
    rand.no <- "\\w\\.\\d\\.\\d"
    
    output$physical.graph <- renderDygraph({
        
        df <- state.model.xts()
        dtrange <- paste(as.character(input$dateRange_ts), collapse = "/")
        
        chbx <- c()
        
        #### Actuals
        
        if ( input$actuals == TRUE) {chbx <- c(chbx,c(input$select_COVID)) }
        
        UKKC <- as.character(input$include_JHU_UKKC)
        
        if ( TRUE %in% grepl(jhu.no, UKKC) & input$physical.mmd == "M" ) {
            JHU_list <- UKKC[grep(jhu.no,UKKC)]
            chbx <- c(chbx, c(JHU_list) )
        } else {
            JHU_list <- UKKC[grep(jhu.no,UKKC)]
            chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) { paste0(as.character( JHU_list[[i]] ),".M" ) } ) ) )  )
        }
        
        if (TRUE %in% grepl(jhu.no, UKKC) & input$physical.iqr == TRUE) {
            JHU_list <- UKKC[grep(jhu.no,UKKC)]
            chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".L" ) } )) ),
                      c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".H" ) } )) ) )
        }
        
        if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$county_ts == "California" ) { 
            chbx <- chbx %>% c("IHME_sts")
        }
        
        if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$IHME.iqr == TRUE & input$county_ts == "California") { 
            IHME <- "IHME_sts"
            chbx <- c(chbx, c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".L") } )) ),
                      c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".H") } )) )
            )
        }
        
        if ( TRUE %in% grepl(rand.no, UKKC ) & input$county_ts == "California" ) { 
            RAND_list <- UKKC[grep(rand.no,UKKC)]
            chbx <- c(chbx, c( RAND_list ) )
        }
        
        if ( TRUE %in% grepl(rand.no, UKKC ) & input$RAND.iqr == TRUE & input$county_ts == "California") { 
            RAND_list <- UKKC[grep(rand.no,UKKC)]
            chbx <- c(chbx, c( as.character(lapply(seq_along(RAND_list), function(i) {paste0(as.character( RAND_list[[i]] ),".L") } )) ),
                    c( as.character(lapply(seq_along(RAND_list), function(i) {paste0(as.character( RAND_list[[i]] ),".H") } )) )
            )
        }
        
        if ( TRUE %in% grepl("weakDistancingNow|strictDistancingNow",UKKC) & 
            
            input$county_ts %in% can_counties == TRUE ) {
            can <- UKKC[grep("weakDistancingNow|strictDistancingNow",UKKC)] 
            chbx <- chbx %>% c(can)
        } 
        
        df <- df[,c(chbx)]
        
        FUNC_JSFormatNumber <- "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"
        
        d <- dygraph(df, main = switch(input$selected_crosswalk,
                                       "1" = paste0(input$county_ts," COVID Hospitalizations"),
                                       "2" = paste0(input$county_ts," COVID ICU Patients"),
                                       "3" = paste0(input$county_ts," COVID Cumulative Deaths")
        )) 
        
        if ( TRUE %in% grepl(jhu.lh, chbx) |  TRUE %in% grepl(jhu.lh.b, chbx) )  {
            
            if ( input$physical.mmd == "M") {
                chbx.M <- chbx[grep(jhu.no,chbx)]
                chbx.M <- unique(str_remove(chbx.M, "\\.[LH]"))
                for (scenario in chbx.M) { 
                    d <- d %>% dySeries(c( paste0(scenario,".L"),paste0(scenario),paste0(scenario,".H")),  label = names(modellist[match(scenario,modellist)]), fillGraph = FALSE)
                }
            } else  {
                chbx.M <- chbx[grep(jhu.M,chbx)]
                chbx.M <- str_remove(chbx.M, ".M")
                for (scenario in chbx.M) { 
                    d <- d %>% dySeries(c( paste0(scenario,".L"),paste0(scenario,".M"),paste0(scenario,".H")),  label = names(modellist[match(scenario,modellist)]), fillGraph = FALSE)
                }
            }
        # No intervals 
        } else {
            if ( input$physical.mmd == "M") {
                chbx.M <- chbx[grep(jhu.no,chbx)]
                for (scenario in chbx.M) { 
                    d <- d %>% dySeries(paste0(scenario),  label = names(modellist[match(scenario,modellist)]), fillGraph = FALSE)
                }
            } else {
                chbx.M <- chbx[grep(jhu.M,chbx)]
                chbx.M <- str_remove(chbx.M, ".M")
                for (scenario in chbx.M) { 
                    d <- d %>% dySeries(paste0(scenario,".M"),  label = names(modellist[match(scenario,modellist)]), fillGraph = FALSE)
                }
            }
            
        }
        
        if ( TRUE %in% grepl("IHME_sts.[LH]", chbx) ){
            if ( "IHME_sts.L" %in% c(chbx) )  {d <- d %>% dySeries(c("IHME_sts.L","IHME_sts","IHME_sts.H"),  label = 'IHME Model', fillGraph = FALSE) }
        } else {
            if ( "IHME_sts" %in% c(chbx) )  {d <- d %>% dySeries("IHME_sts",  label = 'IHME Model', fillGraph = FALSE) }
        }
        
        if ( "weakDistancingNow" %in% c(chbx)   )  {d <- d %>% dySeries("weakDistancingNow",  label = 'CAN: Delay/Distancing', fillGraph = FALSE) }
        if ( "strictDistancingNow" %in% c(chbx) )  {d <- d %>% dySeries("strictDistancingNow",  label = 'CAN: Shelter in Place', fillGraph = FALSE) }
        
        if ( TRUE %in% grepl("\\w\\.\\d\\.\\d.[LH]", chbx) ){
            chbx.R <- chbx[grep(rand.no,chbx)]
            chbx.R <- unique(str_remove(chbx.R, "\\.[LH]"))
            for (scenario in chbx.R) { 
                d <- d %>% dySeries(c( paste0(scenario,".L"),paste0(scenario),paste0(scenario,".H")),  label = names(modellist[match(scenario,modellist)]), fillGraph = FALSE)
            }
        } else {
            chbx.R <- chbx[grep(rand.no,chbx)]
            for (scenario in chbx.R) { 
                d <- d %>% dySeries(paste0(scenario),  label = names(modellist[match(scenario,modellist)]), fillGraph = FALSE)
            }
        }
        
        if ( "Total.Count.Deaths" %in% c(chbx) ) {d <- d %>% dySeries("Total.Count.Deaths",  label = "Total Deaths", fillGraph= FALSE, drawPoints = TRUE, pointSize = 5, pointShape = "square", color = "black") }
        if ( "COVID.19.Positive.Patients" %in% c(chbx) ) {d <- d %>% dySeries("COVID.19.Positive.Patients",  label = "Patients Positive for COVID-19", fillGraph= FALSE, drawPoints = TRUE, pointSize = 5, pointShape = "diamond", color = "black") }
        if ( "ICU.COVID.19.Positive.Patients" %in% c(chbx) ) {d <- d %>% dySeries("ICU.COVID.19.Positive.Patients",  label = "ICU Patients Positive for COVID-19", fillGraph= FALSE, drawPoints = TRUE, pointSize = 5, pointShape = "hexagon", color = "black") }
        
        #### Add county beds
        if ( input$selected_crosswalk == "1" & input$county_ts == "California") { 
            d <- d %>% dyLimit(50000, label = "Phase 1 Surge Capacity", labelLoc = c("left"), color = "black", strokePattern = "dashed") 
        } else {
            if ( input$selected_crosswalk == "1" & !is.na(total.cnty.beds()) == TRUE ) { d <- d %>% dyLimit(total.cnty.beds(), label = "Total Licensed Beds", labelLoc = c("left"), color = "black", strokePattern = "dashed") }
        }
       
        d <- d %>% dyOptions(digitsAfterDecimal=0, strokeWidth = 3, connectSeparatedPoints = TRUE, drawGrid = FALSE) %>%
                    dyAxis("y", axisLabelFormatter=JS(FUNC_JSFormatNumber), valueFormatter=JS(FUNC_JSFormatNumber)) %>%
                    dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
                    dyEvent(Sys.Date(), "Today", labelLoc = "top") %>%
                    dyLegend(show = "always",
                             labelsDiv = "legendDivID2",
                             hideOnMouseOut = TRUE) %>%
                    dyRangeSelector(height = 30, dateWindow = c((Sys.Date() - 30), as.Date("2020-09-30")) )
        
    })

#### Static Daily Estimates ####
    
    output$physical.graph.static <- renderPlot({
        
        df <- state.model.xts()[ paste0( as.Date(input$physical.graph_date_window[[1]]),"/",as.Date(input$physical.graph_date_window[[2]]) ) ]
        #dtrange <- paste(as.character(input$dateRange_ts), collapse = "/")
        
        chbx <- c()
        #### Uncontrolled + Actuals
        #if ( input$overlay_uncontrolled == TRUE ) { chbx <- chbx %>% c("No_Intervention") }
        if ( input$actuals == TRUE) {chbx <- c(chbx,c(input$select_COVID)) }
        
        UKKC <- as.character(input$include_JHU_UKKC)
        
        if ( TRUE %in% grepl(jhu.no, UKKC) & input$physical.mmd == "M" ) {
            JHU_list <- UKKC[grep(jhu.no,UKKC)]
            chbx <- c(chbx, c(JHU_list) )
        } else {
            JHU_list <- UKKC[grep(jhu.no,UKKC)]
            chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) { paste0(as.character( JHU_list[[i]] ),".M" ) } ) ) )  )
        }
        
        if (TRUE %in% grepl(jhu.no, UKKC) & input$physical.iqr == TRUE) {
            JHU_list <- UKKC[grep(jhu.no,UKKC)]
            chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".L" ) } )) ),
                     c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".H" ) } )) ) )
        }
        
        if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$county_ts == "California" ) { 
            chbx <- chbx %>% c("IHME_sts")
        }
        
        if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$IHME.iqr == TRUE & input$county_ts == "California") { 
            IHME <- "IHME_sts"
            chbx <- c(chbx, c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".L") } )) ),
                      c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".H") } )) )
            )
        }
        
        if ( TRUE %in% grepl("weakDistancingNow|strictDistancingNow",UKKC) & 
             input$county_ts %in% can_counties == TRUE ) {
            can <- UKKC[grep("weakDistancingNow|strictDistancingNow",UKKC)] 
            chbx <- chbx %>% c(can)
        } 
        
        if ( TRUE %in% grepl(rand.no, UKKC ) & input$county_ts == "California" ) { 
            RAND_list <- UKKC[grep(rand.no,UKKC)]
            chbx <- c(chbx, c( RAND_list ) )
        }
        
        if ( TRUE %in% grepl(rand.no, UKKC ) & input$RAND.iqr == TRUE & input$county_ts == "California") { 
            RAND_list <- UKKC[grep(rand.no,UKKC)]
            chbx <- c(chbx, c( as.character(lapply(seq_along(RAND_list), function(i) {paste0(as.character( RAND_list[[i]] ),".L") } )) ),
                    c( as.character(lapply(seq_along(RAND_list), function(i) {paste0(as.character( RAND_list[[i]] ),".H") } )) )
            )
        }
        
        df <- df[,c(chbx)]
        
        # nl <- as.numeric(match("No_Intervention",names(df)))
        # maxy <- suppressWarnings( max(df[,-as.numeric(nl)], na.rm=TRUE) + 
        #                               ( max(df[,-as.numeric(nl)], na.rm=TRUE) * 0.05)
        # )
        
        colors <- c("No Intervention"= "black",
                    "IHME Model" = "#023858", 
                    "CAN: Shelter in Place" = "#c994c7", 
                    "CAN: Delay/Distancing" = "#dd1c77",
                    
                    'JHU: NPIs 30-40% Effective' = "#d7301f",
                    'JHU: NPIs 40-50% Effective' = "#238b45",
                    'JHU: NPIs 50-60% Effective' = "#4d004b",
                    'JHU: NPIs 60-70% Effective' = "#67001f",
                    
                    "JHU: Continuing Lockdown" = "#d7301f",
                    'JHU: Slow-paced Reopening' = "#238b45",
                    'JHU: Moderate-paced Reopening' = "#4d004b",
                    'JHU: Fast-paced Reopening' = "#67001f",
                    
                    'RAND: Lift Shelter-in-Place Now' = "#023858",
                    'RAND: Reopen Non-essential Businesses in 2 weeks' = "#3690c0",
                    'RAND: Reopen Bars/Restaurants/Large events in 1 month' = "#a6bddb",
                    
                    #"Total Confirmed Cases" = "red",
                    "Total Deaths" = "black",
                    "Patients Positive for COVID-19" = "black",
                    "ICU Patients Positive for COVID-19" = "black"
                    #"Positive + Suspected Patients" = "green",
                    #"Positive + Suspected ICU" = "blue"
        )
        
        #test_colors <- c("Continued_Lockdown" = "#d7301f")
        
        p <- ggplot() 
        
        if (input$selected_crosswalk == "1" & input$drop_hline == TRUE & input$county_ts == "California") { 
            
            p <- p + geom_line(df, mapping = aes(x= Index, y = 50000), color = "black", linetype = "dashed") + 
                geom_text(aes(x = as.Date(input$physical.graph_date_window[[1]]), y= 50000, 
                              label = "Phase 1 Surge Capacity"), 
                              hjust = -0.1,
                              vjust = -0.3) 
        } else {
            
            if ( input$selected_crosswalk == "1" & !is.na(total.cnty.beds()) == TRUE ) { 
                p <- p + geom_line(df, mapping = aes(x= Index, y = total.cnty.beds()), color = "black", linetype = "dashed") + 
                         geom_text(aes(x = as.Date(input$physical.graph_date_window[[1]]), y= total.cnty.beds(), 
                                       label = "Total Licensed Beds"), 
                                       hjust = -0.1,
                                       vjust = -0.3) 
            }
        }
        
        #if ( "No_Intervention" %in% c(chbx) ) { p <- p + geom_line(df, mapping = aes(x = Index, y = No_Intervention), color = "black", size = 1.5, linetype = "dashed") }
        
        ### JHU Scenarios
        
        if ( TRUE %in% grepl(jhu.no, chbx))  {
            chbx.M <- chbx[grep(jhu.no,chbx)]
            chbx.M <- unique(str_remove(chbx.M, "\\.[MLH]"))
            for (scenario in chbx.M) { 
                c <- as.character(colors[match(names(modellist[match(scenario,modellist)]),names(colors))])
                if ( scenario %in% c(chbx)   ) { p <- p + geom_line(df, mapping = aes_string(x="Index", y=scenario, color = shQuote(names(modellist[match(scenario,modellist)])) ), size = 1.5, linetype = "solid") }
                if ( paste0(scenario,".M") %in% c(chbx)   ) { p <- p + geom_line(df, mapping = aes_string(x="Index", y=paste0(scenario,".M"), color = shQuote(names(modellist[match(scenario,modellist)])) ), size = 1.5, linetype = "solid") }
                if ( paste0(scenario,".L") %in% c(chbx) ) { p <- p + geom_ribbon(df, mapping = aes_string(x ="Index", ymin = paste0(scenario,".L"), ymax = paste0(scenario,".H") ), fill=c, color = c, alpha = 0.2) }
            }
        }
        
        ### Other Models/Scenarios
       
        if ( "IHME_sts" %in% c(chbx) ) { p <- p + geom_line(df, mapping = aes(x=Index, y=IHME_sts, color = "IHME Model"),  size = 1.5, linetype = "solid") }
        if ( "IHME_sts.L" %in% c(chbx) ) { p <- p + geom_ribbon(df, mapping = aes(x = Index, ymin = IHME_sts.L, ymax = IHME_sts.H), fill="#a6bddb", color = "#a6bddb", alpha = 0.2) }
        
        if ( "strictDistancingNow" %in% c(chbx)   ) { p <- p + geom_line(df, mapping = aes(x=Index, y=strictDistancingNow, color = "CAN: Shelter in Place"), size = 1.5 ) }
        if ( "weakDistancingNow" %in% c(chbx)     ) { p <- p + geom_line(df, mapping = aes(x=Index, y=weakDistancingNow, color = "CAN: Delay/Distancing" ), size = 1.5 )   }

        
        ### RAND Models
        
        if ( TRUE %in% grepl(rand.no, chbx))  {
            chbx.M <- chbx[grep(rand.no,chbx)]
            chbx.M <- unique(str_remove(chbx.M, "\\.[LH]"))
            for (scenario in chbx.M) { 
                c <- as.character(colors[match(names(modellist[match(scenario,modellist)]),names(colors))])
                if ( scenario %in% c(chbx)   ) { p <- p + geom_line(df, mapping = aes_string(x="Index", y=scenario, color = shQuote(names(modellist[match(scenario,modellist)])) ), size = 1.5, linetype = "solid") }
                if ( paste0(scenario,".L") %in% c(chbx) ) { p <- p + geom_ribbon(df, mapping = aes_string(x ="Index", ymin = paste0(scenario,".L"), ymax = paste0(scenario,".H") ), fill=c, color = c, alpha = 0.2) }
            }
        }
        
        ### Actuals
        
        if ( "Total.Count.Deaths" %in% c(chbx) ) {p <- p + geom_point(df, mapping = aes(x = Index, y = Total.Count.Deaths,  color = "Total Deaths"), shape = 15, fill = "black", size = 3 ) }
        if ( "COVID.19.Positive.Patients" %in% c(chbx) ) {p <- p + geom_point(df, mapping = aes(x = Index, y = COVID.19.Positive.Patients, color = "Patients Positive for COVID-19"), shape = 23, fill = "black", size = 3 ) }
        if ( "ICU.COVID.19.Positive.Patients" %in% c(chbx) ) {p <- p + geom_point(df, mapping = aes(x = Index, y = ICU.COVID.19.Positive.Patients,  color = "ICU Patients Positive for COVID-19"), shape = 19, fill = "black", size = 3 ) }
       
        # if ( input$overlay_uncontrolled == TRUE ) {
        #     p <- p + scale_y_continuous(labels = scales::comma, limits = c(0, as.numeric(maxy)) ) 
        # }  else {
            p <- p + scale_y_continuous(labels = scales::comma) 
        #}
        
        p <- p + labs(x = "Date", 
                      y = switch(input$selected_crosswalk,
                                 "1" = "Hospital Bed Occupancy",
                                 "2" = "ICU Bed Occupancy",
                                 "3" = "Cumulative Deaths"),
                      color = "Legend") + scale_color_manual(values = colors) +
            
            ggtitle(switch(input$selected_crosswalk,
                           "1" = paste0(input$county_ts," COVID Hospitalizations"),
                           "2" = paste0(input$county_ts," COVID ICU Patients"),
                           "3" = paste0(input$county_ts," COVID Cumulative Deaths")
            )) +
            
            theme(plot.title = element_text(size = 18, face = "bold"),
                  axis.title = element_text(face = "bold", size = 18, colour = "black"),
                  axis.text.x = element_text(face = "bold", color = "black", size = 18),
                  axis.text.y = element_text(face = "bold", color = "black", size = 18),
                  axis.line = element_line(color = "black", size = 1, linetype = "solid"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  legend.text=element_text(size=14),
                  legend.position = "bottom"
                  
            ) 
        
        return(p)
        
    })
    
    
    ## download Static figure data 
    
    static.plot.data <- reactive({
        df <- state.model.xts()[ paste0( as.Date(input$physical.graph_date_window[[1]]),"/",as.Date(input$physical.graph_date_window[[2]]) ) ]
        #dtrange <- paste(as.character(input$dateRange_ts), collapse = "/")
        
        chbx <- c()
        #### Uncontrolled + Actuals
        if ( input$actuals == TRUE) {chbx <- c(chbx,c(input$select_COVID)) }
        
        UKKC <- as.character(input$include_JHU_UKKC)
        
        if ( TRUE %in% grepl("UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}", UKKC) & input$physical.mmd == "M" ) {
            JHU_list <- UKKC[grep("UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}",UKKC)]
            chbx <- c(chbx, c(JHU_list) )
        } else {
            JHU_list <- UKKC[grep("UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}",UKKC)]
            chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) { paste0(as.character( JHU_list[[i]] ),".M" ) } ) ) )  )
        }
        
        if (TRUE %in% grepl("UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}", UKKC) & input$physical.iqr == TRUE) {
            JHU_list <- UKKC[grep("UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}",UKKC)]
            chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".L" ) } )) ),
                      c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".H" ) } )) ) )
        }
        
        if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$county_ts == "California" ) { 
            chbx <- chbx %>% c("IHME_sts")
        }
        
        if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$IHME.iqr == TRUE & input$county_ts == "California") { 
            IHME <- "IHME_sts"
            chbx <- c(chbx, c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".L") } )) ),
                      c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".H") } )) )
            )
        }
        
        if ( TRUE %in% grepl("weakDistancingNow|strictDistancingNow",UKKC) & input$selected_crosswalk != "2") {
            can <- UKKC[grep("weakDistancingNow|strictDistancingNow",UKKC)] 
            chbx <- chbx %>% c(can)
        } 
        
        df <- df[,c(chbx)] %>% data.frame() %>% mutate(Date = seq(as.Date(input$physical.graph_date_window[[1]]),as.Date(input$physical.graph_date_window[[2]]), by = "day"))
        
        df
        
    })
    
    
    output$dlScenario <- downloadHandler(
        filename = function () {
            paste0("COVID_Scenarios_",input$county_ts,".csv")
        },
        
        content = function(file) {
            
            # Title
            t <- c(paste("Long-term COVID Scenarios for ",input$county_ts, sep = ""),rep("",ncol(static.plot.data())-1))
            #Subtitle
            tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(static.plot.data())-1))
            #Column labels
            l <- names(static.plot.data())
            
            df <- static.plot.data()
            df[is.na(df)] <- 0
            df[] <- lapply(df, as.character)
            
            #Source
            s <- c("Please see the Technical Notes tab of the application for data sources.",rep("",ncol(static.plot.data())-1))
            p <- c("Prepared by: California Department of Public Health - COVID Modeling Team",rep("",ncol(static.plot.data())-1))
            
            dlm <- rbind(t, tt, l, df, s, p)
            write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
            
            #write.csv(df, file, row.names = F)
        }
        
    )
    
    
    
} # End Server

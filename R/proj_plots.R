hosp.proj.plot.memoized <- memoize2(function(df, approve_upload, county, hosp) {
   
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
        if (approve_upload == TRUE & county == "California" & hosp == TRUE) { 
          
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
    plotly_build2(p)
    
})


county.hosp.plot.memoized <- memoize2(function(select.county.hosp, df, approve_upload, county, hosp) {

    c <- names(fipslist[match(select.county.hosp,fipslist)])
    
    # df <- hosp.proj.cnty.ts()  
    
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
    if (select.county.hosp %in% canfipslist) {
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
    if ( approve_upload == TRUE & county == c & hosp == TRUE) { 
        
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
            title = as.character(counties[match(select.county.hosp, counties$fips),1]),
            xaxis = list(title = NULL, showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
            yaxis = list(title = "Hospitalziations", showline = TRUE, showgrid = FALSE, zeroline = FALSE),
            margin = list(l = 100),
            showlegend = TRUE, 
            shapes = list(today)
        ) 
    plotly_build2(p)
    
})

cdeath.proj.plot.memoized <- memoize2(function(df) {
        
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
    p <- p %>% 
        layout(title = NULL,
               xaxis = list(title = " ", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
               yaxis = list(title = "Total Deaths", showline = TRUE, showgrid = FALSE, zeroline = FALSE, hoverformat = ',.2r' ),
               margin = list(l = 100),
               legend = list(traceorder = "reversed"),
               showlegend = TRUE)

    plotly_build2(p)
})


county.death.plot.memoized <- memoize2(function(select.county.death, df,
                                                approve_upload, county, death) {
    c <- names(fipslist[match(select.county.death,fipslist)])

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
    if (select.county.death %in% canfipslist) {
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
    if (approve_upload == TRUE & county == c & death == TRUE) { 
        
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
            title = as.character(counties[match(select.county.death, counties$fips),1]),
            xaxis = list(title = NULL, showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
            yaxis = list(title = "Total Deaths", showline = TRUE, showgrid = FALSE, zeroline = FALSE),
            margin = list(l = 100),
            showlegend = TRUE, 
            shapes = list(today)
        ) 
    
    plotly_build2(p)
})

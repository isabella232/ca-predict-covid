rt.plot.memoized <- memoize2(function(df, approve_upload, county, reff) {
   
    # df <- rt.ts() %>% filter(index < Sys.Date()-1 & index > Sys.Date() -80)
    
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
    if ( approve_upload == TRUE & county == "California" & reff == TRUE) { 
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
    
  plotly_build2(p)
})        


county.rt.plot.memoized <- memoize2(function(df, select.county.rt, approve_upload, county, reff) {

    # df <- county.rt()
    
    c <- names(canfipslist[match(select.county.rt,canfipslist)])
    
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
    if ( approve_upload == TRUE & county == c & reff == TRUE) { 
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
                        title = as.character(counties[match(select.county.rt, counties$fips),1]),
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
    
    plotly_build2(p)
    
})


rt.dot.plot.memoized <- memoize2(function(df) {
   
    # df <- cnty.7.day.rt
    
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
    plotly_build2(p)    
})


reff_county_map_memoized <- memoize2(function(df) {

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
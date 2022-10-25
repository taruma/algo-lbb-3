shinyServer(
  function(input, output) {
    # OUTPUT PLOT 1
    output$plot_bar <- renderPlotly({
        
        #dive deeper vids_count 
        vids_count <- 
            vids_clean %>% 
            group_by(category_id) %>% 
            summarise(count_vids = n()) %>% # n() -> untuk menghitung frekuensi
            arrange(desc(count_vids))
        
        #data
        vids_count <- 
            vids_count %>% 
            mutate(label = glue("Category Video : {category_id}
                      Total Video : {count_vids}"))
        
        #ggplot
        plot1 <- ggplot(data = vids_count, 
                        mapping = aes(x = count_vids, 
                                      y = reorder(category_id, count_vids),
                                      text = label)) + 
            geom_col(aes(fill = count_vids), show.legend = F) +
            scale_fill_gradient(low = "red", high = "black") +
            labs(
                title = "Trending Categories on YouTube US 2017",
                y = "",
                x = "Video Count"
            ) +
            theme_minimal()
        
        #ggplotly
        ggplotly(plot1, tooltip = "text")
        

    })
    
    output$plot_lolipop <- renderPlotly({
        #dive deeper vids_top_channel
        vids_top_channel <- 
            vids_clean %>% 
            filter(category_id == input$select_category) %>% 
            group_by(channel_title) %>% 
            summarise(total_views = sum(views)) %>% 
            arrange(desc(total_views)) %>% 
            head(10)
        
        #data
        vids_top_channel <- 
            vids_top_channel %>% 
            mutate(label = glue("Channel Title : {channel_title}
                      Total views : {comma(total_views)}"))
        
        #ggplot
        plot2 <- ggplot(data = vids_top_channel, 
                        mapping = aes(x = total_views, 
                                      y = reorder(channel_title, total_views),
                                      color = total_views,
                                      text = label)) + 
            geom_point(size = 3) +
            geom_segment(aes(x = 0,xend = total_views, yend = channel_title), size = 2)+
            scale_color_gradient(low = "red", high = "black") +
            labs(
                title = glue("Top Channel in {input$select_category}"),
                y = "",
                x = "Total Views") +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) +
            scale_x_continuous(labels = comma)
        
        #ggplotly
        ggplotly(plot2, tooltip = "text")
    })
    
    output$plot_line <- renderPlotly({
        #data
        vids_trend <- 
            vids_clean %>%
            filter(category_id == input$select_category) %>% 
            group_by(publish_hour) %>%
            summarise(mean_views = mean(views)) %>% 
            mutate(label = glue("Publish Hour : {publish_hour}
                      Average Views : {comma(mean_views)}"))
        
        #ggplot
        plot3 <- 
            ggplot(data = vids_trend, 
                   mapping = aes(x=publish_hour,
                                 y=mean_views)) +
            geom_line( color="red", size=1, linetype=1) +
            geom_point(color="black", size=2, aes(text = label)) +
            labs(
                title = glue("Viewers Activity for {input$select_category} Videos"),
                y = "Views",
                x = "Publish Hour") +
            scale_y_continuous(labels = comma) +
            scale_x_continuous(breaks = seq(0,25,3)) +
            theme_minimal()
        
        #ggplotly
        ggplotly(plot3, tooltip = "text")
    })
  }
)

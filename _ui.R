dashboardPage(
    skin = "red",

    # HEADER
    dashboardHeader(title = "YouTube Analysis"),
    
    # SIDEBAR
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "tab_overview", icon = icon("video")),
            menuItem("Channels", tabName = "tab_channel", icon = icon("youtube")),
            menuItem("Data", tabName = "tab_data", icon = icon("table"))
        )
    ),
    
    # BODY
    dashboardBody(
        tabItems(
            # PAGE 1
            tabItem(tabName = "tab_overview",
                #ROW1 -> infobox
                fluidRow(
                    infoBox("TOTAL UNIQUE VIDEO", 
                            length(unique(vids_clean$title)), 
                            icon = icon("video"),
                            color = "red"),
                    infoBox("TOTAL UNIQUE CHANNELS", 
                            length(unique(vids_clean$channel_title)),
                            icon = icon("headset"),
                            color = "black"),
                    infoBox("CATEGORIES", 
                            length(unique(vids_clean$category_id)),
                            icon = icon("youtube"),
                            color = "red")
                ),
                
                #ROW 2 -> plot
                fluidRow(
                    box(width = 12,
                        
                        # plotlyOutput(outputId = "plot_bar")
                    )
                )
            ),
            
            # PAGE 2
            tabItem(tabName = "tab_channel",
                
                #ROW 1
                fluidRow(
                    box(width = 12,
                        selectInput(inputId = "select_category", 
                                    label = "Choose Video Category", 
                                    choices = unique(vids_clean$category_id), 
                                    selected = "Entertainment"
                                    )
                        )
                ),
                
                
                #ROW 2
                fluidRow(
                    
                    # BOX 1 -> Plot Lolipop
                    box(width = 6,
                        plotlyOutput(outputId = "plot_lolipop")
                    ),
                    
                    # BOX 2 -> Plot Line
                    box(width = 6,
                        
                        plotlyOutput(outputId = "plot_line")
                        
                    )
                )
                
            ),
            
            # PAGE 3
            tabItem(tabName = "tab_data",
                h1("Halaman 3")
                
            )
            
        )
    )
)


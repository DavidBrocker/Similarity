# -------------------------------------------------------------------------
# Load packages
# -------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(spotifyr)
library(shiny)
library(stringr)
library(shinydashboard)
library(ggraph)
library(igraph)
library(shinycssloaders)
library(shinybrowser)
library(shinydashboardPlus)
# -------------------------------------------------------------------------
# Set Environment
# -------------------------------------------------------------------------
Sys.setenv(SPOTIFY_CLIENT_ID = '1f6c9bc7d5314065bb3467e9ff984084')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b1b24f99d46541769013cf20539b191e')
access_token <- get_spotify_access_token()

# -------------------------------------------------------------------------
# For Example -------------------------------------------------------------
# -------------------------------------------------------------------------
example <- read.csv("https://raw.githubusercontent.com/DavidBrocker/Similarity2/main/Aesop%20Rock%20-%20Dessiderium.csv")
example2 <- read.csv("https://raw.githubusercontent.com/DavidBrocker/Similarity2/main/ax_mb.csv")
# -------------------------------------------------------------------------
# Establish Functions -----------------------------------------------------
# -------------------------------------------------------------------------
artist_similarity <- function(artist){
  # Get similar artists
  artist_sim <-
    # Need to use GAF to get artist_id
    get_artist_audio_features(artist,
                              include_groups = c("album", "single")) |>
    # Isolate id
    select(artist_id) |>
    # Get Unique value
    unique() |>
    # Pipe into get_related_artists
    get_related_artists() |> 
    # Unnest by the id column
    unnest(id, names_sep = "") |>
    # Make the id column contain the related artists for that id
    mutate(id = id |> map(get_related_artists)) |>
    # Get full list
    unnest(id, names_sep = "id") |> 
    mutate(original_artist = artist) |> 
    rename(parent_artist = name,
           related_artist = ididname,
           related_artist_genres = ididgenres,
           related_artist_followers.total = ididfollowers.total,
           related_artist_images = ididimages) |> 
    select(original_artist,parent_artist,genres,followers.total,
           related_artist, images,related_artist, related_artist_genres,
           related_artist_followers.total, related_artist_images)
}
# ------------------------------------------------------------------------------
# Test for Similarity: First Order ---------------------------------------------
# ------------------------------------------------------------------------------
first_order <- function(artist) {
  artist <-
    artist |>
    select(original_artist, parent_artist) |>
    group_by(original_artist) |>
    unique() |>
    group_by(parent_artist) |>
    summarise(isDup = n() > 1,
              n = n()) |>
    group_by(isDup) |>
    count() -> er_mgmt
  er_mgmt
  if (length(er_mgmt[er_mgmt$isDup == TRUE]) < 1) {
    "0%" -> pct_alike
  } else {
    er_mgmt |>
      ungroup() |>
      mutate(alike = n / sum(n) * 100) |>
      filter(isDup == "TRUE") |>
      select(alike) |>
      as.numeric() |>
      round(2) |>
      paste0("%") -> pct_alike
  }
  mget(ls())
}
# ------------------------------------------------------------------------------
# Test for Similarity: Second Order --------------------------------------------
# ------------------------------------------------------------------------------
second_order <- function(artist) {
  artist |>
    select(original_artist, related_artist) |>
    group_by(related_artist) |>
    unique() |>
    summarize(isDup = n() > 1, n = n()) |>
    group_by(isDup) |>
    count() -> er_mgmt
  if (length(er_mgmt[er_mgmt$isDup == TRUE]) < 1) {
    "0%" -> pct_alike
  } else {
    er_mgmt |>
      ungroup() |>
      mutate(alike = n / sum(n) * 100) |>
      filter(isDup == "TRUE") |>
      select(alike) |>
      as.numeric() |>
      round(2) |>
      paste0("%") -> pct_alike
  }
  mget(ls())
}
# -------------------------------------------------------------------------
# Create Function: Artist Top Track ---------------------------------------
# -------------------------------------------------------------------------
top_track <- function(artists) {
  artist_info <- map_df(artists, get_artist_audio_features,
                        include_groups = c("album", "single"))
  
  artist_info |>
    select(artist_id) |>
    unique() |>
    unnest(artist_id, names_sep = "") |>
    mutate(top_t = map(artist_id, get_artist_top_tracks)) |>
    unnest(top_t) |> 
    select(artist_id,id) |> 
    group_by(artist_id) |> 
    slice(1) |> 
    ungroup() |>  
    mutate(url = paste0("https://open.spotify.com/embed/track/",
                        id,"?utm_source=generator")
    )
}

# -------------------------------------------------------------------------
# Create Function: Artist Network Graphing --------------------------------
# -------------------------------------------------------------------------
artist_network <- function(artist){
  artist |> 
    select(original_artist,parent_artist) |> 
    group_by(original_artist,parent_artist) |> 
    unique() |> 
    ungroup() -> edgelist
  
  names(edgelist) <- c("from","to")
  
  artist_edge <- as.data.frame(edgelist)
  
  artist_graph <- graph_from_data_frame(artist_edge,directed = FALSE)
  
  community <- cluster_louvain(artist_graph)
  
  V(artist_graph)$color <- community$membership
  
  ggraph(artist_graph, layout = "fr") +
    geom_edge_arc(strength = 0.3, width = 0.7, alpha = 0.30,
                  color = "white") +
    geom_node_point(aes(color = factor(color),
                        fill = factor(color))) +
    geom_node_label(aes(label = name,
                        fill = factor(color)),
                        colour = "white",
                    label.padding =.4, 
                    repel = T,
                    label.size = NA) +
    theme_void() +
    scale_fill_manual(values = c("#4A001E","#0B194C")) +
    scale_color_manual(values = c("#4A001E","#0B194C")) +
    theme(legend.position = "none") 
}
# Define UI for application that draws a histogram
ui <-
  dashboardPage(
    skin = "midnight",
    dashboardHeader(
      titleWidth = "300px",
      title = span(
        "Six Degrees of Music",
        tags$img(
          src = "https://www.citypng.com/public/uploads/small/11661570388xlqve2emckykh8duxvsgpvh7twc500yxmhrxeqceos5tlsy69cafnjapavvuls7qozpoi4rz8u97zecjlqnva0yy38a7xxuxbu2r.png",
          height = '30',
          width = '30'
        )
      ),
      dropdownMenu(type = "notifications",
                   badgeStatus = NULL,
                   icon = icon("twitter"),
                   headerText = tags$a(href = "https://twitter.com/DaveBrocker","Follow me on Twitter!"))
    ),
    dashboardSidebar(
      collapsed = F,
      width = 300,
      sidebarMenu(id = "",
        sidebarSearchForm(
          textId = "input",
          buttonId = "click",
          label = "Artist1, Artist2",
          icon = icon("music")
        ),
        menuItem("Artist Similarity", tabName = "main", icon = icon('link')),
        menuItem("Example", icon = icon("search"), tabName = "Example",
                 menuSubItem("Example with No Matches",tabName = "Example1",icon=icon("sitemap")),
                 menuSubItem("Example with Matches",tabName = "Example2",icon=icon("sitemap"))
        ),
        menuItem("Source Code", 
                 icon = icon('code'),
                 href = "",
                 newtab = T),
        menuItemOutput("first_similarity"),
        menuItemOutput("second_similarity")
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "main",
                htmlOutput("artist1Frame"),
                htmlOutput("artist2Frame"),
                plotOutput("network")),
        tabItem(tabName = "Example1",
                htmlOutput("example1frame"),
                htmlOutput("example2frame"),
                plotOutput("exampleplot")),
        tabItem(tabName = "Example2",
                htmlOutput("example21frame"),
                htmlOutput("example22frame"),
                plotOutput("example2plot"))
        )
      )
    )
# Define server logic required to draw a histogram
server <- function(input, output) {
  # Show Message
  showNotification("Welcome! Enter two artists in the sidebar. This app 
                   uses the Spotify API to pull 20 related artists from both artists
                   resulting in 40 artists. Any duplicates in the list creates a similarity index.",
                   type = "message",
                   closeButton = TRUE,
                   duration = 30)
  
  my_reactive <- reactive({input$input})
  
  observeEvent(input$click,
  {
    # Separate Artists
    withProgress({
      artists <- unlist(str_split(my_reactive(),", "))
      artist <- artists
    incProgress(.25, message = "Separating artists and finding matches...")
    
    # Get First and Second Order Similarity
    artists <- map_df(artists,artist_similarity)
    incProgress(.25)
    
    # Get First Order Similarity
    artists1 <- first_order(artists)
    incProgress(.10, message = "Getting first order similarity...")
    
    # Get Second Order Similarity
    artists2 <- second_order(artists)
    incProgress(.10, message = "Getting second order similarity...")
    
    # Get Tracks
    track <- top_track(artist)
    incProgress(.10, message = "Getting tracks...")
    
    # Make First Order Menu
    output$first_similarity <- renderMenu({
      sidebarMenu(
        menuItem(
          paste0("First Order Similarity: ",
                str_to_sentence(artists1$pct_alike)), 
          icon = icon("music"))
      )
    })

    # Make Second Order Menu
    output$second_similarity <- renderMenu({
      sidebarMenu(
        menuItem(
          paste0("Second Order Similarity: ",
                 str_to_sentence(artists2$pct_alike)), 
          icon = icon("music"))
      )
    })
    
    # Make Network Graph
    output$network <- renderPlot(
      artist_network(artists),bg = "transparent"
    )
    incProgress(.20, message = "Making network graph...")
    })
    
    # Show Top Track
    output$artist1Frame <- renderUI({
      tags$iframe(
        src = track$url[1],
        seamless = "seamless",
        width = "100%%",
        height = "80px",
        frameBorder = "none"
      )
    })
    # Show Top Track
    output$artist2Frame <- renderUI({
      tags$iframe(
        src = track$url[2],
        seamless = "seamless",
        width = "100%%",
        height = "80px",
        frameBorder = "none"
      )
    })
  })


# Run When Example Click --------------------------------------------------
  output$example1frame <- renderUI({
    tags$iframe(
      src = "https://open.spotify.com/embed/track/124DjfCOxrqEXpHD04Hk8i?utm_source=generator",
      seamless = "seamless",
      width = "100%%",
      height = "80px",
      frameBorder = "none"
    )
  })
  
  output$example2frame <- renderUI({
    tags$iframe(
      src = "https://open.spotify.com/embed/track/7HD673WgVMKW3GQP1N2FjF?utm_source=generator",
      seamless = "seamless",
      width = "100%%",
      height = "80px",
      frameBorder = "none"
    )
  })

  output$exampleplot <- renderPlot(
    artist_network(example),
    bg = "transparent")



# Run When Example 2 Click ------------------------------------------------
output$example21frame <- renderUI({
  tags$iframe(
    src = "https://open.spotify.com/embed/track/6KF3l0dtaqHanpwJVmz3mB?utm_source=generator",
    seamless = "seamless",
    width = "100%%",
    height = "80px",
    frameBorder = "none"
  )
})

output$example22frame <- renderUI({
  tags$iframe(
    src = "https://open.spotify.com/embed/track/0LJTfmgOMvlLd0u4HU9twm?utm_source=generator",
    seamless = "seamless",
    width = "100%%",
    height = "80px",
    frameBorder = "none"
  )
})

output$example2plot <- renderPlot(
  artist_network(example2),
  bg = "transparent")
}


# Run the application 
shinyApp(ui = ui, server = server)

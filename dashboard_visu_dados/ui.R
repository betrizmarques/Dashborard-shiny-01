#

library(shiny)

# Define UI for application that draws a histogram
slidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")),
    tabItem(tabName = "widgets",
            h2('Widgets tab content')),
    
    dashboardPage(
      dashboardHeader(title = "Simple tabs"),
      slidebar,
      body
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  slidebar,
  body)


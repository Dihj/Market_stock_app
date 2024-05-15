# Load required libraries
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(openxlsx)
library(DT)
library(shinyWidgets)

# Define data
index_data <- read.csv("remain_items.csv", stringsAsFactors = FALSE)
index_data <- index_data[order(index_data$Ref), ]
index_data <- index_data %>%
  filter(Stock != 0)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(
    title = "My Stock Monitoring Tool",
    tags$li(
      class = "dropdown",
      tags$img(src = "logo.jpg", height = 50)
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Daily delivery", tabName = "daily_delivery"),
      menuItem("Price and Delivery Daily Graph", tabName = "delivery_graph"),
      menuItem("Stock", tabName = "stock")
    )
  ),
  dashboardBody(
    skin = "black",
    tabItems(
      tabItem(
        tabName = "daily_delivery",
        fluidRow(
          box(
            title = "New Delivery",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            textInput("client_name", "Client:"),
            textInput("vendeur_name", "Vendeur:"),
            textInput("telephone", "Tel:"),
            selectInput("command_index", "Ref:", choices = NULL),
            textInput("command_description", "Description:", value = ""),
            selectInput("size", "Size:", choices = NULL),
            selectInput("color", "Color:", choices = NULL),
            dateInput("delivery_date", "Delivery Date:", value = Sys.Date()),
            numericInput("total_price", "Price (Ar):", value = NULL),
            actionButton("submit", "Submit")
          )
        ),
        fluidRow(
          box(
            title = "Delivery Data",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            useShinyalert(),  # Set up shinyalert
            fluidRow(
              column(9, DTOutput("data_table")),
              column(3, actionButton("removeButton", "Remove Selected Row"))
            )
          )
        )
      ),
      tabItem(
        tabName = "delivery_graph",
        fluidRow(
          box(
            title = "Price and Delivery Daily Graph",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            plotlyOutput("delivery_plot")
          )
        )
      ),
      tabItem(
        tabName = "stock",
        fluidRow(
          box(
            title = "Stock Overview",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            selectInput("filter_index", "Filter by Index:", choices = NULL),
            plotlyOutput("stock_plot"),
            fluidRow(
              column(width = 6, 
                     tableOutput("stock_table")),
              column(width = 6, 
                     plotlyOutput("stock_plot_table"))
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update the command index dropdown menu with the choices
  updateSelectInput(session, "command_index", choices = unique(index_data$Ref))
  
  # Update the filter index dropdown menu with the choices
  updateSelectInput(session, "filter_index", choices = unique(index_data$Ref))
  
  # Update the command description based on the selected index
  observeEvent(input$command_index, {
    selected_index <- input$command_index
    index_row <- which(index_data$Ref == selected_index)
    updateTextInput(session, "command_description", value = index_data$Description[index_row[1]])
    updateTextInput(session, "total_price", value = index_data$Price[index_row[1]])
    updateSelectInput(session, "size", choices = unique(index_data$Size[index_row]))
  })
  
  # Update the color choices based on the selected size and index
  observeEvent(input$size, {
    if (!is.null(input$command_index) && !is.null(input$size)) {
      selected_index <- input$command_index
      available_colors <- unique(index_data$Color[index_data$Ref == selected_index & index_data$Size == input$size])
      updateSelectInput(session, "color", choices = available_colors)
    }
  })
  
  # Initialize reactiveValues to store the data
  data <- reactiveValues(df = NULL, stock = NULL)
  excel_data <- reactiveValues(dely = NULL)
  
  # Load existing data from Excel file if it exists, otherwise create an empty dataframe
  observe({
    if (file.exists("stock_commands.xlsx")) {
      data$df <- read_excel("stock_commands.xlsx")
    } else {
      data$df <- data.frame(Client = character(), 
                            Vendeur = character(), 
                            Telephone = character(), 
                            Ref = character(), 
                            Description = character(), 
                            Size = character(), 
                            Color = character(),
                            Delivery_Date = character(), 
                            Price = numeric(), 
                            stringsAsFactors = FALSE)
    }
  })
  
  # Update data and clear input fields when Submit button is clicked
  observeEvent(input$submit, {
    client <- input$client_name
    vendeur <- input$vendeur_name
    telephone <- input$telephone
    command_index <- input$command_index
    command_description <- input$command_description
    size <- input$size
    color <- input$color
    delivery_date <- input$delivery_date
    total_price <- input$total_price
    
    # Append new data to dataframe
    new_data <- data.frame(Client = client, 
                           Vendeur = vendeur,
                           Telephone = telephone, 
                           Ref = command_index, 
                           Description = command_description,
                           Size = size, 
                           Color = color,
                           Delivery_Date = as.character(delivery_date), 
                           Price = total_price, 
                           stringsAsFactors = FALSE)
    
    # Write updated data to Excel file
    write.xlsx(new_data, "stock_commands.xlsx", append = TRUE)
    
    # Append delivery data to another Excel file
    if (!file.exists("all_deliveries.xlsx")) {
      write.xlsx(data$df, "all_deliveries.xlsx")
    } else {
      zz <- read_xlsx('all_deliveries.xlsx')
      dz <- rbind(zz, new_data)
      names(dz) <- c('Client', 'Vendeur', 'Telephone', 'Ref', 'Description', 'Size', 'Color', 'Delivery_Date', 'Price')
      write.xlsx(dz, "all_deliveries.xlsx", append = TRUE)
    }
    
    # Update stock for the items in the latest delivery
    if (!is.null(data$stock)) {
      stock <- data$stock
      latest_delivery <- new_data %>%
        mutate(Delivery_Date = as.Date(Delivery_Date)) %>%
        arrange(desc(Delivery_Date)) %>%
        slice(1)  # Select only the latest delivery
      
      for (i in 1:nrow(latest_delivery)) {
        index <- latest_delivery[i, "Ref"]
        size <- latest_delivery[i, "Size"]
        color <- latest_delivery[i, "Color"]
        
        if (index %in% index_data$Ref & size %in% index_data$Size & color %in% index_data$Color) {
          stock_index <- which(stock$Index == index & stock$Size == size & stock$Color == color)
          stock$Stock[stock_index] <- stock$Stock[stock_index] - 1
        }
      }
      
      data$stock <- stock
      stoky <- data$stock
      names(stoky) <- c("Ref","Description","Size","Color","Price","Stock")
      write.csv(stoky, "remain_items.csv", row.names = FALSE)
    }
    
    # Clear input fields
    updateTextInput(session, "client_name", value = "")
    updateTextInput(session, "vendeur_name", value = "")
    updateTextInput(session, "telephone", value = "")
    updateSelectInput(session, "command_index", selected = NULL)
    updateTextInput(session, "command_description", value = "")
    updateTextInput(session, "size", value = "")
    updateTextInput(session, "color", value = "")
    updateDateInput(session, "delivery_date", value = Sys.Date())
    updateNumericInput(session, "total_price", value = NULL)
    
    excel_data$dely <- read_excel('all_deliveries.xlsx')
  })
  
  # Try to do this to update delivary data
  observe({
    excel_data$dely <- read_excel('all_deliveries.xlsx')
  })
  
  # Calculate total price and number of deliveries each day
  delivery_data <- reactive({
    if (!is.null(excel_data$dely)) {
      excel_data$dely %>%
        mutate(Delivery_Date = as.Date(Delivery_Date)) %>%
        group_by(Delivery_Date) %>%
        summarise(Total_Price = sum(Price),
                  Number_of_Deliveries = n())
    }
  })
  
  output$delivery_plot <- renderPlotly({
    if (!is.null(delivery_data()) && nrow(delivery_data()) > 0) {
      ggplotly(
        ggplot(delivery_data(), aes(x = Delivery_Date)) +
          geom_bar(aes(y = Total_Price), fill = "blue", alpha = 0.5, stat = "identity") +
          geom_line(aes(y = Number_of_Deliveries * 1000), color = "red") +
          scale_y_continuous(
            name = "Total Price (Ar)",
            sec.axis = sec_axis(~./1000, name = "Number of Deliveries")
          ) +
          labs(title = "Total Price and Number of Deliveries Over Time", x = "Date", y = "Total Price (Ar)") +
          theme_minimal()
      )
    }
  })
  
  # Update available stock
  observeEvent(data$df, {
    if (!is.null(data$df)) {
      stock <- data.frame(Index = index_data$Ref,
                          Description = index_data$Description,
                          Size = index_data$Size,
                          Color = index_data$Color,
                          Price = index_data$Price,
                          Stock = index_data$Stock)
      
      for (i in 1:nrow(data$df)) {
        index <- data$df[i, "Ref"]
        size <- data$df[i, "Size"]
        color <- data$df[i, "Color"]
        
        if (index %in% index_data$Ref & size %in% index_data$Size & color %in% index_data$Color) {
          stock_index <- which(stock$Index == index & stock$Size == size & stock$Color == color)
          stock$Stock[stock_index] <- stock$Stock[stock_index] - 1
        }
      }
      
      data$stock <- stock
    }
  })
  
  # Update delivery data
#  observeEvent(excel_data$dely, {
#    excel_data$dely <- read_excel('all_deliveries_test.xlsx')
#  })
  
  observe({
    output$data_table <- renderDT({
      datatable(tail(excel_data$dely, dim(excel_data$dely)[1]), escape = FALSE, options = list(pageLength = 10), selection = 'single')
    })
  })
  ### This is for plot Stock
    observe({
    # Display the stock table
    output$stock_table <- renderTable({
      if (!is.null(data$stock)) {
        filtered_stock <- data$stock
        if (!is.null(input$filter_index)) {
          filtered_stock <- filtered_stock[filtered_stock$Index == input$filter_index, ]
        }
        filtered_stock
      }
    })
    
    # Plot available stock
    output$stock_plot <- renderPlotly({
      
     if (!is.null(data$stock)) {
       ggplotly(
          ggplot(data$stock, aes(x = Index, y = Stock)) +
            geom_bar(stat = "identity", fill = "blue") +
            labs(title = "Available Stock", x = "Index", y = "Available Stock") +
            theme_minimal()
        )
      }
    })
  # PLot stock table for each index items
    output$stock_plot_table <- renderPlotly({
      if (!is.null(data$stock)) {
        #        if (!is.null(data$stock)) {
        filtered_stock <- data$stock
        if (!is.null(input$filter_index)) {
          filtered_stock <- filtered_stock[filtered_stock$Index == input$filter_index, ]
        }
        filtered_stock
        
        ggplotly(
          ggplot(filtered_stock, aes(x = Size, y = Color, color = as.factor(Stock), size = Stock)) + 
            geom_point(alpha = 0.7) +
            scale_color_manual(values = c("blue", "green", "red", "purple", "orange", "cyan", "magenta"), name = "Stock") +
            labs(title = "Stock Distribution by Size and Color", x = "Size", y = "Color", color = "Stock") +
            theme_minimal()
          
        )
      }
    
    })
    # Display detailed stock information
    output$stock_detail <- renderText({
      if (!is.null(input$command_index) && !is.null(data$stock)) {
        selected_index <- input$command_index
        stock_info <- data$stock %>%
          filter(Index == selected_index)
        paste("Description:", stock_info$Description, "\n",
              "Size:", stock_info$Size, "\n",
              "Color:", stock_info$Color, "\n",
              "Available Stock:", stock_info$Stock)
      }
    })
  
    
  })
  
  
  # This is to remove selected row
  observeEvent(input$removeButton, {
    if (length(input$data_table_rows_selected) > 0) {
      confirmSweetAlert(
        session = session,
        inputId = "remove_row",
        title = "Confirmation",
        text = "Are you sure you want to remove the selected row?",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonColor = "#DD6B55",
        confirmButtonText = "Yes, remove it!",
        cancelButtonText = "No, cancel it"
      )
    } else {
      confirmSweetAlert(
        session = session,
        inputId = "no_row_selected",
        title = "Error",
        text = "Please select a row to remove.",
        type = "error",
        showCancelButton = FALSE
      )
    }
  })
  
  observeEvent(input$remove_row, {
    if (input$remove_row) {
      rowIndex <- input$data_table_rows_selected
      
      # Remove selected row
      removed_row <- excel_data$dely[rowIndex, ]
      removed_row$Stock <- 1  # Add Stock column with value 1
      
      # Subset the necessary columns
      removed_values <- removed_row[, c("Ref","Description","Price","Color","Size","Stock")]
      # Update the Excel file
      tryCatch({
        # Remove selected row from the data frame
       # data$df <- data$df[-rowIndex, , drop = FALSE]
        excel_data$dely <- excel_data$dely[-rowIndex, , drop = FALSE]
       # updated_data <- data$df
        updated_data <- excel_data$dely
        write.xlsx(updated_data, "all_deliveries.xlsx", row.names = FALSE)
        # Update the table
        output$table <- renderDT({
         # datatable(data$df, options = list(pageLength = 5), selection = 'single')
          datatable(tail(excel_data$dely, dim(excel_data$dely)[1]), escape = FALSE, options = list(pageLength = 10), selection = 'single')
        })
        
        # Show success message
        confirmSweetAlert(
          session = session,
          inputId = "removed_successfully",
          title = "Success",
          text = "Row removed successfully.",
          type = "success",
          showCancelButton = FALSE
        )
      }, error = function(e) {
        # Show error message
        confirmSweetAlert(
          session = session,
          inputId = "error",
          title = "Error",
          text = "An error occurred while updating files. Please try again later.",
          type = "error",
          showCancelButton = FALSE
        )
      })
      
     # Update remain_items.csv to accumulate stock values if there are duplicates
      updated_remain_items <- read.csv("remain_items.csv", stringsAsFactors = FALSE)
      updated_remain_items <- rbind(updated_remain_items, removed_values)
      updated_remain_items <- aggregate(Stock ~ ., updated_remain_items, sum)
      write.csv(updated_remain_items, "remain_items.csv", row.names = FALSE)
      excel_data$dely <- read_excel('all_deliveries.xlsx')
    } else {
      # Show cancel message
      confirmSweetAlert(
        session = session,
        inputId = "cancelled_removal",
        title = "Cancelled",
        text = "Row removal cancelled.",
        type = "info",
        showCancelButton = FALSE
      )
    }
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

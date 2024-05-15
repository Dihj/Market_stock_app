
# Market Stock Monitoring Shiny App

This repository contains a simple Shiny web application for monitoring market stock. The application allows users to manage and track their inventory of items.

## Getting Started

To use this application, follow these steps:

1. Clone or download this repository to your local machine.
2. Make sure you have R and RStudio installed.
3. Install the required R packages by running the following command in RStudio:
   ```R
   install.packages(c('shiny', 'shinydashboard', 'readxl', 'dplyr', 'ggplot2', 'plotly', 'openxlsx', 'DT', 'shinyWidgets'))
   ```
4. Place your list of items inside the `remain_items.csv` file. This file should contain information about each item, including the amount of stock available.
5. Run the Shiny app by opening the `app.R` file in RStudio and clicking on the "Run App" button.

## Usage

Once the application is running, you can interact with it through the web interface. The main functionalities include:

- **Daily Delivery**: Enter new delivery information, including client details, item information, and delivery date.
- **Price and Delivery Daily Graph**: View graphical representations of total price and number of deliveries over time.
- **Stock Overview**: Monitor the current stock levels and filter items by index.

## Contributing

Feel free to open an issue or submit a pull request.

## License

This project is licensed under the [MIT License](LICENSE).


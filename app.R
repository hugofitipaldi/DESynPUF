library(rio)
library(tidyverse)
library(shiny)
library(shinythemes)
library(lubridate)


benefitiary_data1 <- rio::import("data/DE1_0_2008_Beneficiary_Summary_File_Sample_1.csv")
claims_data2 <- rio::import("data/DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.csv")
pxs_data3 <- rio::import("data/DE1_0_2008_to_2010_Prescription_Drug_Events_Sample_1.csv")

disease.options = c("Alzheimer", "Cancer", "Diabetes")
names(benefitiary_data1)

benefitiary_data1$BENE_BIRTH_DT <- as.Date(as.character(benefitiary_data1$BENE_BIRTH_DT), format = "%Y%m%d")

filtered_df <- benefitiary_data1 %>%
  filter(SP_DIABETES == 1 | SP_ALZHDMTA == 1 | SP_CNCR == 1)
filtered_df$Disease <- NA
filtered_df[filtered_df$SP_DIABETES == 1,]$Disease <- "Diabetes"
filtered_df[filtered_df$SP_ALZHDMTA == 1,]$Disease <- "Alzheimer"
filtered_df[filtered_df$SP_CNCR == 1,]$Disease <- "Cancer"

filtered_df$BENE_SEX_IDENT_CD <- factor(filtered_df$BENE_SEX_IDENT_CD, labels = c("Male", "Female"))

filtered_df$year_of_age <- lubridate::year(filtered_df$BENE_BIRTH_DT)
filtered_df$age <- 2008 - filtered_df$year_of_age

# ui ---------------------------------------------------------------------------

ui <-  navbarPage("Patient monitoring dashboard",
             theme = shinytheme("flatly"),
             tabPanel("Demographics",
                      pageWithSidebar(
                        headerPanel('Diasease dashboard'),
                        sidebarPanel("The CMS linkable 2008–2010 Medicare Data Entrepreneurs’ Synthetic Public Use File (DE-SynPUF) was designed to create new type of file that would be useful for data entrepreneurs for software and application development and training purposes. The files preserve the detailed data structure and metadata of key variables at both the beneficiary and claim levels. However, the data are fully “synthetic,” meaning no beneficiary in the DE-SynPUF is an actual Medicare beneficiary. They are all synthetic beneficiaries meant to represent actual beneficiaries. In order to protect the privacy of beneficiaries and to greatly reduce the risk of re-identification, a significant amount of interdependence and co-variation among variables has been altered in the synthetic process. The synthetic process used significantly diminishes the analytic utility of the file to produce reliable inferences about the actual Medicare beneficiary population (i.e., univariate statistics and regression coefficients produced with the DE-SynPUF will be biased).",
                                     selectInput("disease_opt", "Select a disease:", disease.options)
                                     ),
                        mainPanel(
                          fluidRow(
                            plotlyOutput('gender.plot')
                            ),
                          fluidRow(
                            plotlyOutput('age.hist')
                            )
                          )
                        )
                      ),
             tabPanel("More Info"),

)



# server -----------------------------------------------------------------------

server <- function(input, output) {

  #pie_palette <- wes_palette("Royal1")
  pie_palette <- c("#1B9E77", "#D95F02")

  output$gender.plot <- renderPlotly(
    filtered_df %>%
      filter(Disease == input$disease_opt) %>%
      group_by(BENE_SEX_IDENT_CD) %>%
      tally(name = "total") %>%
      mutate(prop = total/sum(total)) %>%
      plot_ly(labels = ~BENE_SEX_IDENT_CD,
              values = ~prop * 100,
              type = 'pie',
              marker = list(colors = pie_palette, line = list(color = '#FFFFFF', width = 1)),
              hoverinfo = 'text',
              hovertext = ~paste('</br> Gender: ', BENE_SEX_IDENT_CD,
                                 '</br>', round(prop * 100, 2), "%")) %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  )

  output$age.hist <- renderPlotly(
    filtered_df %>%
      mutate(
        # Create categories
        age_group = dplyr::case_when(
          age <= 17            ~ "0-17",
          age > 17 & age <= 30 ~ "18-30",
          age > 30 & age <= 40 ~ "30-40",
          age > 40 & age <= 50 ~ "40-50",
          age > 50 & age <= 60 ~ "50-60",
          age > 60 & age <= 70 ~ "60-70",
          age > 70 & age <= 80 ~ "70-80",
          age > 80 & age <= 90 ~ "80-90",
          age > 90             ~ "> 90"
        ),
        # Convert to factor
        age_group = factor(
          age_group,
          level = c("0-17", "18-30", "30-40","40-50","50-60","60-70","70-80","80-90","> 90"
          )
        )) %>%
      filter(Disease == input$disease_opt) %>%
      group_by(BENE_SEX_IDENT_CD, age_group) %>%
      tally(name = "count") %>%
      plot_ly(x = ~age_group,
              y = ~count,
              color = ~BENE_SEX_IDENT_CD,
              colors = pie_palette) %>%
      layout(yaxis = list(title = ''),
             xaxis = list(title = ''))
  )


}

shinyApp(ui, server)

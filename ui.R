library(shiny)
library(DT)
library(tidyverse)
library(shinyBS)
library(cowplot)
library(shinyWidgets)
library(streamgraph)

jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

source('text_pt.R')

shinyUI(fluidPage(
    tags$head(tags$script(HTML(jscode))),
    # Application title
    titlePanel(website_legends$title,windowTitle = website_legends$title),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            width = 4,
            h4(website_legends$header_1),
            website_explanations$tool_explanation_text,
            bsCollapse(
                id = "collapsable_menu", open = "",
                bsCollapsePanel(
                    style = "primary",
                    website_legends$add_data_csv,
                    fileInput(
                        inputId = "csv_file",
                        website_explanations$add_data_csv_explanation)),
                bsCollapsePanel(
                    style = "primary",
                    website_legends$add_data_manual,
                    tagAppendAttributes(
                        textInput(inputId = "party",label = website_legends$political_party),
                        `data-proxy-click` = "add"
                    ),
                    tagAppendAttributes(
                        numericInput(inputId = "prop",label = website_legends$proportion,value = 0),
                        `data-proxy-click` = "add"
                    ),tagAppendAttributes(
                        numericInput(inputId = 'total',label = website_legends$polled_total,value = 0),
                        `data-proxy-click` = "add"
                    ),
                    tagAppendAttributes(
                        dateInput(inputId = "poll_date",label = website_legends$date),
                        `data-proxy-click` = "add"
                    ),
                    tagAppendAttributes(
                        textInput(inputId = "poll_id",label = website_legends$poll_id),
                        `data-proxy-click` = "add"
                    ),
                    actionButton(inputId = 'add',label = website_legends$add)),
                bsCollapsePanel(
                    style = "primary",
                    "Apagar dados",
                    tagAppendAttributes(
                        textInput(inputId = "rows_to_delete",
                                  label = website_legends$delete_rows_comma_separated),
                        `data-proxy-click` = "delete_rows"
                    ),
                    actionButton(inputId = "delete_rows",label = website_legends$delete),
                    actionButton(inputId = "clear",label = website_legends$delete_all,
                                 style="color:#ffffff; background-color: #B73239")),
                bsCollapsePanel(
                    style = "primary",
                    website_legends$download_data_title,
                    div(
                        align = "center",
                        downloadButton("download_simple", website_legends$simplified),
                        downloadButton("download_with_error", website_legends$error_calculated),
                        downloadButton("download_complete", website_legends$complete)
                    )
                    ),
                bsCollapsePanel(
                    style = "primary",
                    website_legends$confidence,
                    sliderInput("confidence",
                                website_legends$confidence_for_ci,
                                min = 50, max = 100,
                                post = '%',
                                #step = 1,
                                value = 95)),
                bsCollapsePanel(
                    style = "primary",
                    website_legends$approximation_kalman,
                    website_explanations$kalman_explanation_text,
                    prettySwitch("kalman_trigger",website_legends$activate_visualization_kalman)),
                bsCollapsePanel(
                    style = "info",
                    website_legends$help,website_explanations$help_text),
                bsCollapsePanel(
                    style = "info",
                    website_legends$statistical_significance,
                    website_explanations$statistical_significance_text),
                bsCollapsePanel(
                    style = "info",
                    website_legends$intervals_and_confidence,
                    website_explanations$intervals_text),
                bsCollapsePanel(
                  style = "primary",
                  website_legends$calculate_error,
                  numericInput("calc_proportion",website_legends$proportion_percent,5,
                               min = 0,max = 100,step = 1),
                  numericInput("calc_sample_size",website_legends$sample_size,100,min = 1),
                  numericInput("calc_confidence",website_legends$confidence_percent,95,min = 0,max = 100,step = 1),
                  tableOutput("calc_output")),
                bsCollapsePanel(
                  style = "primary",
                  website_legends$compare_proportions,
                  website_explanations$compare_proportions,
                  fluidRow(
                    column(
                      width = 6,
                      numericInput("calc_proportion_1",website_legends$proportion_1_percent,5,
                                   min = 0,max = 100,step = 1)),
                    column(
                      width = 6,
                      numericInput("calc_proportion_2",website_legends$proportion_1_percent,5,
                                   min = 0,max = 100,step = 1))),
                  fluidRow(
                    column(
                      width = 6,
                      numericInput("calc_sample_size_1",website_legends$sample_size,100,
                                   min = 1)),
                    column(
                      width = 6,
                      numericInput("calc_confidence_2",website_legends$confidence_percent,95,min = 0,max = 100,step = 1))),
                  actionButton("compare_proportions",label = website_legends$compare_proportions),
                  tableOutput("calc_output_compare"),
                  plotOutput("compare_plot"))
                ),
            
            website_explanations$disclaimer_text
        ),
        mainPanel(
            style = 'max-height:10000px;',
            tabsetPanel(
                type = "tabs",
                tabPanel("Dados",div(br(),dataTableOutput('data_table') %>% addSpinner())),
                navbarMenu(
                    "Gráficos",
                    tabPanel(website_legends$plot_disc,
                             br(),prettySwitch("log_plot",website_legends$log_scale),
                             plotOutput("poll_plot") %>% addSpinner()),
                    tabPanel(website_legends$plot_comp,
                             br(),prettySwitch("log_plot_all",website_legends$log_scale),
                             plotOutput("poll_plot_all") %>% addSpinner()),
                    tabPanel(website_legends$plot_bars,
                             br(),plotOutput("poll_plot_bars") %>% addSpinner()),
                    tabPanel(website_legends$plot_vars,
                             br(),plotOutput("poll_plot_variations") %>% addSpinner()),
                    tabPanel(website_legends$streamgraph,
                             br(),streamgraphOutput("poll_plot_streamgraph"))),
                navbarMenu(website_legends$code,
                           tabPanel(a("Github",href = "https://github.com/josegcpa/sondagens"))),
                navbarMenu(website_legends$contact,
                           tabPanel(a("E-mail",href = "mailto:jose.gcp.almeida@gmail.com")))
        ))
    )
))
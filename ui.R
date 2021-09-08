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
    titlePanel(website_legends$title,windowTitle = 'São sondagens, senhor'),
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
                        textInput(inputId = "party",label = "Partido político"),
                        `data-proxy-click` = "add"
                    ),
                    tagAppendAttributes(
                        numericInput(inputId = "prop",label = "Proporção",value = 0),
                        `data-proxy-click` = "add"
                    ),tagAppendAttributes(
                        numericInput(inputId = 'total',label = 'Total de sondados',value = 0),
                        `data-proxy-click` = "add"
                    ),
                    tagAppendAttributes(
                        dateInput(inputId = "poll_date",label = "Data"),
                        `data-proxy-click` = "add"
                    ),
                    tagAppendAttributes(
                        textInput(inputId = "poll_id",label = "Sondagem"),
                        `data-proxy-click` = "add"
                    ),
                    actionButton(inputId = 'add',label = "Acrescentar")),
                bsCollapsePanel(
                    style = "primary",
                    "Apagar dados",
                    tagAppendAttributes(
                        textInput(inputId = "rows_to_delete",
                                  label = "Linhas a apagar (separadas por vírgulas)"),
                        `data-proxy-click` = "delete_rows"
                    ),
                    actionButton(inputId = "delete_rows",label = "Apagar"),
                    actionButton(inputId = "clear",label = "Apagar tudo",
                                 style="color:#ffffff; background-color: #B73239")),
                bsCollapsePanel(
                    style = "primary",
                    "Download dos dados",
                    div(
                        align = "center",
                        downloadButton("download_simple", "Simplificado"),
                        downloadButton("download_with_error", "Com erros calculados"),
                        downloadButton("download_complete", "Completo")
                    )
                    ),
                bsCollapsePanel(
                    style = "primary",
                    "Confiança",
                    sliderInput("confidence",
                                "Confiança (para CI):",
                                min = 50, max = 100,
                                post = '%',
                                #step = 1,
                                value = 95)),
                bsCollapsePanel(
                    style = "primary",
                    "Aproximação (filtro de Kalman)",
                    website_explanations$kalman_explanation_text,
                    prettySwitch("kalman_trigger","Ativar visualização")),
                bsCollapsePanel(
                    style = "info",
                    "Ajuda",website_explanations$help_text),
                bsCollapsePanel(
                    style = "info",
                    "Significância estatística?",
                    website_explanations$statistical_significance_text),
                bsCollapsePanel(
                    style = "info",
                    "Intervalos e confiança - mas porquê?",
                    website_explanations$intervals_text),
                bsCollapsePanel(
                  style = "primary",
                  website_legends$calculate_error,
                  numericInput("calc_proportion","Proporção (%)",5,min = 0,max = 100,step = 1),
                  numericInput("calc_sample_size","Tamanho da amostra",100,min = 1),
                  numericInput("calc_confidence","Confiança (%)",95,min = 0,max = 100,step = 1),
                  tableOutput("calc_output")),
                bsCollapsePanel(
                  style = "primary",
                  website_legends$compare_proportions,
                  p("A diferença entre as duas probabilidades tem significância estatística se o valor-p ",
                    " for inferior a 0.05 ou o intervalo da diferença das proporções não contiver o 0. ",
                    "Este teste não é válido para amostras pequenas (<30)."),
                  fluidRow(
                    column(
                      width = 6,
                      numericInput("calc_proportion_1","Proporção 1 (%)",5,min = 0,max = 100,step = 1)),
                    column(
                      width = 6,
                      numericInput("calc_proportion_2","Proporção 2 (%)",5,min = 0,max = 100,step = 1))),
                  fluidRow(
                    column(
                      width = 6,
                      numericInput("calc_sample_size_1","Tamanho da amostra",100,min = 1)),
                    column(
                      width = 6,
                      numericInput("calc_confidence_2","Confiança (%)",95,min = 0,max = 100,step = 1))),
                  actionButton("compare_proportions",label = "Comparar"),
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
                           tabPanel(a("Github",href = ""))),
                navbarMenu(website_legends$contact,
                           tabPanel(a("E-mail",href = "mailto:jose.gcp.almeida@gmail.com")))
        ))
    )
))
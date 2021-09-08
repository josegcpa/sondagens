library(shiny)
library(tidyverse)
library(DT)
library(shinyBS)
library(cowplot)
library(dlm)
library(tseries)
library(streamgraph)

PALETTE <- c("#332288","#6699CC","#88CCEE","#44AA99","#117733",
             "#999933","#DDCC77","#661100","#CC6677","#AA4466",
             "#882255","#AA4499")
RES <- 200

CONVERSION_LIST <- list(
    Partido.Social.Democrata = 'PSD',
    Partido.Socialista = 'PS',
    Bloco.de.Esquerda = 'BE',
    CDS.Partido.Popular = 'CDS-PP',
    Coligação.Democrática.Unitária = 'CDU',
    Pessoas.Animais.Natureza = 'PAN',
    LIVRE = 'LIVRE',
    Iniciativa.Liberal = 'IL',
    Aliança = 'A',
    Chega = 'CH',
    Other = 'Outros'
)


download_pt_polls <- function() {
    data_url <- 'https://filipvanlaenen.github.io/eopaod/pt.csv'
    download.file(data_url, "data/pt.csv")
}

load_pt_polls <- function() {
    tmp <- read.csv("data/pt.csv") %>%
        gather(key = "Partido",value = "Proporção",names(CONVERSION_LIST)) %>%
        select(
            Partido = Partido,
            Total = Sample.Size,
            Data = Fieldwork.End,
            Proporção = Proporção,
            Sondagem = Polling.Firm
            ) %>%
        mutate(Partido = unlist(CONVERSION_LIST[Partido])) %>%
        mutate(Proporção = gsub('%','',Proporção) %>% 
                   as.character %>%
                   as.numeric) %>%
        mutate(Proporção = Proporção / 100)
    return(tmp)
}

kalman_filter <- function(values,dates) {
    dates <- as.POSIXct(as.Date(dates))
    irregular_time_series <- irts(dates,values)
    inter_ts <- approx.irts(irregular_time_series,
                            time = seq(min(dates),max(dates),by = 'months'))
    inter_values <- ts(inter_ts$value,frequency = 0.25)

    dlm_build <- function(par) {
        dlmModPoly(1,dV = par[1],dW = par[2])
    }
    mle_solution <- dlmMLE(inter_values,rep(1,2),dlm_build,method = "Nelder-Mead")
    dlm_model <- dlm_build(mle_solution$par)
    filtered_solution <- dlmFilter(inter_values, dlm_model)
    smoothed_solution <- dlmSmooth(filtered_solution)
    
    return(list(values = smoothed_solution$s,dates = inter_ts$time))
}

statistical_test <- function(p1,p2,n1,n2) {
    M <- matrix(
        c(round(p1*n1),round(n1-p1*n1),
          round(p2*n2),round(n2-p2*n2)),ncol=2)
    if (!any(unlist(is.na(M)))) {
        return(fisher.test(M)$p.val)
    } else {
        return(NA)
    }
}

make_plots <- function(values_for_plot,log_plot=T,log_plot_all=T) {
    values_for_plot <- values_for_plot %>%
        group_by(Partido) %>% 
        arrange(Partido,Data) %>% 
        mutate(PrevP = c(NA,Proporção[-length(Proporção)]),
               PrevT = c(NA,Total[-length(Total)])) %>% 
        rowwise() %>%
        mutate(p.val.prev = statistical_test(
            Proporção,
            PrevP,
            Total,
            PrevT
        )) %>% 
        mutate(Significante = ifelse(p.val.prev < 0.05,"Sim","Não")) %>%
        mutate(Significante = ifelse(is.na(Significante),"Não",Significante))
    poll_plot <- values_for_plot %>% 
        subset(!is.na(Proporção)) %>% 
        ggplot(aes(x = Data,y = Proporção,
                   ymin = Mínimo,ymax = Máximo,
                   colour = Partido,fill = Partido,
                   shape = Significante,
                   group = Partido)) + 
        geom_point(size = 0.4) + 
        geom_line(size = 0.2) +
        geom_ribbon(alpha = 0.3,color = NA) +
        facet_wrap(~ Partido,ncol = 3) + 
        theme_minimal(base_size = 7) + 
        scale_colour_manual(values = PALETTE,guide=F) + 
        scale_fill_manual(values = PALETTE,guide=F) + 
        scale_shape_manual(values = list(Sim = 17,Não = 16),
                           name = "A mudança para este valor é estatisticamente significante?") +
        scale_x_date(date_labels = "%b\n%Y") + 
        ggtitle("Progresso dos partidos para cada sondagem") + 
        theme(legend.position = "bottom",
              panel.grid.minor.y = element_blank())
    if (log_plot == T) {
        poll_plot <- poll_plot + 
            scale_y_continuous(trans='log10')
    }
    
    poll_plot_all <- values_for_plot %>% 
        subset(!is.na(Proporção)) %>% 
        ggplot(aes(x = Data,y = Proporção,
                   ymin = Mínimo,ymax = Máximo,
                   colour = Partido,fill = Partido,
                   shape = Significante,
                   group = Partido)) + 
        geom_point(size = 0.8) + 
        geom_line(size = 0.3) +
        geom_ribbon(alpha = 0.2,color = NA) +
        theme_minimal(base_size = 7) + 
        scale_colour_manual(values = PALETTE,name = NULL) + 
        scale_fill_manual(values = PALETTE,guide=F) + 
        scale_shape_manual(values = list(Sim = 17,Não = 16),
                           name = "A mudança para este valor é estatisticamente significante?") +
        scale_x_date(date_labels = "%b\n%Y") + 
        ggtitle("Progresso dos partidos para cada sondagem") +
        theme(legend.position = "bottom")
    if (log_plot_all == T) {
        poll_plot_all <- poll_plot_all + 
            scale_y_continuous(trans='log10')
    }
    colour_legend_poll_plot_all <- get_legend(poll_plot_all+scale_shape(guide=F))
    poll_plot_all <- plot_grid(poll_plot_all+scale_colour_manual(values = PALETTE,guide = F),
                               colour_legend_poll_plot_all,
                               rel_heights = c(0.9,0.1),
                               ncol=1)
    poll_plot_bars <- values_for_plot %>% 
        ggplot(aes(x = Partido,y = Proporção,
                   ymin = Mínimo,ymax = Máximo,
                   fill = Partido)) + 
        geom_bar(stat = "identity",
                 position = "dodge",
                 color = 'black',
                 size = 0.2) + 
        geom_text(aes(label = round(Proporção,2),y = Máximo),
                  vjust = -0.4,size = 1.4) +
        geom_errorbar(colour = 'black',size=0.2,width = 0.5) + 
        facet_wrap(~ plot_facet,ncol = 3,scales = "free_x") + 
        theme_minimal(base_size = 7) +
        scale_fill_manual(values = PALETTE,guide = F) + 
        scale_y_continuous(expand = c(0,0.05)) +
        ggtitle("Comparação entre partidos por sondagem") + 
        ggpubr::rotate_x_text() + 
        theme(legend.position = "bottom",
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank())
    poll_plot_variations <- values_for_plot %>%
        group_by(Partido) %>%
        arrange(Data) %>%
        mutate(var = c(NA,diff(Proporção)),
               var_error = sqrt(Erro^2 + c(NA,Erro[-1])^2)) %>%
        mutate(plot_facet = reorder(gsub(' ','\n',plot_facet),as.Date(Data))) %>% 
        subset(!is.na(var)) %>% 
        ggplot(aes(x = Data,y = var,ymin = var - var_error,ymax = var + var_error,
                   colour = Partido)) + 
        geom_hline(yintercept = 0,size = 0.2,alpha = 1) +
        geom_point(size = 0.2,
                   alpha=0.5) +
        geom_linerange(size=0.1,
                       alpha = 0.5) + 
        facet_wrap(~ Partido,ncol = 1,scales = "free_x") + 
        theme_minimal(base_size = 7) +
        scale_colour_manual(values = PALETTE,guide = F) + 
        scale_x_date(date_labels = "%b\n%Y") + 
        scale_y_continuous(breaks = seq(-1,1,by=0.1),expand = c(0.03,0.03)) +
        ggtitle("Variação observada entre sondagens") + 
        ggpubr::rotate_x_text() + 
        theme(legend.position = "bottom",
              panel.grid = element_blank(),
              panel.border = element_rect(fill=NA,colour='black',size=0.2),
              axis.ticks.x = element_line(size = 0.2),
              axis.text = element_text(size = 4)) + 
        xlab("Sondagens") + 
        ylab("Variação")
    
    poll_plot_streamgraph <- streamgraph(values_for_plot,
                                         "Partido","Proporção", "Data", 
                                         interactive=TRUE) %>% 
        sg_axis_x(4, "month","%b\n%Y") %>% 
        sg_fill_manual(values = PALETTE)
    
    return(list(poll_plot = poll_plot,
                poll_plot_all = poll_plot_all,
                poll_plot_bars = poll_plot_bars,
                poll_plot_variations = poll_plot_variations,
                poll_plot_streamgraph = poll_plot_streamgraph))
}

make_plots_smooth <- function(values_for_plot,log_plot=F,log_plot_all=F) {
    values_for_plot <- values_for_plot %>%
        group_by(Partido) %>% 
        arrange(Partido,Data) %>% 
        mutate(PrevP = c(NA,Proporção[-length(Proporção)]),
               PrevT = c(NA,Total[-length(Total)])) %>% 
        rowwise() %>%
        mutate(p.val.prev = statistical_test(
            Proporção,
            PrevP,
            Total,
            PrevT
        )) %>% 
        mutate(Significante = ifelse(p.val.prev < 0.05,"Sim","Não")) %>%
        mutate(Significante = ifelse(is.na(Significante),"Não",Significante))

    smooth_values_for_plot <- lapply(
        unique(values_for_plot$Partido),
        FUN = function(x) {
            tmp <- values_for_plot %>% 
                subset(Partido == x) %>%
                subset(!is.na(Proporção))
            values <- tmp$Proporção
            dates <- tmp$Data
            smooth_values <- kalman_filter(values,dates)
            data.frame(
                SmoothValues = smooth_values$values[-1],
                Data = smooth_values$dates,
                Partido = x
            ) %>%
                mutate(Data = as.Date(Data)) %>% 
                subset(Data >= min(tmp$Data)) %>% 
                return
        }) %>%
        do.call(what = rbind) %>%
        as.data.frame() %>%
        mutate(Data = as.Date(Data))
    
    poll_plot <- values_for_plot %>% 
        subset(!is.na(Proporção)) %>% 
        ggplot(aes(x = Data,y = Proporção,
                   colour = Partido,fill = Partido,
                   group = Partido)) + 
        geom_point(size = 0.4,aes(shape = Significante),alpha = 0.45) + 
        geom_line(size = 0.4,
                  data = smooth_values_for_plot,
                  inherit.aes = F,
                  aes(x = Data,
                      colour = Partido,
                      y = SmoothValues),
                  na.rm = T) +
        facet_wrap(~ Partido,ncol = 3) + 
        theme_minimal(base_size = 7) + 
        scale_colour_manual(values = PALETTE,guide=F) + 
        scale_fill_manual(values = PALETTE,guide=F) + 
        scale_shape_manual(values = list(Sim = 17,Não = 16),
                           name = "A mudança para este valor é estatisticamente significante?") +
        scale_x_date(date_labels = "%b\n%Y") + 
        ggtitle("Progresso dos partidos para cada sondagem") + 
        theme(legend.position = "bottom",
              panel.grid.minor.y = element_blank())
    if (log_plot == T) {
        poll_plot <- poll_plot + 
            scale_y_continuous(trans='log10')
    }
    
    poll_plot_all <- values_for_plot %>% 
        subset(!is.na(Proporção)) %>% 
        ggplot(aes(x = Data,y = Proporção,
                   colour = Partido,fill = Partido,
                   group = Partido)) + 
        geom_point(size = 0.5,aes(shape = Significante),alpha = 0.4) + 
        geom_line(size = 0.6,
                  data = smooth_values_for_plot,
                  inherit.aes = F,
                  aes(x = Data,
                      colour = Partido,
                      y = SmoothValues),
                  na.rm = T) +
        theme_minimal(base_size = 7) + 
        scale_colour_manual(values = PALETTE,name = NULL) + 
        scale_fill_manual(values = PALETTE,guide=F) + 
        scale_shape_manual(values = list(Sim = 17,Não = 16),
                           name = "A mudança para este valor é estatisticamente significante?") +
        scale_x_date(date_labels = "%b\n%Y") + 
        ggtitle("Progresso dos partidos para cada sondagem") +
        theme(legend.position = "bottom")
    if (log_plot_all == T) {
        poll_plot_all <- poll_plot_all + 
            scale_y_continuous(trans='log10')
    }
    
    colour_legend_poll_plot_all <- get_legend(poll_plot_all +
                                                  scale_shape(guide=F) + 
                                                  guides(colour = guide_legend(nrow = 2)))
    poll_plot_all <- plot_grid(poll_plot_all+scale_colour_manual(values = PALETTE,guide = F),
                               colour_legend_poll_plot_all,
                               rel_heights = c(0.9,0.1),
                               ncol=1)
    
    poll_plot_streamgraph <- streamgraph(smooth_values_for_plot,
                                         "Partido","SmoothValues", "Data", 
                                         interactive=TRUE) %>% 
        sg_axis_x(4, "month","%b\n%Y") %>% 
        sg_fill_manual(values = PALETTE)
    
    return(list(poll_plot = poll_plot,
                poll_plot_all = poll_plot_all,
                poll_plot_streamgraph = poll_plot_streamgraph,
                data = smooth_values_for_plot))
}

select_values_for_plot <- function(values_for_plot) {
    if (length(unique(values_for_plot$Partido)) > 12) {
        values_for_plot <- values_for_plot %>% 
            group_by(Partido) %>% 
            mutate(freq = length(unique(Data)))
        if (max(values_for_plot$freq) > 1) { 
            values_for_plot <- values_for_plot %>%
                subset(freq > 1)
        }
    }
    if (length(unique(values_for_plot$Partido)) > 12) {
        values_for_plot <- values_for_plot %>% 
            group_by(Partido) %>% 
            mutate(M = max(Proporção))
        if (max(values_for_plot$freq) > 1) { 
            values_for_plot <- values_for_plot %>%
                subset(M >= sort(M,decreasing=T)[12])
        }
    }
    return(values_for_plot)
}

calculate_error <- function(P,N,C = 0.95) {
    O <- qnorm(C) * sqrt(P * (1-P)/N)
    return(O)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    observeEvent(
        input$calc_proportion | input$calc_sample_size | input$calc_confidence,
        {
            err <- calculate_error(input$calc_proportion/100,
                                   input$calc_sample_size,
                                   input$calc_confidence/100)
            output_table <- data.frame(
                A = sprintf("%.2f%%",err * 100),
                B = max(sprintf("%.2f%%",input$calc_proportion - err * 100),0),
                C = max(0,sprintf("%.2f%%",input$calc_proportion + err * 100))
            )
            colnames(output_table) <- c(
                "Erro",
                "Intervalo (mínimo)",
                "Intervalo (máximo)"
            )
            output$calc_output <- renderTable(
                output_table
            )
        }
    )
    
    observeEvent(
        input$compare_proportions,
        {
            err_1 <- calculate_error(input$calc_proportion_1/100,
                                     input$calc_sample_size_1,
                                     input$calc_confidence_2/100)
            err_2 <- calculate_error(input$calc_proportion_2/100,
                                     input$calc_sample_size_1,
                                     input$calc_confidence_2/100)
            p1 <- input$calc_proportion_1/100
            p2 <- input$calc_proportion_2/100
            n1 <- input$calc_sample_size_1
            n2 <- input$calc_sample_size_1
            d <- p1-p2
            p <- (p1 * n1 + p2 * n2) / (n1 + n2)
            se <- sqrt(p*(1-p)*(1/n1 + 1/n2))
            z <- d / se
            p_value <- pnorm(abs(z),lower.tail = F)*2
            dr <- c(d - qnorm(input$calc_confidence_2/100)*se,
                    d + qnorm(input$calc_confidence_2/100)*se)
            
            output_table <- data.frame(
                Z = c("1","2"),
                A = c(sprintf("%.2f%%",err_1 * 100),sprintf("%.2f%%",err_2 * 100)),
                B = c(max(sprintf("%.2f%%",input$calc_proportion_1 - err_1 * 100),0),
                      max(sprintf("%.2f%%",input$calc_proportion_2 - err_2 * 100),0)),
                C = c(max(sprintf("%.2f%%",input$calc_proportion_1 + err_1 * 100),0),
                      max(sprintf("%.2f%%",input$calc_proportion_2 + err_2 * 100),0))
            )
            colnames(output_table) <- c("ID","Erro","Intervalo (mínimo)","Intervalo (máximo)")
            output$calc_output_compare <- renderTable(output_table)
            P <- ggplot(mutate(output_table,P = c(input$calc_proportion_1,input$calc_proportion_2)),
                        aes(x = ID,y = P,
                            ymin = as.numeric(gsub("%","",`Intervalo (mínimo)`)),
                            ymax = as.numeric(gsub("%","",`Intervalo (máximo)`)))) + 
                geom_point() + 
                geom_linerange() +
                theme_minimal() +
                xlab("") + 
                ylab("Percentagem (%)") +
                ggtitle(sprintf("diferença das proporções entre %.2f%% e %.2f%%",dr[1]*100,dr[2]*100),
                        sprintf("p-value = %.5f",p_value))
            output$compare_plot <- renderPlot(P)
        }
    )
    
    dir.create('data',showWarnings = F)
    download_pt_polls()
    values <- reactiveValues()
    values$dt <- data.frame(
        Partido = character(0),
        Total = numeric(0),
        Data = character(0),
        Sondagem = character(0),
        Proporção = double(0),
        Mínimo = numeric(0),
        Máximo = numeric(0),
        Erro = numeric(0)
    )
    df <- load_pt_polls() %>% 
        mutate(Erro = round(qnorm(0.95)*sqrt(Proporção*(1-Proporção)/Total),3)) %>%
        mutate(Mínimo = Proporção - Erro) %>%
        mutate(Máximo = Proporção + Erro) %>%
        select(
            Partido,Total,Data,Sondagem,Proporção,Mínimo,Máximo,Erro
        )
    isolate(values$dt <- bind_rows(values$dt,df))
    isolate({
        values_for_plot <- values$dt %>% 
            mutate(Data = as.Date(Data),
                   Proporção = as.numeric(Proporção),
                   Mínimo = as.numeric(Mínimo),
                   Máximo = as.numeric(Máximo)) %>% 
            mutate(Partido = reorder(Partido,Proporção)) %>% 
            mutate(plot_facet = sprintf('%s (%s)',Sondagem,Data)) %>%
            mutate(plot_facet = reorder(plot_facet,Data))
        
        values_for_plot <- select_values_for_plot(values_for_plot)
        plots <- make_plots(values_for_plot)
        heights <- list(
            poll_plot = min(
                (floor(length(unique(values_for_plot$Partido)) / 3)+1) * 200),
            poll_plot_all = 700,
            poll_plot_bars = min(
                (floor(length(unique(paste(values_for_plot$Sondagem,
                                           values_for_plot$Data))) / 3)+1) * 200)+100,
            poll_plot_variations = min(
                (floor(length(unique(values_for_plot$Partido)) / 3)+1) * 150)
        )
        plots <- make_plots(values_for_plot)
        output$poll_plot <- renderPlot(
            plots$poll_plot,
            res = RES,
            height = heights$poll_plot)
        output$poll_plot_all <- renderPlot(
            plots$poll_plot_all,
            res = RES,
            height = heights$poll_plot_all)
        output$poll_plot_bars <- renderPlot(
            plots$poll_plot_bars,
            res = RES,
            height = heights$poll_plot_bars)
        output$poll_plot_variations <- renderPlot(
            plots$poll_plot_variations,
            res = RES,
            height = heights$poll_plot_variations)
    }
    )
    
    observeEvent(input$csv_file,{
        df <- read.csv(input$csv_file$datapath,header = T) %>% 
            as.data.frame()
        colnames(df) <- c("Partido","Total","Data","Sondagem","Proporção")
        df <- df %>% 
            mutate(Erro = qnorm(input$confidence/100)*sqrt(Proporção*(1-Proporção)/Total)) %>%
            mutate(Mínimo = Proporção - Erro) %>%
            mutate(Máximo = Proporção + Erro) %>%
            select(
                Partido,Total,Data,Sondagem,Proporção,Mínimo,Máximo,Erro
            )
        values$dt <- bind_rows(values$dt,df)
        isolate(values$dt <- values$dt %>% distinct)
    })
    observeEvent(input$confidence,{
        df <- values$dt %>%
            mutate(Erro = round(
                qnorm(input$confidence/100)*sqrt(Proporção*(1-Proporção)/Total),
                3)) %>%
            mutate(Mínimo = Proporção - Erro,
                   Máximo = Proporção + Erro)
        values$dt <- df
    })
    
    observeEvent(input$add,{
        prop <- input$prop
        std_error <- sqrt(prop * (1 - prop) / input$total)
        std_error <- round(qnorm(0.95) * std_error,3)
        m <- prop - std_error 
        M <- prop + std_error 
        m <- ifelse(m < 0,0,m)
        tmp <- as.character(input$poll_date)
        isolate(values$dt[nrow(values$dt) + 1,] <- list(input$party, 
                                                        input$total,
                                                        tmp,
                                                        input$poll_id,
                                                        prop,
                                                        m,
                                                        M,
                                                        std_error))
        isolate(values$dt <- values$dt %>% distinct)
    })
    observeEvent(input$clear,{
        values$dt <- data.frame(
            Partido = character(0),
            Total = numeric(0),
            Data = character(0),
            Sondagem = character(0),
            Proporção = double(0),
            Mínimo = numeric(0),
            Máximo = numeric(0),
            Erro = numeric(0)
        )
        output$poll_plot_bars <- renderPlot(ggplot()+theme_minimal())
        output$poll_plot_all <- renderPlot(ggplot()+theme_minimal())
        output$poll_plot <- renderPlot(ggplot()+theme_minimal())
        output$poll_plot_variations <- renderPlot(ggplot()+theme_minimal())
    })
    
    observeEvent({
        input$add | input$csv_file | input$delete_rows | 
            input$confidence | input$log_plot | input$log_plot_all |
            input$kalman_trigger
        },{
        isolate({
            values_for_plot <- values$dt %>% 
                mutate(Data = as.Date(Data),
                       Proporção = as.numeric(Proporção),
                       Mínimo = as.numeric(Mínimo),
                       Máximo = as.numeric(Máximo)) %>% 
                mutate(Partido = reorder(Partido,Proporção)) %>% 
                mutate(plot_facet = sprintf('%s (%s)',Sondagem,Data)) %>%
                mutate(plot_facet = reorder(plot_facet,Data))
            
            values_for_plot <- select_values_for_plot(values_for_plot)
            if (nrow(values_for_plot) > 0) {
                heights <- list(
                    poll_plot = min(
                        (floor(length(unique(values_for_plot$Partido)) / 3)+1) * 200),
                    poll_plot_all = 700,
                    poll_plot_bars = min(
                        (floor(length(unique(paste(values_for_plot$Sondagem,
                                                   values_for_plot$Data))) / 3)+1) * 200)+100,
                    poll_plot_variations = min(
                        (floor(length(unique(values_for_plot$Partido)) / 1)+1) * 200)
                )
                plots <- make_plots(values_for_plot,input$log_plot,input$log_plot_all)
                output$poll_plot_bars <- renderPlot(
                    plots$poll_plot_bars,
                    res = RES,
                    height = heights$poll_plot_bars)
                output$poll_plot_variations <- renderPlot(
                    plots$poll_plot_variations,
                    res = RES,
                    height = heights$poll_plot_variations)
                if (input$kalman_trigger == F) {
                    output$poll_plot <- renderPlot(
                        plots$poll_plot,
                        res = RES,
                        height = heights$poll_plot)
                    output$poll_plot_all <- renderPlot(
                        plots$poll_plot_all,
                        res = RES,
                        height = heights$poll_plot_all)
                    output$poll_plot_streamgraph <- renderStreamgraph(
                        plots$poll_plot_streamgraph)
                } else {
                    smooth_plots <- make_plots_smooth(
                        values_for_plot,input$log_plot,input$log_plot_all)
                    output$poll_plot <- renderPlot(
                        smooth_plots$poll_plot,
                        res = RES,
                        height = heights$poll_plot)
                    output$poll_plot_all <- renderPlot(
                        smooth_plots$poll_plot_all,
                        res = RES,
                        height = heights$poll_plot_all)
                    output$poll_plot_streamgraph <- renderStreamgraph(
                        smooth_plots$poll_plot_streamgraph)
                }
            } else {

            }
        }
        )
    })
    
    observeEvent(input$delete_rows,{
        isolate({
            row_idxs <- str_split(input$rows_to_delete,pattern = ',') %>% 
                unlist() %>% 
                gsub(pattern = " ",replacement = "") %>%
                as.numeric() %>%
                na.omit
            row_idxs <- row_idxs[row_idxs <= nrow(values$dt)]
            if (length(row_idxs) > 0) {
                values$dt <- values$dt[-row_idxs,]
                rownames(values$dt) <- seq(1:nrow(values$dt))
            }
        })
    })
    
    output$data_table <- renderDataTable(values$dt,
                                         selection = 'none',
                                         editable = 'none')
    
    observeEvent(input$collapsable_menu,{
        complete_values <- values$dt %>% 
            group_by(Partido) %>% 
            arrange(Partido,Data) %>% 
            mutate(PrevP = c(NA,Proporção[-length(Proporção)]),
                   PrevT = c(NA,Total[-length(Total)])) %>% 
            rowwise() %>%
            mutate(`p-value para ponto anterior` = statistical_test(
                Proporção,
                PrevP,
                Total,
                PrevT
            )) %>% 
            mutate(Significante = ifelse(`p-value para ponto anterior` < 0.05,
                                         "Sim","Não")) %>%
            mutate(Significante = ifelse(is.na(Significante),
                                         "Não",Significante)) %>%
            group_by(Partido) %>%
            arrange(Data) %>%
            mutate(Variação = c(NA,diff(Proporção)),
                   `Erro na variação` = sqrt(Erro^2 + c(NA,Erro[-1])^2)) %>%
            select(-PrevP,-PrevT)
        output$download_simple <- downloadHandler(
            filename = function() {"sondagens_simples.csv"},
            content = function(file) {
                write.csv(select(
                    values$dt,
                    Partido,Total,Data,Sondagem,Proporção), 
                    fileEncoding = "UTF-8",file, row.names = FALSE)
            }
        )
        output$download_with_error <- downloadHandler(
            filename = function() {"sondagens_com_erros.csv"},
            content = function(file) {
                write.csv(values$dt, file,
                          fileEncoding = "UTF-8", row.names = FALSE)
            }
        )
        output$download_complete <- downloadHandler(
            filename = function() {"sondagens_completo.csv"},
            content = function(file) {
                write.csv(complete_values, 
                          fileEncoding = "UTF-8",
                          file, row.names = FALSE)
            }
        )
    }
    )
})

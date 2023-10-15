library(shiny)
library(tidyverse)
library(DT)
library(shinyBS)
library(cowplot)
library(dlm)
library(tseries)
library(streamgraph)

source("text_pt.R")

PALETTE <- c("#332288","#6699CC","#88CCEE","#44AA99","#117733",
             "#999933","#DDCC77","#661100","#CC6677","#AA4466",
             "#882255","#AA4499")
RES <- 200

CONVERSION_LIST <- list(
    PSD = 'PSD',
    PS = 'PS',
    BE = 'BE',
    CDS.PP = 'CDS-PP',
    CDU = 'CDU',
    PAN = 'PAN',
    L = 'LIVRE',
    IL = 'IL',
    A = 'A',
    CH = 'CH',
    Other = 'Outros'
)

yes_no_list <- list(Sim = 17,Não = 16)
names(yes_no_list) <- c(
    website_legends$yes,
    website_legends$no
)

download_pt_polls <- function() {
    data_url <- 'https://storage.googleapis.com/asapop-website-20220812/_csv/pt.csv'
    download.file(data_url, "data/pt.csv")
}

load_pt_polls <- function() {
    tmp <- read.csv("data/pt.csv") %>%
        gather(key = "PoliticalParty",value = "Proportion",names(CONVERSION_LIST)) %>%
        select(
            PoliticalParty = PoliticalParty,
            Total = Sample.Size,
            Date = Fieldwork.End,
            Proportion = Proportion,
            PollID = Polling.Firm
            ) %>%
        mutate(PoliticalParty = unlist(CONVERSION_LIST[PoliticalParty])) %>%
        mutate(Proportion = gsub('%','',Proportion) %>% 
                   as.character %>%
                   as.numeric) %>%
        mutate(Total = Total %>% 
               as.character %>%
               as.numeric) %>%
        subset(!is.na(Proportion)) %>%
        subset(!is.na(Total)) %>%
        mutate(Proportion = Proportion / 100)
    return(tmp)
}

kalman_filter <- function(values,dates) {
    dates <- as.POSIXct(as.Date(dates))
    irregular_time_series <- irts(dates,values)
    inter_ts <- approx.irts(irregular_time_series,
                            time = seq(min(dates),max(dates),by = 'months'))
    inter_values <- ts(inter_ts$value,frequency = 1)

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
        group_by(PoliticalParty) %>% 
        arrange(PoliticalParty,Date) %>% 
        mutate(PrevP = c(NA,Proportion[-length(Proportion)]),
               PrevT = c(NA,Total[-length(Total)])) %>% 
        rowwise() %>%
        mutate(p.val.prev = statistical_test(
            Proportion,
            PrevP,
            Total,
            PrevT
        )) %>% 
        mutate(Significante = ifelse(p.val.prev < 0.05,website_legends$yes,website_legends$no)) %>%
        mutate(Significante = ifelse(is.na(Significante),website_legends$no,Significante))
    poll_plot <- values_for_plot %>% 
        subset(!is.na(Proportion)) %>% 
        ggplot(aes(x = Date,y = Proportion,
                   ymin = Minimum,ymax = Maximum,
                   colour = PoliticalParty,fill = PoliticalParty,
                   shape = Significante,
                   group = PoliticalParty)) + 
        geom_point(size = 0.4) + 
        geom_line(size = 0.2) +
        geom_ribbon(alpha = 0.3,color = NA) +
        facet_wrap(~ PoliticalParty,ncol = 3) + 
        theme_minimal(base_size = 7) + 
        scale_colour_manual(values = PALETTE,guide=F) + 
        scale_fill_manual(values = PALETTE,guide=F) + 
        scale_shape_manual(values = yes_no_list,
                           name = website_explanations$is_change_significant) +
        scale_x_date(date_labels = "%b\n%Y") + 
        ggtitle(website_explanations$party_progress) + 
        theme(legend.position = "bottom",
              panel.grid.minor.y = element_blank(),
              legend.key.size = unit(0.5,"line")) +
        xlab(website_legends$date) + 
        ylab(website_legends$proportion)
    
    if (log_plot == T) {
        poll_plot <- poll_plot + 
            scale_y_continuous(trans='log10')
    }

    poll_plot_all <- values_for_plot %>% 
        subset(!is.na(Proportion)) %>% 
        ggplot(aes(x = Date,y = Proportion,
                   ymin = Minimum,ymax = Maximum,
                   colour = PoliticalParty,fill = PoliticalParty,
                   shape = Significante,
                   group = PoliticalParty)) + 
        geom_point(size = 0.8) + 
        geom_line(size = 0.3) +
        geom_ribbon(alpha = 0.2,color = NA) +
        theme_minimal(base_size = 7) + 
        scale_colour_manual(values = PALETTE,name = NULL) + 
        scale_fill_manual(values = PALETTE,guide=F) + 
        scale_shape_manual(values = yes_no_list,
                           name = website_explanations$is_change_significant) +
        scale_x_date(date_labels = "%b\n%Y") + 
        ggtitle(website_explanations$party_progress) +
        theme(legend.position = "bottom",
              legend.key.size = unit(0.5,"line")) +
        xlab(website_legends$date) + 
        ylab(website_legends$proportion)
    
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
        ggplot(aes(x = PoliticalParty,y = Proportion,
                   ymin = Minimum,ymax = Maximum,
                   fill = PoliticalParty)) + 
        geom_bar(stat = "identity",
                 position = "dodge",
                 color = 'black',
                 size = 0.2) + 
        geom_text(aes(label = round(Proportion,2),y = Maximum),
                  vjust = -0.4,size = 1.4) +
        geom_errorbar(colour = 'black',size=0.2,width = 0.5) + 
        facet_wrap(~ plot_facet,ncol = 3,scales = "free_x") + 
        theme_minimal(base_size = 7) +
        scale_fill_manual(values = PALETTE,guide = F) + 
        scale_y_continuous(expand = c(0,0.05)) +
        ggtitle(website_explanations$compare_parties) + 
        ggpubr::rotate_x_text() + 
        theme(legend.position = "bottom",
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.key.size = unit(0.5,"line")) +
        xlab(website_legends$political_party) + 
        ylab(website_legends$proportion)
    
    poll_plot_variations <- values_for_plot %>%
        group_by(PoliticalParty) %>%
        arrange(Date) %>%
        mutate(var = c(NA,diff(Proportion)),
               var_error = sqrt(Error^2 + c(NA,Error[-1])^2)) %>%
        mutate(plot_facet = reorder(gsub(' ','\n',plot_facet),as.Date(Date))) %>% 
        subset(!is.na(var)) %>% 
        ggplot(aes(x = Date,y = var,ymin = var - var_error,ymax = var + var_error,
                   colour = PoliticalParty)) + 
        geom_hline(yintercept = 0,size = 0.2,alpha = 1) +
        geom_point(size = 0.2,
                   alpha=0.5) +
        geom_linerange(size=0.1,
                       alpha = 0.5) + 
        facet_wrap(~ PoliticalParty,ncol = 1,scales = "free_x") + 
        theme_minimal(base_size = 7) +
        scale_colour_manual(values = PALETTE,guide = F) + 
        scale_x_date(date_labels = "%b\n%Y") + 
        scale_y_continuous(breaks = seq(-1,1,by=0.1),expand = c(0.03,0.03)) +
        ggtitle(website_explanations$variation_observed) + 
        ggpubr::rotate_x_text() + 
        theme(legend.position = "bottom",
              panel.grid = element_blank(),
              panel.border = element_rect(fill=NA,colour='black',size=0.2),
              axis.ticks.x = element_line(size = 0.2),
              axis.text = element_text(size = 4),
              legend.key.size = unit(0.5,"line")) + 
        xlab(website_legends$poll_id) + 
        ylab(website_legends$variation)
    
    poll_plot_streamgraph <- streamgraph(values_for_plot,
                                         "PoliticalParty","Proportion","Date", 
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
        group_by(PoliticalParty) %>% 
        arrange(PoliticalParty,Date) %>% 
        mutate(PrevP = c(NA,Proportion[-length(Proportion)]),
               PrevT = c(NA,Total[-length(Total)])) %>% 
        rowwise() %>%
        mutate(p.val.prev = statistical_test(
            Proportion,
            PrevP,
            Total,
            PrevT
        )) %>% 
        mutate(Significante = ifelse(p.val.prev < 0.05,website_legends$yes,website_legends$no)) %>%
        mutate(Significante = ifelse(is.na(Significante),website_legends$no,Significante))

    smooth_values_for_plot <- lapply(
        unique(values_for_plot$PoliticalParty),
        FUN = function(x) {
            tmp <- values_for_plot %>% 
                subset(PoliticalParty == x) %>%
                subset(!is.na(Proportion))
            values <- tmp$Proportion
            dates <- tmp$Date
            smooth_values <- kalman_filter(values,dates)
            data.frame(
                SmoothValues = smooth_values$values[-1],
                Date = smooth_values$dates,
                PoliticalParty = x
            ) %>%
                mutate(Date = as.Date(Date)) %>% 
                subset(Date >= min(tmp$Date)) %>% 
                return
        }) %>%
        do.call(what = rbind) %>%
        as.data.frame() %>%
        mutate(Date = as.Date(Date))
    
    poll_plot <- values_for_plot %>% 
        subset(!is.na(Proportion)) %>% 
        ggplot(aes(x = Date,y = Proportion,
                   colour = PoliticalParty,fill = PoliticalParty,
                   group = PoliticalParty)) + 
        geom_point(size = 0.4,aes(shape = Significante),alpha = 0.45) + 
        geom_line(size = 0.4,
                  data = smooth_values_for_plot,
                  inherit.aes = F,
                  aes(x = Date,
                      colour = PoliticalParty,
                      y = SmoothValues),
                  na.rm = T) +
        facet_wrap(~ PoliticalParty,ncol = 3) + 
        theme_minimal(base_size = 7) + 
        scale_colour_manual(values = PALETTE,guide=F) + 
        scale_fill_manual(values = PALETTE,guide=F) + 
        scale_shape_manual(values = yes_no_list,
                           name =  website_explanations$is_change_significant) +
        scale_x_date(date_labels = "%b\n%Y") + 
        ggtitle(website_explanations$party_progress) + 
        theme(legend.position = "bottom",
              panel.grid.minor.y = element_blank(),
              legend.key.size = unit(0.5,"line")) +
        xlab(website_legends$date) +
        ylab(website_legends$proportion)
    
    if (log_plot == T) {
        poll_plot <- poll_plot + 
            scale_y_continuous(trans='log10')
    }
    
    poll_plot_all <- values_for_plot %>% 
        subset(!is.na(Proportion)) %>% 
        ggplot(aes(x = Date,y = Proportion,
                   colour = PoliticalParty,fill = PoliticalParty,
                   group = PoliticalParty)) + 
        geom_point(size = 0.5,aes(shape = Significante),alpha = 0.4) + 
        geom_line(size = 0.6,
                  data = smooth_values_for_plot,
                  inherit.aes = F,
                  aes(x = Date,
                      colour = PoliticalParty,
                      y = SmoothValues),
                  na.rm = T) +
        theme_minimal(base_size = 7) + 
        scale_colour_manual(values = PALETTE,name = NULL) + 
        scale_fill_manual(values = PALETTE,guide=F) + 
        scale_shape_manual(values = yes_no_list,
                           name = website_explanations$is_change_significant) +
        scale_x_date(date_labels = "%b\n%Y") + 
        ggtitle(website_explanations$party_progress) +
        theme(legend.position = "bottom",
              legend.key.size = unit(0.5,"line")) +
        xlab(website_legends$date) +
        ylab(website_legends$proportion)
    
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
                                         "PoliticalParty","SmoothValues", "Date", 
                                         interactive=TRUE) %>% 
        sg_axis_x(4, "month","%b\n%Y") %>% 
        sg_fill_manual(values = PALETTE)
    
    return(list(poll_plot = poll_plot,
                poll_plot_all = poll_plot_all,
                poll_plot_streamgraph = poll_plot_streamgraph,
                data = smooth_values_for_plot))
}

select_values_for_plot <- function(values_for_plot) {
    if (length(unique(values_for_plot$PoliticalParty)) > 12) {
        values_for_plot <- values_for_plot %>% 
            group_by(PoliticalParty) %>% 
            mutate(freq = length(unique(Date)))
        if (max(values_for_plot$freq) > 1) { 
            values_for_plot <- values_for_plot %>%
                subset(freq > 1)
        }
    }
    if (length(unique(values_for_plot$PoliticalParty)) > 12) {
        values_for_plot <- values_for_plot %>% 
            group_by(PoliticalParty) %>% 
            mutate(M = max(Proportion))
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
                website_legends$error,
                website_legends$interval_minimum,
                website_legends$interval_maximum
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
            colnames(output_table) <- c("ID",website_legends$error,
                                        "interval_min","interval_max")
            output$calc_output_compare <- renderTable(output_table)
            P <- ggplot(mutate(output_table,P = c(input$calc_proportion_1,input$calc_proportion_2)),
                        aes(x = ID,y = P,
                            ymin = as.numeric(gsub("%","",interval_min)),
                            ymax = as.numeric(gsub("%","",interval_max)))) + 
                geom_point() + 
                geom_linerange() +
                theme_minimal() +
                xlab("") + 
                ylab(website_legends$proportion_percent) +
                ggtitle(sprintf("dif. prop = [%.2f%%,%.2f%%]",dr[1]*100,dr[2]*100),
                        sprintf("p-value = %.5f",p_value))
            output$compare_plot <- renderPlot(P)
        }
    )
    
    dir.create('data',showWarnings = F)
    download_pt_polls()
    values <- reactiveValues()
    values$dt <- data.frame(
        PoliticalParty = character(0),
        Total = numeric(0),
        Date = character(0),
        PollID = character(0),
        Proportion = double(0),
        Minimum = numeric(0),
        Maximum = numeric(0),
        Error = numeric(0)
    )
    df <- load_pt_polls() %>% 
        mutate(Error = round(qnorm(0.95)*sqrt(Proportion*(1-Proportion)/Total),3)) %>%
        mutate(Minimum = Proportion - Error) %>%
        mutate(Maximum = Proportion + Error) %>%
        select(
            PoliticalParty,Total,Date,PollID,Proportion,Minimum,Maximum,Error
        )
    isolate(values$dt <- bind_rows(values$dt,df))
    isolate({
        values_for_plot <- values$dt %>% 
            mutate(Date = as.Date(Date),
                   Proportion = as.numeric(Proportion),
                   Minimum = as.numeric(Minimum),
                   Maximum = as.numeric(Maximum)) %>% 
            mutate(PoliticalParty = reorder(PoliticalParty,Proportion)) %>% 
            mutate(plot_facet = sprintf('%s (%s)',PollID,Date)) %>%
            mutate(plot_facet = reorder(plot_facet,Date))
        
        values_for_plot <- select_values_for_plot(values_for_plot)
        plots <- make_plots(values_for_plot)
        heights <- list(
            poll_plot = min(
                (floor(length(unique(values_for_plot$PoliticalParty)) / 3)+1) * 200),
            poll_plot_all = 700,
            poll_plot_bars = min(
                (floor(length(unique(paste(values_for_plot$PollID,
                                           values_for_plot$Date))) / 3)+1) * 200)+100,
            poll_plot_variations = min(
                (floor(length(unique(values_for_plot$PoliticalParty)) / 3)+1) * 150)
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
        colnames(df) <- c("PoliticalParty","Total",
                          "Date","PollID","Proportion")
        df <- df %>% 
            mutate(Error = qnorm(input$confidence/100)*sqrt(Proportion*(1-Proportion)/Total)) %>%
            mutate(Minimum = Proportion - Error) %>%
            mutate(Maximum = Proportion + Error) %>%
            select(
                PoliticalParty,Total,Date,PollID,Proportion,Minimum,Maximum,Error
            )
        values$dt <- bind_rows(values$dt,df)
        isolate(values$dt <- values$dt %>% distinct)
    })
    observeEvent(input$confidence,{
        df <- values$dt %>%
            mutate(Error = round(
                qnorm(input$confidence/100)*sqrt(Proportion*(1-Proportion)/Total),
                3)) %>%
            mutate(Minimum = Proportion - Error,
                   Maximum = Proportion + Error)
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
            PoliticalParty = character(0),
            Total = numeric(0),
            Date = character(0),
            PollID = character(0),
            Proportion = double(0),
            Minimum = numeric(0),
            Maximum = numeric(0),
            Error = numeric(0)
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
                mutate(Date = as.Date(Date),
                       Proportion = as.numeric(Proportion),
                       Minimum = as.numeric(Minimum),
                       Maximum = as.numeric(Maximum)) %>% 
                mutate(PoliticalParty = reorder(PoliticalParty,Proportion)) %>% 
                mutate(plot_facet = sprintf('%s (%s)',PollID,Date)) %>%
                mutate(plot_facet = reorder(plot_facet,Date))
            
            values_for_plot <- select_values_for_plot(values_for_plot)
            if (nrow(values_for_plot) > 0) {
                heights <- list(
                    poll_plot = min(
                        (floor(length(unique(values_for_plot$PoliticalParty)) / 3)+1) * 200),
                    poll_plot_all = 700,
                    poll_plot_bars = min(
                        (floor(length(unique(paste(values_for_plot$PollID,
                                                   values_for_plot$Date))) / 3)+1) * 200)+100,
                    poll_plot_variations = min(
                        (floor(length(unique(values_for_plot$PoliticalParty)) / 1)+1) * 200)
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
            group_by(PoliticalParty) %>% 
            arrange(PoliticalParty,Date) %>% 
            mutate(PrevP = c(NA,Proportion[-length(Proportion)]),
                   PrevT = c(NA,Total[-length(Total)])) %>% 
            rowwise() %>%
            mutate(`p-value para ponto anterior` = statistical_test(
                Proportion,
                PrevP,
                Total,
                PrevT
            )) %>% 
            mutate(Significante = ifelse(`p-value para ponto anterior` < 0.05,
                                         website_legends$yes,website_legends$yes)) %>%
            mutate(Significante = ifelse(is.na(Significante),
                                         website_legends$no,Significante)) %>%
            group_by(PoliticalParty) %>%
            arrange(Date) %>%
            mutate(Variação = c(NA,diff(Proportion)),
                   VariationError = sqrt(Error^2 + c(NA,Error[-1])^2)) %>%
            select(-PrevP,-PrevT)
        output$download_simple <- downloadHandler(
            filename = function() {"polls_simplified.csv"},
            content = function(file) {
                write.csv(select(
                    values$dt,
                    PoliticalParty,Total,Date,PollID,Proportion), 
                    fileEncoding = "UTF-8",file, row.names = FALSE)
            }
        )
        output$download_with_error <- downloadHandler(
            filename = function() {"polls_with_errors.csv"},
            content = function(file) {
                write.csv(values$dt, file,
                          fileEncoding = "UTF-8", row.names = FALSE)
            }
        )
        output$download_complete <- downloadHandler(
            filename = function() {"polls_complete.csv"},
            content = function(file) {
                write.csv(complete_values, 
                          fileEncoding = "UTF-8",
                          file, row.names = FALSE)
            }
        )
    }
    )
})

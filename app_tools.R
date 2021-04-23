# This script provides plotting, risk modeling and prediction tools for the app

# tools ----

  require(plyr)
  require(tidyverse)
  require(cowplot)
  require(gridExtra)

# data container ----

  app_tools <- list()
  
# globals ----
  
  proj_globs <- list()
  
  ## graphics
  
  proj_globs$common_text <- element_text(size = 14, 
                                         face = 'plain', 
                                         color = 'black')
  
  proj_globs$common_margin <- margin(t = 5, 
                                     l = 5, 
                                     r = 5, 
                                     unit = 'mm')
  
  proj_globs$common_theme <- theme_classic() + theme(axis.text = proj_globs$common_text, 
                                                     axis.title = proj_globs$common_text, 
                                                     plot.title = element_text(size = 16, 
                                                                               face = 'bold'), 
                                                     plot.subtitle = proj_globs$common_text, 
                                                     plot.tag = element_text(size = 14, 
                                                                             face = 'plain', 
                                                                             color = 'black', 
                                                                             hjust = 0, 
                                                                             vjust = 1), 
                                                     plot.tag.position = 'bottom', 
                                                     legend.text = proj_globs$common_text, 
                                                     legend.title = proj_globs$common_text, 
                                                     strip.text = proj_globs$common_text,
                                                     plot.margin = proj_globs$common_margin)
  
# functions ----
  
  score_table <- function(multi_organ_sum, anosmia_acute, forgetfulness_acute, breath_short_acute, 
                          comorb_class_neuropsych, extrasystole_acute, imp_concentration_acute) {
    
    ## generates a table with sub-values of the ALPS, arguments van be logical or numeric
    
    out_tbl <- tibble(`Score component` = c('# multi-organ phenotype symptoms', 
                                            'Acute tachypnea (1 if present)', 
                                            'Acute heart palpitations (1 if present)', 
                                            'Acute anosmia or smell impairment (1 if present)', 
                                            'Acute concentration problems (1 if present)', 
                                            'Acute memory problems (1 if present)',  
                                            'Pre-exsiting neurological or psychiatric condition (1 if present)'), 
                      `Your value` = c(multi_organ_sum, 
                                       breath_short_acute, 
                                       extrasystole_acute, 
                                       anosmia_acute, 
                                       imp_concentration_acute, 
                                       forgetfulness_acute,
                                       comorb_class_neuropsych) %>% 
                        as.integer)
    
    out_tbl <- out_tbl %>% 
      rbind(tibble(`Score component` = 'Total ALPS', 
                   `Your value` = sum(out_tbl[['Your value']])))
    
    return(out_tbl)
    
  }
  
  predict_test <- function(score_val, response = 'chronic_covid') {
    
    ## predicts the risk of chronic covid (hook for other responses present as well)
    ## for the given score value
    ## confidence intervals obtained from the normal distribution (alpha = 0.05)
    
    risk_model <- app_tools$risk_models[[response]]
    
    predicted_risk <- predict.glm(risk_model, 
                                  newdata = tibble(score = score_val), 
                                  type = 'response', se.fit = T)
    
    return(tibble(p = predicted_risk$fit, 
                  se = predicted_risk$se.fit, 
                  lower_ci = predicted_risk$fit + predicted_risk$se.fit * qnorm(0.025), 
                  upper_ci = predicted_risk$fit + predicted_risk$se.fit * qnorm(0.975)))
    
  }
  
  count_feature <- function(inp_tbl, var_to_count, remove_na = T) {
    
    ## calculates the percentage and number of participants with/without the given feature
    
    if(remove_na) {
      
      count_tbl <- inp_tbl %>% 
        filter(!is.na(.data[[var_to_count]]), 
               .data[[var_to_count]] != 'no_answer')
      
    } else {
      
      count_tbl <- inp_tbl
      
    }
    
    feature_counts <- count_tbl %>% 
      count(.data[[var_to_count]]) %>% 
      mutate(percent = n/sum(n) * 100, 
             total_n = sum(n))
    
    return(feature_counts)
    
  }
  
  plot_score_event <- function(score_tbl, score_val, strata_breaks, strata_labels = NULL, 
                               fill_color = 'coral3', label_bars = T, 
                               x_lab = 'Score', y_lab = '% cases within score strata', 
                               plot_title = NULL, plot_subtitle = NULL, 
                               plot_tag = NULL, fontsize = 4) {
    
    ## calculates a summary table with the percent positive response in the each score strata
    ## defined by strata_breaks.
    ## Generates and returns the respective bar plot with the strata containing the given score value
    ## (score_val) highlighted
    
    plotting_tbl <- score_tbl %>% 
      mutate(score_strata = cut(score, 
                                breaks = strata_breaks, 
                                labels = strata_labels)) %>% 
      dlply(.(score_strata), count_feature, 'response') %>% 
      map2_dfr(., names(.), function(x, y) mutate(x, score_strata = y)) %>% 
      filter(response == 1)
    
    highlight_strata <- tibble(score = score_val) %>% 
      mutate(score_strata = cut(score,
                                breaks = strata_breaks, 
                                labels = strata_labels)) %>% 
      .$score_strata
    
    plotting_tbl <- plotting_tbl %>% 
      mutate(fill_var = ifelse(score_strata == highlight_strata, 
                               'highlight', 
                               'normal'))
    
    if(label_bars) {
      
      ## should percent response and n number be displayed in the plot?
      
      plotting_tbl <- plotting_tbl %>% 
        mutate(plot_lab = paste(signif(percent, 2), 
                                '%\nn =',
                                total_n))
      
      score_plot <- plotting_tbl %>% 
        ggplot(aes(x = score_strata, 
                   y = percent)) + 
        geom_text(aes(label = plot_lab), 
                  size = fontsize, 
                  hjust = 0.5, 
                  vjust = 0, 
                  nudge_y = 1)
      
    } else {
      
      score_plot <- plotting_tbl %>% 
        ggplot(aes(x = score_strata, 
                   y = percent))
      
    }
    
    score_plot <- score_plot + 
      geom_bar(aes(fill = fill_var), 
               stat = 'identity', 
               color = 'black') + 
      proj_globs$common_theme + 
      labs(x = x_lab, 
           y = y_lab, 
           title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag) + 
      scale_fill_manual(values = c(highlight = 'firebrick4', 
                                   normal = fill_color)) + 
      guides(fill = F)
    
    if(!is.null(strata_labels)) {
      
      score_plot <- score_plot + 
        scale_x_discrete(limits = strata_labels)
      
    }
    
    return(score_plot)
    
  }

  plot_score_response <- function(score_val, response = 'chronic_covid', fontsize = 4) {
    
    ## generates a plot with real-life data of score and risk distribution
    ## response is a hook for other readouts (physical impairment, complete convalescence, etc.)
    
    score_tbl <- app_tools$score_tbls[[response]]
    
    fill_color <- switch(response, 
                         'chronic_covid' = 'coral3', 
                         'subj_impairment' = 'steelblue4')
    
    
    x_lab <- switch(response, 
                    'chronic_covid' = 'Long COVID Prediction Score', 
                    'subj_impairment' = 'COVID impairment prediction score')
    
    plot_title <- switch(response, 
                         'chronic_covid' = 'Long COVID Prediction Score', 
                         'subj_impairment' = 'COVID impairment prediction score')
    
    plot_subtitle <- switch(response, 
                            'chronic_covid' = 'Risk of developing at least one post-acute COVID-19 symptom', 
                            'subj_impairment' = 'Risk of >50% performance impairment after COVID')
    
    plot_tag <- paste('\nn =', 
                      nrow(score_tbl))
    
    strata_breaks <- app_tools$real_life_plots$strata_breaks[[response]]
    
    strata_labels <- app_tools$real_life_plots$strata_labels[[response]]
    
    real_life_risk <- plot_score_event(score_tbl = score_tbl, 
                                       score_val = score_val, 
                                       strata_breaks = strata_breaks, 
                                       strata_labels = strata_labels, 
                                       fill_color = fill_color, 
                                       label_bars = T, 
                                       x_lab = x_lab, 
                                       plot_title = plot_title, 
                                       plot_subtitle = plot_subtitle, 
                                       plot_tag = plot_tag, 
                                       fontsize = fontsize) + 
      expand_limits(y = app_tools$real_life_plots$y_expands[response])
    
    return(real_life_risk)
    
  }
  
  plot_mod_results <- function(score_val, response = 'chronic_covid', fontsize = 4) {
    
    ## makes a simple plot of the modeled risk value in comparison to the prevalence in the study population
    
    ## prevalence of the response in the study cohort
    
    comparator_tbl <- app_tools$score_tbls[[response]] %>% 
      count_feature('response') %>% 
      filter(response == 1)
    
    comparator_prevalence <- comparator_tbl$percent
    
    comparator_n <- comparator_tbl$total_n
    
    ## risk values predicted from the model
    
    pred_tbl <- predict_test(score_val = score_val, 
                             response = response)
    
    ## plot
    
    risk_plot <- pred_tbl %>% 
      ggplot(aes(x = p * 100, 
                 y = 1, 
                 fill = p * 100))
    
    ## adding the low (0 to 25%), intermediate (25 - 50%) and high risk (over 50%) ranges to the plot
    
    risk_plot <- risk_plot + 
      annotate('rect', 
               xmin = -Inf, 
               xmax = 25, 
               ymin = -Inf, 
               ymax = Inf, 
               alpha = 0.15, 
               fill = 'steelblue4') + 
      annotate('rect', 
               xmin = 25, 
               xmax = 50, 
               ymin = -Inf, 
               ymax = Inf, 
               alpha = 0.15, 
               fill = 'white') + 
      annotate('rect', 
               xmin = 50, 
               xmax = Inf, 
               ymin = -Inf, 
               ymax = Inf, 
               alpha = 0.15, 
               fill = 'firebrick4') + 
      annotate('text', 
               label = 'Low risk', 
               size = fontsize, 
               x = 25/2, 
               y = 1.7, 
               color = 'steelblue4', 
               fontface = 'bold') + 
      annotate('text', 
               label = 'Intermediate risk', 
               size = fontsize, 
               x = (50 - 25)/2 + 25, 
               y = 1.7, 
               color = 'gray60', 
               fontface = 'bold') + 
      annotate('text', 
               label = 'High risk', 
               size = fontsize, 
               x = (100 - 50)/2 + 50, 
               y = 1.7, 
               color = 'firebrick4', 
               fontface = 'bold')
    
    ## all other layers
    
    risk_plot <- risk_plot + 
      geom_vline(xintercept = comparator_prevalence, 
                 linetype = 'dashed') + 
      geom_point(data = tibble(p = comparator_prevalence), 
                 aes(x = p, 
                     y = 1), 
                 shape = 22, 
                 size = 4, 
                 fill = 'white') + 
      geom_errorbarh(aes(xmin = lower_ci * 100, 
                         xmax = upper_ci * 100), 
                     height = 0.25) + 
      geom_point(shape = 21, 
                 size = 4) + 
      proj_globs$common_theme + 
      theme(axis.title.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.line.y = element_blank()) + 
      scale_fill_gradient2(low = 'steelblue4', 
                           mid = 'white', 
                           high = 'firebrick4', 
                           midpoint = comparator_prevalence, 
                           limits = c(0, 100), 
                           name = 'Predicted\nrisk') + 
      scale_x_continuous(limits = app_tools$mod_plots_scales[[response]], 
                         name = 'Predicted risk, %') + 
      scale_y_continuous(limits = c(0.5, 1.8)) + 
      guides(fill = F) + 
      annotate('text', 
               label = paste('Study prevalence = ', 
                             signif(comparator_prevalence, 3), 
                             '%', sep = ''), 
               size = fontsize, 
               x = comparator_prevalence + 2, 
               y = 1.3, 
               hjust = 0, 
               vjust = 0)
    
    
      
    
    return(risk_plot)
    
  }
  
  render_report <- function(score_val, score_comp_table, path_to_save, response = 'chronic_covid') {
    
    ## makes a simple ggplot/cowplot report to be saved by the user
    ## score value and score component table has to be provided
    ## based on cowplot - faster than Rmarkdown for simple, dynamic reports
    
    model_plot <- plot_mod_results(score_val = score_val, 
                                   response = response, 
                                   fontsize = 2.75) ## forest plot
    
    prev_plot <- plot_score_response(score_val = score_val, 
                                     response = response, 
                                     fontsize = 2.75) ## plot with prevalence as a function of the score
    
    model_predictions <- predict_test(score_val = score_val, 
                                      response = response) ## GLM predicted risk with 95% CI
    
    ## table with score components
    
    hj <- matrix(c(0, 1), 
                 ncol = 2, 
                 nrow = nrow(score_comp_table), 
                 byrow = TRUE) ## justification of the table columns
    
    x <- matrix(c(0.05, 0.95), 
                ncol = 2, 
                nrow = nrow(score_comp_table), 
                byrow = TRUE) ## justification of the table comumns
    
    
    theme_1 <- ttheme_minimal(core = list(fg_params = list(hjust = as.vector(hj), 
                                                           x = as.vector(x), 
                                                           fontsize = 10, 
                                                           fontface = c(rep(1, nrow(score_comp_table) - 1), 2))),
                              colhead = list(fg_params = list(fontsize = 11, 
                                                              fontface = 'bold', 
                                                              hjust = as.vector(hj), 
                                                              x = as.vector(x)), 
                                             bg_params = list(fill = 'gray96')))
    
    
    table_grob <- tableGrob(score_comp_table, 
                            theme = theme_1, 
                            rows = NULL)
    
    ## putting everything into a report
    
    report <- list()
    
    report$banner <- plot_grid(ggdraw() + 
                                 draw_image('./data/mui_logo.jpg'), 
                               nrow = 1) + 
      theme(plot.margin = margin(l = 3, r = 3, t = 2, b = 4, unit = 'mm'))
    
    report$title_panel <- plot_grid(ggdraw() + 
                                      draw_text('ALPS: Austrian Long COVID Prediction Score\ncalculator report', 
                                                size = 16, 
                                                color = '#5d86bb', 
                                                fontface = 'bold'), 
                                    ggdraw() + 
                                      draw_text(c('Long COVID is defined as presence of at least one post-acute persistent symptom of COVID-19 for', 
                                                  '28 days or longer after symptom onset. Score development, modeling of long COVID risk and real-life', 
                                                  'prevalence data are based on the results of the Health after COVID-19 study in Tyrol/Austria.') %>% 
                                                  paste(collapse = '\n'), 
                                                size = 11, 
                                                hjust = 0, 
                                                vjust = 0, 
                                                x = 0.1, 
                                                y = 0.2), 
                                    ggdraw() + 
                                      draw_text('Long COVID Prediction Score calculation', 
                                                size = 14, 
                                                color = '#5d86bb', 
                                                hjust = 0, 
                                                vjust = 0, 
                                                x = 0.1, 
                                                y = 0.2, 
                                                fontface = 'bold'), 
                                    table_grob, ## here comes the table with ALPS components
                                    ggdraw() + 
                                      draw_text(paste('Your chronic COVID prediction score:', 
                                                      score_val), 
                                                size = 11, 
                                                hjust = 0, 
                                                vjust = 0, 
                                                x = 0.1, 
                                                y = 0.2), 
                                    ggdraw() + 
                                      draw_text('Risk prediction by modeling', 
                                                size = 14, 
                                                color = '#5d86bb', 
                                                hjust = 0, 
                                                vjust = 0, 
                                                x = 0.1, 
                                                y = 0.2, 
                                                fontface = 'bold'), 
                                    ggdraw() + 
                                      draw_text(paste('Risk estimate obtained by logistic regression (n = 1009).\nYour predicted risk of developing at least one post-acute symptom: ', 
                                                      signif(model_predictions$p[1], 2) * 100, 
                                                      '%, [95%CI: ', 
                                                      signif(model_predictions$lower_ci[1], 2) * 100, 
                                                      ' to ', 
                                                      signif(model_predictions$upper_ci[1], 2) * 100, 
                                                      '%]', sep = ''), 
                                                size = 11, 
                                                hjust = 0, 
                                                vjust = 0, 
                                                x = 0.1, 
                                                y = 0.2), 
                                    model_plot + 
                                      theme(axis.title = element_text(size = 8), 
                                            axis.text = element_text(size = 8), 
                                            plot.subtitle = element_text(size = 8), 
                                            plot.title = element_text(size = 8), 
                                            plot.tag = element_text(size = 8), 
                                            plot.margin = margin(r = 18, l = 18, t = 4, unit = 'mm')), 
                                    ggdraw() + 
                                      draw_text('Real-life prevalence', 
                                                size = 14, 
                                                color = '#5d86bb', 
                                                hjust = 0, 
                                                vjust = 0, 
                                                x = 0.1, 
                                                y = 0.2, 
                                                fontface = 'bold'), 
                                    prev_plot + 
                                      theme(axis.title = element_text(size = 8), 
                                            axis.text = element_text(size = 8), 
                                            plot.subtitle = element_text(size = 8), 
                                            plot.title = element_text(size = 8), 
                                            plot.tag = element_text(size = 8), 
                                            plot.margin = margin(r = 15, l = 15, t = 4, unit = 'mm')), 
                                    ncol = 1, 
                                    rel_heights = c(0.25, 0.32, 0.15, 0.9, 0.15, 0.15, 0.2, 0.37, 0.1, 1.3))
  
    
    report_file <- plot_grid(plotlist = report, 
                             ncol = 1, 
                             rel_heights = c(0.06, 1))
    
    ggsave(plot = report_file, 
           filename = path_to_save, 
           width = 210, 
           height = 290, 
           units = 'mm')
    
  }

# reading the tables with values of both prediction scores and outcomes in the establishment cohort -----
  
  app_tools$score_tbls$chronic_covid <- read_tsv('./data/chronic_covid_score_tbl.txt')

# generating the risk models: logistic regression -----
  
  app_tools$risk_models$chronic_covid <- app_tools$score_tbls$chronic_covid %>% 
    glm(response ~ score, 
        data = ., 
        family = 'binomial')
  
# globals for generating the plots with modeling results, hooks for other responses ----
  
  app_tools$mod_plots_scales <- list(chronic_covid = c(0, 100), 
                                     subj_impairment = c(0, 25))
  
# globals for generating the plots with real life data, hooks for other responses ----
  
  app_tools$real_life_plots$strata_breaks = list(chronic_covid = c(-1, 0, 2, 5, 10, 33), 
                                                 subj_impairment = c(-1, 2, 5, 10, 29))
  
  app_tools$real_life_plots$strata_labels = list(chronic_covid = c('0', '1 - 2', '3 - 5', '6 - 10', '11 - 32'), 
                                                 subj_impairment = c('0 - 2', '3 - 5', '6 - 10', '11 - 28'))
  
  app_tools$real_life_plots$y_expands <- c(100, 38) %>% 
    set_names(names(app_tools$real_life_plots$strata_breaks))

# END ----
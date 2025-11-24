patientTimelineServer <- function(id, database = NULL, cohort_id = -1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (!is.null(database)) {
      
      subject_id_list <- reactive({
        if (isTruthy(cohort_id) && cohort_id == -1) {
          dplyr::tbl(database(), in_schema("mimiciv_hosp", "patients")) %>%
            select("subject_id") %>%
            collect()
        } else if (isTruthy(cohort_id) && cohort_id != -1) {
          dplyr::tbl(database(), in_schema("public", "cohort")) %>%
            filter(cohort_id == !!cohort_id) %>%
            select("subject_id") %>%
            distinct() %>%
            collect()
        } else {
          NULL
        }
      })
      
      search_api_subject_id <- function(data, q, database) {
        has_matching <- function(field) {
          startsWith(as.character(field), as.character(q))
        }
        if (!is.null(data)) {
          search_result <- data %>%
            filter(has_matching(subject_id)) %>%
            head(100)
          
          if (nrow(search_result) > 0) {
            request <- paste0(
              'select subject_id, gender, COUNT(*) as "total_hadm",
               coalesce((select count(*) from mimiciv_hosp.patients i_p
                         join mimiciv_icu.icustays i USING(subject_id)
                         where i_p.subject_id = o_p.subject_id group by subject_id), 0) as "total_stay",
               (SELECT i_p.anchor_age + DATE_PART(\'year\', admittime) - anchor_year
                FROM mimiciv_hosp.admissions i_a natural join mimiciv_hosp.patients i_p
                WHERE i_a.subject_id = o_p.subject_id ORDER BY admittime LIMIT 1) as "age"
               from mimiciv_hosp.patients o_p
               join mimiciv_hosp.admissions a USING(subject_id)
               where subject_id in (',
              paste0(search_result[['subject_id']], collapse = ','),
              ') group by subject_id, gender limit 100'
            )
            
            dplyr::tbl(database, sql(request)) %>%
              collect() %>%
              transmute(
                name = paste0(
                  subject_id,
                  "<div class='ui ",
                  case_when(gender == "M" ~ "blue", .default = "olive"),
                  " label'>", gender,
                  "<div class='detail'>", age, "</div></div>",
                  "<div class='ui teal label'>", total_hadm,
                  "<div class='detail'>HADM</div></div>",
                  case_when(
                    total_stay > 0 ~ paste0(
                      "<div class='ui yellow label'>",
                      total_stay,
                      "<div class='detail'>ICU</div></div>"
                    ),
                    .default = "<div class='ui label'>0<div class='detail'>ICU</div></div>"
                  )
                ),
                value = subject_id
              )
          }
        }
      }
      
      search_api_url_subject_id <- reactive({
        custom_register_search(
          session,
          subject_id_list(),
          search_api_subject_id,
          "search_api_subject_id_timeline",
          isolate(database())
        )
      })
      
      output$select_patient <- renderUI({
        if (!is.reactive(cohort_id) || isTruthy(cohort_id)) {
          form(
            fields(
              field(
                tags$label("Select a patient (subject_id)"),
                search_selection_api(
                  ns("subject_id"),
                  search_api_url_subject_id(),
                  multiple = FALSE
                ),
                class = "ten wide"
              ),
              field(
                tags$label("Filters"),
                checkbox_input(ns("icu_only"), "ICU admissions only", FALSE),
                checkbox_input(ns("show_plots"), "Show time series plots", FALSE),
                class = "six wide"
              ),
              class = "inline"
            ),
            class = "mb-10"
          )
        }
      })
      
      output$patient_summary <- renderUI({
        req(input$subject_id)
        
        patient_info <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "patients")) %>%
          filter(subject_id == !!input$subject_id) %>%
          collect()
        
        admissions_count <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "admissions")) %>%
          filter(subject_id == !!input$subject_id) %>%
          count() %>%
          collect() %>%
          pull(n)
        
        div(
          class = "ui segment",
          div(class = "ui header", paste("Patient", input$subject_id)),
          div(
            tags$b("Gender: "), patient_info$gender[1], " | ",
            tags$b("Total Admissions: "), admissions_count
          )
        )
      })
      
      all_patient_data <- reactive({
        req(input$subject_id)
        
        withProgress(message = "Loading patient data...", {
          setProgress(0.05, message = "Fetching admissions...")
          admissions <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "admissions")) %>%
            filter(subject_id == !!input$subject_id) %>%
            collect() %>%
            arrange(admittime)
          
          setProgress(0.1, message = "Fetching ICU stays...")
          icustays <- dplyr::tbl(database(), in_schema("mimiciv_icu", "icustays")) %>%
            filter(subject_id == !!input$subject_id) %>%
            collect()
          
          setProgress(0.15, message = "Fetching diagnoses...")
          diagnoses <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "diagnoses_icd")) %>%
            filter(subject_id == !!input$subject_id) %>%
            left_join(dplyr::tbl(database(), in_schema("mimiciv_hosp", "d_icd_diagnoses")), by = c("icd_code", "icd_version")) %>%
            collect()
          
          setProgress(0.2, message = "Fetching procedures...")
          procedures <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "procedures_icd")) %>%
            filter(subject_id == !!input$subject_id) %>%
            left_join(dplyr::tbl(database(), in_schema("mimiciv_hosp", "d_icd_procedures")), by = c("icd_code", "icd_version")) %>%
            collect()
          
          setProgress(0.25, message = "Fetching prescriptions...")
          prescriptions <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "prescriptions")) %>%
            filter(subject_id == !!input$subject_id) %>%
            collect()
          
          setProgress(0.35, message = "Fetching chart events...")
          chartevents <- dplyr::tbl(database(), in_schema("mimiciv_icu", "chartevents")) %>%
            filter(subject_id == !!input$subject_id) %>%
            inner_join(dplyr::tbl(database(), in_schema("mimiciv_icu", "d_items")), by = "itemid") %>%
            select(hadm_id, stay_id, charttime, itemid, label, value, valuenum, valueuom, category) %>%
            collect()
          
          setProgress(0.5, message = "Fetching lab events...")
          labevents <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "labevents")) %>%
            filter(subject_id == !!input$subject_id) %>%
            inner_join(dplyr::tbl(database(), in_schema("mimiciv_hosp", "d_labitems")), by = "itemid") %>%
            select(hadm_id, charttime, itemid, label, value, valuenum, valueuom, category, flag) %>%
            collect()
          
          setProgress(0.6, message = "Fetching input events...")
          inputevents <- dplyr::tbl(database(), in_schema("mimiciv_icu", "inputevents")) %>%
            filter(subject_id == !!input$subject_id) %>%
            inner_join(dplyr::tbl(database(), in_schema("mimiciv_icu", "d_items")), by = "itemid") %>%
            select(hadm_id, stay_id, starttime, endtime, itemid, label, amount, amountuom, rate, rateuom, category) %>%
            collect()
          
          setProgress(0.7, message = "Fetching output events...")
          outputevents <- dplyr::tbl(database(), in_schema("mimiciv_icu", "outputevents")) %>%
            filter(subject_id == !!input$subject_id) %>%
            inner_join(dplyr::tbl(database(), in_schema("mimiciv_icu", "d_items")), by = "itemid") %>%
            select(hadm_id, stay_id, charttime, itemid, label, value, valueuom, category) %>%
            collect()
          
          setProgress(0.8, message = "Fetching procedure events...")
          procedureevents <- dplyr::tbl(database(), in_schema("mimiciv_icu", "procedureevents")) %>%
            filter(subject_id == !!input$subject_id) %>%
            inner_join(dplyr::tbl(database(), in_schema("mimiciv_icu", "d_items")), by = "itemid") %>%
            select(hadm_id, stay_id, starttime, endtime, itemid, label, value, valueuom, category) %>%
            collect()
          
          setProgress(0.9, message = "Fetching microbiology...") 
          microbiologyevents <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "microbiologyevents")) %>%
            filter(subject_id == !!input$subject_id) %>%
            collect()
          
          setProgress(1, message = "Done!")
          
          list(
            admissions = admissions,
            icustays = icustays,
            diagnoses = diagnoses,
            procedures = procedures,
            prescriptions = prescriptions,
            chartevents = chartevents,
            labevents = labevents,
            inputevents = inputevents,
            outputevents = outputevents,
            procedureevents = procedureevents,
            microbiologyevents = microbiologyevents
          )
        })
      })
      
      output$timeline_content <- renderUI({
        data <- all_patient_data()
        req(data)
        
        admissions <- data$admissions
        
        # Filter admissions by ICU-only toggle
        if (isTruthy(input$icu_only) && input$icu_only) {
          icu_hadm_ids <- data$icustays %>% pull(hadm_id) %>% unique()
          admissions <- admissions %>% filter(hadm_id %in% icu_hadm_ids)
        }
        
        if (nrow(admissions) == 0) {
          return(div(
            class = "ui warning message",
            div(class = "header", "No admissions to display"),
            tags$p("This patient has no admissions", if(isTruthy(input$icu_only) && input$icu_only) " with ICU stays" else "", ".")
          ))
        }
        
        admission_sections <- lapply(1:nrow(admissions), function(i) {
          adm <- admissions[i, ]
          hadm <- adm$hadm_id
          
          icu <- data$icustays %>% filter(hadm_id == hadm)
          diag <- data$diagnoses %>% filter(hadm_id == hadm)
          proc <- data$procedures %>% filter(hadm_id == hadm)
          rx <- data$prescriptions %>% filter(hadm_id == hadm)
          charts <- data$chartevents %>% filter(hadm_id == hadm)
          labs <- data$labevents %>% filter(hadm_id == hadm)
          inputs <- data$inputevents %>% filter(hadm_id == hadm)
          outputs <- data$outputevents %>% filter(hadm_id == hadm)
          proc_events <- data$procedureevents %>% filter(hadm_id == hadm)
          micro <- data$microbiologyevents %>% filter(hadm_id == hadm)
          
          chart_plot_ids <- list()
          lab_plot_ids <- list()
          chart_low_count <- NULL
          lab_low_count <- NULL
          
          show_all <- isTruthy(input$show_plots) && input$show_plots
          
          if (nrow(charts) > 0) {
            chart_counts <- charts %>%
              filter(!is.na(valuenum)) %>%
              group_by(itemid, label, valueuom, category) %>%
              summarise(n = n(), .groups = "drop") %>%
              arrange(desc(n))
            
            if (show_all) {
              chart_high <- chart_counts %>% filter(n > 4)
              chart_low_ids <- chart_counts %>% filter(n <= 4) %>% pull(itemid)
              
              if (nrow(chart_high) > 0) {
                chart_plot_ids <- lapply(1:nrow(chart_high), function(j) {
                  list(
                    id = paste0("chart_", hadm, "_", j),
                    itemid = chart_high$itemid[j],
                    label = chart_high$label[j],
                    unit = chart_high$valueuom[j],
                    category = chart_high$category[j]
                  )
                })
              }
              
              if (length(chart_low_ids) > 0) {
                chart_low_count <- charts %>%
                  filter(itemid %in% chart_low_ids, !is.na(valuenum)) %>%
                  select(charttime, itemid, label, valuenum, valueuom, category) %>%
                  arrange(label, charttime)
              }
            } else {
              chart_high <- chart_counts %>% filter(n > 10)
              if (nrow(chart_high) > 0) {
                chart_plot_ids <- lapply(1:min(10, nrow(chart_high)), function(j) {
                  list(
                    id = paste0("chart_", hadm, "_", j),
                    itemid = chart_high$itemid[j],
                    label = chart_high$label[j],
                    unit = chart_high$valueuom[j],
                    category = chart_high$category[j]
                  )
                })
              }
            }
          }
          
          if (nrow(labs) > 0) {
            lab_counts <- labs %>%
              filter(!is.na(valuenum)) %>%
              group_by(itemid, label, valueuom, category) %>%
              summarise(n = n(), .groups = "drop") %>%
              arrange(desc(n))
            
            if (show_all) {
              lab_high <- lab_counts %>% filter(n > 4)
              lab_low_ids <- lab_counts %>% filter(n <= 4) %>% pull(itemid)
              
              if (nrow(lab_high) > 0) {
                lab_plot_ids <- lapply(1:nrow(lab_high), function(j) {
                  list(
                    id = paste0("lab_", hadm, "_", j),
                    itemid = lab_high$itemid[j],
                    label = lab_high$label[j],
                    unit = lab_high$valueuom[j],
                    category = lab_high$category[j]
                  )
                })
              }
              
              if (length(lab_low_ids) > 0) {
                lab_low_count <- labs %>%
                  filter(itemid %in% lab_low_ids, !is.na(valuenum)) %>%
                  select(charttime, itemid, label, valuenum, valueuom, category) %>%
                  arrange(label, charttime)
              }
            } else {
              lab_high <- lab_counts %>% filter(n > 10)
              if (nrow(lab_high) > 0) {
                lab_plot_ids <- lapply(1:min(10, nrow(lab_high)), function(j) {
                  list(
                    id = paste0("lab_", hadm, "_", j),
                    itemid = lab_high$itemid[j],
                    label = lab_high$label[j],
                    unit = lab_high$valueuom[j],
                    category = lab_high$category[j]
                  )
                })
              }
            }
          }
          
          lapply(chart_plot_ids, function(plot_info) {
            local({
              local_plot_info <- plot_info
              local_charts <- charts
              output[[local_plot_info$id]] <- renderPlotly({
                plot_data <- local_charts %>%
                  filter(itemid == local_plot_info$itemid, !is.na(valuenum))
                
                if (nrow(plot_data) > 0) {
                  plot_ly(plot_data, x = ~charttime, y = ~valuenum,
                         type = 'scatter', mode = 'lines+markers',
                         name = local_plot_info$label,
                         text = ~paste("Time:", charttime, "<br>Value:", valuenum, local_plot_info$unit),
                         hoverinfo = 'text') %>%
                    layout(
                      title = paste(local_plot_info$label, "(", local_plot_info$unit, ")"),
                      xaxis = list(title = "Time"),
                      yaxis = list(title = paste("Value (", local_plot_info$unit, ")")),
                      hovermode = 'closest'
                    )
                }
              })
            })
          })
          
          lapply(lab_plot_ids, function(plot_info) {
            local({
              local_plot_info <- plot_info
              local_labs <- labs
              output[[local_plot_info$id]] <- renderPlotly({
                plot_data <- local_labs %>%
                  filter(itemid == local_plot_info$itemid, !is.na(valuenum))
                
                if (nrow(plot_data) > 0) {
                  plot_ly(plot_data, x = ~charttime, y = ~valuenum,
                         type = 'scatter', mode = 'lines+markers',
                         name = local_plot_info$label,
                         text = ~paste("Time:", charttime, "<br>Value:", valuenum, local_plot_info$unit),
                         hoverinfo = 'text') %>%
                    layout(
                      title = paste(local_plot_info$label, "(", local_plot_info$unit, ")"),
                      xaxis = list(title = "Time"),
                      yaxis = list(title = paste("Value (", local_plot_info$unit, ")")),
                      hovermode = 'closest'
                    )
                }
              })
            })
          })
          
          output[[paste0("icu_", hadm)]] <- DT::renderDataTable({
            if (nrow(icu) > 0) DT::datatable(icu, options = list(pageLength = 5, scrollX = TRUE))
          })
          
          output[[paste0("diag_", hadm)]] <- DT::renderDataTable({
            if (nrow(diag) > 0) DT::datatable(diag, options = list(pageLength = 10, scrollX = TRUE))
          })
          
          output[[paste0("proc_", hadm)]] <- DT::renderDataTable({
            if (nrow(proc) > 0) DT::datatable(proc, options = list(pageLength = 10, scrollX = TRUE))
          })
          
          output[[paste0("rx_", hadm)]] <- DT::renderDataTable({
            if (nrow(rx) > 0) DT::datatable(rx, options = list(pageLength = 10, scrollX = TRUE))
          })
          
          output[[paste0("chart_low_", hadm)]] <- DT::renderDataTable({
            if (!is.null(chart_low_count) && nrow(chart_low_count) > 0) {
              DT::datatable(chart_low_count, options = list(pageLength = 10, scrollX = TRUE))
            }
          })
          
          output[[paste0("lab_low_", hadm)]] <- DT::renderDataTable({
            if (!is.null(lab_low_count) && nrow(lab_low_count) > 0) {
              DT::datatable(lab_low_count, options = list(pageLength = 10, scrollX = TRUE))
            }
          })
          
          output[[paste0("charts_", hadm)]] <- DT::renderDataTable({
            if (nrow(charts) > 0) DT::datatable(charts, options = list(pageLength = 20, scrollX = TRUE))
          })
          
          output[[paste0("labs_", hadm)]] <- DT::renderDataTable({
            if (nrow(labs) > 0) DT::datatable(labs, options = list(pageLength = 20, scrollX = TRUE))
          })
          
          output[[paste0("inputs_", hadm)]] <- DT::renderDataTable({
            if (nrow(inputs) > 0) DT::datatable(inputs, options = list(pageLength = 10, scrollX = TRUE))
          })
          
          output[[paste0("outputs_", hadm)]] <- DT::renderDataTable({
            if (nrow(outputs) > 0) DT::datatable(outputs, options = list(pageLength = 10, scrollX = TRUE))
          })
          
          output[[paste0("proc_events_", hadm)]] <- DT::renderDataTable({
            if (nrow(proc_events) > 0) DT::datatable(proc_events, options = list(pageLength = 10, scrollX = TRUE))
          })
          
          output[[paste0("micro_", hadm)]] <- DT::renderDataTable({
            if (nrow(micro) > 0) DT::datatable(micro, options = list(pageLength = 10, scrollX = TRUE))
          })
          
          div(
            class = "ui raised segment",
            div(class = "ui blue ribbon label", paste("Admission", hadm, "-", adm$admission_type)),
            div(
              style = "margin-top:10px;",
              tags$b("Admit: "), as.character(adm$admittime), " | ",
              tags$b("Discharge: "), as.character(adm$dischtime), " | ",
              tags$b("ICU Stays: "), nrow(icu)
            ),
            
            if (nrow(icu) > 0) div(class = "ui segment", div(class = "ui header", "ICU Stays"), DT::dataTableOutput(ns(paste0("icu_", hadm)))),
            if (nrow(diag) > 0) div(class = "ui segment", div(class = "ui header", "Diagnoses (ICD)"), DT::dataTableOutput(ns(paste0("diag_", hadm)))),
            if (nrow(proc) > 0) div(class = "ui segment", div(class = "ui header", "Procedures (ICD)"), DT::dataTableOutput(ns(paste0("proc_", hadm)))),
            if (nrow(rx) > 0) div(class = "ui segment", div(class = "ui header", "Prescriptions"), DT::dataTableOutput(ns(paste0("rx_", hadm)))),
            
            if (length(chart_plot_ids) > 0) {
              div(
                class = "ui segment",
                div(class = "ui header", 
                    if (show_all) "Chart Events (Time Series - All with >4 values)" else "Chart Events (Time Series - Top 10)"),
                lapply(chart_plot_ids, function(p) plotlyOutput(ns(p$id), height = "300px"))
              )
            },
            
            if (show_all && !is.null(chart_low_count) && nrow(chart_low_count) > 0) {
              div(class = "ui segment", 
                  div(class = "ui header", "Chart Events (Low Data Count - ≤4 values)"), 
                  DT::dataTableOutput(ns(paste0("chart_low_", hadm))))
            },
            
            if (nrow(charts) > 0) div(class = "ui segment", div(class = "ui header", "Chart Events (All Data)"), DT::dataTableOutput(ns(paste0("charts_", hadm)))),
            
            if (length(lab_plot_ids) > 0) {
              div(
                class = "ui segment",
                div(class = "ui header", 
                    if (show_all) "Lab Events (Time Series - All with >4 values)" else "Lab Events (Time Series - Top 10)"),
                lapply(lab_plot_ids, function(p) plotlyOutput(ns(p$id), height = "300px"))
              )
            },
            
            if (show_all && !is.null(lab_low_count) && nrow(lab_low_count) > 0) {
              div(class = "ui segment", 
                  div(class = "ui header", "Lab Events (Low Data Count - ≤4 values)"), 
                  DT::dataTableOutput(ns(paste0("lab_low_", hadm))))
            },
            
            if (nrow(labs) > 0) div(class = "ui segment", div(class = "ui header", "Lab Events (All Data)"), DT::dataTableOutput(ns(paste0("labs_", hadm)))),
            if (nrow(inputs) > 0) div(class = "ui segment", div(class = "ui header", "Input Events"), DT::dataTableOutput(ns(paste0("inputs_", hadm)))),
            if (nrow(outputs) > 0) div(class = "ui segment", div(class = "ui header", "Output Events"), DT::dataTableOutput(ns(paste0("outputs_", hadm)))),
            if (nrow(proc_events) > 0) div(class = "ui segment", div(class = "ui header", "Procedure Events"), DT::dataTableOutput(ns(paste0("proc_events_", hadm)))),
            if (nrow(micro) > 0) div(class = "ui segment", div(class = "ui header", "Microbiology Events"), DT::dataTableOutput(ns(paste0("micro_", hadm))))
          )
        })
        
        tagList(admission_sections)
      })
    }
  })
}

# Cargar las librerías necesarias
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(bslib)  # Librería para usar page_sidebar()
library(DT)     # Para mostrar la tabla de datos
library(bupaR)  # Para usar el processAnimater()
library(bupaverse)
library(tidyverse)
library(processanimateR)
library(shinycssloaders)
library(eventdataR)
library(edeaR)
library(DiagrammeR)
library(plotly)
library(dplyr)
library(shinyjs)


# Cargar BD
df_causas_judiciales <- read_delim(file = './combined_tabla_actos_procesales_no_penales_cordoba_v4_2_1.csv.gz', delim=",")
df_causas_judiciales$acto_procesal_fecha <- paste0(df_causas_judiciales$acto_procesal_fecha, ' 00:00:00')
df_causas_judiciales %>%
    #filter( materia_estadistica == 'Civil patrimonial y comercial' | materia_estadistica == 'Civil patrimonial y Comercial')%>%
    # rename timestamp variables appropriately
    dplyr::rename(start = acto_procesal_fecha#,
           #complete = acto_procesal_fecha
                    ) %>%
    # convert timestamps to
    convert_timestamps(columns = "start", format = ymd_hms) %>%
    activitylog(case_id = "causa_id",
                activity_id = "actividad",
                timestamps = "start",
                resource_id="unidad_id") -> lg_causas_judiciales

# Crear variables para filtros
circunscripcion <- as.list(unique(lg_causas_judiciales$circunscripcion_descripcion))
materia <- as.list(unique(lg_causas_judiciales$materia_descripcion))
unidad <- as.list(unique(lg_causas_judiciales$unidad_descripcion))
objeto_litigio <- as.list(unique(lg_causas_judiciales$objeto_litigio_descripcion))
objeto_litigio_est <- as.list(unique(lg_causas_judiciales$objeto_litigio_estadistico))
estado_procesal <- as.list(unique(lg_causas_judiciales$estado_procesal))
actividades<-as.list(unique(lg_causas_judiciales$actividad))
#max_th <- max(data.frame(lg_causas_judiciales%>%throughput_time("case",units="days"))$throughput_time)

# Definir el UI (User Interface)
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "yeti"),  # Tema Bootstrap 4
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      .kpi_value {
        font-size: 2rem;
        font-weight: bold;
        color: #2471A3;
      }
                    
     .info_proc {
        background-color: #567ebb;
        font-weight: bold;
        color: white;
     }"))
  ),
  title = "Process Mining en Procesos Judiciales de Córdoba [Fuero Civ y Com]",
 
  # Sidebar con filtros y barra desplazadora si es necesario
  sidebar = sidebar(
    id = "sidebar",  # ID del sidebar
    title="Panel de Filtros",
    width = 250,     # Definir el ancho del sidebar
    #style = "overflow-y: auto; max-height: 90vh;",  # Barra de scroll dentro del sidebar si el contenido excede el tamaño
    selectInput("filtro1", "Circunscripción", choices = c("Seleccione",circunscripcion),"Seleccione"),
    selectInput("filtro2", "Materia", choices = c("Seleccione",materia),"Seleccione"),
    selectInput("filtro3", "Unidad", choices = c("Seleccione",unidad),"Seleccione"),
    selectInput("filtro4", "Estado Procesal", choices = c("Seleccione",estado_procesal),"Seleccione"),
    tooltip(selectInput("filtro5_est", "Objeto Litigio (est)", choices = c("Seleccione",objeto_litigio_est),"Seleccione"),"Este filtro es una macro-categoria del filtro de abajo", id="tip", placement="right"),
    selectInput("filtro5", "Objeto Litigio", choices = c("Seleccione",objeto_litigio),"Seleccione"),
      
    #selectInput("filtro6", "Causa ID", choices = c("Seleccione",causa_id),"Seleccione"),
    
    # Filtro de rango de fechas
    dateRangeInput("rango_fechas", "Selecciona un rango de fechas:",
                   start = "2022-01-01",#min(lg_causas_judiciales$causa_fecha_inicio),#Sys.Date() - 30,  # Fecha de inicio predeterminada
                   end = "2022-12-31"#max(lg_causas_judiciales$causa_fecha_inicio)#Sys.Date()     # Fecha final predeterminada
                  )
      
  ),
  useShinyjs(),
  # Contenido principal con tabs
  navbarPage("Dashboards de Análisis",id="navbar",
             
             # Tab 1: Con componentes y gráficos responsivos
             tabPanel("Overview",value="tab1",
                      fluidRow(
                        column(2,
                               # KPI 1: Reemplazando la Card 1
                               fluidRow(
                                 #div(class = "kpi-box", style = "font-size:2rem;", "KPI 1: 123"),
                                 #style = "padding: 10px;"
                                 column(12,card(
                                     card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'># CASOS</p>")),
                                     card_body(textOutput('cant_casos'), class="kpi_value")
                                 )),
                                 #column(6,card(
                                 #    card_header("# PROMEDIO ACTOS PROCESALES"),
                                 #    card_body(textOutput("prom_actos_proc"))
                                 #))
                               ),
                               # KPI 2: Reemplazando la Card 2
                               fluidRow(
                                 #div(class = "kpi-box", style = "font-size:2rem;", "KPI 1: 123"),
                                 #style = "padding: 10px;"
                                 #column(6,card(
                                 #    card_header("# CASOS"),
                                 #    card_body(textOutput("cant_casos"))
                                 #)),
                                 column(12,card(
                                     card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'># PROMEDIO ACTOS PROCESALES</p>")),
                                     card_body(textOutput("prom_actos_proc"), class="kpi_value")
                                 ))
                               ),
                               # Gráfico de barras horizontales (categorías vs. repeticiones)
                               #fluidRow(
                               #  card(title = "Repetición de Actos Procesales", status = "primary", width = 12, solidHeader = TRUE,
                               #      plotlyOutput("grafico_barras_izquierda", height = "610px"))
                               #)
                               # KPI 3: Reemplazando la Card 2
                               fluidRow(
                                 #div(class = "kpi-box", style = "font-size:2rem;", "KPI 1: 123"),
                                 #style = "padding: 10px;"
                                 #column(6,card(
                                 #    card_header("# CASOS"),
                                 #    card_body(textOutput("cant_casos"))
                                 #)),
                                 
                               
                               



                               
                        )),
                        
                        column(10,
                               # Gráfico 1: Evolución temporal
                               fluidRow(
                                 card(title = "Evolución Temporal Casos", status = "primary", width = 12, solidHeader = TRUE,
                                     plotlyOutput("grafico_temporal", height = "300px"))
                               )
                               
                        )
                      ),
                      fluidRow(
                          # Gráfico 2: Gráfico de barras horizontales con selector
                               column(12,
                                 card(title = "Cantidad de Casos", status = "primary", width = 12, solidHeader = TRUE,
                                     selectInput("variable_selector", "Seleccione una variable:", choices = c("Objeto Litigio (est)","Objeto Litigio","Materia", "Circunscripcion")),
                                     plotlyOutput("grafico_barras_derecha", height = "300px"))
                               )
                      ),
                      fluidRow(
                          # Gráfico de barras horizontales (categorías vs. repeticiones)
                               column(12,
                                 card(title = "Repetición de Actos Procesales", status = "primary", width = 12, solidHeader = TRUE,
                                     plotlyOutput("grafico_barras_izquierda", height = "300px"))
                               )
                      )
                      
             ),
             
             # Tab 2: Con filtros específicos y un componente processAnimater()
             tabPanel("Análisis Gráfico de Procesos",value="tab2",
                      #fluidRow(
                           # Notification banner
                           #column(12,
                           #       div(id = "notificationBanner", 
                           #           class = "alert alert-info", 
                           #           style = "display: none;",  # Initially hidden
                           #           "INFO: Si no hay ningun filtro seleccionado, por defecto se muestran datos para procesos con Objeto de Litigios (est) ORDINARIO. Para visualizar datos de otros procesos elija la opción deseada en el filtro Objeto Litigio (est). Algunas combinaciones en los filtros pueden no traer información, en ese caso pruebe con otras combinaciones."
                           #       )
                           #)
                         #),
                      fluidRow(
             column(12,
                    uiOutput("notification_banner")  # Dynamic banner
             )
           ),
                      fluidRow(
                          column(12,
                                 card(card_body(textOutput('info_proceso_ole_selec'),class="info_proc"))
                                
                                )
                      ),
                      fluidRow(
                        column(12,
                               # Filtros dentro de una card
                               #card(
                                 #title = "Filtros", status = "primary", solidHeader = TRUE, width = 12,
                                 fluidRow(
                                  #column(4, sliderInput("slider1", "Velocidad Animación", min = 0, max = 100, value = 50)),
                                   column(3, tooltip(selectInput("dropdown_tab2","Tipo Animación", choices = c("Frecuencia", "Performance")
                                                         , "Frecuencia"),"Frecuencia: muestra cant. de casos de pasan de un nodo a otro. Performance: adiciona los dias promedios que demoran pasar de un nodo a otro",id="tip2",placement="auto")),
                                   #column(3, sliderInput("slider2", "% Casos Visualizados", min = 0, max = 100, value = 70)),
                                   column(3, selectInput("dropdown2_tab2","Primer Actividad", choices = c("Seleccione", actividades)
                                                         , "Seleccione")),
                                   #column(3, selectInput("dropdown3_tab2","Última Actividad", choices = c("Seleccione", actividades)
                                   #                      , "Seleccione"))
                                   column(3,sliderInput("sliderTH", "Casos con Duración (días):", min = 0
                                                        , max = max(lg_causas_judiciales$throughput_time)
                                                        , value = c(0,max(lg_causas_judiciales$throughput_time))
                                                        
                                                       )
                                         )  
                                 ),
                                 fluidRow(
                                    column(3, sliderInput("slider2", "% Casos Visualizados", min = 0, max = 100, value = 70)),
                                    column(3, selectInput("dropdown3_tab2","Última Actividad", choices = c("Seleccione", actividades)
                                                         , "Seleccione")),
                                    column(3, selectInput("dropdown_tieneAct","Contiene la Actividad", choices = c("Seleccione", actividades)
                                                         , "Seleccione")) 
                                 )
                               #)
                        ),
                        fluidRow(
                                 #div(class = "kpi-box", style = "font-size:2rem;", "KPI 1: 123"),
                                 #style = "padding: 10px;"
                                 column(3,card(
                                     card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'># CASOS</p>")),
                                     card_body(textOutput("cant_casos_tab2"), class="kpi_value")
                                 )),
                                 column(3,card(
                                     card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'># PROMEDIO ACTOS PROCESALES</p>")),
                                     card_body(textOutput("prom_actos_proc_tab2"), class="kpi_value")
                                 )),
                                 
                                 column(3,card(
                                     card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'># VARIANTES</p>")),
                                     card_body(textOutput("cant_variantes_tab2"), class="kpi_value")
                                 )),
                                 column(3,card(
                                     card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'>DURACIÓN PROMEDIO (DÍAS)</p>")),
                                     card_body(textOutput("dur_prom_tab2"), class="kpi_value")
                                 ))
                               ),
                        #Graficos presencia activ y tabla detalle
                        fluidRow(
                            # Gráfico de barras horizontales presencia activ proc
                               column(6,
                                 card(title = "Presencia Actividades por Caso", status = "primary", width = 12, solidHeader = TRUE,
                                     plotlyOutput("grafico_barras_peso_activ", height = "520px")
                                     )
                                     ),
                            #tabla detalle
                            column(6,
                                 card(title = "Detalle", status = "primary", width = 12, solidHeader = TRUE,
                                     selectInput("dim_selector","Seleccione una dimensión:"
                                                 , choices = c("Circunscripcion","Objeto Litigio","Materia","Unidad","Estado Procesal")
                                                )
                                      ,card(DTOutput("tabla_detalle_dim", height = "300px"))
                                     )
                               )
                        ),
                          
                        # Componente processAnimater()
                        fluidRow(
                          column(12, #"Flujo del Proceso",
                              card(card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'>FLUJO DEL PROCESO - ESTATICO</p>")),
                                   card_body(grVizOutput("process_plot", width = "100%", height = "90vh"))
                                  )
                          )
                        )
                      )
             ),
             
             # Tab 3: Con filtros específicos y un componente processAnimater()
             tabPanel("Visualización Dinámica de Procesos",value="tab3",
                      fluidRow(
             column(12,
                    uiOutput("notification_banner_tab3")  # Dynamic banner
             )
           ),
                      fluidRow(
                        column(2,
                               # Filtros dentro de una card
                               card(
                                 title = "Filtros", status = "primary", solidHeader = TRUE, width = 12,
                                 fluidRow(
                                   column(12, sliderInput("slider1", "Velocidad Animación", min = 0, max = 100, value = 80)),
                                   #column(4, selectInput("dropdown_tab2","Tipo Animación", choices = c("relative", "absolute"), "relative")),
                                   #column(4, sliderInput("slider2", "% Casos Visualizados", min = 0, max = 100, value = 70))
                                 )
                               )
                        ),
                        fluidRow(
                                 #div(class = "kpi-box", style = "font-size:2rem;", "KPI 1: 123"),
                                 #style = "padding: 10px;"
                                 column(3,card(
                                     card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'># CASOS</p>")),
                                     card_body(textOutput("cant_casos_tab3"), class="kpi_value")
                                 )),
                                 column(3,card(
                                     card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'># PROMEDIO ACTOS PROCESALES</p>")),
                                     card_body(textOutput("prom_actos_proc_tab3"), class="kpi_value")
                                 )),
                                 
                                 column(3,card(
                                     card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'># VARIANTES</p>")),
                                     card_body(textOutput("cant_variantes_tab3"), class="kpi_value")
                                 )),
                                 column(3,card(
                                     card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'>DURACIÓN PROMEDIO (DÍAS)</p>")),
                                     card_body(textOutput("dur_prom_tab3"), class="kpi_value")
                                 ))
                               ),
                        # Componente processAnimater()
                        fluidRow(
                          column(12,
                                 card(card_header(HTML("<p style='font-weight: bold;margin-top: 0;margin-bottom: 0;'>FLUJO DEL PROCESO - DINÁMICO</p>")),
                                      card_body(processanimaterOutput("process_graph", height = "700px"))
                                     )
                          )
                        )
                      )
             ),
             # Tab 4: Tabla de detalle
             tabPanel("Tabla de Detalles",value="tab4",
                      fluidRow(
                        column(12, DTOutput("tabla_detalle"))
                      )
             )
  )
             
)

# Definir el Server
server <- function(input, output, session) {
  
  # Datos simulados para el dataset que alimenta la app
  #datos <- data.frame(
  #  Categoria = c("A", "B", "C", "D"),
  #  Repeticiones = c(10, 15, 7, 12),
  #  Fecha = seq(as.Date("2021-01-01"), by = "day", length.out = 100)  # Simulación de datos de fechas
  #)

  #observeEvent(input$navbar, {
  #  if (input$navbar == "tab2") {
  #    # Show the notification banner
  #    shinyjs::show("notificationBanner")
      
  #  } else {
      # Hide the notification banner when leaving Tab 2
  #    shinyjs::hide("notificationBanner")
        
   # }
  #})
  # Reactive value to track the visibility of the notification banner
  show_banner <- reactiveVal(TRUE)
  
  # Render the notification banner dynamically
  output$notification_banner <- renderUI({
    if (show_banner()) {
      div(
        class = "alert alert-info alert-dismissible fade show",  # Bootstrap alert classes
        role = "alert",
        HTML("INFO: Si no hay ningun filtro seleccionado, por defecto se muestran datos para procesos con Objeto de Litigios (est) ORDINARIO. Para visualizar datos de otros procesos elija la opción deseada en el filtro Objeto Litigio (est). Algunas combinaciones en los filtros pueden no traer información, en ese caso pruebe con otras combinaciones."),
        tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "alert",
          `aria-label` = "Close",
          onclick = "Shiny.setInputValue('close_banner', true, {priority: 'event'})"  # Notify server when closed
        )
      )
    }
  })

    output$notification_banner_tab3 <- renderUI({
    if (show_banner()) {
      div(
        class = "alert alert-info alert-dismissible fade show",  # Bootstrap alert classes
        role = "alert",
        HTML("INFO: Si no hay ningun filtro seleccionado, por defecto se muestran datos para procesos con Objeto de Litigios (est) ORDINARIO. Para visualizar datos de otros procesos elija la opción deseada en el filtro Objeto Litigio (est). Algunas combinaciones en los filtros pueden no traer información, en ese caso pruebe con otras combinaciones."),
        tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "alert",
          `aria-label` = "Close",
          onclick = "Shiny.setInputValue('close_banner', true, {priority: 'event'})"  # Notify server when closed
        )
      )
    }
  })
  
  # Listen for the close button click and hide the banner
  observeEvent(input$close_banner, {
    show_banner(FALSE)
  })
  
  # Reset banner visibility when navigating to Tab 2
  observeEvent(input$navbar, {
    if (input$navbar == "tab2" || input$navbar == "tab3") {
      show_banner(TRUE)  # Show banner when navigating to Tab 2
    }
  })

    
  datos <-  lg_causas_judiciales #%>% filter(causa_id==3440265)
  
  # Reactive expression para filtrar los datos según los inputs
  datos_filtrados <- reactive({
    
      if (input$filtro1 != "Seleccione"){
          #casos <- data.frame(datos %>%filter(circunscripcion_descripcion %in% c(input$filtro1)) %>% group_by(causa_id) %>% count() )
          datos <- datos%>%filter_case_condition( circunscripcion_descripcion == input$filtro1 )#filter(causa_id %in% c(as.list(casos$causa_id)))
          #(circunscripcion_descripcion == input$filtro1)
          #filter( circunscripcion_descripcion == input$filtro1 )
      }
      if (input$filtro2 != "Seleccione"){
          datos <- datos%>%filter_case_condition( materia_descripcion == input$filtro2 )
      }
      if (input$filtro3 != "Seleccione"){
          datos <- datos%>%filter_case_condition(unidad_descripcion == input$filtro3)#filter( unidad_descripcion == input$filtro3 )
      }
      if (input$filtro4 != "Seleccione"){
          datos <- datos%>%filter_case_condition( estado_procesal == input$filtro4 )
      }
      if (input$filtro5 != "Seleccione"){
          datos <- datos%>%filter_case_condition( objeto_litigio_descripcion == input$filtro5 )
      }
      if (input$filtro5_est != "Seleccione"){
          datos <- datos%>%filter_case_condition( objeto_litigio_estadistico == input$filtro5_est )
      }
    
      
      datos[datos$causa_fecha_inicio >= input$rango_fechas[1] & datos$causa_fecha_inicio <= input$rango_fechas[2], ]
  })

  datos_filtrados_pg <- reactive({
      datos <- datos_filtrados()
      datos<- datos %>% filter_throughput_time(interval = c(input$sliderTH), units = "days")
      if(input$filtro5_est == "Seleccione"){
          datos_proceso<-datos%>%
                          filter_case_condition(tolower(objeto_litigio_estadistico)=='ordinario')%>%
                          filter_trace_frequency(percentage = (input$slider2/100))%>%
                          to_eventlog()
      } else {
         datos_proceso<-datos%>%
                          #filter(tolower(objeto_litigio_estadistico)=='ordinario')%>%
                          filter_trace_frequency(percentage = (input$slider2/100))%>%
                          to_eventlog() 
      }
      if(input$dropdown_tieneAct != "Seleccione"){
          datos_proceso<-datos_proceso%>%filter_activity_presence(input$dropdown_tieneAct)
      }
      if(input$dropdown2_tab2 != "Seleccione" & input$dropdown3_tab2 == "Seleccione"){
          datos_proceso<-datos_proceso%>%filter_trim(start_activities = input$dropdown2_tab2 , end_activities =  NULL)
      }
      else if(input$dropdown2_tab2 == "Seleccione" & input$dropdown3_tab2 != "Seleccione"){
          datos_proceso<-datos_proceso%>%filter_trim(start_activities = NULL , end_activities =  input$dropdown3_tab2)
      }
      else if(input$dropdown2_tab2 != "Seleccione" & input$dropdown3_tab2 != "Seleccione"){
          datos_proceso<-datos_proceso%>%filter_trim(start_activities = input$dropdown2_tab2 , end_activities =  input$dropdown3_tab2)
      }
          
      datos_proceso
          
      
  })

  output$info_proceso_ole_selec <- renderText({
      if(input$filtro5_est == "Seleccione"){
          var <- "Información sobre Procesos con Objeto de Litigio (est): ORDINARIO"
      } else{
          var <- paste("Información sobre Procesos con Objeto de Litigio (est): ",input$filtro5_est)
          
      }
      var
      
  })
  # KPI Cant Casos
  output$cant_casos <- renderText({
      datos <- datos_filtrados()
      var <- datos %>% n_cases
      var
      
  })
  # KPI Cant Prom Actos Proc
  output$prom_actos_proc <- renderText({
      datos <- datos_filtrados()
      var <- round(sum(data.frame(datos %>% group_by_case %>% count())$n)/ (datos %>% n_cases),2)
      var
      
  })

    
  # Gráfico de barras horizontales (categorías vs. repeticiones)
  output$grafico_barras_izquierda <- renderPlotly({
    datos <- datos_filtrados()
    df_graf <- data.frame(datos %>% group_by(acto_procesal_descripcion) %>% count(name="Repeticiones"))
    df_graf <- df_graf[order(df_graf$Repeticiones, decreasing = TRUE), ]  %>% slice_head(n=15)
    df_graf <- rename(df_graf,c("Acto_Procesal" = "acto_procesal_descripcion"))
    grafico=ggplot(df_graf, aes(x = Repeticiones, y = reorder(Acto_Procesal, Repeticiones)
                               )) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Repeticiones",y=NULL, title="Repetición de Actos Procesales") +
      theme_minimal()
    ggplotly(grafico,tooltip = "x")
  })
  
  # Gráfico de evolución temporal
  output$grafico_temporal <- renderPlotly({
    datos <- datos_filtrados()
    df_datos<-data.frame(datos[c("causa_fecha_inicio","causa_id")])
    df_datos %>% mutate(Fecha = floor_date(causa_fecha_inicio, "month")) %>% select(Fecha) %>% count(Fecha, name= "Cant_Casos") ->df_agg
    

    grafico=ggplot(df_agg, aes(x = Fecha, y = Cant_Casos)) +
        #geom_line(color = "steelblue") +
        #geom_area(fill="#69b3a2", alpha=0.1) +
      geom_line(color="#69b3a2")+
      labs(x = "Año-Mes", y = "Cantidad de Casos", title = "Evolución Temporal Casos") +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "5 month") + # Formato Año-Mes en eje X
      theme_minimal() +
      theme(axis.text.x = element_text(angle =45, hjust = 1))
    ggplotly(grafico)
  })
  
  # Gráfico de barras horizontales con dropdown (variable seleccionada)
  output$grafico_barras_derecha <- renderPlotly({
    datos <- datos_filtrados()

      
    variable_seleccionada <- case_when(
        input$variable_selector == "Materia"   ~ "materia_descripcion",
        input$variable_selector == "Circunscripcion" ~ "circunscripcion_descripcion",
        input$variable_selector == "Objeto Litigio (est)" ~ "objeto_litigio_estadistico",
        input$variable_selector == "Objeto Litigio" ~ "objeto_litigio_descripcion"
      )
      
    df_datos<-datos[c(toString(variable_seleccionada),"causa_id")]
    names(df_datos)[1] <- "Categoria"
    df_datos  %>% group_by(Categoria,causa_id) %>% count() -> df_datos
    data.frame(df_datos %>% group_by(Categoria) %>% count(name="Cantidad")) -> df_graf
    df_graf%>%drop_na(Categoria)-> df_graf  
    df_graf[order(df_graf$Cantidad, decreasing = TRUE), ] %>% slice_head(n=20) -> datos_variables
    grafico=ggplot(datos_variables, aes(x = Cantidad, y = reorder(Categoria, Cantidad))) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(#x = input$variable_selector, 
           x = "Cantidad Casos",y=NULL, title= paste("Cantidad de Casos según",input$variable_selector, sep=" ")) +
      theme_minimal()
    ggplotly(grafico,tooltip = "x")
  })
  
  

  # KPI Cant Casos tab2
  output$cant_casos_tab2 <- renderText({
      datos <- datos_filtrados_pg()
      var <- datos %>% n_cases
      var
      
  })
  # KPI Cant Prom Actos Proc
  output$prom_actos_proc_tab2 <- renderText({
      datos <- datos_filtrados_pg()
      var <- round(sum(data.frame(datos %>% group_by_case %>% count())$n)/ (datos %>% n_cases),2)
      var
      
  })

 # KPI Cant Variantes
  output$cant_variantes_tab2 <- renderText({
      datos <- datos_filtrados_pg()
      var <- datos %>% n_traces
      var
      
  })
 # KPI Duracion Promedio
  output$dur_prom_tab2 <- renderText({
      datos <- datos_filtrados_pg()
      #datos <- datos %>% filter(!is.na(actividad))
      var <- round(data.frame(datos %>%throughput_time("log",units="days"))$mean)
      var
      
  })

  #grafico peso activ
output$grafico_barras_peso_activ <-renderGrViz({
        datos <- datos_filtrados_pg()
        df_graf<-data.frame(datos %>% filter(!is.na(actividad)) %>%activity_presence() %>% slice_head(n=15))
        df_graf <- rename(df_graf,c( "Peso_Relativo"="relative" ,  "Actividad"= "actividad"))
        grafico=ggplot(df_graf, aes(x = Peso_Relativo, y = reorder(Actividad, Peso_Relativo))) +
              geom_bar(stat = "identity", fill = "steelblue") +
              labs(x = "Peso Relativo",y=NULL, title="Presencia de cada Actividad") +
              theme_minimal()
        ggplotly(grafico,tooltip = "x")
    })
  #tabla detalle dimensiones
  output$tabla_detalle_dim <- renderDT({
      datos <- datos_filtrados_pg()
      th_time <- data.frame(datos%>%throughput_time("case",units="days"))##se sumo
      #datos <- datos%>% throughput_time("case",units="days") %>% augment(datos)
      variable_seleccionada <- case_when(
        input$dim_selector == "Materia"   ~ "materia_descripcion",
        input$dim_selector == "Circunscripcion" ~ "circunscripcion_descripcion",
        input$dim_selector == "Objeto Litigio" ~ "objeto_litigio_descripcion",
        input$dim_selector == "Estado Procesal" ~ "estado_procesal",
        input$dim_selector == "Unidad" ~ "unidad_descripcion"  
      ) 
    
     df_datos<-datos[c(toString(variable_seleccionada),"causa_id"
                       #, "throughput_time"
                      )]
     names(df_datos)[1] <- "Categoria"
     df_datos <- df_datos  %>% group_by(Categoria,causa_id
                                        #,throughput_time
                                       ) %>% count()
     df_datos <- left_join(df_datos, th_time, by = c("causa_id" = "causa_id"))##se sumo
     #data.frame(df_datos %>% group_by(Categoria) %>% count(name="Cantidad")) -> df_graf
     df_datos <- df_datos %>% group_by(Categoria) %>% summarise(Cantidad = n()
                                                                ,TiempoPromedio = round(sum(throughput_time, na.rm = TRUE)/n())
                                                                ,TiempoMin = min(throughput_time)
                                                                ,TiempoMax = max(throughput_time)
                                                                ,DevStandar = round(sd(throughput_time))
                                                                ,CoefVar = round(
                                                                    sd(throughput_time)/abs(as.numeric(sum(throughput_time, na.rm = TRUE)/n()))
                                                                    )*100
                                                               )
     #df_graf[order(df_graf$Cantidad, decreasing = TRUE), ] %>% slice_head(n=20) -> datos_variables
     datatable(df_datos, options = list(scrollX = TRUE, scrollY = TRUE)#,class = 'cell-border stripe hover'
                ,colnames = c("Categoría Agrup." = "Categoria"
                              ,"# Casos" = "Cantidad"
                              , "Dur. Prom. (d)"="TiempoPromedio"
                              , "Dur. Min (d)" = "TiempoMin"
                              , "Dur. Max (d)" = "TiempoMax"
                              , "DS" = "DevStandar"
                              , "CV (%)" = "CoefVar"
                             )
                ,rownames = FALSE 
                ,escape = FALSE
                
              )
  }) 
  # Flujo Estatico Proceso
  output$process_plot <- renderGrViz({
    datos_proceso <-datos_filtrados_pg()

    if (input$dropdown_tab2 == "Frecuencia"){
        plot <- process_map(datos_proceso,type_node=frequency("absolute-case"),sec_node=frequency("relative-case")
                            ,type_edge=frequency("absolute-case")
                            ,render = F)
    }

    if (input$dropdown_tab2 == "Performance"){
        plot <- process_map(datos_proceso,type_node=frequency("absolute-case"),sec_node=frequency("relative-case")
                            ,sec_edge=performance(mean,unit="days",color_edges="dodgerblue4"),type_edge=frequency("absolute-case")
                            ,render = F)
    }

    render_graph(plot)
  })


    
  # KPI Cant Casos tab3
  output$cant_casos_tab3 <- renderText({
      datos <- datos_filtrados_pg()
      var <- datos %>% n_cases
      var
      
  })
  # KPI Cant Prom Actos Proc
  output$prom_actos_proc_tab3 <- renderText({
      datos <- datos_filtrados_pg()
      var <- round(sum(data.frame(datos %>% group_by_case %>% count())$n)/ (datos %>% n_cases),2)
      var
      
  })

 # KPI Cant Variantes
  output$cant_variantes_tab3 <- renderText({
      datos <- datos_filtrados_pg()
      var <- datos %>% n_traces
      var
      
  })
 # KPI Duracion Promedio
  output$dur_prom_tab3 <- renderText({
      datos <- datos_filtrados_pg()
      var <- round(data.frame(datos %>%throughput_time("log",units="days"))$mean)
      var
      
  })
    # Proceso animado (simulación simple con processAnimater)
  output$process_graph <- renderProcessanimater(expr = {
      datos_proceso <-datos_filtrados_pg()
      
      #graph <- processmapR::process_map(datos_proceso, render = F)
      #model <- DiagrammeR::add_global_graph_attrs(graph, attr = "rankdir", value = "vertical", attr_type = "graph")
      animate_process(datos_proceso, #model,
                      mode = "absolute",#input$dropdown_tab2,
                      duration = input$slider1,
                      mapping = token_aes(color = token_scale("red")),
                      initial_state = "paused")


    })
  
  # Mostrar la tabla de detalle en la Tab 3
  output$tabla_detalle <- renderDT({
      datos <- datos_filtrados()
    datatable(datos, options = list(scrollX = TRUE))
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

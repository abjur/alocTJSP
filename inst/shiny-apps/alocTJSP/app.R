library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(stringr)
library(ggplot2)
library(rgdal)

#' @export
matriz <- function(n){
  mat = matrix(0, nrow = 2*n+1, ncol = n^2+n)

  for(i in 1:n){
    for(j in 1:n){
      mat[i,n*(i-1) + j] = 1
    }
  }

  for(i in (n+1):(2*n)){
    for(j in (n+1):(2*n)){
      mat[i,n*(j-n-1)+i-n] = 1
    }
    mat[i,n^2+i-n] = -n
  }


  mat[2*n+1,(n^2+1):(n^2+n)] = rep(1, times = n)
  return(mat)
}

#' @export
i_directions <- function(n) {c(rep('==', times = n),rep("<=",times = n),'==')}

#' @export
i_rhs <- function(n, numvaras){rhs <- c(rep(1,n),rep(0,n),numvaras)}

#' @export
dist_comarcas <- function(d){
  d %>%
    dplyr::filter(stringr::str_detect(comarca,'coma_')) %>%
    dplyr::select(starts_with('coma_')) %>%
    as.matrix() %>%
    igraph::graph.adjacency(mode="undirected") %>%
    igraph::distances() %>%
    as.vector()
}

#' @export
perc_processos <- function(d){
  round(unlist(d[,2]/sum(d[,2])), 4) %>%
    sapply(rep, times = nrow(d))
}

#' @export
objective <- function(d){
  d_comarcas <- dist_comarcas(d)
  p_processos <- perc_processos(d)

  obj = c(p_processos*d_comarcas, rep(0, nrow(d)))
  return(obj)
}

#' @export
aloca <- function(d,numvaras){

  n_comarcas <- nrow(d)
  comarcas <- stringr::str_replace(d$comarca,'coma_','')

  obj <- objective(d)
  mat = matriz(n_comarcas)
  dir <-i_directions(n_comarcas)
  rhs <- i_rhs(n_comarcas, numvaras)
  types <- rep('B', n_comarcas^2+n_comarcas)
  max <- F

  solucao <- Rglpk::Rglpk_solve_LP(obj = obj,mat = mat,dir = dir,rhs = rhs,types = types,max = max,verbosity = 0)
  res <- solucao$optimum
  solution <- solucao %>%
    '$'('solution') %>%
    matrix(nrow = n_comarcas, ncol = n_comarcas, byrow = T) %>%
    reshape2::melt() %>%
    dplyr::filter(value > 0) %>%
    dplyr::transmute(comarca = comarcas[Var1],
                     vara = comarcas[Var2]) %>%
    dplyr::as_data_frame()

  list('alocacao' = solution, 'obj' = res)
}


dd <- readRDS('d_map_comarcas_sp.rds')
coma <- readRDS('d_comarcas.rds')
mat <- readRDS('matriz.rds')
mapa <- readRDS('mapa_sp.rds')

ddd <- select(coma, comarca_foro_distrital_sede, n_processos) %>%
  distinct(comarca_foro_distrital_sede, .keep_all = TRUE)

d_medio <- dd %>%
  select(-n_processos) %>%
  inner_join(ddd, 'comarca_foro_distrital_sede') %>%
  group_by(comarca_foro_distrital_sede, regiao) %>%
  summarise(long = mean(long), lat = mean(lat), n_processos = first(n_processos)) %>%
  ungroup()

# d_medio %>%
#   filter(comarca_foro_distrital_sede != 'SAO PAULO') %>%
#   leaflet() %>%
#   addTiles() %>%
#   # addCircles(radius = ~n_processos * 100) %>%
#   addCircleMarkers(radius = ~n_processos / 10,
#                    popup = ~comarca_foro_distrital_sede)


ui <- dashboardPage(
  dashboardHeader(title = 'Alocomarca'),

  dashboardSidebar(
    selectInput('metrica', 'Métrica',
                c('Número de processos' = 'n_processos',
                  'Número de empresas' = 'n_empresas'),
                selected = 'n_empresas')
  ),

  dashboardBody(
    fluidRow(
      leafletOutput('mapa', width = '100%', height = '600px')
    ),
    fluidRow(
      box(sliderInput('n_regiao01', label = 'Varas na região 01 - Grande São Paulo',
                      value = 0, min = 0, max = 10, step = 1L),
          sliderInput('n_regiao02', label = 'Varas na região 02 - Araçatuba',
                      value = 0, min = 0, max = 10, step = 1L),
          sliderInput('n_regiao03', label = 'Varas na região 03 - Bauru',
                      value = 0, min = 0, max = 10, step = 1L),
          sliderInput('n_regiao04', label = 'Varas na região 04 - Campinas',
                      value = 0, min = 0, max = 10, step = 1L),
          sliderInput('n_regiao05', label = 'Varas na região 05 - Presidente Prudente',
                      value = 0, min = 0, max = 10, step = 1L)
      ),
      box(
          sliderInput('n_regiao06', label = 'Varas na região 06 - Ribeirão Preto',
                      value = 0, min = 0, max = 10, step = 1L),
          sliderInput('n_regiao07', label = 'Varas na região 07 - Santos',
                      value = 0, min = 0, max = 10, step = 1L),
          sliderInput('n_regiao08', label = 'Varas na região 08 - São José do Rio Preto',
                      value = 0, min = 0, max = 10, step = 1L),
          sliderInput('n_regiao09', label = 'Varas na região 09 - São José dos Campos',
                      value = 0, min = 0, max = 10, step = 1L),
          sliderInput('n_regiao10', label = 'Varas na região 10 - Sorocaba',
                      value = 0, min = 0, max = 10, step = 1L))
    )
  )
)

server <- shinyServer(function(input, output, session) {


  dados <- reactive({
    mapa
  })

  aloc10 <- reactive({
    N <- input$n_regiao10
    if (N > 1) {
      r <- mat %>%
        filter(str_detect(regiao, '10')) %>%
        with(regiao) %>%
        unique()
      vv <- mat %>%
        filter(regiao == r) %>%
        distinct(.keep_all = TRUE) %>%
        with(coma1)
      d <- mat %>%
        filter(regiao == r) %>%
        select(comarca = coma1, one_of(input$metrica, vv))
      a <- aloca(d, N)
      res <- a$alocacao
      res$regiao <- r
      res
    }
  })

  aloc9 <- reactive({
    N <- input$n_regiao09
    if (N > 1) {
      r <- mat %>%
        filter(str_detect(regiao, '09')) %>%
        with(regiao) %>%
        unique()
      vv <- mat %>%
        filter(regiao == r) %>%
        distinct(coma1, .keep_all = TRUE) %>%
        with(coma1)
      d <- mat %>%
        filter(regiao == r) %>%
        select(comarca = coma1, one_of(input$metrica, vv))
      a <- aloca(d, N)
      res <- a$alocacao
      res$regiao <- r
      res
    }
  })

  aloc1 <- reactive({
    N <- input$n_regiao01
    if (N > 1) {
      r <- mat %>%
        filter(str_detect(regiao, '01')) %>%
        with(regiao) %>%
        unique()
      vv <- mat %>%
        filter(regiao == r) %>%
        distinct(coma1, .keep_all = TRUE) %>%
        with(coma1)
      d <- mat %>%
        filter(regiao == r) %>%
        select(comarca = coma1, one_of(input$metrica, vv))
      a <- aloca(d, N)
      res <- a$alocacao
      res$regiao <- r
      res
    }
  })

  aloc2 <- reactive({
    N <- input$n_regiao02
    if (N > 1) {
      r <- mat %>%
        filter(str_detect(regiao, '02')) %>%
        with(regiao) %>%
        unique()
      vv <- mat %>%
        filter(regiao == r) %>%
        distinct(coma1, .keep_all = TRUE) %>%
        with(coma1)
      d <- mat %>%
        filter(regiao == r) %>%
        select(comarca = coma1, one_of(input$metrica, vv))
      a <- aloca(d, N)
      res <- a$alocacao
      res$regiao <- r
      res
    }
  })

  aloc3 <- reactive({
    N <- input$n_regiao03
    if (N > 1) {
      r <- mat %>%
        filter(str_detect(regiao, '03')) %>%
        with(regiao) %>%
        unique()
      vv <- mat %>%
        filter(regiao == r) %>%
        distinct(coma1, .keep_all = TRUE) %>%
        with(coma1)
      d <- mat %>%
        filter(regiao == r) %>%
        select(comarca = coma1, one_of(input$metrica, vv))
      a <- aloca(d, N)
      res <- a$alocacao
      res$regiao <- r
      res
    }
  })

  aloc4 <- reactive({
    N <- input$n_regiao04
    if (N > 1) {
      r <- mat %>%
        filter(str_detect(regiao, '04')) %>%
        with(regiao) %>%
        unique()
      vv <- mat %>%
        filter(regiao == r) %>%
        distinct(coma1, .keep_all = TRUE) %>%
        with(coma1)
      d <- mat %>%
        filter(regiao == r) %>%
        select(comarca = coma1, one_of(input$metrica, vv))
      a <- aloca(d, N)
      res <- a$alocacao
      res$regiao <- r
      res
    }
  })

  aloc5 <- reactive({
    N <- input$n_regiao05
    if (N > 1) {
      r <- mat %>%
        filter(str_detect(regiao, '05')) %>%
        with(regiao) %>%
        unique()
      vv <- mat %>%
        filter(regiao == r) %>%
        distinct(coma1, .keep_all = TRUE) %>%
        with(coma1)
      d <- mat %>%
        filter(regiao == r) %>%
        select(comarca = coma1, one_of(input$metrica, vv))
      a <- aloca(d, N)
      res <- a$alocacao
      res$regiao <- r
      res
    }
  })

  aloc6 <- reactive({
    N <- input$n_regiao06
    if (N > 1) {
      r <- mat %>%
        filter(str_detect(regiao, '06')) %>%
        with(regiao) %>%
        unique()
      vv <- mat %>%
        filter(regiao == r) %>%
        distinct(coma1, .keep_all = TRUE) %>%
        with(coma1)
      d <- mat %>%
        filter(regiao == r) %>%
        select(comarca = coma1, one_of(input$metrica, vv))
      a <- aloca(d, N)
      res <- a$alocacao
      res$regiao <- r
      res
    }
  })

  aloc7 <- reactive({
    N <- input$n_regiao07
    if (N > 1) {
      r <- mat %>%
        filter(str_detect(regiao, '07')) %>%
        with(regiao) %>%
        unique()
      vv <- mat %>%
        filter(regiao == r) %>%
        distinct(coma1, .keep_all = TRUE) %>%
        with(coma1)
      d <- mat %>%
        filter(regiao == r) %>%
        select(comarca = coma1, one_of(input$metrica, vv))
      a <- aloca(d, N)
      res <- a$alocacao
      res$regiao <- r
      res
    }
  })

  aloc8 <- reactive({
    N <- input$n_regiao08
    if (N > 1) {
      r <- mat %>%
        filter(str_detect(regiao, '08')) %>%
        with(regiao) %>%
        unique()
      vv <- mat %>%
        filter(regiao == r) %>%
        distinct(coma1, .keep_all = TRUE) %>%
        with(coma1)
      d <- mat %>%
        filter(regiao == r) %>%
        select(comarca = coma1, one_of(input$metrica, vv))
      a <- aloca(d, N)
      res <- a$alocacao
      res$regiao <- r
      res
    }
  })


  alocacao <- reactive({
      d <- bind_rows(
        aloc1(),
        aloc2(),
        aloc3(),
        aloc4(),
        aloc5(),
        aloc6(),
        aloc7(),
        aloc8(),
        aloc9(),
        aloc10()
      )
    d
  })

  output$mapa <- renderLeaflet({
    aloc <- alocacao()
    if (nrow(aloc) > 0) {
      m <- dados()

      d_markers <- d_medio %>%
        semi_join(distinct(aloc, vara, .keep_all = TRUE),
                  c('comarca_foro_distrital_sede' = 'vara'))

      # m@data <- m@data %>%
      #   mutate(ordem = 1:n()) %>%
      #   inner_join(aloc, c('comarca_foro_distrital_sede' = 'comarca', 'regiao')) %>%
      #   arrange(ordem)
      m <- m[m$regiao %in% unique(aloc$regiao), ]
      print(head(m@data))

      copia <- m@data
      copia <- copia %>%
        mutate(ordem = 1:n()) %>%
        inner_join(aloc, c('comarca_foro_distrital_sede' = 'comarca', 'regiao')) %>%
        arrange(ordem)
      m %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(stroke = TRUE,
                    color = 'black',
                    fillOpacity = 0.9,
                    smoothFactor = 0.5,
                    fillColor = colorFactor("YlOrRd", copia$vara)(copia$vara),
                    popup = ~comarca_foro_distrital_sede) %>%
        addMarkers(data = d_markers, popup = ~comarca_foro_distrital_sede)
      # addCircleMarkers(radius = ~n_processos / 10,
      #                  popup = ~comarca_foro_distrital_sede,
      #                  data = filter(d_medio, comarca_foro_distrital_sede != 'SAO PAULO'))

    } else {
      x <- d_medio %>%
        filter(toupper(comarca_foro_distrital_sede) == comarca_foro_distrital_sede)
      x %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(radius = ~n_processos/ 15,
                         color = 'black', weight = 1, opacity = 1,
                         fillOpacity = 0.5,
                         fillColor = ~colorFactor('Blues', x$regiao)(regiao),
                         popup = ~comarca_foro_distrital_sede)
    }
  })

})

# Run the application
shinyApp(ui = ui, server = server)


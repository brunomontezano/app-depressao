library(shiny)

itens_texto <- c(
  "Pouco interesse ou prazer em fazer coisas",
  "Se sentir para baixo, deprimido ou sem esperança",
  "Dificuldade em dormir ou dormir demais",
  "Cansaço ou falta de energia",
  "Falta de apetite ou comer demais",
  "Sentir-se mal consigo mesmo, ou que você é um fracasso ou deixou você
  mo ou sua família",
  "Dificuldade de concentração em coisas, como ler o jornal ou assistir
  tvisão",
  "Movendo-se ou falando tão devagar que outras pessoas poderiam ter notado.
  O oposto - estar inquieto que você tenha sido mais do que o habitual",
  "Pensamentos de que seria melhor estar morto ou de que você ferir-se de
  alguma forma"
)

criar_item <- function(item, enunciado) {
  radioButtons(
    inputId = paste0("a", item),
    label = HTML(paste0(
      "Durante os últimos 14 dias, em quantos foi afetado(a) por:<br>",
      "<b>", enunciado, "</b>"
    )),
    choiceNames = list(
      "Nunca", "Em vários dias", "Em mais da metade dos dias",
      "Em quase todos os dias"
    ),
    choiceValues = list(0, 1, 2, 3),
    width = "100%",
    selected = character(0)
  )
}

ui <- bslib::page_navbar(
  title = "PHQ-9: Questionário sobre a saúde do paciente",
  underline = FALSE,
  fillable = TRUE,
  fillable_mobile = TRUE,
  footer = tags$i("Criado por Bruno Montezano."),
  bslib::nav_spacer(),
  bslib::nav_item(
    tags$a(
      href = "https://github.com/brunomontezano",
      bsicons::bs_icon("github"),
      target = "_blank"
    )
  ),
  bslib::nav_panel_hidden(
    value = "main",
    bslib::navset_hidden(
      id = "hidden_nav",
      bslib::nav_panel_hidden(
        value = "intro",
        p(
          "PHQ-9 é um instrumento auto-aplicável para avaliação de severidade \
          de sintomas depressivos. O instrumento é constituído por 9 questões \
          relacionadas aos sintomas presentes em episódios de humor \
          depressivo. Clique no botão abaixo para iniciar."
        ),
        bslib::layout_columns(
          actionButton(
            inputId = "q1",
            label = "Iniciar auto-aplicação"
          )
        )
      ),
      bslib::nav_panel_hidden(
        value = "q1",
        criar_item(1, itens_texto[1]),
        bslib::layout_columns(
          actionButton(
            inputId = "q2",
            label = "Próximo"
          )
        )
      ),
      bslib::nav_panel_hidden(
        value = "q2",
        criar_item(2, itens_texto[2]),
        bslib::layout_columns(
          actionButton(
            inputId = "q3",
            label = "Próximo"
          )
        )
      ),
      bslib::nav_panel_hidden(
        value = "q3",
        criar_item(3, itens_texto[3]),
        bslib::layout_columns(
          actionButton(
            inputId = "q4",
            label = "Próximo"
          )
        )
      ),
      bslib::nav_panel_hidden(
        value = "q4",
        criar_item(4, itens_texto[4]),
        bslib::layout_columns(
          actionButton(
            inputId = "q5",
            label = "Próximo"
          )
        )
      ),
      bslib::nav_panel_hidden(
        value = "q5",
        criar_item(5, itens_texto[5]),
        bslib::layout_columns(
          actionButton(
            inputId = "q6",
            label = "Próximo"
          )
        )
      ),
      bslib::nav_panel_hidden(
        value = "q6",
        criar_item(6, itens_texto[6]),
        bslib::layout_columns(
          actionButton(
            inputId = "q7",
            label = "Próximo"
          )
        )
      ),
      bslib::nav_panel_hidden(
        value = "q7",
        criar_item(7, itens_texto[7]),
        bslib::layout_columns(
          actionButton(
            inputId = "q8",
            label = "Próximo"
          )
        )
      ),
      bslib::nav_panel_hidden(
        value = "q8",
        criar_item(8, itens_texto[8]),
        bslib::layout_columns(
          actionButton(
            inputId = "q9",
            label = "Próximo"
          )
        )
      ),
      bslib::nav_panel_hidden(
        value = "q9",
        criar_item(9, itens_texto[9]),
        bslib::layout_columns(
          actionButton(
            inputId = "resultados",
            label = "Finalizar e ver resultados"
          )
        )
      ),
      bslib::nav_panel_hidden(
        value = "resultados",
        h3("Resultados"),
        tags$i("O escore da PHQ-9 pode variar de 0 a 27. A escala trata-se de
          uma ferramenta para triagem, e não deve ser considerada como um
          diagnóstico definitivo. Em caso de dúvidas, consulte um profissional
          de saúde."),
        textOutput("escore"),
        DT::DTOutput("tabela"),
        bslib::layout_columns(
          actionButton(
            inputId = "reiniciar",
            label = "Reiniciar questionário"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  itens_id <- paste0("q", 1:9)

  purrr::walk2(
    c(itens_id, "resultados", "reiniciar"),
    c(itens_id, "resultados", "q1"),
    ~ observeEvent(input[[.x]], {
      bslib::nav_select(
        id = "hidden_nav",
        selected = .y
      )
    })
  )

  observeEvent(
    input$resultados,
    {
      score <- sum(
        as.numeric(input$a1),
        as.numeric(input$a2),
        as.numeric(input$a3),
        as.numeric(input$a4),
        as.numeric(input$a5),
        as.numeric(input$a6),
        as.numeric(input$a7),
        as.numeric(input$a8),
        as.numeric(input$a9)
      )
      output$escore <- renderText({
        paste0("O seu escore foi: ", score)
      })
    }
  )

  tabela_interpretacao <- data.frame(
    Escore = c("0-4", "5-9", "10-14", "15-19", "20-27"),
    Severidade = c(
      "Depressão mínima ou nenhuma depressão",
      "Depressão leve",
      "Depressão moderada",
      "Depressão moderadamente severa",
      "Depressão severa"
    )
  )

  output$tabela <- DT::renderDT(
    {
      tabela_interpretacao
    },
    selection = "none",
    rownames = FALSE,
    options = list(
      dom = "t",
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE
    )
  )

  observeEvent(input$reiniciar, {
    purrr::walk(1:9, ~ {
      updateRadioButtons(session, paste0("a", .x), selected = character(0))
    })
  })
}

shinyApp(ui, server)

library(reactable)
library(DT)
library(gt)

library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinyjs)
library(cicerone)

library(magrittr)
library(gemini.R)

ui <- page_sidebar(
  title =
    div(
      "Minesweeper with Reactable",
      a(
        href = "https://github.com/glin/reactable",
        target = "_blank",
        img(
          src = "https://avatars.githubusercontent.com/u/20401866?s=48&v=4",
          style = "height: 2em;"
        )
      ),
      "and gt",
      a(
        href = "https://gt.rstudio.com/index.html",
        target = "_blank",
        img(
          src = "https://github.com/rstudio/gt/raw/master/man/figures/logo.svg",
          style = "height: 2em;"
        )
      ),
      "and DT",
      a(
        href = "https://rstudio.github.io/DT/",
        target = "_blank",
        img(
          src = "https://avatars.githubusercontent.com/u/513560?s=48&v=4",
          style = "height: 2em;"
        )
      ),
      "and Gemini",
      a(
        href = "https://gemini.google.com/",
        target = "_blank",
        bs_icon("stars")
      )
    ),
  tagList(
    use_cicerone(),
    useShinyjs(),
    tags$head(
      tags$style(
        HTML("
        .card {
          height: 100%;
          display: flex;
          flex-direction: column;
        }
        .reactable {
          flex-grow: 1;
          overflow: auto;
        }
        .gt_table{
          width: 100% !important;
        }
      ")
      )
    )
  ),
  sidebar = sidebar(
    width = "25%",
    open = "always",
    style = "height: 100%;",
    radioGroupButtons(
      "size",
      "Size",
      choiceNames = c("S", "M", "L"),
      choiceValues = c(5, 9, 12),
      status = "secondary",
      justified = TRUE
    ),
    layout_columns(
      numericInput("seed", label = NULL, value = 123, min = 1, max = 1000),
      input_task_button(
        "generate",
        label = NULL,
        icon = bs_icon("dice-5"),
        type = "warning",
        style = "font-size: 75%;"
      ) |>
        tooltip("Generate Random Seed")
    ),
    layout_columns(
      input_task_button(
        "start",
        label = NULL,
        icon = bs_icon("caret-right-fill"),
        type = "success"
      ) |>
        tooltip("Start the Game"),
      input_task_button(
        "reset",
        label = NULL,
        icon = bs_icon("arrow-repeat"),
        type = "danger"
      ) |>
        tooltip("Reset the Game"),
      input_task_button(
        "help",
        label = NULL,
        icon = bs_icon("question-circle"),
        type = "info"
      ) |>
        tooltip("Show guide")
    ),
    hr(),
    textInput(
      "API",
      label = popover(
        trigger = list(
          "Gemini API Key",
          bs_icon("box-arrow-up-right")
        ),
        HTML("<a href = 'https://makersuite.google.com/app/apikey' target = '_blank'>Get API</a>")
      )
    ),
    actionButton("submit", "Submit"),
    shinyjs::hidden(
      tagList(
        input_task_button(
          id = "Gemini",
          label = "Ask to Gemini",
          icon = bs_icon("stars"),
          type = "dark"
        ),
        verbatimTextOutput("Gemini.Says"),
      )
    ),
    dataTableOutput("about.dt"),
    gt_output("document.gt")
  ),
  navset_card_pill(
    nav_panel(
      "Map",
      card(
        reactableOutput("mineSweeperBoard", height = "100%"),
        shinyjs::hidden(actionButton(inputId = "foo", label = "foo")),
        card_footer("Table 1: Minesweeper Board with reactable")
      )
    ),
    nav_panel(
      "Answer",
      card(
        reactableOutput("AnswerBoard", height = "100%"),
        card_footer("Table 2: Answer Board with reactable")
      )
    )
  )
)

color_text <- function(value, revealed) {
  if (!revealed) {
    return("")
  }
  if (value == -1) {
    return('<span style="color: red;">💣</span>')
  }
  colors <- c("blue", "green", "red", "purple", "orange", "brown", "cyan", "magenta")
  color <- if (value == 0) "lightgrey" else colors[value]
  sprintf('<span style="color: %s; font-size: 2em;">%s</span>', color, value)
}

initialize_game <- function(size, seed) {
  mines <- round(size * size * 0.1)
  board <- matrix(0, nrow = size, ncol = size, byrow = TRUE)
  set.seed(seed)
  mine_positions <- sample(1:(size * size), mines)
  board[mine_positions] <- -1

  for (i in seq_len(size)) {
    for (j in seq_len(size)) {
      if (board[i, j] != -1) {
        neighbors <- board[max(1, i - 1):min(size, i + 1), max(1, j - 1):min(size, j + 1)]
        board[i, j] <- sum(neighbors == -1)
      }
    }
  }
  colnames(board) <- 1:size
  return(board)
}

check_game_clear <- function(board, revealed) {
  non_mine_cells <- which(board != -1)
  all(revealed[non_mine_cells])
}

server <- function(input, output, session) {
  ## 1. GAME

  observeEvent(input$generate, {
    updateNumericInput(session, "seed", value = sample(1:1000, 1))
  })

  game_state <- reactiveValues()

  observeEvent(input$start, {
    game_state$board <- initialize_game(as.numeric(input$size), input$seed)
    game_state$revealed <- matrix(FALSE, nrow = as.numeric(input$size), ncol = as.numeric(input$size), byrow = TRUE)
    game_state$game_over <- FALSE
    shinyjs::show(id = "Answer")
  })

  observeEvent(input$reset, {
    game_state$board <- initialize_game(as.numeric(input$size), input$seed)
    game_state$revealed <- matrix(FALSE, nrow = as.numeric(input$size), ncol = as.numeric(input$size), byrow = TRUE)
    game_state$game_over <- FALSE

    shinyjs::hide("Answer")
    output$AnswerBoard <- renderReactable({
      NULL
    })
  })

  ## 2. reactable

  output$mineSweeperBoard <- renderReactable({
    req(game_state$board)
    board <- game_state$board
    revealed <- game_state$revealed

    board_df <- as.data.frame(board)
    colnames(board_df) <- paste0("C", seq_len(ncol(board_df)))
    rownames(board_df) <- paste0("R", seq_len(nrow(board_df)))

    board_df[] <- lapply(seq_len(nrow(board_df)), function(i) {
      sapply(seq_len(ncol(board_df)), function(j) {
        color_text(board_df[i, j], revealed[i, j])
      })
    })

    reactable(
      board_df,
      onClick = JS("function(rowInfo, column) {
        Shiny.setInputValue('cell_click', { row: rowInfo.index + 1, column: column.id });
      }"),
      defaultColDef = colDef(
        align = "center",
        vAlign = "center",
        html = TRUE
      ),
      columns = list(
        .rownames = colDef(
          width = 50,
          html = TRUE,
          cell = function(value) {
            sprintf('<span style="font-weight: 600;">%s</span>', value)
          }
        )
      ),
      pagination = FALSE,
      sortable = FALSE,
      bordered = TRUE,
      highlight = TRUE,
      theme = reactableTheme(tableBodyStyle = list(flex = "auto"))
    )
  })

  observeEvent(input$cell_click, {
    if (game_state$game_over) {
      return()
    }

    click <- input$cell_click

    row <- click$row
    col <- as.numeric(gsub("C", "", click$col))
    if (is.na(col)) {
      return()
    } # clicked rownames
    # NOTE data frame handles column first so reverse it.

    if (game_state$board[col, row] == -1) {
      showModal(modalDialog(
        title = "Game Over",
        "You clicked on a mine!",
        easyClose = TRUE
      ))
      game_state$game_over <- TRUE
    } else {
      game_state$revealed[col, row] <- TRUE

      if (check_game_clear(game_state$board, game_state$revealed)) {
        showModal(modalDialog(
          title = "Congratulations!",
          "You have cleared the game!",
          easyClose = TRUE
        ))
        game_state$game_over <- TRUE
        shinyjs::click("Answer")
      }
    }
  })

  output$AnswerBoard <- renderReactable({
    if (is.null(game_state$board)) {
      return(NULL)
    }
    board <- t(game_state$board)
    board_df <- as.data.frame(board)
    colnames(board_df) <- paste0("AC", seq_len(ncol(board_df)))
    rownames(board_df) <- paste0("AR", seq_len(nrow(board_df)))

    reactable(
      board_df,
      defaultColDef = colDef(
        align = "center",
        vAlign = "center",
        html = TRUE,
        cell = function(value) {
          color_text(value, TRUE)
        }
      ),
      columns = list(
        .rownames = colDef(
          width = 50,
          html = TRUE,
          cell = function(value) {
            sprintf('<span style="font-weight: 600;">%s</span>', value)
          }
        )
      ),
      pagination = FALSE,
      sortable = FALSE,
      bordered = TRUE,
      theme = reactableTheme(tableBodyStyle = list(flex = "auto"))
    )
  })

  ## 3. dt

  output$about.dt <- renderDataTable({
    datatable(
      data.frame(c('<button 
                      type="button" 
                      class="btn btn-primary" 
                      style="width:100%"
                      onclick="window.open(`https://github.com/jhk0530/mine.tables`)"
                      >See code</button>')),
      escape = FALSE,
      colnames = NULL,
      options = list(
        dom = "tp",
        ordering = FALSE,
        searching = FALSE
      ),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: bottom; text-align: center;",
        "Table 3: Code Button with DT"
      )
    )
  })

  ## 4. gt (documents)

  output$document.gt <- render_gt({
    data.frame(intToUtf8(160)) |>
      gt() |>
      tab_header(
        title = gt::html('<button type="button" class="btn btn-primary" style="width:100%" onclick="Shiny.setInputValue(`foo`, {priority: `event`});">Documents</button>')
      ) |>
      tab_options(
        column_labels.hidden = TRUE
      ) |>
      tab_style(
        style = cell_borders(sides = "all", weight = px(0)),
        locations = cells_body()
      ) |>
      tab_footnote("Table 4: Document button with gt")
  })

  observeEvent(input$foo, {
    shinyjs::runjs('window.open("https://jhk0530.github.io/mine.tables/")')
  })

  # 5. gemini
  observeEvent(input$submit, {
    if (input$API == "") {
      shiny::showNotification(
        type = "warning",
        "API Key not exist",
        duration = 3
      )
      return(NULL)
    }

    shiny::showNotification(
      type = "message",
      "API Key Saved",
      duration = 3
    )

    setAPI(input$API)
    shinyjs::hide("API")
    shinyjs::hide("submit")
    shinyjs::show("Gemini")
    shinyjs::show("Gemini.Says")
  })

  answer <- eventReactive(input$Gemini, {
    board <- game_state$board
    revealed <- game_state$revealed

    board_gemini <- board

    # if revealed, board_gemini gets value of board else NA
    for (i in seq_len(nrow(board))) {
      for (j in seq_len(ncol(board))) {
        if (!revealed[i, j]) {
          board_gemini[i, j] <- NA
        }
      }
    }

    board_gemini <- paste0(board_gemini, collapse = ", ")

    text1 <- "As minesweeper with below: what can be next action?, "
    text2 <- paste0("Dimension of map is ", as.numeric(input$size), ", ")
    text3 <- paste0("game status is, ", board_gemini, ", NA is not clicked cell, ")
    text4 <- "answer can have format with combination of Ri, Cj: i is row starts from 1, j is column starts from 1"

    text <- paste0(text1, text2, text3, text4)

    answer <- NULL
    answer <- gemini(text)
    while (is.null(answer)) {
      Sys.sleep(1)
    }
    return(unname(answer))
  })

  output$Gemini.Says <- renderPrint({
    answer()
  })

  # 5. cicerone
  observeEvent(input$help, {
    guide <- Cicerone$
      new()$
      step(
      el = "size",
      title = "1. size",
      description = "Select map's size. S: 5x5, M: 9x9, L: 12x12, Mine will be 15%"
    )$
      step(
      el = "generate",
      title = "2. seed",
      description = "randomly choose seed"
    )$
      step(
      el = "seed",
      title = "3. seed",
      description = "or manually select number between 1 ~ 1000"
    )$
      step(
      el = "start",
      title = "3. Start",
      description = "Now game start !"
    )


    guide$init()$start()
  })
}

shinyApp(ui, server)

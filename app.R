########################################################################################################################
# Diagonal Shaded Square Word Puzzle Generator                                                                         #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2019-07-04                                                                                                     #
#                                                                                                                      #
# This Shiny app lets people generate their very own diagonal shaded square word puzzles. The generator takes a word   #
# input that slots in the diagonal shaded squares, and then it generates all possible valid words in the Grady         #
# Augmented dictionary that fits each row of the square. It also allows people to fill in clues to let others guess    #
# each word combo, and generates an image of the completed puzzle.                                                     #
########################################################################################################################

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(qdapDictionaries)

puzzle_generator <- function(word, dictionary) {
    possible_answers <- vector(mode="list", length=nchar(word))
    
    for (i in seq_len(nchar(word))) {
        character_length <- nchar(word)
        letter <- str_sub(word, i, i)
        
        long_word <- dictionary %>%
            tibble(long_word=.) %>%
            filter(str_detect(long_word, "[^a-z]", negate=TRUE),
                   nchar(long_word)==character_length)
        
        short_word <- dictionary %>%
            tibble(short_word=.) %>%
            filter(str_detect(short_word, "[^a-z]", negate=TRUE),
                   nchar(short_word)==character_length-1)
        
        possible_answers[[i]] <- long_word %>%
            filter(str_sub(long_word, i, i)==letter) %>%
            mutate(short_word=`str_sub<-`(string=long_word, start=i, end=i, value="")) %>%
            inner_join(short_word, by="short_word") %>%
            mutate(display=toupper(paste(short_word, long_word, sep="/"))) %>%
            pull(display)
    }
    
    return(possible_answers)
}

########################################################################################################################
# Build the dictionaries. I'm putting this here outside of the ui/server objects because I want to do these once,      #
# instead of recalculating this every time a reactive value updates.                                                   #
########################################################################################################################

data("DICTIONARY")
data("NAMES")
data("GradyAugmented")

main_dictionary <- tibble(words=c(DICTIONARY$word, NAMES$name)) %>%
    mutate(words=tolower(words)) %>%
    distinct() %>%
    filter(str_detect(words, "[^a-z]", negate=TRUE)) %>%
    arrange(words) %>%
    pull(words)

extended_dictionary <- tibble(words=GradyAugmented) %>%
    filter(str_detect(words, "[^a-z]", negate=TRUE)) %>%
    arrange(words) %>%
    pull(words)

########################################################################################################################
# Create custom Javascript functions that will be called by shinyjs.                                                   #
########################################################################################################################

# When added as a script tag in the head, this creates a custom HTML tag that adds functionality for the Enter key.
enter_submit <- '$(function() {
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
                 });'

# This R function tells Shiny what the loading indicator should look like and where to display it.
withBusyIndicatorUI <- function(button) {
    id <- button[['attribs']][['id']]
    div(
        singleton(tags$head(
            tags$style(".btn-loading-container {
                        margin-left: 10px;
                        font-size: 1.2em;
                        }
                        .btn-done-indicator {
                        color: green;
                        }")
        )),
        `data-for-btn` = id,
        button,
        span(
            class = "btn-loading-container",
            shinyjs::hidden(
                icon("spinner", class = "btn-loading-indicator fa-spin"),
                icon("check", class = "btn-done-indicator")
            )
        )
    )
}

# This R function tells Shiny how the loading indicator works.
withBusyIndicatorServer <- function(buttonId, expr) {
    loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
    doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
    shinyjs::disable(buttonId)
    shinyjs::show(selector = loadingEl)
    shinyjs::hide(selector = doneEl)
    on.exit({
        shinyjs::enable(buttonId)
        shinyjs::hide(selector = loadingEl)
    })
    
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
}

########################################################################################################################
# Build the ui object.                                                                                                 #
########################################################################################################################

ui <- fluidPage(
    useShinyjs(),
    tags$head(tags$script(HTML(enter_submit))),
    titlePanel("Diagonal Shaded Square Puzzle Generator"),
    sidebarLayout(
        sidebarPanel(
            # I'm using the enter_submit JS function here to trigger the submit button when Enter is pressed in the
            # diagonal word text input.
            tagAppendAttributes(
                textInput(inputId="diagonal_word",
                          label="Diagonal Word"),
                `data-proxy-click` = "submit_word"
            ),
            # By wrapping the submit button in the withBusyIndicatorUI() function, I'm activating the loading indicator.
            withBusyIndicatorUI(
                actionButton(inputId="submit_word",
                             label="Submit Diagonal Word",
                             width="90%",
                             style="font-weight: bold;
                                    color: #ffffff;
                                    background-color: #42adf4;
                                    border-color: #000000;")
            ),
            uiOutput(outputId="word_selectors")
        ),
        mainPanel(
            plotOutput(outputId="puzzle"),
            uiOutput(outputId="clue_inputs")
        )
    )
)

########################################################################################################################
# Build the server object.                                                                                             #
########################################################################################################################

server <- function(input, output) {
    diagonal_word <- eventReactive(input$submit_word, {
        toupper(input$diagonal_word)
    })
    
    diagonal_word_nchar <- eventReactive(input$submit_word, {
        nchar(input$diagonal_word)
    })
    
    dictionary_choice <- reactive({
        if (is.null(input$dictionary_toggle)) {
            main_dictionary
        } else if (input$dictionary_toggle==FALSE) {
            main_dictionary
        } else if (input$dictionary_toggle==TRUE) {
            extended_dictionary
        }
    })
    
    generated_words <- eventReactive(c(input$submit_word, input$dictionary_toggle), {
        # This triggers the busy indicator when the submit button is pressed.
        withBusyIndicatorServer("submit_word", {
            puzzle_generator(input$diagonal_word, dictionary_choice())
        })
    })
    
    # This chunk creates a dynamic renderUI() object, which uses the map() function to generate the number of selector
    # boxes needed.
    output$word_selectors <- renderUI({
        if (diagonal_word_nchar() > 0) {
            fluidPage(
                hr(),
                materialSwitch(inputId="dictionary_toggle",
                               label="Use Extended Dictionary",
                               value=ifelse(length(dictionary_choice())==length(main_dictionary), FALSE, TRUE),
                               status="success"),
                map(seq_len(diagonal_word_nchar()), function(i) {
                    selectInput(inputId=paste("word", i, sep="_"),
                                label=str_sub(diagonal_word(), i, i),
                                # The choices argument in selectInput() allows for a blank first element, where the name
                                # of the blank first element will be interpreted as a placeholder. I'm using setNames()
                                # to dynamically update the name, but since I'm doing that, I have to give blank string
                                # names to the rest of the choices as well.
                                choices=setNames(c("", generated_words()[[i]]),
                                                 c(paste(length(generated_words()[[i]]),
                                                         if_else(length(generated_words()[[i]])==1,
                                                                 "word combination found",
                                                                 "word combinations found")),
                                                   rep("", length(generated_words()[[i]])))))
                }),
                hr(),
                materialSwitch(inputId="show_answers",
                               label="Show Answers",
                               value=TRUE,
                               status="primary"),
                downloadButton(outputId="download_puzzle",
                               label="Download Puzzle")
            )
        }
    })
    
    # I had to use eval(parse()) here because the number of input$word_X inputs is dynamic. This evaluates that string
    # as an expression and combines all word inputs into a single vector, then takes that back to generated_words() and
    # finds the long and short words for each word choice.
    selected_words <- reactive({
        statement <- map_chr(seq_len(diagonal_word_nchar()), ~paste0("input$word_", .x)) %>%
            paste(collapse=", ") %>%
            paste0("c(", ., ")")
        
        eval(parse(text=statement)) %>%
            enframe(name="word_id", value="display_word") %>%
            separate(display_word, c("short_word", "long_word"), sep="/", remove=TRUE, fill="right") %>%
            # There doesn't appear a way to fill "both" with tidyr::separate(), so simulate it here.
            mutate(short_word=if_else(is.na(long_word), NA_character_, short_word))
    })
    
    output$clue_inputs <- renderUI({
        if (diagonal_word_nchar() > 0) {
            fluidPage(
                map(seq_len(diagonal_word_nchar()), function(i) {
                    fluidRow(
                        column(5,
                               textInput(inputId=paste0("clue_L", i),
                                         label=if_else(is.na(selected_words()$short_word[i]),
                                                       paste("Short Clue", i),
                                                       selected_words()$short_word[i]),
                                         # Setting the default value as this value ensures that the inputted text is not
                                         # cleared away when redrawing the labels on these text inputs. Also, wrapping
                                         # this in an isolate() call means that it won't keep trying to update this
                                         # every time entered_clues() is changed (which is constantly). Removing the
                                         # isolate() wrapper doesn't change any functionality, but every time
                                         # entered_clues() is recalculated, the text input box loses focus.
                                         value=isolate(entered_clues()$left_clue[i]),
                                         width="100%")
                        ),
                        column(2),
                        column(5,
                               textInput(inputId=paste0("clue_R", i),
                                         label=if_else(is.na(selected_words()$long_word[i]),
                                                       paste("Long Clue", i),
                                                       selected_words()$long_word[i]),
                                         value=isolate(entered_clues()$right_clue[i]),
                                         width="100%")
                        )
                    )
                })
            )
        }
    })
    
    entered_clues <- reactive({
        left_statement <- map_chr(seq_len(diagonal_word_nchar()), ~paste0("input$clue_L", .x)) %>%
            paste(collapse=", ") %>%
            paste0("c(", ., ")")
        
        right_statement <- map_chr(seq_len(diagonal_word_nchar()), ~paste0("input$clue_R", .x)) %>%
            paste(collapse=", ") %>%
            paste0("c(", ., ")")
        
        if (!is.null(input$clue_L1)) {
            tibble(left_clue=eval(parse(text=left_statement)),
                   right_clue=eval(parse(text=right_statement)),
                   left_placement=0,
                   right_placement=1+diagonal_word_nchar()) %>%
                mutate(word_id=row_number())
        }
    })
    
    # This builds the table that contains the puzzle data. The placeholder object builds a blank puzzle if no words are
    # chosen yet in the sidebar.
    puzzle_data <- reactive({
        if (nrow(selected_words()) > 0) {
            if (any(!is.na(selected_words()$long_word))==TRUE) {
                real_puzzle_data <- selected_words() %>%
                    select(word_id, long_word) %>%
                    mutate(long_word=toupper(long_word),
                           long_word=str_split(long_word, "")) %>%
                    unnest(long_word) %>%
                    group_by(word_id) %>%
                    mutate(letter_id=row_number()) %>%
                    ungroup() %>%
                    rename(letter=long_word)
            } else {
                real_puzzle_data <- tibble(letter=NA_character_,
                                           word_id=NA_integer_,
                                           letter_id=NA_integer_)
            }
            
            placeholder_puzzle_data <- crossing(word_id=seq_len(diagonal_word_nchar()),
                                                letter_id=seq_len(diagonal_word_nchar())) %>%
                mutate(blank="")
            
            real_puzzle_data %>%
                right_join(placeholder_puzzle_data, by=c("word_id", "letter_id")) %>%
                mutate(letter=if_else(is.na(letter), blank, letter),
                       letter=if_else(word_id==letter_id, str_sub(diagonal_word(), word_id, letter_id), letter),
                       fill_color=if_else(word_id==letter_id, "black", "white"),
                       text_color=if_else(word_id==letter_id, "white", "black"),
                       # Hide the letters if the answer switch is flipped.
                       show_letters=input$show_answers,
                       letter=if_else(show_letters==FALSE, blank, letter))
        }
    })
    
    puzzle <- reactive({
        if (!is.null(puzzle_data())) {
            ggplot() +
                geom_tile(data=puzzle_data(),
                          aes(x=word_id, y=letter_id, fill=fill_color), color="black") +
                geom_text(data=puzzle_data(),
                          aes(x=letter_id, y=word_id, label=letter, color=text_color), size=60/diagonal_word_nchar()) +
                geom_text(data=entered_clues(),
                          aes(x=left_placement, y=word_id, label=left_clue), hjust=1, size=40/diagonal_word_nchar()) +
                geom_text(data=entered_clues(),
                          aes(x=right_placement, y=word_id, label=right_clue), hjust=0, size=40/diagonal_word_nchar()) +
                scale_x_continuous(limits=c(-6, 7+diagonal_word_nchar())) +
                scale_y_reverse() +
                scale_fill_manual(values=c("black"="#000000", "white"="#FFFFFF")) +
                scale_color_manual(values=c("black"="#000000", "white"="#FFFFFF")) +
                coord_equal() +
                theme_void() +
                theme(legend.position="none")
        }
    })

    output$puzzle <- renderPlot({
        puzzle()
    })
    
    output$download_puzzle <- downloadHandler(
        filename=function() {
            paste0("puzzle_", Sys.Date(), ".png")
        },
        content=function(file) {
            ggsave(file, plot=puzzle(), device="png", width=10, height=4, units="in")
        },
        contentType="image/png"
    )
}

########################################################################################################################
# Run the app.                                                                                                         #
########################################################################################################################

shinyApp(ui, server)

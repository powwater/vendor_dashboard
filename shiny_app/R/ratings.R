
star_icon <- function(empty = FALSE, class_) {
    shiny::icon("star", class = class_,  style = paste("color: ", if (empty) "#edf0f2" else "orange"))
}


rating_stars <- function(rating, max_rating = 5, span = FALSE, class = NULL) {

  class_ <- class

  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) star_icon(empty = FALSE, class_ = class_) else star_icon(empty = TRUE, class_ = class_)
  })

  label <- sprintf("%s out of %s", rating, max_rating)

  if (!span) {
    out <- div(title = label, "aria-label" = label, role = "img", stars)
    return(out)
  }

  span(title = label, "aria-label" = label, role = "img", stars)
}

#' @importFrom shiny icon tagAppendAttributes
#' @importFrom tidyselect all_of
ratings_to_stars <- function(dat, cols = c("rating")) {

  mutate_at(dat, vars(tidyselect::all_of(cols)), function(rating) {
    rating = case_when(
      is.na(rating) | is.null(rating) ~ paste0(tags$span(shiny::icon('ban'))),
      rating == 0 ~ paste0(tags$span(shiny::icon('ban'))),
      rating == 0.5 ~ paste0(tags$span(
        shiny::icon('star-half') %>% shiny::tagAppendAttributes(style = "color: orange;")
      )),
      rating == 1 ~ paste0(tags$span(
        shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
      )),
      rating == 1.5 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star-half') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 2 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 2.5 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star-half') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 3 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 3.5 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star-half') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 4 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 4.5 ~ paste0(
        tagList(
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star-half') %>% shiny::tagAppendAttributes(style = "color: orange;")
          )
        )
      ),
      rating == 5 ~ paste0(
        tagList(
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          )
        )
      )
    )
  })
}

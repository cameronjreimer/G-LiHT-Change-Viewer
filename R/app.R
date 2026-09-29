
# Shiny app: compare pre- (2017) and post-hurricane (2018) imagery by quadrat
# launch with RStudio's "Run App" button (working directory = R/)

library(shiny)
library(bslib)
library(terra)

# load quadrats; user-facing quadrat number = quadrat_nu + 1 (1-96)
q40 <- vect('../data/shapefiles/LFDP/LFDP_window_40.shp')
q40 <- q40[order(q40$quadrat_nu)]
q40$quadrat <- q40$quadrat_nu + 1
plot_outline <- aggregate(q40)
n_quad <- nrow(q40)

# load high-res imagery
pre <- rast('../data/imagery/cropped/LFDP_2017.tif')
post <- rast('../data/imagery/cropped/LFDP_2018.tif')

# load CHMs
pre_chm <- rast('../data/imagery/cropped/LFDP_CHM_2017_1m.tiff')
post_chm <- rast('../data/imagery/cropped/LFDP_CHM_2018_1m.tiff')

# load CHM height loss (absolute loss flipped so loss is positive)
abs_loss <- rast('../data/imagery/cropped/LFDP_CHM_2017_2018_Absolute_Height_Loss.tif') * -1
rel_loss <- rast('../data/imagery/cropped/LFDP_CHM_2017_2018_Relative_Height_Loss.tif')

# fixed CHM height range so pre and post brightness are comparable
chm_max <- ceiling(max(
  global(crop(pre_chm, plot_outline), 'max', na.rm = TRUE)[[1]],
  global(crop(post_chm, plot_outline), 'max', na.rm = TRUE)[[1]]
))
chm_cols <- map.pal('grey', 100)

# fixed loss ranges so colours are comparable across quadrats
abs_loss_max <- ceiling(global(crop(abs_loss, plot_outline), 'max', na.rm = TRUE)[[1]])
loss_cols <- map.pal('viridis', 100)

# every panel uses the same margins (with room for a legend on the right) and
# the quadrat's exact extent, so images line up between the top and bottom rows
panel_mar <- c(0.5, 0.5, 1.5, 4)

plot_rgb <- function(r, w) plotRGB(crop(r, w), ext = ext(w), mar = panel_mar)

plot_scalar <- function(r, w, col, range, legend) {
  plot(crop(r, w, snap = 'out'), ext = ext(w), axes = FALSE, col = col, range = range,
       mar = panel_mar, plg = list(title = legend))
}
plot_chm <- function(chm, w) plot_scalar(chm, w, chm_cols, c(0, chm_max), 'Height (m)')

# the four image panels, in the order the full-screen viewer cycles through.
# each panel has one or more views; panels with several views get a toggle
view <- function(label, title, draw) list(label = label, title = title, draw = draw)
panels <- list(
  pre_rgb = list(title = 'Pre-hurricane RGB (2017)', views = list(
    view('2017', 'Pre-hurricane RGB (2017)', function(w) plot_rgb(pre, w))
  )),
  post_rgb = list(title = 'Post-hurricane RGB (2018)', views = list(
    view('2018', 'Post-hurricane RGB (2018)', function(w) plot_rgb(post, w))
  )),
  loss = list(title = 'CHM height loss', views = list(
    view('Absolute', 'Absolute CHM height loss, 2017-2018 (m)',
         function(w) plot_scalar(abs_loss, w, loss_cols, c(0, abs_loss_max), 'Loss (m)')),
    view('Relative', 'Relative CHM height loss, 2017-2018 (%)',
         function(w) plot_scalar(rel_loss, w, loss_cols, c(0, 100), 'Loss (%)'))
  )),
  chm = list(title = 'CHM', views = list(
    view('2017', 'Pre-hurricane CHM (2017)', function(w) plot_chm(pre_chm, w)),
    view('2018', 'Post-hurricane CHM (2018)', function(w) plot_chm(post_chm, w))
  ))
)
n_panels <- length(panels)


# a card whose plot stretches to fill all available space;
# clicking the image or the expand icon opens the full-screen viewer
img_card <- function(id) {
  views <- panels[[id]]$views
  toggle <- if (length(views) > 1) {
    radioButtons(paste0('mode_', id), NULL, inline = TRUE,
                 choices = setNames(seq_along(views), sapply(views, `[[`, 'label')))
  }
  card(
    card_header(
      class = 'py-1 d-flex justify-content-between align-items-center gap-3',
      div(class = 'me-auto', panels[[id]]$title),
      toggle,
      actionLink(paste0('expand_', id), icon('expand'), title = 'Full screen')
    ),
    card_body(
      plotOutput(id, height = '100%', click = paste0('click_', id)),
      padding = 0, min_height = '300px', style = 'cursor: zoom-in;'
    )
  )
}

# full-screen modal styling, and left/right arrow keys to cycle images
viewer_head <- tags$head(
  tags$style(HTML('
    #shiny-modal .modal-dialog { max-width: 100vw; width: 100vw; height: 100vh; margin: 0; }
    #shiny-modal .modal-content { height: 100vh; border-radius: 0; }
    #shiny-modal .modal-title { width: 100%; }
    #shiny-modal .modal-body { position: relative; padding: 0 70px 8px; }
    .viewer-arrow { position: absolute; top: 50%; transform: translateY(-50%);
                    font-size: 2rem; width: 56px; height: 90px; z-index: 10; }
    .viewer-arrow.left { left: 8px; }
    .viewer-arrow.right { right: 8px; }
    .card-header .form-group { margin-bottom: 0; }
  ')),
  tags$script(HTML("
    document.addEventListener('keydown', function(e) {
      if (!document.querySelector('#shiny-modal.show')) return;
      if (e.key === 'ArrowLeft') Shiny.setInputValue('viewer_step', -1, {priority: 'event'});
      if (e.key === 'ArrowRight') Shiny.setInputValue('viewer_step', 1, {priority: 'event'});
    });
  "))
)

ui <- page_sidebar(
  viewer_head,
  sidebar = sidebar(
    width = 300,
    numericInput('quadrat', sprintf('Quadrat (1-%d)', n_quad), value = 1,
                 min = 1, max = n_quad, step = 1),
    div(
      class = 'd-flex gap-2',
      actionButton('prev', '< Prev', class = 'flex-fill'),
      actionButton('next_q', 'Next >', class = 'flex-fill')
    ),
    plotOutput('loc_map', height = '360px', click = 'loc_click'),
    helpText('Click a quadrat on the map to select it. Click an image to view it full screen.')
  ),
  padding = 8,
  gap = 8,
  layout_column_wrap(
    width = 1/2,
    heights_equal = 'row',
    gap = '8px',
    !!!lapply(names(panels), img_card)
  )
)


server <- function(input, output, session) {

  set_quadrat <- function(i) {
    i <- ((i - 1) %% n_quad) + 1  # wrap around 1-96
    updateNumericInput(session, 'quadrat', value = i)
  }

  # typed quadrat number, debounced so typing '40' doesn't briefly load quadrat 4;
  # invalid entries are ignored and the last valid quadrat stays on screen
  typed_quadrat <- debounce(reactive(input$quadrat), 400)
  quadrat <- reactiveVal(1)
  observeEvent(typed_quadrat(), {
    q <- typed_quadrat()
    if (isTRUE(q == round(q) && q >= 1 && q <= n_quad)) quadrat(q)
  })

  observeEvent(input$prev, set_quadrat(quadrat() - 1))
  observeEvent(input$next_q, set_quadrat(quadrat() + 1))

  # select quadrat by clicking on the location map
  observeEvent(input$loc_click, {
    pt <- vect(cbind(input$loc_click$x, input$loc_click$y), crs = crs(q40))
    hit <- which(relate(q40, pt, 'intersects'))
    if (length(hit) > 0) set_quadrat(q40$quadrat[hit[1]])
  })

  tile_window <- reactive({
    q40[q40$quadrat == quadrat()]
  })

  # full-screen viewer showing one panel at a time
  viewer_idx <- reactiveVal(1)

  open_viewer <- function(i) {
    viewer_idx(i)
    showModal(modalDialog(
      title = div(
        class = 'd-flex justify-content-between align-items-center w-100',
        textOutput('viewer_title', inline = TRUE),
        tags$button(type = 'button', class = 'btn-close', `data-bs-dismiss` = 'modal',
                    title = 'Close (Esc)')
      ),
      actionButton('viewer_prev', icon('chevron-left'), class = 'viewer-arrow left'),
      actionButton('viewer_next', icon('chevron-right'), class = 'viewer-arrow right'),
      plotOutput('viewer', height = 'calc(100vh - 90px)'),
      footer = NULL, easyClose = TRUE, size = 'xl'
    ))
  }

  step_viewer <- function(d) viewer_idx(((viewer_idx() - 1 + d) %% n_panels) + 1)
  observeEvent(input$viewer_prev, step_viewer(-1))
  observeEvent(input$viewer_next, step_viewer(1))
  observeEvent(input$viewer_step, step_viewer(input$viewer_step))

  # the view currently toggled on for a panel
  current_view <- function(id) {
    mode <- input[[paste0('mode_', id)]]
    panels[[id]]$views[[if (is.null(mode)) 1 else as.integer(mode)]]
  }

  output$viewer_title <- renderText(sprintf(
    'Quadrat %d: %s (%d/%d)', quadrat(),
    current_view(names(panels)[viewer_idx()])$title, viewer_idx(), n_panels
  ))
  output$viewer <- renderPlot(current_view(names(panels)[viewer_idx()])$draw(tile_window()))

  # render the four panels and wire up their full-screen triggers
  lapply(seq_along(panels), function(i) {
    id <- names(panels)[i]
    output[[id]] <- renderPlot(current_view(id)$draw(tile_window()))
    observeEvent(input[[paste0('expand_', id)]], open_viewer(i))
    observeEvent(input[[paste0('click_', id)]], open_viewer(i))
  })

  output$loc_map <- renderPlot({
    plot(q40, border = 'grey60', axes = FALSE, mar = c(0.5, 0.5, 0.5, 0.5))
    plot(tile_window(), col = '#d7301f', border = '#d7301f', add = TRUE)
    lines(plot_outline, lwd = 2)
    text(q40, labels = q40$quadrat, cex = 0.55)
  })
}

shinyApp(ui, server)

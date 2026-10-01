if (!require("dplyr")) install.packages("dplyr")
library("dplyr")
if (!require("geometry")) install.packages("geometry")
library("geometry")
if (!require("jsonlite")) install.packages("jsonlite")
library("jsonlite")
if (!require("DT")) install.packages("DT")
library("DT")
if (!require("MASS")) install.packages("MASS")
library("MASS")
if (!require("plotly")) install.packages("plotly")
library("plotly")
if (!require("rcdd")) install.packages("rcdd")
library("rcdd")
if (!require("shiny")) install.packages("shiny")
library("shiny")
if (!require("shinyalert")) install.packages("shinyalert")
library("shinyalert")
if (!require("shinyBS")) install.packages("shinyBS")
library("shinyBS")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
library("shinycssloaders")
if (!require("shinyjs")) install.packages("shinyjs")
library("shinyjs")
if (!require("shinyvalidate")) install.packages("shinyvalidate")
library("shinyvalidate")
if (!require("stringr")) install.packages("stringr")
library("stringr")
if (!require("tidyr")) install.packages("tidyr")
library("tidyr")
if (!require("volesti")) install.packages("volesti")
library("volesti")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
library("shinyWidgets")
if (!require("rhandsontable")) install.packages("rhandsontable")
library("rhandsontable")
if (!require("writexl")) install.packages("writexl")
library("writexl")
if (!require("readxl")) install.packages("readxl")
library("readxl")
if (!require("shinybusy")) install.packages("shinybusy")
library("shinybusy")
if (!require("forstringr")) install.packages("forstringr")
library("forstringr")
library("parallel")

options(warn = -1)

fairy_svg <- HTML(
  '<svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
     <g fill="none" stroke-linecap="round" stroke-linejoin="round">
       <!-- wings -->
       <path d="M31 34 C16 20, 6 24, 10 34 C6 44, 18 44, 31 34 Z" fill="#8fb3ff" stroke="#5b7fd6" stroke-width="1.5" opacity="0.85"/>
       <path d="M33 34 C48 20, 58 24, 54 34 C58 44, 46 44, 33 34 Z" fill="#a9c4ff" stroke="#5b7fd6" stroke-width="1.5" opacity="0.85"/>
       <!-- body -->
       <circle cx="32" cy="20" r="5" fill="#ffd98a" stroke="#e0a94a" stroke-width="1.4"/>
       <path d="M32 25 L32 44 M32 30 L26 38 M32 30 L38 38 M32 44 L27 52 M32 44 L37 52"
             stroke="#f2b84b" stroke-width="2.4"/>
       <!-- sparkle -->
       <path d="M50 12 l1.6 3.4 3.4 1.6 -3.4 1.6 -1.6 3.4 -1.6 -3.4 -3.4 -1.6 3.4 -1.6 Z" fill="#ffe08a"/>
     </g>
   </svg>'
)

# One row of the "Select probabilities" dim_1/dim_2/dim_3 pickers on the
# Plot Polytope(s) tab — the three call sites were identical except for the
# id suffix and default selection.
dim_picker_row <- function(id, selected) {
  fluidRow(column(
    12,
    offset = 0,
    pickerInput(paste0("dim_", id), "p #",
      list(1, 2, 3),
      selected = selected
    )
  ))
}

ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
    tags$style(HTML(
      "

      :root {
        --fairy-bg: #f4f6fa;
        --fairy-panel: #ffffff;
        --fairy-border: #e2e6ee;
        --fairy-primary: #3a6df0;
        --fairy-primary-dark: #2851c4;
        /* Background for any element with WHITE/light text sitting
           directly on it (active toggle buttons, primary filled
           buttons, the active navbar tab, …) — separate from
           --fairy-primary, which
           is also used as a plain TEXT/icon color on light backgrounds
           (its own contrast requirement runs the other direction). In
           light mode --fairy-primary itself already passes WCAG AA
           (4.5:1) for white text, so this is identical to it here. */
        --fairy-primary-contrast-bg: #3a6df0;
        --fairy-primary-contrast-bg-hover: #2851c4;
        --fairy-accent: #16a37a;
        --fairy-text: #1f2430;
        --fairy-text-muted: #667085;
        --fairy-navbar: #1b2130;
        --ct-border: #dde3f0;
        --ct-default: #f7f8fb;
        --ct-diag: #eef2fe;
        --ct-nodim: #f5f5f5;
        --ct-disjoint: #fff0f0;
        --ct-equiv: #fde8e8;
        --ct-nest: #f0f8f0;
        --ct-overlap: #fffbe6;
        --ct-info-bg: #f9fafd;
      }

      body.dark-mode {
        --fairy-bg: #0e1117;
        --fairy-panel: #161b27;
        --fairy-border: #2d3347;
        --fairy-primary: #6089f8;
        --fairy-primary-dark: #4a73e0;
        /* Dark mode's own --fairy-primary (#6089f8) is lighter than
           light mode's, chosen for its own text/icon-on-dark-background
           contrast — but that SAME shade under white text only reaches
           ~3.3:1, well under WCAG AA's 4.5:1 (confirmed directly:
           computed via the WCAG relative-luminance formula). A dedicated,
           darker shade for backgrounds carrying white text instead —
           #4c6dc6 measures ~4.9:1 with white text, comfortably clearing
           AA, while --fairy-primary itself stays unchanged for its own
           (already-passing) text-color uses. --fairy-primary-dark is
           NOT reused for this even though it's already a darker shade —
           it's independently tuned for ITS OWN role as a plain text
           color (link/icon hover), and darkening it further to also
           satisfy white-on-it would have broken that (confirmed
           directly: a candidate dark enough for white text dropped its
           own text-on-background contrast under AA). */
        --fairy-primary-contrast-bg: #4c6dc6;
        --fairy-primary-contrast-bg-hover: #405ca8;
        --fairy-accent: #1fcc9a;
        --fairy-text: #dde1ea;
        --fairy-text-muted: #8892aa;
        --fairy-navbar: #0b0f1a;
        --ct-border: #2d3950;
        --ct-default: #1a1f2c;
        --ct-diag: #192240;
        --ct-nodim: #1e2028;
        --ct-disjoint: #2a1818;
        --ct-equiv: #2a1520;
        --ct-nest: #152515;
        --ct-overlap: #26230a;
        --ct-info-bg: #192030;
      }

      /* Accessibility: respects the OS-level reduce-motion setting
         (vestibular disorders / motion sensitivity) — the app has quite
         a few animations (the corner mascot, the intro-pulse glow on
         newly-appeared buttons, the stale-results ring on Go, several
         flash/blink cues) and NONE of them previously checked this at
         all. The universal override below (every animation/transition
         collapsed to near-zero duration, one iteration) is the standard
         pattern for this — catches every current AND future animation
         in the app automatically, with no need to hunt down and edit
         each @keyframes/animation declaration individually, and without
         changing layout (only timing). */
      @media (prefers-reduced-motion: reduce) {
        *, *::before, *::after {
          animation-duration: 0.01ms !important;
          animation-iteration-count: 1 !important;
          transition-duration: 0.01ms !important;
          scroll-behavior: auto !important;
        }
      }

      /* Comparison table cell colour classes (themed via CSS variables) */
      .ct-default  { background: var(--ct-default) !important; }
      .ct-diag     { background: var(--ct-diag) !important; color: var(--fairy-text) !important; }
      .ct-nodim    { background: var(--ct-nodim) !important; }
      .ct-disjoint { background: var(--ct-disjoint) !important; }
      .ct-equiv    { background: var(--ct-equiv) !important; }
      .ct-nest     { background: var(--ct-nest) !important; }
      .ct-overlap  { background: var(--ct-overlap) !important; }
      .ct-header   { background: var(--ct-diag) !important; color: var(--fairy-text) !important; }
      .ct-infobox  {
        background: var(--ct-info-bg) !important;
        color: var(--fairy-text) !important;
        border-left-color: var(--fairy-primary) !important;
      }
      .ct-infobox p, .ct-infobox li, .ct-infobox ul { color: var(--fairy-text) !important; }
      .ct-infobox p[style*='color:#888'], .ct-infobox p.ct-muted { color: var(--fairy-text-muted) !important; }

      /* Dark mode: panels, wells, tables, inputs */
      body.dark-mode .well,
      body.dark-mode .panel.panel-default > .panel-body {
        background-color: var(--fairy-panel) !important;
        border-color: var(--fairy-border) !important;
        color: var(--fairy-text) !important;
      }
      body.dark-mode .panel.panel-default > .panel-heading {
        background-color: var(--fairy-panel) !important;
        border-color: var(--fairy-border) !important;
        color: var(--fairy-text) !important;
      }
      body.dark-mode textarea,
      body.dark-mode input[type='text'],
      body.dark-mode input[type='number'],
      body.dark-mode .selectize-input {
        background-color: var(--fairy-panel) !important;
        color: var(--fairy-text) !important;
      }
      body.dark-mode .selectize-dropdown { background: var(--fairy-panel) !important; color: var(--fairy-text) !important; border-color: var(--fairy-border) !important; }
      body.dark-mode .selectize-dropdown .option:hover,
      body.dark-mode .selectize-dropdown .option.active { background: var(--ct-diag) !important; }
      body.dark-mode table.dataTable,
      body.dark-mode .dataTables_wrapper { color: var(--fairy-text) !important; }
      body.dark-mode table.dataTable thead th,
      body.dark-mode table.dataTable thead td {
        background: var(--fairy-panel) !important;
        color: var(--fairy-text) !important;
        border-color: var(--fairy-border) !important;
      }
      body.dark-mode table.dataTable tbody tr,
      body.dark-mode table.dataTable tbody tr.odd,
      body.dark-mode table.dataTable tbody tr.even {
        background-color: var(--fairy-panel) !important;
        color: var(--fairy-text) !important;
      }
      body.dark-mode table.dataTable tbody tr.odd > td,
      body.dark-mode table.dataTable tbody tr.odd > th {
        background-color: var(--ct-default) !important;
      }
      body.dark-mode table.dataTable tbody tr.even > td,
      body.dark-mode table.dataTable tbody tr.even > th {
        background-color: var(--fairy-panel) !important;
      }
      body.dark-mode table.dataTable tbody tr:hover > td,
      body.dark-mode table.dataTable tbody tr:hover > th {
        background-color: var(--ct-diag) !important;
      }
      body.dark-mode table.dataTable td, body.dark-mode table.dataTable th {
        border-color: var(--fairy-border) !important;
        color: var(--fairy-text) !important;
      }
      body.dark-mode .dataTables_wrapper .dataTables_filter input,
      body.dark-mode .dataTables_wrapper .dataTables_length select {
        background: var(--fairy-panel) !important;
        color: var(--fairy-text) !important;
        border-color: var(--fairy-border) !important;
      }
      body.dark-mode .dataTables_wrapper .dataTables_info,
      body.dark-mode .dataTables_wrapper .dataTables_paginate { color: var(--fairy-text-muted) !important; }
      body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button { color: var(--fairy-text) !important; }
      body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: var(--ct-diag) !important;
        color: var(--fairy-text) !important;
        border-color: var(--fairy-border) !important;
      }
      /* Card overlay: ensure DataTables inside use dark theme */
      body.dark-mode #fairy-card-overlay-inner table.dataTable tbody tr.odd > td { background: var(--ct-default) !important; }
      body.dark-mode #fairy-card-overlay-inner table.dataTable tbody tr.even > td { background: var(--fairy-panel) !important; }
      body.dark-mode #fairy-card-overlay-inner { background: var(--fairy-panel) !important; color: var(--fairy-text) !important; }
      /* Tab/panel containers */
      body.dark-mode .tab-content,
      body.dark-mode .tab-pane,
      body.dark-mode .shiny-html-output,
      body.dark-mode .shiny-bound-output { background: transparent !important; }
      body.dark-mode .container-fluid,
      body.dark-mode .row { color: var(--fairy-text) !important; }
      /* Info boxes — extra specificity to beat any inherited white */
      body.dark-mode .ct-infobox {
        background-color: var(--ct-info-bg) !important;
        background: var(--ct-info-bg) !important;
        color: var(--fairy-text) !important;
      }
      body.dark-mode details > div.ct-infobox { background: var(--ct-info-bg) !important; }
      /* Plotly: instant CSS background so there's never a white frame visible.
         NOTE: main-svg must stay transparent for gl3d (WebGL) plots — it's an
         overlay (hover/ticks/titles) that sits on top of a separate <canvas>;
         an opaque background here would paint over and hide the WebGL plot
         entirely. Only 2D (SVG-rendered) charts get their svg background set,
         and only via JS (applyOnePlot), which can check for a scene layout. */
      body.dark-mode .js-plotly-plot { background: var(--fairy-panel) !important; }
      body.dark-mode .shiny-plot-output,
      body.dark-mode .plotly.html-widget { background: var(--fairy-panel) !important; }
      body.dark-mode .grey-out { background-color: #1e2438 !important; }
      body.dark-mode .def-col--pnames { background-color: var(--ct-default) !important; }
      body.dark-mode .def-col--pnames textarea,
      body.dark-mode #fairy-card-overlay-inner { background: var(--fairy-panel) !important; color: var(--fairy-text) !important; }
      body.dark-mode .bttn-default.bttn-jelly,
      body.dark-mode .bttn-default.bttn-unite { background: #252b3d !important; color: var(--fairy-text) !important; }
      body.dark-mode #show_approx_erros, body.dark-mode #show_v_rep,
      body.dark-mode #show_intersections, body.dark-mode #show_mixtures,
      body.dark-mode body.dark-mode #show_items {
        background: #1e2438 !important;
        border-color: var(--fairy-border) !important;
        color: var(--fairy-text) !important;
      }
      body.dark-mode #show_approx_erros:hover, body.dark-mode #show_v_rep:hover,
      body.dark-mode #show_intersections:hover, body.dark-mode #show_mixtures:hover,
      body.dark-mode #show_items:hover {
        background: var(--ct-diag) !important;
      }
      body.dark-mode .model-card { background: var(--fairy-panel) !important; }
      body.dark-mode h3, body.dark-mode h4, body.dark-mode h5 { color: var(--fairy-text) !important; }
      body.dark-mode details summary { color: var(--fairy-primary) !important; }
      body.dark-mode .shiny-output-error-validation { color: var(--fairy-text-muted) !important; }
      body.dark-mode .modal-content {
        background-color: var(--fairy-panel) !important;
        color: var(--fairy-text) !important;
        border-color: var(--fairy-border) !important;
      }
      body.dark-mode .modal-header,
      body.dark-mode .modal-footer {
        border-color: var(--fairy-border) !important;
      }
      body.dark-mode .modal-title { color: var(--fairy-text) !important; }
      body.dark-mode .close { color: var(--fairy-text) !important; opacity: 0.7; }
      body.dark-mode .close:hover { opacity: 1; }
      /* shinyalert / SweetAlert v1 popups */
      body.dark-mode .sweet-alert {
        background-color: var(--fairy-panel) !important;
        color: var(--fairy-text) !important;
      }
      body.dark-mode .sweet-alert h2 { color: var(--fairy-text) !important; }
      body.dark-mode .sweet-alert p  { color: var(--fairy-text-muted) !important; }
      body.dark-mode .sweet-alert button.confirm {
        background-color: var(--fairy-primary-contrast-bg) !important;
        color: #fff !important;
        box-shadow: none !important;
      }
      body.dark-mode .sweet-alert button.cancel {
        background-color: var(--fairy-border) !important;
        color: var(--fairy-text) !important;
      }
      body.dark-mode .sweet-alert .sa-icon.sa-success { border-color: var(--fairy-accent) !important; }
      body.dark-mode .sweet-alert .sa-icon.sa-success::before,
      body.dark-mode .sweet-alert .sa-icon.sa-success::after { background: var(--fairy-panel) !important; }
      body.dark-mode .sweet-alert .sa-icon.sa-success .sa-fix { background-color: var(--fairy-panel) !important; }
      body.dark-mode .sweet-alert .sa-icon.sa-success .sa-placeholder { border-color: rgba(100,220,150,0.25) !important; }
      body.dark-mode .sweet-alert .sa-icon.sa-success .sa-line { background-color: var(--fairy-accent) !important; }
      body.dark-mode .sweet-alert .sa-icon.sa-error { border-color: #e05252 !important; }
      body.dark-mode .sweet-alert .sa-icon.sa-warning { border-color: #e0a030 !important; }
      /* Also cover SweetAlert2 in case newer shinyalert versions use it */
      body.dark-mode .swal2-popup {
        background: var(--fairy-panel) !important;
        color: var(--fairy-text) !important;
      }
      body.dark-mode .swal2-title,
      body.dark-mode .swal2-html-container { color: var(--fairy-text) !important; }
      body.dark-mode .swal2-confirm { background-color: var(--fairy-primary-contrast-bg) !important; color: #fff !important; }
      body.dark-mode .swal2-cancel  { background-color: var(--fairy-border) !important; color: var(--fairy-text) !important; }
      body.dark-mode [style*='color:#888'], body.dark-mode [style*='color: #888'] { color: var(--fairy-text-muted) !important; }
      body.dark-mode [style*='color:#333'], body.dark-mode [style*='color: #333'] { color: var(--fairy-text) !important; }
      body.dark-mode [style*='color:#444'], body.dark-mode [style*='color: #444'] { color: var(--fairy-text) !important; }
      body.dark-mode [style*='background:#f9fafd'] { background: var(--ct-info-bg) !important; }
      body.dark-mode [style*='background:#f7f8fb'] { background: var(--ct-default) !important; }
      body.dark-mode [style*='background:#ffffff'] { background: var(--fairy-panel) !important; }
      body.dark-mode [style*='background:#f5f5f5'] { background: var(--ct-nodim) !important; }
      body.dark-mode [style*='background:#fff8e1'] { background: #23210a !important; }

      /* Dark mode toggle button */
      #dm-toggle {
        position: fixed;
        top: 13px;
        right: 62px;
        z-index: 1060;
        background: rgba(255,255,255,0.10);
        border: none;
        border-radius: 999px;
        width: 28px; height: 28px;
        display: flex; align-items: center; justify-content: center;
        font-size: 15px;
        cursor: pointer;
        transition: background 0.2s;
        color: #d0d4e0;
        line-height: 1;
        padding: 0;
      }
      #dm-toggle:hover { background: rgba(255,255,255,0.22); }

      .fairy-compact-toggle-btn:hover { background: var(--ct-default) !important; }
      .fairy-compact-toggle-btn.active { background: var(--fairy-primary-contrast-bg) !important; color: #fff !important; border-color: var(--fairy-primary-contrast-bg) !important; }
      .fairy-sidebar-col-hidden { display: none !important; }
      .fairy-main-col-full { width: 100% !important; }

      /* Floating panel holding the real, reparented primary-action button
         (and any .fairy-primary-controls) shown only while the sidebar
         is hidden — see the sidebar-toggle script. */
      /* ── Floating panel — clean-slate layout ──────────────────────
         Two independent parts, on purpose:
         1. .fairy-primary-action — Go/Compute ALONE. Always visible,
            absolutely positioned at the panel's own fixed bottom-right
            corner (right/bottom offsets, not left/top), so its
            on-screen position depends only on the PANEL's own
            right:24/bottom:24 anchor — which never moves — and never on
            how much flow content .fairy-primary-controls is currently
            showing. This is the fix for it visibly shifting position on
            hover, which several earlier attempts (centering tweaks,
            order:999 combined with other changes, a shared corner
            cluster with Download/the stale badge) did not reliably
            solve or overcomplicated. Stale results now blink the button
            itself (fairy-stale-blink) instead of a separate badge.
         2. .fairy-primary-controls — everything else (Download/Upload,
            Parsimony's switches). Ordinary flow content above Go,
            shown only on hover/focus, collapsed to nothing otherwise —
            while collapsed, Go is the ONLY thing visible. Panel
            padding-bottom reserves exactly Go's own height so the two
            never overlap. */
      #sidebar-fab-panel {
        position: fixed;
        right: 24px;
        bottom: 24px;
        z-index: 1060;
        background: var(--fairy-panel);
        border: 1px solid var(--fairy-border);
        border-radius: 14px;
        /* Softer, closer shadow than the old single 20px-blur/0.22-alpha
           rule — that read as a heavy dark smudge under the panel
           (part of the clunky-looking complaint) rather than a light
           lift off the page. Two thin layers (a close ambient one + a
           barely-there ground contact one) is the standard floating-
           card recipe and looks lighter at the same visibility. */
        box-shadow: 0 2px 8px rgba(0,0,0,0.10), 0 8px 24px rgba(0,0,0,0.10);
        /* Bottom padding reserved for Go/Compute (36px tall, bottom:8px
           within the panel — 36+8=44), plus a little breathing room so
           the flow content above it (the handle) doesn't render flush
           against it. Both Go AND the icon row next to it are pinned
           via position:absolute below (not normal flow) — two earlier
           attempts at a flow-based layout (flex-wrap with a percentage
           flex-basis break, then a max-content CSS grid) each fixed one
           symptom but visibly shifted Go's own on-screen position
           between the collapsed/expanded states in a DIFFERENT way,
           because in both, Go's position was ultimately a function of
           how much OTHER content the browser had just laid out beside
           it. Pinning both to a fixed offset from the panel's own
           corner — which itself never moves — is the only version of
           this that has actually held up: nothing about Go's position
           depends on what else is visible. */
        padding: 6px 10px 50px 10px;
        min-width: 90px;
        max-width: 210px;
        display: block;
        transition: min-width 0.12s ease;
      }
      /* Both Go and the icon row are position:absolute (see below) —
         neither participates in the panel's own shrink-to-fit width on
         its own, so without this rule the panel stayed at its narrow
         collapsed width even once the icon row revealed on hover, and
         the icons rendered spilling past the card's own left edge
         instead of sitting inside it. Explicitly widen the panel to
         actually contain Download/Upload + Go: 92px (icon row's own
         offset from Go) + 76px (icon row's own width, two 36px icons +
         a 6px gap) + 10px right padding, rounded up a little. */
      #sidebar-fab-panel:hover,
      #sidebar-fab-panel:focus-within {
        min-width: 188px;
      }
      #sidebar-fab-panel .fairy-primary-action {
        position: absolute !important;
        /* 26px, not a plain 10px corner inset — the extra 16px is
           deliberate room for the drag handle, which now sits
           vertically beside Go's own right edge (see .fairy-fab-handle
           below) rather than up along the panel's top edge. */
        right: 26px;
        bottom: 8px;
      }
      /* Download/Upload's row sits immediately to Go's own left, same
         fixed-offset-from-the-corner technique — NOT flex/grid flow
         relative to Go, so revealing/hiding it can't move Go by even a
         pixel. The 92px offset is Go/Compute's own measured rendered
         width (~59px, from the shared .fairy-primary-action sizing
         rule — both buttons say Go, same padding/font, so this is
         one constant either tab can use) plus its own 26px corner
         offset (see that rule's own comment on the handle-clearance
         room baked into it) plus this row's 6px gap to it, rounded up
         slightly. If Go's label or sizing ever changes, this needs
         updating to match — there's no way to derive it automatically
         without JS measuring the rendered button, which is more
         machinery than this is worth. */
      #sidebar-fab-panel .fairy-primary-controls:not([data-fab-layout='column']) {
        position: absolute !important;
        bottom: 8px;
        right: 92px;
        /* Without an explicit width, a position:absolute box with only
           `right` set (no `left`) shrink-to-fits — and for a flex-wrap
           container, that sizing pass can resolve to the min-content
           width (one icon) rather than fitting both side by side,
           wrapping the second icon onto its own line underneath
           instead of keeping the row intact. nowrap forces both icons
           to stay on one line regardless of how that width gets
           resolved. */
        flex-wrap: nowrap !important;
        width: max-content;
      }
      /* Manual press feedback for the Cmd/Ctrl+Enter shortcut — see the
         keydown handler above; a programmatic .click() never triggers
         the button's own :active style. */
      .fairy-kbd-press { transform: scale(0.9) !important; transition: transform 0.08s ease !important; }
      /* Collapsed by default (just the fairy-fab-handle grip bar and
         the always-visible corner show) — expands to reveal
         .fairy-primary-controls (switches / Upload) on hover, or while
         focus is inside it (keyboard users tabbing through, not just
         mouse hover). */
      #sidebar-fab-panel .fairy-primary-controls {
        display: none !important;
      }
      #sidebar-fab-panel:hover .fairy-primary-controls,
      #sidebar-fab-panel:focus-within .fairy-primary-controls {
        display: flex !important;
      }
      /* Same specificity as the generic reveal rule above AND the
         data-fab-layout=column rule below (each is id + 2 class-like
         selectors) — which one wins was coming down to source order
         alone, and kept flipping as unrelated edits moved code around
         (that is the actual reason Parsimony's switches kept
         reappearing as one row instead of stacked). The extra attribute
         selector here makes this strictly more specific than either, so
         column layout reliably wins on hover regardless of source order. */
      #sidebar-fab-panel:hover .fairy-primary-controls[data-fab-layout='column'],
      #sidebar-fab-panel:focus-within .fairy-primary-controls[data-fab-layout='column'] {
        display: block !important;
      }
      /* Handle: the only normal-flow child now (Go and the row-variant
         icon row are both pinned via position:absolute above/below) —
         just a plain full-width block, no special sizing needed. */
      #sidebar-fab-panel .fairy-primary-controls {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 6px;
      }
      /* Shiny's own base CSS gives every .shiny-input-container a hard
         width:300px (materialSwitch's outer div carries that class) —
         that's what was forcing each algorithm switch onto its own full
         row despite flex-wrap: the wrapper sized itself around a 300px-
         wide child. Shrink it back to content. */
      #sidebar-fab-panel .fairy-primary-controls .shiny-input-container {
        width: auto !important;
      }
      /* Parsimony's algorithm switches sit in .fairy-primary-controls'
         own default flex row (see that rule above) — just needs its
         .form-group's default 15px bottom margin trimmed down, since
         that's excess once nothing wraps to a second line below it. */
      #sidebar-fab-panel .fairy-primary-controls .form-group {
        margin-bottom: 2px;
      }
      /* Parsimony's column variant stays in NORMAL flow, stacking below
         the handle — it's tall enough (3 switches + a row) that Go
         being pinned to the corner below/beside it (see
         .fairy-primary-action above) is what keeps IT stable, same
         reasoning as the row variant above just mirrored: whichever of
         Go/controls is content-driven in height, the OTHER one has to
         be the one that's pinned. */
      #sidebar-fab-panel .fairy-primary-controls[data-fab-layout='column'] {
        display: block;
        /* More clearance than the row variant needs — the switches'
           own labels (see the font-size rule below) still render at
           normal body text size and sat close enough to the handle bar
           above to read as crowding/overlap right at the seam. */
        margin-top: 14px;
      }
      /* Switch labels (Cooling Bodies etc.) at the browser's default
         body font-size (usually ~16px) read oversized and loose next
         to a compact floating panel — this is the main thing making
         the column layout feel crowded/bulky overall, not just the
         handle clearance above. Match the panel's own 14px elsewhere. */
      #sidebar-fab-panel .fairy-primary-controls[data-fab-layout='column'] label {
        font-size: 14px;
      }
      /* Settings sits immediately beside Go — same position:absolute,
         right:92px/bottom:8px-from-the-panel's-own-corner technique as
         Input's Download/Upload row (see .fairy-primary-controls's own
         comment on why that offset value is what it is). */
      #sidebar-fab-panel .fairy-fab-action-row {
        position: absolute;
        bottom: 8px;
        right: 92px;
        display: flex;
        align-items: center;
        gap: 6px;
      }
      /* Go itself matched to the exact same 30px height as
         Download/Upload (see .fairy-toolbar-iconbtn /
         .fairy-toolbar-fileinput .btn-file) — the jelly bttn style's
         own baked-in padding otherwise makes it taller than everything
         else in this panel, which was the actual root of the whole
         buttons-dont-line-up complaint, not just Download vs Upload. */
      #sidebar-fab-panel .fairy-primary-action {
        height: 36px !important;
        min-height: 0 !important;
        line-height: 34px !important;
        padding: 0 20px !important;
        font-size: 14px !important;
        box-sizing: border-box !important;
        border: none !important;
        box-shadow: none !important;
        margin: 0 !important;
        /* The jelly bttn style's own baked-in border-radius is a full
           pill (~50px) — a visibly different shape language from the
           squared-off Download/Upload icon buttons sitting right next
           to it (.fairy-toolbar-iconbtn's 8px radius), which is most of
           what read as clunky/mismatched. Rounding Go down to match
           unifies the whole panel to one consistent rounded-rectangle
           style instead of pill + squares side by side. */
        border-radius: 10px !important;
        /* The jelly bttn style bakes in overflow:hidden (to clip its own
           glow pseudo-element) — since this button also carries
           fairy-tooltip directly (has to, so the tooltip travels along
           when updateFab() reparents it — see that comment on the Go/
           Compute buttons), that hidden overflow was clipping away the
           ::after tooltip bubble too, silently, the entire time. */
        overflow: visible !important;
      }
      /* A slim center-weighted grip bar rather than two chunky vertical
         dot columns — the old glyph read as a random floating icon
         disconnected from drag me, not an actual grip affordance, and
         took up more vertical room than a drag handle needs. Dims to
         near-invisible until the panel itself is hovered/focused
         (matches .fairy-primary-controls'
         own reveal), so it doesn't clutter the collapsed Go-only state. */
      /* Text-align/padding tuning, then a position:relative wrapper +
         absolutely-positioned ::before, both made no correct visible
         difference in testing (the ::before version actually landed
         far to the LEFT, outside the card, which doesn't match
         right:6px on any containing block this rule could plausibly
         have — something about the extra positioning-context layer
         wasn't resolving the way this reasoning expects). Removing
         that layer entirely: the handle DIV itself is now positioned
         directly and IS the bar (no ::before), same
         position:absolute + fixed-offset-from-#sidebar-fab-panel
         technique already proven reliable for Go and the icon row —
         one fewer moving part than a nested positioning context. */
      /* Vertical grip beside Go's own right edge, not a horizontal bar
         along the panel's top — sits in the 16px of room Go's own
         right:26px (rather than a plain 10px) now deliberately leaves
         for it. bottom:18px/height:16px centers it vertically within
         Go's own 36px-tall box (bottom:8px..44px), same idea as
         centering any two adjacent controls of different heights. */
      #sidebar-fab-panel .fairy-fab-handle {
        position: absolute;
        /* Bigger than the visual grip alone needs to be — a 4x16 bar
           was hard to actually grab with a mouse/finger. box-sizing:
           border-box + padding gives it a real hit area that extends
           beyond what's painted (the background), same trick a small
           icon button uses to keep a comfortable click target without
           looking oversized. */
        right: 2px;
        bottom: 7px;
        box-sizing: border-box;
        width: 26px;
        height: 38px;
        padding: 7px 9px;
        cursor: move;
        margin: 0;
        user-select: none;
        touch-action: none;
      }
      #sidebar-fab-panel .fairy-fab-handle::before {
        content: '';
        display: block;
        width: 100%;
        height: 100%;
        border-radius: 999px;
        background: var(--fairy-border);
        opacity: 0.9;
        transition: background 0.15s ease;
      }
      #sidebar-fab-panel:hover .fairy-fab-handle::before,
      #sidebar-fab-panel:focus-within .fairy-fab-handle::before {
        background: var(--fairy-text-muted);
      }

      /* Per-model delete button — same 'ring, not shading/growing' hover
         language as the V-rep toggle/jelly buttons above. Blue like
         every other hover ring in the app (a red ring here read as
         inconsistent, not as an extra danger cue), text color is still
         the red darken so the delete intent stays visible. */
      .fairy-del-model-btn:hover {
        color: #922b21 !important;
        box-shadow: 0 0 0 3px color-mix(in srgb, var(--fairy-primary, #3a6df0) 25%, transparent) !important;
        border-radius: 6px;
      }
      /* Delete all models, right next to the V-rep master toggle above
         row 1 — same pill-shaped background/border .fairy-vrep-toggle-
         btn has (that button's own CSS, not duplicated here — this
         picks it up directly) rather than a bare borderless glyph, so
         the two read as one matched pair instead of one boxed button
         next to one floating icon. Kept as its own class for its hover
         rather than reusing .fairy-del-model-btn so its click handler
         (see the script below) can't be confused with the per-model
         one's data-model-idx-based routing. */
      .fairy-del-all-models-btn {
        background-color: var(--ct-default) !important;
        border: 1px solid var(--fairy-border) !important;
        border-radius: 8px !important;
        line-height: 1;
        box-shadow: none !important;
        padding: 5px 12px !important;
        display: inline-flex !important;
        align-items: center !important;
        justify-content: center !important;
      }
      .fairy-del-all-models-btn:hover {
        color: #922b21 !important;
        box-shadow: 0 0 0 3px color-mix(in srgb, var(--fairy-primary, #3a6df0) 25%, transparent) !important;
        border-radius: 8px !important;
      }
      body {
        padding-top: 62px;
        padding-bottom: 32px;
        background-color: var(--fairy-bg);
        color: var(--fairy-text);
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }

      h3, h4, h5 {
        color: var(--fairy-text);
        font-weight: 600;
      }

      h5 {
        font-size: 14px;
        letter-spacing: 0.01em;
        margin-top: 4px;
        margin-bottom: 8px;
      }

      hr {
        border-top: 1px solid var(--fairy-border);
        margin: 14px 0;
      }

      .grey-out {
          background-color: var(--ct-nodim);
          opacity: 0.55;
          transition: opacity 0.2s ease;
      }

      /* Trivial-bound (0<=p<=1) H-representation rows — see the
         fairy-trivial-row class emitted in the H-rep render and the
         segmented-control click handler on the H-representation tab
         (search fairy-segmented in the script below). */
      #outP.trivial-gray .fairy-trivial-row { opacity: 0.35; }
      #outP.trivial-hide .fairy-trivial-row { display: none; }
      /* Same state, mirrored onto the card-expand overlay (see
         openOverlay() copying the class across) so a card that's
         currently graying-out/hiding trivial bounds keeps doing so once
         expanded full-screen, instead of resetting to show everything
         just because the overlay is a different container than #outP. */
      #fairy-card-overlay-content.trivial-gray .fairy-trivial-row { opacity: 0.35; }
      #fairy-card-overlay-content.trivial-hide .fairy-trivial-row { display: none; }

      /* H-rep table view — Layout (Aligned: one column per parameter,
         no color needed since position is already the same-parameter
         cue -- vs. Compact: packed, no reserved-but-empty columns) and
         Color (off/on) are two INDEPENDENT toggles (see
         h_layout_toggle_group / h_color_toggle_group's own comments),
         but there are only 4 actual table variants to show, so the
         script below combines both toggles' current state into one
         data-h-view value here ('' = aligned+off, the default; plus
         'aligned-color', 'compact', 'compact-color') rather than this
         CSS trying to express two independently-varying attributes at
         once. All four tables are always rendered (see
         .fairy-h-aligned-plain-table/-color-table and .fairy-h-packed-
         plain-table/-color-table below); only this value decides which
         one shows, so switching is instant either toggle is used. */
      #outP[data-h-view='aligned-color'] .fairy-h-aligned-plain-table,
      #outP[data-h-view='compact'] .fairy-h-aligned-plain-table,
      #outP[data-h-view='compact-color'] .fairy-h-aligned-plain-table { display: none !important; }
      #outP[data-h-view='aligned-color'] .fairy-h-aligned-color-table { display: inline-table !important; }
      #outP[data-h-view='compact'] .fairy-h-packed-plain-table { display: inline-table !important; }
      #outP[data-h-view='compact-color'] .fairy-h-packed-color-table { display: inline-table !important; }
      #fairy-card-overlay-content[data-h-view='aligned-color'] .fairy-h-aligned-plain-table,
      #fairy-card-overlay-content[data-h-view='compact'] .fairy-h-aligned-plain-table,
      #fairy-card-overlay-content[data-h-view='compact-color'] .fairy-h-aligned-plain-table { display: none !important; }
      #fairy-card-overlay-content[data-h-view='aligned-color'] .fairy-h-aligned-color-table { display: inline-table !important; }
      #fairy-card-overlay-content[data-h-view='compact'] .fairy-h-packed-plain-table { display: inline-table !important; }
      #fairy-card-overlay-content[data-h-view='compact-color'] .fairy-h-packed-color-table { display: inline-table !important; }
      /* Segmented (3-button) control shared by both the Trivial bounds
         and Table layout toggles above — replaces what used to be a
         single button silently cycling through 3 states (reported as
         unintuitive: nothing on screen showed a 3rd state existed, and
         getting back to an earlier one meant cycling all the way
         around). All three options are visible at once here, with the
         active one highlighted; each button sets its state directly. */
      .fairy-segmented {
        display: inline-flex;
        border-radius: 10px;
        border: 1px solid var(--fairy-border);
        background: var(--ct-default);
        overflow: hidden;
      }
      .fairy-segmented button {
        box-sizing: border-box;
        width: 26px;
        height: 32px;
        padding: 0;
        border: none;
        border-left: 1px solid var(--fairy-border);
        background: transparent;
        color: var(--fairy-text-muted);
        font-size: 11px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        transition: background-color 0.12s ease, color 0.12s ease;
      }
      .fairy-segmented button:first-child { border-left: none; }
      .fairy-segmented button svg { color: inherit !important; fill: currentColor !important; opacity: 1 !important; }
      .fairy-segmented button:hover:not(.active) {
        background: color-mix(in srgb, var(--fairy-primary) 10%, var(--ct-default));
        color: var(--fairy-primary);
      }
      .fairy-segmented button.active {
        background: var(--fairy-primary-contrast-bg);
        color: #fff;
      }
      /* Thin separator between the display toggles (Trivial bounds/
         Layout/Color) and the downloads below them in the H-rep/V-rep
         toolbar columns — a plain <hr> defaults to a full-width block
         rule; this narrows and centers it to match the toolbar's own
         narrow icon-column width instead of bleeding past it. */
      .fairy-toolbar-divider {
        width: 34px;
        flex: 0 0 auto;
        margin: 4px 0;
        border: none;
        border-top: 1px solid var(--fairy-border);
      }

      /* Navbar */
      .navbar {
        background-color: var(--fairy-navbar) !important;
        font-family: 'Inter', sans-serif;
        font-size: 13.5px;
        border: none;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
      }
      .navbar-default .navbar-nav > li > a {
        color: #c7cede !important;
        font-weight: 500;
        transition: color 0.15s ease;
      }
      .navbar-default .navbar-nav > li > a:hover {
        color: #ffffff !important;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        color: #ffffff !important;
        background-color: var(--fairy-primary-contrast-bg) !important;
        border-radius: 4px;
      }
      .navbar-default .navbar-brand {
        color: #ffffff !important;
        font-weight: 700;
      }
      .navbar-dropdown {
        background-color: #262c3d;
        font-family: 'Inter', sans-serif;
        font-size: 13.5px;
        color: #e6e8ee;
      }

      /* Fairy brand */
      .fairy-brand {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        vertical-align: middle;
      }
      .fairy-brand svg {
        filter: drop-shadow(0 1px 2px rgba(0,0,0,0.35));
      }
      .fairy-brand-text {
        color: #ffffff;
        font-weight: 700;
        font-size: 16px;
        letter-spacing: 0.01em;
      }
      .navbar-default .navbar-brand { padding-top: 12px; }

      .fairy-brand svg { width: 26px; height: 26px; }

      /* Dancing fairy in the upper-right corner (busy indicator).
         Idle: a gentle float. Busy: a random trick each time. */
      #corner-fairy {
        position: fixed;
        top: 8px;
        right: 20px;
        z-index: 1060;
        opacity: 0.6;
        transition: opacity 0.25s ease;
      }
      #corner-fairy svg {
        width: 34px; height: 34px;
        transform-origin: 50% 60%;
        animation: fairy-idle 3.2s ease-in-out infinite;
      }
      #corner-fairy.dancing { opacity: 1; }
      #corner-fairy.fairy-away { opacity: 0 !important; pointer-events: none; }

      @keyframes fairy-idle {
        0%, 100% { transform: translateY(0); }
        50%      { transform: translateY(-3px); }
      }
      /* trick: sway/dance */
      @keyframes fairy-dance {
        0%,100% { transform: translateY(0)    rotate(-9deg); }
        25%     { transform: translateY(-4px) rotate(9deg); }
        50%     { transform: translateY(0)    rotate(-9deg); }
        75%     { transform: translateY(-4px) rotate(9deg); }
      }
      /* trick: spin */
      @keyframes fairy-spin { to { transform: rotate(360deg); } }
      /* trick: loop-the-loop flight */
      @keyframes fairy-loop {
        0%   { transform: translate(0,0)     rotate(0deg); }
        25%  { transform: translate(-11px,-9px) rotate(-90deg); }
        50%  { transform: translate(0,-15px)  rotate(-180deg); }
        75%  { transform: translate(11px,-9px)  rotate(-270deg); }
        100% { transform: translate(0,0)     rotate(-360deg); }
      }
      /* trick: barrel roll (flip) */
      @keyframes fairy-flip {
        0%   { transform: rotateY(0deg); }
        100% { transform: rotateY(360deg); }
      }
      /* trick: bouncy squash-and-stretch */
      @keyframes fairy-bounce {
        0%,100% { transform: translateY(0)    scale(1, 1); }
        30%     { transform: translateY(-13px) scale(0.88, 1.12); }
        50%     { transform: translateY(0)    scale(1.12, 0.88); }
        70%     { transform: translateY(-7px)  scale(0.95, 1.05); }
      }
      #corner-fairy.trick-dance  svg { animation: fairy-dance  0.55s ease-in-out infinite; }
      #corner-fairy.trick-spin   svg { animation: fairy-spin   0.9s  linear         infinite; }
      #corner-fairy.trick-loop   svg { animation: fairy-loop   1.15s ease-in-out    infinite; }
      #corner-fairy.trick-flip   svg { animation: fairy-flip   1s    ease-in-out    infinite; }
      #corner-fairy.trick-bounce svg { animation: fairy-bounce 0.7s  ease-in-out    infinite; }

      /* Easter egg: click the corner fairy for a spell burst + a quip. */
      #corner-fairy { cursor: pointer; }
      #corner-fairy.trick-cast svg { animation: fairy-cast 0.6s ease-in-out; }
      @keyframes fairy-cast {
        0%   { transform: rotate(0deg)   scale(1); }
        25%  { transform: rotate(-18deg) scale(1.12); }
        55%  { transform: rotate(14deg)  scale(1.18); }
        100% { transform: rotate(0deg)   scale(1); }
      }
      .fairy-sparkle {
        position: fixed; z-index: 999998; pointer-events: none;
        font-size: 16px; opacity: 1; will-change: transform, opacity;
        animation: fairy-sparkle-out 1.05s cubic-bezier(.2,.8,.3,1) forwards;
        filter: drop-shadow(0 0 3px rgba(255,220,120,0.9));
      }
      @keyframes fairy-sparkle-out {
        0%   { transform: translate(0,0) scale(0.4) rotate(0deg);   opacity: 1; }
        70%  { opacity: 1; }
        100% { transform: var(--fairy-sparkle-end) scale(1.1) rotate(180deg); opacity: 0; }
      }
      .fairy-speech-bubble {
        position: fixed; z-index: 999999; max-width: 240px;
        background: var(--fairy-panel, #fff); color: var(--fairy-text, #1f2430);
        border: 1px solid var(--fairy-primary, #3a6df0); border-radius: 12px;
        padding: 8px 12px; font-size: 12.5px; line-height: 1.5;
        box-shadow: 0 6px 20px rgba(0,0,0,0.18);
        opacity: 0; transform: translateY(4px) scale(0.92);
        transition: opacity 0.18s ease, transform 0.18s ease;
        pointer-events: none;
      }
      .fairy-speech-bubble.fairy-bubble-show {
        opacity: 1; transform: translateY(0) scale(1);
      }
      .fairy-speech-bubble::after {
        content: ''; position: absolute; top: -6px; right: 22px;
        width: 10px; height: 10px; background: inherit;
        border-left: 1px solid var(--fairy-primary, #3a6df0);
        border-top: 1px solid var(--fairy-primary, #3a6df0);
        transform: rotate(45deg);
      }

      /* Panels */
      .well, .sidebarPanel, div.sidebar-panel {
        background-color: var(--fairy-panel);
        border: 1px solid var(--fairy-border);
        border-radius: 10px;
        box-shadow: 0 1px 3px rgba(16, 24, 40, 0.06);
        padding: 20px 18px;
      }
      /* Sidebar breathing room between sections */
      .well hr { margin: 16px 0; }
      .well h4 { font-size: 15px; font-weight: 700; margin-bottom: 10px; color: var(--fairy-text); }
      .well h5 { font-size: 13px; font-weight: 700; margin-bottom: 8px; color: var(--fairy-text); }
      .well .shiny-input-container,
      .well .form-group { margin-bottom: 10px; }
      .well label { font-size: 13px !important; color: var(--fairy-text) !important; }
      .well .shiny-download-link,
      .well .btn { margin-top: 2px; }

      .tab-content {
        background-color: transparent;
      }

      /* Robust sidebar/main layout: flex row so the sidebar keeps its own
         width at every viewport size and the main panel takes the rest,
         instead of Bootstrap percentage columns colliding with the fixed
         200px sidebar well. */
      .tab-content > .tab-pane.active {
        display: flex;
        flex-wrap: nowrap;
        align-items: flex-start;
        gap: 10px;
      }
      .tab-content > .tab-pane.active > div[class*='col-sm'] {
        float: none;
      }
      .tab-content > .tab-pane.active > div[class*='col-sm']:first-child {
        flex: 0 0 auto;
        width: auto;
        padding-left: 0;
      }
      .tab-content > .tab-pane.active > div[class*='col-sm']:last-child {
        flex: 1 1 auto;
        width: auto;
        min-width: 0;
        overflow-x: auto;
      }

      /* Model-definition panel (Input tab) */
      .model-def-panel {
        background-color: var(--fairy-panel);
        border: 1px solid var(--fairy-border);
        border-radius: 12px;
        box-shadow: 0 1px 3px rgba(16, 24, 40, 0.06);
        padding: 18px 22px 24px;
        max-width: 1000px;
        margin: 8px auto 0;
      }
      .section-title {
        font-size: 15px;
        font-weight: 600;
        color: var(--fairy-text);
        text-align: left;
        margin: 0 0 14px;
        padding-bottom: 10px;
        border-bottom: 1px solid var(--fairy-border);
      }
      /* Consistent columns; group the p-name inputs in one subtle card
         instead of the old dashed-blue highlight box. */
      .def-col { vertical-align: top; }
      .def-col--pnames {
        background-color: var(--ct-default);
        border: 1px solid var(--fairy-border);
        border-radius: 10px;
        padding: 6px 8px;
      }
      .def-col--pnames textarea { background-color: var(--fairy-panel); color: var(--fairy-text); }
      /* Probabilities as a horizontal header row above the model rows
         (instead of their own narrow leftmost column) — lays the
         Name-of-p_i boxes out left-to-right, wrapping as needed. */
      .def-header--pnames { margin-bottom: 10px; }
      .def-header--pnames > div {
        display: flex;
        flex-wrap: wrap;
        gap: 10px 16px;
        align-items: flex-start;
      }
      .def-header--pnames .form-group { margin-bottom: 0; }
      /* Same jelly/default style as the other add/remove buttons on this
         page, just darkened a bit for contrast against the pnames card's
         own tinted background. */
      #rm_prob, #rm_ie { background-color: #9aa0ab !important; color: #222 !important; border-color: #8a919e !important; }
      body.dark-mode #rm_prob, body.dark-mode #rm_ie { background-color: #454b56 !important; color: #f0f0f0 !important; border-color: #333844 !important; }
      /* Card container around the model-specification area, matching the
         probabilities card above it so the two are visually distinct. */
      .def-col--models-card {
        background-color: var(--ct-default);
        border: 1px solid var(--fairy-border);
        border-radius: 10px;
        padding: 10px 8px;
        margin-top: 10px;
      }
      /* Per-model row styling. There's no single element wrapping all
         three of a model's boxes (Model Name / Unique Model Specification
         / Model Specification live in three separately-rendered column
         lists), so row i here means: the same data-row-idx applied
         independently to each of the three boxes at list position i —
         see the hover-sync script near the top of the UI that ties them
         together on mouseover despite that. */
      .fairy-model-row {
        border-radius: 6px;
        padding: 2px 4px;
        margin: -2px -4px;
        transition: background-color 0.12s;
      }
      .fairy-row-even { background-color: var(--ct-default); }
      .fairy-row-hover { background-color: var(--ct-diag) !important; }
      /* Compact mode: toggled by #compact-toggle, shrinks the model-row
         boxes so more models fit on screen at once. Inline heights from
         textAreaInput() need !important to be overridden here; the
         complete_ textarea (kept zero-size/hidden) is excluded so this
         can't accidentally reveal it. */
      body.fairy-compact-models textarea[id^='textin_relations_name'],
      body.fairy-compact-models textarea[id^='textin_relations_']:not([id^='textin_relations_name']):not([id^='textin_relations_complete_']) {
        height: 60px !important;
      }
      body.fairy-compact-models .fairy-model-spec-outer,
      body.fairy-compact-models pre[id^='model_spec_display_'] {
        height: 60px !important;
      }
      /* Disabled/derived spec boxes (intersection & mixture rows) hold
         longer, non-editable text — at the compacted 46px height that
         wraps to two lines and clips mid-word (e.g. p1 >=p), which reads
         as broken rather than merely short. Since there's nothing to edit
         here anyway, show one clean truncated line with an ellipsis
         instead of a mangled wrap. */
      body.fairy-compact-models textarea[id^='textin_relations_']:disabled,
      body.fairy-compact-models .fairy-model-spec-outer pre[id^='model_spec_display_'] {
        white-space: nowrap !important;
        overflow: hidden !important;
        text-overflow: ellipsis !important;
        font-size: 12px !important;
        padding-top: 6px !important;
      }
      /* The icon column (delete / V-toggle / edit / preview / copy / ≈)
         was sized for the normal 150px row height — stacked vertically,
         up to five of them plus gaps don't fit inside a compacted 46px
         row, spilling into the next one. Instead of squeezing them into
         one thin column, wrap them into a small 2-column grid (2 icons
         per row, wrapping as needed) so the block stays short and wide
         rather than tall and cramped. */
      body.fairy-compact-models .fairy-model-icon-col {
        display: flex !important;
        flex-flow: row wrap !important;
        align-content: center !important;
        justify-content: flex-start !important;
        align-self: stretch !important;
        width: 46px !important;
        height: auto !important;
        gap: 2px !important;
        /* Row 1's icon column normally gets extra top padding (in the
           inline style) to line up with the column headers above it —
           fine at full row height, but in compact mode that offset just
           pushes the icons down away from the row's own vertical center,
           leaving a big dead gap. Center on the row's height instead. */
        padding-top: 0 !important;
      }
      body.fairy-compact-models .fairy-del-model-btn,
      body.fairy-compact-models .fairy-eq-tol-btn,
      body.fairy-compact-models .fairy-vrep-toggle-btn {
        font-size: 10px !important;
        padding: 1px 5px !important;
        line-height: 1.2 !important;
        flex: 0 0 auto !important;
      }
      .model-def-panel .form-group { margin-bottom: 0; }
      .model-def-panel label {
        font-weight: 600;
        color: var(--fairy-text-muted);
        font-size: 12.5px;
      }

      /* One card per model (H-representation / V-representation tabs) */
      .model-card {
        background-color: var(--fairy-panel);
        border: 1px solid var(--fairy-border);
        border-radius: 12px;
        box-shadow: 0 1px 3px rgba(16, 24, 40, 0.06);
        padding: 20px 26px;
        max-width: 1300px;
        margin: 0 auto 16px;
        cursor: zoom-in;
        transition: box-shadow 0.15s ease, border-color 0.15s ease;
        position: relative;
      }
      .model-card:hover {
        border-color: var(--fairy-primary, #6e45e2);
        box-shadow: 0 2px 10px rgba(110, 69, 226, 0.15);
      }
      /* Non-trivial equality/inequality count — corner chip on each
         H-representation card (see count_badge in the H-rep render),
         separate from the model-name title beneath it. */
      .fairy-h-count-badge {
        /* No longer poking above the card's own top edge (top:-9px) —
           when a card sits near the top of the scroll area, that
           negative offset could put the badge partly outside whatever
           ancestor clips overflow, making it unreadable/half-cut. Kept
           fully inside the card instead. */
        display: inline-block;
        margin: -4px 0 6px;
        background: var(--fairy-primary, #6e45e2);
        color: #fff;
        font-size: 10.5px;
        font-weight: 600;
        padding: 2px 8px;
        border-radius: 8px;
        line-height: 1.4;
        box-shadow: 0 1px 3px rgba(16, 24, 40, 0.15);
      }
      /* This badge sits at a card's own top-left corner, so the default
         .fairy-tooltip::after (opens upward, centered) either got
         clipped by the viewport top or, opened downward instead,
         covered the card's title/content right underneath it. Opening
         sideways avoids both — nothing above or below the badge to
         clip against or cover. */
      .fairy-h-count-badge.fairy-tooltip::after {
        /* Sideways (left:100%) got clipped by .model-card's own
           overflow-x:auto (needed for wide DT tables to scroll instead
           of overflowing the page) — anything escaping the card
           horizontally gets cut at that edge. Vertical isn't affected
           by overflow-x, so open downward instead, kept short (see the
           shortened data-tooltip text) to minimize how much of the
           card it still covers. */
        bottom: auto;
        top: 125%;
        left: 0;
        transform: none;
      }
      /* Self-built 'Computing X' progress overlay (#fairy-progress-overlay
         in the UI) — a single, always-present backdrop + card that the
         server only ever shows/hides and rewrites the text of (see
         fairy_progress_open/_update/_close and their JS handlers). Kept
         entirely outside shinyalert/swal2 on purpose: repeatedly calling
         shinyalert() to show 'model N of M' visibly flashed the dialog
         closed-then-reopened on every single model, and reaching into
         swal2's own DOM after the fact proved unreliable. A plain fixed
         div with a CSS opacity transition has neither problem — toggling
         one class never tears down or recreates the node. */
      #fairy-progress-overlay {
        position: fixed;
        inset: 0;
        z-index: 20000;
        display: flex;
        align-items: center;
        justify-content: center;
        background: rgba(20, 20, 30, 0.45);
        opacity: 0;
        pointer-events: none;
        transition: opacity 0.15s ease;
      }
      #fairy-progress-overlay.active {
        opacity: 1;
        pointer-events: all;
      }
      #fairy-progress-overlay-card {
        background: var(--fairy-panel, #fff);
        color: var(--fairy-text, #222);
        border-radius: 14px;
        box-shadow: 0 12px 40px rgba(0, 0, 0, 0.25);
        padding: 24px 28px;
        max-width: 480px;
        width: calc(100% - 48px);
        max-height: 80vh;
        overflow-y: auto;
        overflow-x: hidden;
        box-sizing: border-box;
      }
      #fairy-progress-overlay-title {
        font-size: 17px;
        font-weight: 600;
        text-align: center;
        margin-bottom: 6px;
      }
      /* Progress fairy: climbs down into the dialog then spins */
      @keyframes fairy-climb-in {
        0%   { transform: translateY(-120px) rotate(-15deg); opacity: 0; }
        60%  { transform: translateY(6px)   rotate(5deg);   opacity: 1; }
        80%  { transform: translateY(-4px)  rotate(-3deg);  opacity: 1; }
        100% { transform: translateY(0)     rotate(0deg);   opacity: 1; }
      }
      @keyframes fairy-idle-spin {
        0%   { transform: rotate(-8deg) scale(1);    }
        25%  { transform: rotate(8deg)  scale(1.06); }
        50%  { transform: rotate(-8deg) scale(1);    }
        75%  { transform: rotate(5deg)  scale(0.96); }
        100% { transform: rotate(-8deg) scale(1);    }
      }
      .fairy-progress-wrap {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 10px;
        padding: 4px 0 2px;
      }
      .fairy-progress-figure {
        width: 56px; height: 56px;
        visibility: hidden;
        animation: fairy-idle-spin 2.4s ease-in-out infinite;
      }
      .fairy-progress-figure.fairy-landed {
        visibility: visible;
      }
      .fairy-progress-figure svg { width: 100%; height: 100%; }
      /* The 'corner fairy flies in and lands' animation (see the
         .fairy-landed toggling JS) only ever targets a .swal2-popup /
         .sweet-alert dialog to compute its landing spot — it never
         fires for our own #fairy-progress-overlay, which isn't a swal2
         dialog. Left at its default visibility: hidden, the figure
         still reserved its full 56px box here with nothing to show for
         it — the large empty gap reported between the chip lists and
         the progress bar. Collapse it to zero size in this one context
         instead of touching the shared class (still used, and still
         landing normally, on swal2 popups elsewhere). */
      #fairy-progress-overlay-body .fairy-progress-figure {
        display: none;
      }
      .fairy-progress-dots {
        font-size: 20px;
        letter-spacing: 4px;
        color: var(--fairy-primary, #3a6df0);
        opacity: 0.6;
        animation: fairy-idle-spin 1.8s ease-in-out infinite;
      }
      .fairy-progress-cores {
        font-size: 14px;
        font-weight: 600;
        color: var(--fairy-primary, #3a6df0);
        background: color-mix(in srgb, var(--fairy-primary, #3a6df0) 10%, transparent);
        border-radius: 999px;
        padding: 5px 14px;
        display: inline-flex;
        align-items: center;
        gap: 6px;
      }
      .fairy-progress-done {
        margin-top: 4px;
        padding-top: 12px;
        border-top: 1px solid var(--fairy-border, #e5e5e5);
        text-align: left;
        width: 100%;
        max-width: 340px;
      }
      .fairy-progress-done-label {
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.05em;
        text-transform: uppercase;
        color: #2e7d32;
        margin-bottom: 8px;
        display: flex;
        align-items: center;
        gap: 5px;
      }
      .fairy-progress-done-chips {
        display: flex;
        flex-wrap: wrap;
        gap: 7px;
      }
      .fairy-progress-done-chip {
        display: inline-flex;
        align-items: center;
        gap: 5px;
        font-size: 13.5px;
        font-weight: 500;
        color: #2e7d32;
        background: rgba(46, 125, 50, 0.08);
        border: 1px solid rgba(46, 125, 50, 0.22);
        border-radius: 999px;
        padding: 4px 12px 4px 9px;
      }
      .fairy-progress-items {
        text-align: left;
        width: 100%;
        max-width: 340px;
      }
      .fairy-progress-items-label {
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.05em;
        text-transform: uppercase;
        color: var(--fairy-primary, #3a6df0);
        margin-bottom: 8px;
      }
      .fairy-progress-items-chips {
        display: flex;
        flex-wrap: wrap;
        gap: 7px;
      }
      .fairy-progress-items-chip {
        display: inline-flex;
        align-items: center;
        font-size: 13.5px;
        font-weight: 500;
        color: var(--fairy-primary, #3a6df0);
        background: color-mix(in srgb, var(--fairy-primary, #3a6df0) 8%, transparent);
        border: 1px solid color-mix(in srgb, var(--fairy-primary, #3a6df0) 22%, transparent);
        border-radius: 999px;
        padding: 4px 12px;
      }
      /* Overflow marker chip ('+N more') appended once a chip list is
         capped — styled muted/dashed so it visibly reads as 'there's
         more, not shown' rather than as just another model. */
      .fairy-progress-chip-more {
        color: var(--fairy-text-muted, #888) !important;
        background: transparent !important;
        border-style: dashed !important;
        font-style: italic;
      }
      .fairy-progress-bar-track {
        width: 220px;
        height: 6px;
        border-radius: 999px;
        background: color-mix(in srgb, var(--fairy-primary, #3a6df0) 12%, transparent);
        overflow: hidden;
      }
      .fairy-progress-bar-fill {
        height: 100%;
        border-radius: 999px;
        background: var(--fairy-primary, #3a6df0);
        transition: width 0.3s ease;
      }
      .fairy-progress-bar-indeterminate {
        height: 100%;
        width: 40%;
        border-radius: 999px;
        background: var(--fairy-primary, #3a6df0);
        animation: fairy-bar-slide 1.1s ease-in-out infinite;
      }
      @keyframes fairy-bar-slide {
        0%   { transform: translateX(-100%); }
        100% { transform: translateX(350%); }
      }
      .fairy-progress-count {
        font-size: 12.5px;
        font-weight: 500;
        color: #999;
      }
      .model-card::after {
        content: 'click to expand';
        position: absolute;
        bottom: 6px; right: 10px;
        font-size: 10px;
        color: var(--fairy-text-muted, #aaa);
        opacity: 0;
        transition: opacity 0.2s ease;
        pointer-events: none;
        letter-spacing: 0.02em;
      }
      .model-card:hover::after { opacity: 1; }

      /* Fullscreen overlay for double-clicked cards */
      #fairy-card-overlay {
        display: flex;
        position: fixed;
        inset: 0;
        z-index: 9999;
        background: rgba(0,0,0,0);
        pointer-events: none;
        transition: background 0.18s ease;
      }
      #fairy-card-overlay.active {
        background: rgba(0,0,0,0.85);
        pointer-events: all;
      }
      #fairy-card-overlay-inner {
        background: var(--fairy-panel, #fff);
        border-radius: 0;
        padding: 56px 48px 40px;
        width: 100vw;
        height: 100vh;
        overflow: auto;
        position: relative;
        cursor: default;
        opacity: 0;
        transition: opacity 0.18s ease;
        box-sizing: border-box;
        /* Center the expanded content in the middle of the screen
           instead of pinning it to the top-left of the fullscreen
           overlay — display:flex on the scrollable container itself so
           content taller than the viewport still scrolls normally
           rather than getting clipped by a fixed-height centered box. */
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
      }
      #fairy-card-overlay-content {
        /* NOT width:100% — an explicit width overrides align-items:
           center on the flex parent (align-items only centers items
           whose own cross-axis size is auto), which was silently
           defeating the centering above. max-width alone still keeps
           wide content from overflowing. */
        max-width: 100%;
      }
      #fairy-card-overlay.active #fairy-card-overlay-inner {
        opacity: 1;
      }
      #fairy-card-overlay-inner textarea {
        min-height: 320px;
      }
      #fairy-card-overlay-content .model-card-title {
        font-size: 17px;
        margin-bottom: 20px;
      }
      #fairy-card-overlay-close {
        position: fixed;
        top: 16px; right: 20px;
        background: rgba(120,120,120,0.15);
        border: none;
        border-radius: 999px;
        width: 32px; height: 32px;
        display: flex; align-items: center; justify-content: center;
        font-size: 18px; cursor: pointer;
        color: var(--fairy-text-muted, #666);
        line-height: 1;
        transition: background 0.15s, color 0.15s;
        z-index: 10000;
      }
      #fairy-card-overlay-close:hover {
        background: var(--fairy-primary, #6e45e2);
        color: #fff;
      }
      .model-card .model-card-title {
        font-size: 14px;
        font-weight: 600;
        color: var(--fairy-text);
        margin: 0 0 10px;
        padding-bottom: 8px;
        border-bottom: 1px solid var(--fairy-border);
      }
      .model-card .dataTables_wrapper { margin: 0; }
      /* DataTables' own Show-N-entries and Search controls sit on the
         same row with no built-in gap between them — fine when the
         table has plenty of width, but they can crowd/overlap once it
         doesn't. A little breathing room plus letting them wrap onto
         separate lines if genuinely tight keeps that from happening. */
      .dataTables_wrapper .dataTables_length { margin-right: 24px; }
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter {
        display: inline-block;
        max-width: 100%;
      }
      /* wide tables / equations must scroll inside the card, never spill
         out. NOTE: overflow-x:auto with no overflow-y set is NOT safe to
         put on an ancestor of a native <select> — per the CSS spec, if
         one axis is non-visible the OTHER is forced off visible too
         (even set explicitly to visible, it still computes to auto),
         so this unavoidably makes the element a scroll-clipping container
         on BOTH axes. That clipped/broke native <select> dropdown popups
         living inside it in several browsers (confirmed: the parsimony
         table's Show-N-entries length picker stopped registering clicks
         on its options once .model-def-panel — the tab's own outer
         wrapper, home to plenty of <select>s — picked up this rule too).
         Scoped to .model-card only, which is used purely for small result
         cards (an equation, a chart) that never contain a <select>. */
      .model-card {
        overflow-x: auto;
        max-width: 100%;
      }
      .model-def-panel {
        overflow: visible;
        max-width: 100%;
      }
      .model-card table.dataTable { width: 100% !important; }

      /* Long H-representation tables: cap height and scroll inside the
         normal (unexpanded) card so a 100+ row model doesn't blow the
         card out to several screens tall — expanding the card (see
         openOverlay) lifts this cap so the full table is visible at
         once there instead. */
      .fairy-h-table-scroll {
        max-height: 420px;
        max-width: 100%;
        overflow-y: auto;
        /* A model with many item-expanded parameters (each side of the
           table now gets its own column per parameter — see the H-rep
           sign-normalization redesign) can get far wider than the card;
           without this it just overflowed straight past the card's own
           edges with no way to see the rest, instead of scrolling
           within its own boundary the way the vertical case already
           did. */
        overflow-x: auto;
      }
      #fairy-card-overlay-content .fairy-h-table-scroll {
        max-height: none;
        overflow-y: visible;
      }

      /* Settings buttons (Approximate equalities / V-representation) -
         replace the uneven 'unite' pills with clean, consistent blocks. */
      #show_approx_erros, #show_v_rep, #show_intersections, #show_mixtures, #show_items {
        display: block !important;
        width: 100% !important;
        margin: 5px 0 !important;
        padding: 8px 12px !important;
        font-size: 13px !important;
        font-weight: 500 !important;
        line-height: 1.4 !important;
        white-space: normal !important;
        text-align: left !important;
        border-radius: 8px !important;
        border: 1px solid var(--fairy-border) !important;
        background: var(--ct-default) !important;
        color: var(--fairy-text) !important;
        box-shadow: none !important;
        transition: background 0.15s ease, border-color 0.15s ease, color 0.15s ease !important;
      }
      #show_intersections.disabled, #show_intersections:disabled,
      #show_mixtures.disabled, #show_mixtures:disabled {
        opacity: 0.45 !important;
        cursor: not-allowed !important;
        background: var(--ct-nodim) !important;
        color: var(--fairy-text-muted) !important;
      }
      /* Greys out individual checkboxGroupButtons choices (intersection /
         mixture pickers) that are incompatible with the current selection —
         different model type or a different number of parameters. */
      .choice-disabled {
        opacity: 0.35 !important;
        pointer-events: none !important;
        filter: grayscale(60%);
      }
      #show_approx_erros:hover, #show_v_rep:hover, #show_intersections:hover, #show_mixtures:hover:hover, #show_items:hover,
      #show_approx_erros:focus, #show_v_rep:focus, #show_intersections:focus, #show_mixtures:focus:focus, #show_items:focus {
        background: var(--ct-diag) !important;
        border-color: var(--fairy-primary) !important;
        color: var(--fairy-primary-dark) !important;
        /* Same hover ring used across the app now (V-rep toggle, jelly
           buttons, delete) — kept here too for one consistent hover
           language, on top of this row's own existing background/
           border/text feedback rather than replacing it. Folded into
           the SAME rule as :hover (not a separate later :focus rule
           that resets box-shadow to none) — clicking a button leaves it
           both hovered and focused at once, and a later same-specificity
           !important rule always wins, so the old separate :focus block
           was silently erasing this ring the moment it was reported not
           showing up. */
        box-shadow: 0 0 0 3px color-mix(in srgb, var(--fairy-primary, #3a6df0) 25%, transparent) !important;
        outline: none !important;
      }
      /* Kill the 'unite' hover-fill pseudo-elements (the half-blue diagonal) */
      #show_approx_erros::before, #show_v_rep::before, #show_intersections::before, #show_mixtures::before::before, #show_items::before,
      #show_approx_erros::after, #show_v_rep::after, #show_intersections::after, #show_mixtures::after::after, #show_items::after {
        display: none !important;
        content: none !important;
        background: none !important;
      }

      .parallel-switch-wrap .form-group { margin-bottom: 0 !important; }
      .parallel-switch-wrap { padding: 2px 0; }
      .parallel-switch-wrap label { font-size: 13px !important; color: var(--fairy-text) !important; font-weight: 500 !important; }

      /* Green highlight for selected checkbox-group buttons */
      .checkbox-group-buttons .btn.active,
      .checkbox-group-buttons .btn:active {
        background-color: #3d9970 !important;
        border-color: #2e7d5a !important;
        color: #fff !important;
      }

      /* Model Properties tab — visually distinct, rightmost */
      .navbar-default .navbar-nav > li:last-child > a {
        color: var(--fairy-primary) !important;
        font-weight: 600 !important;
        border-left: 2px solid var(--fairy-border);
        margin-left: 6px;
        padding-left: 14px;
      }
      .navbar-default .navbar-nav > li:last-child > a:hover {
        background: var(--ct-diag) !important;
      }
      .navbar-default .navbar-nav > li:last-child.active > a,
      .navbar-default .navbar-nav > li:last-child.active > a:hover,
      .navbar-default .navbar-nav > li:last-child.active > a:focus {
        background: var(--fairy-primary-contrast-bg) !important;
        color: #fff !important;
      }

      /* Buttons - keep shinyWidgets 'jelly'/'unite' styles but recolor
         primary. --fairy-primary-contrast-bg (not --fairy-primary
         directly): every one of these renders white text on the
         filled background (Go, every modal's Save/Add/Submit, …) — see
         that variable's own comment for why dark mode specifically
         needs a separate, darker shade to keep that text at WCAG AA
         contrast. */
      .bttn-primary.bttn-jelly, .bttn-primary.bttn-unite {
        background: var(--fairy-primary-contrast-bg) !important;
      }
      .bttn-default.bttn-jelly, .bttn-default.bttn-unite {
        background: var(--ct-nodim) !important;
        color: var(--fairy-text) !important;
      }

      /* Inputs */
      textarea, input[type='text'], input[type='number'], .selectize-input {
        border-radius: 6px !important;
        border: 1px solid var(--fairy-border) !important;
      }
      textarea:focus, input:focus, .selectize-input.focus {
        border-color: var(--fairy-primary) !important;
        box-shadow: 0 0 0 3px rgba(58, 109, 240, 0.15) !important;
      }

      /* Tooltips */
      .tooltip-inner {
        background-color: #1b2130;
        color: #f4f6fa;
        font-size: 12px;
        max-width: 320px;
        text-align: left;
        border-radius: 6px;
      }

      /* Bottom banner - slim bar. The outer .navbar-fixed-bottom carries a
         Bootstrap min-height:50px that leaves a tall empty band (and balloons
         when zoomed); collapse it so the bar hugs its text. */
      .navbar-fixed-bottom,
      .navbar-fixed-bottom .container-fluid,
      .navbar-fixed-bottom .container {
        min-height: 0 !important;
        height: auto !important;
      }
      #banner {
        background-color: var(--fairy-navbar) !important;
        border: none;
        box-shadow: 0 -2px 8px rgba(0,0,0,0.15);
        min-height: 0;
        margin-bottom: 0;
      }
      .navbar-fixed-bottom .navbar-brand,
      .navbar-fixed-bottom .navbar-header {
        display: none !important;
      }
      #banner .container-fluid, #banner .container { padding: 0; }
      #banner p {
        margin: 0;
        padding: 2px 12px;
      }
      #banner span, #banner p {
        line-height: 1.2;
        font-size: 10.5px;
      }

      a { color: var(--fairy-primary); }
      a:hover { color: var(--fairy-primary-dark); }
      "
    )),
    # A CSS "plays once on mount" animation (.fairy-intro-pulse-wrap,
    # used by the V-rep master toggle, delete-all, and intersection/
    # mixture buttons) restarts whenever an ancestor goes through
    # display:none and back — e.g. switching to another top-level tab
    # and back to Input, which hides/shows that whole tab-pane. The
    # server side only ever adds this class on a genuine first
    # appearance (see vrep_toggle_all_appear_id's own comment for that
    # half of the fix), but once added it just sits in the DOM
    # indefinitely with nothing to remove it again — so ANY later
    # display:none/block toggle on an ancestor replays the animation
    # from scratch, with no new server render involved at all. Strip the
    # class once the animation has actually had time to finish playing
    # (0.6s x 5 iterations, see fairy-intro-pulse-kf) so nothing is left
    # in the DOM for a later tab-visibility change to restart. Delegated
    # via MutationObserver rather than a per-button event binding, since
    # new instances of this class can appear at any time from any of
    # several different renderUI outputs.
    # Accessibility: most icon-only controls in this app (trash, cube,
    # segmented toggles, downloads, …) get their only human-readable
    # label from data-tooltip on a WRAPPING span (see .fairy-tooltip's
    # own CSS) — shiny::icon()'s own auto aria-label is neutralized by
    # the role=presentation it also sets, and a data-tooltip attribute
    # on a non-interactive wrapper is invisible to a screen reader
    # regardless (they announce the focusable element's OWN accessible
    # name, not an ancestor's data attribute). Confirmed directly: none
    # of these buttons had a usable aria-label at all. Copies
    # data-tooltip onto the nearest actually-interactive descendant as
    # aria-label instead, generically for every .fairy-tooltip in the
    # app rather than touching each call site — reuses the SAME
    # MutationObserver already watching for newly-added elements (see
    # fairyStripPulse just below) rather than adding a second one.
    tags$script(HTML(
      "(function() {
        function fairyStripPulse(el) {
          setTimeout(function() { el.classList.remove('fairy-intro-pulse-wrap'); }, 3000);
        }
        function fairySyncTooltipAria(el) {
          var tip = el.getAttribute('data-tooltip');
          if (!tip) return;
          var target = el.matches('button,a,input,select,textarea')
            ? el : el.querySelector('button,a,input,select,textarea');
          if (target && !target.getAttribute('aria-label')) target.setAttribute('aria-label', tip);
        }
        function fairyHandleAddedNode(node) {
          if (node.nodeType !== 1) return;
          if (node.classList && node.classList.contains('fairy-intro-pulse-wrap')) fairyStripPulse(node);
          if (node.querySelectorAll) {
            node.querySelectorAll('.fairy-intro-pulse-wrap').forEach(fairyStripPulse);
          }
          if (node.matches && node.matches('.fairy-tooltip[data-tooltip]')) fairySyncTooltipAria(node);
          if (node.querySelectorAll) {
            node.querySelectorAll('.fairy-tooltip[data-tooltip]').forEach(fairySyncTooltipAria);
          }
        }
        var fairyPulseObserver = new MutationObserver(function(mutations) {
          mutations.forEach(function(m) {
            m.addedNodes.forEach(fairyHandleAddedNode);
          });
        });
        function fairyStartPulseObserver() {
          document.querySelectorAll('.fairy-tooltip[data-tooltip]').forEach(fairySyncTooltipAria);
          fairyPulseObserver.observe(document.body, {childList: true, subtree: true});
        }
        if (document.body) {
          fairyStartPulseObserver();
        } else {
          document.addEventListener('DOMContentLoaded', fairyStartPulseObserver);
        }
      })();
      "
    )),
    tags$script(HTML(
      "var fairyTricks = ['trick-dance','trick-spin','trick-loop','trick-flip','trick-bounce'];
       $(document).on('shiny:busy', function(){
         var f = $('#corner-fairy');
         f.removeClass(fairyTricks.join(' '));
         var t = fairyTricks[Math.floor(Math.random() * fairyTricks.length)];
         f.addClass('dancing').addClass(t);
       });

       // Easter egg: click the corner fairy for a wand-wave, a burst of
       // sparkles, and a quip in her own voice (paraphrasing the
       // mathematics fairy from the paper, e.g. 'Nice models, everyone!
       // Let me clean up a few things for you.').
       (function() {
         var quips = [
           'Nice models, everyone! Let me clean up a few things for you.',
           'Redundant constraints? Poof \\u2014 gone.',
           'Your verbal theory is now a convex polytope. You are welcome.',
           'H-representation, \\ud835\\udcb1-representation \\u2014 just call me the conversion fairy.',
           'I minimized your description. It took three times forever.',
           'Falsifiable and proud of it.',
           'Ambiguity spotted. Ambiguity resolved.',
           'A tight theory is a happy theory.',
           'I do not do interval scales. Ask me about probabilities instead.',
           'Somewhere, a Null hypothesis just sighed in relief.',
           'Some models have more edge cases than a dodecahedron. I have seen things.',
           'I do not do point predictions. Too mainstream for a polytope.',
           'Careful with that equality constraint \\u2014 I have seen what over-specification does to a theory.',
           'Vertices enumerated, volume estimated, dignity of the strawman Null preserved.',
           'I once minimized a description from 64 constraints down to 12. Ask me about my day.',
           'Somewhere a reviewer just asked \\u201Cbut what does this predict, exactly\\u201D \\u2014 not on my watch.',
           'Intersection checks are quick. Waiting for you to click submit is the slow part.',
           'I do not do overfitting. I do fitting, and then I stop.',
           'A p-value walked in here once. I redirected it to the Parsimony tab.',
           'I asked your model for its degrees of freedom. It said \\u201Cit is complicated.\\u201D',
           'Your Null hypothesis called. It wants to know why nobody invites it anywhere.',
           'I do not grant wishes. I grant nonredundant \\ud835\\udcb1-representations.',
           'Behind every tight theory is a fairy who checked the boundary cases twice.',
           'I turn hand-wavy claims into inequalities for a living. Ask me how many, I dare you.',
           'Two models walked into a bar. Only one of them was full-dimensional.',
           'I do not believe in coincidences. I believe in disjoint polytopes.',
           'Your marginal probabilities called. They said the joint model started it.',
           'I have seen theories so vague they had a Bayes factor of 1. Tragic.',
           'Ask me anything, except to interpret a nonsignificant result as evidence for the Null.',
           'I do not do measurement error. I do binary outcomes and a clean conscience.',
           'Somewhere, a strawman Null hypothesis is being rejected. It is fine. It is used to it.',
           'I once watched a scholar add seventeen auxiliary assumptions. I aged a century.',
           'If a replication ever fails, check your sampling assumption before you blame the theory.',
           'Smaller volume, sharper theory. It is basically Occam\\u2019s razor with better geometry.',
           'Somewhere a scholar just said \\u201Cthe theory predicts an increase\\u201D without saying in what. I felt that.',
           'I count vertices for a living. It is faster than it sounds.',
           'If a hidden auxiliary assumption ever tries to sneak into your model, I ask it to state itself as an inequality. It always declines.',
           'Two labs disagreeing? Adorable. Wait until you see them jointly, in 18 dimensions.',
           'I do not do rhetoric. I do falsifiable conjunctions of order constraints.',
           'Somewhere, a reviewer wrote \\u201Cbut is this really testable\\u201D and I have never been so offended on your behalf.',
           'I do not editorialize. I just enumerate your polytope and let it speak for itself.',
           'I asked your hypothesis what it does NOT predict. It answered instantly, without blinking. That is the good kind of falsifiable.',
           'Give me a verbal theory and I will give you a polytope. Give me a vague verbal theory and I will give you a nap.',
           'I do not do point predictions, but I will happily watch you defend one at your next talk.',
           'An empty intersection means maximally discriminable theories. Tidy, in its own way.',
           'I redundancy-checked your constraints while you were still typing the third one. Occupational hazard.',
           'Somewhere a scale-level debate is happening. I am not attending. I have probabilities and they are enough.',
           'A max Bayes factor can be a number, or it can be infinity. I do not play favorites.',
           'I do not grant three wishes. I grant one minimal \\ud835\\udcb1-representation and call it a day.',
           'A theory with a near-zero volume walked past. I bowed. It deserved it.'
         ];
         var sparkleGlyphs = ['\\u2728','\\u2b50','\\u2726','\\u22c6'];

         function burstSparkles(cx, cy) {
           for (var i = 0; i < 16; i++) {
             var el = document.createElement('span');
             el.className = 'fairy-sparkle';
             el.textContent = sparkleGlyphs[Math.floor(Math.random() * sparkleGlyphs.length)];
             var ang = Math.random() * Math.PI * 2;
             var dist = 70 + Math.random() * 150;
             var dx = Math.cos(ang) * dist, dy = Math.sin(ang) * dist;
             el.style.left = cx + 'px';
             el.style.top  = cy + 'px';
             el.style.setProperty('--fairy-sparkle-end', 'translate(' + dx + 'px,' + dy + 'px)');
             document.body.appendChild(el);
             (function(node) { setTimeout(function() { node.remove(); }, 1100); })(el);
           }
         }

         function showBubble(anchorRect, text) {
           var old = document.getElementById('fairy-speech-bubble');
           if (old) old.remove();
           var b = document.createElement('div');
           b.id = 'fairy-speech-bubble';
           b.className = 'fairy-speech-bubble';
           b.textContent = text;
           b.style.top   = (anchorRect.bottom + 12) + 'px';
           b.style.right = (window.innerWidth - anchorRect.right) + 'px';
           document.body.appendChild(b);
           requestAnimationFrame(function() { b.classList.add('fairy-bubble-show'); });
           setTimeout(function() {
             b.classList.remove('fairy-bubble-show');
             setTimeout(function() { b.remove(); }, 220);
           }, 3200);
         }

         $(document).on('click', '#corner-fairy', function() {
           var el = document.getElementById('corner-fairy');
           if (!el || el.classList.contains('fairy-away')) return;
           var r = el.getBoundingClientRect();
           var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
           burstSparkles(cx, cy);
           el.classList.remove('trick-cast'); void el.offsetWidth; el.classList.add('trick-cast');
           showBubble(r, quips[Math.floor(Math.random() * quips.length)]);
         });

         // She is busy while riding a progress dialog -- clicking her there
         // gets a different, I-am-working joke instead of the normal quip,
         // and a single sparkle poking fun at the burst she would normally throw.
         var busyQuips = [
           'I am mid-spell. This is a terrible time for a photo.',
           'Busy! Ask me again once your polytope exists.',
           'One sparkle burst per model, please \\u2014 the wand only has so much glitter.',
           'I would wave my wand, but both hands are full of your constraints.',
           'This is the core-algebra face. It is not glamorous.',
           'Poof later. Redundancy elimination now.',
           'I already used my one sparkle burst today, on your H-representation.',
           'Do not distract the fairy mid-lpcdd() call.',
           'I can enumerate your vertices, or I can pose for this. Not both.',
           'The convex hull does not hull itself, you know.',
           'Ask the little dots. They are doing the actual work right now.',
           'Mid-redundant() call. Both hands are full of matrices.',
           'I am scdd()-ing as fast as fairy-ly possible.',
           'Every constraint you typed is now my problem. Please hold.',
           'This dialog box is basically my office right now.',
           'No selfies during a feasibility check. House rules.',
           'I am one Sys.sleep() away from a well-deserved nap.',
           'Counting extreme points. Do not make me lose my place.',
           'I am inside a linear program right now. Literally.',
           'Volumes do not estimate themselves. Well, mostly they do. I still supervise.',
           'This is not a coffee break, it is a rational-arithmetic break.',
           'Give me a second \\u2014 rcdd does not believe in shortcuts.',
           'Parsimony is not going to compute itself, and neither am I, apparently, right now.',
           'The dots are load-bearing. Please do not interrupt them.',
           'I would love to chat, but your polytope has trust issues and needs checking.',
           'Somewhere a redundant inequality is hiding. I will find it.',
           'This is what concentration looks like. Also glitter.',
           'I am busy turning your words into inequalities. Rude of you to notice.',
           'The wand is warm. That means it is working.',
           'Ask me later \\u2014 right now I only speak fractions.',
           'I promised your model a clean minimal description. A fairy keeps her word.',
           'I am somewhere between a vertex and a headache right now.',
           'Every equation gets my full attention, one at a time, thank you.',
           'This is technically a business meeting. With math.',
           'I will be right there. The polytope will not enumerate itself.'
         ];
         $(document).on('click', '.fairy-progress-figure.fairy-landed', function(e) {
           e.stopPropagation();
           var el = this;
           var r = el.getBoundingClientRect();
           var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
           var s = document.createElement('span');
           s.className = 'fairy-sparkle';
           s.textContent = sparkleGlyphs[Math.floor(Math.random() * sparkleGlyphs.length)];
           s.style.left = cx + 'px'; s.style.top = cy + 'px';
           s.style.setProperty('--fairy-sparkle-end', 'translate(0px,-40px)');
           document.body.appendChild(s);
           setTimeout(function() { s.remove(); }, 1100);
           var svg = el.querySelector('svg');
           if (svg) {
             svg.animate([
               { transform: 'rotate(0deg)' },
               { transform: 'rotate(-10deg)' },
               { transform: 'rotate(8deg)' },
               { transform: 'rotate(0deg)' }
             ], { duration: 350, easing: 'ease-in-out' });
           }
           showBubble(r, busyQuips[Math.floor(Math.random() * busyQuips.length)]);
         });
       })();
       $(document).on('shiny:idle', function(){
         $('#corner-fairy').removeClass('dancing ' + fairyTricks.join(' '));
       });
       // Fairy flies from corner into the dialog when a progress alert opens
       (function() {
         var SVG = '<svg viewBox=\"0 0 64 64\" xmlns=\"http://www.w3.org/2000/svg\"><g fill=\"none\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M31 34 C16 20,6 24,10 34 C6 44,18 44,31 34 Z\" fill=\"#8fb3ff\" stroke=\"#5b7fd6\" stroke-width=\"1.5\" opacity=\"0.85\"/><path d=\"M33 34 C48 20,58 24,54 34 C58 44,46 44,33 34 Z\" fill=\"#a9c4ff\" stroke=\"#5b7fd6\" stroke-width=\"1.5\" opacity=\"0.85\"/><circle cx=\"32\" cy=\"20\" r=\"5\" fill=\"#ffd98a\" stroke=\"#e0a94a\" stroke-width=\"1.4\"/><path d=\"M32 25 L32 44 M32 30 L26 38 M32 30 L38 38 M32 44 L27 52 M32 44 L37 52\" stroke=\"#f2b84b\" stroke-width=\"2.4\"/><path d=\"M50 12 l1.6 3.4 3.4 1.6-3.4 1.6-1.6 3.4-1.6-3.4-3.4-1.6 3.4-1.6 Z\" fill=\"#ffe08a\"/></g></svg>';
         var prev = false;
         var flyTimer = null;
         var didFly = false;

         function spawnFlyer(sx, sy) {
           var old = document.getElementById('fairy-flyer');
           if (old) old.remove();
           var f = document.createElement('div');
           f.id = 'fairy-flyer';
           f.innerHTML = SVG;
           f.style.cssText = 'position:fixed;z-index:999999;width:56px;height:56px;' +
             'pointer-events:none;transform-origin:50% 50%;' +
             'left:'+(sx-28)+'px;top:'+(sy-28)+'px;' +
             'filter:drop-shadow(0 2px 8px rgba(90,100,255,.7));';
           document.body.appendChild(f);
           return f;
         }

         function fly() {
           didFly = true;
           var corner = document.getElementById('corner-fairy');
           if (!corner) return;
           var cr = corner.getBoundingClientRect();
           var ox = cr.left + cr.width / 2;
           var oy = cr.top  + cr.height / 2;
           corner.classList.add('fairy-away');
           var f = spawnFlyer(ox, oy);
           setTimeout(function() {
             var dlg = document.querySelector('.swal2-popup, .sweet-alert');
             var tx, ty;
             if (dlg) {
               var dr = dlg.getBoundingClientRect();
               tx = dr.left + dr.width / 2 - ox;
               ty = dr.top  + 110          - oy;
             } else {
               tx = window.innerWidth / 2 - ox;
               ty = window.innerHeight / 2 - oy;
             }
             var ax = tx * 0.4 - 80;
             var ay = Math.min(-160, ty - 160);
             f.animate([
               { transform: 'translate(0,0) rotate(0deg) scale(1)',                         offset: 0    },
               { transform: 'translate(-6px,-20px) rotate(8deg) scale(1.25)',               offset: 0.07 },
               { transform: 'translate('+ax+'px,'+ay+'px) rotate(-50deg) scale(1.5)',       offset: 0.38 },
               { transform: 'translate('+tx+'px,'+(ty+18)+'px) rotate(20deg) scale(1.1)',   offset: 0.80 },
               { transform: 'translate('+tx+'px,'+ty+'px) rotate(-5deg) scale(0.88)',       offset: 0.92 },
               { transform: 'translate('+tx+'px,'+ty+'px) rotate(0deg) scale(1)',           offset: 1    }
             ], { duration: 950, easing: 'ease-in', fill: 'forwards' }
             ).onfinish = function() {
               f.remove();
               var dlgF2 = document.querySelector('.fairy-progress-figure');
               if (dlgF2) dlgF2.classList.add('fairy-landed');
             };
           }, 300);
         }

         function land() {
           if (!didFly) return;
           didFly = false;
           var corner = document.getElementById('corner-fairy');
           var dlgF = document.querySelector('.fairy-progress-figure');
           // Determine start position: dialog fairy if landed, else screen centre
           var sx, sy;
           if (dlgF && dlgF.classList.contains('fairy-landed')) {
             var dr = dlgF.getBoundingClientRect();
             sx = dr.left + dr.width / 2;
             sy = dr.top  + dr.height / 2;
             dlgF.classList.remove('fairy-landed');
           } else {
             var existingF = document.getElementById('fairy-flyer');
             if (existingF) { existingF.remove(); }
             sx = window.innerWidth / 2;
             sy = window.innerHeight / 2;
           }
           if (!corner) return;
           var cr = corner.getBoundingClientRect();
           var ex = cr.left + cr.width / 2;
           var ey = cr.top  + cr.height / 2;
           var tx = ex - sx;
           var ty = ey - sy;
           var ax = tx * 0.3 + 70;
           var ay = Math.min(ty - 100, -80);
           var f2 = spawnFlyer(sx, sy);
           f2.animate([
             { transform: 'translate(0,0) rotate(0deg) scale(1)',                       offset: 0    },
             { transform: 'translate(5px,-22px) rotate(-10deg) scale(1.28)',            offset: 0.09 },
             { transform: 'translate('+ax+'px,'+ay+'px) rotate(45deg) scale(1.45)',     offset: 0.45 },
             { transform: 'translate('+tx+'px,'+(ty+10)+'px) rotate(-12deg) scale(0.9)', offset: 0.87 },
             { transform: 'translate('+tx+'px,'+ty+'px) rotate(0deg) scale(0.82)',      offset: 1    }
           ], { duration: 900, easing: 'ease-in-out', fill: 'forwards' }
           ).onfinish = function() {
             f2.remove();
             corner.classList.remove('fairy-away');
             var svg = corner.querySelector('svg');
             if (svg) {
               svg.animate([
                 { transform: 'scale(0.82) translateY(0)'   },
                 { transform: 'scale(1.3)  translateY(-5px)' },
                 { transform: 'scale(0.95) translateY(0)'   },
                 { transform: 'scale(1.12) translateY(-2px)' },
                 { transform: 'scale(1)    translateY(0)'   }
               ], { duration: 500, easing: 'ease-out' });
             }
           };
         }

         setInterval(function() {
           var dlg = document.querySelector('.sweet-alert');
           var has = !!(dlg && dlg.style.display === 'block');
           if (has === prev) return;
           prev = has;
           if (has) {
             didFly = false;
             flyTimer = setTimeout(fly, 15000);
           } else {
             if (flyTimer) { clearTimeout(flyTimer); flyTimer = null; }
             land();
           }
         }, 150);
       })();"
    )),
    # Cosmetic-only: display ">"/"<" typed into the constraint boxes as
    # "\u2265"/"\u2264" (the app already treats a bare > or < as
    # non-strict internally, so this just makes that visible). The boxes'
    # actual value/what gets sent to the server is left completely
    # untouched — a transparent overlay div is drawn on top showing the
    # substituted text, while the real textarea underneath (still holding
    # plain ">"/"<") keeps handling all typing/focus/paste/selection
    # natively. Deliberately not rewriting the submitted value itself: this
    # app has many separate places that parse that raw text, with no single
    # choke point to intercept, so changing what's actually sent would risk
    # silently breaking one of them.
    tags$style(HTML(
      "
      .fairy-ineq-wrap { position: relative; display: block; width: 100%; }
      .fairy-ineq-overlay {
        position: absolute;
        top: 0; left: 0; right: 0; bottom: 0;
        pointer-events: none;
        overflow: hidden;
        white-space: pre-wrap;
        word-wrap: break-word;
        box-sizing: border-box;
        text-align: left;
      }
      .fairy-ineq-wrap textarea {
        color: transparent !important;
        caret-color: #333;
        position: relative;
        width: 100%;
        box-sizing: border-box;
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace !important;
      }
      .fairy-ineq-overlay { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace !important; }
      body.dark-mode .fairy-ineq-wrap textarea { caret-color: #eee; }
      .fairy-ineq-wrap textarea::placeholder { color: #999; -webkit-text-fill-color: #999; }
      /* Model Specification: read-only display box, styled to match the
         editable boxes next to it (same fixed height, font, border). Row
         layout itself is a plain HTML table now (see the R UI code) — no
         grid/flex/splitLayout involved, so nothing here needs to fight a
         layout mode's own alignment behavior.
         Confirmed Safari-only (fine in Firefox) — matches a known Safari
         behavior: if the Mac is set to always show scrollbars (System
         Settings, Appearance), Safari reserves real layout space for a
         classic scrollbar on a scrollable element, where Chrome/Firefox
         use a space-free overlay scrollbar — which can grow a fixed-height
         scrollable box's actual rendered size out of sync with its
         non-scrollable siblings. .fairy-model-spec-outer is a second,
         outer layer with a hard overflow:hidden at the exact same fixed
         size, so regardless of what the inner element does to fit its own
         scrollbar, nothing can escape this outer boundary. */
      .fairy-plot-expand-btn {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        color: #888 !important;
        padding: 2px 6px !important;
        font-size: 12px !important;
        line-height: 1;
      }
      .fairy-plot-expand-btn:hover { color: var(--fairy-primary) !important; }
      /* Go/Download/Upload all sit in ONE row in the floating panel
         (Input tab) — see the panel's own row-wrap layout, which forces
         .fairy-fab-handle onto its own line above so Go+icons wrap onto
         a shared row below it, all as plain flex siblings. With nothing
         above this row, a hover tooltip popping upward lands in open
         space instead of covering Go, unlike an earlier stacked-below
         layout where it always did. */
      .fairy-toolbar-iconbtn {
        box-sizing: border-box !important;
        width: 36px !important;
        height: 36px !important;
        padding: 0 !important;
        font-size: 14px !important;
        border-radius: 10px !important;
        background: var(--ct-default) !important;
        border: 1px solid var(--fairy-border) !important;
        /* Primary blue (matching Go), not the muted body-text grey —
           the grey read as a disabled/inactive button even though it's
           a perfectly live, clickable one. */
        color: var(--fairy-primary) !important;
        display: inline-flex !important;
        align-items: center;
        justify-content: center;
        transition: border-color 0.12s ease, color 0.12s ease, transform 0.12s ease;
      }
      .fairy-toolbar-iconbtn:hover {
        border-color: var(--fairy-primary) !important;
        color: var(--fairy-primary) !important;
        background: color-mix(in srgb, var(--fairy-primary) 10%, var(--ct-default)) !important;
        transform: translateY(-1px);
      }
      /* Font Awesome's SVG icons don't always fully take the button's
         own `color` — some render with a built-in partial-opacity fill
         regardless of the parent's color, which is why these still
         looked pale/washed-out blue instead of the same solid blue Go
         uses even after the color change above. Force both explicitly
         on the icon itself. */
      .fairy-toolbar-iconbtn svg,
      .fairy-toolbar-fileinput .btn-file svg {
        color: var(--fairy-primary) !important;
        fill: currentColor !important;
        opacity: 1 !important;
      }
      /* Muted (grey, not primary blue) state — Download until there's
         actually something to export (a server-side observer, not
         client-side polling, toggles this — see it near the download
         downloadButton's own R definition). Hardcoded #9aa3b5 rather
         than var(--fairy-text-muted)/currentColor — those went through
         SVG fill/color indirection that, in practice, rendered far
         darker than intended; a flat literal color on both the element
         and its svg, at equal !important specificity, leaves nothing
         to resolve ambiguously. Also genuinely inert, not just styled
         to look inactive — pointer-events:none, since a downloadButton
         is a plain <a href=...> that otherwise still navigates/
         downloads on click regardless of how it's styled. */
      .fairy-toolbar-iconbtn.fairy-btn-muted,
      .fairy-toolbar-iconbtn.fairy-btn-muted:hover,
      .fairy-toolbar-iconbtn.fairy-btn-muted svg,
      .fairy-toolbar-iconbtn.fairy-btn-muted:hover svg {
        color: #9aa3b5 !important;
        fill: #9aa3b5 !important;
      }
      .fairy-toolbar-iconbtn.fairy-btn-muted {
        background: var(--ct-default) !important;
        border-color: var(--fairy-border) !important;
        pointer-events: none !important;
        cursor: default !important;
      }
      .fairy-toolbar-iconbtn.fairy-btn-muted:hover {
        transform: none !important;
      }
      /* Shiny's downloadButton() ships with class=disabled (plus
         aria-disabled/tabindex/an empty href) baked into its default
         HTML, on EVERY download link in the app — cleared only once
         Shiny's own client JS arms that specific link on its first
         interaction, unrelated to fairy-btn-muted or this app's code
         at all (confirmed the exact same disabled class on #d_h,
         untouched by any of this). Bootstrap's own CSS then renders
         that class as pale/dimmed with pointer-events:none — visually
         indistinguishable from fairy-btn-muted, so once content IS
         filled in and fairy-btn-muted is correctly removed, the button
         still looked stuck exactly the same way, purely because this
         SEPARATE Shiny-level flag hadn't cleared yet. Two independent
         looks-muted sources were fighting for the same look with no
         way to tell them apart — force this one to the normal active
         style whenever fairy-btn-muted (the one state that's actually
         supposed to look inactive) isn't present, and let real clicks
         reach it instead of silently bouncing off pointer-events:none,
         so it only takes the one that actually starts the download. */
      .fairy-toolbar-iconbtn.disabled:not(.fairy-btn-muted),
      .fairy-toolbar-iconbtn.disabled:not(.fairy-btn-muted) svg {
        color: var(--fairy-primary) !important;
        fill: var(--fairy-primary) !important;
        opacity: 1 !important;
      }
      .fairy-toolbar-iconbtn.disabled:not(.fairy-btn-muted) {
        pointer-events: auto !important;
        cursor: pointer !important;
      }
      /* d_h/d_v (QTEST) and d_h_multinomineq/d_v_multinomineq
         (multinomineq) share the same plain download icon, with the
         format named only in the hover tooltip — easy to grab the wrong
         one without hovering first. A small corner letter badge (Q/M)
         makes the format visible at a glance instead. */
      #d_h, #d_v, #d_h_multinomineq, #d_v_multinomineq {
        position: relative !important;
      }
      #d_h::after, #d_v::after,
      #d_h_multinomineq::after, #d_v_multinomineq::after {
        content: 'Q';
        position: absolute;
        bottom: -4px;
        right: -4px;
        width: 15px;
        height: 15px;
        border-radius: 999px;
        background: var(--fairy-primary, #3a6df0);
        color: #fff;
        font-size: 9.5px;
        font-weight: 700;
        line-height: 15px;
        text-align: center;
        box-shadow: 0 0 0 2px var(--fairy-panel, #fff);
      }
      #d_h_multinomineq::after, #d_v_multinomineq::after {
        content: 'M';
        background: #8a5cf6;
      }
      /* Stale results: Go/Compute itself blinks instead of a separate
         badge next to it — see the observers toggling this class + the
         button's own data-tooltip together, so hovering the SAME
         element that's blinking explains why.
         Neither background-color NOR box-shadow work here: measured
         live, both stayed constant the entire animation cycle — this
         button (#sidebar-fab-panel .fairy-primary-action) has its own
         box-shadow:none !important (to kill the jelly style's default
         shadow), and .bttn-primary.bttn-jelly sets background with
         !important too. Both consistently beat the matching keyframe
         property regardless of !important inside the keyframe itself.
         filter() is untouched by any existing rule on this button, so
         animate that instead — drop-shadow() paints a colored glow
         without needing box-shadow, and saturate()/brightness() punch
         up the existing blue rather than needing a whole new
         background color. */
      /* hue-rotate()/saturate()/brightness() together read as a cheap,
         gaudy warning-light effect (distorting the button's own color
         rather than adding to it) — replaced with a soft amber glow
         pulse instead (drop-shadow, not box-shadow: box-shadow is
         blocked by !important elsewhere on this button). 0%/100% are
         the SETTLED state — a persistent, clearly-visible glow, not an
         off state — so animation-fill-mode:forwards holds something
         genuinely visible once the 5 iterations finish, rather than
         quietly fading to nothing while still technically stale. 50%
         breathes the glow out further and softer, for the pulse. */
      @keyframes fairyStaleRing {
        0%, 100% {
          filter:
            drop-shadow(0 0 8px rgba(217, 119, 6, 1))
            drop-shadow(0 0 16px rgba(217, 119, 6, 0.55));
          transform: scale(1.04);
        }
        50% {
          filter:
            drop-shadow(0 0 3px rgba(217, 119, 6, 0.6))
            drop-shadow(0 0 22px rgba(217, 119, 6, 0.15));
          transform: scale(1.14);
        }
      }
      .fairy-stale-blink {
        animation: fairyStaleRing 0.9s ease-in-out 5;
        animation-fill-mode: forwards;
      }
      /* fileInput()'s default markup is a Bootstrap .input-group, which
         uses display:table internally and happily claims way more
         width than its content needs — left unchecked, that's what
         made the floating panel balloon out much wider than Go. */
      .fairy-toolbar-fileinput { display: inline-block; position: relative; }
      /* Bootstrap's .btn-file relies on its own position:relative +
         overflow:hidden to CONTAIN the real (invisible, oversized)
         file input it stretches over itself for click-to-browse — the
         usual trick for styling a file input as a button.
         Reported: Download (the button right before this one, sharing
         the same flex row) stopped being clickable at all, while Upload
         itself still worked — exactly what an improperly-contained
         invisible file input bleeding onto its neighbor looks like.
         Pin position/overflow explicitly here rather than trust
         Bootstrap's own rule still wins after everything else in this
         block already overrides .btn-file with !important, and give
         Download itself a higher stacking context so it's never
         coverable by a sibling either way. */
      .fairy-toolbar-fileinput .btn-file {
        position: relative !important;
        overflow: hidden !important;
        z-index: 1;
      }
      #download {
        position: relative;
        z-index: 2;
      }
      .fairy-toolbar-fileinput .form-group {
        margin: 0 !important;
        width: auto !important;
        height: 36px !important;
        position: relative;
        display: flex !important;
        align-items: center;
      }
      .fairy-toolbar-fileinput .input-group { display: inline-flex !important; width: auto !important; height: 100%; }
      .fairy-toolbar-fileinput .input-group-btn { width: auto !important; }
      /* Hides the readonly no-file-selected text box next to Browse —
         the filename isn't especially useful once picked, and dropping
         it is most of what keeps this control from ballooning back out
         to sidebar width. */
      .fairy-toolbar-fileinput input[type='text'] { display: none !important; }
      /* Same square icon-button treatment as Download — Browse's real
         label becomes a Font Awesome upload icon (via fileInput()'s
         buttonLabel, which takes raw HTML) rather than its default
         Browse... text or a hand-drawn CSS shape. */
      .fairy-toolbar-fileinput .btn-file {
        box-sizing: border-box !important;
        width: 36px !important;
        height: 36px !important;
        padding: 0 !important;
        font-size: 14px !important;
        border-radius: 10px !important;
        background: var(--ct-default) !important;
        border: 1px solid var(--fairy-border) !important;
        /* Same primary blue as .fairy-toolbar-iconbtn (Download etc.)
           now uses, not the muted body-text grey — for the same reason:
           grey read as disabled even though the control is perfectly
           live. */
        color: var(--fairy-primary) !important;
        display: inline-flex !important;
        align-items: center;
        justify-content: center;
        vertical-align: top;
      }
      .fairy-toolbar-fileinput .btn-file:hover {
        border-color: var(--fairy-primary) !important;
        color: var(--fairy-primary) !important;
        background: color-mix(in srgb, var(--fairy-primary) 10%, var(--ct-default)) !important;
      }
      /* Shiny's own upload-progress bar hides itself with
         visibility:hidden while idle, not display:none — that still
         reserves its full 90px of layout width even when invisible,
         which is exactly the blank empty-looking gap this left in the
         floating panel. position:absolute takes it out of layout flow
         entirely instead, so it can only ever appear as a genuine
         overlay during an actual upload, never as reserved dead space.
         Constrained to the upload button's own 36px width (not the
         default 90px) so it can't visually spill into neighboring
         elements either way. pointer-events:none on top of that —
         this element is purely informational (an upload-in-progress
         indicator), it never needs to be clicked, so it should never
         be able to sit in front of and intercept clicks meant for a
         REAL button near it (Download, in particular) regardless of
         its exact rendered position/size. */
      .fairy-toolbar-fileinput .progress {
        position: absolute;
        width: 90px;
        max-width: 36px;
        overflow: hidden;
        margin: 4px 0 0;
        pointer-events: none;
      }
      /* Source location for Input's Go/Download/Upload before JS
         reparents them into #sidebar-fab-panel (see FORCE_FLOAT_TABS) —
         never meant to render in place, only to exist somewhere in the
         DOM for that JS to find and move. */
      .fairy-primary-controls-src { display: none; }
      /* Same neutral look as the other per-model icon-column buttons
         (delete, V-rep cube) — background/border/muted text instead of
         a colored pill, for one consistent style across that column. */
      .fairy-eq-tol-btn {
        background-color: var(--ct-default) !important;
        color: var(--fairy-text-muted) !important;
        border: 1px solid var(--fairy-border) !important;
        line-height: 1;
        box-shadow: none !important;
      }
      .fairy-eq-tol-btn:hover, .fairy-eq-tol-btn:focus {
        background-color: var(--ct-nodim) !important;
        border-color: var(--fairy-primary) !important;
        box-shadow: 0 0 0 3px color-mix(in srgb, var(--fairy-primary, #3a6df0) 30%, transparent) !important;
        outline: none !important;
      }
      /* Blink-once-ever animation, added via JS the first time a given
         button id is seen (see the script near the top of the UI) rather
         than kept unconditionally on .fairy-eq-tol-btn itself — this
         element's conditionalPanel wrapper re-evaluates its visibility on
         every keystroke in the model's spec (it depends on that same
         input), and an animation living directly on the always-present
         class would restart on every such re-evaluation, not just the
         first time the equals sign actually appeared. */
      .fairy-eq-tol-btn-flash { animation: fairyEqAppear 0.5s ease-in-out 6; }
      /* V-representation on/off toggle — gray when off, brand blue when
         this model is included (see mytable_v_reactive$value). */
      .fairy-vrep-toggle-btn {
        background-color: var(--ct-default) !important;
        color: var(--fairy-text-muted) !important;
        border: 1px solid var(--fairy-border) !important;
        line-height: 1;
        box-shadow: none !important;
      }
      .fairy-vrep-toggle-btn:hover, .fairy-vrep-toggle-btn:focus {
        /* --ct-nodim vs. this button's own resting --ct-default are
           only a couple shades apart (#f5f5f5 vs #f7f8fb) — reads as no
           feedback at all (reported as no hover effect even after the
           transform fix above). A visible border-color change plus a
           light tinted fill is the same 'clearly-hovering' language the
           jelly buttons now use (box-shadow ring), just as a fill+border
           instead since this button already relies on border-color for
           its own on/off state. */
        background-color: color-mix(in srgb, var(--fairy-primary, #3a6df0) 12%, var(--ct-default)) !important;
        border-color: var(--fairy-primary, #3a6df0) !important;
      }
      /* No known competing rule sets a hover/focus/active transform on
         this button (unlike .bttn-jelly, which has one straight from
         its own library CSS) — but it was reported still visibly
         growing on hover in Safari regardless. Rather than keep
         guessing at a specific source, force every transform-related
         state to identity with maximum specificity (doubled class) and
         kill any transition on transform outright, so there is nothing
         left that COULD animate a size change here, from any rule,
         in any browser. */
      .fairy-vrep-toggle-btn.fairy-vrep-toggle-btn,
      .fairy-vrep-toggle-btn.fairy-vrep-toggle-btn:hover,
      .fairy-vrep-toggle-btn.fairy-vrep-toggle-btn:focus,
      .fairy-vrep-toggle-btn.fairy-vrep-toggle-btn:active {
        transform: none !important;
        -webkit-transform: none !important;
        transition: background-color 0.15s ease, border-color 0.15s ease, color 0.15s ease !important;
      }
      .fairy-vrep-toggle-btn.active {
        background-color: var(--fairy-primary-contrast-bg) !important;
        color: #fff !important;
        border-color: var(--fairy-primary-contrast-bg) !important;
      }
      /* not --fairy-primary-dark here — see --fairy-primary-contrast-bg's own comment */
      .fairy-vrep-toggle-btn.active:hover { background-color: var(--fairy-primary-contrast-bg-hover) !important; }
      /* Master 'include/exclude all' toggle — same colors/states as the
         per-model cube button above (shares .fairy-vrep-toggle-btn), just
         visibly larger so it doesn't get mistaken for just another
         per-row button among the header's other action buttons. */
      .fairy-vrep-toggle-all-btn {
        font-size: 15px !important;
      }
      /* Generic 'just appeared' attention pulse — shared by every
         button that's hidden below some model-count threshold and
         should draw the eye the moment it actually shows up (the
         vrep_toggle_all master toggle, the Intersection/Mixture model
         buttons). Each of those tracks its own 'was visible on the
         previous render' reactiveVal server-side and only wraps the
         button in this on a genuine hidden->visible transition — their
         renderUI re-runs (and the DOM node gets recreated) on every
         relevant change, so a plain 'animate on mount' rule here would
         otherwise replay on every single one of those instead of just
         when it (re)appears. Finite iteration count (not infinite) so
         it settles back to normal after a few pulses instead of
         blinking forever.
         A wrapping <span>, not a class on the button itself — two
         earlier attempts both broke: transform/scale looked odd
         (direct user feedback), and animating box-shadow/filter
         directly on the button ran into the same wall the stale-Go
         blink already hit once (see fairyStaleBlink's own comment) —
         .fairy-vrep-toggle-btn and .bttn-jelly both carry their own
         'box-shadow: none !important' / jelly press-shadow, which
         beats a keyframe on the same property regardless of !important
         inside the keyframe; and filter:drop-shadow, which sidesteps
         that, instead traces the button's actual alpha silhouette —
         fine for a plain icon, but a blurry halo around every letter
         on a text button like 'Intersection model'. A plain wrapper
         span has no competing rules to fight AND no text of its own to
         trace, so box-shadow just works, identically, on every button
         regardless of its content. */
      .fairy-intro-pulse-wrap {
        display: inline-block;
        border-radius: 999px;
        animation: fairy-intro-pulse-kf 0.6s ease-in-out 5;
      }
      @keyframes fairy-intro-pulse-kf {
        0%, 100% {
          box-shadow: 0 0 0 0 transparent;
        }
        50% {
          box-shadow: 0 0 0 4px color-mix(in srgb, var(--fairy-primary, #3a6df0) 35%, transparent);
        }
      }
      /* The 'jelly' bttn.css style's own built-in hover state scales the
         whole button up (its actual rule: .bttn-jelly:focus,
         .bttn-jelly:hover with transform:scale(1.1)) — first disabled
         just on add_intersection_model/add_mixture_model (distracting
         on their wider, text-bearing buttons), then liked enough to
         want everywhere: pin the hover transform back to identity on
         every bttn-jelly button in the app, keeping the rest of the
         jelly style (color, press-down feedback) untouched.
         Same class + pseudo-class as bttn.css's own rule, so on an
         !important tie the browser falls back to source order — worked
         in testing, but reportedly still lost in Safari (a stylesheet
         load-order/insertion-timing difference is the likely cause,
         since shinyWidgets' CSS is attached as a runtime dependency
         rather than sitting statically before this block). The class
         written twice (.bttn-jelly.bttn-jelly) is a standard specificity
         bump — same selector, but literally higher specificity, so this
         wins on that alone regardless of where either stylesheet lands
         in the document, sidestepping the ordering question entirely. */
      .bttn-jelly.bttn-jelly:hover, .bttn-jelly.bttn-jelly:focus {
        transform: none !important;
        -webkit-transform: none !important;
        /* With growth gone, what bttn.css itself still does on hover
           (a subtle box-shadow + a same-color ::before layer fading in
           at 15% opacity) turned out too faint to read as 'something
           happened' on its own — reported as feeling completely static.
           A brightness darken was tried next and disliked too — a
           colored ring around the button instead, same convention
           already used elsewhere in the app for a focused/active state
           (see e.g. the textarea focus ring), rather than tinting the
           button itself. */
        box-shadow: 0 0 0 3px color-mix(in srgb, var(--fairy-primary, #3a6df0) 30%, transparent) !important;
      }
      /* Pure-CSS hover tooltip — no JS init needed, so it works
         immediately for these dynamically-generated per-model buttons
         (a Bootstrap-JS tooltip would need re-initializing every time the
         model list re-renders). */
      .fairy-tooltip { position: relative; }
      .fairy-tooltip::after {
        content: attr(data-tooltip);
        position: absolute;
        bottom: 125%;
        /* Centered on the button rather than left-anchored: these icon
           buttons sit in a narrow ~46px column near the left edge of the
           panel, so a left:0 tooltip shoots almost entirely to the right
           and can run past the row's own right edge (or a neighboring
           box drawn on top of it), reading as clipped/overlapping text.
           Centering keeps the overflow split evenly on both sides. */
        left: 50%;
        transform: translateX(-50%);
        background: #1f2430;
        color: #fff;
        font-size: 11px;
        line-height: 1.3;
        padding: 4px 8px;
        border-radius: 5px;
        white-space: normal;
        width: max-content;
        max-width: 160px;
        text-align: center;
        opacity: 0;
        pointer-events: none;
        transition: opacity 0.1s ease-in-out;
        z-index: 99999;
      }
      .fairy-tooltip:hover::after { opacity: 1; }
      /* Same tooltip, also on keyboard focus (Tab to the button, not
         just mouse hover) — :focus-within so it still works when the
         actual focusable element is a child of the .fairy-tooltip span
         rather than the span itself, matching how these are built
         throughout the app. */
      .fairy-tooltip:focus-within::after { opacity: 1; }
      /* Standard visually-hidden pattern — content present for screen
         readers/other assistive tech, not shown visually. Used for
         model rows 2+'s own Model Name/Unique Model Specification
         labels: row 1 gets a real visible label (see textboxes_relations
         and textboxes_relations_name), and rows below it rely on the
         column header above being visually obvious — but that visual
         context doesn't exist for a screen reader moving row by row, so
         every row still needs SOME label, just not a visibly repeated
         one. */
      .sr-only {
        position: absolute;
        width: 1px; height: 1px;
        padding: 0; margin: -1px;
        overflow: hidden;
        clip: rect(0, 0, 0, 0);
        white-space: nowrap;
        border: 0;
      }
      /* General (not #sidebar-fab-panel-scoped) downward variant — the
         default upward-opening tooltip on an element sitting near the
         very top of the page (e.g. the exact-probability-count box,
         right under the fixed navbar) has nowhere to open upward INTO,
         so it got clipped/overlapped by the navbar instead. Same fix,
         same class name, as the existing #sidebar-fab-panel-scoped one
         below — just not restricted to that one panel. */
      .fairy-tooltip-down::after {
        bottom: auto;
        top: 125%;
      }
      /* The H-rep/V-rep icon columns sit flush against the browser's own
         left edge (not just a panel's left edge like the vrep-toggle
         buttons above), so even a centered tooltip still runs off-screen
         on its left half. Anchor those specifically to the button's own
         left edge instead, so the whole bubble opens to the right. */
      .fairy-input-toolbar .fairy-tooltip::after {
        left: 0;
        transform: none;
      }
      /* Same overflow problem, same fix: the per-model delete/V-rep/
         edit icon column sits flush against the model list's own left
         edge, so a centered tooltip there runs off past the panel's own
         left boundary and reads as clipped. */
      .fairy-model-icon-col .fairy-tooltip::after {
        left: 0;
        transform: none;
      }
      /* Same overflow problem, opposite edge: the floating panel sits
         flush against the browser's right edge (right:24px), so a
         centered tooltip on anything near the panel's own right side
         (like the stale badge) runs off-screen to the right instead.
         Anchor to the button's own right edge so the bubble opens
         leftward, matching the toolbar fix above but mirrored. */
      #sidebar-fab-panel .fairy-tooltip::after {
        left: auto;
        right: 0;
        transform: none;
      }
      /* Parsimony's stacked column layout (3 switches + a
         settings/download row + Go, all vertically stacked — see
         data-fab-layout='column') has no direction that's reliably
         clear for an upward/downward tooltip: whatever a bubble opens
         toward, SOMETHING else in the stack is right there (a switch
         opening down hits the row below it; the settings/download row
         or Go opening up hits a switch above). The one direction that's
         always open regardless of position in the stack is sideways —
         the panel sits flush against the browser's own right edge with
         open page to its left. Every tooltip trigger in the column
         layout carries this class instead of relying on the generic
         above/below rules.
         Scoped with the #sidebar-fab-panel prefix (matching the generic
         rule above's specificity) — a bare .fairy-tooltip-left::after
         was silently LOSING to the id-scoped generic rule above despite
         coming later in the file, since specificity (not source order)
         decides when both match the same element (every one of these
         triggers also carries the plain .fairy-tooltip class). This was
         the actual reason the sideways-tooltip fix didn't visibly do
         anything — the generic rule's implicit bottom:125% (inherited
         from .fairy-tooltip::after's own base rule) kept winning. */
      #sidebar-fab-panel .fairy-tooltip-left::after {
        bottom: auto;
        top: 50%;
        left: auto;
        right: 100%;
        transform: translateY(-50%);
        margin-right: 8px;
      }
      /* Input's Go: the icon row sits to its immediate LEFT at the SAME
         height (not above it — see .fairy-primary-controls' own
         position:absolute rule), so the generic upward tooltip (barely
         clearing Go's own top edge) still landed across the icon row
         and the handle bar above it, both close by horizontally. Go is
         also the bottom-most thing in the panel, so opening its
         tooltip DOWNWARD instead — into the empty page below the card
         — is guaranteed clear of everything else in the panel, same
         reasoning as .fairy-tooltip-left above just a different safe
         direction for this particular layout. */
      #sidebar-fab-panel .fairy-tooltip-down::after {
        bottom: auto;
        top: 125%;
      }
      .fairy-h-cards-wrap {
        display: flex;
        flex-wrap: wrap;
        justify-content: flex-start;
        align-content: flex-start;
        /* Default flex align-items is stretch — every card in the same
           row was forced to match the TALLEST card's height regardless
           of its own content (a 1-row model card stretched to fit
           beside an 18-row one). flex-start lets each card size to its
           own content's natural height instead. */
        align-items: flex-start;
        gap: 16px;
      }
      /* .model-card's own margin:0 auto 16px (for when it's centered
         standalone elsewhere) fights justify-content here — flexbox
         gives a flex item's own auto side-margins priority over the
         container's justify-content, so they were re-centering each
         card despite flex-start above. Kill just the auto part inside
         this wrapper; gap (above) already provides the spacing. */
      .fairy-h-cards-wrap .model-card { margin-left: 0; margin-right: 0; }
      @keyframes fairyEqAppear {
        0%, 100% { transform: scale(1); box-shadow: 0 0 0 0 rgba(240, 173, 78, 0.7); }
        50% { transform: scale(1.35); box-shadow: 0 0 0 7px rgba(240, 173, 78, 0); }
      }
      /* Multiple-items modal: Group/Type/tolerance are actually disabled
         (see the row's own JS above) while Items is still 1, not just
         styled — this just dims THOSE fields so that's visible at a
         glance. Not the whole row: the probability name and the Items
         input itself stay fully visible/interactive regardless. */
      .fairy-items-row-inactive > :nth-child(2),
      .fairy-items-row-inactive > :nth-child(4),
      .fairy-items-row-inactive > :nth-child(5) {
        opacity: 0.4;
      }
      /* Approximate-equalities dialog — a compact card matching the rest
         of the app's look instead of a sparse default Bootstrap modal
         (huge title, oversized italic helptext, lots of empty space). */
      .fairy-eq-tol-modal .modal-content {
        border-radius: 12px;
        padding: 4px 6px;
      }
      .fairy-eq-tol-header {
        font-size: 16px;
        color: var(--fairy-text);
        margin-bottom: 4px;
      }
      .fairy-eq-tol-help {
        font-size: 12.5px;
        color: var(--fairy-text-muted);
        margin-bottom: 14px;
        line-height: 1.5;
      }
      .fairy-eq-tol-row {
        padding: 12px 14px;
        background-color: var(--ct-default);
        border: 1px solid var(--fairy-border);
        border-radius: 10px;
        margin-bottom: 10px;
      }
      .fairy-eq-tol-row .form-group { margin-bottom: 0; }
      .fairy-eq-tol-clause {
        display: inline-block;
        background: var(--fairy-panel);
        border: 1px solid var(--fairy-border);
        border-radius: 6px;
        color: var(--fairy-text);
        font-size: 13px;
        padding: 3px 8px;
        margin-bottom: 10px;
      }
      /* Add intersection model / Add mixture model dialogs — same
         compact-card treatment as .fairy-eq-tol-modal, plus styling the
         plain checkboxGroupInput() list as clickable rows instead of the
         default bare checkbox-and-label stack. */
      .fairy-combo-modal .modal-content {
        border-radius: 12px;
        padding: 4px 6px;
      }
      .fairy-combo-header {
        font-size: 16px;
        color: var(--fairy-text);
        margin-bottom: 4px;
      }
      .fairy-combo-help {
        font-size: 12.5px;
        color: var(--fairy-text-muted);
        margin-bottom: 14px;
        line-height: 1.5;
      }
      .fairy-combo-modal .shiny-options-group { display: flex; flex-direction: column; gap: 6px; }
      .fairy-combo-modal .checkbox {
        margin: 0;
        background-color: var(--ct-default);
        border: 1px solid var(--fairy-border);
        border-radius: 8px;
        transition: border-color 0.12s, background-color 0.12s, box-shadow 0.12s;
        cursor: pointer;
      }
      .fairy-combo-modal .checkbox:hover { border-color: var(--fairy-primary); }
      .fairy-combo-modal .checkbox:has(input:checked) {
        background-color: var(--bg-accent, #e6f1fb);
        border-color: var(--fairy-primary);
        box-shadow: 0 0 0 1px var(--fairy-primary);
      }
      .fairy-combo-modal .checkbox label {
        display: flex !important;
        align-items: center;
        gap: 10px;
        font-size: 13.5px;
        color: var(--fairy-text);
        font-weight: 400;
        cursor: pointer;
        margin: 0;
        width: 100%;
        padding: 10px 14px !important;
        min-height: 0 !important;
        box-sizing: border-box;
      }
      .fairy-combo-modal .checkbox input[type='checkbox'] {
        position: static !important;
        width: 16px;
        height: 16px;
        margin: 0 !important;
        flex-shrink: 0;
        accent-color: var(--fairy-primary);
        cursor: pointer;
      }
      .fairy-combo-modal .checkbox label span {
        flex: 1;
        min-width: 0;
      }
      .fairy-eq-tol-switch-line {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 10px;
      }
      .fairy-eq-tol-switch-line label {
        font-size: 13.5px;
        color: var(--fairy-text-muted);
        font-weight: 400;
        margin: 0;
      }
      .fairy-eq-tol-row .control-label {
        font-size: 12.5px;
        color: var(--fairy-text-muted);
        font-weight: 400;
        display: block;
        margin-top: 8px;
        margin-bottom: 6px;
        line-height: 1.4;
      }
      .fairy-model-spec-outer {
        display: block;
        width: 100%;
        height: 130px;
        box-sizing: border-box;
        overflow: hidden;
        border-radius: 4px;
      }
      pre[id^='model_spec_display_'] {
        display: block;
        width: 100%;
        height: 100%;
        box-sizing: border-box;
        background-color: #f5f5f5;
        cursor: default;
        text-align: left;
        white-space: pre-wrap;
        word-wrap: break-word;
        overflow: auto;
        border: 1px solid #ccc;
        border-radius: 4px;
        padding: 6px 12px;
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
        font-size: inherit;
        color: inherit;
        margin: 0;
      }
      /* Hides the scrollbar's own visual widget (still scrollable via
         wheel/trackpad/keyboard) rather than trying to give Safari's
         classic scrollbar room to render — this is what actually stops
         Safari growing the box for scrollbar space in the first place,
         since with no visible scrollbar there's nothing for it to reserve
         room for, regardless of the always-show-scrollbars system
         setting. */
      pre[id^='model_spec_display_']::-webkit-scrollbar { display: none; width: 0; height: 0; }
      pre[id^='model_spec_display_'] { scrollbar-width: none; -ms-overflow-style: none; }
      /* A typed clause the H-representation shows is already implied by
         the model's other constraints (see model_redundant_clause_flags)
         — red like the rest of the app's warning language, underlined
         rather than just colored so it still reads clearly against the
         red-on-white contrast, and a tooltip explaining why since the
         color alone doesn't say what 'redundant' means here. */
      .fairy-redundant-clause {
        color: #c0392b;
        text-decoration: underline;
        text-decoration-style: dashed;
        text-decoration-thickness: 1.5px;
        text-underline-offset: 2px;
        cursor: help;
      }
      /* Shared fixed-position tooltip for .fairy-redundant-clause (see
         its JS: shown/positioned on hover via getBoundingClientRect,
         not CSS ::after — those spans sit inside an overflow-clipped
         ancestor that would hide a ::after bubble regardless of
         direction). Same visual style as .fairy-tooltip::after
         elsewhere in the app, just positioned differently. */
      #fairy-clause-tooltip {
        position: fixed;
        transform: translate(-50%, -100%);
        background: #1f2430;
        color: #fff;
        font-size: 11px;
        line-height: 1.3;
        padding: 4px 8px;
        border-radius: 5px;
        white-space: normal;
        width: max-content;
        max-width: 220px;
        text-align: center;
        opacity: 0;
        pointer-events: none;
        transition: opacity 0.1s ease-in-out;
        z-index: 99999;
      }
      #fairy-clause-tooltip.active { opacity: 1; }
      /* Expand button: reparents the real textarea (inside its
         .fairy-ineq-wrap) into this overlay and back — one input, one
         Shiny binding, throughout. */
      .fairy-expand-backdrop {
        display: none;
        position: fixed; inset: 0;
        background: rgba(0,0,0,0.4);
        z-index: 9998;
      }
      .fairy-expand-backdrop.fairy-expand-on { display: flex; align-items: center; justify-content: center; }
      .fairy-expand-box {
        background: #fff;
        border-radius: 8px;
        padding: 20px;
        width: 800px;
        max-width: 90vw;
        box-shadow: 0 12px 40px rgba(0,0,0,0.35);
      }
      body.dark-mode .fairy-expand-box { background: #2a2a2a; }
      .fairy-expand-box .fairy-ineq-wrap textarea { height: 50vh !important; }
      .fairy-expand-names {
        display: flex;
        flex-wrap: wrap;
        gap: 6px 10px;
        margin-bottom: 12px;
        padding-bottom: 10px;
        border-bottom: 1px solid #ddd;
      }
      body.dark-mode .fairy-expand-names { border-bottom-color: #444; }
      .fairy-expand-name-badge {
        display: inline-block;
        padding: 2px 8px;
        border-radius: 12px;
        background: #f0f0f0;
        color: #333;
        font-size: 12px;
        white-space: nowrap;
      }
      body.dark-mode .fairy-expand-name-badge { background: #3a3a3a; color: #ddd; }
      "
    )),
    tags$script(HTML(
      "
      (function() {
        // This exact overlay approach is the one confirmed working — two
        // later attempts to add click-to-expand on top of it both broke
        // display for reasons never pinned down. Do not add expand-related
        // code back into this block; if that feature is revisited, build
        // it as a fully separate button/element instead of hooking into
        // this textarea's own events.
        function subst(s) { return s.replace(/>/g, '\\u2265').replace(/</g, '\\u2264'); }
        // Same idea as subst(), but for a specific Unique Model
        // Specification textarea (textin_relations_<i>): its own equals
        // signs become the approx symbol when THAT clause has been
        // marked approximate for this model — read from the hidden
        // eq_approx_flags_unique_<i> output (see its own R-side comment).
        // Every other wired box (add_for_all_models, which has no single
        // model to look up) keeps the plain >/< -only substitution.
        function substForBox(ta, s) {
          var m = ta.id.match(/^textin_relations_(\\d+)$/);
          if (!m) return subst(s);
          var flagsEl = document.getElementById('eq_approx_flags_unique_' + m[1]);
          var flags = flagsEl && flagsEl.textContent ? flagsEl.textContent.split(',') : [];
          var eqIdx = 0;
          var withApprox = s.replace(/=/g, function() {
            var isApprox = flags[eqIdx] === '1';
            eqIdx++;
            return isApprox ? '\\u2248' : '=';
          });
          return subst(withApprox);
        }
        var wiredBoxes = [];

        function wireBox(ta) {
          if (!ta || ta.dataset.fairyIneqWired) return;
          ta.dataset.fairyIneqWired = '1';

          // Deliberately NOT substituting the placeholder text — it is
          // telling the user which characters to actually type, not
          // showing what gets displayed afterward, so it stays plain.

          var wrap = document.createElement('div');
          wrap.className = 'fairy-ineq-wrap';
          ta.parentNode.insertBefore(wrap, ta);
          wrap.appendChild(ta);

          var overlay = document.createElement('div');
          overlay.className = 'fairy-ineq-overlay';
          wrap.appendChild(overlay);

          function matchStyle() {
            var cs = window.getComputedStyle(ta);
            ['font', 'fontSize', 'fontFamily', 'fontWeight', 'lineHeight',
             'padding', 'paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft',
             'border', 'borderWidth', 'borderStyle', 'letterSpacing'].forEach(function(p) {
              overlay.style[p] = cs[p];
            });
            overlay.style.borderColor = 'transparent';
          }

          function sync() {
            overlay.textContent = substForBox(ta, ta.value);
            overlay.scrollTop = ta.scrollTop;
            overlay.scrollLeft = ta.scrollLeft;
          }

          matchStyle();
          sync();
          ta.addEventListener('input', sync);
          ta.addEventListener('scroll', sync);
          wiredBoxes.push({ ta: ta, sync: sync });
        }

        function scan(root) {
          (root || document).querySelectorAll(
            'textarea[id^=\"textin_relations_\"]:not([id^=\"textin_relations_name\"]):not([id^=\"textin_relations_complete_\"]), textarea#add_for_all_models'
          ).forEach(wireBox);
        }

        $(document).on('shiny:connected shiny:value shiny:visualchange', function() { scan(); });

        // document.body can be null at the exact moment this script runs
        // (observed as a real must-be-an-instance-of-Node error, which
        // — since it throws — silently aborted every line after it below,
        // including scan() and the setInterval: the actual root cause of
        // every previous nothing-shows-up report). Deferring the
        // MutationObserver + first scan until body definitely exists.
        function startWiring() {
          new MutationObserver(function(muts) {
            muts.forEach(function(m) {
              m.addedNodes && m.addedNodes.forEach(function(n) {
                if (n.nodeType === 1) scan(n);
              });
            });
          }).observe(document.body, { childList: true, subtree: true });
          scan();

          setInterval(function() {
            wiredBoxes.forEach(function(w) { w.sync(); });
          }, 200);
        }
        if (document.body) {
          startWiring();
        } else {
          document.addEventListener('DOMContentLoaded', startWiring);
        }
      })();
      "
    )),
    # Fully separate from the overlay script above — does not touch any
    # textarea's own events, only clicks on the expand buttons and the
    # backdrop. Reparents the real wrap element into the expanded box and
    # back, so there is exactly one input/one Shiny binding at all times.
    tags$script(HTML(
      "
      (function() {
        function startExpand() {
          var backdrop = document.createElement('div');
          backdrop.className = 'fairy-expand-backdrop';
          var box = document.createElement('div');
          box.className = 'fairy-expand-box';
          backdrop.appendChild(box);
          document.body.appendChild(backdrop);

          var homeParent = null, homeNext = null, homeWrap = null;
          var namesStrip = null;

          // Shows the current probability names (p1, p2, ...) above the
          // expanded box, since those are what the constraint text refers
          // to and they're otherwise scrolled out of view once expanded.
          // Read-only snapshot built from the real name textareas' current
          // values, not live-bound elements — refreshed each time on open
          // and via input events while the box is open, but never
          // reparented/duplicated (would collide with Shiny's own ids).
          function buildNamesStrip() {
            var strip = document.createElement('div');
            strip.className = 'fairy-expand-names';
            document.querySelectorAll('textarea[id^=\"textin_name_\"]').forEach(function(ta) {
              var m = ta.id.match(/textin_name_(\\d+)$/);
              var i = m ? m[1] : '';
              var badge = document.createElement('span');
              badge.className = 'fairy-expand-name-badge';
              badge.textContent = 'p' + i + ': ' + (ta.value || ('p_{' + i + '}'));
              strip.appendChild(badge);
            });
            return strip;
          }

          function refreshNamesStrip() {
            if (!namesStrip || !namesStrip.parentNode) return;
            var fresh = buildNamesStrip();
            namesStrip.parentNode.replaceChild(fresh, namesStrip);
            namesStrip = fresh;
          }

          function closeExpand() {
            if (!homeWrap) return;
            homeParent.insertBefore(homeWrap, homeNext);
            backdrop.classList.remove('fairy-expand-on');
            homeWrap = homeParent = homeNext = null;
            if (namesStrip) { namesStrip.remove(); namesStrip = null; }
          }
          backdrop.addEventListener('click', function(e) {
            if (e.target === backdrop) closeExpand();
          });
          document.addEventListener('keydown', function(e) {
            if (e.key === 'Escape' && homeWrap) closeExpand();
          });
          $(document).on('input', 'textarea[id^=\"textin_name_\"]', refreshNamesStrip);

          $(document).on('click', '.fairy-expand-btn', function(e) {
            e.preventDefault();
            var targetId = $(this).data('target');
            var ta = document.getElementById(targetId);
            if (!ta) return;
            var wrap = ta.closest('.fairy-ineq-wrap') || ta.parentNode;
            if (!wrap) return;
            closeExpand();
            homeParent = wrap.parentNode;
            homeNext = wrap.nextSibling;
            homeWrap = wrap;
            namesStrip = buildNamesStrip();
            box.appendChild(namesStrip);
            box.appendChild(wrap);
            backdrop.classList.add('fairy-expand-on');
            ta.focus();
          });
        }
        if (document.body) {
          startExpand();
        } else {
          document.addEventListener('DOMContentLoaded', startExpand);
        }
      })();
      "
    )),
    # Per-model delete button: just forwards which index was clicked to
    # the server as an event-priority input; all the actual shifting of
    # data happens server-side (observeEvent(input$delete_model_idx, ...)).
    tags$script(HTML(
      "
      (function() {
        $(document).on('click', '.fairy-del-model-btn', function(e) {
          e.preventDefault();
          var idx = parseInt($(this).data('model-idx'), 10);
          if (isNaN(idx)) return;
          if (!window.confirm('Delete this model? This cannot be undone.')) return;
          Shiny.setInputValue('delete_model_idx', idx, { priority: 'event' });
        });
        // Delete all models — same confirm-then-forward pattern as the
        // per-model button above, just no index to parse (the server
        // side, observeEvent(input$delete_all_models_btn, ...), always
        // means every current row).
        $(document).on('click', '#delete_all_models_btn', function(e) {
          e.preventDefault();
          if (!window.confirm('Delete ALL models? This cannot be undone.')) return;
          // priority:'event' re-triggers the server-side observer on
          // every click regardless of value, so there's no need for a
          // changing value the way an actionButton's own click counter
          // provides automatically.
          Shiny.setInputValue('delete_all_models_btn', true, { priority: 'event' });
        });
      })();
      "
    )),
    # Row hover-sync: a model's Name / Unique Model Specification / Model
    # Specification boxes live in three separately-rendered column lists
    # (see the R comment on .fairy-model-row), so highlighting "the row"
    # on hover means finding every box sharing the same data-row-idx and
    # toggling the highlight class on all of them together, not just the
    # one actually under the cursor. Delegated, so it keeps working as
    # rows are added/removed/re-rendered without re-wiring.
    tags$script(HTML(
      "
      (function() {
        $(document).on('mouseenter', '.fairy-model-row', function() {
          var idx = $(this).data('row-idx');
          $('.fairy-model-row[data-row-idx=\"' + idx + '\"]').addClass('fairy-row-hover');
        });
        $(document).on('mouseleave', '.fairy-model-row', function() {
          var idx = $(this).data('row-idx');
          $('.fairy-model-row[data-row-idx=\"' + idx + '\"]').removeClass('fairy-row-hover');
        });
      })();
      "
    )),
    # Blink the "≈" button every time it transitions from hidden to
    # visible (a new "=" appeared — including deleting one and typing it
    # again later), but not on every poll tick while it's already
    # visible (conditionalPanel re-evaluates its condition on every
    # keystroke in the model's spec, not just when the result actually
    # changes). Visibility is tracked per button id in a plain JS object
    # (window.fairyEqBtnVisible) rather than on the DOM node itself, since
    # that node gets destroyed and recreated whenever an unrelated model
    # is added/removed and the whole list re-renders. Polling rather than
    # a MutationObserver/visibility-change hook because conditionalPanel
    # toggles visibility via a plain style change with no event of its
    # own to listen for.
    tags$script(HTML(
      "
      (function() {
        window.fairyEqBtnVisible = window.fairyEqBtnVisible || {};
        // conditionalPanel's own wrapper is the element that carries an
        // explicit inline display style — walk up to it specifically and
        // read THAT, rather than offsetParent (which also comes back
        // null whenever an ancestor tab is simply not the active one,
        // not just when this button's own condition is false — switching
        // tabs and back was re-triggering the blink for exactly that
        // reason).
        function conditionSaysVisible(btn) {
          var el = btn;
          while (el && el !== document.body) {
            if (el.style && el.style.display) {
              return el.style.display !== 'none';
            }
            el = el.parentElement;
          }
          return true;
        }
        function scanEqBtnFlash() {
          // Scoped to the actual eq-tolerance button by id prefix, NOT
          // the shared .fairy-eq-tol-btn class — that class is reused
          // purely for visual styling by other icon-column buttons too
          // (items_edit_btn_/items_preview_btn_/copy_model_btn_), none of
          // which should flash on every appear/re-render; only the real
          // \"≈\" button wants the appear animation.
          document.querySelectorAll('[id^=\"eq_tol_btn_\"]').forEach(function(btn) {
            if (!btn.id) return;
            var isVisible = conditionSaysVisible(btn);
            var wasVisible = !!window.fairyEqBtnVisible[btn.id];
            if (isVisible && !wasVisible) {
              btn.classList.remove('fairy-eq-tol-btn-flash');
              void btn.offsetWidth;
              btn.classList.add('fairy-eq-tol-btn-flash');
              // Strip the class once the animation actually finishes —
              // otherwise it lingers on the element, and switching away
              // from this tab and back (a real display:none -> block
              // cycle on the ancestor tab-pane, nothing to do with this
              // button's own conditionalPanel) replays any animation
              // class still present, independent of the visibility
              // bookkeeping above.
              btn.addEventListener('animationend', function handler() {
                btn.classList.remove('fairy-eq-tol-btn-flash');
                btn.removeEventListener('animationend', handler);
              });
            }
            window.fairyEqBtnVisible[btn.id] = isVisible;
          });
        }
        setInterval(scanEqBtnFlash, 300);
      })();
      "
    )),
    # Hides (or, via msg.included = true, re-shows) the matching model-
    # card in the V-representation tab the moment its per-model V toggle
    # is switched — see the server-side sendCustomMessage in both
    # observeEvent(input$vrep_toggle_btn_i, ...) (single model) and
    # observeEvent(input$vrep_toggle_all_btn, ...) (all models at once,
    # msg.model omitted to mean "every card") — a client-only visibility
    # change, since the table itself is a static snapshot rebuilt only on
    # the next full Go/Parsimony run.
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('fairy_hide_vrep_card', function(msg) {
        var disp = msg.included ? '' : 'none';
        document.querySelectorAll('#v_rep .model-card-title').forEach(function(title) {
          if (!msg.model || title.textContent === msg.model) {
            title.closest('.model-card').style.display = disp;
          }
        });
      });
      "
    )),
    # Sets a textarea's placeholder attribute directly — used to keep a
    # mixture model's "Mixture of ..." description live-synced with its
    # sources' current names (see the server-side sendCustomMessage in
    # the wired_derived_ids observer) without needing a full re-render.
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('fairy_set_placeholder', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) el.placeholder = msg.placeholder;
      });
      "
    )),
    # Makes the whole styled row in the "Add intersection/mixture model"
    # dialogs clickable, not just the small checkbox square itself — the
    # label wraps both, so clicking it (or the text inside) already
    # toggles the checkbox via native browser behavior. What ISN'T
    # covered is the row's own padding, added by CSS around that label
    # (see .fairy-combo-modal .checkbox) to make it a nicely-sized button
    # rather than a bare checkbox — a click landing on that padding hits
    # the plain wrapping div, not the label, so nothing happens by
    # default. Only handling clicks where e.target IS that div itself
    # (not any descendant) is what avoids double-toggling: a click that
    # already landed on the label/span/input got handled natively and
    # must be left alone here. Delegated since dialog content is inserted
    # fresh each time, not present at page load.
    tags$script(HTML(
      "
      $(document).on('click', '.fairy-combo-modal .checkbox', function(e) {
        if (e.target !== this) return;
        var cb = this.querySelector('input[type=\"checkbox\"]');
        if (!cb) return;
        cb.checked = !cb.checked;
        $(cb).trigger('change');
      });
      "
    )),
    # Compact-mode toggle: purely a body class flip, no server round-trip
    # needed since it's cosmetic-only (see the CSS rules gated on
    # body.fairy-compact-models).
    tags$script(HTML(
      "
      (function() {
        $(document).on('click', '#compact-toggle', function() {
          var on = document.body.classList.toggle('fairy-compact-models');
          $(this).toggleClass('active', on);
        });
      })();
      // Stale results are indicated by fairy-stale-blink's own CSS glow
      // pulse alone now (a sparkle burst here was tried and found too
      // distracting) — nothing left to wire up client-side for it.
      "
    )),
    # Lets the floating fab panel (Go / Compute parsimony / algorithm
    # controls, shown while the sidebar is collapsed) be dragged anywhere
    # on screen by its grip handle. Switches the panel from its default
    # right/bottom-anchored position to an explicit left/top the first
    # time it's dragged, so it doesn't jump on the first move.
    tags$script(HTML(
      "
      (function() {
        // Delegated on document (not attached directly to the handle)
        // since this script runs before #sidebar-fab-panel exists in the
        // DOM (it's defined further down the page) — a direct
        // getElementById()/addEventListener() here would silently find
        // nothing and wire up no drag behavior at all.
        // Tracked (and applied to the panel) as right/bottom, NOT
        // left/top — the panel's default (undragged) position is
        // right:24px/bottom:24px, and Go/the icon row are pinned via
        // right:10px/bottom:8px FROM THE PANEL'S OWN EDGES (see their
        // own CSS), which only stays visually stable across the
        // collapsed<->hovered width change (icons revealing) when the
        // panel's RIGHT edge is the fixed one — width then grows
        // leftward, same as the default state. Dragging used to switch
        // the panel to left/top anchoring instead, which is exactly
        // backwards for this: with the LEFT edge fixed, growing width
        // pushes the RIGHT edge (and Go along with it, right:10px from
        // that edge) rightward on every hover, after any drag. Keeping
        // right/bottom as the anchor in both states — just with a
        // different value after a drop — removes that whole class of
        // bug instead of only this one symptom of it.
        var dragging = false, startX = 0, startY = 0, startRight = 0, startBottom = 0, panel = null;
        $(document).on('pointerdown', '.fairy-fab-handle', function(e) {
          panel = document.getElementById('sidebar-fab-panel');
          if (!panel) return;
          dragging = true;
          var rect = panel.getBoundingClientRect();
          startX = e.clientX; startY = e.clientY;
          startRight = window.innerWidth - rect.right;
          startBottom = window.innerHeight - rect.bottom;
          panel.style.right = startRight + 'px';
          panel.style.bottom = startBottom + 'px';
          panel.style.left = 'auto';
          panel.style.top = 'auto';
          e.preventDefault();
        });
        $(document).on('pointermove', function(e) {
          if (!dragging || !panel) return;
          var dx = e.clientX - startX, dy = e.clientY - startY;
          var maxRight = window.innerWidth - panel.offsetWidth - 4;
          var maxBottom = window.innerHeight - panel.offsetHeight - 4;
          // Moving the pointer right/down means the panel's distance
          // from the right/bottom edge SHRINKS, hence the minus signs.
          panel.style.right = Math.min(Math.max(4, startRight - dx), maxRight) + 'px';
          panel.style.bottom = Math.min(Math.max(4, startBottom - dy), maxBottom) + 'px';
        });
        $(document).on('pointerup pointercancel', function() {
          if (dragging && panel) {
            // Remembered per active tab (see window.fairyFabPositions in
            // the sidebar-toggle script) so each tab's panel keeps its
            // own dropped position instead of sharing one across all of
            // them, even though they all reuse this same DOM node.
            var pane = document.querySelector('.tab-pane.active');
            var key = pane ? (pane.getAttribute('data-value') || pane.id || null) : null;
            if (key) {
              window.fairyFabPositions = window.fairyFabPositions || {};
              window.fairyFabPositions[key] = {
                right: parseFloat(panel.style.right) || 0,
                bottom: parseFloat(panel.style.bottom) || 0
              };
            }
          }
          dragging = false;
        });
      })();
      "
    )),
    # Sidebar collapse toggle: hides the left settings sidebar and lets the
    # model-definition area use the freed-up width. Targets the sidebar's
    # own outer bootstrap column (not just the .well inside it) so the
    # width is actually reclaimed, not just visually emptied; the main
    # content column right after it is grown to fill the gap.
    tags$script(HTML(
      "
      (function() {
        // One toggle state shared across every tab: navbarPage renders all
        // tab panels' sidebars into the DOM at once (Bootstrap just shows
        // one at a time), so this walks every '.fairy-sidebar' rather than
        // a single id, and keeps them all in sync regardless of which tab
        // is currently active when the button is clicked.
        var hiddenState = false;

        // Reparents the REAL primary-action button (and, if present, its
        // .fairy-primary-controls sibling — e.g. the algorithm checkboxes
        // on Model Properties) into the floating panel while the sidebar
        // is hidden, then puts them back exactly where they came from.
        // Moving the actual elements (not a proxy button showing static
        // 'Go' text, and not a clone, which would duplicate Shiny input
        // ids) means the label always matches the real button and any
        // controls it depends on stay genuinely usable, not just visible.
        // Same reparent-and-restore technique already used for the
        // expand-box overlay elsewhere on this page.
        var homed = null; // [{node, parent, next}, ...] to restore later

        function restoreFab() {
          var panel = document.getElementById('sidebar-fab-panel');
          if (homed) {
            homed.reverse().forEach(function(h) {
              h.parent.insertBefore(h.node, h.next);
            });
            homed = null;
          }
          // Not innerHTML='' here — homed's insertBefore calls above
          // already put everything reparented back where it came from,
          // and this panel keeps a permanent .fairy-fab-handle child
          // (the drag handle) that innerHTML='' would wipe out.
          if (panel) { panel.style.display = 'none'; }
        }

        function activePrimaryBtn() {
          var pane = document.querySelector('.tab-pane.active');
          if (!pane) return null;
          return pane.querySelector('.fairy-primary-action');
        }

        // Cmd/Ctrl+Enter recalculates (clicks whichever tab's primary Go /
        // Compute button is currently showing) — OR, when a modal dialog
        // is open (Repeated items, Intersection/Mixture model, Approximate
        // equalities, Algorithm settings, ...), clicks THAT dialog's own
        // Save/Add/Submit button instead. Every one of those buttons is
        // built with actionBttn(..., color = primary), which shinyWidgets
        // renders with a shared .bttn-primary class regardless of which
        // bttn style (jelly/material-flat/...) it otherwise uses — one
        // selector covers all of them without needing to list each
        // modal's own button id (several are per-row dynamic ids, e.g.
        // submit_eq_tol_<i>, anyway). Bootstrap 3's own class for a
        // currently-shown modal is .modal.in; .modal.show is included too
        // in case that ever changes to Bootstrap 4/5's naming. Cmd/Ctrl+R
        // was tried first for the Go/Compute case, but browsers reserve
        // that combo for page reload and never let page JS intercept it
        // (same as Ctrl+T/Ctrl+W) no matter what preventDefault() does —
        // Enter isn't reserved, and matches the run-cell convention
        // RStudio/Jupyter already use. Search the whole document rather
        // than via activePrimaryBtn()'s pane lookup for the Go/Compute
        // case — by the time this fires the button has usually already
        // been reparented into #sidebar-fab-panel (see updateFab()), so
        // it's no longer inside its original .tab-pane at all;
        // document-wide is fine since only one tab's primary action (and
        // at most one open modal) is ever present at a time.
        document.addEventListener('keydown', function(e) {
          if (e.key !== 'Enter') return;
          if (!(e.metaKey || e.ctrlKey)) return;
          var openModal = document.querySelector('.modal.in, .modal.show');
          var btn = openModal
            ? openModal.querySelector('.bttn-primary')
            : document.querySelector('.fairy-primary-action');
          if (!btn || btn.disabled || btn.classList.contains('disabled')) return;
          e.preventDefault();
          // A programmatic .click() fires the click event fine, but
          // never triggers :active — that's driven by real pointer-down
          // state, not the click event — so the button's own press
          // feedback silently doesn't play for the keyboard shortcut.
          // Fake it with a short manual scale animation instead (see
          // .fairy-kbd-press below).
          btn.classList.add('fairy-kbd-press');
          setTimeout(function() { btn.classList.remove('fairy-kbd-press'); }, 150);
          btn.click();
        });

        // Quickkeys for the four +/- counter buttons (Probabilities and
        // Model(s), on the Input tab) — requested directly, tooltip text
        // on each button already names its own combo (see the R markup).
        // Distinct modifier per pair (Alt for probabilities, Shift for
        // models) rather than one shared combo, so they can't be
        // confused for each other; Cmd/Ctrl+Enter above already owns
        // the plain no-extra-modifier case. Both '=' and '+' map to
        // add — which one event.key actually reports for the same
        // physical +/= key depends on whether Shift is ALSO down (it
        // types '+' on most layouts) and on the layout itself, and the
        // model shortcut holds Shift as its own modifier here, so
        // relying on '=' alone silently never matched for that one.
        var fairyCounterKeys = {
          '=':  { alt: 'add_prob',  shift: 'add_ie' },
          '+':  { alt: 'add_prob',  shift: 'add_ie' },
          '-':  { alt: 'rm_prob',   shift: 'rm_ie' }
        };
        document.addEventListener('keydown', function(e) {
          var pair = fairyCounterKeys[e.key];
          if (!pair) return;
          if (!(e.metaKey || e.ctrlKey)) return;
          var id = e.altKey && !e.shiftKey ? pair.alt : (e.shiftKey && !e.altKey ? pair.shift : null);
          if (!id) return;
          var btn = document.getElementById(id);
          if (!btn || btn.disabled || btn.classList.contains('disabled')) return;
          e.preventDefault();
          btn.classList.add('fairy-kbd-press');
          setTimeout(function() { btn.classList.remove('fairy-kbd-press'); }, 150);
          btn.click();
        });

        // Each tab remembers its own dragged panel position (see the
        // pointerup handler below) so moving the panel on, say, the
        // Model Properties tab doesn't also relocate it on the Input
        // tab — they share the same DOM node (reparented content in,
        // content out), but not a single shared position.
        window.fairyFabPositions = window.fairyFabPositions || {};
        function activeTabKey() {
          var pane = document.querySelector('.tab-pane.active');
          if (!pane) return null;
          return pane.getAttribute('data-value') || pane.id || null;
        }

        // Tabs with no sidebar at all (see the Input tabPanel's own
        // comment) have nothing for the sidebar-collapse toggle to
        // collapse, so hiddenState never becomes true just from being
        // on one of these — the floating panel is these tabs' ONLY
        // home for their primary action, so it must always show there,
        // not only once the (nonexistent) sidebar happens to be hidden.
        var FORCE_FLOAT_TABS = ['Input', 'Model Properties'];

        function updateFab() {
          var panel = document.getElementById('sidebar-fab-panel');
          if (!panel) return;
          restoreFab();
          var pane = document.querySelector('.tab-pane.active');
          var forceFloat = pane && FORCE_FLOAT_TABS.indexOf(pane.getAttribute('data-value') || pane.id) > -1;
          if (!hiddenState && !forceFloat) return;
          var btn = pane ? pane.querySelector('.fairy-primary-action') : null;
          if (!btn) return;
          homed = [];
          // Flow content (Parsimony's switches+Download, Input's
          // Download/Upload) — shown only on hover/focus (see the CSS),
          // reparented first so it renders ABOVE Go/Compute.
          var controls = pane.querySelector('.fairy-primary-controls');
          if (controls) {
            homed.push({ node: controls, parent: controls.parentNode, next: controls.nextSibling });
            panel.appendChild(controls);
          }
          // Go/Compute alone is always visible, absolutely positioned at
          // the panel's own fixed bottom-right corner (see
          // .fairy-primary-action's CSS) — deliberately taken out of the
          // panel's normal flex flow so its on-screen position never
          // depends on what .fairy-primary-controls is showing/hiding,
          // which is what caused it to visibly shift on hover before.
          homed.push({ node: btn, parent: btn.parentNode, next: btn.nextSibling });
          panel.appendChild(btn);
          panel.style.display = 'flex';

          var key = activeTabKey();
          var pos = key ? window.fairyFabPositions[key] : null;
          if (pos) {
            // right/bottom, matching the drag handlers above and the
            // default (undragged) anchor style — see their own comment
            // on why left/top here specifically re-broke Go's position
            // on every hover after a drag.
            panel.style.right = pos.right + 'px';
            panel.style.bottom = pos.bottom + 'px';
            panel.style.left = 'auto';
            panel.style.top = 'auto';
          } else {
            panel.style.left = 'auto';
            panel.style.top = 'auto';
            panel.style.right = '';
            panel.style.bottom = '';
          }
        }

        function applyState() {
          document.querySelectorAll('.fairy-sidebar').forEach(function(well) {
            var sidebarCol = well.closest('[class*=\"col-sm-\"]') || well.parentNode;
            if (!sidebarCol) return;
            var mainCol = sidebarCol.nextElementSibling;
            sidebarCol.classList.toggle('fairy-sidebar-col-hidden', hiddenState);
            if (mainCol) mainCol.classList.toggle('fairy-main-col-full', hiddenState);
          });
          var btn = document.getElementById('sidebar-toggle');
          if (btn) btn.classList.toggle('fairy-sidebar-hidden-state', hiddenState);
          updateFab();
        }
        function toggleSidebar() {
          hiddenState = !hiddenState;
          applyState();
        }
        $(document).on('click', '#sidebar-toggle', toggleSidebar);
        // Re-apply whenever a tab is switched to — the previous tab's
        // reparented controls (if any) need to go home first (they'd
        // otherwise sit, detached from their now-hidden tab-pane, inside
        // the floating panel forever) before the newly active tab's own
        // controls (if that tab even has a .fairy-primary-action) get
        // reparented in.
        $(document).on('shown.bs.tab', applyState);
        // The very first tab (Input, by default) is already active
        // before any 'shown.bs.tab' event ever fires for it — without
        // this, a FORCE_FLOAT_TABS tab landed on directly at page load
        // would leave its Go button stuck inside its display:none
        // source block (see .fairy-primary-controls-src) until the user
        // happened to switch tabs away and back.
        $(document).ready(applyState);
      })();
      "
    )),
    # Read-only "Model Specification" display: shows plain >/</= text sent
    # from R (see ineq_words()'s own comment for why the substitution to
    # the words >=/<= isn't done server-side), rewritten here client-side
    # instead. Simpler than the editable-box overlay technique since
    # there's no typing to preserve here — the pre's own text is just
    # replaced outright, polled the same way for the same reason
    # (Shiny's DOM updates to this element don't reliably fire events).
    # Each "=" additionally becomes "≈" instead of staying "=" when that
    # specific equality (by left-to-right occurrence) has been marked
    # approximate — see the hidden eq_approx_flags_i output next to each
    # pre, a comma-separated 1/0 list in the same order the popup and the
    # actual computation use. The TRUE raw text is read fresh every poll
    # from the hidden textin_relations_complete_i textarea (never mutated
    # by this script, unlike the pre itself) rather than from the pre's
    # own (possibly already-substituted) textContent — reusing already-
    # written ≈/≥/≤ characters as the input to the next pass would lose
    # the original "=" a toggled-off approximation needs to revert to,
    # since Shiny doesn't necessarily re-push a value that hasn't changed
    # as a string even though the reactive did re-run.
    tags$script(HTML(
      "
      (function() {
        function subst(s, flagsStr) {
          var flags = flagsStr ? flagsStr.split(',') : [];
          var eqIdx = 0;
          var withApprox = s.replace(/=/g, function() {
            var isApprox = flags[eqIdx] === '1';
            eqIdx++;
            return isApprox ? '\\u2248' : '=';
          });
          return withApprox.replace(/>/g, '\\u2265').replace(/</g, '\\u2264');
        }
        // Mirrors split_spec()'s R-side logic exactly (see its own
        // comment): commas OUTSIDE any '{...}' become clause separators
        // too (alongside ';'), commas INSIDE '{...}' stay put (they
        // belong to the '{p1,p2} < {p3}' batch shortcut) — brace depth
        // tracked the same way, char by char.
        function splitClauses(s) {
          var depth = 0, out = '';
          for (var i = 0; i < s.length; i++) {
            var c = s[i];
            if (c === '{') depth++;
            if (c === '}') depth--;
            out += (c === ',' && depth <= 0) ? ';' : c;
          }
          return out.split(';').map(function(p) { return p.trim(); }).filter(function(p) { return p.length > 0; });
        }
        // Unpacks ONE typed clause into the real row(s) it stands for —
        // both a CHAIN ('p1=p4=0' -> 'p1=p4', 'p4=0', two real rows for
        // one typed clause) and a '{p1,p2} < {p3,3*p4}' batch shortcut
        // (-> 'p1<p3','p1<3*p4','p2<p3','p2<3*p4'), and any combination
        // of the two. This is a straight port of the R-side
        // expand_clause_to_rows() (used for the exact same purpose —
        // aligning per-row data, there equality-tolerance indices, here
        // redundant-row flags — with what the Go pipeline actually
        // produces) — has to be reimplemented here in JS rather than
        // shared, since this path works from the hidden textarea's raw
        // text on a client-side poll, not a server render. A previous
        // version of this only handled the brace-batch case, silently
        // dropping ALL redundant-clause highlighting for any model with
        // a chained relation, since the row COUNT it produced no longer
        // matched R's per-row flag count (see the length check below).
        // Returns an array — [clause] itself (unchanged) when there's
        // no operator at all to expand, so callers can always just
        // concatenate the result without a branch.
        function expandBatchShortcut(clause) {
          var opRe = /[><=\\u2265\\u2264\\u2248]/g;
          var ops = clause.match(opRe);
          if (!ops) return [clause];
          var args = clause.split(opRe);
          if (args.length < 2) return [clause];
          function sideItems(side) {
            side = side.trim();
            if (side.charAt(0) === '{') {
              return side.replace(/[{} ]/g, '').split(',');
            }
            return [side];
          }
          var segs = [];
          for (var k = 0; k < args.length - 1; k++) {
            var b1 = sideItems(args[k]);
            var b2 = sideItems(args[k + 1]);
            b1.forEach(function(a) {
              b2.forEach(function(b) { segs.push(a + ops[k] + b); });
            });
          }
          return segs;
        }
        function escapeHtml(s) {
          return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
        }
        function startDisplaySubst() {
          setInterval(function() {
            document.querySelectorAll('pre[id^=\"model_spec_display_\"]').forEach(function(pre) {
              var m = pre.id.match(/model_spec_display_(\\d+)/);
              if (!m) return;
              var placeholderEl = document.getElementById('model_spec_is_placeholder_' + m[1]);
              if (placeholderEl && placeholderEl.textContent === '1') return;
              var hiddenTa = document.getElementById('textin_relations_complete_' + m[1]);
              var trueRaw = hiddenTa ? hiddenTa.value : pre.textContent;
              var flagsEl = document.getElementById('eq_approx_flags_' + m[1]);
              var flagsStr = flagsEl ? flagsEl.textContent : '';
              // Which actual H-rep ROWS turned out logically redundant
              // (see model_redundant_clause_flags's own R-side comment
              // for when this is deliberately left empty instead of
              // guessing wrong) — '1' per ROW, in the same left-to-right
              // order this script's own expansion below produces (a
              // shortcut clause contributes several rows here, not one;
              // see that R-side function's comment on why collapsing to
              // one flag per TYPED clause hid genuine partial-clause
              // redundancy).
              var redundantEl = document.getElementById('redundant_flags_' + m[1]);
              var redundantStr = redundantEl ? redundantEl.textContent : '';
              var key = trueRaw + '\\u0001' + flagsStr + '\\u0001' + redundantStr;
              if (pre.dataset.fairyKey === key) return;
              pre.dataset.fairyKey = key;
              var substituted = subst(trueRaw, flagsStr);
              var redundantFlags = redundantStr ? redundantStr.split(',') : null;
              var rawClauses = splitClauses(substituted);
              // Expand each raw (possibly '{...}') clause into its real
              // rows first, THEN zip 1:1 against redundantFlags — no
              // more per-clause index indirection, so there's nothing
              // left to misalign.
              var clauses = [];
              rawClauses.forEach(function(c) {
                expandBatchShortcut(c).forEach(function(e) { clauses.push(e); });
              });
              var flagsOut = clauses.map(function(_, idx) {
                return redundantFlags ? redundantFlags[idx] : null;
              });
              if (redundantFlags && clauses.length !== redundantFlags.length) {
                // Shouldn't happen (R and JS expand the same way from
                // the same string) — ignore the (misaligned) flags
                // rather than risk mis-highlighting; expansion (and the
                // plain, unhighlighted display) still applies.
                flagsOut = clauses.map(function() { return null; });
              }
              // Plain title attribute here, NOT the .fairy-tooltip::after
              // bubble used everywhere else in the app — this span lives
              // inside .fairy-model-spec-outer/the pre itself, both a
              // hard overflow:hidden/auto scroll boundary (see that div's
              // own comment), which clips an absolutely-positioned ::after
              // bubble to invisibility no matter which direction it opens.
              // #fairy-clause-tooltip (see its own comment near the top
              // of the UI) shows it instead, reading this data-tooltip
              // attribute on hover.
              pre.innerHTML = clauses.map(function(clause, idx) {
                var html = escapeHtml(clause);
                return flagsOut[idx] === '1'
                  ? '<span class=\"fairy-redundant-clause\" data-tooltip=\"Redundant: already implied by the other constraints\">' + html + '</span>'
                  : html;
              }).join('; ');
            });
          }, 200);
        }
        if (document.body) {
          startDisplaySubst();
        } else {
          document.addEventListener('DOMContentLoaded', startDisplaySubst);
        }
      })();
      "
    )),
    tags$div(
      id = "fairy-card-overlay",
      tags$div(
        id = "fairy-card-overlay-inner",
        tags$button(id = "fairy-card-overlay-close", title = "Close (Esc)", HTML("&times;")),
        tags$div(id = "fairy-card-overlay-content")
      )
    ),
    # Own-built, always-present-in-the-DOM progress overlay for "Computing
    # X" feedback during Go/Parsimony batches — replaces the previous
    # approach of repeatedly calling shinyalert() (or reaching into its
    # swal2 DOM after the fact), which visibly flashed the dialog closed
    # and reopened on every single model in a batch no matter how the
    # timing was tuned. This div is rendered ONCE, right here, and stays
    # in the page the whole session; the server only ever toggles its
    # 'active' class and rewrites the two text nodes below via
    # fairy_progress_open()/_update()/_close() (see their own comments),
    # so there is nothing to tear down and recreate between models.
    # Single shared tooltip for the redundant-clause spans in the Model
    # Specification display (see .fairy-redundant-clause) — those spans
    # sit inside a hard overflow:hidden/auto scroll boundary, which
    # clips the app's usual .fairy-tooltip::after bubble to invisibility
    # regardless of which direction it opens; a native title= attribute
    # was tried next and, per this app's own established experience
    # elsewhere (see the buttonLabel/tooltip history), wasn't reliable
    # either. This sidesteps both: position:fixed (immune to any
    # ancestor's overflow), positioned in JS from the hovered span's own
    # getBoundingClientRect() rather than CSS ::after positioning.
    tags$div(id = "fairy-clause-tooltip"),
    tags$script(HTML(
      "
      (function() {
        function initClauseTooltip() {
          var tip = document.getElementById('fairy-clause-tooltip');
          if (!tip) return;
          document.addEventListener('mouseover', function(e) {
            var el = e.target.closest && e.target.closest('.fairy-redundant-clause');
            if (!el) return;
            tip.textContent = el.getAttribute('data-tooltip') || el.getAttribute('title') || '';
            var r = el.getBoundingClientRect();
            tip.style.left = (r.left + r.width / 2) + 'px';
            tip.style.top = (r.top - 8) + 'px';
            tip.classList.add('active');
          });
          document.addEventListener('mouseout', function(e) {
            var el = e.target.closest && e.target.closest('.fairy-redundant-clause');
            if (!el) return;
            tip.classList.remove('active');
          });
        }
        if (document.body) {
          initClauseTooltip();
        } else {
          document.addEventListener('DOMContentLoaded', initClauseTooltip);
        }
      })();
      "
    )),
    tags$div(
      id = "fairy-progress-overlay",
      tags$div(
        id = "fairy-progress-overlay-card",
        tags$div(id = "fairy-progress-overlay-title"),
        tags$div(id = "fairy-progress-overlay-body")
      )
    ),
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('fairy_progress_open', function(msg) {
        var ov = document.getElementById('fairy-progress-overlay');
        var t  = document.getElementById('fairy-progress-overlay-title');
        var b  = document.getElementById('fairy-progress-overlay-body');
        if (!ov) return;
        t.innerHTML = msg.title || '';
        b.innerHTML = msg.text || '';
        ov.classList.add('active');
      });
      Shiny.addCustomMessageHandler('fairy_progress_update', function(msg) {
        var t = document.getElementById('fairy-progress-overlay-title');
        var b = document.getElementById('fairy-progress-overlay-body');
        if (msg.title !== null && t) t.innerHTML = msg.title;
        if (msg.text  !== null && b) b.innerHTML  = msg.text;
      });
      Shiny.addCustomMessageHandler('fairy_progress_close', function(msg) {
        var ov = document.getElementById('fairy-progress-overlay');
        if (ov) ov.classList.remove('active');
      });
      "
    )),
    tags$script(HTML(
      "(function() {
         function fitContentToWidth() {
           var inner   = document.getElementById('fairy-card-overlay-inner');
           var content = document.getElementById('fairy-card-overlay-content');
           content.style.transform       = 'none';
           content.style.transformOrigin = '';
           requestAnimationFrame(function() {
             var availW = inner.clientWidth - 96;
             var natW   = content.scrollWidth;
             if (natW < 1 || availW < 1) return;
             var scale = availW / natW;
             scale = Math.max(0.05, Math.min(scale, 4));
             if (scale < 0.99) {
               content.style.transformOrigin = 'top left';
               content.style.transform       = 'scale(' + scale + ')';
             }
           });
         }

         // A model with many item-expanded parameters gives the H-rep
         // table one column per parameter per side — mostly-blank
         // columns that can add up to far wider than the small card
         // (see .fairy-h-table-scroll's own comment). Rather than only
         // relying on horizontal scroll, shrink the TABLE itself
         // (transform:scale, same technique fitContentToWidth already
         // uses for the expand overlay) down to whatever fits the card,
         // with a floor so it never becomes illegibly tiny — past that
         // floor, scroll takes back over. Exposed on window so the
         // MathJax retry-typeset loop above (which is what actually
         // finishes changing column widths) can call it once typesetting
         // is done, not just once the raw HTML lands.
         window.fitHTablesToWidth = function() {
           document.querySelectorAll('.fairy-h-table-scroll').forEach(function(wrap) {
             // A wrap can hold FOUR tables now (Aligned/Compact x
             // plain/color — see h_layout_toggle_group /
             // h_color_toggle_group), only one visible at a time via CSS;
             // fit whichever one is actually shown, not just the first
             // in DOM order (which is always the Aligned one, even
             // while Compact is the one on screen).
             var tables = wrap.querySelectorAll('table');
             var table = null;
             tables.forEach(function(t) {
               if (getComputedStyle(t).display !== 'none') table = t;
             });
             if (!table) return;
             table.style.transform = 'none';
             table.style.transformOrigin = '';
             wrap.style.height = '';
             // Release any width this wrap (and so its .model-card
             // ancestor, which shrink-wraps to its content) was
             // explicitly given on a PREVIOUS call — before this, the
             // card only ever shrank to fit an oversized table via the
             // scale transform below; it never grew back, or shrank
             // back down, for a DIFFERENT table becoming visible (e.g.
             // switching the H-rep layout toggle from Aligned to
             // Compact left the card exactly as wide as Aligned needed,
             // since wrap.clientWidth here used to just read back
             // whatever width the card had already frozen at, rather
             // than an independent, always-current bound). Reading the
             // card's own PARENT (the cards row) instead of the wrap
             // itself breaks that circularity, so the card can now
             // genuinely grow or shrink to match whichever table is
             // currently shown.
             wrap.style.width = '';
             var card = wrap.closest('.model-card');
             var availW = (card && card.parentElement) ? card.parentElement.clientWidth : wrap.clientWidth;
             var natW = table.scrollWidth;
             if (natW < 1 || availW < 1) return;
             var targetW = Math.min(natW, availW);
             wrap.style.width = targetW + 'px';
             var scale = targetW / natW;
             scale = Math.max(0.5, Math.min(scale, 1));
             if (scale < 0.995) {
               table.style.transformOrigin = 'top left';
               table.style.transform = 'scale(' + scale + ')';
               // transform doesn't shrink the LAYOUT box, only the
               // painted pixels — without this the wrap (which scrolls
               // vertically on its own, see its max-height) would still
               // reserve the table's full, un-scaled height below it as
               // blank space.
               wrap.style.height = (table.scrollHeight * scale) + 'px';
             }
           });
         };

         function openOverlay(card) {
           var content = $(card).clone();
           content.find('.model-card-expand-hint').remove();
           // Undo any shrink-to-fit the small card's own table got (see
           // window.fitHTablesToWidth) — the overlay has far more room,
           // so it should start from the table's natural, full-size
           // rendering and let fitContentToWidth() below re-decide
           // whether the WHOLE card still needs scaling at that width,
           // rather than compounding an already-shrunk table.
           content.find('.fairy-h-table-scroll').each(function() {
             this.style.height = '';
             // All four tables (Aligned/Compact x plain/color — see
             // h_layout_toggle_group / h_color_toggle_group), not just
             // the first: whichever one is hidden right now could
             // become the visible one after the view carried onto
             // overlayContent below, and it needs to start unscaled too.
             this.querySelectorAll('table').forEach(function(t) { t.style.transform = 'none'; });
           });
           var overlayContent = $('#fairy-card-overlay-content');
           overlayContent.empty().append(content.children());
           // Carry the H-representation tab's trivial-bounds show/gray/
           // hide state (see #outP.trivial-gray/.trivial-hide) onto the
           // expanded card too — otherwise a card that's currently
           // graying-out or hiding those rows would reset to showing
           // everything the instant it's expanded, just because the
           // overlay is a different container than #outP.
           overlayContent.removeClass('trivial-gray trivial-hide').removeAttr('data-h-view');
           var outP = document.getElementById('outP');
           if (outP) {
             if (outP.classList.contains('trivial-gray')) overlayContent.addClass('trivial-gray');
             else if (outP.classList.contains('trivial-hide')) overlayContent.addClass('trivial-hide');
             // Same carry-over as the trivial-bounds state above, for
             // the Layout/Color toggles (see h_layout_toggle_group /
             // h_color_toggle_group and fairyApplyHView) — otherwise an
             // expanded card would always show Aligned+no-color
             // regardless of the current toggles.
             var curView = outP.getAttribute('data-h-view');
             if (curView) overlayContent.attr('data-h-view', curView);
           }
           $('#fairy-card-overlay').addClass('active');
           document.body.style.overflow = 'hidden';
           fitContentToWidth();
         }

         // Parsimony's volume plot needs the REAL node moved in, not a
         // clone — Plotly's internal state (what redraws/resizes it) is
         // tied to the exact DOM element it was drawn into, so a cloned
         // .js-plotly-plot is just inert markup. Tracked separately from
         // openOverlay()'s clone-based content so closeOverlay() knows
         // to put it back home rather than just discarding it.
         var parsimonyPlotHome = null, parsimonyPlotNext = null;
         function openParsimonyPlotFull() {
           var el = document.getElementById('parsimony_plot_large_home');
           if (!el) return;
           parsimonyPlotHome = el.parentNode;
           parsimonyPlotNext = el.nextSibling;
           $('#fairy-card-overlay-content').empty();
           document.getElementById('fairy-card-overlay-content').appendChild(el);
           el.style.display = 'block';
           $('#fairy-card-overlay').addClass('active');
           document.body.style.overflow = 'hidden';
           // Plotly doesn't auto-redraw just because its container
           // became visible/resized — nudge it once the reparented node
           // has settled into its new (much bigger) box.
           setTimeout(function() {
             var gd = el.querySelector('.js-plotly-plot');
             if (gd && window.Plotly) { Plotly.Plots.resize(gd); }
           }, 60);
         }

         function closeOverlay() {
           $('#fairy-card-overlay').removeClass('active');
           document.body.style.overflow = '';
           if (parsimonyPlotHome) {
             var el = document.getElementById('parsimony_plot_large_home');
             if (el) {
               el.style.display = 'none';
               parsimonyPlotHome.insertBefore(el, parsimonyPlotNext);
             }
             parsimonyPlotHome = parsimonyPlotNext = null;
           }
         }

         $(document).on('click', '.model-card', function(e) {
           if ($(e.target).closest('table, textarea, input, .btn, button').length) return;
           // The parsimony volume plot has its own full-screen expand
           // (openParsimonyPlotFull(), reparenting the real plot node,
           // see its own comment above) rather than the generic clone-
           // based openOverlay(), which would reparent a CLONE of the
           // small plot instead — inert, so expanding it visibly did
           // nothing to the plot itself. Defer to that instead when
           // clicking anywhere else on this specific card.
           if ($(this).find('#expand_parsimony_plot_btn').length) { openParsimonyPlotFull(); return; }
           openOverlay(this);
         });
         $(document).on('click', '#expand_parsimony_plot_btn', function(e) {
           e.stopPropagation();
           openParsimonyPlotFull();
         });
         // The comparison table is one big <table>, so the .model-card
         // click-anywhere-to-expand behavior above would never fire (it
         // deliberately excludes clicks landing on a <table>, since that's
         // where users click to actually interact with model-card
         // content elsewhere). An explicit button is needed here instead.
         $(document).on('click', '#comparison-table-fullscreen-btn', function() {
           var wrap = document.getElementById('comparison_table_wrap');
           if (wrap) openOverlay(wrap);
         });
         $(document).on('click', '#fairy-card-overlay', function(e) {
           if (e.target === this) closeOverlay();
         });
         $(document).on('click', '#fairy-card-overlay-close', closeOverlay);
         $(document).on('keydown', function(e) {
           if (e.key === 'Escape' && $('#fairy-card-overlay').hasClass('active')) closeOverlay();
         });
       })();
       // Dark mode toggle
       (function() {
         var DARK_BG   = '#0e1117';
         var DARK_PLOT = '#161b27';
         var DARK_FONT = '#dde1ea';
         var DARK_GRID = '#2d3347';
         var LITE_BG   = '#ffffff';
         var LITE_PLOT = '#ffffff';
         var LITE_FONT = '#1f2430';
         var LITE_GRID = '#e2e6ee';

         function applyOnePlot(div, isDark) {
           if (!window.Plotly || !div._fullLayout) return;
           var bg   = isDark ? DARK_BG   : LITE_BG;
           var plot = isDark ? DARK_PLOT : LITE_PLOT;
           var font = isDark ? DARK_FONT : LITE_FONT;
           var grid = isDark ? DARK_GRID : LITE_GRID;
           var isScene = !!div._fullLayout.scene;
           // Plotly.relayout() is unreliable on this Plotly build for both
           // 2D and 3D (gl3d) plots — it can leave the canvas blank/stale.
           // Plotly.react() with a full layout object does a proper
           // diff/redraw and is safe for both, so use it everywhere.
           try {
             var layout = JSON.parse(JSON.stringify(div.layout || {}));
             layout.paper_bgcolor = bg;
             if (isScene) {
               layout.scene = layout.scene || {};
               layout.scene.bgcolor = plot;
               ['xaxis', 'yaxis', 'zaxis'].forEach(function(ax) {
                 layout.scene[ax] = layout.scene[ax] || {};
                 layout.scene[ax].gridcolor = grid;
                 layout.scene[ax].color = font;
                 layout.scene[ax].backgroundcolor = plot;
               });
             } else {
               // NOTE: deliberately not touching svg.main-svg's own CSS
               // background here — animated plots (this one uses
               // frame=~vert_no) render a second main-svg for the
               // slider/updatemenu layer, and painting all main-svg
               // elements opaque covers the actual chart the same way it
               // did for gl3d. paper_bgcolor/plot_bgcolor below already
               // paint the correct internal .bg rect via react().
               layout.plot_bgcolor = plot;
               layout.font = Object.assign({}, layout.font, { color: font });
               ['xaxis', 'yaxis'].forEach(function(ax) {
                 layout[ax] = layout[ax] || {};
                 layout[ax].gridcolor = grid;
                 layout[ax].zerolinecolor = grid;
                 layout[ax].tickfont = Object.assign({}, layout[ax].tickfont, { color: font });
               });
             }
             Plotly.react(div, div.data, layout);
             // react() alone can leave a stale WebGL/SVG paint on this build
             // until something forces a reflow — resize does that reliably,
             // without which the new theme only shows up after a tab
             // revisit.
             setTimeout(function() {
               try { Plotly.Plots.resize(div); } catch(e) {}
             }, 0);
           } catch(e) {}
         }

         function applyPlotlyTheme(isDark) {
           document.querySelectorAll('.js-plotly-plot').forEach(function(div) {
             applyOnePlot(div, isDark);
           });
         }

         // Find .js-plotly-plot within root OR root itself — Shiny fires
         // shiny:value directly on the plotly div (it IS .js-plotly-plot),
         // and querySelectorAll only matches descendants, so a plain
         // descendant query silently finds nothing for that case.
         function findPlotlyPlots(root) {
           var out = [];
           if (root.matches && root.matches('.js-plotly-plot')) out.push(root);
           root.querySelectorAll && root.querySelectorAll('.js-plotly-plot').forEach(function(d) { out.push(d); });
           return out;
         }

         // Hide plotly outputs the instant they start recalculating, so the
         // fresh (light-themed) widget Shiny builds underneath is never
         // visibly painted. findPlotlyPlots correctly matches the output
         // element itself as well as descendants, and shiny:value below
         // unconditionally restores visibility on its last pass, so a plot
         // can never get stuck hidden.
         $(document).on('shiny:recalculating', function(e) {
           if (!document.body.classList.contains('dark-mode')) return;
           findPlotlyPlots(e.target).forEach(function(div) {
             div.style.visibility = 'hidden';
           });
         });

         // Re-theme plotly charts after Shiny re-renders them. Shiny finishes
         // building the widget before firing shiny:value, so re-theme
         // immediately (delay 0) to avoid a visible white flash; the later
         // passes are a safety net for any slow-to-initialise 3D scenes.
         $(document).on('shiny:value', function(e) {
           var tgt = e.target;
           var dark = document.body.classList.contains('dark-mode');
           [0, 300, 1000].forEach(function(delay, idx) {
             var isLastPass = idx === 2;
             setTimeout(function() {
               findPlotlyPlots(tgt).forEach(function(div) {
                 var ready = !dark || !!div._fullLayout;
                 if (dark && div._fullLayout) applyOnePlot(div, true);
                 // Only reveal once actually themed (or not in dark mode at
                 // all) — never on a pass where theming silently no-opped,
                 // that's what let the untheme white state show through
                 // before. The last pass always reveals regardless, so a
                 // plot can never get stuck hidden if something goes wrong.
                 if (ready || isLastPass) div.style.visibility = '';
               });
             }, delay);
           });
         });

         // withMathJax()'s own auto-retypeset doesn't reliably fire for
         // output$h when it re-renders in response to a plain input change
         // (e.g. the Model insights checkbox) rather than a fresh Shiny
         // value push from the server-side computation loop — the notes
         // toggle left the H-representation table showing literal
         // backslash-paren math delimiters instead of typeset math. Force
         // a retypeset on every shiny:value for the H-representation output.
         $(document).on('shiny:value', function(e) {
           // Scoping the typeset call to e.target specifically was found
           // (via live testing) to silently no-op, likely because the node
           // reference goes stale by the time the queued job runs — typeset
           // the whole Hub instead. Queuing synchronously (delay 0) inside
           // this handler was ALSO found to silently no-op — Shiny fires
           // shiny:value before it finishes swapping the new HTML into the
           // DOM, so MathJax scans the not-yet-updated content. A single
           // fixed delay was still occasionally too short/racy in testing,
           // so retry at a few delays (matching the plotly re-theme pattern
           // below) rather than betting on one timing.
           if (e.target && e.target.id === 'h' && window.MathJax && MathJax.Hub) {
             [50, 250, 600].forEach(function(delay) {
               setTimeout(function() {
                 // fitHTablesToWidth (see its own definition) has to run
                 // AFTER MathJax has actually typeset the equations, not
                 // just after the raw HTML lands — column widths depend
                 // entirely on the rendered math, not the source text.
                 // Hub.Queue's second argument runs once that specific
                 // Typeset job finishes, same as the retry-typeset
                 // comment above already explains for the job itself.
                 MathJax.Hub.Queue(['Typeset', MathJax.Hub], window.fitHTablesToWidth);
               }, delay);
             });
           }
         });

         // Plotly widgets that render while their Bootstrap tab is inactive
         // (display:none) get a 0x0 drawing buffer — this is especially
         // fatal for the WebGL-based 3D plots, which never repaint on their
         // own once the tab becomes visible. Force a resize/redraw whenever
         // any tab is shown so plots that initialised while hidden actually
         // draw their content.
         $(document).on('shown.bs.tab', function(e) {
           var pane = e.target.getAttribute('href') || e.target.getAttribute('data-value');
           var root = pane ? document.querySelector(pane) : document;
           if (!root) root = document;
           setTimeout(function() {
             findPlotlyPlots(root).forEach(function(div) {
               if (window.Plotly) Plotly.Plots.resize(div);
               applyOnePlot(div, document.body.classList.contains('dark-mode'));
             });
           }, 50);
         });

         if (localStorage.getItem('fairy-dark') === '1') {
           document.body.classList.add('dark-mode');
           $(document).ready(function() { applyPlotlyTheme(true); });
         }
         $(document).ready(function() {
           var btn = document.getElementById('dm-toggle');
           if (!btn) return;
           function updateIcon() {
             btn.textContent = document.body.classList.contains('dark-mode') ? '☀' : '🌙';
           }
           updateIcon();
           btn.addEventListener('click', function() {
             var isDark = document.body.classList.toggle('dark-mode');
             localStorage.setItem('fairy-dark', isDark ? '1' : '0');
             updateIcon();
             applyPlotlyTheme(isDark);
           });
         });
       })();
       // Grey out incompatible choices in the intersection/mixture
       // checkboxGroupButtons pickers, driven by a server-side custom
       // message (see greyOutChoices sendCustomMessage calls).
       (function() {
         Shiny.addCustomMessageHandler('greyOutChoices', function(msg) {
           // type_grouped_picker() renders one checkboxGroupButtons PER
           // group (base / derived / replication / items) but reuses the
           // SAME id for all of them, so a picker row is actually several
           // DOM elements sharing one id. getElementById only returns the
           // first, silently missing choices that live in a later group
           // (e.g. a replication model) — use an attribute selector to get
           // all of them instead.
           var containers = document.querySelectorAll('[id=\"' + msg.id + '\"]');
           if (!containers.length) return;
           // Shiny serializes R's disabled character vector with
           // auto_unbox=TRUE: a single-element vector like c(m2) becomes
           // the bare JSON string m2 (not an array), and NULL becomes an
           // empty object (not an array) — both break plain .forEach and,
           // left unhandled, throw on every future greyOutChoices message
           // thereafter. Normalize all three shapes.
           var disabled = Array.isArray(msg.disabled) ? msg.disabled :
             (typeof msg.disabled === 'string' ? [msg.disabled] : []);
           containers.forEach(function(container) {
             // Each choice is a button.checkbtn wrapping the actual checkbox
             // input (shinyWidgets checkboxGroupButtons markup) — not a
             // label, despite that being the more common Bootstrap pattern
             // elsewhere.
             container.querySelectorAll('button.checkbtn').forEach(function(btn) {
               btn.classList.remove('choice-disabled');
             });
             disabled.forEach(function(val) {
               var input = container.querySelector('input[value=\"' + CSS.escape(val) + '\"]');
               if (!input) return;
               var btn = input.closest('button');
               if (btn) btn.classList.add('choice-disabled');
             });
           });
         });
       })();"
    ))
  ),
  tags$button(id = "dm-toggle", title = "Toggle dark mode", "\U0001F319"),
  # The manual sidebar-collapse toggle button itself is gone (most tabs
  # don't have a sidebar to collapse any more — see the Input/H-rep/
  # V-rep tabPanel's own comments), but the underlying mechanism it used
  # to drive (hiddenState, applyState(), updateFab()) is still very much
  # alive: FORCE_FLOAT_TABS (see the script below) makes it always float
  # Go/Download/Upload on the Input tab regardless of hiddenState, and
  # Parsimony/the Plot tabs still have real sidebars of their own,
  # unaffected either way.
  # Floating panel that the active tab's real primary-action button (Go /
  # Compute parsimony / ...) — and, if present, its .fairy-primary-controls
  # sibling (e.g. the algorithm checkboxes on Model Properties) — gets
  # reparented into while the sidebar is hidden and so otherwise out of
  # reach. The actual elements move here (not a proxy/clone), so the
  # label and any controls it depends on are always exactly right; see
  # the sidebar-toggle script's own comment.
  tags$div(
    id = "sidebar-fab-panel", style = "display:none;",
    tags$div(class = "fairy-fab-handle", title = "Drag to move")
  ),
  tags$div(id = "corner-fairy", class = "fairy-brand", fairy_svg),
  # Accessibility: the app had no heading structure at all — zero h1-h4
  # anywhere, only a handful of h5s deep in specific tabs (Parsimony
  # results) — and navbarPage's own title is deliberately empty (the
  # visible brand is corner-fairy's mascot SVG instead, not text). A
  # screen reader's own "jump to next heading" navigation, one of the
  # most common ways that's used to get oriented on a page at all,
  # therefore had nothing to land on for the app as a whole. A single
  # visually-hidden h1 (see .sr-only) gives that a real top-level
  # landmark without changing anything visually — windowTitle above
  # already carries this same name into the browser tab.
  tags$h1(class = "sr-only", "Modeling Fairy App"),
  navbarPage(
    title = "",
    position = "fixed-top",
    id = "tabs",
    fluid = T,
    inverse = T,
    windowTitle = "Modeling Fairy App",
    tabPanel(
      "Input",
      tags$head(
        tags$style(type = "text/css", ".btn-default.active, .btn-default:active, .btn-group > .btn-default.active { background-color: #5cb85c !important; border-color: #4cae4c !important; color: #fff !important; }"),
        tags$style(type = "text/css", "select { max-width: 200px; }"),
        tags$style(type = "text/css", ".span4 { max-width: 200px; }"),
        tags$style(type = "text/css", ".well { max-width: 200px; }"),
        tags$style(type = "text/css", "select { min-width: 200px; }"),
        tags$style(type = "text/css", ".span4 { min-width: 200px; }"),
        tags$style(type = "text/css", ".well { min-width: 200px; }")
      ),
      # No sidebar on this tab, and no static top toolbar either — Go,
      # the stale-results warning, and Download/Upload all live in the
      # SAME free-floating/draggable panel the sidebar-collapse feature
      # already built (#sidebar-fab-panel — see updateFab() and the
      # comment on FORCE_FLOAT_TABS below), so nothing here claims any
      # fixed width from the model rows. This block itself renders
      # display:none (see .fairy-primary-controls-src) — its content
      # only ever appears after JS reparents it into the floating panel.
      div(
        class = "fairy-primary-controls-src",
        # Go is the ONLY thing always visible while the panel is
        # collapsed — absolutely positioned at the panel's fixed
        # bottom-right corner (see .fairy-primary-action's CSS), so its
        # on-screen position never depends on what .fairy-primary-
        # controls is showing/hiding. Stale results aren't a separate
        # badge any more; Go itself blinks (fairy-stale-blink, toggled
        # by an observer on the server) and its own tooltip explains why.
        # Back to the default upward-opening tooltip — now that the
        # panel actually widens on hover to contain Download/Upload +
        # Go (see #sidebar-fab-panel's own hover rule), there's real
        # room above Go inside the card again, so the downward-opening
        # workaround (fairy-tooltip-down) that dodged the icon row is
        # no longer needed and read as detached from the card instead.
        actionBttn("go_v_h", "Go", style = "jelly", color = "primary", size = "sm",
          class = "fairy-primary-action fairy-tooltip", `data-tooltip` = "Recalculate (⌘/Ctrl+Enter)"),
        # Download/Upload: ordinary hover-collapsible flow content.
        div(class = "fairy-primary-controls",
          tags$span(class = "fairy-tooltip", `data-tooltip` = "Download app input",
            # Starts muted (grey, not the usual active blue) — nothing
            # meaningful to export yet. A server-side observer (see
            # "Mute Download until something's typed" below) toggles
            # fairy-btn-muted off via shinyjs::toggleClass the moment any
            # model specification box actually has content, and back on
            # if everything gets cleared out again — driven by Shiny's
            # own reactive graph rather than a client-side DOM poll, so
            # there's no separate copy of "what counts as filled in" to
            # keep in sync and no polling-interval timing to get wrong.
            downloadButton("download", NULL, icon = icon("download"), class = "fairy-toolbar-iconbtn fairy-btn-muted")
          ),
          # A hand-rolled <input type=file> here would NOT get Shiny's
          # real upload wiring (the multipart POST + progress bar live in
          # a dedicated JS binding keyed off fileInput()'s own DOM shape)
          # — input$upload would just never populate. Keep the real
          # fileInput() and restyle it with CSS instead (see
          # .fairy-toolbar-fileinput) rather than reimplementing upload.
          # buttonLabel takes raw HTML, so a real Font Awesome icon (same
          # family as downloadButton's own icon() above) can go there
          # directly instead of faking one with hand-drawn CSS shapes.
          div(class = "fairy-toolbar-fileinput fairy-tooltip", `data-tooltip` = "Upload app input (.xlsx)",
            fileInput("upload", NULL, multiple = FALSE, accept = c(".xlsx"), placeholder = "",
              buttonLabel = HTML(as.character(icon("upload"))))
          )
        )
      ),
      mainPanel(
        width = 12,
        shinyBS::bsTooltip("textbox_ui_rel",
          'Write linear in/equalities using +, -, *, /, parentheses, fractional and decimal numbers, p1, p2, p3, and &lt;, &gt;, =. Separate constraints with ";" such as "p1 &lt; p2; p2 &lt; p3".<br><br>"{p1,p2} &lt; {p3,3*p4}" is a shortcut for "p1 &lt; p3; p1 &lt; 3 * p4; p2 &lt; p3; p2 &lt; 3*p4".',
          placement = "top", trigger = "hover"
        ),
        # Same syntax help, on the Shared Model Specification box
        # (id = add_for_all_models — a real, stable id, not a per-model
        # dynamic one like textin_relations_i, so it can be targeted
        # directly rather than through textbox_ui_rel's wrapper).
        shinyBS::bsTooltip("add_for_all_models",
          'Write linear in/equalities using +, -, *, /, parentheses, fractional and decimal numbers, p1, p2, p3, and &lt;, &gt;, =. Separate constraints with ";" such as "p1 &lt; p2; p2 &lt; p3".<br><br>"{p1,p2} &lt; {p3,3*p4}" is a shortcut for "p1 &lt; p3; p1 &lt; 3 * p4; p2 &lt; p3; p2 &lt; 3*p4".',
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          "textbox_approx",
          ".05 means... <br><br> ... p1 = .5 will be set to p1 < .55 and p1 > .45, <br> ...  p1 = 1 will be set to p1 < 1 and p1 > .95,  <br> ... p1 = p2 will be set to p1 - p2 < .05 and  - p1 + p2 < .05.",
        ),
        div(
          class = "model-def-panel",
          # Probabilities as a horizontal header above the model rows,
          # rather than their own narrow leftmost column — frees up that
          # width for Model Name / Unique Model Specification / Model
          # Specification, which is where the actually-long content lives.
          div(
            class = "def-col def-col--pnames def-header--pnames",
            div(
              style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
              # tags$h2, not tags$strong — part of the same heading-
              # structure fix as the hidden h1 above (a section under the
              # page's own title); styled to look pixel-identical to the
              # old <strong> (inline, no margin, inherited size) rather
              # than an h2's own block/large/margin defaults, so nothing
              # changes visually, only the semantics underneath.
              tags$h2("Probabilities", style = "display:inline; margin:0; font-size:inherit; font-weight:bold; color: var(--fairy-text-muted);"),
              # Quickkeys for all four counter buttons here and on
              # Model(s) below — see the global keydown handler (search
              # fairyCounterKeys) for what actually fires the click.
              # fairy-tooltip-down: this row sits right under the fixed
              # navbar (see set_prob_count's own tooltip a few lines
              # down for the same fix/reasoning).
              tags$span(class = "fairy-tooltip fairy-tooltip-down", `data-tooltip` = "Remove a probability (⌘/Ctrl+Alt+-)",
                actionBttn("rm_prob", "", icon = icon("minus"), size = "xs", style = "jelly", color = "default")),
              tags$span(class = "fairy-tooltip fairy-tooltip-down", `data-tooltip` = "Add a probability (⌘/Ctrl+Alt++)",
                actionBttn("add_prob", "", icon = icon("plus"), size = "xs", style = "jelly", color = "primary")),
              # Typing an exact count directly, rather than only being
              # able to step it one at a time with +/- — reported
              # directly as a wanted alternative for setting a specific
              # number of probabilities. Kept alongside (not instead of)
              # the +/- buttons; both act on the same manual_p_floor, see
              # its own comment and observeEvent(input$set_prob_count,...).
              # fairy-tooltip-down: this row sits right under the fixed
              # navbar, so the default upward-opening tooltip had
              # nowhere to open into and got clipped/overlapped by it
              # instead (reported directly, screenshot showed the
              # tooltip text cut off behind the navbar).
              tags$span(class = "fairy-tooltip fairy-tooltip-down", `data-tooltip` = "Set an exact number of probabilities",
                numericInput("set_prob_count", NULL, value = 3, min = 3, max = 30, width = "62px")),
              actionBttn("show_items_modal", "Repeated items", icon = icon("layer-group"),
                size = "xs", style = "jelly", color = "default"
              )
            ),
            uiOutput("textbox_ui_name")
          ),
          div(
            class = "def-col--models-card",
            div(
              style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
              # Same heading-structure fix as "Probabilities" above —
              # see its own comment.
              tags$h2("Model(s)", style = "display:inline; margin:0; font-size:inherit; font-weight:bold; color: var(--fairy-text-muted);"),
              tags$span(class = "fairy-tooltip", `data-tooltip` = "Remove a model (⌘/Ctrl+Shift+-)",
                actionBttn("rm_ie", "", icon = icon("minus"), size = "xs", style = "jelly", color = "default")),
              tags$span(class = "fairy-tooltip", `data-tooltip` = "Add a model (⌘/Ctrl+Shift++)",
                actionBttn("add_ie", "", icon = icon("plus"), size = "xs", style = "jelly", color = "primary")),
              # Both buttons only make sense with 2+ models (an
              # intersection/mixture of one model is that model) — see
              # intersection_mixture_btns_ui's own comment for why they
              # now actually disappear below that threshold instead of
              # just greying out, and blink back in each time they
              # reappear.
              uiOutput("intersection_mixture_btns_ui", inline = TRUE),
              # The master V-representation include/exclude-all toggle
              # used to live here; moved to sit directly above the model
              # rows' own icon column instead (see fairy-model-icon-col),
              # so it visually reads as "applies to the column below it"
              # rather than as one more header-toolbar button.
              tags$button(
                id = "compact-toggle", type = "button", class = "fairy-compact-toggle-btn",
                title = "Toggle compact view",
                style = "margin-left:auto; border:1px solid var(--fairy-border); background:transparent; color:var(--fairy-text-muted); border-radius:6px; padding:3px 10px; font-size:12px; cursor:pointer;",
                icon("compress"), "Compact"
              )
            ),
            useShinyjs(),
            # A plain HTML table for both rows. inline-block (splitLayout),
            # CSS grid, and absolute positioning were all tried before this
            # and each looked correctly top-aligned in testing here but was
            # repeatedly reported as visibly misaligned in the field,
            # pointing to a real environment difference in how one of
            # those modern layout mechanisms gets resolved — rather than
            # keep guessing at which one, this uses table-cell vertical
            # alignment, which every browser has implemented identically
            # for decades.
            tags$table(
              style = "width:100%; border-collapse:collapse; table-layout:fixed;",
              # Percentage widths (not fixed px) so the columns shrink to
              # fit whatever width the card actually has — e.g. with the
              # sidebar visible on a narrower window — instead of forcing
              # a hard total width that pushes the third column outside
              # the card. table-layout:fixed itself (needed for reliable
              # cross-browser vertical alignment) works the same either
              # way; only the unit here changes.
              tags$colgroup(
                tags$col(style = "width:16%;"),
                tags$col(style = "width:42%;"),
                tags$col(style = "width:42%;")
              ),
              tags$tr(
                tags$td(style = "vertical-align:top; padding:0 6px;"),
                tags$td(style = "vertical-align:top; padding:0 6px;"),
                tags$td(
                  style = "vertical-align:top; padding:0 6px;",
                  tags$div(
                    style = "position: relative;",
                    tags$button(
                      class = "fairy-expand-btn", type = "button",
                      title = "Expand",
                      `data-target` = "add_for_all_models",
                      style = "position:absolute; top:26px; right:4px; z-index:5; border:none; background:transparent; cursor:pointer; font-size:14px; color:#888;",
                      HTML("&#10530;")
                    ),
                    textAreaInput(
                      inputId = "add_for_all_models",
                      label = "Shared Model Specification",
                      value = "",
                      width = "100%",
                      height = "80px",
                      placeholder = "p1 < .5; p2 < .5; ...",
                      resize = "none",
                    )
                  )
                )
              ),
              tags$tr(
                tags$td(style = "vertical-align:top; padding:6px;", uiOutput("textbox_ui_name_rel", class = "def-col")),
                tags$td(style = "vertical-align:top; padding:6px;", uiOutput("textbox_ui_rel", class = "def-col")),
                tags$td(style = "vertical-align:top; padding:6px;", uiOutput("textbox_ui_rel_complete", class = "def-col"))
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "H-representation",
      # No sidebar — its only content was three download links. A
      # horizontal row of labeled buttons ate up roughly half the
      # screen's width for something that's really just "download this,
      # in one of three formats" — a narrow column of small icon
      # buttons, laid out as a genuine flex row alongside the
      # H-representation cards (a plain CSS float was tried first, but
      # floats only pull INLINE content around them, not sibling block
      # boxes like these .model-card divs, so the cards just rendered
      # underneath/behind it instead of beside it), takes barely any
      # width at all.
      div(
        style = "display:flex; align-items:flex-start; gap:8px; width:100%;",
        div(
          class = "fairy-input-toolbar",
          style = "display:flex; flex-direction:column; align-items:center; gap:6px; padding:4px; flex:0 0 auto;",
          # Was a single button that cycled shown -> grayed -> hidden on
          # repeat clicks — reported as unintuitive: nothing on screen
          # told a user a 3rd state existed, and getting back to a
          # earlier state meant cycling all the way around again. A
          # segmented control (3 mini-buttons, one per state, the active
          # one highlighted) makes all three choices visible up front and
          # every state one click away — same underlying #outP class
          # (trivial-gray/trivial-hide) as before, just set directly by
          # each button instead of advanced by one shared cycling button.
          tags$div(class = "fairy-segmented", id = "trivial_ineq_toggle_group",
            tags$button(type = "button", class = "active", `data-state` = "shown", icon("eye")),
            tags$button(type = "button", `data-state` = "gray", icon("circle-half-stroke")),
            tags$button(type = "button", `data-state` = "hide", icon("eye-slash"))
          ),
          # Two ways to lay out each H-rep table (all four Layout x
          # Color variants rendered server-side up front, see
          # build_aligned_rows/build_packed_rows below — this toggle,
          # combined with the Color one right after it, just shows one
          # and hides the rest, no recompute). "Aligned" keeps one
          # column per parameter so the same parameter sits in the same
          # column on every row (good for scanning down a column);
          # "Compact" packs each row's own terms together with no
          # reserved space for parameters that row doesn't use (good once
          # a model has enough parameters that Aligned's many mostly-
          # empty columns spread a row's real content far apart). Same
          # segmented-control treatment as Trivial bounds above, for the
          # same "single button silently cycling through 3 states"
          # reason a single Aligned/Compact/Compact+color button used to
          # have.
          tags$div(class = "fairy-segmented", id = "h_layout_toggle_group",
            tags$button(type = "button", class = "active", `data-state` = "aligned", icon("table-cells")),
            tags$button(type = "button", `data-state` = "compact", icon("bars"))
          ),
          # Color (by parameter, see param_color_map) used to be
          # available ONLY under Compact, bundled into the same 3-way
          # control as its own third state — reported as wanting color
          # independent of layout instead, so it's its own toggle here,
          # working under either Aligned or Compact. A 2-way segmented
          # control (not a bare on/off button) for the same "state must
          # be visible, not just cycled" reason as the others, even
          # though there are only two states — a single button flipping
          # a hidden boolean on click still doesn't SHOW which state is
          # active without reading its own color/tooltip.
          tags$div(class = "fairy-segmented", id = "h_color_toggle_group",
            tags$button(type = "button", class = "active", `data-state` = "off", icon("droplet-slash")),
            tags$button(type = "button", `data-state` = "on", icon("droplet"))
          ),
          # Separates the display toggles above (Trivial bounds, Layout,
          # Color — all affect how the SAME data looks) from the
          # downloads below (each produces a separate file) — reported
          # as visually running together into one undifferentiated
          # button stack.
          tags$hr(class = "fairy-toolbar-divider"),
          tags$span(class = "fairy-tooltip", `data-tooltip` = "H-representation for QTEST",
            downloadButton("d_h", NULL, icon = icon("download"), class = "fairy-toolbar-iconbtn")),
          tags$span(class = "fairy-tooltip", `data-tooltip` = "H-representation for multinomineq",
            downloadButton("d_h_multinomineq", NULL, icon = icon("download"), class = "fairy-toolbar-iconbtn")),
          tags$span(class = "fairy-tooltip", `data-tooltip` = "LaTeX file of H-representation",
            downloadButton("d_latex", NULL, icon = icon("file-alt"), class = "fairy-toolbar-iconbtn"))
        ),
        div(style = "flex: 1 1 auto; min-width: 0;",
      mainPanel(
        width = 12,
        id = "outP",
        fluidPage(
          withMathJax(),
          uiOutput("h_stale_note_hrep"),
          uiOutput("h"),
          uiOutput("h_repl")
        ),
        tags$script(
          '
            $("#go_v_h").click(function(){
                            $("#outP").removeClass("grey-out");
                        });
             $("#approx_equal").click(function(){
                            $("#outP").addClass("grey-out");
                        });
        $("#add_ie").click(function(){
                            $("#outP").addClass("grey-out");
                        });
           $("#rm_btn").click(function(){
                            $("#outP").addClass("grey-out");
                        });
        $("#add_btn").click(function(){
                            $("#outP").addClass("grey-out");
                        });
        // Trivial-bound (0<=p<=1) rows: shown / grayed / hidden, picked
        // directly by clicking the matching button in the segmented
        // control (see .fairy-segmented) rather than cycling through
        // them one at a time. Purely client-side (CSS class + row-level
        // markup already computed server-side, see the fairy-trivial-row
        // class in the H-representation render) so it applies instantly
        // and needs no recompute.
        $("#trivial_ineq_toggle_group button").click(function(){
          var el = $("#outP");
          var group = $("#trivial_ineq_toggle_group");
          var state = $(this).data("state");
          group.find("button").removeClass("active");
          $(this).addClass("active");
          el.removeClass("trivial-gray trivial-hide");
          if (state === "gray") el.addClass("trivial-gray");
          else if (state === "hide") el.addClass("trivial-hide");
        });
        // H-rep table Layout (aligned/compact) and Color (off/on) are
        // two INDEPENDENT segmented controls now (used to be one 3-way
        // control with color only reachable under Compact) -- combined
        // here into the single data-h-view value the CSS actually
        // switches on (see its own comment for the 4 values), so either
        // toggle just needs to read the other control current state, not
        // coordinate through it. All four table variants are always
        // rendered server-side (see .fairy-h-aligned-plain-table/
        // -color-table and .fairy-h-packed-plain-table/-color-table),
        // this only ever flips which one CSS shows, so it needs no
        // recompute and applies to every card at once.
        function fairyApplyHView() {
          var el = $("#outP");
          var layout = $("#h_layout_toggle_group button.active").data("state") || "aligned";
          var color  = $("#h_color_toggle_group button.active").data("state") || "off";
          var view = layout === "compact"
            ? (color === "on" ? "compact-color" : "compact")
            : (color === "on" ? "aligned-color" : "");
          if (view) el.attr("data-h-view", view); else el.removeAttr("data-h-view");
          // The card itself does not auto-shrink/grow just because CSS
          // swapped which (differently-sized) table is display:none --
          // window.fitHTablesToWidth left a stale transform/height on
          // the table that was visible before the switch, sized for
          // that width, not the newly-shown table. Clear that stale
          // state on every table in the wrap first (same reset
          // openOverlay already does when a card expands) so the
          // now-visible table remeasures from its own natural size.
          document.querySelectorAll(".fairy-h-table-scroll").forEach(function(scrollWrap) {
            scrollWrap.style.height = "";
            scrollWrap.querySelectorAll("table").forEach(function(t) { t.style.transform = "none"; });
          });
          if (window.fitHTablesToWidth) window.fitHTablesToWidth();
        }
        $("#h_layout_toggle_group button, #h_color_toggle_group button").click(function(){
          var group = $(this).parent();
          group.find("button").removeClass("active");
          $(this).addClass("active");
          fairyApplyHView();
        });
                        '
        )
      )
        )
      )
    ),
    tabPanel(
      "V-representation",
      # No sidebar — same reasoning as H-representation's own toolbar
      # (see its comment): a narrow flex column of icon buttons beside
      # the content instead of a wide row above it. "Show vertices in
      # words" isn't a download, but it's one small checkbox — it just
      # sits below the icons in that same narrow column rather than
      # getting a whole rail of its own.
      div(
        style = "display:flex; align-items:flex-start; gap:8px; width:100%;",
        div(
          class = "fairy-input-toolbar",
          style = "display:flex; flex-direction:column; align-items:flex-start; gap:6px; padding:4px; flex:0 0 auto;",
          tags$span(class = "fairy-tooltip", `data-tooltip` = "V-representation for QTEST",
            downloadButton("d_v", NULL, icon = icon("download"), class = "fairy-toolbar-iconbtn")),
          tags$span(class = "fairy-tooltip", `data-tooltip` = "V-representation for multinomineq",
            downloadButton("d_v_multinomineq", NULL, icon = icon("download"), class = "fairy-toolbar-iconbtn")),
          tags$span(class = "fairy-tooltip", `data-tooltip` = "LaTeX table (words: all/none/fractions)",
            downloadButton("d_v_latex", NULL, icon = icon("file-alt"), class = "fairy-toolbar-iconbtn"))
        ),
        div(style = "flex: 1 1 auto; min-width: 0;",
      mainPanel(
        width = 12,
        # The "Show vertices in words" checkbox used to live in the icon
        # column on the left — its label text alone was wider than all
        # three icon buttons combined, so it dragged that whole narrow
        # column out to its width instead of the icons'. It belongs with
        # the table it affects anyway, not bundled in with the download
        # icons just because both happened to be "V-representation
        # controls" — moving it here keeps the icon column genuinely
        # icon-width and gives the checkbox as much room as it wants.
        uiOutput("h_stale_note_vrep"),
        checkboxInput("v_rep_words", "Show vertices in words", value = FALSE),
        fluidRow(column(
          id = "v_rep",
          12, uiOutput("v_representation_table")
        )),
        tags$script(
          '
         $("#go_v_h").click(function(){
                            $("#v_rep").removeClass("grey-out");
                        });
             $("#approx_equal").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
                        $("#rm_ie").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
        $("#add_ie").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
           $("#rm_btn").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
        $("#add_btn").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
                        '
        )
      )
        )
      )
    ),
    # navbarPage
    tabPanel(
      "Plot Edge Cases",
      sidebarPanel(
        class = "fairy-sidebar",
        style = "position:sticky;top:70px;width:inherit;",
        width = 2,
        fluidRow(
          column(12,
            offset = 0,
            h5("Choose a model")
          ),
          column(12,
            offset = 0,
            pickerInput("go_example", "", choices = NA)
          ),
        ),
      ),
      mainPanel(fluidRow(
        column(12, uiOutput("h_stale_note_edge")),
        column(
          id = "example_out",
          12, div(plotlyOutput("plot_example", width = "100%", height = "80vh"), align = "center")
        )
      ))
    ),
    tabPanel(
      "Plot Polytope(s)",
      sidebarPanel(
        class = "fairy-sidebar",
        style = "position:sticky;top:70px;width:inherit;",
        width = 2,
        fluidRow(
          column(12,
            offset = 0,
            h5("Remove or add all models")
          ),
          column(
            12,
            offset = 0,
            actionBttn("subtr_models", "",
              icon = icon("minus"), size = "sm",
              style = "jelly", color = "default"
            ),
            actionBttn("add_models", "",
              icon = icon("plus"), size = "sm",
              style = "jelly", color = "primary"
            )
          )
        ),
        fluidRow(column(
          12,
          offset = 0,
          hr(),
          textAreaInput(
            inputId = "name_model_plot",
            label = "Input field for model names",
            value = "",
            width = "100%",
            height = "120px",
            resize = "none"
          )
        )),
        fluidRow(column(
          12,
          offset = 0,
          hr(),
          materialSwitch("auto_rotate_plot", "Auto-rotate",
            value = FALSE, status = "primary", right = TRUE
          )
        )),
        fluidRow(column(
          12,
          offset = 0,
          hr(),
          h5("Select probabilities")
        )),
        dim_picker_row(1, 1),
        dim_picker_row(2, 2),
        dim_picker_row(3, 3)
      ),
      mainPanel(
        fluidRow(column(12, uiOutput("h_stale_note_poly"))),
        fluidRow(column(
          12, div(

            plotlyOutput("plot", width = "100%", height = "800px"),
            align = "center"
          )
        )),
        tags$script(HTML(
          "
          (function() {
            var rotateTimer = null;
            var theta = 0, phi = 0;
            function startRotate() {
              if (rotateTimer) return;
              var gd0 = document.getElementById('plot');
              // Keep whatever zoom level is already on screen instead of
              // snapping to a fixed distance — read the current eye vector
              // (falling back to Plotly's own default) and rotate around
              // ITS radius, so toggling auto-rotate never changes the zoom.
              var eye0 = (gd0 && gd0.layout && gd0.layout.scene &&
                gd0.layout.scene.camera && gd0.layout.scene.camera.eye) ||
                {x: 1.25, y: 1.25, z: 1.25};
              var r = Math.sqrt(eye0.x*eye0.x + eye0.y*eye0.y + eye0.z*eye0.z) || 1.8;
              theta = Math.atan2(eye0.y, eye0.x);
              phi   = Math.asin(eye0.z / r);
              rotateTimer = setInterval(function() {
                var gd = document.getElementById('plot');
                if (!gd || !gd.layout) return;
                // theta tumbles the view around the vertical axis, phi
                // drifts (and wraps via sin/cos) up and down through the
                // poles, so the combined path covers all directions
                // instead of spinning flat around a single axis. r is
                // fixed for the whole rotation, so distance never changes.
                theta += 0.020;
                phi   += 0.007;
                var eye = {
                  x: r * Math.cos(theta) * Math.cos(phi),
                  y: r * Math.sin(theta) * Math.cos(phi),
                  z: r * Math.sin(phi)
                };
                Plotly.relayout(gd, {'scene.camera.eye': eye});
              }, 50);
            }
            function stopRotate() {
              if (rotateTimer) { clearInterval(rotateTimer); rotateTimer = null; }
            }
            $(document).on('change', '#auto_rotate_plot', function() {
              if ($(this).is(':checked')) startRotate(); else stopRotate();
            });
          })();
          "
        ))
      )
    ),
    tabPanel(
      HTML("&#9654; Parsimony"),
      value = "Model Properties",
      # No sidebar — this tab's controls (algorithm switches, Compute,
      # settings, download) live entirely in the same always-floating
      # panel Input's Go/Download/Upload use (see FORCE_FLOAT_TABS),
      # rather than duplicating that pattern as yet another dedicated
      # rail. bsTooltip's container="body" means it isn't affected by
      # this button being reparented into the panel later.
      div(
        class = "fairy-primary-controls-src",
        # Icon-only + a plain fairy-tooltip (same pattern as the icon
        # buttons elsewhere) instead of a labeled button — "Compute
        # parsimony" spelled out doesn't fit the compact floating panel
        # nearly as well as Go/Download/Upload's short labels did. The
        # tooltip attribute has to sit on the BUTTON itself, not a
        # wrapping span — updateFab() finds and reparents this element
        # via querySelector('.fairy-primary-action') specifically (not
        # its parent), so a wrapping span's data-tooltip would get left
        # behind, tooltip-less, the moment this button floats.
        # A single data-tooltip, kept in sync with the button's
        # enabled/disabled state by the input$CB|SoB|CG observer below
        # (which toggles the text via runjs) — a separate bsTooltip used
        # to sit here too for the disabled-state message, but shinyBS
        # binds unconditionally on hover regardless of disabled state,
        # so both tooltips ended up showing stacked/overlapping at once.
        # Compute is the ONLY thing always visible while collapsed — see
        # the matching comment on the Input tab's Go button. Stale
        # results blink Compute itself instead of a separate badge.
        # Sideways tooltip (fairy-tooltip-left) — same as every other
        # trigger in this panel (switches, settings, download), NOT the
        # upward/downward treatment Input's Go uses. Input's panel now
        # widens on hover to fit Download/Upload/Go on one row with
        # open space above, so upward works there; this panel is still
        # a TALL stack (3 switches sit directly above this same row),
        # so upward would land right back on "Sequ. of Balls" and
        # downward reads as detached from the card. Sideways is the one
        # direction that's actually clear regardless of stack position.
        actionBttn("go", "Go", style = "jelly", color = "primary", size = "sm",
          class = "fairy-primary-action fairy-tooltip fairy-tooltip-left", `data-tooltip` = "Compute parsimony (⌘/Ctrl+Enter)"),
        div(
          # Vertical stack (one switch per line) — hover-collapsible flow
          # content, same as Input's Download/Upload. Settings/Download
          # share their own row at the bottom of the stack.
          class = "fairy-primary-controls", `data-fab-layout` = "column",
          tags$span(class = "fairy-tooltip fairy-tooltip-left", style = "display:block;",
            `data-tooltip` = "General-purpose, recommended",
            materialSwitch("CB", "Cooling Bodies", value = TRUE, status = "primary", right = TRUE)
          ),
          tags$span(class = "fairy-tooltip fairy-tooltip-left", style = "display:block;",
            `data-tooltip` = "Gaussian annealing",
            materialSwitch("CG", "Cooling Gaussian", value = FALSE, status = "primary", right = TRUE)
          ),
          tags$span(class = "fairy-tooltip fairy-tooltip-left", style = "display:block;",
            `data-tooltip` = "Ball-based annealing",
            materialSwitch("SoB", "Sequ. of Balls", value = FALSE, status = "primary", right = TRUE)
          ),
          div(class = "fairy-fab-action-row",
            tags$span(class = "fairy-tooltip fairy-tooltip-left", `data-tooltip` = "Algorithm settings",
              # Same square fairy-toolbar-iconbtn look Download uses
              # (and Input's Download/Upload both use) — was the round
              # jelly-pill style before, visually inconsistent with the
              # icon next to it.
              actionButton("show", NULL, icon = icon("sliders"), class = "fairy-toolbar-iconbtn")
            )
          )
        )
      ),
      mainPanel(
        width = 12,
        fluidPage(
          div(
            class = "model-def-panel",
            style = "padding: 18px 22px;",
            uiOutput("parsimony_placeholder_ui"),
            div(id = "parsim_out",
              uiOutput("parsimony_stale_note"),
              uiOutput("parsimony_results_header_ui"),
              uiOutput("parsimony_plot_ui"),
              uiOutput("parsimony_spinner_table")
            )
          )
        )
      )
    )
  ),
  navbarPage(
    title = "",
    position = "fixed-bottom",
    fluid = T,
    inverse = F,
    id = "banner"
  )
))

server <- shinyServer(function(input, output, session) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  win_cluster <- NULL

  par_lapply <- function(X, FUN, ...) {
    if (isTRUE(isolate(input$use_parallel))) {
      n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
      if (.Platform$OS.type == "windows") {
        # mclapply forks, which the Windows kernel does not support (it
        # silently runs sequentially there) — a PSOCK cluster of worker
        # processes is the Windows-compatible equivalent. Reuse one cluster
        # for the whole session instead of spinning one up per call, since
        # starting worker processes is comparatively slow.
        if (is.null(win_cluster)) {
          win_cluster <<- parallel::makeCluster(n_cores)
          parallel::clusterEvalQ(win_cluster, library(rcdd))
          session$onSessionEnded(function() {
            tryCatch(parallel::stopCluster(win_cluster), error = function(e) NULL)
          })
        }
        parallel::parLapply(win_cluster, X, FUN, ...)
      } else {
        parallel::mclapply(X, FUN, mc.cores = n_cores, ...)
      }
    } else {
      lapply(X, FUN, ...)
    }
  }

  par_lapply_cores <- function() max(1L, parallel::detectCores(logical = FALSE) - 1L)

  # Own-built "Computing X" progress overlay (#fairy-progress-overlay in
  # the UI) — see that div's own comment for why this replaced repeated
  # shinyalert() calls entirely instead of patching around them. Three
  # calls cover the whole lifecycle of one batch:
  #   fairy_progress_open(title, text)   — once, at the start of a batch
  #   fairy_progress_update(title, text) — once per later model in that
  #                                        batch (either arg may be
  #                                        omitted/NULL to leave it as-is)
  #   fairy_progress_close()             — once, when the batch is done
  #                                        (or on error, before showing
  #                                        the error shinyalert)
  fairy_progress_open <- function(title, text) {
    session$sendCustomMessage("fairy_progress_open", list(title = title, text = text))
  }
  fairy_progress_update <- function(title = NULL, text = NULL) {
    session$sendCustomMessage("fairy_progress_update",
      list(title = title %||% NA, text = text %||% NA))
  }
  fairy_progress_close <- function() {
    session$sendCustomMessage("fairy_progress_close", list())
  }

  # How long to hold each "Computing X" progress-dialog frame on screen
  # before moving to the next model. A flat delay either flickers by too
  # fast to read (a handful of models that each compute in a few ms) or
  # adds seconds of pointless waiting (a long batch of many small models)
  # — this scales the per-frame hold down as the batch grows, capped at
  # both ends. Halved from an earlier 0.4s-2s range (direct user
  # feedback: the feedback screens sat too long) — still readable, just
  # snappier, with the same shape (tapering down once a batch is
  # genuinely large) preserved.
  fairy_progress_hold <- function(total) {
    if (is.null(total) || total < 1) total <- 1
    min(1, max(0.2, 3 / total))
  }

  # Show whole chips only, never a row sliced off mid-chip by a
  # max-height/overflow clip (which, seen in a static popup that isn't
  # actively being scrolled, just reads as broken/overlapping content).
  # A capped chip count plus a "+N more" chip guarantees every visible
  # row is complete, regardless of how many models are in the batch.
  # Shared by fairy_progress_text() and fairy_success_chips() below.
  fairy_chip_row <- function(label_html, chips, chip_class, max_n = 12) {
    if (is.null(chips) || length(chips) == 0) return("")
    shown <- if (length(chips) > max_n) chips[seq_len(max_n)] else chips
    more_n <- length(chips) - length(shown)
    more_html <- if (more_n > 0)
      paste0("<span class='", chip_class, " fairy-progress-chip-more'>+", more_n, " more</span>") else ""
    paste0(
      label_html,
      "<div class='", chip_class, "s'>",
      paste0("<span class='", chip_class, "'>", shown, "</span>", collapse = ""),
      more_html,
      "</div>"
    )
  }

  # Builds the shinyalert progress-dialog body used while H-representations,
  # V-representations, intersections, and mixtures are computed. Keeps
  # "already done" (a faded list below a divider) visually separate from
  # "currently running" (the spinner/bar above it), and — when running under
  # the parallel-computation switch — surfaces that mode and the core count
  # instead of leaving it invisible to the user.
  fairy_progress_text <- function(items = NULL, done = NULL, total = NULL, parallel = FALSE, n_jobs = NULL) {
    svg <- "<div class='fairy-progress-figure'><svg viewBox='0 0 64 64' xmlns='http://www.w3.org/2000/svg'><g fill='none' stroke-linecap='round' stroke-linejoin='round'><path d='M31 34 C16 20, 6 24, 10 34 C6 44, 18 44, 31 34 Z' fill='#8fb3ff' stroke='#5b7fd6' stroke-width='1.5' opacity='0.85'/><path d='M33 34 C48 20, 58 24, 54 34 C58 44, 46 44, 33 34 Z' fill='#a9c4ff' stroke='#5b7fd6' stroke-width='1.5' opacity='0.85'/><circle cx='32' cy='20' r='5' fill='#ffd98a' stroke='#e0a94a' stroke-width='1.4'/><path d='M32 25 L32 44 M32 30 L26 38 M32 30 L38 38 M32 44 L27 52 M32 44 L37 52' stroke='#f2b84b' stroke-width='2.4'/><path d='M50 12 l1.6 3.4 3.4 1.6 -3.4 1.6 -1.6 3.4 -1.6 -3.4 -3.4 -1.6 3.4 -1.6 Z' fill='#ffe08a'/></g></svg></div>"
    dots <- "<div class='fairy-progress-dots'>&#x22EF;</div>"
    cores_html <- if (isTRUE(parallel)) {
      # mclapply/parLapply never spawn more workers than there are jobs, so
      # showing the machine's full core budget when there are fewer jobs
      # than that would overstate how many cores are actually in use.
      cores_used <- if (!is.null(n_jobs)) min(par_lapply_cores(), max(1L, n_jobs)) else par_lapply_cores()
      paste0("<div class='fairy-progress-cores'>&#9889; Parallel &middot; ", cores_used, " core",
        if (cores_used == 1) "" else "s", "</div>")
    } else ""
    if (!is.null(total) && total > 0) {
      pct <- round(100 * length(done) / total)
      remaining <- total - length(done)
      bar_html <- paste0(
        "<div class='fairy-progress-bar-track'><div class='fairy-progress-bar-fill' style='width:", pct, "%;'></div></div>",
        "<div class='fairy-progress-count'>", length(done), " / ", total, " done &middot; ",
        remaining, " remaining</div>"
      )
    } else if (isTRUE(parallel)) {
      bar_html <- "<div class='fairy-progress-bar-track'><div class='fairy-progress-bar-indeterminate'></div></div>"
    } else {
      bar_html <- ""
    }
    items_html <- if (!is.null(items) && length(items) > 0) paste0(
      "<div class='fairy-progress-items'>",
      fairy_chip_row("<div class='fairy-progress-items-label'>Computing</div>", items, "fairy-progress-items-chip"),
      "</div>"
    ) else ""
    # Most-recently-finished model first — that's the one the user just
    # watched complete and is most likely checking for, and it also keeps
    # the chip that appears/moves each frame anchored at a fixed spot
    # (top-left of the Done list) instead of jumping to a growing tail.
    done_html <- if (!is.null(done) && length(done) > 0) paste0(
      "<div class='fairy-progress-done'>",
      fairy_chip_row("<div class='fairy-progress-done-label'>&#10003; Done</div>", rev(done), "fairy-progress-done-chip"),
      "</div>"
    ) else ""
    paste0("<div class='fairy-progress-wrap'>", items_html, svg, cores_html, bar_html, dots, done_html, "</div>")
  }

  # Companion to fairy_progress_text() for the final success dialog after a
  # batch finishes: same chip-list treatment as the "Done" section above,
  # without the spinner/bar — keeps the shinyalert title short (a long
  # semicolon-separated model list as an h2 title wraps badly) while still
  # naming every model that was built.
  fairy_success_chips <- function(items) {
    if (is.null(items) || length(items) == 0) return("")
    paste0(
      "<div class='fairy-progress-wrap'><div class='fairy-progress-done' style='margin-top:0;padding-top:0;border-top:none;'>",
      # Same most-recent-first ordering as fairy_progress_text()'s own
      # Done list (see its comment) — kept consistent here since this is
      # exactly that list's final, permanent state once the batch ends.
      fairy_chip_row("", rev(items), "fairy-progress-done-chip"),
      "</div></div>"
    )
  }

  shinyjs::html(
    id = "banner",
    html = "<SPAN STYLE='color:#FFFFFF'><p><center> &#129668; The Shiny app is under active development (Beta release 07/28/26), please click <a href='mailto:mjekel@uni-koeln.de?subject=bug-report fairy app' style='color: red;'>here</a> to report bugs. &#127984; </center></p></SPAN>",
    add = TRUE
  )


  hide("hidden_b")
  hide("open_b")


  hideTab(inputId = "tabs", target = "H-representation")
  hideTab(inputId = "tabs", target = "V-representation")
  hideTab(inputId = "tabs", target = "Model Properties")
  hideTab(inputId = "tabs", target = "Plot Polytope(s)")
  hideTab(inputId = "tabs", target = "Plot Edge Cases")

  observe({
    if (length(names_models_reactive$value) < 1 || all(is.na(names_models_reactive$value)))
      hideTab(inputId = "tabs", target = "Model Properties")
  })

  ##### Global Variables ####

  h_reactive <- reactiveValues()
  v_reactive <- reactiveValues()

  h_pars_reactive     <- reactiveValues()
  h_int_vol_reactive  <- reactiveValues()   # intersection volumes for Model Properties
  comp_int_reactive   <- reactiveValues(names = character())  # pairwise intersections auto-added for Parsimony
  # Row count of a model's H-representation BEFORE redundant() strips
  # implied clauses, captured where it's built (redundant()'s output is
  # what gets stored in h_reactive, so nothing downstream can otherwise see
  # how many rows there originally were). Used by the "theory-building
  # notes" redundancy check — comparing the already-minimized stored H
  # against itself would trivially always find nothing.
  h_raw_rows_reactive <- reactiveValues()

  mytable_v_reactive  <- reactiveValues(value = NULL)

  # Per-equality (not per-model) approximate-tolerance state: each of these
  # is keyed by model index (as a character, e.g. "1") to a plain numeric
  # vector where position k is the tolerance for the k-th "=" clause
  # encountered left-to-right in that model's spec (0 = exact, kept as a
  # real equality). Replaces the old single per-model scalar
  # (mytable_approx_reactive / the manual "Approximate equalities" dialog)
  # entirely — see the "≈" button that appears next to a model's Model
  # Specification box the moment it contains an "=" (conditionalPanel in
  # textboxes_relations_complete()), and the observeEvent(input$
  # eq_tol_btn_i, ...) wiring below that opens the dialog on click.
  #   equality_tolerances_reactive$value[[key]] — saved tolerances, one
  #     entry per equality occurrence (the actual setting used in
  #     computation, see "#### approx equal" below).
  #   equality_pending_reactive$value[[key]] — the clause list shown in
  #     the currently-open dialog for that model, read back by the
  #     dialog's own Save handler (by occurrence index, not by text) to
  #     know which eq_tol_switch_/eq_tol_value_ ids to collect.
  equality_tolerances_reactive <- reactiveValues(value = list())
  equality_pending_reactive <- reactiveValues(value = list())

  # Cache for the polytope-overlap (intersection) computation so it only
  # recomputes when the SET of plotted models changes, not on axis changes.
  overlap_cache_reactive <- reactiveValues(key = NULL, coords = NULL, empty = FALSE)

  # Names of the pairwise model intersections whose H-descriptions are stored
  # in h_reactive (kept out of names_models_reactive so they do not enter the
  # per-model loops for parsimony / V-representations).
  intersections_reactive  <- reactiveValues(names = NULL, disjoint = NULL)
  mixtures_reactive       <- reactiveValues(names = NULL)
  mytable_mix_reactive    <- reactiveValues(value = NULL)
  mytable_items_reactive  <- reactiveValues(value = NULL)
  items_reactive          <- reactiveValues(names = character())
  counter_mix             <- reactiveValues(n = 1)
  counter_items           <- reactiveValues(n = 1)
  vrep_choices            <- reactiveVal(character(0))

  # S4 class for volesti (defined once here; Parsimony observer re-calls setClass
  # which is idempotent when the definition matches)
  if (!isClass("model_s4"))
    setClass("model_s4", representation(A = "matrix", b = "numeric", type = "character"))

# ── Multiple-items H-rep builders (fully-crossed, arbitrary factors) ────────

  # factors:      list of list(name, n) — ordered factor definitions
  # param_factors: named integer vector — param name → factor index (1-based), 0 = Shared
  # substitutable: if TRUE, within each factor all level-combinations are used per slot;
  #                if FALSE (joint / fully-crossed), one level per factor per constraint copy.
  # factor_types: optional PER-FACTOR override of the above (a character
  # vector aligned with `factors`, values "joint"/"substitutable"/
  # "identical" — "identical" uses the joint/fully-crossed grid here too,
  # since its equality constraints are layered on afterward, not decided
  # at the grid level) — lets a single combined model mix substitutable
  # and joint/identical factors, rather than one substitutable/joint
  # choice for the whole model. Falls back to the uniform `substitutable`
  # flag when not given.
  # An items-expanded column's superscript — base^{(group,level)} — used
  # the group's name even for a SOLO (ungrouped) factor, where that name
  # is just the probability's own name again (see rows' own "name = ...
  # first$pname" for a solo row), producing e.g. "p_{1}^{(\text{p_{1}},1)}"
  # — the same name shown twice, once as the base and once inside its
  # own superscript. Reported directly as confusing ("repetitions in the
  # superscript and subscript"). A solo factor's copy index doesn't need
  # a group label at all — there's only one member, so "which group" is
  # never actually in question — so this drops the "\text{name},"
  # portion whenever the factor has exactly one member and its own name
  # is just that member's name (the solo-row signature), leaving the
  # plain "^{(1)}" a group name would otherwise be redundant next to.
  # A real (user-named) group still gets the full "^{(\text{name},1)}"
  # form, since there the name is the only thing distinguishing which
  # group a copy belongs to.
  items_superscript <- function(nm_f, group_name, lev) {
    is_solo <- length(nm_f) == 1 && identical(plain_p_name(group_name), plain_p_name(nm_f))
    if (is_solo) paste0(nm_f, "^{(", lev, ")}")
    else paste0(nm_f, "^{(\\text{", group_name, "},", lev, ")}")
  }
  build_items_h_core <- function(h_base, factors, param_factors, substitutable = FALSE, factor_types = NULL) {
    h_num   <- q2d(h_base)
    p_names <- colnames(h_base)[-(1:2)]
    n_p     <- length(p_names)
    F       <- length(factors)
    f_ns    <- vapply(factors, function(f) f$n, integer(1))   # levels per factor

    pf <- as.integer(param_factors[p_names])
    pf[is.na(pf)] <- 0L

    params_of <- lapply(seq_len(F), function(f) which(pf == f))  # orig indices per factor
    shared_pi <- which(pf == 0L)
    n_sh      <- length(shared_pi)
    n_pf      <- vapply(params_of, length, integer(1))

    # Column layout: [h1,h2 | shared | f1_lev1..levN1 | f2_lev1..levN2 | ...]
    tc <- 2L + n_sh + sum(n_pf * f_ns)
    fbase <- integer(F)                   # column start offset (after shared) per factor
    acc   <- 0L
    for (f in seq_len(F)) { fbase[f] <- acc; acc <- acc + n_pf[f] * f_ns[f] }

    col_sh <- function(li)         2L + li
    col_f  <- function(f, lev, li) 2L + n_sh + fbase[f] + (lev - 1L) * n_pf[f] + li

    all_rows <- list()

    for (ri in seq_len(nrow(h_num))) {
      a_vec <- h_num[ri, 3:(2L + n_p)]

      # Which factors have nonzero coefficients here, and which original param indices?
      nz_by_f <- lapply(seq_len(F), function(f) {
        pi <- params_of[[f]]; pi[a_vec[pi] != 0]
      })
      involved <- which(vapply(nz_by_f, length, integer(1)) > 0)
      nz_sh    <- shared_pi[a_vec[shared_pi] != 0]

      if (length(involved) == 0) {
        # constant or shared-only row
        row <- numeric(tc); row[1] <- h_num[ri, 1]; row[2] <- h_num[ri, 2]
        for (si in seq_along(nz_sh)) {
          li <- which(shared_pi == nz_sh[si])
          row[col_sh(li)] <- row[col_sh(li)] + a_vec[nz_sh[si]]
        }
        all_rows[[length(all_rows) + 1L]] <- row; next
      }

      # "average"-type factors don't duplicate the row per level like
      # joint/substitutable do — they want the ORIGINAL constraint to
      # hold for the MEAN across a member's copies, i.e. one row with
      # that member's original coefficient split evenly (a/n_f) across
      # all n_f of its level-columns, summed into a single column-value
      # (rather than the coefficient repeated whole in each of n_f
      # separate rows). That's simple enough to fold directly into a
      # base row template used by every combo below, with no grid/combo
      # step of its own — so these factors are excluded from `grids`
      # entirely, never taking part in the Cartesian product.
      is_avg <- function(f) !is.null(factor_types) && identical(factor_types[f], "average")
      avg_involved  <- involved[vapply(involved, is_avg, logical(1))]
      grid_involved <- involved[!vapply(involved, is_avg, logical(1))]

      base_row <- numeric(tc); base_row[1] <- h_num[ri, 1]; base_row[2] <- h_num[ri, 2]
      for (si in seq_along(nz_sh)) {
        li <- which(shared_pi == nz_sh[si])
        base_row[col_sh(li)] <- base_row[col_sh(li)] + a_vec[nz_sh[si]]
      }
      for (f in avg_involved) {
        nz  <- nz_by_f[[f]]
        n_f <- f_ns[f]
        for (slot in seq_along(nz)) {
          li <- which(params_of[[f]] == nz[slot])
          w  <- a_vec[nz[slot]] / n_f
          for (lev in seq_len(n_f)) {
            col <- col_f(f, lev, li)
            base_row[col] <- base_row[col] + w
          }
        }
      }

      if (length(grid_involved) == 0) {
        all_rows[[length(all_rows) + 1L]] <- base_row; next
      }

      # Build one level-assignment grid per remaining (joint/substitutable) factor
      grids <- lapply(grid_involved, function(f) {
        nz  <- nz_by_f[[f]]
        n_f <- f_ns[f]
        f_is_substitutable <- if (!is.null(factor_types)) identical(factor_types[f], "substitutable") else substitutable
        if (f_is_substitutable) {
          as.matrix(expand.grid(rep(list(seq_len(n_f)), length(nz)), KEEP.OUT.ATTRS = FALSE))
        } else {
          # fully crossed joint: same level for every slot of this factor
          matrix(rep(seq_len(n_f), each = length(nz)), ncol = length(nz), byrow = TRUE)
        }
      })

      # Cartesian product across the remaining factors' grid rows
      combo <- as.matrix(expand.grid(
        lapply(grids, function(g) seq_len(nrow(g))), KEEP.OUT.ATTRS = FALSE))

      for (ci in seq_len(nrow(combo))) {
        row <- base_row
        for (fi in seq_along(grid_involved)) {
          f   <- grid_involved[fi]
          nz  <- nz_by_f[[f]]
          g   <- grids[[fi]]
          gi  <- combo[ci, fi]
          for (slot in seq_along(nz)) {
            li  <- which(params_of[[f]] == nz[slot])
            lev <- g[gi, slot]
            col <- col_f(f, lev, li)
            row[col] <- row[col] + a_vec[nz[slot]]
          }
        }
        all_rows[[length(all_rows) + 1L]] <- row
      }
    }

    # "average" factors fold h_base's OWN rows into one averaged
    # constraint per row (see above) — but h_base's rows also include
    # the ordinary 0<=x<=1 domain bounds every probability needs, and
    # averaging THOSE away too would leave each individual copy's own
    # validity as a probability unconstrained: only the copies' AVERAGE
    # would stay pinned to [0,1], while an individual copy could drift
    # arbitrarily far outside it (confirmed directly — scdd() on the
    # resulting H-rep produces real vertices with a lone copy at 2, not
    # just a theoretical possibility). Re-add that per-copy domain bound
    # explicitly for every column of an "average" factor, regardless of
    # what happened to h_base's own bound rows; harmless/redundant()
    # strips it away for joint/substitutable columns, which already get
    # it for free by duplicating h_base's own bound rows unchanged.
    for (f in seq_len(F)) {
      if (is.null(factor_types) || !identical(factor_types[f], "average")) next
      for (li in seq_len(n_pf[f])) {
        for (lev in seq_len(f_ns[f])) {
          col <- col_f(f, lev, li)
          up <- numeric(tc); up[2] <- 1; up[col] <- -1  # copy <= 1
          lo <- numeric(tc); lo[2] <- 0; lo[col] <-  1  # copy >= 0
          all_rows[[length(all_rows) + 1L]] <- up
          all_rows[[length(all_rows) + 1L]] <- lo
        }
      }
    }

    mat <- do.call(rbind, all_rows)
    # Attach colnames using the exact same pf/params_of/shared_pi computed above
    cn_shared  <- if (n_sh > 0) p_names[shared_pi] else character(0)
    cn_factors <- unlist(lapply(seq_len(F), function(f) {
      nm_f <- p_names[params_of[[f]]]
      unlist(lapply(seq_len(f_ns[f]), function(lev) items_superscript(nm_f, factors[[f]]$name, lev)))
    }))
    colnames(mat) <- c("", "", cn_shared, cn_factors)
    mat
  }

  # Fully general per-factor H-rep builder for the "Repeated items" modal's
  # combined mode, where each probability picks its own type. Each
  # factor's grid strategy (joint/fully-crossed,
  # substitutable/all-permutations, or average/one-row-with-averaged-
  # coefficients) is honored independently via build_items_h_core's
  # factor_types argument — a factor's own semantics are the real thing
  # here, not approximated via another type, and coexist correctly with
  # other factors marked differently in the very same combined model.
  # "Identical" equality rows are then layered on top for the factors
  # flagged "identical" (average factors need no such extra layer — the
  # averaging is entirely built into build_items_h_core's own row
  # construction). Each MEMBER of an
  # "identical" factor gets its own tolerance (eps_per_param, keyed by
  # probability name) rather than one tolerance for the whole factor —
  # e.g. within one group, p1's copies can be forced equal within ±0.05
  # while p2's copies (same group) tolerate ±0.1; nothing about the
  # underlying equality rows requires them to match, each member's rows
  # are independent of every other member's. This composes cleanly
  # regardless of what grid strategy other factors used, since it only
  # depends on the (grid-strategy-independent) column layout.
  build_items_h_mixed <- function(h_base, factors, param_factors, factor_types, eps_per_param) {
    p_names <- colnames(h_base)[-(1:2)]
    F       <- length(factors)
    f_ns    <- vapply(factors, function(f) f$n, integer(1))
    pf      <- as.integer(param_factors[p_names]); pf[is.na(pf)] <- 0L
    n_pf    <- vapply(seq_len(F), function(f) sum(pf == f), integer(1))
    n_sh    <- sum(pf == 0L)
    tc      <- 2L + n_sh + sum(n_pf * f_ns)
    fbase   <- integer(F); acc <- 0L
    for (f in seq_len(F)) { fbase[f] <- acc; acc <- acc + n_pf[f] * f_ns[f] }
    col_f   <- function(f, lev, li) 2L + n_sh + fbase[f] + (lev - 1L) * n_pf[f] + li
    jh      <- build_items_h_core(h_base, factors, param_factors, factor_types = factor_types)
    eq_rows <- list()
    for (f in seq_len(F)) {
      if (!identical(factor_types[f], "identical")) next
      if (f_ns[f] < 2 || n_pf[f] == 0) next
      members_f <- p_names[pf == f]
      for (lev in 2:f_ns[f]) {
        for (li in seq_len(n_pf[f])) {
          eps <- if (!is.null(eps_per_param) && !is.na(eps_per_param[members_f[li]])) eps_per_param[members_f[li]] else 0
          if (eps == 0) {
            row <- numeric(tc); row[1] <- 1
            row[col_f(f, 1L,  li)] <-  1
            row[col_f(f, lev, li)] <- -1
            eq_rows[[length(eq_rows) + 1L]] <- row
          } else {
            row_a <- numeric(tc); row_a[2] <- eps
            row_a[col_f(f, 1L, li)] <- 1; row_a[col_f(f, lev, li)] <- -1
            row_b <- numeric(tc); row_b[2] <- eps
            row_b[col_f(f, 1L, li)] <- -1; row_b[col_f(f, lev, li)] <- 1
            eq_rows[[length(eq_rows) + 1L]] <- row_a
            eq_rows[[length(eq_rows) + 1L]] <- row_b
          }
        }
      }
    }
    if (length(eq_rows) > 0) {
      mat <- rbind(jh, do.call(rbind, eq_rows))
      colnames(mat) <- colnames(jh)
      mat
    } else jh
  }

  # Renders an already-computed items H-representation matrix as plain
  # ">/</=" constraint text using fresh, sequential p1, p2, ... names —
  # "what you would have had to type by hand, in a brand-new model with
  # exactly this many parameters, to get this same result" (direct user
  # request: an actual preview of the generated input, not just the
  # human-readable "N copies of X, linked together" description). Column
  # ORDER (not the real, item-tagged names like p1^{(A,1)}) becomes the
  # new p-numbering, so two items models with the same shape always
  # relabel identically. Reuses the same "move negative terms/constants
  # to the other side so nothing prints with a leading minus" convention
  # already used for the real H-representation table display, just as
  # plain text (";"-joined, no LaTeX/HTML) instead of an HTML table.
  h_matrix_to_plain_spec <- function(h_mat) {
    if (is.null(h_mat) || nrow(h_mat) == 0) return("")
    # build_items_h_core/build_items_h_mixed (this function's only caller)
    # work in and return plain numeric throughout, unlike most of this
    # app's other H-representation matrices, which stay in rcdd's
    # character/rational form until explicitly q2d()'d for display —
    # q2d() on an already-numeric matrix errors ("argument must be
    # character"), so only convert when it's actually still character.
    h_num <- if (is.character(h_mat)) q2d(h_mat) else h_mat
    n_p   <- ncol(h_num) - 2L
    new_names <- paste0("p", seq_len(n_p))
    # Skip the automatic "keep this probability inside [0,1]" bounds
    # (every model gets these added for free — see extract_info's own
    # `limits`) — they were never something you'd actually type, so
    # including them here would make this look unlike real input. Same
    # single-nonzero-unit-coefficient test already used for trivial_vec
    # in the real H-representation table.
    is_trivial <- function(coeffs, rhs) {
      nz <- which(coeffs != 0)
      length(nz) == 1 && ((coeffs[nz] == 1 && rhs == 1) || (coeffs[nz] == -1 && rhs == 0))
    }
    # Term/operator syntax matching what you'd actually type (see the
    # "Write linear in/equalities..." help text): bare "<"/">" — this
    # function's rows are always the "<=" convention (never flipped, see
    # the real table's own comment on that), so plain "<" — a coefficient
    # of 1 needs no "*", anything else uses "coef*pN" as documented, and
    # no leading "+" on a term (only between terms, where it's the actual
    # addition operator, not a sign).
    term_str <- function(coef, name) {
      if (coef == 1) name else paste0(num2str_lin(coef), "*", name)
    }
    join_terms <- function(const, terms) {
      parts <- c(if (const > 0) num2str_lin(const) else character(0), terms)
      if (length(parts) == 0) return("0")
      paste(parts, collapse = "+")
    }
    rows_txt <- character(0)
    for (ri in seq_len(nrow(h_num))) {
      is_eq  <- h_num[ri, 1] == 1
      rhs    <- h_num[ri, 2]
      coeffs <- -h_num[ri, 3:(2L + n_p)]
      if (!is_eq && is_trivial(coeffs, rhs)) next
      nz_all  <- which(coeffs != 0)
      neg_all <- nz_all[coeffs[nz_all] < 0]
      lhs_terms <- vapply(setdiff(nz_all, neg_all), function(j)
        term_str(coeffs[j], new_names[j]), character(1))
      moved_terms <- vapply(neg_all, function(j)
        term_str(abs(coeffs[j]), new_names[j]), character(1))
      lhs_const <- if (rhs < 0) abs(rhs) else 0
      rhs_const <- if (rhs > 0) rhs else 0
      lhs_str <- join_terms(lhs_const, lhs_terms)
      rhs_str <- join_terms(rhs_const, moved_terms)
      rows_txt <- c(rows_txt, paste0(lhs_str, if (is_eq) "=" else "<", rhs_str))
    }
    paste(rows_txt, collapse = "; ")
  }

  # Plain-English legend for what each fresh p1, p2, ... in
  # h_matrix_to_plain_spec's output actually is — which original
  # probability it came from, and (for a repeated one) which group/item
  # it stands for. Column ORDER here must match build_items_h_core's own
  # column construction exactly (shared params first, then each factor's
  # columns level-major/parameter-minor) since h_matrix_to_plain_spec's
  # p1, p2, ... are numbered by that same column order.
  items_column_legend <- function(h_base, factors, param_factors) {
    p_names <- colnames(h_base)[-(1:2)]
    F  <- length(factors)
    pf <- as.integer(param_factors[p_names]); pf[is.na(pf)] <- 0L
    # A probability's display name is only guaranteed non-blank when its
    # Name field was never touched at all (extract_info's own name_p
    # falls back to "p_{i}" only when the input is entirely absent, i.e.
    # length 0) — a Name field cleared to empty text instead leaves
    # colnames(h_base) with a literal "" for that column, which
    # plain_p_name() then passes straight through, producing a legend
    # line with nothing before "(not repeated)"/"(group ...)". Fall back
    # to that column's own position ("param3") whenever its real name is
    # blank, so the legend always names something concrete.
    label_of <- function(nm, idx) {
      pn <- plain_p_name(nm)
      ifelse(nzchar(trimws(pn)), pn, paste0("param", idx))
    }
    # paste0(character(0), "x") does NOT return character(0) — it returns
    # "x" (length 1, missing part just blank), unlike most vectorized R
    # functions. With zero shared parameters (shared_idx empty, the
    # common/correct case whenever every real probability is grouped),
    # the naive paste0() below still produced one phantom, name-less
    # "(not repeated)" legend entry — the actual cause of the "still
    # shows an extra ungrouped parameter" report, confirmed via debug
    # output showing param_factors correctly all-grouped (1,1,1) with no
    # zeros. Guard every paste0() here that builds FROM a which()-derived
    # index vector with an explicit length check instead of trusting
    # paste0's recycling.
    shared_idx <- which(pf == 0L)
    legend_shared <- if (length(shared_idx) > 0) {
      paste0(label_of(p_names[shared_idx], shared_idx), " (not repeated)")
    } else character(0)
    legend_factors <- unlist(lapply(seq_len(F), function(f) {
      idx_f <- which(pf == f)
      if (length(idx_f) == 0) return(character(0))
      unlist(lapply(seq_len(factors[[f]]$n), function(lev)
        paste0(label_of(p_names[idx_f], idx_f), " (group ", factors[[f]]$name, ", item ", lev, ")")))
    }))
    c(legend_shared, legend_factors)
  }

  # Helper: build expanded colnames from factors + param_factors
  items_colnames <- function(h_base, factors, param_factors) {
    p_names <- colnames(h_base)[-(1:2)]
    F       <- length(factors)
    pf      <- as.integer(param_factors[p_names]); pf[is.na(pf)] <- 0L
    shared_nm <- p_names[pf == 0L]
    exp_nm <- unlist(lapply(seq_len(F), function(f) {
      nm_f <- p_names[pf == f]
      unlist(lapply(seq_len(factors[[f]]$n), function(lev)
        items_superscript(nm_f, factors[[f]]$name, lev)))
    }))
    c("", "", shared_nm, exp_nm)
  }

  get_name_models_reactive <- function() {
    nms <- character()
    for (ls in seq_len(counter_input$n)) {
      nms <- c(nms, isolate(input[[paste0("textin_relations_name", ls)]]))
    }
    nms[!is.na(nms) & nms != ""]
  }

  # All currently computed models: base + items
  get_all_computed_models <- function() {
    c(get_name_models_reactive(),
      isolate(items_reactive$names))
  }

  model_type <- function(nm) {
    if (nm %in% isolate(items_reactive$names))        return("multi-item")
    return("base")  # base models, intersections, and mixtures all share the base parameter space
  }

  check_combinable <- function(sel) {
    if (length(sel) < 2) return(NULL)
    types <- vapply(sel, model_type, character(1))
    if (length(unique(types)) == 1) return(NULL)
    paste0("Cannot combine models of different types (",
      paste(paste0(sel, " (", types, ")"), collapse = ", "),
      "). Only base models, or only replication models, or only multi-item models can be combined — their dimensions represent different parameters.")
  }

  # Given the models already selected in an intersection/mixture picker row,
  # which of the remaining `choices` would be incompatible if added — either
  # a different model type (see model_type) or a different parameter count.
  # Used to grey those out client-side (see the 'greyOutChoices' custom
  # message handler) instead of only warning about it after the fact.
  disabled_choices_for <- function(sel, choices) {
    candidates <- setdiff(choices, sel)
    if (length(sel) == 0 || length(candidates) == 0) return(character(0))
    sel_types <- unique(vapply(sel, model_type, character(1)))
    ncols_sel <- vapply(sel, function(m) {
      h <- isolate(h_reactive[[m]])
      if (is.null(h)) NA_integer_ else ncol(h)
    }, integer(1))
    ncols_sel <- ncols_sel[!is.na(ncols_sel)]
    incompatible <- vapply(candidates, function(cand) {
      if (length(sel_types) == 1 && model_type(cand) != sel_types[1]) return(TRUE)
      if (length(ncols_sel) == 0) return(FALSE)
      h_cand <- isolate(h_reactive[[cand]])
      if (is.null(h_cand)) return(FALSE)
      ncol(h_cand) != ncols_sel[1]
    }, logical(1))
    candidates[incompatible]
  }

  # Probability names are typically typed as LaTeX source (e.g. "p_{1}",
  # rendered as a proper subscript via MathJax wherever the app actually
  # typesets math — the H-representation tables, mostly). A generated
  # MODEL name/description is plain text, though (a textAreaInput value,
  # not MathJax'd), so embedding the raw LaTeX source there just shows
  # the literal underscores and braces instead of anything nicer. Strip
  # them for display purposes only — the raw name is still what's passed
  # into build_items_h_core for column-name generation, where it DOES
  # get properly MathJax-rendered.
  plain_p_name <- function(nm) gsub("[_{}]", "", nm)

  # Compact-but-readable per-factor tag for an items-model's auto-
  # generated name/description, e.g. "p1x3" (joint, the common case,
  # gets no suffix at all), "p1x3 subst" (substitutable) or
  # "p1x3 ident" / "p1x3 ident±0.05" (identical, exact or with a
  # tolerance). The old spelled-out "p_{1}=3 joint" form got unreadably
  # long fast once a model had more than one or two expanded
  # probabilities, but a first attempt at shortening it (× ~ ≡ symbols
  # with no legend anywhere) just traded "too long" for "cryptic" —
  # short plain-English words are the middle ground. Used identically
  # everywhere a name/description is built from a factor
  # (items_spec_names, submit_items_multi, and the Go pipeline's own
  # copy) so they keep matching.
  # member_eps: one tolerance per MEMBER of this factor (not one for the
  # whole factor — see build_items_h_mixed's own comment), in the same
  # order as the factor's members. Uniform across members (the common
  # case) collapses to the old single "same±0.05" tag; members that
  # differ are spelled out so two specs that differ only in a member's
  # tolerance still get distinct cache keys/display names.
  items_factor_tag <- function(name, n, type, member_eps = 0) {
    # joint is the common case and gets no suffix at all (see this
    # function's own header comment) — everything else does.
    # "substitutable" was abbreviated "swap" here, diverging from the
    # header comment's own documented "subst" and from what the word
    # actually means in this app (every level tried against every
    # other, not two things trading places) — reported directly as
    # confusing ("p4:2 swap? What does swap mean?").
    kind <- switch(type,
      joint = "",
      substitutable = "subst",
      average = "avg",
      identical = {
        ev <- member_eps
        if (length(ev) == 0 || all(ev == 0)) "same"
        else if (length(unique(round(ev, 10))) == 1) paste0("same±", frac_str(ev[1]))
        else paste0("same±", paste(vapply(ev, frac_str, character(1)), collapse = ","))
      }
    )
    paste0(plain_p_name(name), ":", n, if (nzchar(kind)) paste0(" ", kind) else "")
  }

  # Items model names derived from specs — available before Parsimony runs
  # Name(s) a single items spec produces — kept in one place since both
  # get_pending_items_names() and the model-row delete handler (which
  # needs to find and drop the spec matching a just-deleted row, so it
  # doesn't silently reappear on the next Go — see delete_model_idx's own
  # comment) must compute the EXACT same name(s) the Go pipeline's
  # "Multiple-items models" block would.
  # Member tolerances for factor `fi`, in that factor's own member order
  # (matching how build_items_h_mixed iterates them) — eps_per_param is a
  # named vector keyed by probability name, so this just looks each
  # member up by name; missing/NULL reads as 0 (no tolerance).
  member_eps_for_factor <- function(param_factors, eps_per_param, fi) {
    members <- names(param_factors)[param_factors == fi]
    if (is.null(eps_per_param)) return(rep(0, length(members)))
    vals <- unname(eps_per_param[members])
    vals[is.na(vals)] <- 0
    vals
  }

  items_spec_names <- function(spec) {
    base_m  <- spec$base
    factors <- spec$factors
    if (is.null(base_m) || nchar(base_m) == 0 || length(factors) == 0) return(character(0))
    factor_tag <- paste(vapply(seq_along(factors), function(fi)
      items_factor_tag(factors[[fi]]$name, factors[[fi]]$n, spec$factor_types[fi],
        member_eps_for_factor(spec$param_factors, spec$eps_per_param, fi)),
      character(1)), collapse = ", ")
    paste0(base_m, " [", factor_tag, "]")
  }

  get_pending_items_names <- function() {
    specs <- isolate(mytable_items_reactive$value)
    if (is.null(specs) || length(specs) == 0) return(character(0))
    unique(unlist(lapply(specs, items_spec_names)))
  }

  # User selection of WHICH model pairs to intersect. The chosen pairs are the
  # only intersections shown anywhere (H-representation cards, downloads,
  # polytope overlap highlights and the overlap matrix).
  mytable_int_reactive <- reactiveValues(value = NULL)

  # How many intersections the user is defining (one column per intersection
  # in the "Model intersections" modal).
  counter_int <- reactiveValues(n = 1)

  # Whether the detailed per-algorithm parsimony table should default open —
  # only when it contains a model that ISN'T shown in any comparison table
  # (so the info wouldn't otherwise be visible anywhere).
  parsim_needs_open <- reactiveValues(value = FALSE)

  # The Parsimony tab's results (header/plot/table) only exist once
  # observeEvent(input$go, ...) has actually run — before that first
  # "Compute parsimony" click, their uiOutput()s are simply empty and the
  # whole tab reads as a blank page. This flag drives a placeholder (see
  # output$parsimony_placeholder_ui) that tells the user what to do
  # instead, and disappears the moment real results exist.
  parsim_has_results <- reactiveVal(FALSE)

  output$parsimony_placeholder_ui <- renderUI({
    if (isTRUE(parsim_has_results())) return(NULL)
    # Deliberately NOT class="model-card" — that class's ::after hover
    # rule adds a "click to expand" hint to every card, meant for the
    # real double-click-to-fullscreen cards elsewhere; this placeholder
    # isn't one of those; it doesn't do anything on a click, so hinting
    # that it does would just be misleading. Plain equivalent styling
    # instead, and a real icon() instead of an emoji glyph.
    div(
      style = paste(
        "text-align:center; padding:28px 20px; color:#777;",
        "background-color: var(--fairy-panel); border: 1px solid var(--fairy-border);",
        "border-radius: 12px; box-shadow: 0 1px 3px rgba(16, 24, 40, 0.06);"
      ),
      icon("chart-column", style = "font-size:28px; color:var(--fairy-text-muted);"),
      p(style = "margin:10px 0 0 0; font-size:13.5px;",
        "No results yet. Hover the floating panel — Cooling Bodies is preselected — and click ",
        strong("Go"), " (or press ⌘/Ctrl+Enter) to see volumes here."
      )
    )
  })

  # "Multiple items" (see the button next to the Probabilities header):
  # ONE modal covering the whole set of probabilities at once, rather than
  # a separate icon/modal per p — the base model is chosen once, and each
  # probability gets its own row (items count, joint/substitutable/
  # identical, approx tolerance) so a single submit can expand several
  # probabilities together into one combined model.
  #
  # Also reused to EDIT an existing items model (see the small icon on
  # its row, wired below) — active_items_edit() records which row/spec is
  # being edited so submit_items_multi knows to update it in place rather
  # than add a new one, and prefill_base/prefill_rows pre-populate the
  # same modal from that spec's current values instead of the all-1s/
  # all-Joint defaults a brand new one starts from.
  active_items_edit <- reactiveVal(NULL)
  item_model_spec_idx <- reactiveValues(value = list())

  open_items_modal <- function(prefill_base = NULL, prefill_rows = NULL) {
    model_choices <- get_name_models_reactive()
    if (length(model_choices) == 0) {
      shinyalert("No models", "Define at least one model first.",
        type = "info", closeOnClickOutside = TRUE)
      return()
    }
    n_p_all <- isolate(counter$n)
    p_names <- vapply(seq_len(n_p_all), function(i) {
      nm <- trimws(isolate(input[[paste0("textin_name_", i)]]) %||% "")
      if (nzchar(nm)) nm else paste0("p", i)
    }, character(1))
    is_edit <- !is.null(prefill_rows)

    showModal(modalDialog(
      title = NULL,
      size = "m",
      class = "fairy-eq-tol-modal",
      tags$div(class = "fairy-eq-tol-header", tags$strong(if (is_edit) "Edit repeated items" else "Repeated items")),
      tags$p(class = "fairy-eq-tol-help",
        "Expand any of these probabilities across several items — e.g. one parameter per stimulus instead of a single shared value. Leave a probability's items at 1 to keep it as-is."
      ),
      selectInput("items_base_model", "Base model", choices = model_choices,
        selected = prefill_base %||% model_choices[1], width = "100%"),
      # Each row is its OWN grid container rather than one flat grid for
      # header + all rows — conditionalPanel hides the tolerance cell via
      # display:none, and a display:none item drops out of grid flow
      # entirely (not just invisibly), which shifted every following
      # row's cells left by one column when a flat shared grid was used.
      # A row-local grid isn't affected by what a DIFFERENT row's grid
      # does with its own (independent) 4th column.
      #
      tags$details(style = "margin-top:2px;",
        tags$summary(style = "cursor:pointer;font-size:12.5px;color:var(--fairy-primary);font-weight:600;user-select:none;",
          HTML("&#9432; What do Group and Type do? <span style='font-size:11px;font-weight:normal;color:var(--fairy-text-muted);'>(click to expand)</span>")
        ),
        tags$div(style = "margin-top:8px;",
          tags$p(class = "fairy-eq-tol-help", style = "margin-top:2px;background:var(--ct-diag);padding:8px 10px;border-radius:6px;",
            HTML("<b>Short version:</b> Type only changes anything once two or more probabilities share the <i>same</i> Group. Left ungrouped (the default), probabilities are always compared against every combination of each other's items no matter what Type says below — Joint vs Substitutable only matters once you actually group them together. See the worked example at the bottom.")
          ),
          tags$p(class = "fairy-eq-tol-help", style = "margin-top:8px;",
            HTML("<b>Group</b> — type the exact same word into two or more probabilities' Group boxes to combine them into one shared item count. Leave blank for a probability to keep its own, independent item count (this is also what \"ungrouped\" in the box means).")
          ),
          tags$p(class = "fairy-eq-tol-help", style = "margin-top:2px;",
            HTML("<b>Type</b> — for probabilities placed in the <i>same</i> Group, how their items line up.<br><b>Joint</b>: paired one-to-one — item 1 with item 1, item 2 with item 2. This is the one Type that actually changes anything compared to leaving probabilities ungrouped.<br><b>Substitutable</b>: every item against every other — the exact same result you already get by leaving probabilities ungrouped, just now sharing one name/one item count instead of each having its own.<br><b>Identical</b>: like Joint, but the grouped probabilities' items must additionally be forced equal in value. Set Approx. tol. above 0 to allow a small deviation instead of exact equality.<br><b>Average</b>: items stay individually free, but the original condition applies to their <i>average</i> instead of to every item on its own — e.g. \"p1 &gt; .5\" becomes \"the average of p1's items &gt; .5\", not \"every one of p1's items &gt; .5\".")
          ),
          tags$p(class = "fairy-eq-tol-help", style = "margin-top:8px;",
            HTML("<b>Worked example</b> — p1 and p2 each given 2 items, condition \"p1 &gt; p2\":<br>&bull; Left ungrouped (any Type): 4 comparisons — every item of p1 against every item of p2.<br>&bull; Grouped, Type = Substitutable: the same 4 comparisons — grouping did not change anything here.<br>&bull; Grouped, Type = Joint: only 2 comparisons — item 1 of p1 against item 1 of p2, item 2 against item 2. This is the case where grouping actually matters.")
          )
        )
      ),
      tags$div(
        id = "fairy-items-header-row",
        style = "display:grid; grid-template-columns: 1.6fr 1.6fr 1fr 1.6fr 1.4fr; gap:6px 10px; align-items:center; margin-top:6px;",
        tags$strong("Probability", style = "font-size:11.5px; color:var(--fairy-text-muted);"),
        tags$strong("Group", class = "fairy-items-group-cell", style = "font-size:11.5px; color:var(--fairy-text-muted);"),
        tags$strong("Items", style = "font-size:11.5px; color:var(--fairy-text-muted);"),
        tags$strong("Type", style = "font-size:11.5px; color:var(--fairy-text-muted);"),
        tags$strong("Approx. tol.", style = "font-size:11.5px; color:var(--fairy-text-muted);")
      ),
      lapply(seq_along(p_names), function(i) {
        pf <- prefill_rows[[p_names[i]]]
        # Group/Type/tolerance are all meaningless (no-ops) while this
        # row's own Items is still 1 — nothing to group, substitute, or
        # treat as identical yet. Greyed out and actually disabled until
        # Items > 1 (see the delegated JS below), rather than sitting
        # there editable but inert.
        tags$div(
          class = "fairy-items-row", style = "display:grid; grid-template-columns: 1.6fr 1.6fr 1fr 1.6fr 1.4fr; gap:6px 10px; align-items:center; margin-top:4px;",
          tags$span(p_names[i], style = "font-size:13px;"),
          tags$div(class = "fairy-items-group-cell",
            textInput(paste0("items_multi_group_", i), NULL, value = pf$group %||% "", placeholder = "ungrouped", width = "100%")
          ),
          numericInput(paste0("items_multi_n_", i), NULL, value = pf$n %||% 1, min = 1, max = 50, step = 1, width = "100%"),
          selectInput(paste0("items_multi_type_", i), NULL,
            choices = c("Joint" = "joint", "Substitutable" = "substitutable", "Identical" = "identical", "Average" = "average"),
            selected = pf$type %||% "joint", width = "100%"),
          conditionalPanel(
            condition = paste0("input.items_multi_type_", i, " == 'identical'"),
            numericInput(paste0("items_multi_eps_", i), NULL, value = pf$eps %||% 0, min = 0, max = 1, step = 0.01, width = "100%")
          )
        )
      }),
      tags$script(HTML(
        "
        (function() {
          function updateItemsRow(row) {
            // Tag-qualified (input[...]/select[...]), not just
            // [id^=...] — Shiny gives every one of these inputs a
            // sibling <label id=\"<inputId>-label\"> even when label=NULL
            // is passed, which also matches a bare [id^=...] attribute
            // selector and (being first in the DOM) was the one
            // querySelector actually returned — reading .value off a
            // <label> is always undefined, so n parsed as NaN -> fell
            // back to the 1/inactive default every time, regardless of
            // what was actually typed.
            var nInput = row.querySelector('input[id^=items_multi_n_]');
            var n = parseInt(nInput && nInput.value, 10) || 1;
            var active = n > 1;
            row.classList.toggle('fairy-items-row-inactive', !active);
            // Group stays editable regardless of this row's OWN Items
            // value — a grouped member inherits the group's item count
            // from whichever member actually has one set (see
            // submit_items_multi's own comment), so gating this on
            // n>1 made it impossible to even TYPE a group name into a
            // probability before first bumping its own Items to match,
            // which read as 'grouping it does nothing' (direct user
            // report: a probability assigned to a group still counted
            // as its own separate single-item factor).
            var typeSelect = row.querySelector('select[id^=items_multi_type_]');
            if (typeSelect) {
              typeSelect.disabled = !active;
              // selectInput's default selectize=TRUE wraps the real
              // <select> (now disabled) in its own styled UI, which
              // does not pick up the native disabled state on its own.
              if (typeSelect.selectize) {
                if (active) typeSelect.selectize.enable(); else typeSelect.selectize.disable();
              }
            }
            var eps = row.querySelector('input[id^=items_multi_eps_]');
            if (eps) eps.disabled = !active;
          }
          function updateAllItemsRows() {
            document.querySelectorAll('.fairy-items-row').forEach(updateItemsRow);
          }
          // Rows sharing a non-empty Group must always show the SAME
          // Items count, Type, and (when Type is Identical) Approx. tol.
          // -- all three are properties of the GROUP/factor, not of each
          // individual probability (see the modal's own Group/Type help
          // text: they share one item count and how the items inside a
          // group line up) -- sync them live as you type, rather than
          // only forcing agreement at Submit time, which let the fields
          // visibly disagree while the modal was still open.
          function syncGroupItems(changedRow, changedField) {
            var groups = {};
            document.querySelectorAll('.fairy-items-row').forEach(function(row) {
              var gInput = row.querySelector('input[id^=items_multi_group_]');
              var g = gInput && gInput.value.trim();
              if (!g) return;
              (groups[g] = groups[g] || []).push(row);
            });
            Object.keys(groups).forEach(function(g) {
              var rows = groups[g];
              if (rows.length < 2) return;
              var isChangedGroup = changedRow && rows.indexOf(changedRow) > -1;

              // Items: the actively-edited row wins ties; otherwise the
              // largest value currently set in the group.
              var targetN = null;
              if (isChangedGroup && changedField === 'n') {
                var cInput = changedRow.querySelector('input[id^=items_multi_n_]');
                targetN = parseInt(cInput && cInput.value, 10) || null;
              }
              if (!targetN) {
                rows.forEach(function(row) {
                  var nInput = row.querySelector('input[id^=items_multi_n_]');
                  var n = parseInt(nInput && nInput.value, 10) || 1;
                  if (targetN === null || n > targetN) targetN = n;
                });
              }
              rows.forEach(function(row) {
                var nInput = row.querySelector('input[id^=items_multi_n_]');
                if (nInput && parseInt(nInput.value, 10) !== targetN) {
                  nInput.value = targetN;
                  $(nInput).trigger('change');
                }
              });

              // Type: the actively-edited row wins; otherwise the first
              // row's current selection.
              var targetType = null;
              if (isChangedGroup && changedField === 'type') {
                var cSel = changedRow.querySelector('select[id^=items_multi_type_]');
                targetType = cSel && cSel.value;
              }
              if (!targetType) {
                var firstSel = rows[0].querySelector('select[id^=items_multi_type_]');
                targetType = firstSel && firstSel.value;
              }
              if (targetType) {
                rows.forEach(function(row) {
                  var sel = row.querySelector('select[id^=items_multi_type_]');
                  if (!sel || sel.value === targetType) return;
                  // selectize.js replaces the real <select> with its own
                  // styled dropdown UI and owns rendering it — setting
                  // sel.value directly changes the hidden native element
                  // but leaves the visible styled control (and selectize's
                  // own idea of the current value) stale/blank. Go through
                  // its own API instead so both the native select AND the
                  // visible UI end up in sync; selectize.setValue already
                  // updates the native select and fires Shiny's change
                  // binding on its own, non-silent so Shiny actually picks
                  // it up.
                  if (sel.selectize) sel.selectize.setValue(targetType);
                  else { sel.value = targetType; $(sel).trigger('change'); }
                });
              }

              // Approx. tol. is NOT synced across group members — unlike
              // Items and Type, it's genuinely a per-MEMBER setting (see
              // build_items_h_mixed's own comment): within one group,
              // p1's copies can be forced equal within a different
              // tolerance than p2's copies. Each row's own eps field is
              // left exactly as typed.

              rows.forEach(updateItemsRow);
            });
          }
          $(document).on('shown.bs.modal', updateAllItemsRows);
          $(document).on('shown.bs.modal', function() { syncGroupItems(null); });
          $(document).on('input change', '.fairy-items-row [id^=items_multi_n_]', function() {
            var row = this.closest('.fairy-items-row');
            updateItemsRow(row);
            syncGroupItems(row, 'n');
          });
          $(document).on('input change', '.fairy-items-row [id^=items_multi_group_]', function() {
            syncGroupItems(this.closest('.fairy-items-row'), 'group');
          });
          $(document).on('change', '.fairy-items-row [id^=items_multi_type_]', function() {
            syncGroupItems(this.closest('.fairy-items-row'), 'type');
          });
          // Selectize finishes initializing asynchronously after this
          // script runs, so the very first pass (before it exists yet)
          // wouldn't find .selectize on the type dropdown — one retry
          // shortly after covers that without polling indefinitely.
          setTimeout(updateAllItemsRows, 50);
          updateAllItemsRows();
        })();
        "
      )),
      footer = tagList(
        modalButton("Cancel"),
        actionBttn("submit_items_multi", if (is_edit) "Save" else "Add", style = "material-flat", color = "primary", size = "sm")
      ),
      easyClose = TRUE, fade = TRUE
    ))
  }

  observeEvent(input$show_items_modal, {
    active_items_edit(NULL)
    open_items_modal()
  })

  # "copy_<name>" for a fresh copy, "copy_<name>_2", "_3", ... if that's
  # already taken too — used by copy_model_btn_ below so a copied row
  # never collides with an existing model name.
  unique_copy_name <- function(orig, existing) {
    base <- paste0("copy_", orig)
    if (!(base %in% existing)) return(base)
    k <- 2
    repeat {
      cand <- paste0(base, "_", k)
      if (!(cand %in% existing)) return(cand)
      k <- k + 1
    }
  }

  # Small icon on an items-model row (see the icon column render, and
  # item_model_spec_idx which flags which rows these are) — re-wired
  # whenever the model count changes, same setdiff-only-new-ids pattern
  # used throughout this file (see wired_eq_tol_ids's own comment).
  wired_items_edit_ids <- reactiveVal(integer(0))
  observeEvent(counter_input$n, {
    n <- counter_input$n
    already <- isolate(wired_items_edit_ids())
    new_ids <- setdiff(seq_len(n), already)
    for (loop_n in new_ids) {
      local({
        ii <- loop_n
        observeEvent(input[[paste0("items_edit_btn_", ii)]], {
          spec_idx <- isolate(item_model_spec_idx$value[[as.character(ii)]])
          specs <- isolate(mytable_items_reactive$value)
          if (is.null(spec_idx) || is.null(specs) || spec_idx > length(specs)) return()
          spec <- specs[[spec_idx]]
          if (is.null(spec$factor_types)) return()  # only the new spec shape is editable

          # Keyed by MEMBER probability name (not the factor's own name,
          # which for a shared-group factor is the group label, not any
          # one probability's name) — each member of a factor gets that
          # factor's n/type, its OWN eps (per-member, not per-factor —
          # see build_items_h_mixed's own comment), plus its group label
          # ONLY when the factor actually has more than one member, so a
          # plain single-probability factor's row still shows an empty
          # (own-factor) Group field rather than a redundant self-group.
          eps_pp <- spec$eps_per_param
          prefill_rows <- list()
          for (fi in seq_along(spec$factors)) {
            members <- names(spec$param_factors)[spec$param_factors == fi]
            for (m in members) {
              m_eps <- if (!is.null(eps_pp) && !is.na(eps_pp[m])) unname(eps_pp[m]) else 0
              prefill_rows[[m]] <- list(
                n = spec$factors[[fi]]$n, type = spec$factor_types[fi], eps = m_eps,
                group = if (length(members) > 1) spec$factors[[fi]]$name else ""
              )
            }
          }
          active_items_edit(list(row_idx = ii, spec_idx = spec_idx))
          open_items_modal(prefill_base = spec$base, prefill_rows = prefill_rows)
        }, ignoreInit = TRUE)

        # "Show as typed input" — computes the expansion FRESH right now
        # (not from anything cached at Add/Save time), so it can never go
        # stale and correctly reflects whether the base model has an
        # H-representation to expand yet at all.
        observeEvent(input[[paste0("items_preview_btn_", ii)]], {
          spec_idx <- isolate(item_model_spec_idx$value[[as.character(ii)]])
          specs <- isolate(mytable_items_reactive$value)
          if (is.null(spec_idx) || is.null(specs) || spec_idx > length(specs)) return()
          spec <- specs[[spec_idx]]
          if (is.null(spec$factor_types)) return()

          h_base_preview <- isolate(h_reactive[[spec$base]])
          if (is.null(h_base_preview)) {
            showModal(modalDialog(
              title = "Show as typed input",
              "This model's base (", tags$code(spec$base), ") hasn't been computed yet — click Go first, then try again.",
              easyClose = TRUE
            ))
            return()
          }

          h_preview <- tryCatch(
            build_items_h_mixed(h_base_preview, spec$factors, spec$param_factors, spec$factor_types, spec$eps_per_param),
            error = function(e) NULL
          )
          if (is.null(h_preview)) {
            showModal(modalDialog(title = "Show as typed input",
              "Could not compute a preview for this model.", easyClose = TRUE))
            return()
          }

          preview_text <- h_matrix_to_plain_spec(h_preview)
          legend <- items_column_legend(h_base_preview, spec$factors, spec$param_factors)
          legend_rows <- lapply(seq_along(legend), function(j)
            tags$li(tags$code(paste0("p", j)), " = ", legend[j]))

          showModal(modalDialog(
            title = "Show as typed input",
            size = "m",
            tags$p(class = "fairy-eq-tol-help",
              "Exactly what you'd get typing a fresh model with this many probabilities, instead of using Repeated items:"),
            # A real, readonly, click-to-select-all textarea — copy/paste
            # friendly, unlike a plain <pre> block — this is meant to be
            # pasted straight into a Unique Model Specification box.
            tags$textarea(
              readonly = "readonly", rows = "3",
              style = "width:100%; font-family:ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; font-size:13px; background:var(--ct-default); border:1px solid var(--fairy-border); border-radius:6px; padding:8px 12px; resize:vertical;",
              onclick = "this.select();",
              preview_text
            ),
            tags$p(class = "fairy-eq-tol-help", style = "margin-top:14px;", tags$b("What each probability is:")),
            tags$ul(style = "font-size:13px; line-height:1.6; padding-left:18px;", legend_rows),
            easyClose = TRUE, fade = TRUE
          ))
        }, ignoreInit = TRUE)

        # Duplicates this row into a brand-new model row, so it can be
        # tweaked into a variant without touching the original. Two
        # cases: a repeated-items row's underlying spec gets its own
        # independent copy (its own entry appended to
        # mytable_items_reactive$value, not shared with the source row —
        # editing one afterward via items_edit_btn_ never affects the
        # other, even though right after copying they're the same model
        # until one is actually changed); a plain typed row's own Unique
        # Model Specification text is copied as-is. Not shown for
        # intersection/mixture rows (see the UI side's own comment) — see
        # observeEvent(input$submit_items_multi, ...)'s own "add a new
        # row" branch, which this mirrors for the items case.
        observeEvent(input[[paste0("copy_model_btn_", ii)]], {
          n <- isolate(counter_input$n)
          existing_names <- vapply(seq_len(n), function(k)
            isolate(input[[paste0("textin_relations_name", k)]]) %||% paste0("m", k), character(1))
          orig_name <- isolate(input[[paste0("textin_relations_name", ii)]]) %||% paste0("m", ii)
          new_model_name <- unique_copy_name(orig_name, existing_names)
          new_n <- n + 1

          spec_idx <- isolate(item_model_spec_idx$value[[as.character(ii)]])
          specs    <- isolate(mytable_items_reactive$value)
          is_items_row <- !is.null(spec_idx) && !is.null(specs) && spec_idx <= length(specs) &&
            !is.null(specs[[spec_idx]]$factor_types)

          if (is_items_row) {
            spec <- specs[[spec_idx]]
            description <- isolate(mixture_model_flags$value[[as.character(ii)]]) %||% ""
            cur_specs <- specs
            cur_specs[[length(cur_specs) + 1]] <- spec
            mytable_items_reactive$value <- cur_specs
            counter_items$n <- length(cur_specs)
            pending_relations_restore(c(rep(NA_character_, n), ""))
            pending_relations_name_restore(c(rep(NA_character_, n), new_model_name))
            mixture_model_flags$value[[as.character(new_n)]] <- description
            item_model_spec_idx$value[[as.character(new_n)]] <- length(cur_specs)
          } else {
            orig_spec_text <- isolate(input[[paste0("textin_relations_", ii)]]) %||% ""
            pending_relations_restore(c(rep(NA_character_, n), orig_spec_text))
            pending_relations_name_restore(c(rep(NA_character_, n), new_model_name))
          }

          counter_input$n <- new_n
        }, ignoreInit = TRUE)
      })
    }
    wired_items_edit_ids(union(already, new_ids))
  }, ignoreNULL = FALSE)

  observeEvent(input$submit_items_multi, {
    base_m <- input$items_base_model
    if (is.null(base_m) || nchar(base_m) == 0) { removeModal(); return() }

    n_p_all <- isolate(counter$n)
    p_names <- vapply(seq_len(n_p_all), function(i) {
      nm <- trimws(isolate(input[[paste0("textin_name_", i)]]) %||% "")
      if (nzchar(nm)) nm else paste0("p", i)
    }, character(1))

    raw_rows <- lapply(seq_along(p_names), function(i) {
      n_items <- suppressWarnings(as.integer(input[[paste0("items_multi_n_", i)]]))
      if (is.null(n_items) || is.na(n_items)) n_items <- 1L
      type <- input[[paste0("items_multi_type_", i)]] %||% "joint"
      eps <- 0
      if (identical(type, "identical")) {
        v <- suppressWarnings(as.numeric(input[[paste0("items_multi_eps_", i)]]))
        if (!is.null(v) && !is.na(v) && v >= 0) eps <- v
      }
      group <- trimws(input[[paste0("items_multi_group_", i)]] %||% "")
      list(pname = p_names[i], n = n_items, type = type, eps = eps, group = group)
    })

    # Two or more probabilities sharing a non-empty Group become ONE
    # factor (real substitutable/identical semantics across them — see
    # the modal's own help text); everything else is its own singleton
    # factor, same as before Group existed. A shared factor's type comes
    # from the FIRST member row (in probability order) — the live JS
    # above keeps every member's own Type field in sync with the rest of
    # the group while the modal is open, so "first row" and "every row"
    # are expected to already agree by the time Submit is clicked; this
    # is just where that resolved value is read from. eps is deliberately
    # NOT resolved here — it stays per-member (see build_items_h_mixed's
    # own comment) and is read straight off each raw row further down.
    # Grouping happens BEFORE the ">= 2 items"
    # filter below, and that filter runs on the grouped factor's own n
    # (not each raw row's own n) — filtering per-probability first would
    # silently drop a group member whose OWN Items field was left at 1,
    # even though the group's actual (first-member-derived) n is >= 2.
    # Default: every row is its own solo factor (independent item
    # counts, no relationship assumed between probabilities — e.g. p1
    # can have 2 items and p3 can have 3 without one silently overriding
    # the other). Tried defaulting to "everyone in one shared group" at
    # one point specifically so Joint would look joint across multiple
    # probabilities without touching Group, but that forced every
    # grouped row onto ONE shared item count (the first row's), silently
    # discarding a later row's own different count — reverted; different
    # item counts per probability needs to keep working by default.
    # Blank Group means solo for EVERY Type, Substitutable included —
    # this used to auto-pool every blank-Group Substitutable row into
    # one shared implicit group, on the reasoning that a solo
    # Substitutable factor is a no-op (mathematically identical to
    # Joint, since permuting one probability's own items against itself
    # doesn't do anything a fully-crossed grid doesn't already do — see
    # build_items_h_core's own grid logic). That reasoning was correct
    # but the UX consequence was a trap: once auto-pooled (e.g. via
    # Copy), clearing the Group box could never actually get back OUT
    # of the group while staying Substitutable, since blank always
    # re-triggered the same pooling — reported directly as "can't undo
    # grouping". A solo Substitutable row being a redundant (not
    # incorrect) no-op is a fine, reversible default; a Group box that
    # cannot be cleared is not.
    group_keys <- vapply(seq_along(raw_rows), function(i) {
      g <- raw_rows[[i]]$group
      if (nchar(g) > 0) return(g)
      paste0("__solo_", i)
    }, character(1))
    rows <- lapply(unique(group_keys), function(gk) {
      members <- raw_rows[group_keys == gk]
      first <- members[[1]]
      member_ns <- vapply(members, function(m) m$n, integer(1))
      list(
        name = if (grepl("^__solo_", gk)) first$pname else gk,
        # max(), not first$n: a member just typed into Group without also
        # bumping ITS OWN Items past the 1 default (now possible — see
        # the row JS's own comment, Group is no longer disabled below
        # Items>1) would otherwise, as the group's FIRST member, collapse
        # the whole group's item count to 1 and drop it entirely below
        # (n>=2 filter) even though its groupmates plainly do have a real
        # item count set.
        n = max(member_ns), type = first$type,
        members = vapply(members, function(m) m$pname, character(1))
      )
    })
    rows <- Filter(function(r) r$n >= 2, rows)
    if (length(rows) == 0) { removeModal(); return() }

    # Group members must share one Items count and Type — both are a
    # property of the group/factor as a whole (see the modal's own
    # Group/Type help text), computed above from max(member_ns)/
    # first$type. The live JS above keeps the fields in sync while the
    # modal is open, but this is a server-side safety net for any client
    # that submitted before that JS ran: without it, a member typed with
    # a value that disagreed with its groupmates kept showing that stale
    # value in its own field even though the group's resolved n/type
    # (computed above) is what actually got used — the field and the
    # real spec disagreed. Sync every member's own Items/Type inputs to
    # the group's resolved values so what's displayed always matches
    # what's used; harmless no-op for solo (ungrouped) rows since their
    # only "member" is themselves. eps is NOT synced here — it stays
    # per-member (see build_items_h_mixed's own comment) and is left
    # exactly as each row's own field has it.
    for (r in rows) {
      for (pname in r$members) {
        idx <- match(pname, p_names)
        if (!is.na(idx)) {
          updateNumericInput(session, paste0("items_multi_n_", idx), value = r$n)
          updateSelectInput(session, paste0("items_multi_type_", idx), selected = r$type)
        }
      }
    }

    factors <- lapply(rows, function(r) list(name = r$name, n = r$n))
    param_factors <- setNames(rep(0L, length(p_names)), p_names)
    for (fi in seq_along(rows)) param_factors[rows[[fi]]$members] <- fi
    factor_types <- vapply(rows, function(r) r$type, character(1))
    # One tolerance PER PROBABILITY (not per factor/group) — each raw
    # row already carries its own eps as typed; a group's members can
    # have different tolerances from each other (see
    # build_items_h_mixed's own comment).
    eps_per_param <- setNames(vapply(raw_rows, function(rr) rr$eps, numeric(1)), p_names)

    # A single spec now carries a type PER FACTOR (factor_types) and a
    # tolerance PER PROBABILITY (eps_per_param) instead of one type/eps
    # applied to every factor — see build_items_h_mixed and the
    # "Multiple-items models" block in the Go pipeline, which branches on
    # whether this field is present so the (still-present, just
    # unreachable via UI) old single-type spec shape keeps working
    # unmodified.
    spec <- list(base = base_m, factors = factors, param_factors = param_factors,
      factor_types = factor_types, eps_per_param = eps_per_param)

    # Must match EXACTLY the name the Go pipeline generates for a
    # factor_types spec (see items_name there) so this Input-tab row and
    # its real H-representation line up under the same h_reactive key.
    factor_tag <- paste(vapply(rows, function(r)
      items_factor_tag(r$name, r$n, r$type, unname(eps_per_param[r$members])), character(1)), collapse = ", ")
    items_name <- paste0(base_m, " [", factor_tag, "]")
    describe_items_row <- function(n, name, type, member_eps) {
      how <- switch(type,
        joint = "linked together",
        substitutable = "interchangeable",
        average = "free individually, constrained on their average",
        identical = {
          ev <- member_eps
          if (length(ev) == 0 || all(ev == 0)) "forced equal"
          else if (length(unique(round(ev, 10))) == 1) paste0("forced equal (±", frac_str(ev[1]), " tol.)")
          else paste0("forced equal (tol. ", paste(vapply(ev, frac_str, character(1)), collapse = ", "), ")")
        })
      paste0(n, " copies of ", plain_p_name(name), ", ", how)
    }
    description <- paste(vapply(rows, function(r)
      describe_items_row(r$n, r$name, r$type, unname(eps_per_param[r$members])),
      character(1)), collapse = "; ")

    edit_info <- isolate(active_items_edit())
    if (!is.null(edit_info)) {
      # Editing an existing row in place: replace its spec at the same
      # index (so item_model_spec_idx's pointer to it stays valid), then
      # update the row's own name/description without touching
      # counter_input$n or any other row — see purge_model_cache for why
      # the OLD name's stale H/V-representation must go too when a
      # setting change (e.g. item count) changes the generated name.
      # purge_model_cache, NOT purge_model_artifacts: the latter also
      # removes any OTHER spec whose name matches old_name, which would
      # wrongly delete an unedited row's spec if it happens to still
      # share old_name with this one (e.g. an unedited Copy — see
      # copy_model_btn_'s own comment on why two rows can legitimately
      # share a name until one of them changes).
      cur_specs <- isolate(mytable_items_reactive$value)
      old_name <- items_spec_names(cur_specs[[edit_info$spec_idx]])
      cur_specs[[edit_info$spec_idx]] <- spec
      mytable_items_reactive$value <- cur_specs

      if (!identical(old_name, items_name)) purge_model_cache(old_name)
      updateTextAreaInput(session, inputId = paste0("textin_relations_name", edit_info$row_idx), value = items_name)
      mixture_model_flags$value[[as.character(edit_info$row_idx)]] <- description
      # The Unique Model Specification box's placeholder was set once at
      # row-creation time by textboxes_relations() (isolate()d against
      # everything but counter_input$n — see its own comment — so it
      # won't pick up this mixture_model_flags change on its own). Push
      # the new description straight to that textarea's placeholder
      # client-side instead of forcing a full re-render of every row.
      session$sendCustomMessage("fairy_set_placeholder", list(
        id = paste0("textin_relations_", edit_info$row_idx), placeholder = description
      ))
      active_items_edit(NULL)
      removeModal()
      return()
    }

    cur_specs <- isolate(mytable_items_reactive$value)
    if (is.null(cur_specs)) cur_specs <- list()
    cur_specs[[length(cur_specs) + 1]] <- spec
    mytable_items_reactive$value <- cur_specs
    counter_items$n <- length(cur_specs)

    n <- isolate(counter_input$n)
    new_n <- n + 1
    pending_relations_restore(c(rep(NA_character_, n), ""))
    pending_relations_name_restore(c(rep(NA_character_, n), items_name))
    mixture_model_flags$value[[as.character(new_n)]] <- description
    item_model_spec_idx$value[[as.character(new_n)]] <- length(cur_specs)
    counter_input$n <- new_n

    removeModal()
  })

  # Keep each "(n=...)" sidebar button label in sync with its underlying
  # reactive value directly, rather than only updating it inside the
  # corresponding submit_* handler. Without this, restoring these values via
  # upload (which sets them directly, bypassing submit_*) left the button
  # labels stale/blank even though the data itself was correctly restored.
  observe({
    specs <- mytable_items_reactive$value
    n_total <- if (is.null(specs)) 0 else sum(vapply(specs, function(s) length(s$types), integer(1)))
    updateActionButton(session, "show_items",
      label = if (n_total > 0) paste0("Repeated items (n=", n_total, ")") else "Repeated items")
  })

  model_sel_multiple_reactive <- reactiveValues(value = "None")

  equation_all_total_reactive <- reactiveValues(value = NULL)
  # Plain-LaTeX (no HTML) per-model align* blocks, built alongside
  # equation_all_total_reactive's HTML/MathJax cards for the on-screen
  # H-representation display — used only by the "LaTeX file" download
  # (d_latex), which needs a real, compilable .tex body, not the <table>/
  # <span style="color:..."> markup the on-screen cards use.
  equation_all_total_latex_reactive <- reactiveValues(value = NULL)
  parsim_wide_table_reactive <- reactiveValues(value = NULL)

  input_multi_knob_reactive <- reactiveValues(value = 0)

  ineq_eq_left_reactive <- reactiveValues(value = NA)
  ineq_eq_right_reactive <- reactiveValues(value = NA)
  n_user_clauses_reactive <- reactiveValues(value = NA)
  numb_p_reactive <- reactiveValues(value = NA)
  probs_reactive <- reactiveValues(value = NA)

  names_available_models_reactive <- reactiveValues(value = NA)
  names_models_reactive <- reactiveValues(value = NA)

  all_operators_reactive <- reactiveValues(value = NA)

  input_volume_reactive <- reactiveValues(value = c(
    rep("default", 5), "none",
    rep("default", 3), "none",
    rep("default", 4), "none"
  ))

  counter <- reactiveValues(n = 3)

  # Tracks which model indices already have their eq_tol_btn_i/
  # vrep_toggle_btn_i observers wired (see the two observeEvent(
  # counter_input$n, ...) blocks below), so re-wiring on a model-count
  # change only registers observers for genuinely NEW indices instead of
  # re-registering fresh (duplicate) ones for models that already had
  # them. Idempotent handlers tolerate that duplication fine, but a
  # toggle (flip the current boolean) does not — N accumulated duplicate
  # observers firing on one click flips N times, which is a no-op on an
  # even N and cancels out exactly the "stuck" behavior this was written
  # to fix.
  wired_eq_tol_ids <- reactiveVal(integer(0))

  # Marks a model row (by index, as a character key) as a "mixture
  # model" added via the "Mixture model" button — its Unique Model
  # Specification box is shown disabled/grayed with this text as a
  # placeholder instead of an editable constraint field, since a mixture
  # genuinely isn't expressible as a plain constraint list the way an
  # intersection is (it's a probabilistic blend of the source models'
  # vertex sets, not a conjunction of their constraints). Kept in sync
  # with index shifts in observeEvent(input$delete_model_idx, ...).
  mixture_model_flags <- reactiveValues(value = list())

  # Marks a model row (by index, as a character key) as derived from
  # other models via the "Intersection model"/"Mixture model" buttons:
  # list(type = "intersection"|"mixture", source_idx = c(...)). Used to
  # (a) render that row's Unique Model Specification box disabled — it's
  # a live combination of its sources, not something to type into
  # directly — and (b) for "intersection", keep it live: an observer (see
  # wired_derived_ids below) recomputes both the combined spec text and
  # the combined per-equality approximate tolerances (concatenated in the
  # same source order) whenever any source model's own spec or
  # tolerances change, via updateTextAreaInput — not a one-time snapshot
  # copied in at creation time. "mixture" rows have no live spec (a
  # mixture isn't expressible as a constraint list at all — see
  # mixture_model_flags above) so there's nothing to recompute for them.
  # Known limitation: if an EARLIER model is deleted, source_idx values
  # pointing past it are shifted down to stay correct (see
  # observeEvent(input$delete_model_idx, ...)), but a source_idx that
  # pointed AT the deleted model itself is left dangling rather than
  # cleaned up — an edge case not handled here.
  derived_model_sources <- reactiveValues(value = list())
  wired_derived_ids <- reactiveVal(integer(0))

  # Set TRUE for the duration of an upload restore (observeEvent(input$upload,
  # ...) below) so the auto-detect observer a few lines down doesn't race
  # with the explicit restore of "Name of p_i" boxes: that observer also
  # reassigns counter$n (from a debounce fired by the restored spec-text
  # boxes), forcing a second re-render of the name textboxes that can land
  # before the restored names have round-tripped back to the server,
  # silently reverting them to their "p_{i}" placeholders.
  restoring_upload <- reactiveVal(FALSE)

  # Holds the restored "Names of probabilities" vector for exactly the one
  # textboxes_names() render triggered by an upload, so that render can use
  # the real names directly instead of the "p_{i}" placeholder. See
  # textboxes_names()'s use of this below.
  pending_name_restore <- reactiveVal(NULL)

  # Same pattern, for the "Unique Model Specification" and "Model Name"
  # boxes restored on upload — without this, textboxes_relations() and
  # textboxes_relations_name() render fresh (blank/default) boxes the
  # instant counter_input$n changes, before the delayed updateTextAreaInput
  # calls land, and the model-count change now also happens to recreate
  # the DOM nodes (fresh renderUI), which orphans the client-side ">"/"<"
  # display overlay wired to the old nodes until the new ones are (re-)wired
  # — so the box can sit visibly blank rather than just briefly default.
  pending_relations_restore <- reactiveVal(NULL)
  pending_relations_name_restore <- reactiveVal(NULL)

  counter_input <- reactiveValues(n = 1)

  # Manual add/remove-probability buttons act as an extra floor alongside
  # the auto-detected "highest p_N referenced" floor below — without this,
  # clicking "+" to add a probability nobody's constraint text references
  # yet would just get silently shrunk back down again by the very next
  # auto-detect pass.
  manual_p_floor <- reactiveVal(3L)

  observeEvent(input$add_prob, {
    manual_p_floor(min(30L, manual_p_floor() + 1L))
    counter$n <- max(isolate(counter$n), manual_p_floor())
  })
  observeEvent(input$rm_prob, {
    manual_p_floor(max(3L, manual_p_floor() - 1L))
    # Recompute from scratch (rather than just decrementing counter$n)
    # so removing the manual floor doesn't drop below whatever's still
    # actually referenced in the constraint text.
    n_m <- isolate(counter_input$n)
    all_text <- paste(vapply(seq_len(n_m), function(i)
      isolate(input[[paste0("textin_relations_", i)]]) %||% "", character(1)), collapse = ";")
    nums <- suppressWarnings(as.integer(unlist(stringr::str_extract_all(all_text, "(?<=p)\\d+"))))
    nums <- nums[!is.na(nums) & nums > 0]
    counter$n <- max(3L, if (length(nums) == 0) 0L else max(nums), manual_p_floor())
  })
  # Same manual_p_floor mechanism as add_prob/rm_prob above, just set
  # directly to a typed number instead of stepped by one — invalid/blank
  # input (NA while the box is mid-edit, e.g. momentarily empty) is
  # simply ignored rather than treated as 0, so a currently-referenced
  # higher p doesn't get silently clamped away while the user is still
  # typing.
  observeEvent(input$set_prob_count, {
    val <- suppressWarnings(as.integer(input$set_prob_count))
    if (is.na(val)) return()
    val <- max(3L, min(30L, val))
    manual_p_floor(val)
    n_m <- isolate(counter_input$n)
    all_text <- paste(vapply(seq_len(n_m), function(i)
      isolate(input[[paste0("textin_relations_", i)]]) %||% "", character(1)), collapse = ";")
    nums <- suppressWarnings(as.integer(unlist(stringr::str_extract_all(all_text, "(?<=p)\\d+"))))
    nums <- nums[!is.na(nums) & nums > 0]
    counter$n <- max(val, if (length(nums) == 0) 0L else max(nums))
  }, ignoreInit = TRUE)
  # Keeps the numeric box itself showing the CURRENT count regardless of
  # which of the three ways (+, -, or typing here) last changed it, or
  # even if the auto-detected "highest p referenced" floor grew it past
  # whatever was last typed — a no-op (no new input event, no risk of
  # interrupting an in-progress edit here) whenever the box already
  # shows this value.
  observe({
    updateNumericInput(session, "set_prob_count", value = counter$n)
  })

  # Keep the number of declared probabilities in sync with the highest p
  # index actually referenced across all model specs (unique + shared spec,
  # already joined into "Model Specification"), growing OR shrinking as
  # needed — instead of only picking this up later inside extract_info()
  # when Go is clicked. Debounced: recomputing on every keystroke would
  # briefly see zero p references whenever a field is cleared mid-edit (e.g.
  # select-all-to-retype), instantly shrinking counter$n and losing any
  # custom names set for the higher p's even though the user was about to
  # type new content. Waiting for a short pause in typing avoids that.
  combined_specs_text <- reactive({
    n_m <- counter_input$n
    texts <- unlist(lapply(seq_len(n_m), function(i) input[[paste0("textin_relations_complete_", i)]]))
    paste(texts, collapse = ";")
  })
  combined_specs_text_d <- debounce(combined_specs_text, 800)

  observe({
    combined_specs_text_d()  # stay subscribed even while skipping below
    if (isTRUE(isolate(restoring_upload()))) return()
    p_nums <- suppressWarnings(as.numeric(unlist(str_extract_all(combined_specs_text_d(), "(?<=p)[0-9]+"))))
    max_p <- suppressWarnings(max(p_nums, na.rm = TRUE))
    counter$n <- max(3, if (is.finite(max_p)) max_p else 0, isolate(manual_p_floor()))
  })

  # Plain-text staleness flags, alongside the button-glow class — for
  # people who might not notice/understand a glowing button on its own.
  # Read by output$h_stale_note / output$parsimony_stale_note, rendered
  # right above the actual results.
  input_results_stale <- reactiveVal(FALSE)
  parsimony_results_stale <- reactiveVal(FALSE)

  # Snapshot saved on every Go click — used by the stale-warning renderUI.
  last_computed_snapshot <- reactiveVal(NULL)
  # Snapshot saved only when a recomputation actually ran — used to skip
  # redundant recomputations when Go is clicked with unchanged inputs.
  last_recomputed_snapshot <- reactiveVal(NULL)
  # Per-model cache: named list of model_name -> snapshot that produced the
  # current h_reactive entry.  Allows skipping unchanged models when only
  # one model (or a new model) has been added/edited.
  model_snap_cache <- reactiveVal(list())

  # Repeated-items specs are keyed by probability NAME — param_factors'
  # and eps_per_param's own names, and (for a solo, ungrouped factor) the
  # factor's own $name too, see submit_items_multi's own comment — not by
  # a stable index. Renaming a probability (the "Name of p1" boxes) was
  # therefore silently orphaning any existing items spec that referenced
  # it: the spec kept expecting the OLD name while h_base's own columns
  # (and everything downstream — see build_items_h_core) moved to the new
  # one, surfacing as an outright computation error ("length of dimnames
  # [2] not equal to array extent"), not just a stale display. Watch every
  # probability-name input and, whenever one actually changes, rewrite
  # that old name to the new one everywhere an existing items spec
  # references it, so the spec keeps tracking the same probability
  # instead of falling out of sync with it. Chained renames (old->mid,
  # then mid->new) stay correct since this compares against the
  # immediately-preceding value on every firing, not a one-time snapshot.
  prev_p_names_for_items <- reactiveVal(character(0))
  observe({
    n   <- counter$n
    cur <- vapply(seq_len(n), function(i) input[[paste0("textin_name_", i)]] %||% "", character(1))
    old <- isolate(prev_p_names_for_items())
    if (length(old) == length(cur)) {
      changed <- which(old != cur & nzchar(old) & nzchar(cur))
      if (length(changed) > 0) {
        specs <- isolate(mytable_items_reactive$value)
        if (!is.null(specs) && length(specs) > 0) {
          # Renaming a group whose own $name equals the probability
          # (a solo factor) also changes items_spec_names(spec)'s
          # output — the SAME rename that just fixed the spec's
          # internal keys also orphans whatever was already cached in
          # h_reactive/v_reactive/... under the OLD generated name
          # (confirmed directly: the old "m1 [p1:2 joint]" card kept
          # showing up as a stale, no-longer-reachable "0 eq · 0 ineq"
          # entry alongside the correctly-renamed new one). Purge the
          # old name's cache for any spec whose computed name actually
          # changes, same as editing a repeated-items row's settings
          # already does via purge_model_cache.
          old_names <- vapply(specs, function(s) paste(items_spec_names(s), collapse = "|"), character(1))
          for (ci in changed) {
            old_nm <- old[ci]; new_nm <- cur[ci]
            specs <- lapply(specs, function(s) {
              if (!is.null(s$param_factors) && old_nm %in% names(s$param_factors)) {
                names(s$param_factors)[names(s$param_factors) == old_nm] <- new_nm
              }
              if (!is.null(s$eps_per_param) && old_nm %in% names(s$eps_per_param)) {
                names(s$eps_per_param)[names(s$eps_per_param) == old_nm] <- new_nm
              }
              if (!is.null(s$factors)) {
                s$factors <- lapply(s$factors, function(f) {
                  if (identical(f$name, old_nm)) f$name <- new_nm
                  f
                })
              }
              s
            })
          }
          new_names <- vapply(specs, function(s) paste(items_spec_names(s), collapse = "|"), character(1))
          changed_names <- old_names[old_names != new_names]
          for (nm in unique(changed_names)) purge_model_cache(nm)
          mytable_items_reactive$value <- specs

          # The spec itself is now fixed, and its stale cache purged —
          # but the ROW's own "Model Name" box (textin_relations_name_i)
          # is a SEPARATE piece of state that nothing above touches; it
          # was left holding the literal OLD auto-generated name text
          # (confirmed directly: it still read "m1 [p1:2 joint]" after
          # the rename), so the base-model list built from that box's
          # own text kept trying to show a model under a name no spec
          # produces anymore, alongside the correctly-renamed new card
          # from the items-computation path — two cards for one row.
          # Push each affected row's own Name box to match, the same way
          # the "edit repeated-items settings" flow already does after
          # a settings change.
          spec_idx_map <- isolate(item_model_spec_idx$value)
          for (row_key in names(spec_idx_map)) {
            si <- spec_idx_map[[row_key]]
            if (!is.null(si) && si >= 1 && si <= length(specs) &&
                !is.na(old_names[si]) && old_names[si] != new_names[si]) {
              updateTextAreaInput(session, inputId = paste0("textin_relations_name", row_key), value = new_names[si])
            }
          }
        }
      }
    }
    prev_p_names_for_items(cur)
  })

  # Same rename-orphaning problem as prev_p_names_for_items just above,
  # but for the BASE MODEL a repeated-items spec was built from
  # (spec$base — see submit_items_multi) rather than a probability. That
  # field is a captured NAME STRING too, and nothing previously kept it
  # in sync with the base row's own "Model Name" box: renaming the base
  # model left spec$base pointing at a name no longer reachable, so the
  # Go pipeline's own `base_m %in% names(h_reactive)` guard (see the
  # "Multiple-items models" block) silently skipped recomputing it
  # instead of erroring — whatever was last cached under the items
  # model's own (now-stale) name just kept being shown, including, if
  # the base had no real constraints yet at the time of the rename, a
  # totally unconstrained placeholder that never updated afterward even
  # once the base model was properly specified.
  prev_model_names_for_items <- reactiveVal(character(0))
  observe({
    n   <- counter_input$n
    cur <- vapply(seq_len(n), function(i) input[[paste0("textin_relations_name", i)]] %||% "", character(1))
    old <- isolate(prev_model_names_for_items())
    if (length(old) == length(cur)) {
      changed <- which(old != cur & nzchar(old) & nzchar(cur))
      if (length(changed) > 0) {
        specs <- isolate(mytable_items_reactive$value)
        if (!is.null(specs) && length(specs) > 0) {
          old_names <- vapply(specs, function(s) paste(items_spec_names(s), collapse = "|"), character(1))
          for (ci in changed) {
            old_nm <- old[ci]; new_nm <- cur[ci]
            specs <- lapply(specs, function(s) {
              if (identical(s$base, old_nm)) s$base <- new_nm
              s
            })
          }
          new_names <- vapply(specs, function(s) paste(items_spec_names(s), collapse = "|"), character(1))
          changed_names <- old_names[old_names != new_names]
          for (nm in unique(changed_names)) purge_model_cache(nm)
          mytable_items_reactive$value <- specs

          # Same follow-up as prev_p_names_for_items: push each affected
          # row's own Name box to the new computed name too, so the row
          # doesn't keep displaying the stale pre-rename items_name text
          # alongside a correctly-renamed card computed under the new one.
          spec_idx_map <- isolate(item_model_spec_idx$value)
          for (row_key in names(spec_idx_map)) {
            si <- spec_idx_map[[row_key]]
            if (!is.null(si) && si >= 1 && si <= length(specs) &&
                !is.na(old_names[si]) && old_names[si] != new_names[si]) {
              updateTextAreaInput(session, inputId = paste0("textin_relations_name", row_key), value = new_names[si])
            }
          }
        }
      }
    }
    prev_model_names_for_items(cur)
  })

  make_snapshot <- function() {
    n_m <- isolate(counter_input$n)
    n_p <- isolate(counter$n)
    list(
      relations     = unlist(lapply(seq_len(n_m), function(i) isolate(input[[paste0("textin_relations_", i)]]))),
      # A probability's own NAME is a real part of the model — it's
      # literally h_base's column name (see build_items_h_core), and a
      # repeated-items spec keys param_factors/eps_per_param by it too.
      # Without tracking it here, renaming a probability leaves this
      # snapshot byte-identical to the last computed one, so Go's own
      # "skip if nothing changed" check (see the go_v_h observer just
      # below) silently no-ops on the very next click — the display
      # stays frozen on the pre-rename H-representation, permanently
      # stale, with no stale-blink warning either (confirmed directly:
      # renaming p1 after computing left both the base and repeated-
      # items cards showing the OLD name until an unrelated field also
      # changed).
      p_names       = unlist(lapply(seq_len(n_p), function(i) isolate(input[[paste0("textin_name_", i)]]))),
      # Same class of bug as p_names just above, but for a MODEL row's
      # own display name (the "Model Name" box) rather than a
      # probability's — renaming a model (base or a repeated-items row
      # alike) changes neither `relations` (constraint text is
      # untouched) nor anything else tracked here, so without this the
      # snapshot stayed byte-identical and Go's "skip if nothing
      # changed" check silently no-op'd on the very next click.
      # Confirmed directly: renaming a repeated-items row left it
      # showing as a second, unconstrained card under the new name
      # until an unrelated field also changed — not because the alias/
      # recompute logic downstream was wrong, but because it never even
      # ran.
      model_names   = unlist(lapply(seq_len(n_m), function(i) isolate(input[[paste0("textin_relations_name", i)]]))),
      shared        = isolate(input$add_for_all_models),
      n_p           = n_p,
      n_m           = n_m,
      approx        = isolate(equality_tolerances_reactive$value),
      intersections = isolate(mytable_int_reactive$value),
      mixtures      = isolate(mytable_mix_reactive$value),
      items         = isolate(mytable_items_reactive$value),
      v_selection   = isolate(mytable_v_reactive$value)
    )
  }

  observeEvent(input$go_v_h, {
    last_computed_snapshot(make_snapshot())
  }, priority = 10)

  # Same staleness pattern as make_snapshot()/last_computed_snapshot above,
  # but for the Parsimony tab: extends the H-representation snapshot with
  # the parsimony-specific settings (which algorithms are selected) and
  # the full current model list (base + replication + multi-item), since
  # adding/removing a model changes what Parsimony should cover even when
  # no existing model's own specification changed.
  last_parsimony_snapshot <- reactiveVal(NULL)
  # live=FALSE (default): every read isolated — used to freeze a snapshot
  # at Go-click time. live=TRUE: no isolation — used inside a reactive
  # renderUI so it actually re-runs when these values change. This can't
  # just call make_snapshot() (which always isolates internally) for the
  # live case, same reason output$stale_warning above builds its own
  # "current" object by hand rather than calling make_snapshot() there.
  make_parsimony_snapshot <- function(live = FALSE) {
    v <- function(x) if (live) x else isolate(x)
    n_m <- v(counter_input$n)
    n_p <- v(counter$n)
    list(
      relations     = unlist(lapply(seq_len(n_m), function(i) v(input[[paste0("textin_relations_", i)]]))),
      p_names       = unlist(lapply(seq_len(n_p), function(i) v(input[[paste0("textin_name_", i)]]))),
      shared        = v(input$add_for_all_models),
      n_p           = n_p,
      n_m           = n_m,
      approx        = v(equality_tolerances_reactive$value),
      intersections = v(mytable_int_reactive$value),
      mixtures      = v(mytable_mix_reactive$value),
      items         = v(mytable_items_reactive$value),
      v_selection   = v(mytable_v_reactive$value),
      # Deliberately NOT including CB/CG/SoB here — this snapshot only
      # tracks whether the MODELS changed since the last compute (the
      # stale badge's tooltip literally says "Models changed..."), and
      # picking a different algorithm doesn't change any model; it was
      # incorrectly flagging results as stale on every algorithm toggle.
      models        = sort(if (live) all_comparison_model_names() else isolate(all_comparison_model_names()))
    )
  }

  # Stale results no longer render a separate badge — Go/Compute itself
  # blinks (fairy-stale-blink) and its own data-tooltip explains why,
  # both toggled here via shinyjs rather than through a renderUI output.
  observe({
    snap <- last_computed_snapshot()
    is_stale <- FALSE
    if (!is.null(snap)) {
      n_m <- counter_input$n
      n_p <- counter$n
      current <- list(
        relations     = unlist(lapply(seq_len(n_m), function(i) input[[paste0("textin_relations_", i)]])),
        p_names       = unlist(lapply(seq_len(n_p), function(i) input[[paste0("textin_name_", i)]])),
        # Must mirror make_snapshot()'s own model_names field exactly
        # (same fields, same order) — this "current" list is compared
        # against a snap FROM make_snapshot() via identical(), so any
        # shape mismatch between the two makes them permanently unequal
        # regardless of actual changes, i.e. a permanently-stuck stale
        # warning. Confirmed directly: adding model_names to
        # make_snapshot() without adding it here too left Go blinking
        # stale constantly, even right after a fresh Go run.
        model_names   = unlist(lapply(seq_len(n_m), function(i) input[[paste0("textin_relations_name", i)]])),
        shared        = input$add_for_all_models,
        n_p           = n_p,
        n_m           = n_m,
        approx        = equality_tolerances_reactive$value,
        intersections = mytable_int_reactive$value,
        mixtures      = mytable_mix_reactive$value,
        items         = mytable_items_reactive$value,
        v_selection   = mytable_v_reactive$value
      )
      is_stale <- !identical(snap, current)
    }
    input_results_stale(is_stale)
    shinyjs::toggleClass(id = "go_v_h", class = "fairy-stale-blink", condition = is_stale)
    tooltip_txt <- if (is_stale) {
      "Results outdated — click to recalculate (⌘/Ctrl+Enter)"
    } else {
      "Recalculate (⌘/Ctrl+Enter)"
    }
    runjs(sprintf("document.getElementById('go_v_h').setAttribute('data-tooltip', '%s');", tooltip_txt))
  })

  observe({
    snap <- last_parsimony_snapshot()
    is_stale <- FALSE
    if (!is.null(snap)) {
      current <- make_parsimony_snapshot(live = TRUE)
      is_stale <- !identical(snap, current)
    }
    parsimony_results_stale(is_stale)
    shinyjs::toggleClass(id = "go", class = "fairy-stale-blink", condition = is_stale)
    # The input$CB|SoB|CG observer elsewhere also sets this button's
    # data-tooltip (disabled-state message vs. the normal Compute
    # message) — this branch only fires while it's enabled, so the two
    # never fight over the same attribute at the same time.
    if (!(input$CB == 0 & input$SoB == 0 & input$CG == 0)) {
      tooltip_txt <- if (is_stale) {
        "Models changed since parsimony was last computed — click to update (⌘/Ctrl+Enter)"
      } else {
        "Compute parsimony (⌘/Ctrl+Enter)"
      }
      runjs(sprintf("document.getElementById('go').setAttribute('data-tooltip', '%s');", tooltip_txt))
    }
  })

  # Plain-text staleness notes — a written alternative to the blinking
  # Go/Compute button, right above the results themselves, for anyone
  # who might not notice or understand a glowing button on its own.
  # One render function, bound to FOUR separate outputs (h_stale_note_
  # hrep/vrep/edge/poly) — one per tab that shows it. They can't share a
  # single uiOutput id: that puts the same id="..." on multiple DOM
  # elements, and Shiny's binding only ever updates the first element
  # with a given id, silently leaving the rest permanently blank (which
  # is exactly what happened on the H-representation tab specifically —
  # whichever tab's uiOutput happened to come first in the page still
  # worked, the other three never did).
  render_stale_note <- function() {
    if (!isTRUE(input_results_stale())) return(NULL)
    div(
      style = paste0(
        "margin: 0 0 10px; padding: 5px 10px; font-size: 12px;",
        "color: #b05000; background: #fff8e1; border: 1px solid #f9a825;",
        "border-radius: 6px; display: inline-block;"
      ),
      icon("triangle-exclamation", style = "margin-right:5px;"),
      # This note is shared across H-representation, V-representation,
      # and both Plot tabs — none of which have their own Go button
      # (only Input and Parsimony do, in the floating panel) — so it
      # says where to find it rather than just "click Go".
      "Results may be outdated — go to the Input tab and click Go to refresh."
    )
  }
  output$h_stale_note_hrep <- renderUI(render_stale_note())
  output$h_stale_note_vrep <- renderUI(render_stale_note())
  output$h_stale_note_edge <- renderUI(render_stale_note())
  output$h_stale_note_poly <- renderUI(render_stale_note())

  output$parsimony_stale_note <- renderUI({
    if (!isTRUE(parsimony_results_stale())) return(NULL)
    div(
      style = paste0(
        "margin: 0 0 10px; padding: 5px 10px; font-size: 12px;",
        "color: #b05000; background: #fff8e1; border: 1px solid #f9a825;",
        "border-radius: 6px; display: inline-block;"
      ),
      icon("triangle-exclamation", style = "margin-right:5px;"),
      "Results may be outdated — click Go to refresh."
    )
  })

  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  })


  #### Button Events ####

  observeEvent(input$remove_inter, {
    counter_inter$n <- 1
  })

  observeEvent(input$more_inter, {
    counter_inter$n <- isolate(counter_inter$n + 1)
  })

  observeEvent(input$less_inter, {
    if (counter_inter$n > 1) {
      counter_inter$n <- counter_inter$n - 1
    }
  })

  # Auto-sync parameter count with the highest p_N referenced in any constraint.
  # Caps at 30 to avoid runaway growth from typos (e.g. accidental "p89").
  # Also shrinks back when a typo is corrected, but never below 3.
  observe({
    n_m      <- counter_input$n
    if (isTRUE(isolate(restoring_upload()))) return()
    all_text <- paste(vapply(seq_len(n_m), function(i)
      input[[paste0("textin_relations_", i)]] %||% "", character(1)), collapse = ";")
    nums <- suppressWarnings(
      as.integer(unlist(stringr::str_extract_all(all_text, "(?<=p)\\d+"))))
    nums <- nums[!is.na(nums) & nums > 0]
    target <- max(3L, if (length(nums) == 0) 0L else max(nums), isolate(manual_p_floor()))
    cur <- isolate(counter$n)
    if (target != cur) counter$n <- target
  })

  observeEvent(input$add_ie, {
    counter_input$n <- counter_input$n + 1
  })

  observeEvent(input$rm_ie, {
    if (counter_input$n > 1) {
      counter_input$n <- counter_input$n - 1
    }
  })

  # Per-model delete (the trash-can button on each "Model Name" box):
  # unlike rm_ie above (which always drops the last model), this removes
  # one specific model out of the middle of the list. Since every model's
  # data lives in fixed-index inputs (textin_relations_name<i>,
  # textin_relations_<i>, ...), "removing" model idx is done by shifting
  # every later model's values down by one index via updateTextAreaInput
  # (ascending order so each read happens before its slot is overwritten),
  # then shrinking counter_input$n by one so the now-duplicate last box is
  # dropped. With only one model left, there is nothing to shift down to
  # and the UI always expects at least one model box, so this just clears
  # that model's fields back to their defaults instead of removing it.
  # Purges every trace of a just-deleted model so it can never silently
  # reappear elsewhere: its cached H/V-representation and volume (keyed
  # by name in the various reactiveValues below — nothing purges these on
  # its own otherwise), its row in the V-representation selection table,
  # and — the actual bug this was written for — its "Multiple items"
  # spec in mytable_items_reactive$value, which used to be left behind
  # entirely and would silently regenerate the "deleted" model on the
  # very next Go (see items_spec_names's own comment).
  # Cache-only half of purge_model_artifacts below — clears a name's
  # computed H/V-representation (and its row in the V-rep selection
  # table) WITHOUT touching mytable_items_reactive$value's spec list.
  # Use this whenever a row's own display name is changing but its
  # underlying spec list should be left alone — critical since Copy (see
  # copy_model_btn_) can leave two rows legitimately sharing one
  # name/spec until one of them is actually edited. purge_model_artifacts
  # itself removes any spec whose name MATCHES the purged name — exactly
  # right when a whole row is deleted, but wrong here: editing one of two
  # identically-named rows would purge the OTHER (untouched) row's spec
  # too, breaking a model nobody touched (confirmed directly — editing a
  # copy left the original showing an empty/uncomputed card).
  purge_model_cache <- function(deleted_name) {
    if (is.null(deleted_name) || !nchar(deleted_name)) return()
    h_reactive[[deleted_name]] <- NULL
    v_reactive[[deleted_name]] <- NULL
    h_pars_reactive[[deleted_name]] <- NULL
    h_raw_rows_reactive[[deleted_name]] <- NULL
    vtab <- isolate(mytable_v_reactive$value)
    if (!is.null(vtab) && ncol(vtab) >= 1 && nrow(vtab) > 0) {
      mytable_v_reactive$value <- vtab[vtab[[1]] != deleted_name, , drop = FALSE]
    }
  }

  # Removes ONE items spec by its POSITION in mytable_items_reactive$value
  # (not by matching a display name — see purge_model_cache's own comment
  # on why name-matching is unsafe once two rows can share a name), and
  # shifts every item_model_spec_idx pointer above that position down by
  # one to follow, same bookkeeping purge_model_artifacts's old
  # name-matching block used to do.
  remove_items_spec_at <- function(pos) {
    cur_specs <- isolate(mytable_items_reactive$value)
    if (is.null(cur_specs) || is.null(pos) || pos < 1 || pos > length(cur_specs)) return()
    old_map <- isolate(item_model_spec_idx$value)
    new_map <- old_map
    for (key in names(old_map)) {
      p <- old_map[[key]]
      if (is.null(p)) next
      if (identical(p, pos)) {
        new_map[[key]] <- NULL
      } else if (p > pos) {
        new_map[[key]] <- p - 1L
      }
    }
    item_model_spec_idx$value <- new_map
    mytable_items_reactive$value <- cur_specs[-pos]
  }

  # Pulled out of the observer below into its own function — "Delete all
  # models" needs to run this exact same per-row cleanup repeatedly (once
  # per row, from the end backward), and duplicating ~75 lines of index-
  # shifting/derived-model/items-spec bookkeeping for that would be an
  # easy way to let the two fall out of sync with each other.
  delete_model_at_idx <- function(idx) {
    n <- isolate(counter_input$n)
    if (is.na(idx) || idx < 1 || idx > n) {
      return()
    }
    deleted_name <- isolate(input[[paste0("textin_relations_name", idx)]]) %||% paste0("m", idx)
    # purge_model_cache + remove THIS row's own spec by position, not
    # purge_model_artifacts's name-matching removal — a Copy (see
    # copy_model_btn_'s own comment) can leave another row legitimately
    # sharing deleted_name via its own separate, unedited spec entry;
    # matching by name would delete that other row's spec too even
    # though only THIS row is being removed.
    purge_model_cache(deleted_name)
    deleted_spec_pos <- isolate(item_model_spec_idx$value[[as.character(idx)]])
    if (!is.null(deleted_spec_pos)) remove_items_spec_at(deleted_spec_pos)
    if (n <= 1) {
      updateTextAreaInput(session, inputId = paste0("textin_relations_name", idx), value = paste0("m", idx))
      updateTextAreaInput(session, inputId = paste0("textin_relations_", idx), value = "")
      mixture_model_flags$value[[as.character(idx)]] <- NULL
      derived_model_sources$value[[as.character(idx)]] <- NULL
      item_model_spec_idx$value[[as.character(idx)]] <- NULL
      return()
    }
    old_flags <- isolate(mixture_model_flags$value)
    new_flags <- old_flags
    new_flags[[as.character(idx)]] <- NULL

    old_item_idx <- isolate(item_model_spec_idx$value)
    new_item_idx <- old_item_idx
    new_item_idx[[as.character(idx)]] <- NULL

    # Same idea as the flags above, but also has to renumber each
    # remaining derived model's OWN source_idx values — a source_idx
    # pointing past the deleted row needs to shift down by one too, or
    # it'd end up pointing at the wrong model after the delete. A
    # source_idx pointing AT the deleted row itself is left as-is (see
    # derived_model_sources's own comment on this known limitation).
    old_derived <- isolate(derived_model_sources$value)
    new_derived <- lapply(old_derived, function(info) {
      info$source_idx <- ifelse(info$source_idx > idx, info$source_idx - 1L, info$source_idx)
      info
    })
    new_derived[[as.character(idx)]] <- NULL

    if (idx < n) {
      for (j in seq(idx, n - 1)) {
        updateTextAreaInput(session,
          inputId = paste0("textin_relations_name", j),
          value   = isolate(input[[paste0("textin_relations_name", j + 1)]])
        )
        updateTextAreaInput(session,
          inputId = paste0("textin_relations_", j),
          value   = isolate(input[[paste0("textin_relations_", j + 1)]])
        )
        # Shift the mixture flag alongside the values above, same as the
        # actual model data — a mixture row's flag needs to follow it
        # down an index when something earlier in the list is deleted.
        shifted <- old_flags[[as.character(j + 1)]]
        new_flags[[as.character(j)]] <- shifted
        new_flags[[as.character(j + 1)]] <- NULL

        shifted_derived <- new_derived[[as.character(j + 1)]]
        new_derived[[as.character(j)]] <- shifted_derived
        new_derived[[as.character(j + 1)]] <- NULL

        shifted_item_idx <- new_item_idx[[as.character(j + 1)]]
        new_item_idx[[as.character(j)]] <- shifted_item_idx
        new_item_idx[[as.character(j + 1)]] <- NULL
      }
    }
    mixture_model_flags$value <- new_flags
    derived_model_sources$value <- new_derived
    item_model_spec_idx$value <- new_item_idx
    counter_input$n <- n - 1
  }

  observeEvent(input$delete_model_idx, {
    delete_model_at_idx(suppressWarnings(as.integer(input$delete_model_idx)))
  })

  # "Delete all models": repeats the exact same per-row delete, one row
  # at a time from the END backward (matching how a user clicking each
  # row's own trash icon in sequence would behave — later rows never
  # need their OWN index adjusted for an earlier deletion this way,
  # unlike deleting front-to-back). The very last call (idx=1, n=1) hits
  # delete_model_at_idx's own n<=1 branch, resetting row 1 to a blank
  # default instead of leaving zero rows — this app always has at least
  # one model row, the same end state as deleting down to the last one
  # manually.
  observeEvent(input$delete_all_models_btn, {
    n <- isolate(counter_input$n)
    if (n < 2) return()
    for (idx in n:1) delete_model_at_idx(idx)
  })

  # "Intersection model": adds a brand-new model row whose Unique Model
  # Specification is just every selected existing model's own Unique
  # Model Specification concatenated with ";" — a real, independent model
  # in the list (not the separate "Model intersections" feature, which
  # computes and stores an H/V-representation intersection rather than
  # adding a model row here). Shared Model Specification is deliberately
  # NOT copied in — it already applies to every model automatically,
  # including this new one, so repeating it here would just duplicate
  # those constraints.
  # Both buttons only make sense once there's actually something to
  # combine (2+ models) — used to just grey out below that via
  # shinyjs::toggleState, but a disabled-yet-still-visible button reads
  # as "this exists but I can't use it yet" rather than "not relevant at
  # your current model count", so they're hidden entirely instead, same
  # as the vrep_toggle_all master button's own n > 1 gate. Tracks
  # "was visible on the previous render" (not a one-shot "seen ever"
  # flag) so the intro pulse below plays again every time these come
  # back from being hidden, e.g. deleting down to 1 model then adding a
  # 2nd again — not just once per session.
  intersection_mixture_btns_was_visible <- reactiveVal(FALSE)
  # Same id-based fix as vrep_toggle_all_appear_id (see its own long
  # comment for the full story): deciding "just appeared" INLINE, every
  # time this renderUI itself executes, replayed the pulse on every
  # resume from tab-hidden suspension too — switching to another tab
  # and back re-glowed these two buttons with nothing else touched. An
  # always-running observer (never suspended by tab visibility) stamps
  # each genuine appearance with a new id instead, and the renderUI
  # tracks which id it has already shown a pulse for, so a later re-read
  # of the same id shows nothing.
  intersection_mixture_appear_id <- reactiveVal(0)
  intersection_mixture_last_shown_id <- reactiveVal(0)
  observe({
    now_visible <- isTRUE(counter_input$n >= 2)
    if (now_visible && !isolate(intersection_mixture_btns_was_visible())) {
      intersection_mixture_appear_id(isolate(intersection_mixture_appear_id()) + 1)
    }
    intersection_mixture_btns_was_visible(now_visible)
  }, priority = 10)
  output$intersection_mixture_btns_ui <- renderUI({
    now_visible <- isTRUE(counter_input$n >= 2)
    cur_id <- intersection_mixture_appear_id()
    just_appeared <- cur_id > isolate(intersection_mixture_last_shown_id())
    if (just_appeared) intersection_mixture_last_shown_id(cur_id)
    if (!now_visible) return(NULL)
    btns <- tagList(
      actionBttn("add_intersection_model", "Intersection model", icon = icon("object-group"),
        size = "xs", style = "jelly", color = "default"
      ),
      actionBttn("add_mixture_model", "Mixture model", icon = icon("blender"),
        size = "xs", style = "jelly", color = "default"
      )
    )
    # See .fairy-intro-pulse-wrap's own comment for why this is a
    # wrapping span around both buttons rather than a class on each.
    if (just_appeared) tags$span(class = "fairy-intro-pulse-wrap", btns) else btns
  })

  observeEvent(input$add_intersection_model, {
    n <- isolate(counter_input$n)
    if (n < 2) {
      return()
    }
    model_names <- vapply(seq_len(n), function(i) {
      isolate(input[[paste0("textin_relations_name", i)]]) %||% paste0("m", i)
    }, character(1))

    showModal(modalDialog(
      title = NULL,
      size = "s",
      class = "fairy-combo-modal",
      tags$div(class = "fairy-combo-header", tags$strong("Add intersection model")),
      tags$p(class = "fairy-combo-help", "Pick two or more models — the new model's Unique Model Specification will be all of their constraints combined."),
      checkboxGroupInput("intersection_model_picker", NULL, choices = setNames(seq_len(n), model_names)),
      footer = tagList(
        modalButton("Cancel"),
        actionBttn("submit_intersection_model", "Add", style = "material-flat", color = "primary", size = "sm")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$submit_intersection_model, {
    selected_idx <- suppressWarnings(as.integer(input$intersection_model_picker))
    selected_idx <- selected_idx[!is.na(selected_idx)]
    if (length(selected_idx) < 2) {
      return()
    }

    n <- isolate(counter_input$n)
    selected_specs <- vapply(selected_idx, function(i) {
      isolate(input[[paste0("textin_relations_", i)]]) %||% ""
    }, character(1))
    selected_names <- vapply(selected_idx, function(i) {
      isolate(input[[paste0("textin_relations_name", i)]]) %||% paste0("m", i)
    }, character(1))

    combined_spec <- paste(selected_specs[nzchar(trimws(selected_specs))], collapse = "; ")
    combined_name <- paste0("inters_", paste(selected_names, collapse = "_"))

    combined_tol <- unlist(lapply(selected_idx, unique_model_tol_slice))

    # A mixture has no constraint text at all (see mixture_model_flags's
    # own comment), and neither does an intersection that itself already
    # involves one — so an intersection built from either kind of source
    # can't be shown as real constraint text either, only described. This
    # checks both directly-mixture sources and transitively (an
    # intersection-of-a-mixture used as a source here).
    involves_mixture <- any(vapply(selected_idx, function(i) {
      !is.null(isolate(mixture_model_flags$value[[as.character(i)]]))
    }, logical(1)))

    new_n <- n + 1
    pending_relations_restore(c(rep(NA_character_, n), combined_spec))
    pending_relations_name_restore(c(rep(NA_character_, n), combined_name))
    # Both of these must be set BEFORE bumping counter_input$n below —
    # the observeEvent(counter_input$n, ...) that wires up this model's
    # live recompute (see wired_derived_ids's own comment) reads
    # derived_model_sources when IT fires, which only happens once n
    # actually changes.
    derived_model_sources$value[[as.character(new_n)]] <- list(type = "intersection", source_idx = selected_idx)
    equality_tolerances_reactive$value[[as.character(new_n)]] <- combined_tol
    if (involves_mixture) {
      # Reuses the exact same placeholder mechanism as a real mixture row
      # (mixture_model_flags) — both the Unique Model Specification box
      # (see textboxes_relations()) and the read-only Model Specification
      # preview (see textboxes_relations_complete()) check this and show
      # the description instead of constraint text/its (partial,
      # misleading) concatenation.
      mixture_model_flags$value[[as.character(new_n)]] <- paste0(
        "Intersection of ", paste(selected_names, collapse = ", ")
      )
    }
    counter_input$n <- new_n

    removeModal()
  })

  # "Mixture model": adds a model row standing in for a mixture of the
  # selected models, same as the intersection button above except the
  # new row's Unique Model Specification box is shown disabled/grayed
  # with a description in place of an editable constraint list — a
  # mixture is a probabilistic blend of its source models' vertex sets,
  # not a conjunction of their constraints, so there's no plain
  # constraint text that would actually describe it (see
  # mixture_model_flags's own comment).
  observeEvent(input$add_mixture_model, {
    n <- isolate(counter_input$n)
    if (n < 2) {
      return()
    }
    model_names <- vapply(seq_len(n), function(i) {
      isolate(input[[paste0("textin_relations_name", i)]]) %||% paste0("m", i)
    }, character(1))

    showModal(modalDialog(
      title = NULL,
      size = "s",
      class = "fairy-combo-modal",
      tags$div(class = "fairy-combo-header", tags$strong("Add mixture model")),
      tags$p(class = "fairy-combo-help", "Pick two or more models for a mixture."),
      checkboxGroupInput("mixture_model_picker", NULL, choices = setNames(seq_len(n), model_names)),
      footer = tagList(
        modalButton("Cancel"),
        actionBttn("submit_mixture_model", "Add", style = "material-flat", color = "primary", size = "sm")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$submit_mixture_model, {
    selected_idx <- suppressWarnings(as.integer(input$mixture_model_picker))
    selected_idx <- selected_idx[!is.na(selected_idx)]
    if (length(selected_idx) < 2) {
      return()
    }

    n <- isolate(counter_input$n)
    selected_names <- vapply(selected_idx, function(i) {
      isolate(input[[paste0("textin_relations_name", i)]]) %||% paste0("m", i)
    }, character(1))

    combined_name <- paste0("mix_", paste(selected_names, collapse = "_"))
    new_n <- n + 1

    pending_relations_restore(c(rep(NA_character_, n), ""))
    pending_relations_name_restore(c(rep(NA_character_, n), combined_name))
    mixture_model_flags$value[[as.character(new_n)]] <- paste0("Mixture of ", paste(selected_names, collapse = ", "))
    derived_model_sources$value[[as.character(new_n)]] <- list(type = "mixture", source_idx = selected_idx)
    counter_input$n <- new_n

    removeModal()
  })

  observeEvent(input$add_models, {
    names_available_models <- isolate(names_available_models_reactive$value)

    models_to_plot <- input$name_model_plot
    models_to_plot <- unlist(str_split(models_to_plot, ";"))
    models_to_plot <- trimws(models_to_plot)
    models_to_plot <- unlist(models_to_plot)

    models_to_plot <- unique(c(models_to_plot, names_available_models))
    models_to_plot <- models_to_plot[models_to_plot != ""]

    updateTextAreaInput(
      inputId = "name_model_plot",
      value = paste(models_to_plot, collapse = ";")
    )
  })

  observeEvent(input$subtr_models, {
    models_to_plot <- input$name_model_plot
    models_to_plot <- unlist(str_split(models_to_plot, ";"))
    models_to_plot <- trimws(models_to_plot)
    models_to_plot <- unlist(models_to_plot)

    models_to_plot <- ""

    updateTextAreaInput(
      inputId = "name_model_plot",
      value = paste(models_to_plot, collapse = ";")
    )
  })

  observeEvent(input$submit_inter, {
    removeModal()
  })

  ###

  observeEvent(input$CB | input$SoB | input$CG, {
    if (input$CB == 0 & input$SoB == 0 & input$CG == 0) {
      disable("go")
      runjs("document.getElementById('go').setAttribute('data-tooltip', 'Choose at least one algorithm to enable this option.');")
    } else {
      enable("go")
      runjs("document.getElementById('go').setAttribute('data-tooltip', 'Compute parsimony (⌘/Ctrl+Enter)');")
    }
  })



  #### Checkbox Events ####

  textboxes_min <- reactive({
    n <- counter$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          numericInput(
            min = 0,
            max = 1,
            step = .01,
            inputId = paste0("textin_min_", i),
            label = paste0("Minimum of p", i),
            value = ifelse(is.null(AllInputs()[[paste0("textin_min_", i)]]) == TRUE,
              0, AllInputs()[[paste0("textin_min_", i)]]
            )
          )
        })
      })
    }
  })

  textboxes_max <- reactive({
    n <- counter$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          numericInput(
            min = 0,
            max = 1,
            step = .01,
            inputId = paste0("textin_max_", i),
            label = paste0("Maximum of p", i),
            value = ifelse(is.null(AllInputs()[[paste0("textin_max_", i)]]) == TRUE,
              1, AllInputs()[[paste0("textin_max_", i)]]
            )
          )
        })
      })
    }
  })

  textboxes_names <- reactive({
    n <- counter$n

    if (n > 0) {
      isolate({
        # If an upload restore is pending, use its names directly for this
        # render instead of the "p_{i}" placeholder — the box is then born
        # with the correct value instead of relying on a later
        # updateTextAreaInput() to overwrite the placeholder, which can lose
        # a race against the debounced auto-detect observer above and leave
        # the last box(es) stuck on the default. See restoring_upload.
        pending <- pending_name_restore()
        lapply(seq_len(n), function(i) {
          restored <- if (!is.null(pending) && i <= length(pending) &&
                          !is.na(pending[i]) && nchar(pending[i]) > 0) pending[i] else NULL
          textAreaInput(
            inputId = paste0("textin_name_", i),
            label = paste0("Name of p", i),
            value = if (!is.null(restored)) restored else ifelse(
              is.null(AllInputs()[[paste0("textin_name_", i)]]) == TRUE,
              paste0("p_{", i, "}"),
              AllInputs()[[paste0("textin_name_", i)]]
            ),
            width = "160px",
            height = "34px",
            resize = "none"
          )
        })
      })
    }
  })

  ####

  # Joins a model's unique spec with the shared spec for the "Model
  # Specification" preview — only inserting "; " between them when BOTH
  # sides actually have content, so an empty unique or shared spec doesn't
  # leave a bare/dangling ";" in the preview.
  # NOTE: this box's real value is NOT cosmetic-only — the Go/Parsimony
  # pipeline reads input$textin_relations_complete_N directly as the source
  # of the parsed constraints (see make_parsimony_snapshot / the Go
  # handler's mytable_input construction), so it must stay literal ">"/"<".
  # An earlier version of this baked ≥/≤ into it for display,
  # which silently broke computation — do not reintroduce that here.
  join_specs <- function(a, b) {
    a <- paste(a, collapse = "")
    b <- paste(b, collapse = "")
    if (nzchar(trimws(a)) && nzchar(trimws(b))) paste(a, b, sep = "; ") else paste0(a, b)
  }

  observeEvent(c(counter_input$n, input$add_for_all_models), {
    for (loop_n in 1:counter_input$n) {
      full_descri <- paste("observeEvent(input$textin_relations_", loop_n, ", {for(loop_conjung in 1 : counter_input$n){updateTextAreaInput(inputId = paste0('textin_relations_complete_', loop_conjung),value = join_specs(AllInputs()[[paste0('textin_relations_', loop_conjung)]],input$add_for_all_models))}})", sep = "")
      eval(parse(text = full_descri))
    }
  })

  # Mute Download until something's typed — genuinely reactive (Shiny's
  # own dependency graph, not a client-side DOM poll guessing at when to
  # recheck): re-runs whenever counter_input$n changes (a model added/
  # removed) or ANY of the current Unique/Shared Model Specification
  # boxes' text changes, and toggles fairy-btn-muted (see its own CSS)
  # via shinyjs accordingly. isolate() on the per-model reads is safe —
  # this observe() already has its own real reactive dependency on each
  # one via the direct (non-isolated) input$textin_relations_<i> read
  # inside the loop below.
  observe({
    n <- counter_input$n
    filled <- nzchar(trimws(input$add_for_all_models %||% ""))
    if (!filled && n > 0) {
      for (i in seq_len(n)) {
        if (nzchar(trimws(input[[paste0("textin_relations_", i)]] %||% ""))) {
          filled <- TRUE
          break
        }
      }
    }
    shinyjs::toggleClass(id = "download", class = "fairy-btn-muted", condition = !filled)
  })

  ####

  # NOTE: this box's DISPLAY substitution (plain >/< shown as the words
  # >=/<=  is done client-side (see the small script near the top of the
  # UI matching id^="model_spec_display_") rather than here in R. An
  # R-side version using unicode escapes / enc2utf8() was tried first and
  # reliably rendered as the literal text "<U+2265>" instead of the actual
  # character — that is R's own fallback notation for a character it
  # cannot represent in the server's locale (observed with an ASCII-only
  # LC_CTYPE), which no amount of encoding-marking on the R side fixes.
  # Expands the "{p1,p2} < {p3,3*p4}" shortcut into the real clauses it
  # stands for ("p1<p3; p1<3*p4; p2<p3; p2<3*p4") for DISPLAY only, in
  # the read-only Model Specification preview — so a user can see what
  # the shorthand actually means without having to mentally expand it
  # themselves. Mirrors expand_scalar's own logic (the version the Go
  # pipeline actually computes from, further down in this file) rather
  # than calling it directly — that one lives deep inside the parsing
  # pipeline and isn't meant to be called standalone from here.
  expand_batch_shortcuts_for_display <- function(spec) {
    if (is.null(spec) || !nzchar(spec)) return(spec %||% "")
    clauses <- unlist(strsplit(spec, ";"))
    expanded <- vapply(clauses, function(clause) {
      if (!grepl("{", clause, fixed = TRUE)) return(clause)
      batches <- strsplit(gsub("[{} ]", "", clause), "[><=]")[[1]]
      if (length(batches) != 2) return(clause)
      batch1 <- unlist(strsplit(batches[1], ","))
      batch2 <- unlist(strsplit(batches[2], ","))
      separator <- gsub("[p{}]", "", gsub("[^><=]", "", clause))
      comparisons <- character()
      for (a in batch1) for (b in batch2) comparisons <- c(comparisons, paste0(a, separator, b))
      paste(comparisons, collapse = "; ")
    }, character(1), USE.NAMES = FALSE)
    paste(expanded, collapse = "; ")
  }

  # This function used to only strip the <U+2265>-style fallback
  # notation; now also unpacks {..}/{..} shortcuts for display (see
  # expand_batch_shortcuts_for_display's own comment) before that.
  ineq_words <- function(s) expand_batch_shortcuts_for_display(s %||% "")

  # Comma-separated "1"/"0" per "=" clause found (left-to-right) in `spec`,
  # "1" only for a clause whose corresponding entry in `tol_vec` is a real,
  # actually-set nonzero tolerance — never for one that's merely present
  # but still exact. Shared by both the read-only Model Specification
  # display and the editable Unique Model Specification box's own overlay
  # (see their respective eq_approx_flags_*/substForBox() client-side
  # code) so "which = is approximate" is computed exactly once, the same
  # way, in both places.
  equality_flags_str <- function(spec, tol_vec) {
    raw_clauses <- trimws(unlist(strsplit(spec %||% "", ";")))
    raw_clauses <- raw_clauses[nzchar(raw_clauses)]
    # Chain-expand each raw clause (see expand_clause_to_rows's own
    # comment — "p1=p4=0" is one typed clause but two real "=" rows) so
    # this counts the same one-flag-per-literal-"=" the client-side
    # subst() substitution actually walks, and the same per-row order
    # tol_vec (equality_tolerances_reactive) is keyed by.
    clauses <- unlist(lapply(raw_clauses, expand_clause_to_rows))
    clauses <- clauses[grepl("=", clauses)]
    if (length(clauses) == 0) {
      return("")
    }
    flags <- vapply(seq_along(clauses), function(k) {
      if (!is.null(tol_vec) && k <= length(tol_vec) && !is.na(tol_vec[k]) && tol_vec[k] != 0) "1" else "0"
    }, character(1))
    paste(flags, collapse = ",")
  }

  # The slice of `src_idx`'s own equality-tolerance vector that actually
  # belongs to its Unique Model Specification alone (join_specs() puts
  # unique text first, so the tolerance vector's first N entries are
  # always the unique text's own N equality clauses — see
  # eq_approx_flags_unique_i's identical reasoning). Used to carry a
  # source model's approximate-equality settings over into a derived
  # intersection model, whose combined spec is built purely from source
  # models' unique specs (never their shared spec, which already applies
  # automatically anyway).
  unique_model_tol_slice <- function(src_idx) {
    spec <- input[[paste0("textin_relations_", src_idx)]] %||% ""
    raw_clauses <- trimws(unlist(strsplit(spec, ";")))
    raw_clauses <- raw_clauses[nzchar(raw_clauses)]
    # Chain-expand — see expand_clause_to_rows's own comment — so this
    # slices by the same per-row occurrence index equality_tolerances_
    # reactive is actually keyed by, not by raw semicolon-clause count.
    clauses <- unlist(lapply(raw_clauses, expand_clause_to_rows))
    clauses <- clauses[grepl("=", clauses)]
    if (length(clauses) == 0) {
      return(numeric(0))
    }
    tol_vec <- equality_tolerances_reactive$value[[as.character(src_idx)]]
    vapply(seq_along(clauses), function(k) {
      if (!is.null(tol_vec) && k <= length(tol_vec) && !is.na(tol_vec[k])) tol_vec[k] else 0
    }, numeric(1))
  }

  textboxes_relations <- reactive({
    n <- counter_input$n

    if (n > 0) {
      isolate({
        pending <- pending_relations_restore()
        lapply(seq_len(n), function(i) {
          restored <- if (!is.null(pending) && i <= length(pending) && !is.na(pending[i])) pending[i] else NULL
          mix_label <- mixture_model_flags$value[[as.character(i)]]
          derived_info <- derived_model_sources$value[[as.character(i)]]
          is_derived <- !is.null(derived_info)
          box <- if (!is.null(mix_label)) {
            # A mixture isn't expressible as a plain constraint list (see
            # mixture_model_flags's own comment) — disabled and grayed
            # out with the description as its placeholder, rather than an
            # editable box, since there's nothing meaningful to type here.
            shinyjs::disabled(
              textAreaInput(
                inputId = paste0("textin_relations_", i),
                label = if (i == 1) "Unique Model Specification" else tags$span(class = "sr-only", paste0("Unique Model Specification (row ", i, ")")),
                value = "",
                width = "100%",
                height = "130px",
                placeholder = mix_label,
                resize = "none"
              )
            )
          } else if (is_derived) {
            # Intersection model: disabled too, but shows the REAL (live,
            # not a one-time snapshot — see wired_derived_ids) combined
            # spec rather than a placeholder, since this one genuinely
            # does have constraint text worth showing.
            shinyjs::disabled(
              textAreaInput(
                inputId = paste0("textin_relations_", i),
                label = if (i == 1) "Unique Model Specification" else tags$span(class = "sr-only", paste0("Unique Model Specification (row ", i, ")")),
                value = if (!is.null(restored)) restored else AllInputs()[[paste0("textin_relations_", i)]],
                width = "100%",
                height = "130px",
                resize = "none"
              )
            )
          } else {
            textAreaInput(
              inputId = paste0("textin_relations_", i),
              # Header shown once, above the first model only — repeating
              # it on every row read as clutter once there were more than
              # a couple of models.
              label = if (i == 1) "Unique Model Specification" else tags$span(class = "sr-only", paste0("Unique Model Specification (row ", i, ")")),
              value = if (!is.null(restored)) restored else AllInputs()[[paste0("textin_relations_", i)]],
              width = "100%",
              height = "130px",
              placeholder = "p1 > p2; ...",
              resize = "none"
            )
          }
          tags$div(
            class = paste("fairy-model-row", if (i %% 2 == 0) "fairy-row-even" else "fairy-row-odd"),
            `data-row-idx` = i,
            style = "position: relative;",
            if (is.null(mix_label) && !is_derived) tags$button(
              class = "fairy-expand-btn", type = "button",
              title = "Expand",
              `data-target` = paste0("textin_relations_", i),
              style = paste0("position:absolute; top:", if (i == 1) "26px" else "4px", "; right:4px; z-index:5; border:none; background:transparent; cursor:pointer; font-size:14px; color:#888;"),
              HTML("&#10530;")
            ),
            box,
            tags$div(
              style = "display:none;",
              textOutput(paste0("eq_approx_flags_unique_", i))
            )
          )
        })
      })
    }
  })

  textboxes_relations_complete <- reactive({
    n <- counter_input$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          # The client-side overlay technique used for the editable boxes
          # (see the JS near the top of the UI) turned out unreliable for
          # this specific box across repeated attempts — its content only
          # ever changes via server-pushed updateTextAreaInput() calls, and
          # exactly why the overlay didn't reliably pick those up was never
          # pinned down. Sidestepping that class of bug entirely here: the
          # real textarea (id textin_relations_complete_i, still literal
          # >/<) stays exactly as before and keeps feeding the Go/
          # Parsimony pipeline unchanged, just visually hidden — a plain,
          # genuinely reactive text output sits in its place showing the
          # words-substituted version, recomputed natively by Shiny on
          # every keystroke, no client-side JS or event-timing involved.
          local({
            ii <- i
            output_id <- paste0("model_spec_display_", ii)
            output[[output_id]] <- renderText({
              # A mixture (or an intersection involving one) has no
              # constraint text to show at all — same placeholder as its
              # Unique Model Specification box (see mixture_model_flags's
              # own comment and textboxes_relations()) instead of
              # whatever join_specs()/ineq_words() would otherwise compute
              # from its (empty, or misleadingly partial) spec.
              mix_label <- mixture_model_flags$value[[as.character(ii)]]
              if (!is.null(mix_label)) {
                return(mix_label)
              }
              # Also depend on this model's approx tolerances (even though
              # they don't change the STRING itself — that only ever has
              # literal >/</=, never ≈, per ineq_words()'s own comment) so
              # Shiny re-sends this text whenever a tolerance is
              # toggled/changed. That re-send is what lets the client-side
              # poll (near the top of the UI, matching
              # id^="model_spec_display_") tell the difference between "="
              # that should now render as "≈" and one that shouldn't —
              # without it, toggling approximate on/off wouldn't change
              # anything already sitting in the DOM since the underlying
              # spec text itself didn't change.
              equality_tolerances_reactive$value[[as.character(ii)]]
              ineq_words(join_specs(input[[paste0("textin_relations_", ii)]], input$add_for_all_models))
            })

            # Comma-separated 1/0 flags, one per "=" clause (in the same
            # left-to-right order the popup/computation use), read by that
            # same client-side poll to know which "=" occurrences in the
            # text above should display as "≈" (only ever "1" for a
            # clause actually switched to approximate — see
            # equality_flags_str()'s own comment). Hidden — purely a data
            # channel to the client, never shown itself.
            flags_id <- paste0("eq_approx_flags_", ii)
            output[[flags_id]] <- renderText({
              spec <- join_specs(input[[paste0("textin_relations_", ii)]], input$add_for_all_models)
              equality_flags_str(spec, equality_tolerances_reactive$value[[as.character(ii)]])
            })
            # This output lives in a display:none wrapper (it's a pure
            # data channel to the client-side poll, never shown itself) —
            # Shiny suspends computing outputs it thinks aren't visible by
            # default, which would otherwise mean it never actually runs.
            outputOptions(output, flags_id, suspendWhenHidden = FALSE)

            # Same idea, but scoped to JUST this model's Unique Model
            # Specification box (not the Unique+Shared combined text) —
            # read by the editable-box overlay script (see wireBox()'s
            # substForBox()) so that box shows "≈" too, not only the
            # read-only Model Specification preview above. Since
            # join_specs() puts the unique text first ("unique; shared"),
            # the k-th "=" clause in the unique text alone is always the
            # SAME k-th entry of this model's tolerance vector — no
            # separate storage needed, just a shorter clause list.
            flags_unique_id <- paste0("eq_approx_flags_unique_", ii)
            output[[flags_unique_id]] <- renderText({
              equality_flags_str(input[[paste0("textin_relations_", ii)]], equality_tolerances_reactive$value[[as.character(ii)]])
            })
            outputOptions(output, flags_unique_id, suspendWhenHidden = FALSE)

            # Comma-separated 1/0 flags, one per actual H-rep ROW in the
            # Model Specification box (see model_redundant_clause_flags's
            # own comment on why this is per-row, not per typed clause —
            # a shortcut clause's rows don't all have to be redundant
            # together) — "1" means that row turned out to be logically
            # redundant given the model's H-representation. Empty string
            # when nothing should be highlighted (no H-rep yet, or the
            # row-to-clause mapping couldn't be trusted for this model).
            # Read by the same client-side poll that already recolors this
            # text for ≥/≤/≈.
            redundant_flags_id <- paste0("redundant_flags_", ii)
            output[[redundant_flags_id]] <- renderText({
              spec <- join_specs(input[[paste0("textin_relations_", ii)]], input$add_for_all_models)
              model_name <- input[[paste0("textin_relations_name", ii)]] %||% paste0("m", ii)
              flags <- model_redundant_clause_flags(model_name, spec)
              if (is.null(flags)) "" else paste(ifelse(flags, "1", "0"), collapse = ",")
            })
            outputOptions(output, redundant_flags_id, suspendWhenHidden = FALSE)

            # Tells the client-side substitution poll (near the top of the
            # UI) to leave this row's Model Specification pre completely
            # alone — for a mixture (or mixture-involving intersection),
            # model_spec_display_i's own text IS the real content (the
            # placeholder description), not raw constraint text to
            # re-derive ≥/≤/≈ from; without this the poll would overwrite
            # it with whatever's in the (empty) hidden textarea instead.
            placeholder_flag_id <- paste0("model_spec_is_placeholder_", ii)
            output[[placeholder_flag_id]] <- renderText({
              if (!is.null(mixture_model_flags$value[[as.character(ii)]])) "1" else "0"
            })
            outputOptions(output, placeholder_flag_id, suspendWhenHidden = FALSE)
          })
          tags$div(
            class = paste("fairy-model-row", if (i %% 2 == 0) "fairy-row-even" else "fairy-row-odd"),
            `data-row-idx` = i,
            # The hidden textarea is pulled fully out of the visual flow
            # via absolute positioning + zero size, so it cannot affect
            # the layout of the label/box below it regardless of anything
            # else on this page.
            tags$textarea(
              id = paste0("textin_relations_complete_", i),
              class = "form-control",
              style = "position:absolute; width:0; height:0; padding:0; margin:0; border:0; overflow:hidden; opacity:0; pointer-events:none;",
              `aria-hidden` = "true",
              tabindex = "-1",
              readonly = "readonly",
              AllInputs()[[paste0("textin_relations_", i)]] %||% ""
            ),
            # Plain flow — label then box, nothing exotic. Now that the
            # row itself is a plain table (see above), there's no more
            # need for the label/box to be independently pixel-pinned;
            # ordinary block stacking is enough.
            # (The approximate-tolerance shortcut for this model's
            # equalities lives next to Model Name now, alongside the
            # delete button — see textboxes_relations_name().)
            # Header shown once, above the first model only.
            if (i == 1) tags$label(paste0("Model Specification"), class = "control-label"),
            # Outer div is a hard overflow:hidden boundary at the fixed
            # box size — see .fairy-model-spec-outer's own comment for why
            # (a Safari-specific scrollbar-space quirk).
            tags$div(
              class = "fairy-model-spec-outer",
              verbatimTextOutput(paste0("model_spec_display_", i))
            ),
            tags$div(
              style = "display:none;",
              textOutput(paste0("eq_approx_flags_", i))
            ),
            tags$div(
              style = "display:none;",
              textOutput(paste0("redundant_flags_", i))
            ),
            tags$div(
              style = "display:none;",
              textOutput(paste0("model_spec_is_placeholder_", i))
            )
          )
        })
      })
    }
  })


  textboxes_check <- reactive({
    n <- counter_input$n
    if (n > 0) {
      ({
        lapply(seq_len(n), function(i) {
          materialSwitch(paste0("textin_include_v_", i),
            label = HTML(paste("Include V-repres. for ",
              (AllInputs()[[paste("textin_relations_name", i, sep = "")]]),
              sep = ""
            )),
            status = "primary",
            value = AllInputs()[[paste0("textin_include_v_", i)]],
            right = T
          )
        })
      })
    }
  })

  textboxes_inter_check <- reactive({
    if ((input$show_inter) != 0) {
      inters_models <- hot_to_r(input$mytable_inter)

      n <- nrow(inters_models)

      if (n > 0) {
        ({
          lapply(seq_len(n), function(i) {
            materialSwitch(paste0("textin_include_inter_v_", i),
              label = HTML(paste("Include V-repres. for ",
                inters_models[i, 1],
                sep = ""
              )),
              status = "primary",
              value = AllInputs()[[paste0("textin_include_inter_v_", i)]],
              right = T
            )
          })
        })
      }
    }
  })


  textboxes_relations_name <- reactive({
    n <- counter_input$n

    if (n > 0) {
      isolate({
        pending <- pending_relations_name_restore()
        lapply(seq_len(n), function(i) {
          restored <- if (!is.null(pending) && i <= length(pending) &&
                          !is.na(pending[i]) && nchar(pending[i]) > 0) pending[i] else NULL

          # This model's V-representation on/off button is its own small
          # reactive output (not just plain HTML like the other two icons)
          # because its color needs to track mytable_v_reactive$value —
          # the same shared state the "V-representations" dialog reads
          # and writes — so ticking it in either place stays in sync.
          local({
            ii <- i
            output[[paste0("vrep_toggle_ui_", ii)]] <- renderUI({
              # A mixture's V-representation is always built as part of
              # constructing the mixture itself (see the helpText in the
              # "Mixture model" dialog) — shown permanently "on" but
              # disabled, since there's nothing to actually toggle. A
              # "Multiple items" row's disabled Unique Model Specification
              # box reuses this SAME mixture_model_flags mechanism (see
              # submit_items_multi's own comment) purely for the visual
              # "disabled, auto-generated text" styling — its
              # V-representation is a real, independently computed thing
              # a user may genuinely want to include/exclude, unlike a
              # mixture's, so item_model_spec_idx excludes it from this
              # forced-on-and-disabled branch.
              if (!is.null(mixture_model_flags$value[[as.character(ii)]]) &&
                  is.null(item_model_spec_idx$value[[as.character(ii)]])) {
                return(shinyjs::disabled(
                  actionButton(
                    paste0("vrep_toggle_btn_", ii), label = NULL, icon = icon("cube"),
                    class = "btn action-button fairy-vrep-toggle-btn active fairy-tooltip",
                    `data-tooltip` = "Always included for mixtures",
                    style = "padding:2px 7px; font-size:11px; border-radius:6px; min-width:0;"
                  )
                ))
              }
              model_name <- input[[paste0("textin_relations_name", ii)]] %||% paste0("m", ii)
              vtab <- mytable_v_reactive$value
              active <- !is.null(vtab) && nrow(vtab) > 0 &&
                model_name %in% vtab[[1]][vtab[[2]] == TRUE]
              actionButton(
                paste0("vrep_toggle_btn_", ii), label = NULL, icon = icon("cube"),
                class = paste("btn action-button fairy-vrep-toggle-btn fairy-tooltip", if (active) "active" else ""),
                `data-tooltip` = if (active) "Included in V-representation — click to remove" else "Include this model in V-representation computation",
                style = "padding:2px 7px; font-size:11px; border-radius:6px; min-width:0;"
              )
            })
          })

          tags$div(
            class = paste("fairy-model-row", if (i %% 2 == 0) "fairy-row-even" else "fairy-row-odd"),
            `data-row-idx` = i,
            # flex-start, not stretch: a repeated-items row's icon column
            # can hold up to 5 buttons (delete/cube/edit/preview/copy)
            # stacked vertically — taller than a plain row's 2-3 and
            # taller than the Model Name box's own fixed height. Stretch
            # would force THIS row's Model Name/spec boxes to grow to
            # match its own (taller) icon column, leaving every
            # repeated-items row visibly bigger than a plain model row
            # right next to it — exactly the "different sized boxes"
            # mismatch reported. flex-start instead lets each child keep
            # its own natural height: the boxes stay the same fixed size
            # on every row regardless of icon count, and a tall icon
            # column simply extends a little past the box's bottom edge
            # instead of dragging the box down with it.
            style = "display:flex; align-items:flex-start; gap:6px;",
            # All per-model icon buttons (delete, V-representation on/off,
            # and — once this model has an "=" — the approximate-tolerance
            # shortcut) live together to the left of the Model Name box
            # instead of scattered across the row, so there is one place
            # to look for "actions on this model".
            tags$div(
              class = "fairy-model-icon-col",
              # Plain single-column stack, same as always — a 2-column
              # grid was tried to keep a repeated-items row's up-to-5
              # icons from overflowing its box, but packing small round
              # icon buttons into a cramped 2-wide block looked worse
              # than the overflow it was fixing. Simpler fix: the boxes
              # themselves (Model Name / Unique Model Specification /
              # Model Specification — see their own height=, all kept in
              # sync) are now tall enough for a single column of 5 icons
              # to fit without overflowing OR needing to stretch per-row,
              # so every row gets the same uniform (taller) box height
              # regardless of its own icon count.
              style = paste0(
                "display:flex; flex-direction:column; align-items:center; gap:4px; padding-top:",
                if (i == 1) "24px" else "4px", ";",
                if (i == 1) " position:relative;" else ""
              ),
              # Master V-rep include/exclude-all toggle floats directly
              # above the first row's own per-model cube button (rather
              # than off in the "Model(s)" header toolbar) so it visually
              # reads as "applies to the whole column below it". Absolutely
              # positioned (not a normal flow sibling) specifically so it
              # adds no height to row 1's icon column — an earlier, in-flow
              # version made row 1 taller than every other row, which then
              # stretched row 1's Model Name / Unique Model Specification
              # boxes taller too (fairy-model-row uses align-items:stretch)
              # and broke the uniform row height every other row still had.
              # Only makes sense with 2+ models — "toggle everything" is a
              # no-op (identical to the single per-model button) with one.
              if (i == 1 && n > 1) tags$div(
                # left:0 (not centered) — this pair is wider than row 1's
                # own narrow icon column, so centering it over that column
                # pushed its left half out past the card's own left edge
                # (visibly hanging off the panel border). Left-aligned to
                # the icon column's own edge instead, growing rightward
                # into the empty space above Model Name, keeps the whole
                # pair inside the card no matter how many buttons it has.
                style = "position:absolute; top:-40px; left:0; display:flex; align-items:center; gap:6px;",
                tags$span(class = "fairy-tooltip",
                  `data-tooltip` = "Include or exclude every model's V-representation at once",
                  uiOutput("vrep_toggle_all_ui", inline = TRUE)),
                # Same n>1 gate as the V-rep master toggle right next to
                # it — "delete everything" is meaningless with only one
                # model already (and delete_model_at_idx's own n<=1
                # branch, which this ultimately still ends on, exists
                # specifically to reset that LAST row rather than leave
                # zero — not to be reachable directly from here). Same
                # first-appearance pulse as that button too — id-based
                # consume-once check, same as vrep_toggle_all_appear_id's
                # own comment (a one-shot boolean flag isn't enough:
                # this render pass can be re-read many times with no new
                # transition, e.g. every resume from tab-hidden
                # suspension, and needs to remember which id it already
                # showed a pulse for).
                (function() {
                  cur_id_del <- del_all_models_appear_id()
                  just_appeared <- cur_id_del > isolate(del_all_models_last_shown_id())
                  if (just_appeared) del_all_models_last_shown_id(cur_id_del)
                  btn <- tags$span(class = "fairy-tooltip",
                    `data-tooltip` = "Delete all models",
                    tags$button(
                      id = "delete_all_models_btn", class = "fairy-del-all-models-btn",
                      type = "button",
                      style = "cursor:pointer; font-size:14px; color:#c0392b;",
                      HTML("&#128465;")
                    )
                  )
                  if (just_appeared) tags$span(class = "fairy-intro-pulse-wrap", btn) else btn
                })(),
                # Fills the empty space that otherwise just sits there next
                # to these two buttons with a bit of context for what they
                # act on — its own renderUI (not computed inline here) so
                # it reactively updates on its own whenever model count or
                # V-representation inclusion changes, not just on a full
                # row re-render.
                uiOutput("model_count_summary_ui", inline = TRUE)
              ),
              tags$button(
                class = "fairy-del-model-btn fairy-tooltip", type = "button",
                `data-tooltip` = "Delete this model",
                `data-model-idx` = i,
                style = "border:none; background:transparent; cursor:pointer; font-size:14px; color:#c0392b; padding:0;",
                HTML("&#128465;")
              ),
              uiOutput(paste0("vrep_toggle_ui_", i), inline = TRUE),
              # Reopens the "Multiple items" modal pre-filled with this
              # row's current settings (see item_model_spec_idx and
              # open_items_modal) — only shown on rows actually created
              # that way, so a settings change can be made after the
              # fact instead of only at creation time.
              if (!is.null(item_model_spec_idx$value[[as.character(i)]])) actionButton(
                paste0("items_edit_btn_", i), label = NULL, icon = icon("sliders"),
                class = "btn action-button fairy-eq-tol-btn fairy-tooltip",
                `data-tooltip` = "Edit repeated-items settings",
                style = "padding:2px 7px; font-size:11px; border-radius:6px; min-width:0;"
              ),
              # Shows the generated model as plain p1, p2, ... constraint
              # text — "as if freshly typed" — plus a legend for what each
              # of those stands for, on click (see
              # observeEvent(input$items_preview_btn_i, ...)). Computed
              # fresh at click time rather than cached anywhere, so it's
              # never stale and always reflects whether the base model has
              # actually been computed yet.
              if (!is.null(item_model_spec_idx$value[[as.character(i)]])) actionButton(
                paste0("items_preview_btn_", i), label = NULL, icon = icon("keyboard"),
                class = "btn action-button fairy-eq-tol-btn fairy-tooltip",
                `data-tooltip` = "Show as typed input",
                style = "padding:2px 7px; font-size:11px; border-radius:6px; min-width:0;"
              ),
              # Duplicates this row into a new, independent model row —
              # its repeated-items spec too, if it has one — a starting
              # point for trying a variant without losing or overwriting
              # the original. Not shown for an intersection/mixture row,
              # same reasoning as the eq-tolerance button just below: its
              # content is derived from source models, not a standalone
              # spec there's anything meaningful to copy.
              if (is.null(derived_model_sources$value[[as.character(i)]])) actionButton(
                paste0("copy_model_btn_", i), label = NULL, icon = icon("copy"),
                class = "btn action-button fairy-eq-tol-btn fairy-tooltip",
                `data-tooltip` = "Copy this model to a new row",
                style = "padding:2px 7px; font-size:11px; border-radius:6px; min-width:0;"
              ),
              # Not shown for an intersection model — its approximate
              # tolerances are carried over from (and kept live-synced
              # with) its source models automatically, see
              # derived_model_sources; a manual override here would just
              # get overwritten the next time a source changes.
              if (is.null(derived_model_sources$value[[as.character(i)]])) conditionalPanel(
                condition = paste0(
                  "input.textin_relations_complete_", i,
                  " && input.textin_relations_complete_", i, ".indexOf('=') > -1"
                ),
                actionButton(
                  paste0("eq_tol_btn_", i), label = NULL, icon = icon("equals"),
                  class = "btn action-button fairy-eq-tol-btn fairy-tooltip",
                  `data-tooltip` = "Set approximate tolerance for equalities in this model",
                  style = "padding:2px 7px; font-size:11px; border-radius:6px; min-width:0;"
                )
              )
            ),
            tags$div(
              style = "flex: 1; min-width: 0;",
              textAreaInput(
                inputId = paste0("textin_relations_name", i),
                label = if (i == 1) "Model Name" else tags$span(class = "sr-only", paste0("Model Name (row ", i, ")")),
                value = if (!is.null(restored)) restored else ifelse(
                  is.null(AllInputs()[[paste0("textin_relations_name", i)]]) == TRUE,
                  paste0("m", i),
                  AllInputs()[[paste0("textin_relations_name", i)]]
                ),
                width = "100%",
                height = "130px",
                resize = "none"
              )
            )
          )
        })
      })
    }
  })

  output$textbox_ui_min <- renderUI({
    textboxes_min()
  })
  output$textbox_ui_name <- renderUI({
    textboxes_names()
  })
  output$textbox_ui_max <- renderUI({
    textboxes_max()
  })
  output$textbox_ui_rel <- renderUI({
    textboxes_relations()
  })
  output$textbox_ui_rel_complete <- renderUI({
    textboxes_relations_complete()
  })
  output$textbox_ui_check <- renderUI({
    textboxes_check()
  })

  output$textbox_ui_inter_check <- renderUI({
    textboxes_inter_check()
  })


  output$textbox_ui_repl <- renderUI({
    textboxes_repl()
  })

  #### Display Intersection

  output$checkbox_ui_row <- renderUI({
    check_box_row()
  })
  output$textbox_ui_name_rel <-
    renderUI({
      textboxes_relations_name()
    })


  #### Download ####

  output$download <- downloadHandler(
    filename = function() {
      paste0("user_input_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      missing_table <- data.frame("none")
      colnames(missing_table) <- ""

      mytable_v <- mytable_v_reactive$value
      if (is.null(mytable_v)) {
        mytable_v <- missing_table
      }

      name_probs <- numeric()

      for (loop_save in seq_len(counter$n)) {
        name_probs <- c(name_probs, isolate(input[[paste0("textin_name_", loop_save)]]))
      }

      name_models <- numeric()

      for (loop_save in seq_len(counter_input$n)) {
        name_models <- c(name_models, isolate(input[[paste0("textin_relations_name", loop_save)]]))
      }

      input_models <- numeric()

      for (loop_save in seq_len(counter_input$n)) {
        input_models <- c(input_models, isolate(input[[paste0("textin_relations_", loop_save)]]))
      }

      shared_input_model <- AllInputs()$add_for_all_models

      # Per-equality tolerances (one number per individual "=" occurrence,
      # keyed by model index) — see equality_tolerances_reactive's own
      # comment. Nested/nonuniform shape, so JSON like replication/items
      # below rather than a flat table like the old per-model version of
      # this sheet.
      equality_tol_json <- toJSON(equality_tolerances_reactive$value, auto_unbox = TRUE, null = "null")

      # Model intersections / mixtures are flat data.frames (rows = pairs,
      # columns = which models are selected), so they round-trip through
      # xlsx directly. Replication / multi-item specs are nested R lists
      # (base model + factor definitions + parameter-to-type assignments,
      # not a flat table), so they're serialized to a single JSON cell
      # instead — write_xlsx/read_excel can't represent nested structures.
      mytable_int   <- mytable_int_reactive$value
      if (is.null(mytable_int)) mytable_int <- missing_table

      mytable_mix   <- mytable_mix_reactive$value
      if (is.null(mytable_mix)) mytable_mix <- missing_table

      # param_factors and eps_per_param are both NAMED vectors (p1/p2/p3
      # -> type index / -> tolerance). toJSON serializes a bare named
      # atomic vector as an array, dropping the names — convert both to
      # named lists first so they round-trip as JSON objects instead,
      # preserving which parameter maps to which value.
      items_for_json <- lapply(mytable_items_reactive$value, function(s) {
        s$param_factors <- as.list(s$param_factors)
        if (!is.null(s$eps_per_param)) s$eps_per_param <- as.list(s$eps_per_param)
        s
      })
      items_json <- toJSON(items_for_json, auto_unbox = TRUE, null = "null")

      # These three are only ever written by the "Intersection model" /
      # "Mixture model" / "Multiple items" buttons (not saved anywhere
      # before this) — without them, a restored mixture-model row loses
      # the state that actually drives its convex-hull computation
      # (its Unique Model Specification is deliberately left empty by
      # design, so the row would otherwise come back as just a blank,
      # non-computing model — see mixture_model_flags's own comment),
      # an intersection-model row loses its live-sync-with-sources
      # behavior, and a multi-item row loses its "reopen settings" edit
      # button. Same named-list-keyed-by-row-index-string shape as
      # equality_tolerances_reactive above, so the same toJSON/fromJSON
      # round-trip already proven for that applies here unchanged.
      derived_sources_json <- toJSON(derived_model_sources$value, auto_unbox = TRUE, null = "null")
      mixture_flags_json   <- toJSON(mixture_model_flags$value, auto_unbox = TRUE, null = "null")
      item_spec_idx_json   <- toJSON(item_model_spec_idx$value, auto_unbox = TRUE, null = "null")

      out <- list(
        "V-description" = mytable_v,
        "Number of models" = data.frame(counter_input$n),
        "Number of probabilitiy" = data.frame(counter$n),
        "Names of probabilities" = data.frame(name_probs),
        "Name of models" = data.frame(name_models),
        "Unique input models" = data.frame(input_models),
        "Shared input models" = data.frame(shared_input_model),
        "Approximately identical equalities" = data.frame(json = as.character(equality_tol_json)),
        "Model intersections" = mytable_int,
        "Model mixtures" = mytable_mix,
        "Multi-item models" = data.frame(json = as.character(items_json)),
        "Derived model sources" = data.frame(json = as.character(derived_sources_json)),
        "Mixture model flags" = data.frame(json = as.character(mixture_flags_json)),
        "Item model spec idx" = data.frame(json = as.character(item_spec_idx_json))
      )

      write_xlsx(out, file)
    }
  )
  # Without this, Shiny SUSPENDS this output (default suspendWhenHidden
  # = TRUE, checked per DOM element) rather than ever sending it a real
  # value — this button's actual DOM home is .fairy-primary-controls-src
  # (permanently display:none — see its own comment), only ever made
  # VISIBLE by being manually reparented into #sidebar-fab-panel via
  # JS (updateFab()), which Shiny's own visibility tracking has no way
  # to know about (that's a plain DOM move, not a standard show/hide
  # Shiny listens for). Suspended forever = the client never receives
  # the real download URL, so DownloadLinkOutputBinding.renderValue()
  # (see shiny.js) never fires to set the real href/clear the disabled
  # class — clicking it instead falls back to the browser's own default
  # for an empty href with a download attribute: downloading the
  # CURRENT PAGE as an HTML file. That's the actual root cause of "it
  # downloads some html file" / "does not download the input of the
  # app" — not a styling issue, and nothing client-side (no amount of
  # synthetic click/focus priming) can fix a value the server never
  # sends in the first place.
  outputOptions(output, "download", suspendWhenHidden = FALSE)

  #### Upload ####

  # Reconstruct multi-item specs (list of {base, factors, param_factors,
  # types}) — param_factors in particular must come back as a NAMED INTEGER
  # VECTOR (see how it's built in observeEvent(input$submit_items, ...) and
  # consumed via vapply(pf, function(fi) factors[[fi]]$name, ...) elsewhere),
  # not the named list jsonlite would otherwise hand back.
  # Rebuilds the CURRENT items-spec shape: base/factors/param_factors/
  # factor_types/eps_per_param — a type PER FACTOR and a tolerance PER
  # PROBABILITY, not per factor (see build_items_h_mixed's own comment).
  # This used to reconstruct an older, single-"types"-vector-per-spec
  # shape instead (types = a field that was never actually serialized, so
  # it always came back NULL) — every restored repeated-items model
  # silently lost its factor_types/tolerance data, which is what actually
  # drives Group/Type/tolerance, and fell back to legacy handling that
  # isn't reachable via the current UI (see submit_items_multi's own
  # "only the new spec shape is editable" check) — the direct cause of a
  # repeated-items model "looking different" after a download/upload
  # round-trip.
  items_specs_from_json <- function(json_str) {
    raw <- tryCatch(fromJSON(json_str, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(raw) || length(raw) == 0) return(NULL)
    lapply(raw, function(s) {
      pf     <- s$param_factors
      eps_pp <- s$eps_per_param
      list(
        base = s$base,
        factors = lapply(s$factors, function(f) list(name = f$name, n = as.integer(f$n))),
        param_factors = setNames(as.integer(unlist(pf)), names(pf)),
        factor_types = unlist(s$factor_types),
        eps_per_param = if (is.null(eps_pp)) NULL else setNames(as.numeric(unlist(eps_pp)), names(eps_pp))
      )
    })
  }

  # Reconstruct derived_model_sources$value (list, keyed by row index as a
  # string, of {type, source_idx}) — same named-list-of-objects shape as
  # equality_tolerances_reactive, round-tripped the same proven way.
  # source_idx is an integer VECTOR (which existing model rows an
  # intersection/mixture row was built from); fromJSON(simplifyVector =
  # FALSE) hands that back as a list of scalars, not a plain vector.
  derived_sources_from_json <- function(json_str) {
    raw <- tryCatch(fromJSON(json_str, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(raw) || length(raw) == 0) return(list())
    lapply(raw, function(s) list(type = s$type, source_idx = as.integer(unlist(s$source_idx))))
  }

  # mixture_model_flags$value: list, keyed by row index as a string, of a
  # single placeholder-description string ("Mixture of m1, m2").
  mixture_flags_from_json <- function(json_str) {
    raw <- tryCatch(fromJSON(json_str, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(raw) || length(raw) == 0) return(list())
    lapply(raw, function(s) as.character(unlist(s)))
  }

  # item_model_spec_idx$value: list, keyed by row index as a string, of a
  # single integer (that row's index into mytable_items_reactive$value).
  item_spec_idx_from_json <- function(json_str) {
    raw <- tryCatch(fromJSON(json_str, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(raw) || length(raw) == 0) return(list())
    lapply(raw, function(s) as.integer(unlist(s)))
  }

  observeEvent(input$upload, {
    # Block the auto-detect observer (app.R ~2771) from fighting with this
    # restore for the whole restore window — see restoring_upload's own
    # comment for why. Cleared further down once the delayed restore of the
    # name/spec boxes has had time to fully round-trip.
    restoring_upload(TRUE)

    # read_excel() returns a tibble, whose `[` never drops to a bare vector
    # like a plain data.frame's does — code elsewhere (e.g. `which(mytable_v[,
    # 2])`, `sum(tab[, 2])`) expects mytable_v_reactive$value to behave like
    # a normal data.frame column access. Coerce immediately, same fix as for
    # mytable_int_reactive/mytable_mix_reactive below.
    mytable_v_reactive$value <- as.data.frame(read_excel(input$upload$datapath, 1))

    if (length(mytable_v_reactive$value) == 1) {
      mytable_v_reactive$value <- NULL
    }

    # Read everything before touching counters
    new_counter_input_n <- unlist(read_excel(input$upload$datapath, 2))
    new_counter_n       <- unlist(read_excel(input$upload$datapath, 3))

    # write_xlsx/read_excel does NOT round-trip an empty string as an
    # empty string — it comes back as NA (confirmed directly: writing
    # c("p1>p2", "", "p3>p4") and reading it back gives c("p1>p2", NA,
    # "p3>p4")). Every one of these four is routinely blank in practice
    # (an unnamed probability, a model with no Unique Model
    # Specification, an empty Shared Model Specification — the last one
    # especially, since most models never set one) — restoring the NA
    # as-is fed straight into updateTextAreaInput()/pending_*_restore(),
    # which show it as the literal text "NA" instead of leaving the box
    # empty. Direct cause of a model "looking different" after a
    # download/upload round-trip.
    blank_na <- function(x) { x[is.na(x)] <- ""; x }

    name_probs <- blank_na(unlist(read_excel(input$upload$datapath, 4)))
    names(name_probs) <- NULL

    name_models <- blank_na(unlist(read_excel(input$upload$datapath, 5)))
    names(name_models) <- NULL

    input_models <- blank_na(unlist(read_excel(input$upload$datapath, 6)))
    names(input_models) <- NULL

    shared_input_model <- blank_na(unlist(read_excel(input$upload$datapath, 7)))
    names(shared_input_model) <- NULL

    # Sheet 8 used to be a flat "one tolerance per model" table (the old
    # manual "Approximate equalities" dialog); it's now a single JSON cell
    # holding equality_tolerances_reactive$value (one tolerance per
    # individual "=" occurrence, keyed by model index) — same
    # nested-structure-via-JSON approach as replication/items below. A
    # save file from before this change has the old flat-table shape
    # there instead, which fromJSON() will fail to parse — caught here so
    # older files still load, just without their approx settings, same
    # graceful-degradation as the sheets-9-12-missing case below.
    equality_tolerances_reactive$value <- tryCatch({
      raw <- fromJSON(read_excel(input$upload$datapath, 8)$json[1], simplifyVector = FALSE)
      lapply(raw, function(x) as.numeric(unlist(x)))
    }, error = function(e) list())

    # Sheets 9-11 (intersections/mixtures/items) were added
    # after the original save format — wrap in tryCatch so save files from
    # before this existed still load instead of erroring out entirely.
    mytable_int_reactive$value <- tryCatch({
      # read_excel() returns a tibble. Unlike a plain data.frame, tibble's
      # `[` never drops to a bare vector, so prev[k, cn] elsewhere (e.g.
      # observeEvent(input$show_intersections, ...)) would return a 1x1
      # tibble instead of a scalar TRUE/FALSE — and isTRUE() on a tibble is
      # always FALSE regardless of its actual value, silently discarding
      # every restored checkbox selection. Coerce to a plain data.frame so
      # it behaves exactly like the in-session (non-upload) code path that
      # builds this table via observeEvent(input$submit_int, ...).
      tab <- as.data.frame(read_excel(input$upload$datapath, 9))
      if (length(tab) == 1) NULL else tab
    }, error = function(e) NULL)

    mytable_mix_reactive$value <- tryCatch({
      tab <- as.data.frame(read_excel(input$upload$datapath, 10))
      if (length(tab) == 1) NULL else tab
    }, error = function(e) NULL)

    mytable_items_reactive$value <- tryCatch({
      items_specs_from_json(read_excel(input$upload$datapath, 11)$json[1])
    }, error = function(e) NULL)

    # Sheets 12-14 (added after sheets 9-11) — same tryCatch/graceful-
    # degradation pattern for save files from before these existed.
    # Without these, a restored mixture-model row silently loses the
    # state driving its actual computation (see the download side's own
    # comment); intersection-model rows lose their live-source-sync;
    # multi-item rows lose their "reopen settings" edit button.
    derived_model_sources$value <- tryCatch({
      derived_sources_from_json(read_excel(input$upload$datapath, 12)$json[1])
    }, error = function(e) list())

    mixture_model_flags$value <- tryCatch({
      mixture_flags_from_json(read_excel(input$upload$datapath, 13)$json[1])
    }, error = function(e) list())

    item_model_spec_idx$value <- tryCatch({
      item_spec_idx_from_json(read_excel(input$upload$datapath, 14)$json[1])
    }, error = function(e) list())

    # Each modal's row count is a SEPARATE counter (counter_int$n,
    # counter_mix$n, ...) from the actual data just restored above — the
    # modal only renders seq_len(counter$n) rows regardless of how many
    # rows/specs the restored table actually has. Without updating these,
    # reopening a modal after upload would only show however many rows the
    # counter happened to be at, hiding the rest of what was restored.
    counter_int$n   <- max(1, if (!is.null(mytable_int_reactive$value))   nrow(mytable_int_reactive$value)     else 1)
    counter_mix$n   <- max(1, if (!is.null(mytable_mix_reactive$value))   nrow(mytable_mix_reactive$value)     else 1)
    counter_items$n <- max(1, if (!is.null(mytable_items_reactive$value)) length(mytable_items_reactive$value) else 1)

    # Stash the restored names/specs so the renders that counter_input$n and
    # counter$n are about to trigger (below) can use them directly instead
    # of blank/default boxes — see pending_name_restore's own comment.
    pending_name_restore(name_probs)
    pending_relations_restore(input_models)
    pending_relations_name_restore(name_models)

    # Update counters — this triggers Shiny to re-render the input rows
    counter_input$n <- new_counter_input_n
    counter$n       <- new_counter_n

    # Still update via updateTextAreaInput too, as a fallback for any box
    # that existed (and so wasn't freshly rendered) before this upload, and
    # to restore the other input types below.
    shinyjs::delay(400, {
      pending_name_restore(NULL)
      pending_relations_restore(NULL)
      pending_relations_name_restore(NULL)
      for (loop_load in seq_len(length(name_probs))) {
        updateTextAreaInput(session,
          inputId = paste0("textin_name_", loop_load),
          value   = name_probs[loop_load]
        )
      }
      for (loop_load in seq_len(length(name_models))) {
        updateTextAreaInput(session,
          inputId = paste0("textin_relations_name", loop_load),
          value   = name_models[loop_load]
        )
      }
      for (loop_load in seq_len(length(input_models))) {
        updateTextAreaInput(session,
          inputId = paste0("textin_relations_", loop_load),
          value   = input_models[loop_load]
        )
      }
      updateTextAreaInput(session,
        inputId = "add_for_all_models",
        value   = shared_input_model
      )
    })

    # Release the guard well after the restored spec-text boxes have both
    # round-tripped back to the server (400ms delay above) and let the
    # 800ms-debounced auto-detect observer settle on them, so it doesn't
    # fire mid-restore and clobber a just-restored name.
    shinyjs::delay(1600, {
      restoring_upload(FALSE)
    })
  })

  #### Function LaTeX ####

  latex <- function(latex_object) {
    formula_h <- ""

    latex_object[, 3:ncol(latex_object)] <- (-1 * latex_object[, 3:ncol(latex_object)])
    h_representation_pl_minus <- latex_object

    sign_p <- ifelse(data.frame(h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)]) < 0, "-", "+")
    sign_p <- ifelse(data.frame(h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)]) == 0, "", sign_p)

    h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)] <-
      abs(h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)])

    for (loop_pl in seq_len(nrow(latex_object))) {
      formula_h_act <- ((as.character(h_representation_pl_minus[loop_pl, ])))

      sign_p_act <- sign_p[loop_pl, ]

      sign_pl <- ifelse(formula_h_act[1] == "1", "=", "\\leq")

      col_names_p <- colnames(h_representation_pl_minus)[3:ncol(h_representation_pl_minus)]

      p_with_factors <- paste(formula_h_act[3:(length(formula_h_act))], " \\times  ", col_names_p,
        sep =
          ""
      )

      p_with_factors[formula_h_act[3:(length(formula_h_act))] == "0"] <- ""



      p_with_factors <- paste(sign_p_act, " & ", p_with_factors, sep = "")


      p_with_factors <- ifelse(col_names_p == "", " & ", p_with_factors)


      p_with_factors <- paste(p_with_factors, " & ", collapse = "")

      p_with_factors <- paste(p_with_factors, sign_pl, " & ", formula_h_act[2],
        collapse =
          ""
      )

      p_with_factors <- paste(paste(p_with_factors, collapse = " "), " \\\\",
        collapse =
          " "
      )


      formula_h <- paste(c(formula_h, p_with_factors), collapse = " ")
    }

    formula_h
  }

  #### Function Plot ####

  # Adds one model's marker + surface trace(s) to a Plot Polytope(s) figure.
  # matrix_pl: data.frame of the model's vertices, already subset to the 3
  # currently-selected plot dimensions. Returns the updated plotly object.
  add_model_trace <- function(p, matrix_pl, model_col, name_model,
                               mesh_opacity, mesh_opacity_hull) {
    trace1 <- list(
      mode = "markers",
      type = "scatter3d",
      x = (matrix_pl[, 1]),
      y = (matrix_pl[, 2]),
      z = (matrix_pl[, 3])
    )

    trace2 <- list(
      type = "mesh3d",
      x = (matrix_pl[, 1]),
      y = (matrix_pl[, 2]),
      z = (matrix_pl[, 3]),
      opacity = 0.05,
      alphahull = 0
    )

    ### check dimensionality using principal component analysis

    prcomp_sol <- summary(prcomp(matrix_pl))$importance[2, ]
    shape_point <- ifelse(sum(prcomp_sol != 0) == 1, 1, 0)
    shape_point <- ifelse(nrow(matrix_pl) == 1, 1, shape_point)
    shape_cube <- ifelse(sum(prcomp_sol != 0) == 3, 1, 0)
    shape_cube <- ifelse(shape_point == 1, 0, shape_cube)

    if (shape_point != 1) {
      p <-
        add_trace(
          p,
          mode = trace1$mode,
          type = trace1$type,
          x = trace1$x,
          y = trace1$y,
          z = trace1$z,
          marker = list(color = model_col, size = 4),
          name = name_model,
          legendgroup = name_model
        )

      # Plotly's own triangulation (alphahull=0 for the cube case, a
      # per-axis Delaunay stack otherwise) can silently draw nothing or
      # produce wrong/crossing facets for some point sets — the same
      # issue diagnosed and fixed for the overlap-intersection mesh
      # below. Compute the exact convex hull ourselves and hand Plotly
      # explicit face indices, for both shapes, for consistency and
      # correctness; only fall back to the per-axis approximation if
      # the point set is degenerate (e.g. exactly coplanar) and has no
      # true 3D convex hull.
      hull_faces <- tryCatch(
        geometry::convhulln(as.matrix(matrix_pl)),
        error = function(e) NULL
      )
      if (!is.null(hull_faces)) {
        p <-
          add_trace(
            p,
            type = "mesh3d",
            x = trace2$x, y = trace2$y, z = trace2$z,
            i = hull_faces[, 1] - 1, j = hull_faces[, 2] - 1, k = hull_faces[, 3] - 1,
            opacity = if (shape_cube == 1) mesh_opacity else mesh_opacity_hull,
            color = I(model_col),
            name = name_model,
            legendgroup = name_model,
            showlegend = FALSE
          )
      } else {
        for (d_axis in c("x", "y", "z")) {
          p <-
            add_trace(
              p,
              type = trace2$type,
              x = trace2$x,
              y = trace2$y,
              z = trace2$z,
              opacity = mesh_opacity_hull,
              color = I(model_col),
              alphahull = -1,
              delaunayaxis = d_axis,
              name = name_model,
              legendgroup = name_model,
              showlegend = FALSE
            )
        }
      }
    } else {
      p <-
        add_trace(
          p,
          x = trace1$x,
          y = trace1$y,
          z = trace1$z,
          type = "scatter3d",
          mode = "lines+markers",
          opacity = 1,
          line = list(width = 4, color = model_col),
          marker = list(color = model_col),
          name = name_model,
          legendgroup = name_model
        )
    }
    p
  }

  # Adds the "Highlight model overlaps" traces (intersection markers + hull
  # surface for each currently-plotted pair) to a Plot Polytope(s) figure.
  # Reuses the intersections the user selected in the "Model intersections"
  # modal (already computed as H-descriptions and stored in h_reactive).
  # Only pairs whose BOTH models are currently plotted are drawn. Vertices
  # are enumerated once per set and cached, so changing plot axes does not
  # recompute them.
  add_overlap_traces <- function(p, select_models_to_plot, select_plot, dim_names) {
    int_names_all <- isolate(intersections_reactive$names)
    if (is.null(int_names_all)) int_names_all <- character()

    # keep only intersections whose member models are all plotted
    int_names_plot <- int_names_all[vapply(int_names_all, function(nm) {
      parts <- unlist(str_split(nm, fixed(" AND ")))
      length(parts) >= 2 && all(parts %in% select_models_to_plot)
    }, logical(1))]

    # An intersection that has its own V-description is already drawn by
    # the model loop above; do not draw it a second time as a highlight.
    int_names_plot <- int_names_plot[!(int_names_plot %in% select_models_to_plot)]

    if (length(int_names_plot) == 0) return(p)

    model_key <- paste(sort(int_names_plot), collapse = "|")

    if (!identical(isolate(overlap_cache_reactive$key), model_key)) {
      overlaps <- list()

      for (nm in int_names_plot) {
        H_int <- isolate(h_reactive[[nm]])
        if (is.null(H_int)) next

        V_int <- tryCatch(scdd(H_int)$output, error = function(e) NULL)
        if (is.null(V_int) || nrow(V_int) == 0) next

        overlaps[[length(overlaps) + 1]] <- list(
          name = nm,
          coords = matrix(as.numeric(q2d(V_int[, 3:ncol(V_int)])),
            ncol = ncol(V_int) - 2
          )
        )
      }

      isolate({
        overlap_cache_reactive$coords <- overlaps
        overlap_cache_reactive$empty <- (length(overlaps) == 0)
        overlap_cache_reactive$key <- model_key
      })
    }

    overlaps <- isolate(overlap_cache_reactive$coords)

    if (isTRUE(isolate(overlap_cache_reactive$empty)) || length(overlaps) == 0) return(p)

    # Distinct colour per overlapping pair.
    overlap_pal <- c(
      "#E31A1C", "#FF7F00", "#6A3D9A", "#B15928", "#E7298A",
      "#1F78B4", "#33A02C", "#A6761D", "#666666"
    )
    for (ov_i in seq_along(overlaps)) {
      ov <- overlaps[[ov_i]]
      ov_col <- overlap_pal[((ov_i - 1) %% length(overlap_pal)) + 1]
      int_pl <- data.frame(ov$coords)[, select_plot, drop = FALSE]
      colnames(int_pl) <- dim_names

      p <- add_trace(p,
        type = "scatter3d", mode = "markers",
        x = int_pl[, 1], y = int_pl[, 2], z = int_pl[, 3],
        marker = list(color = ov_col, size = 5),
        name = ov$name, legendgroup = ov$name, showlegend = TRUE
      )
      if (nrow(int_pl) >= 4) {
        # Plotly's own alphahull=0 triangulation can silently fail to
        # draw any surface for some point sets, and a per-axis
        # Delaunay approximation (three stacked 2D triangulations)
        # can produce spurious crossing facets that don't match the
        # true polytope boundary. Compute the exact convex hull
        # ourselves and hand Plotly explicit face indices instead.
        hull_faces <- tryCatch(
          geometry::convhulln(as.matrix(int_pl)),
          error = function(e) NULL
        )
        if (!is.null(hull_faces)) {
          p <- add_trace(p,
            type = "mesh3d",
            x = int_pl[, 1], y = int_pl[, 2], z = int_pl[, 3],
            i = hull_faces[, 1] - 1, j = hull_faces[, 2] - 1, k = hull_faces[, 3] - 1,
            opacity = 0.45, color = I(ov_col),
            name = ov$name, legendgroup = ov$name, showlegend = FALSE
          )
        }
      }
    }
    p
  }

  function_plot <- function(pl_obj, n_submodels) {
    showTab(inputId = "tabs", target = "Plot Polytope(s)")
    showTab(inputId = "tabs", target = "Plot Edge Cases")



    output$plot <- renderPlotly({
      all_v_rep_in_list <- pl_obj

      models_to_plot <- input$name_model_plot
      models_to_plot <- unlist(str_split(models_to_plot, ";"))
      models_to_plot <- trimws(models_to_plot)
      models_to_plot <- unlist(models_to_plot)

      names_available_models <- rep(NA, n_submodels)

      matrix_pl_all <- numeric()
      expected_ncol <- NULL

      for (loop_pl in seq_len(n_submodels)) {
        extract_name <- all_v_rep_in_list[[loop_pl]]
        this_name <- extract_name[1, 1]

        all_plot_actual <- all_v_rep_in_list[[loop_pl]]
        all_plot_actual <- all_plot_actual[, 2:ncol(all_plot_actual)]

        if (is.null(expected_ncol)) {
          expected_ncol <- ncol(all_plot_actual)
          colnames_v_representation_plot_all <- colnames(all_plot_actual)
        } else if (ncol(all_plot_actual) != expected_ncol) {
          # Different parameter-space size than the first available model,
          # so it can't share matrix_pl_all's columns. Leave its name OUT of
          # names_available_models (not just skip adding its rows) — the
          # previous version recorded the name unconditionally above, so an
          # incompatible model could still be picked in "Plot Polytope(s)",
          # pass the models_to_plot filter below, and end up as a 0-row
          # matrix that crashed prcomp()/the hull code downstream.
          next
        }

        names_available_models[loop_pl] <- this_name

        all_plot_actual <- matrix(as.numeric(q2d(unlist(all_plot_actual))), ncol = ncol(all_plot_actual))

        matrix_pl_all <- rbind(
          matrix_pl_all,
          data.frame(names_available_models[loop_pl], (all_plot_actual))
        )
      }

      colnames(matrix_pl_all) <- c("models", colnames_v_representation_plot_all)

      names_available_models_reactive$value <- names_available_models


      select_models_to_plot <- names_available_models[names_available_models %in% models_to_plot]

      matrix_pl_all <- matrix_pl_all[matrix_pl_all[, 1] %in% select_models_to_plot, ]

      if (length(select_models_to_plot) > 0) {
        for (loop_pl in 1:length(select_models_to_plot)) {
          all_plot_actual <- matrix_pl_all[matrix_pl_all[, 1] == select_models_to_plot[loop_pl], 2:ncol(matrix_pl_all)]

          mixture_v_plot_names <- colnames(all_plot_actual)

          name_model <- select_models_to_plot[loop_pl]
          matrix_pl <- data.frame(all_plot_actual)
          colnames(matrix_pl) <- colnames(all_plot_actual)

          # Distinct colour per model + a visible fill opacity, so where two
          # translucent polytopes overlap the colours blend and the shared
          # region reads darker (Plot Polytope overlap cue).
          model_pal <- c(
            "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7",
            "#0072B2", "#F0E442", "#000000", "#999999"
          )
          model_col <- model_pal[((loop_pl - 1) %% length(model_pal)) + 1]
          # Single convex mesh (cube case) can take a moderate opacity; the
          # hull case below stacks THREE meshes (delaunay x/y/z), so it needs a
          # much lower per-mesh opacity to avoid a muddy near-opaque result.
          mesh_opacity <- 0.35
          mesh_opacity_hull <- 0.12

          select_plot <- as.numeric(c(input$dim_1, input$dim_2, input$dim_3))

          updatePickerInput(
            session,
            inputId = "dim_1",
            choices = (1:ncol(matrix_pl))[-select_plot[c(2, 3)]],
            selected = input$dim_1
          )
          updatePickerInput(
            session,
            inputId = "dim_2",
            choices = (1:ncol(matrix_pl))[-select_plot[c(1, 3)]],
            selected = input$dim_2
          )
          updatePickerInput(
            session,
            inputId = "dim_3",
            choices = (1:ncol(matrix_pl))[-select_plot[c(1, 2)]],
            selected = input$dim_3
          )

          matrix_pl <- matrix_pl[, select_plot]


          dim_names <- mixture_v_plot_names[c(
            as.numeric(input$dim_1),
            as.numeric(input$dim_2),
            as.numeric(input$dim_3)
          )]

          colnames(matrix_pl) <- dim_names


          axx <- list(
            nticks = .1,
            range = c(0, 1),
            title = dim_names[1]
          )

          axy <- list(
            nticks = .1,
            range = c(0, 1),
            title = dim_names[2]
          )

          axz <- list(
            nticks = .1,
            range = c(0, 1),
            title = dim_names[3]
          )

          if (loop_pl == 1) {
            p <- plot_ly(
              colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999"),
              height = 800,
              name = name_model
            )
          }

          p <- add_model_trace(p, matrix_pl, model_col, name_model, mesh_opacity, mesh_opacity_hull)
        }

        overlap_note <- NULL
        p <- add_overlap_traces(p, select_models_to_plot, select_plot, dim_names)

        p %>% layout(
          annotations = if (!is.null(overlap_note)) {
            list(list(
              text = overlap_note, showarrow = FALSE,
              xref = "paper", yref = "paper", x = 0.5, y = 1,
              font = list(color = "#E31A1C", size = 14)
            ))
          } else {
            list()
          },
          scene = list(
            xaxis = axx,
            yaxis = axy,
            zaxis = axz,
            aspectmode = "manual",
            aspectratio = list(x = 1, y = 1, z = 1),
            camera = list(eye = list(
              x = 2, y = 2, z = 2
            ))
          )
        )
      } else {
        # No models selected (e.g. after clicking "-" to clear the list).
        # A NULL return here would leave the previous plot's stale widget on
        # screen — renderPlotly's htmlwidget binding doesn't clear it the
        # way renderPlot does — so explicitly hand back an empty scene.
        plot_ly(type = "scatter3d", mode = "markers") %>%
          layout(scene = list(
            xaxis = list(range = c(0, 1)),
            yaxis = list(range = c(0, 1)),
            zaxis = list(range = c(0, 1))
          ))
      }
    })
  }

  #### Theory-building notes ####
  # Supports the paper's "back and forth between formalization and verbal
  # model" argument (formalization uncovers hidden assumptions; the verbal
  # theory in turn constrains the formal model to its intended scope) by
  # surfacing, right where a model's H-representation is displayed: (a)
  # whether any stated clauses turned out to be logically redundant (a
  # commitment the verbal theory didn't need to make explicit), (b) a plain-
  # language reverse-paraphrase of each constraint (so the user can check
  # the formal model against what they actually meant), and (c) a prompt
  # about exact vs. approximate equality, a common source of ambiguity in
  # verbal theories. Deliberately built on the app's existing, already-
  # proven redundant()/H-representation machinery rather than new custom
  # entailment logic, since we can't interactively verify novel LP code
  # against this app's real data during this session.
  # Wrap a probability name (e.g. "p_{1}") in MathJax inline delimiters so
  # it renders as proper LaTeX rather than literal underscore/brace text.
  mathify <- function(s) paste0("\\(", s, "\\)")

  # A default probability name (e.g. "p_{1}") is valid LaTeX on its own —
  # render it as-is. A user-typed custom label (e.g. "success rate", stored
  # with spaces already turned into underscores) is NOT: math mode renders
  # multi-character names as spaced-out italic variables (each letter
  # treated as its own factor), not as a readable word. Wrap those in
  # \text{...} instead, with underscores turned back into spaces, so they
  # render as the label the user actually typed.
  # Vectorized (ifelse, not if) — every call site so far happened to pass
  # a single name, until the V-representation DT header fix passed a whole
  # column-name vector at once and crashed with "condition has length > 1".
  tex_name <- function(nm) {
    # Items-expanded column names already carry their OWN LaTeX
    # structure — base^{(\text{group},level)}, see cn_factors in
    # build_items_h_core — so only the base portion in front of that
    # structure is a raw probability name that might need \text{}
    # wrapping. The previous version ran the whole string (base AND
    # superscript together) through one check and, for any custom name,
    # wrapped the ENTIRE thing — superscript included — in an outer
    # \text{...}. \text{}'s content is literal text and can't itself
    # contain a nested ^{...}/\text{...} structure, so MathJax failed to
    # parse the result and fell back to showing the raw, unrendered
    # LaTeX source (exactly what got reported for any items-expanded
    # model using custom probability names). Split off the "^{...}"
    # suffix first (if any — a replication name's own outer brace pair,
    # e.g. "{p_{1}}^{(2)}", counts as part of the base, not the suffix,
    # same as the old regex's optional trailing group did), wrap only
    # the base, then reattach the suffix untouched.
    base_of   <- sub("\\^\\{.*$", "", nm)
    suffix_of <- ifelse(grepl("\\^\\{", nm), sub("^[^\\^]+", "", nm), "")
    is_default <- grepl("^\\{?p_\\{[0-9]+\\}\\}?$", base_of)
    base_tex <- ifelse(is_default, base_of, paste0("\\text{", gsub("_", " ", base_of), "}"))
    paste0(base_tex, suffix_of)
  }

  # Same base-name split tex_name uses (an items-expanded column's
  # "^{...}" suffix is which COPY, not which parameter), so every copy
  # of one parameter shares one color regardless of table layout.
  param_base_name <- function(nm) sub("\\^\\{.*$", "", nm)

  # A fixed, colorblind-conscious qualitative palette, cycled by each
  # parameter's position among the app's probabilities — assigns the
  # same color to every occurrence (every item-copy, every model card)
  # of one probability, so e.g. p1 is always the same color whether
  # you're looking at model m1 or m2, not just within one table.
  fairy_param_palette <- c(
    "#2563eb", "#dc2626", "#16a34a", "#d97706", "#9333ea",
    "#0891b2", "#db2777", "#65a30d", "#4f46e5", "#ea580c"
  )
  # global_order: the app's full, current list of probability names (in
  # their display order) — when given, color is keyed by each name's
  # position in THAT list, so it's shared across every model/table.
  # Falls back to per-table position (old behavior) when omitted, so any
  # caller that hasn't been updated to pass it still works.
  param_color_map <- function(p_names, global_order = NULL) {
    base <- param_base_name(p_names)
    if (!is.null(global_order) && length(global_order) > 0) {
      idx <- match(base, global_order)
      missing <- which(is.na(idx))
      if (length(missing) > 0) {
        extra_uniq <- unique(base[missing])
        idx[missing] <- length(global_order) + match(base[missing], extra_uniq)
      }
      return(setNames(fairy_param_palette[((idx - 1) %% length(fairy_param_palette)) + 1], NULL))
    }
    uniq <- unique(base)
    pal  <- fairy_param_palette[((seq_along(uniq) - 1) %% length(fairy_param_palette)) + 1]
    setNames(pal[match(base, uniq)], NULL)
  }

  # Plain-text (NOT LaTeX) column-name label, for contexts that cannot
  # render LaTeX at all — e.g. a plotly axis, which just prints tex_name's
  # raw "\text{...}" source literally instead of typesetting it, since
  # plotly text has no MathJax pass the way the H-rep tables do. Turns
  # "p_{1}^{(\text{g},1)}" into "p1 [g #1]", or "p_{1}" alone into "p1";
  # a SOLO factor (group label == the probability's own name) collapses
  # to "p1 (copy 1)" instead of repeating the name in brackets. Peels the
  # suffix apart with plain string stripping rather than one regex with a
  # nested capture group — a DEFAULT probability name is itself "p_{i}",
  # so a solo factor's group label contains its own {}'s (e.g.
  # "...^{(\text{p_{1}},1)}"), which a single "stop at the first }"
  # capture group cannot see past (confirmed directly: it silently
  # returned "p_1" for every copy instead of "p_1 (copy 1)"/"(copy 2)").
  plain_axis_name <- function(nm) {
    vapply(nm, function(x) {
      if (!grepl("\\^\\{", x)) return(gsub("[{}]", "", x))
      base_of    <- sub("\\^\\{.*$", "", x)
      base_plain <- gsub("[{}]", "", base_of)
      # Strip the fixed "^{(\text{" prefix and ")}" suffix that wrap
      # every items-expanded column (see cn_factors), leaving just
      # "GROUP},LEVEL" — GROUP may still carry its own {}'s (a default
      # probability name), LEVEL never does, so split on the LAST comma.
      inner   <- sub("^\\^\\{\\(\\\\text\\{", "", sub("^[^\\^]+", "", x))
      inner   <- sub("\\)\\}$", "", inner)
      lev     <- sub(".*,([0-9]+)$", "\\1", inner)
      grp     <- gsub("[{}]", "", sub("\\}$", "", sub(",[0-9]+$", "", inner)))
      if (identical(grp, base_plain)) paste0(base_plain, " (copy ", lev, ")")
      else paste0(base_plain, " [", grp, " #", lev, "]")
    }, character(1), USE.NAMES = FALSE)
  }

  # Long decimals like 0.333333333333333 (from thirds, etc.) are hard to
  # read — show them as simplified fractions instead, same convention the
  # H-representation table itself already uses (MASS::fractions). Defined
  # via num2str_lin (declared below in this same server scope, safe to
  # reference here since neither is called until the app is live) so both
  # share its guard against fractions() silently rounding tiny values to 0.
  # round() here is only meant to clean up floating-point noise from LP/
  # rational-arithmetic round-trips (e.g. 0.49999999999997 -> 0.5), not to
  # limit precision — 8 digits was tight enough to truncate a genuinely
  # typed value like 0.000000001 to exactly 0 before num2str_lin ever saw
  # it. 12 digits comfortably clears normal floating-point noise while
  # still preserving values down to ~1e-11.
  frac_str <- function(x) trimws(num2str_lin(round(x, 12)))

  gloss_h_row <- function(row, p_names_lt) {
    is_eq  <- row[1] == 1
    rhs    <- row[2]
    # rcdd stores coefficients as the negation of the constraint's true
    # left-hand side (a = -v internally) — negate back before interpreting,
    # or the direction (>/<) comes out backwards. Verified against rcdd
    # directly: encoding "p1>p2" produces the stored row (+1,-1|<=0), but
    # the polytope's actual vertices ((1,1),(1,0),(0,0), excluding (0,1))
    # show the true region is p1>=p2, matching only after this negation.
    coeffs <- -row[-(1:2)]
    nz     <- which(coeffs != 0)
    if (length(nz) == 2 && rhs == 0 && sum(coeffs[nz]) == 0) {
      # Row means sum(coeffs*p) <= rhs (or = rhs). For two nonzero coeffs
      # summing to 0 with rhs=0, e.g. -p1+p2<=0, that's p2<=p1, i.e. the
      # NEGATIVE-coefficient variable is the larger one (moving it to the
      # other side flips its sign) — not the positive one.
      a <- nz[which(coeffs[nz] < 0)]; b <- nz[which(coeffs[nz] > 0)]
      if (length(a) == 1 && length(b) == 1) {
        rel <- if (is_eq) "equals" else "is greater than"
        return(paste0(mathify(tex_name(p_names_lt[a])), " ", rel, " ", mathify(tex_name(p_names_lt[b]))))
      }
    }
    if (length(nz) == 1) {
      # co*p_j <= rhs (or = rhs). Dividing by a negative co flips an
      # inequality's direction; for an equality the sign doesn't matter.
      co <- coeffs[nz]
      bound <- rhs / co
      rel <- if (is_eq) "is exactly" else if (co > 0) "is at most" else "is at least"
      return(paste0(mathify(tex_name(p_names_lt[nz])), " ", rel, " ", frac_str(bound)))
    }
    # Flip everything (coeffs and rhs) when negative terms dominate, so the
    # combination reads with mostly/all positive coefficients — e.g.
    # "-1/3p1-1/3p2-1/3p3 <= -4/5" is much harder to parse at a glance than
    # its equivalent "1/3p1+1/3p2+1/3p3 >= 4/5".
    flipped <- sum(coeffs[nz] < 0) > sum(coeffs[nz] > 0)
    if (flipped) coeffs <- -coeffs
    terms <- paste(vapply(nz, function(j) {
      co <- coeffs[j]
      mathify(paste0(if (co > 0) "+" else "-", if (abs(co) == 1) "" else frac_str(abs(co)), tex_name(p_names_lt[j])))
    }, character(1)), collapse = " ")
    display_rhs <- if (flipped) -rhs else rhs
    # "Weighted" only means something when a coefficient isn't 1 — call it
    # a plain sum otherwise, e.g. "p1+p2" isn't weighting anything.
    all_unit <- all(abs(coeffs[nz]) == 1)
    label <- if (all_unit) "the sum" else "a weighted combination"
    rel <- if (is_eq) "exactly " else if (flipped) "at least " else "at most "
    paste0(label, " (", trimws(terms), ") must be ", rel, frac_str(display_rhs))
  }

  #### Model Properties Table ####

  # LP-based nesting: is every point of H_a also in H_b?
  nesting_lp <- function(H_a, H_b) {
    H_a_ch <- H_a   # already a rational character matrix from rcdd
    attr(H_a_ch, "representation") <- "H"
    H_b_n  <- q2d(H_b)
    dvec0   <- rep("0", ncol(H_a_ch) - 2L)
    feas_a <- tryCatch(lpcdd(H_a_ch, dvec0)$solution.type, error = function(e) "Error")
    if (feas_a == "Inconsistent" || feas_a == "Error") return(NA)

    for (j in seq_len(nrow(H_b_n))) {
      a_j   <- as.numeric(H_b_n[j, 3:ncol(H_b_n)])
      dvec  <- as.vector(d2q(matrix(a_j, nrow = 1)))
      res   <- tryCatch(lpcdd(H_a_ch, dvec), error = function(e) NULL)
      if (is.null(res) || res$solution.type %in% c("Inconsistent", "Error")) return(NA)
      if (res$solution.type != "Optimal") next
      min_val <- H_b_n[j, 2] + q2d(res$optimal.value)
      if (H_b_n[j, 1] == 0 && min_val < -1e-8) return(FALSE)
      if (H_b_n[j, 1] == 1) {
        if (min_val < -1e-8) return(FALSE)
        res2 <- tryCatch(lpcdd(H_a_ch, as.vector(d2q(matrix(-a_j, nrow = 1)))), error = function(e) NULL)
        if (!is.null(res2) && res2$solution.type == "Optimal" &&
            H_b_n[j, 2] - q2d(res2$optimal.value) > 1e-8) return(FALSE)
      }
    }
    TRUE
  }

  # Shared helper: build comparison table HTML for a vector of model names.
  # Must be called from within a reactive context (e.g. renderUI).
  build_comp_html <- function(model_names) {
    fmt_vol <- function(v) {
      if (is.null(v) || is.na(v)) return("?")
      if (v < 0) return("0")
      if (v > 1) return(sprintf("<span style='color:#b05000;'>%.4f ⚠</span>", v))
      if (v == 0 || sprintf("%.4f", v) == "0.0000" || v < 5e-5) {
        if (v == 0) return("&lt; 10<sup>-5</sup>")
        exp  <- floor(log10(v))
        mant <- v / 10^exp
        return(sprintf("%.2f &times; 10<sup>%d</sup>", mant, exp))
      }
      sprintf("%.4f", v)
    }

    # Max Bayes factor = 1/volume against the unconstrained model — same
    # definition and formatting as the "Max BF" column on the Parsimony tab.
    # exact=TRUE means the 0 volume is PROVEN (e.g. an equality-constrained
    # subspace, checked directly from the H-representation) — infinity is
    # mathematically justified there. exact=FALSE means the 0 came from a
    # Monte Carlo volume ESTIMATE (h_pars_reactive) that simply never
    # landed a sample inside — that does not prove the true volume is
    # zero, only that it's too small to detect, so claiming "&infin;"
    # there would overclaim certainty the estimate doesn't have.
    fmt_bf <- function(v, exact = TRUE) {
      if (is.null(v) || is.na(v)) return("?")
      if (v <= 0) return(if (exact) "&infin;" else "very large<sup>*</sup>")
      sprintf("%.2f", 1 / v)
    }
    bf_span <- function(v, exact = TRUE) paste0("<small style='color:#666;font-size:11px;'> &middot; max BF: ", fmt_bf(v, exact), "</small>")

    H_list <- lapply(model_names, function(nm) isolate(h_reactive[[nm]]))
    if (any(vapply(H_list, is.null, logical(1))))
      return("<p style='color:#888;'>Click Formalize first to compute H-representations.</p>")

    n_m     <- length(model_names)
    cells   <- matrix("", nrow = n_m, ncol = n_m)
    cls     <- matrix("ct-default", nrow = n_m, ncol = n_m)
    tips    <- matrix("", nrow = n_m, ncol = n_m)
    rel_log <- list()

    # Detect non-full-dimensional models: any equality row (type "1") in H-rep
    # means the model lives on a subspace → volume is exactly 0 in ambient space.
    is_zero_vol <- vapply(H_list, function(H) any(H[, 1] == "1"), logical(1))

    any_missing_pars <- FALSE
    for (i in seq_len(n_m)) {
      if (is_zero_vol[i]) {
        cells[i, i] <- paste0("<small style='color:#666;font-size:11px;'>vol: </small>0", bf_span(0))
        tips[i, i]  <- "Not full-dimensional: volume is exactly 0 in the ambient parameter space (model lives on a subspace defined by equality constraints)"
        cls[i, i] <- "ct-diag"
        next
      }
      pars <- h_pars_reactive[[model_names[i]]]
      if (!is.null(pars) && !is.na(pars)) {
        cells[i, i] <- paste0("<small style='color:#666;font-size:11px;'>vol: </small>", fmt_vol(pars), bf_span(pars))
        tips[i, i]  <- if (pars > 1e-10)
                          sprintf("parsimony ≈ %.2e (~1 in %.0f configurations)", pars, round(1/pars))
                        else "parsimony is extremely small (near-zero volume polytope)"
      } else {
        cells[i, i] <- "?"
        tips[i, i]  <- "Run the Parsimony tab first to compute this value"
        any_missing_pars <- TRUE
      }
      cls[i, i] <- "ct-diag"
    }

    # Two models are compatible iff their H-rep column names (parameters)
    # match exactly — same ncol is necessary but not sufficient. Item
    # models can share both ncol AND column names while assigning
    # different original parameters to the same stimulus type (see
    # type_assignment attr, set where item-model H matrices are built) —
    # comparing/intersecting those would mix designs that don't
    # correspond to the same substantive quantities, so it's included in
    # the compatibility key too.
    comp_key <- function(h) {
      key <- paste(colnames(h)[-(1:2)], collapse = "\n")
      ta  <- attr(h, "type_assignment")
      if (!is.null(ta)) key <- paste0(key, "||", ta)
      key
    }

    for (i in seq_len(n_m)) {
      for (j in seq_len(n_m)) {
        if (i == j) next
        H_i <- H_list[[i]]; H_j <- H_list[[j]]

        if (ncol(H_i) != ncol(H_j) || comp_key(H_i) != comp_key(H_j)) {
          cells[i, j] <- "≠dim"
          tips[i, j]  <- if (ncol(H_i) != ncol(H_j))
              sprintf("Different parameter spaces (%d vs %d params)", ncol(H_i) - 2L, ncol(H_j) - 2L)
            else
              "Same parameter count, but different parameters/design (e.g. items assigned to stimulus types differently) — not directly comparable."
          cls[i, j] <- "ct-nodim"; next
        }

        H_comb <- rbind(H_i, H_j)
        attr(H_comb, "representation") <- "H"
        dvec0  <- rep("0", ncol(H_i) - 2L)
        feasible <- tryCatch(
          lpcdd(H_comb, dvec0)$solution.type != "Inconsistent",
          error = function(e) NA
        )
        if (isTRUE(!feasible)) {
          cells[i, j] <- "∅"; cls[i, j] <- "ct-disjoint"
          tips[i, j]  <- "Disjoint: no parameter values satisfy both models"; next
        }

        ij <- tryCatch(nesting_lp(H_i, H_j), error = function(e) NA)
        ji <- tryCatch(nesting_lp(H_j, H_i), error = function(e) NA)

        pi <- if (is_zero_vol[i]) 0 else h_pars_reactive[[model_names[i]]]
        pj <- if (is_zero_vol[j]) 0 else h_pars_reactive[[model_names[j]]]

        ratio_str <- function(num, den) {
          if (is.null(num) || is.null(den) || is.na(num) || is.na(den)) return("?×")
          if (num <= 0 && den <= 0) return("1×")
          if (num <= 0) return("∞×")
          sprintf("%.1f×", den / num)
        }
        pct_str <- function(part, whole) {
          if (is.null(part) || is.null(whole) || is.na(part) || is.na(whole)) return("?%")
          if (whole <= 0) return("~100%")
          pct <- 100 * part / whole
          # An intersection is always a subset of each parent model, so its
          # share of that model's space can never truly exceed 100% — a
          # part/whole pair computed from two SEPARATE Monte Carlo volume
          # estimates (fitted with independent random samples) can still
          # come out above 100% from estimation noise alone. Clamp the
          # display rather than show an impossible percentage.
          if (pct > 100) return("~100%")
          if (pct == 0) return("< 10<sup>-5</sup>%")
          if (sprintf("%.0f", pct) == "0" || pct < 0.005) {
            exp  <- floor(log10(pct))
            mant <- pct / 10^exp
            return(sprintf("%.1f&times;10<sup>%d</sup>%%", mant, exp))
          }
          sprintf("%.0f%%", pct)
        }

        # When LP says both are nested in each other ("="), sanity-check with
        # volumes: if volumes are known and differ by >1%, the LP gave a false
        # positive (common in high-dimensional models due to q2d float conversion).
        # Override: the smaller-volume model is ⊂ the larger-volume model.
        if (isTRUE(ij) && isTRUE(ji)) {
          if (!is.null(pi) && !is.null(pj) && !is.na(pi) && !is.na(pj) && max(pi, pj) > 0) {
            rel_diff <- abs(pi - pj) / max(pi, pj)
            if (rel_diff > 0.01) {
              if (pi < pj) { ji <- FALSE } else { ij <- FALSE }
            }
          }
        }

        if (isTRUE(ij) && isTRUE(ji)) {
          vstr <- fmt_vol(pi)
          cells[i, j] <- paste0("=<br><small style='color:#666;font-size:11px;'>vol: ", vstr, "</small>", bf_span(pi))
          cls[i, j]   <- "ct-equiv"
          tips[i, j]  <- paste0("Equivalent: same prediction region. Parsimony = ", vstr,
            ". These models are empirically indistinguishable.")
          if (i < j) rel_log[[length(rel_log)+1]] <- list(
            type="eq", a=model_names[i], b=model_names[j], v=vstr, pi=pi, pj=pj)

        } else if (isTRUE(ij)) {
          vstr <- if (is_zero_vol[i]) "0" else fmt_vol(pi)
          rstr <- ratio_str(pi, pj)
          cells[i, j] <- paste0("⊂<br><small style='color:#666;font-size:11px;'>vol: ", vstr, "</small>", bf_span(pi))
          cls[i, j]   <- "ct-nest"
          tips[i, j]  <- paste0(model_names[i], " ⊂ ", model_names[j],
            ": parsimony of ", model_names[i], " = ", vstr,
            "; ", model_names[j], " is ", rstr, " less restrictive.")
          if (i < j) rel_log[[length(rel_log)+1]] <- list(
            type="nested", nested=model_names[i], broader=model_names[j],
            v=vstr, ratio=rstr, pi=pi, pj=pj)

        } else if (isTRUE(ji)) {
          vstr <- if (is_zero_vol[j]) "0" else fmt_vol(pj)
          rstr <- ratio_str(pj, pi)
          cells[i, j] <- paste0("⊃<br><small style='color:#666;font-size:11px;'>vol: ", vstr, "</small>", bf_span(pj))
          cls[i, j]   <- "ct-nest"
          tips[i, j]  <- paste0(model_names[j], " ⊂ ", model_names[i],
            ": parsimony of ", model_names[j], " = ", vstr,
            "; ", model_names[i], " is ", rstr, " less restrictive.")
          if (i < j) rel_log[[length(rel_log)+1]] <- list(
            type="nested", nested=model_names[j], broader=model_names[i],
            v=vstr, ratio=rstr, pi=pj, pj=pi)

        } else {
          # If either model is non-full-dimensional, the intersection also has
          # volume exactly 0 — no need to store or compute it.
          if (is_zero_vol[i] || is_zero_vol[j]) {
            cells[i, j] <- paste0("∩<br><small style='color:#666;font-size:11px;'>vol: 0</small>", bf_span(0))
            cls[i, j]   <- "ct-overlap"
            tips[i, j]  <- paste0("Overlap — but intersection volume is exactly 0 because ",
              if (is_zero_vol[i] && is_zero_vol[j]) "both models are" else
              if (is_zero_vol[i]) model_names[i] else model_names[j],
              " not full-dimensional (lives on a subspace).")
            if (i < j) rel_log[[length(rel_log)+1]] <- list(
              type="overlap", a=model_names[i], b=model_names[j],
              v="0", ci="0%", cj="0%", pint=0, pi=pi, pj=pj)
          } else {
            int_key <- paste0("[", paste(sort(c(model_names[i], model_names[j])), collapse = " ∩ "), "]")
            H_red <- tryCatch(redundant(H_comb)$output, error = function(e) H_comb)
            if (is.null(isolate(h_reactive[[int_key]]))) {
              h_reactive[[int_key]] <- H_red
              cur_names <- isolate(comp_int_reactive$names)
              if (!int_key %in% cur_names)
                comp_int_reactive$names <- c(cur_names, int_key)
            }
            # Exact dimension check — same convention as Table 3 in the
            # paper ("when neither model is nested in the other, [give]
            # the dimension of their overlap"): the minimized H-rep's
            # count of nonredundant EQUALITY rows is the intersection's
            # codimension, computed directly from the H-representation,
            # not estimated. When the overlap is lower-dimensional than
            # the ambient space, its volume there is exactly 0 (not a
            # sampling artifact) and a "volume"/"max BF" number would be
            # meaningless — show the dimension instead, exactly like the
            # paper does, rather than a volume estimate that can't
            # distinguish "empty" from "just very thin."
            n_p_full  <- ncol(H_i) - 2L
            n_eq_int  <- sum(H_red[, 1] == "1")
            dim_int   <- n_p_full - n_eq_int

            if (n_eq_int > 0) {
              cells[i, j] <- paste0("∩<br><small style='color:#666;font-size:11px;'>", dim_int, "D overlap</small>")
              cls[i, j]   <- "ct-overlap"
              tips[i, j]  <- paste0("Overlap confirmed (feasibility check passed), but the overlap is only ",
                dim_int, "-dimensional in this ", n_p_full, "-dimensional space (", n_eq_int,
                " equality constraint", if (n_eq_int == 1) "" else "s",
                " emerge from combining the two models) — its volume in the full space is exactly 0, ",
                "not merely small. Same convention as Table 3 in the paper: dimension is reported ",
                "instead of a volume/BF number for overlaps that aren't full-dimensional.")
              if (i < j) rel_log[[length(rel_log)+1]] <- list(
                type="overlap_lowdim", a=model_names[i], b=model_names[j],
                dim=dim_int, dim_full=n_p_full, pi=pi, pj=pj)
            } else {
              pint <- h_pars_reactive[[int_key]]
              vstr <- fmt_vol(pint)
              ci   <- pct_str(pint, pi)
              cj   <- pct_str(pint, pj)
              # This branch is only reached once BOTH the LP feasibility
              # check above AND the exact dimension check just above have
              # confirmed the overlap is genuinely full-dimensional. A
              # pint of exactly 0 here can therefore only be a sampling
              # detection-limit artifact, not a real empty/degenerate
              # overlap (that case is now fully handled above) — hence
              # exact = FALSE.
              cells[i, j] <- paste0("∩<br><small style='color:#666;font-size:11px;'>vol: ", vstr, "</small>", bf_span(pint, exact = FALSE))
              cls[i, j]   <- "ct-overlap"
              tips[i, j]  <- if (vstr == "?")
                paste0("Overlap — run Parsimony tab to compute intersection volume (", int_key, ")")
              else if (!is.null(pint) && !is.na(pint) && pint <= 0)
                paste0("Overlap confirmed (feasibility check passed, full-dimensional) — but the estimated ",
                  "volume rounds to 0, most likely because the overlap region is too small for the ",
                  "sampling algorithm to detect, not because it's truly empty or lower-dimensional.")
              else
                paste0("∩ parsimony = ", vstr, "; covers ", ci, " of ", model_names[i],
                  "'s space and ", cj, " of ", model_names[j], "'s space.")
              if (i < j) rel_log[[length(rel_log)+1]] <- list(
                type="overlap", a=model_names[i], b=model_names[j],
                v=vstr, ci=ci, cj=cj, pint=pint, pi=pi, pj=pj)
            }
          }
        }

        if (i < j && isTRUE(!feasible)) {
          rel_log[[length(rel_log)+1]] <- list(type="disjoint", a=model_names[i], b=model_names[j])
        }
      }
    }

    pars_avail   <- !any_missing_pars
    nested_rels  <- Filter(function(r) r$type == "nested",  rel_log)
    overlap_rels <- Filter(function(r) r$type == "overlap", rel_log)
    lowdim_rels  <- Filter(function(r) r$type == "overlap_lowdim", rel_log)
    eq_rels      <- Filter(function(r) r$type == "eq",      rel_log)
    disj_rels    <- Filter(function(r) r$type == "disjoint",rel_log)
    all_bullets  <- character(0)

    for (r in nested_rels) {
      txt <- paste0("<b>", r$nested, " ⊂ ", r$broader, "</b>: ",
        r$nested, " is a special case of ", r$broader,
        " — every prediction of ", r$nested, " is also a prediction of ", r$broader, ", but not vice versa.")
      if (pars_avail && !is.null(r$pi) && !is.null(r$pj) && !is.na(r$pi) && !is.na(r$pj) && r$pj > 0) {
        ratio_num <- r$pj / r$pi
        strength  <- if (ratio_num >= 10) "far more" else if (ratio_num >= 3) "considerably more" else "somewhat more"
        txt <- paste0(txt, " ", r$broader, " is ", strength, " permissive (", r$ratio,
          " less restrictive), so ", r$nested, " is the stronger, more falsifiable model.")
      }
      all_bullets <- c(all_bullets, txt)
    }
    for (r in overlap_rels) {
      txt <- paste0("<b>", r$a, " ∩ ", r$b, "</b>: the models partially overlap.")
      if (pars_avail && r$v != "?") {
        ci_num <- if (!is.null(r$pi) && !is.na(r$pi) && r$pi > 0 && !is.null(r$pint) && !is.na(r$pint))
                    100 * r$pint / r$pi else NA
        cj_num <- if (!is.null(r$pj) && !is.na(r$pj) && r$pj > 0 && !is.null(r$pint) && !is.na(r$pint))
                    100 * r$pint / r$pj else NA
        disc <- if (!is.na(ci_num) && !is.na(cj_num)) {
          avg <- (ci_num + cj_num) / 2
          if (avg < 5)  " They are almost entirely discriminable — the shared region is negligibly small."
          else if (avg < 40) " Meaningful room for empirical discrimination remains."
          else " They overlap substantially and may be hard to discriminate empirically."
        } else ""
        txt <- paste0(txt, " Intersection parsimony: ", r$v,
          " (", r$ci, " of ", r$a, "'s space; ", r$cj, " of ", r$b, "'s space).", disc)
      }
      all_bullets <- c(all_bullets, txt)
    }
    for (r in lowdim_rels) {
      all_bullets <- c(all_bullets, paste0(
        "<b>", r$a, " ∩ ", r$b, "</b>: they overlap, but only in a ", r$dim,
        "-dimensional slice of the ", r$dim_full, "-dimensional space — combining the two models forces ",
        r$dim_full - r$dim, " equalit", if (r$dim_full - r$dim == 1) "y" else "ies",
        " among the parameters. Their shared region has zero volume in the full space (not merely small)."))
    }
    for (r in eq_rels) {
      all_bullets <- c(all_bullets, paste0(
        "<b>", r$a, " = ", r$b, "</b>: empirically identical — ",
        "no observation can distinguish them despite potentially different constraint formulations."))
    }
    for (r in disj_rels) {
      all_bullets <- c(all_bullets, paste0(
        "<b>", r$a, " ∅ ", r$b, "</b>: mutually exclusive — ",
        "any observation consistent with one is inconsistent with the other. Maximally discriminable."))
    }

    narrative_html <- if (length(all_bullets) > 0) {
      items <- paste0("<li style='margin:4px 0;'>", all_bullets, "</li>", collapse = "")
      paste0(
        "<details style='margin-top:16px;'>",
        "<summary style='cursor:pointer;font-size:13px;color:var(--fairy-primary);font-weight:600;user-select:none;'>",
        "&#9432; What this means <span style='font-size:11px;font-weight:normal;color:var(--fairy-text-muted);'>(click to expand)</span></summary>",
        "<div class='ct-infobox' style='margin-top:10px;padding:12px 16px;",
        "border-left:3px solid var(--fairy-primary);border-radius:4px;font-size:13px;line-height:1.6;'>",
        "<ul style='margin:0;padding-left:18px;'>", items, "</ul>",
        if (!pars_avail) "<p style='margin:8px 0 0 0;color:#b05000;font-size:12px;'>⚠ Run the Parsimony tab to see quantitative details.</p>" else "",
        "</div></details>"
      )
    } else ""

    hdr <- paste0(
      "<th class='ct-header' style='padding:8px 14px;font-weight:600;",
      "border:1px solid var(--ct-border);font-size:13px;'>",
      c("", model_names), "</th>", collapse = "")
    rows <- paste0(vapply(seq_len(n_m), function(i) {
      cols <- paste0(vapply(seq_len(n_m), function(j) {
        paste0("<td title=\"", gsub('"', '&quot;', tips[i,j]), "\" class='", cls[i,j],
               "' style='text-align:center;padding:9px 18px;",
               "border:1px solid var(--ct-border);font-size:16px;",
               "cursor:default;'>", cells[i,j], "</td>")
      }, character(1)), collapse = "")
      paste0("<tr><th class='ct-header' style='padding:8px 14px;font-weight:600;",
             "border:1px solid var(--ct-border);text-align:left;font-size:13px;'>",
             model_names[i], "</th>", cols, "</tr>")
    }, character(1)), collapse = "")

    paste0(
      "<div style='overflow-x:auto;'>",
      "<table style='border-collapse:collapse;font-family:inherit;'>",
      "<thead><tr>", hdr, "</tr></thead><tbody>", rows, "</tbody></table></div>",
      "<p style='margin-top:10px;font-size:12px;color:var(--fairy-text-muted);line-height:1.8;'>",
      "<b>Read a cell as row [symbol] column</b> — ⊂ means the row model is a special case of (nested in) the column model; ",
      "⊃ means the reverse, the row model is the broader one and the column model is nested in it. &nbsp;&nbsp;",
      "<b>∩</b> overlap &nbsp;·&nbsp; <b>=</b> equivalent &nbsp;·&nbsp; <b>∅</b> disjoint &nbsp;·&nbsp; ",
      "\"vol\" is two different quantities depending on the cell: ",
      "on the <b>diag</b>onal it's that model's own parsimony &nbsp;·&nbsp; ",
      "everywhere else (⊂/⊃/∩/=) it's the two models' overlap parsimony ",
      "(for a nested ⊂/⊃ pair, that equals the narrower model's own parsimony, since it's fully contained in the broader one) &nbsp;·&nbsp; ",
      "<b>≠dim</b> different parameter spaces, not comparable &nbsp;&nbsp;",
      "<span style='color:#bbb;font-size:11px;'>? = run Parsimony tab first</span>",
      "</p>",
      narrative_html
    )
  }

  # Helper: given a derived model name (intersection/mixture), classify it by
  # checking whether any component name from the multi-item list appears as
  # a substring.  Returns "multi-item" or "base".
  derived_model_group <- function(nm) {
    itm <- isolate(items_reactive$names)
    itm <- itm[!is.na(itm) & nchar(itm) > 0]
    if (length(itm) > 0 && any(vapply(itm, function(r) grepl(r, nm, fixed = TRUE), logical(1))))
      return("multi-item")
    "base"
  }

  # Combine every model the user has defined — base, replication, and
  # multi-item, plus their intersections/mixtures — into the single list
  # that feeds ONE comparison table. This mirrors Table 3 in the paper,
  # which spans all ten of its models at once: pairs that share a
  # parameter space get a real relationship (⊂/⊃/∩/XD/=/∅), pairs that
  # don't (e.g. a 3-parameter base model vs. an 18-parameter replication)
  # show ≠dim in that cell, per build_comp_html's own compatibility check
  # — there was previously no reason to physically split these into three
  # separate table widgets, since build_comp_html already handles
  # incompatible pairs cell-by-cell rather than needing pre-grouping.
  # Deliberately NOT isolated: called both from reactive contexts (the
  # comparison table and the stale-warning check below, which both need
  # to re-run when the model list changes) and from inside observeEvent
  # handlers (which Shiny already isolates the body of automatically, so
  # no extra dependencies leak in there). Wrap in isolate() explicitly at
  # any call site that specifically wants a frozen snapshot instead.
  all_comparison_model_names <- function() {
    all_int <- intersections_reactive$names
    all_int <- all_int[!is.na(all_int) & nchar(all_int) > 0]
    all_mix <- mixtures_reactive$names
    all_mix <- all_mix[!is.na(all_mix) & nchar(all_mix) > 0]
    nms <- c(
      names_models_reactive$value,
      items_reactive$names,
      all_int, all_mix
    )
    unique(nms[!is.na(nms) & nchar(nms) > 0])
  }

  output$comparison_section_ui <- renderUI({
    model_names <- all_comparison_model_names()
    if (length(model_names) < 2) return(NULL)

    about <- HTML("
    <details style='margin-bottom:14px;'>
      <summary style='cursor:pointer;font-size:13px;color:var(--fairy-primary);font-weight:600;user-select:none;'>
        &#9432; About this table <span style='font-size:11px;font-weight:normal;color:var(--fairy-text-muted);'>(click to expand)</span>
      </summary>
      <div class='ct-infobox' style='margin-top:10px;padding:12px 16px;border-left:3px solid var(--fairy-primary);border-radius:4px;font-size:13px;line-height:1.7;'>
        <p style='margin:0 0 8px 0;'><b>What the table shows:</b> Every model you've defined — base models, replication models, multi-item models, and their intersections/mixtures — compared pairwise. Each cell describes the formal relationship between two models, determined purely from their mathematical definitions, before any data is collected.</p>
        <ul style='margin:0 0 8px 0;padding-left:18px;'>
          <li><b>⊂</b> — The row model is a special case of the column model (row is narrower). <b>⊃</b> — The row model is the broader one (column is the special case). Every prediction of the narrower model is also a prediction of the broader one, but not vice versa.</li>
          <li><b>∩</b> — The models partially overlap. Some parameter values satisfy both; others satisfy only one. When the overlap is lower-dimensional than the full parameter space (combining the two models forces an equality), the cell shows that dimension (e.g. <b>4D overlap</b>) instead of a volume — its volume in the full space is exactly 0, not merely small.</li>
          <li><b>∅</b> — The models are mutually exclusive. No observation can be consistent with both at once — maximally discriminable.</li>
          <li><b>=</b> — The models are empirically identical. No observation can ever distinguish them, despite potentially different constraint formulations.</li>
          <li><b>≠dim</b> — The two models live in different parameter spaces (different number of parameters, or the same number but a different design — e.g. items assigned to stimulus types differently, or a different number of replication labs) and aren't directly comparable.</li>
          <li><b>diag</b> — The diagonal shows each model's own parsimony (volume).</li>
        </ul>
        <p style='margin:0 0 8px 0;'><b>vol</b> is the volume of the model's polytope — a measure of <b>parsimony</b>. A smaller volume means the model makes more precise predictions and is easier to falsify. A volume of 1 means the model places no constraints at all (the unconstrained model).</p>
        <p style='margin:0 0 4px 0;'><b>Why are intersections and mixtures shown here?</b> Any model you defined — including intersections and mixtures — makes specific, formal predictions, just like a base model does. The table lets you evaluate those predictions: How parsimonious is the region where two theories simultaneously hold? Is that region a special case of a third model, or disjoint from it?</p>
        <p style='margin:0;color:#888;font-size:12px;'>? means parsimony not yet computed — click Compute in the sidebar.</p>
      </div>
    </details>
    ")

    tagList(
      about,
      tags$button(id = "comparison-table-fullscreen-btn", type = "button",
        title = "View this table fullscreen",
        style = "margin-bottom:8px;background:#f1f5f9;border:1px solid #cbd5e1;border-radius:6px;padding:4px 10px;font-size:0.8rem;cursor:pointer;color:#334155;",
        "⛶ Fullscreen"),
      div(id = "comparison_table_wrap", uiOutput("comparison_table_ui"))
    )
  })

  output$comparison_table_ui <- renderUI({
    model_names <- all_comparison_model_names()
    if (length(model_names) < 2)
      return(p("Define at least two models and click Formalize to compare.", style = "color:#888;"))
    HTML(build_comp_html(model_names))
  })

  #### Normalise a linear in/equality ####
  # Rewrites ANY linear constraint into the canonical form
  #   c1*p1 + c2*p2 + ... <op> constant
  # so users no longer have to keep constants on one side or avoid
  # parentheses. Works by evaluating each side as a linear function:
  # the constant is f(0) and the coefficient of p_i is f(e_i) - f(0).
  # Anything that cannot be evaluated is returned untouched, so unusual
  # input still falls through to the original parser.

  num2str_lin <- function(x) {
    if (!is.finite(x)) return(format(x, scientific = FALSE, trim = TRUE))
    if (x == round(x)) return(format(x, scientific = FALSE, trim = TRUE))
    fr <- attr(fractions(x), "fracs")
    # fractions()'s continued-fraction approximation has a limited
    # denominator search and silently rounds values it can't represent
    # well (e.g. 0.000001 -> "0") instead of erroring — always verify the
    # approximation actually reproduces x before trusting it, or a small
    # typed constant gets corrupted to 0.
    if (!is.null(fr) && length(fr) == 1 && !is.na(fr)) {
      fr_val <- tryCatch(eval(parse(text = as.character(fr))), error = function(e) NA_real_)
      # A RELATIVE check, not all.equal()'s absolute tolerance — an
      # absolute 1e-9 tolerance is meaningless once x itself is near 1e-9
      # (comparing 0 vs 1e-9 "passes" an absolute-1e-9 check even though
      # it's 100% wrong), so this must scale with the magnitude of x.
      rel_ok <- if (!is.na(fr_val)) {
        if (x == 0) fr_val == 0 else abs(fr_val - x) / abs(x) < 1e-8
      } else FALSE
      if (rel_ok) return(as.character(fr))
    }
    format(x, scientific = FALSE, trim = TRUE, digits = 15)
  }

  normalize_constraint <- function(cstr, n_p) {
    if (is.na(cstr) || cstr == "") return(cstr)

    # the app treats "<" as "<=", so >=/<= collapse onto >/<
    cstr <- str_replace_all(cstr, ">=|=>", ">")
    cstr <- str_replace_all(cstr, "<=|=<", "<")

    # only single-relation constraints (chains are split earlier)
    if (str_count(cstr, "<|>|=") != 1) return(cstr)

    op <- str_extract(cstr, "<|>|=")
    sides <- str_split_fixed(cstr, "<|>|=", 2)
    lhs <- sides[1]
    rhs <- sides[2]
    if (lhs == "" || rhs == "") return(cstr)

    ev <- function(s, vals) {
      env <- as.list(vals)
      names(env) <- paste0("p", seq_along(vals))
      tryCatch(as.numeric(eval(parse(text = s), envir = env)),
        error = function(e) NA_real_, warning = function(w) NA_real_
      )
    }

    # a value is usable only if it is a single finite number; 1/(p1+p2) at
    # p = 0 yields Inf, which must NOT be treated as a valid evaluation
    ok_num <- function(x) length(x) == 1 && !is.na(x) && is.finite(x)

    # probe points: 0 and two generic interior points. Using non-zero probes
    # keeps expressions such as 1/(p1+p2) from being evaluated only at a pole.
    zero <- rep(0, n_p)
    base <- rep(0.25, n_p)

    c_l <- ev(lhs, zero)
    c_r <- ev(rhs, zero)
    if (!ok_num(c_l) || !ok_num(c_r)) return(cstr)

    coef <- numeric(n_p)
    for (i in seq_len(n_p)) {
      e_i <- zero
      e_i[i] <- 1
      a <- ev(lhs, e_i)
      b <- ev(rhs, e_i)
      if (!ok_num(a) || !ok_num(b)) return(cstr)
      coef[i] <- (a - c_l) - (b - c_r)
    }

    # verify linearity: the difference must scale exactly with the step size,
    # both at a doubled step and around a shifted base point
    for (i in seq_len(n_p)) {
      e_i <- zero
      e_i[i] <- 2
      a <- ev(lhs, e_i)
      b <- ev(rhs, e_i)
      if (!ok_num(a) || !ok_num(b)) return(cstr)
      d2 <- ((a - c_l) - (b - c_r)) - 2 * coef[i]
      if (!ok_num(d2) || abs(d2) > 1e-9) return(cstr)

      f_l <- ev(lhs, base)
      f_r <- ev(rhs, base)
      s_i <- base
      s_i[i] <- s_i[i] + 1
      g_l <- ev(lhs, s_i)
      g_r <- ev(rhs, s_i)
      if (!ok_num(f_l) || !ok_num(f_r) || !ok_num(g_l) || !ok_num(g_r)) return(cstr)
      d3 <- ((g_l - f_l) - (g_r - f_r)) - coef[i]
      if (!ok_num(d3) || abs(d3) > 1e-9) return(cstr)
    }

    if (all(coef == 0)) return(cstr)

    terms <- paste0(
      ifelse(coef >= 0, "+", "-"),
      vapply(abs(coef), num2str_lin, character(1)),
      "*p", seq_len(n_p)
    )

    paste0(paste(terms, collapse = ""), op, num2str_lin(c_r - c_l))
  }

  # TRUE when a single constraint contains a probability but is NOT linear in
  # the p's (e.g. "1/(p1+p3) > p2"). Used to name the offending constraint in
  # the error message instead of showing a generic example.
  is_nonlinear_constraint <- function(cstr, n_p) {
    if (is.na(cstr) || cstr == "") return(FALSE)
    if (!str_detect(cstr, "p[0-9]")) return(FALSE)
    if (str_count(cstr, "<|>|=") != 1) return(FALSE)
    identical(normalize_constraint(cstr, n_p), cstr)
  }

  # Splits a raw model specification the same way extract_info does, so the
  # error message can point at the exact constraint that failed.
  split_spec <- function(spec) {
    s <- str_replace_all(spec, "\n", ";")
    chars <- unlist(str_split(s, ""))
    depth <- 0
    for (i in seq_along(chars)) {
      if (chars[i] == "{") depth <- depth + 1
      if (chars[i] == "}") depth <- depth - 1
      if (chars[i] == "," && depth <= 0) chars[i] <- ";"
    }
    parts <- unlist(str_split(paste(chars, collapse = ""), ";"))
    parts <- trimws(parts)
    parts[parts != ""]
  }

  # How many H-representation rows ONE typed clause turns into, mirroring
  # (not calling — this is purely predictive, for the display below; the
  # real computation still goes through extract_info() untouched) the two
  # ways extract_info() expands a single clause into several rows:
  #   - a chained comparison ("p1>p2>p3") into one row per adjacent pair
  #     (here: "p1>p2", "p2>p3")
  #   - a "{p1,p2}>{p3,p4}" batch shortcut into one row per pair across
  #     the two sets (here: 2*2 = 4 rows)
  # ...and both together ("{p1,p2}>{p3}>{p4}"): chained first, each
  # resulting pair batch-expanded on its own, exactly like extract_info()
  # itself does (chain-split happens before batch-expansion there too).
  clause_expected_row_count <- function(clause) {
    op_pos <- gregexpr("[=><]", clause)[[1]]
    if (op_pos[1] == -1) return(1L)
    args <- strsplit(clause, "[=><]")[[1]]
    if (length(args) < 2) return(1L)
    ops <- regmatches(clause, gregexpr("[=><]", clause))[[1]]
    side_factor <- function(side) {
      side <- trimws(side)
      if (grepl("^\\{", side)) {
        length(unlist(strsplit(gsub("[{} ]", "", side), ",")))
      } else 1L
    }
    total <- 0L
    for (k in seq_len(length(args) - 1)) {
      total <- total + side_factor(args[k]) * side_factor(args[k + 1])
    }
    total
  }

  # Same expansion as clause_expected_row_count, but returns the actual
  # atomic sub-clauses instead of just a count — e.g. "p1=p4=0" becomes
  # c("p1=p4", "p4=0"), "{p1,p2}=p3" becomes c("p1=p3", "p2=p3"). Used to
  # enumerate a model's "=" occurrences in the SAME order/count
  # extract_info() actually produces H-representation rows in, rather than
  # naive one-per-semicolon-clause — a chain like "p1=p4=0" is ONE typed
  # clause but TWO real equality rows, and every equality after it in the
  # spec shifts by however many extra rows the chain added. Both the
  # equality-tolerance popup (equality_tolerances_reactive, keyed by this
  # per-row occurrence index) and the "=" -> "≈" display substitution
  # (equality_flags_str, one flag per literal "=" character the client
  # substitutes) need this same per-row enumeration to stay aligned with
  # what the Go pipeline's all_operators actually iterates over — see the
  # bug this fixed: a model with a chained "=" clause silently applied an
  # approximate-equality tolerance to the WRONG (or no) later equality,
  # because the popup was counting raw semicolon-clauses instead of rows.
  expand_clause_to_rows <- function(clause) {
    op_pos <- gregexpr("[=><]", clause)[[1]]
    if (op_pos[1] == -1) return(clause)
    args <- strsplit(clause, "[=><]")[[1]]
    if (length(args) < 2) return(clause)
    ops <- regmatches(clause, gregexpr("[=><]", clause))[[1]]
    side_items <- function(side) {
      side <- trimws(side)
      if (grepl("^\\{", side)) unlist(strsplit(gsub("[{} ]", "", side), ",")) else side
    }
    segs <- character()
    for (k in seq_len(length(args) - 1)) {
      b1 <- side_items(args[k])
      b2 <- side_items(args[k + 1])
      for (a in b1) for (b in b2) segs <- c(segs, paste0(a, ops[k], b))
    }
    segs
  }

  # Which of a model's typed clauses (in the exact order split_spec()
  # returns them for its full "Model Specification" text) turned out
  # logically redundant — built on the raw-H/redundant()-index capture
  # already done in the Go pipeline (see h_raw_rows_reactive, mixed_eq_ineq's
  # own comment). A clause that expanded into several H-rep rows (a chain
  # or a batch shortcut, see clause_expected_row_count) counts as
  # redundant only if EVERY row it produced did — a partially-redundant
  # chain still asserts something new overall. Returns NULL — meaning
  # "don't highlight anything for this model" — whenever the mapping from
  # row index back to typed-clause index can't be trusted:
  #   - no H-representation captured yet for this model (h_raw_rows_reactive
  #     empty — Go hasn't run since this spec last changed)
  #   - equalities and inequalities were mixed (makeH() reorders rows then,
  #     see mixed_eq_ineq)
  #   - the PREDICTED row count (summed per clause) doesn't match the raw
  #     row count actually attributable to the user — most likely a
  #     trivial "p1<p1" clause silently dropped by extract_info's own
  #     cleanup step, which this predictive count doesn't know about
  # Deliberately silent/no-highlight rather than guessing in these cases —
  # a wrong red clause is worse than none.
  model_redundant_clause_flags <- function(model_name, spec_text) {
    info <- h_raw_rows_reactive[[model_name]]
    if (is.null(info) || isTRUE(info$mixed_eq_ineq)) return(NULL)
    n_user <- info$n_user_clauses
    if (is.null(n_user) || is.na(n_user) || n_user <= 0) return(NULL)
    clauses <- split_spec(spec_text %||% "")
    if (length(clauses) != n_user) return(NULL)
    n_p <- ncol(info$raw_H) - 2L
    user_row_count <- nrow(info$raw_H) - 2L * n_p
    row_counts <- vapply(clauses, clause_expected_row_count, integer(1))
    if (sum(row_counts) != user_row_count) return(NULL)
    is_redundant_row <- logical(user_row_count)
    is_redundant_row[info$redundant_rows[info$redundant_rows >= 1 & info$redundant_rows <= user_row_count]] <- TRUE
    # One flag per actual H-rep ROW, not collapsed to one per typed
    # clause — a shortcut clause ("{p1}>{p2,p3,p5}") expands to several
    # rows that don't all have to be redundant together: if only its
    # "p1>p2" row duplicates a later plain clause, that ROW should be
    # markable on its own. Collapsing to all-rows-in-this-clause-must-
    # agree (the old behavior) hid exactly that — when a shortcut's
    # rows and separately-typed duplicate rows split which one rcdd
    # actually flags as removable, NEITHER clause passed the all-or-
    # nothing test, so nothing lit up despite a genuine duplicate. The
    # client-side display (which independently expands the same
    # shortcuts for rendering — see expandBatchShortcut in the model
    # spec display script) now zips 1:1 against this per-row vector
    # instead of re-deriving a clause-to-row mapping of its own.
    is_redundant_row
  }

  #### Function to extract In/equalities from Input ####

  extract_info <- function(test = input_relations$rel1, loop_numb_models, func_mytable_input) {
    check_input <- paste(func_mytable_input[, 2], collapse = ";")

    max_p <- max(as.numeric(unlist(str_extract_all(check_input, "(?<=p)[0-9]*"))))

    counter$n <- ifelse(counter$n < max_p, max_p, counter$n)

    probs <- data.frame()


    for (loop in 1:counter$n) {
      name_p <- str_replace_all(eval(parse(
        text = paste("unlist(AllInputs()$textin_name_", loop, ")", sep = "")
      )), fixed(" "), "_")
      name_p <- ifelse(length(name_p) == 0, paste("p_{", loop, "}", sep = ""), name_p)


      probs <- rbind(
        probs,
        c(
          paste("p_", loop, sep = ""),
          name_p
        )
      )
    }

    ####

    probs <- data.frame(probs, rep(0, nrow(probs)), rep(1, nrow(probs)))
    colnames(probs) <- c("variable", "name", "p_min", "p_max")
    min_values <- rep(0, nrow(probs))
    max_values <- rep(1, nrow(probs))

    ####* split and remove ####

    limits <- paste(paste(paste("p", 1:counter$n, sep = ""), "< 1", collapse = ";"),
      paste(paste("p", 1:counter$n, sep = ""), "> 0", collapse = ";"),
      sep = ";"
    )


    test <- str_replace_all(test, "\n", ";")

    # Accept "," as a constraint separator as well, but NOT inside "{...}",
    # where commas belong to the "{p1,p2} < {p3}" batch shortcut.
    commas_outside_braces_to_semicolon <- function(s) {
      if (is.na(s) || !str_detect(s, ",")) {
        return(s)
      }
      chars <- unlist(str_split(s, ""))
      depth <- 0
      for (i in seq_along(chars)) {
        if (chars[i] == "{") depth <- depth + 1
        if (chars[i] == "}") depth <- depth - 1
        if (chars[i] == "," && depth <= 0) chars[i] <- ";"
      }
      paste(chars, collapse = "")
    }

    test <- vapply(test, commas_outside_braces_to_semicolon,
      FUN.VALUE = character(1), USE.NAMES = FALSE
    )

    # Count clauses the user actually typed, before the automatic 0-1
    # bounds ("limits") get appended below — this is what lets the
    # theory-building notes distinguish a genuinely redundant typed clause
    # from one of the routine, expected-to-be-redundant automatic bounds.
    n_user_clauses_reactive$value <- length(Filter(nzchar, trimws(unlist(str_split(test, ";")))))

    test <- paste0(test, ";", limits, collapse = "")

    test <- unlist(str_split(test, ";"))
    test <- str_replace_all(test, "\u2013", "-")
    test <- str_replace_all(test, "\u2014", "-")
    test <- str_replace_all(test, " ", "")

    test <- unlist(test)

    if (length(which(test %in% "")) > 0) {
      test <- test[-which(test %in% "")]
    }

    #### new ####

    numb_test <- length(test)


    for (loop_test in 1:numb_test) {
      pos_signs <- str_locate_all(test[loop_test], "[=><]")[[1]][, 1]

      signs <- numeric()

      for (loop_string in 1:length(pos_signs)) {
        signs <- c(signs, substr(test[loop_test], pos_signs[loop_string], pos_signs[loop_string]))
      }

      arguments <- strsplit(test[loop_test], "[=><]")[[1]]

      for (loop_length_arguments in 1:(length(arguments) - 1)) {
        test <- c(test, paste(arguments[loop_length_arguments],
          signs[loop_length_arguments],
          arguments[loop_length_arguments + 1],
          sep = ""
        ))
      }
    }

    test <- test[-(1:numb_test)]

    #### {p1,p2} > {p3,p4}  ####

    expand_scalar <- function(scalar) {
      # Extract the first and second batches of 'p' variables
      batches <- strsplit(gsub("[{} ]", "", scalar), "[><=]")[[1]]
      batch1 <- unlist(strsplit(batches[1], ","))
      batch2 <- unlist(strsplit(batches[2], ","))

      # Determine the separator used in the original scalar
      separator <- gsub("[p{}]", "", gsub("[^><=]", "", scalar))

      comparisons <- character()

      # Generate comparisons between the first and second batch of 'p' variables
      for (p1 in batch1) {
        for (p2 in batch2) {
          comparisons <- c(comparisons, paste0(p1, separator, p2))
        }
      }

      output <- paste(comparisons, collapse = ";")
      return(output)
    }

    test_new <- numeric()

    for (loop_sep in 1:length(test)) {
      if (grepl("{", test[loop_sep], fixed = TRUE) == T) {
        test_new <- c(test_new, expand_scalar(test[loop_sep]))
      } else {
        test_new <- c(test_new, test[loop_sep])
      }
    }

    test <- test_new
    test <- unlist(str_split(test, ";"))

    #### cut string ####

    test_new <- numeric()

    for (loop_check in 1:length(test)) {
      lr_side <- unlist(str_split(test[loop_check], ">|=|<"))

      if (lr_side[1] != lr_side[2]) {
        test_new <- c(test_new, test[loop_check])
      }
    }

    test <- test_new

    # A bare leading "." in a number (".000001") needs a "0" in front so the
    # downstream tokenizer parses it correctly. The clause-initial case
    # (".5<p1") was the only one handled here — a "." right after an
    # operator or sign, anywhere else in the clause (e.g. "p3<.000001",
    # "p1<p2>p3<.000001"), was missed, silently truncating that number to 0.
    test <- str_replace_all(test, "(?<![0-9])\\.", "0.")

    ####* normalise constraints ####
    # allows free-form linear input (parentheses, constants on either side)
    if (length(test) > 0) {
      test <- vapply(test, normalize_constraint,
        FUN.VALUE = character(1), n_p = counter$n, USE.NAMES = FALSE
      )
    }

    ####* extract structure ####

    loc_above <- str_locate_all(test, ">")
    loc_equal <- str_locate_all(test, "=")
    loc_below <- str_locate_all(test, "<")

    loc_add <- str_locate_all(test, fixed("+"))
    loc_subtract <- str_locate_all(test, "-")
    loc_divide <- str_locate_all(test, "/")
    loc_multiply <- str_locate_all(test, fixed("*"))

    loc_digits <- str_locate_all(test, "[:digit:]")
    loc_prob <- str_locate_all(test, fixed("p"))

    loc_paren_open <- str_locate_all(test, fixed("("))
    loc_paren_closed <- str_locate_all(test, fixed(")"))
    loc_point <- str_locate_all(test, fixed("."))

    #** loop here

    loop_all <- 1
    all_extracted <- list()

    for (loop_bit in 1:length(test)) {
      test_actual <- test[loop_bit]
      test_actual_single <- unlist(str_split(test_actual, ""))

      pos <- rbind(
        "above" = data.frame(loc_above[loop_bit]),
        "below" = data.frame(loc_below[loop_bit]),
        "equal" = data.frame(loc_equal[loop_bit]),
        "add" = data.frame(loc_add[loop_bit]),
        "subtract" = data.frame(loc_subtract[loop_bit]),
        "multiply" = data.frame(loc_multiply[loop_bit]),
        "divide" = data.frame(loc_divide[loop_bit]),
        "digit" = data.frame(loc_digits[loop_bit]),
        "p" = data.frame(loc_prob[loop_bit]),
        "point" = data.frame(loc_point[loop_bit]),
        "open" = data.frame(loc_paren_open[loop_bit]),
        "closed" = data.frame(loc_paren_closed[loop_bit])
      )
      pos <- pos[order(pos[, 1]), ]

      length_str <- str_length(test_actual)

      pos <- data.frame(t(pos))

      info_type <- colnames(pos)

      info_type <- str_replace_all(info_type, fixed("."), "")
      info_type <- str_replace_all(info_type, "[:digit:]", "")

      #####* extract sub inequalities and equalities

      ineq_equ_parts_loc <- which(info_type == "above" |
        info_type == "below" |
        info_type == "equal")

      ineq_equ_parts_loc <- c(1, ineq_equ_parts_loc, length(info_type))

      loc_subparts <- numeric()

      for (loop_loc in 1:(length(ineq_equ_parts_loc) - 2)) {
        loc_act <- ineq_equ_parts_loc[c(1, 3) + (loop_loc - 1)]

        if (loop_loc == 1) {
          loc_act[2] <- loc_act[2] - 1
        }

        if (loop_loc < (length(ineq_equ_parts_loc) - 2) &
          loop_loc != 1) {
          loc_act[1] <- loc_act[1] + 1
          loc_act[2] <- loc_act[2] - 1
        }

        if (loop_loc == (length(ineq_equ_parts_loc) - 2)) {
          loc_act[1] <- loc_act[1] + 1
          loc_act[2] <- loc_act[2]
        }

        loc_subparts <- c(loc_subparts, loc_act)
      }

      loc_subparts <- t(matrix(loc_subparts, nrow = 2))

      subparts <- list()

      if (nrow(loc_subparts) == 1) {
        subparts[[paste0("part", loop_loc)]] <-
          rbind(
            info_type[1:length(info_type)],
            test_actual_single[1:length(info_type)]
          )
      } else {
        for (loop_loc in 1:nrow(loc_subparts)) {
          subparts[[paste0("part", loop_loc)]] <-
            rbind(
              info_type[loc_subparts[loop_loc, 1]:loc_subparts[loop_loc, 2]],
              test_actual_single[loc_subparts[loop_loc, 1]:loc_subparts[loop_loc, 2]]
            )
        }
      }

      ###* loop over subparts

      for (loop_sub_part in 1:length(subparts)) {
        subparts_actual <- data.frame(subparts[loop_sub_part])


        pos_p <- which(subparts_actual[1, ] == "p")
        pos_before_p <- pos_p - 1

        if (pos_before_p[1] == 0) {
          subparts_actual <- cbind(cbind(c("add", "+"), c("digit", "1"), c("multiply", "*")), subparts_actual)
        }

        if (subparts_actual[1, 1] == "digit") {
          subparts_actual <- cbind(cbind(c("add", "+")), subparts_actual)
        }

        ####

        loc_after_relation <- which(
          subparts_actual[1, ] == "below" |
            subparts_actual[1, ] == "above" |
            subparts_actual[1, ] == "equal"
        ) + 1

        if (subparts_actual[1, loc_after_relation] == "digit" |
          subparts_actual[1, loc_after_relation] == "point") {
          subparts_actual <- cbind(
            subparts_actual[, 1:(loc_after_relation - 1)],
            c("add", "+"),
            subparts_actual[, loc_after_relation:ncol(subparts_actual)]
          )
        }


        add_multiply_function <- function() {
          pos_p <- which(subparts_actual[1, ] == "p")
          pos_before_p <- pos_p - 1

          add_multiply <- ifelse(
            subparts_actual[1, pos_before_p] == "add" |
              subparts_actual[1, pos_before_p] ==
                "below" |
              subparts_actual[1, pos_before_p] ==
                "above" |
              subparts_actual[1, pos_before_p] ==
                "equal",
            1,
            0
          )

          add_multiply <- ifelse(
            subparts_actual[1, pos_before_p] == "below" |
              subparts_actual[1, pos_before_p] ==
                "above" |
              subparts_actual[1, pos_before_p] ==
                "equal",
            2,
            add_multiply
          )

          add_multiply <- ifelse(subparts_actual[1, pos_before_p] == "subtract", -1,
            add_multiply
          )


          fix_factors <- pos_before_p[add_multiply != 0]
          add_multiply <- add_multiply[add_multiply != 0]

          return(list(fix_factors, add_multiply))
        }

        res_cadd_multiply_function <- add_multiply_function()

        fix_factors <- unlist(res_cadd_multiply_function[1])
        add_multiply <- unlist(res_cadd_multiply_function[2])

        if (length(add_multiply) > 0) {
          for (loop_add_multiply in 1:length(add_multiply)) {
            if (add_multiply[1] == -1) {
              subparts_actual <-
                cbind(
                  subparts_actual[, 1:fix_factors[1]],
                  cbind(c("digit", "1"), c("multiply", "*")),
                  subparts_actual[, (fix_factors[1] + 1):ncol(subparts_actual)]
                )
            }

            if (add_multiply[1] == 1) {
              subparts_actual <-
                cbind(
                  subparts_actual[, 1:fix_factors[1]],
                  cbind(c("digit", "1"), c("multiply", "*")),
                  subparts_actual[, (fix_factors[1] + 1):ncol(subparts_actual)]
                )
            }

            if (add_multiply[1] == 2) {
              subparts_actual <-
                cbind(
                  subparts_actual[, 1:fix_factors[1]],
                  cbind(
                    c("add", "+"),
                    c("digit", "1"),
                    c("multiply", "*")
                  ),
                  subparts_actual[, (fix_factors[1] + 1):ncol(subparts_actual)]
                )
            }

            res_cadd_multiply_function <- add_multiply_function()
            fix_factors <- unlist(res_cadd_multiply_function[1])
            add_multiply <- unlist(res_cadd_multiply_function[2])
          }
        }

        #####* extract p ####

        keep <- numeric()

        for (loop_extract in 1:ncol(subparts_actual)) {
          if (subparts_actual[1, loop_extract] == "p") {
            keep <- c(keep, 1)
          }

          if (loop_extract > 1 &
            subparts_actual[1, loop_extract] != "p") {
            if (subparts_actual[1, loop_extract] != "p" &
              subparts_actual[1, loop_extract] == "digit" &
              keep[loop_extract - 1] == 1) {
              keep <- c(keep, 1)
            } else {
              keep <- c(keep, 0)
            }
          }

          if (loop_extract == 1 &
            subparts_actual[1, loop_extract] != "p") {
            keep <- c(keep, 0)
          }
        }

        extract_p <- subparts_actual[2, keep == 1]
        extract_p <- paste0(extract_p, collapse = "")
        extract_p <- unlist(str_split(extract_p, "p"))
        extract_p <- extract_p[2:length(extract_p)]
        extract_p <- as.numeric(extract_p)

        ####* extract relation ####

        location_relation <- which(
          subparts_actual[1, ] == "below" |
            subparts_actual[1, ] == "above" |
            subparts_actual[1, ] == "equal"
        )

        relation_subpart <- subparts_actual[1, location_relation]
        relation_subpart <- unlist(relation_subpart)

        ####* extract factors ####

        left_side <- data.frame(subparts_actual[, 1:(location_relation -
          1)])
        right_side <- data.frame(subparts_actual[, (location_relation +
          1):ncol(subparts_actual)])


        location_add_subtract_left <- which(left_side[1, ] == "subtract" |
          left_side[1, ] == "add")

        location_p_left <- which(left_side[1, ] == "p")


        location_add_subtract_right <- which(right_side[1, ] == "subtract" |
          right_side[1, ] == "add")

        location_p_right <- which(right_side[1, ] == "p")


        ####** left side

        factor_left <- numeric()

        if (length(location_p_left) == 0) {
          factor_left <- paste0(left_side[2, ], collapse = "")
          extract_p <- c("numb", extract_p)
        } else {
          for (loop_loc in 1:length(location_add_subtract_left)) {
            factor_left <- c(
              factor_left,
              paste0(left_side[2, location_add_subtract_left[loop_loc]:(location_p_left[loop_loc] -
                2)], collapse = "")
            )
          }
        }

        ####** right side

        factor_right <- numeric()

        if (length(location_p_right) == 0) {
          factor_right <- paste0(right_side[2, ], collapse = "")

          extract_p <- c(extract_p, "numb")
        } else {
          for (loop_loc in 1:length(location_add_subtract_right)) {
            factor_right <- c(
              factor_right,
              paste0(right_side[2, location_add_subtract_right[loop_loc]:(location_p_right[loop_loc] -
                2)], collapse = "")
            )
          }
        }

        ####** combine extract

        extracted <- rbind(
          extract_p,
          c(
            rep("left", length(
              factor_left
            )),
            rep("right", length(
              factor_right
            ))
          ),
          c(factor_left, factor_right),
          relation_subpart
        )

        row.names(extracted) <- c("p", "left/right", "factor", "relation")
        colnames(extracted) <- paste("sub_", 1:ncol(extracted), sep = "")
        extracted <- data.frame(extracted)

        all_extracted[[paste0("line", loop_all)]] <- extracted

        loop_all <- loop_all + 1
      }
    }

    if (length(all_extracted) > 0) {
      numb_p <- numeric()

      for (loop_numb_p in 1:length(all_extracted)) {
        numb_p <- c(numb_p, (unlist(data.frame(
          all_extracted[loop_numb_p]
        )[1, ])))
      }

      numb_p <- numb_p[numb_p != "numb"]
      numb_p <- as.numeric(numb_p)
      numb_p <- max(numb_p)
    } else {
      all_extracted <- list()
      numb_p <- 0
    }

    ###

    numb_equl_ineq <- length(all_extracted)

    ineq_eq_left <- matrix(NA, ncol = numb_p, nrow = numb_equl_ineq)
    ineq_eq_right <- rep(NA, numb_equl_ineq)
    all_operators <- rep(NA, numb_equl_ineq)

    ###

    for (loop_relations in 1:length(all_extracted)) {
      actual_relations <- data.frame(all_extracted[loop_relations])

      extract_factors <- numeric()

      for (loop_factors in 1:ncol(actual_relations)) {
        extract_factors <- c(extract_factors, eval(parse(text = actual_relations[3, loop_factors])))
      }

      actual_relations[3, ] <- (extract_factors)

      actual_relations_left <- actual_relations[, actual_relations[2, ] == "left"]
      actual_relations_right <- actual_relations[, actual_relations[2, ] == "right"]
      actual_relations_operator <- actual_relations[4, 1]

      actual_relations_left <- data.frame(actual_relations_left)
      actual_relations_right <- data.frame(actual_relations_right)

      numb_left <- which(actual_relations_left[1, ] == "numb")
      numb_right <- which(actual_relations_right[1, ] == "numb")

      non_numb_left <- which(actual_relations_left[1, ] != "numb")
      non_numb_right <- which(actual_relations_right[1, ] != "numb")

      #### change signs

      if (actual_relations_operator == "equal" &
        length(non_numb_right) > 0 & length(non_numb_left) > 0) {
        actual_relations_right[3, non_numb_right] <- -1 * as.numeric(actual_relations_right[3, non_numb_right])
      }

      if (actual_relations_operator == "below" &
        length(non_numb_right) > 0) {
        actual_relations_right[3, non_numb_right] <- -1 * as.numeric(actual_relations_right[3, non_numb_right])
        actual_relations_left[3, numb_left] <- -1 * as.numeric(actual_relations_left[3, numb_left])
      }

      if (actual_relations_operator == "above" &
        length(non_numb_left) > 0) {
        actual_relations_left[3, non_numb_left] <- -1 * as.numeric(actual_relations_left[3, non_numb_left])
        actual_relations_right[3, numb_right] <- -1 * as.numeric(actual_relations_right[3, numb_right])
        actual_relations_operator <- "below"
        actual_relations_left[4, ] <- "below"
        actual_relations_right[4, ] <- "below"
      }

      ## do fractions

      actual_relations_left <- data.frame(actual_relations_left)
      actual_relations_right <- data.frame(actual_relations_right)

      actual_relations_left[3, ] <- (d2q(as.numeric(actual_relations_left[3, ])))
      actual_relations_right[3, ] <- (d2q(as.numeric(actual_relations_right[3, ])))


      ## translate into RCDD input

      total_rcdd <- data.frame(actual_relations_left, actual_relations_right)

      total_rcdd_leftside <- data.frame(total_rcdd[, total_rcdd[1, ] != "numb"])
      total_rcdd_rightside <- data.frame(total_rcdd[, total_rcdd[1, ] == "numb"])

      if (length(total_rcdd_rightside) == 0) {
        total_rcdd_rightside <- t(data.frame("numb", "right", "0", actual_relations_operator))
      }

      ###

      ineq_eq_left[loop_relations, as.numeric(unlist(total_rcdd_leftside[1, ]))] <- unlist(total_rcdd_leftside[3, ])
      ineq_eq_left[loop_relations, -as.numeric(unlist(total_rcdd_leftside[1, ]))] <- "0"
      ineq_eq_right[loop_relations] <- total_rcdd_rightside[3, ]
      all_operators[loop_relations] <- actual_relations_operator
    }

    #### approx equal


    add_ineq_eq_left <- numeric()
    add_ineq_eq_right <- numeric()
    add_all_operators <- numeric()

    ####*** add approx equalities when selected ####

    # Per-equality tolerances: tol_vec[k] is the tolerance for the k-th
    # "=" clause encountered left-to-right in this model's spec (set via
    # the popup that appears the moment a new one is typed — see
    # equality_tolerances_reactive's own comment). An occurrence with no
    # tolerance recorded for it (vector too short, or a value of 0) stays
    # an exact equality; only occurrences with a nonzero tolerance get
    # converted to the below/below pair below. This replaces the old
    # single per-model tune_knob that was applied uniformly to every "="
    # in a model.
    tol_vec <- equality_tolerances_reactive$value[[as.character(loop_numb_models)]]

    keep_as_equal <- rep(TRUE, length(all_operators))
    eq_occurrence <- 0

    for (loop_tune in seq_along(all_operators)) {
      if (all_operators[loop_tune] == "equal") {
        eq_occurrence <- eq_occurrence + 1
        this_tol <- if (!is.null(tol_vec) && eq_occurrence <= length(tol_vec) && !is.na(tol_vec[eq_occurrence])) tol_vec[eq_occurrence] else 0

        if (this_tol != 0) {
          keep_as_equal[loop_tune] <- FALSE

          add_ineq_eq_left <- rbind(add_ineq_eq_left, (ineq_eq_left[loop_tune, ]))
          add_ineq_eq_left <- rbind(add_ineq_eq_left, d2q(-1 * q2d(ineq_eq_left[loop_tune, ])))

          add_ineq_eq_right <- c(
            add_ineq_eq_right,
            q2d(ineq_eq_right[loop_tune]) + this_tol,
            -(q2d(ineq_eq_right[loop_tune])) + this_tol
          )

          add_all_operators <- c(add_all_operators, rep("below", 2))
        }
      }
    }

    if (length(add_all_operators) > 0) {
      add_ineq_eq_right <- d2q(add_ineq_eq_right)

      ineq_eq_left <- ineq_eq_left[keep_as_equal, ]
      ineq_eq_right <- ineq_eq_right[keep_as_equal]
      all_operators <- all_operators[keep_as_equal]

      ineq_eq_left <- rbind(ineq_eq_left, add_ineq_eq_left)
      ineq_eq_right <- c(ineq_eq_right, add_ineq_eq_right)
      all_operators <- c(all_operators, add_all_operators)
    }



    all_operators_reactive$value <- all_operators
    ineq_eq_left_reactive$value <- ineq_eq_left
    ineq_eq_right_reactive$value <- ineq_eq_right
    numb_p_reactive$value <- numb_p
    probs_reactive$value <- probs
  }

  #### Function for Representations ####

  ####* H-representation input models ####

  # The main "Go" pipeline. Everything here shares local state (mytable_input,
  # empty_set, recomputed_models, ...) and runs strictly in this order —
  # later sections read results (and reactive values: h_reactive, v_reactive,
  # intersections_reactive, mixtures_reactive, items_reactive, ...) that
  # earlier sections wrote, so they cannot be safely reordered or extracted
  # without re-threading that state explicitly. Stages, in order:
  #   1. Parse Input tab rows into mytable_input (model name + spec)
  #   2. Build each model's H-representation (####** loop models)
  #   3. Auto-detect pairwise intersections for the Model Properties table
  #   4. Compute user-selected model intersections (####* Model intersection)
  #   5. Compute user-selected model mixtures (####* Model mixtures)
  #   6. Compute multi-item models — runs before replication so items models
  #      can serve as a replication base (####* Multiple-items models)
  #   7. Compute replication models (####* Replication models)
  #   8. Compute V-representations for everything selected (####* V-representation)
  #   9. Build the H-representation formula/LaTeX display and V-rep tables
  observeEvent(input$go_v_h, {
    # Skip if inputs are identical to the last recomputation.
    current_snap <- make_snapshot()
    if (!is.null(isolate(last_recomputed_snapshot())) &&
        identical(isolate(last_recomputed_snapshot()), current_snap)) {
      return()
    }
    last_recomputed_snapshot(current_snap)

    n_input_models <- counter_input$n

    ####** input

    mytable_input <- data.frame()

    for (loop in 1:counter_input$n) {
      mytable_input <- rbind(
        mytable_input,
        c(
          eval(parse(
            text = paste(
              "unlist(AllInputs()$textin_relations_name",
              loop,
              ")",
              sep = ""
            )
          )),
          eval(parse(
            text = paste(
              "unlist(AllInputs()$textin_relations_complete_",
              loop,
              ")",
              sep = ""
            )
          ))
        )
      )
    }

    colnames(mytable_input) <- c("model name", "input_models")
    # mytable_input[, 2] = str_replace_all( mytable_input[, 2],",",";") # !!!!!!!!

    names_models_reactive$value <- mytable_input$`model name`

    if (length(mytable_input$`model name`) >= 1)
      showTab(inputId = "tabs", target = "Model Properties")

    ####** start conversion ####

    empty_set <- numeric()

    # A "Mixture model" row (see mixture_model_flags) is deliberately left
    # with an empty spec — it's a placeholder, not a real constraint list
    # (see that reactiveValues' own comment) — so it shouldn't trip the
    # "you left a spec blank" validation below the way an ordinary empty
    # model would. It still gets converted like any other row after this
    # point, on whatever its (empty) spec parses to — full mixture support
    # in this pipeline is a separate, larger piece of follow-up work.
    mixture_rows <- as.integer(names(isolate(mixture_model_flags$value)))
    blocking_empty <- sum(mytable_input[, 2] == "" & !(seq_len(nrow(mytable_input)) %in% mixture_rows))

    if (blocking_empty == 0) {
      showTab(inputId = "tabs", target = "H-representation")

      ####** loop models ####

      numb_models_to_convert <- n_input_models

      feedback_h_models <- numeric()

      feedback_name <- numeric()

      cache <- isolate(model_snap_cache())
      recomputed_models <- character()
      h_dialog_open <- FALSE

      for (loop_numb_models in 1:numb_models_to_convert) {
        name_m <- mytable_input[loop_numb_models, 1]

        model_snap <- list(
          spec    = mytable_input[loop_numb_models, 2],
          n_p     = counter$n,
          # A renamed probability doesn't change `spec` (the typed
          # constraint text always uses the fixed p1/p2/... handles, not
          # the display name) but DOES change h_base's own column names
          # (see build_items_h_core) — without tracking it here, this
          # per-model cache kept reusing the pre-rename h_reactive entry
          # even after the global go_v_h snapshot check (see
          # make_snapshot's own comment on the same class of bug) was
          # fixed to no longer skip the whole recompute on a rename.
          p_names = vapply(seq_len(counter$n), function(i)
            isolate(input[[paste0("textin_name_", i)]]) %||% "", character(1)),
          approx = isolate(equality_tolerances_reactive$value[[as.character(loop_numb_models)]])
        )

        if (!is.null(cache[[name_m]]) &&
            identical(cache[[name_m]], model_snap) &&
            !is.null(isolate(h_reactive[[name_m]]))) {
          feedback_name <- c(feedback_name, name_m)
          next
        }
        recomputed_models <- c(recomputed_models, name_m)

        all_h_names <- mytable_input[seq_len(numb_models_to_convert), 1]
        h_prog_title <- paste0("Computing H-representation: ", name_m)
        h_prog_text <- fairy_progress_text(
          items = setdiff(all_h_names, feedback_name),
          done = feedback_name, total = numb_models_to_convert)
        if (!h_dialog_open) {
          fairy_progress_open(h_prog_title, h_prog_text)
          h_dialog_open <- TRUE
        } else {
          fairy_progress_update(title = h_prog_title, text = h_prog_text)
        }
        Sys.sleep(fairy_progress_hold(numb_models_to_convert))



        feedback_name <- c(feedback_name, name_m)


        # A malformed specification should report the offending model instead
        # of taking the whole app down.
        parse_ok <- tryCatch(
          {
            extract_info(mytable_input[loop_numb_models, 2], loop_numb_models, mytable_input)
            TRUE
          },
          error = function(e) FALSE
        )
        # Captured immediately, not re-read later via isolate() — extract_info()
        # writes n_user_clauses_reactive$value as a side effect, and it's the
        # SAME shared reactive for every model in this loop. The h_raw_rows_
        # reactive assignment further down used to re-read it fresh at that
        # point instead of using this captured value, which was fine as long
        # as nothing else touched the reactive in between — but was one
        # accidental extra write away (this loop, a future edit, anything
        # triggering a flush) from silently attributing the WRONG model's
        # clause count to THIS model's h_raw_rows_reactive entry, breaking
        # redundant-clause highlighting for it intermittently. Capturing right
        # here removes that window entirely.
        n_user_clauses_here <- isolate(n_user_clauses_reactive$value)

        if (!parse_ok) {
          spec_txt <- mytable_input[loop_numb_models, 2]

          bad <- tryCatch(
            {
              pieces <- split_spec(spec_txt)
              pieces[vapply(str_replace_all(pieces, " ", ""),
                is_nonlinear_constraint,
                FUN.VALUE = logical(1), n_p = counter$n
              )]
            },
            error = function(e) character()
          )

          if (length(bad) > 0) {
            msg <- paste0(
              "This constraint is not <b>linear</b> in the probabilities:",
              "<br><br><b>", paste(bad, collapse = "</b><br><b>"), "</b>",
              "<br><br>A model is a polytope, so a probability may not be divided by, ",
              "multiplied by, or raised to the power of another probability. ",
              "Please rewrite the constraint so that each probability appears only ",
              "multiplied by a number."
            )
          } else {
            msg <- paste0(
              "Please check the input:<br><br><b>", spec_txt, "</b>",
              "<br><br>Separate constraints with \";\" or \",\" and use only linear ",
              "terms in the probabilities."
            )
          }

          fairy_progress_close()
          shinyalert(
            title = paste0("Could not read the specification of model ", name_m, "."),
            text = msg,
            html = TRUE, type = "error", closeOnClickOutside = TRUE
          )
          return(NULL)
        }

        feedback_h_models <- paste(feedback_h_models,
          paste(
            "#", loop_numb_models, " of ",
            numb_models_to_convert,
            ": Creating H-representation of ",
            name_m
          ),
          sep = "<br>"
        )

        probs <- isolate(probs_reactive$value)

        ineq_eq_left <- isolate(ineq_eq_left_reactive$value)
        ineq_eq_right <- isolate(ineq_eq_right_reactive$value)
        all_operators <- isolate(all_operators_reactive$value)

        if (sum(all_operators != "equal") > 0 &
          sum(all_operators == "equal") > 0) {
          h_representation <- makeH(
            ineq_eq_left[all_operators != "equal", ],
            ineq_eq_right[all_operators != "equal"],
            ineq_eq_left[all_operators == "equal", ],
            ineq_eq_right[all_operators == "equal"]
          )
        }

        if (sum(all_operators != "equal") == 0 &
          sum(all_operators == "equal") > 0) {
          h_representation <- makeH(
            a2 = ineq_eq_left,
            b2 = ineq_eq_right
          )
        }

        if (sum(all_operators != "equal") > 0 &
          sum(all_operators == "equal") == 0) {
          h_representation <- makeH(
            ineq_eq_left,
            ineq_eq_right
          )
        }

        names_p_multi <- c("", "", probs[, 2])

        # Row order matches ineq_eq_left (user clauses first, automatic
        # 0-1 bounds appended after) only when no "=" clauses are mixed in
        # with inequalities — the makeH() branches above reorder rows
        # (all inequalities, then all equalities) when both are present.
        # n_user_clauses is still used to separate "your clause" from
        # "automatic bound" in that mixed case; it can mislabel a handful
        # of rows there, but is exact for the common inequality-only case.
        # 'mixed': whether both equalities and inequalities were present
        # for this model — that's exactly the condition (see comment
        # above) under which makeH() reorders rows relative to
        # ineq_eq_left's original per-clause order, so redundant_rows can
        # no longer be trusted to index into the typed clause order.
        # Used by the "Model Specification" redundant-clause highlighting
        # (see model_redundant_clause_flags) to skip that model entirely
        # rather than risk mislabeling a clause as redundant.
        mixed_eq_ineq <- sum(all_operators != "equal") > 0 && sum(all_operators == "equal") > 0
        if (nrow(h_representation) > 1) {
          red_check <- redundant(h_representation)
          h_raw_rows_reactive[[mytable_input[loop_numb_models, 1]]] <- list(
            raw_H = h_representation,
            redundant_rows = red_check$redundant,
            # implied.linearity: row numbers of inequality rows that,
            # TOGETHER, force an equality (e.g. p1<=p2 and p1>=p2 typed
            # separately actually assert p1==p2) — rcdd computes this
            # directly as part of redundant(), no extra LP needed.
            implied_linearity = red_check$implied.linearity,
            n_user_clauses = n_user_clauses_here,
            mixed_eq_ineq = mixed_eq_ineq
          )
          h_representation <- red_check$output
        } else {
          h_raw_rows_reactive[[mytable_input[loop_numb_models, 1]]] <- list(
            raw_H = h_representation,
            redundant_rows = integer(0),
            implied_linearity = integer(0),
            n_user_clauses = n_user_clauses_here,
            mixed_eq_ineq = mixed_eq_ineq
          )
        }

        colnames(h_representation) <- names_p_multi

        valid_hrep <- lpcdd(h_representation, rep("0", ncol(h_representation) - 2))
        valid_hrep <- ifelse(valid_hrep$solution.type == "Inconsistent", 0, 1)


        ####* Save h-represenation ####

        h_reactive[[mytable_input[loop_numb_models, 1]]] <- h_representation
        cache[[name_m]] <- model_snap


        if (valid_hrep == 0) {
          # loop_numb_models always ranges within 1:nrow(mytable_input) here
          # (both are built from counter_input$n), so this branch was
          # unconditionally taken; a since-removed `else` referenced an
          # undefined `mytable_inter` object that would have crashed this
          # handler had the loop bound ever changed relative to
          # mytable_input.
          empty_set <- c(empty_set, mytable_input[loop_numb_models, 1])
        }

        if (loop_numb_models == numb_models_to_convert) {
          fairy_progress_close()
          show_title <- "H-representation created"
          add_text <- fairy_success_chips(feedback_name)
          if (length(empty_set) > 0) {
            add_text <- paste0(add_text,
              "<p style='margin-top:12px;color:#b05000;font-size:13px;'><b>Warning:</b> do not interpret the ",
              "H-description of model(s) ", paste0(empty_set, collapse = ", "), " — they form an empty set. ",
              "For instance, the input 'p1 > .5; p1 < .1' cannot be simultaneously satisfied.</p>")
          }

          if (is.null(mytable_v_reactive$value) == FALSE) {
            shinyalert(
              title = show_title,
              text = add_text,
              html = TRUE,
              showConfirmButton = F,
              type = "success",
              animation = F
            )
            # Long enough to actually register as a distinct "H-rep done"
            # beat even when the computation itself was near-instant (fast
            # models otherwise flash this success message for well under a
            # second before the V-representation step's popup replaces it).
            Sys.sleep(0.8)
            shinyalert::closeAlert()
          } else {
            shinyalert(
              title = show_title,
              text = add_text,
              html = TRUE,
              showConfirmButton = F,
              type = "success",
              animation = F
            )
            Sys.sleep(0.8)
            shinyalert::closeAlert()
          }
        }
      }

      model_snap_cache(cache)

      ####* Auto-detect pairwise intersections for Model Properties ####
      # Run pairwise nesting/overlap LP checks now so intersection H-reps
      # exist before the user ever visits the Parsimony tab.
      {
        cur_model_names <- names_models_reactive$value
        n_models <- length(cur_model_names)
        if (n_models >= 2) {
          for (ii in seq_len(n_models - 1)) {
            for (jj in (ii + 1):n_models) {
              nm_i <- cur_model_names[ii]
              nm_j <- cur_model_names[jj]
              H_i <- isolate(h_reactive[[nm_i]])
              H_j <- isolate(h_reactive[[nm_j]])
              if (is.null(H_i) || is.null(H_j)) next

              ij <- tryCatch(nesting_lp(H_i, H_j), error = function(e) NA)
              ji <- tryCatch(nesting_lp(H_j, H_i), error = function(e) NA)

              # only overlapping (non-nested, non-disjoint) pairs need an
              # intersection model; nested models are subsets so volume is
              # the smaller model's own volume
              is_nested <- isTRUE(ij) || isTRUE(ji)
              is_disjoint <- isFALSE(ij) && isFALSE(ji)

              if (!is_nested && !is_disjoint && !is.na(ij) && !is.na(ji)) {
                int_key <- paste0("[", paste(sort(c(nm_i, nm_j)), collapse = " ∩ "), "]")
                if (is.null(isolate(h_reactive[[int_key]]))) {
                  H_comb <- rbind(H_i, H_j)
                  attr(H_comb, "representation") <- "H"
                  H_red <- tryCatch(redundant(H_comb)$output, error = function(e) H_comb)
                  h_reactive[[int_key]] <- H_red
                  cur_ci_names <- isolate(comp_int_reactive$names)
                  if (!int_key %in% cur_ci_names)
                    comp_int_reactive$names <- c(cur_ci_names, int_key)
                }
              }
            }
          }
        }
      }

      if (length(names_models_reactive$value) >= 2)
        showTab(inputId = "tabs", target = "Model Properties")

      ####* Model intersection ####
      # The intersection of a set of models is simply their combined
      # H-description (all constraints must hold simultaneously). The user
      # picks the models in the "Model intersections" modal, so any number of
      # models can be intersected (two -> pairwise, three or more ->
      # higher-order). A cheap LP feasibility test (lpcdd) detects an empty
      # intersection; otherwise the result is reduced with redundant() and
      # stored in h_reactive under "m1 AND m2 AND ..." so it can be displayed,
      # downloaded and converted to a V-description like any other model.
      # Computed BEFORE the V-representation block so V-descriptions can be
      # built for the intersections too.

      intersection_names <- character()
      disjoint_pairs <- character()

      int_table <- isolate(mytable_int_reactive$value)

      int_jobs <- list()
      if (!is.null(int_table) && ncol(int_table) >= 2 && nrow(int_table) >= 1) {
        # one intersection per ROW; columns are the models
        model_cols <- colnames(int_table)[-1]

        for (row_k in seq_len(nrow(int_table))) {
          ticked <- vapply(model_cols, function(cn) isTRUE(int_table[row_k, cn]), logical(1))
          selected_models <- model_cols[ticked]
          all_valid <- c(names_models_reactive$value,
            isolate(items_reactive$names))
          selected_models <- selected_models[selected_models %in% all_valid]

          if (length(selected_models) < 2) next

          int_name <- paste(selected_models, collapse = " AND ")

          # a set already handled by another row
          if (int_name %in% intersection_names || int_name %in% disjoint_pairs) next

          # skip if cached and no component was recomputed
          if (!is.null(isolate(h_reactive[[int_name]])) &&
              !any(selected_models %in% recomputed_models)) {
            intersection_names <- c(intersection_names, int_name)
            next
          }

          int_jobs[[int_name]] <- selected_models
        }
      }

      if (length(int_jobs) > 0) {
        fairy_progress_open(
          "Computing model intersection(s)",
          fairy_progress_text(items = names(int_jobs),
            parallel = isTRUE(isolate(input$use_parallel)), n_jobs = length(int_jobs))
        )
        Sys.sleep(fairy_progress_hold(length(int_jobs)))

        h_snapshot <- setNames(
          lapply(names(h_reactive), function(m) isolate(h_reactive[[m]])),
          names(h_reactive)
        )
        int_results <- par_lapply(names(int_jobs), function(int_name) {
          sel <- int_jobs[[int_name]]
          H_sel <- lapply(sel, function(m) h_snapshot[[m]])
          if (any(vapply(H_sel, is.null, logical(1)))) return(list(status = "missing"))
          ncols <- vapply(H_sel, ncol, integer(1))
          if (length(unique(ncols)) > 1) return(list(status = "incompatible"))
          H_comb <- do.call(rbind, H_sel)
          attr(H_comb, "representation") <- "H"
          feasible <- tryCatch(
            lpcdd(H_comb, rep("0", ncol(H_comb) - 2))$solution.type != "Inconsistent",
            error = function(e) TRUE
          )
          if (!feasible) return(list(status = "disjoint"))
          hh <- tryCatch({
            if (nrow(H_comb) > 1) H_comb <- redundant(H_comb)$output
            colnames(H_comb) <- colnames(H_sel[[1]])
            H_comb
          }, error = function(e) NULL)
          list(status = "ok", h = hh)
        })
        names(int_results) <- names(int_jobs)

        incompatible_ints <- character()
        for (int_name in names(int_results)) {
          res <- int_results[[int_name]]
          if (res$status == "incompatible") {
            incompatible_ints <- c(incompatible_ints, int_name)
          } else if (res$status == "disjoint") {
            disjoint_pairs <- c(disjoint_pairs, int_name)
          } else if (res$status == "ok" && !is.null(res$h)) {
            h_reactive[[int_name]] <- res$h
            intersection_names <- c(intersection_names, int_name)
            # Without this, the later V-representation recompute check
            # (`m %in% recomputed_models`) never sees intersection names, so
            # an intersection's V-rep (vertices used for plotting/volume)
            # stays stale after its H-rep has just been rebuilt above.
            recomputed_models <- c(recomputed_models, int_name)
          }
        }
        if (length(incompatible_ints) > 0) {
          shinyalert(
            title = "Incompatible parameter spaces",
            text  = paste0("The following intersections were skipped because the selected models have different numbers of parameters:\n",
              paste(incompatible_ints, collapse = "\n")),
            type = "warning", closeOnClickOutside = TRUE
          )
        }

        fairy_progress_close()
      }

      intersections_reactive$names <- intersection_names
      intersections_reactive$disjoint <- disjoint_pairs

      ####* Model mixtures ####
      # The mixture of a set of models is the convex hull of the union of their
      # V-representations. Pool all vertices, then call scdd() to get the H-rep
      # of the convex hull. Must run AFTER the V-rep block since it needs
      # v_reactive to be populated.
      #
      # Sourced from derived_model_sources (the "Mixture model" button on
      # the Input tab — see its own comment) rather than the old
      # mytable_mix_reactive dialog table: a mixture is now a real row in
      # the model list (with its own given name, e.g. "mix_m1_m2") instead
      # of a synthetic "m1 OR m2" name kept separate from the main model
      # list. Its trivial/empty H-representation from the main per-model
      # loop above gets OVERWRITTEN here with the real convex-hull result.
      # No caching against a previous run — mixtures are recomputed every
      # Go press, which is fine since the underlying scdd() work is only
      # as expensive as an intersection of the same size.

      mixture_names <- character()
      mixture_specs <- Filter(function(x) x$type == "mixture", isolate(derived_model_sources$value))

      if (length(mixture_specs) > 0) {
        mix_feedback <- character()
        mix_dialog_open <- FALSE
        all_mix_names <- vapply(names(mixture_specs), function(mix_key) {
          mix_row_idx <- suppressWarnings(as.integer(mix_key))
          if (is.na(mix_row_idx) || mix_row_idx > nrow(mytable_input)) return(NA_character_)
          mytable_input$`model name`[mix_row_idx]
        }, character(1))
        all_mix_names <- all_mix_names[!is.na(all_mix_names)]

        for (mix_key in names(mixture_specs)) {
          info <- mixture_specs[[mix_key]]
          mix_row_idx <- suppressWarnings(as.integer(mix_key))
          if (is.na(mix_row_idx) || mix_row_idx > nrow(mytable_input)) next
          mix_name <- mytable_input$`model name`[mix_row_idx]

          selected_mix <- mytable_input$`model name`[info$source_idx[info$source_idx <= nrow(mytable_input)]]
          all_valid_mix <- c(names_models_reactive$value,
            isolate(items_reactive$names))
          selected_mix <- selected_mix[selected_mix %in% all_valid_mix]

          if (length(selected_mix) < 2) next

          mix_prog_title <- paste0("Computing mixture: ", mix_name)
          mix_prog_text <- fairy_progress_text(
            items = setdiff(all_mix_names, mix_feedback),
            done = mix_feedback, total = length(mixture_specs),
            parallel = isTRUE(isolate(input$use_parallel)), n_jobs = length(selected_mix))
          if (!mix_dialog_open) {
            fairy_progress_open(mix_prog_title, mix_prog_text)
            mix_dialog_open <- TRUE
          } else {
            fairy_progress_update(title = mix_prog_title, text = mix_prog_text)
          }
          Sys.sleep(fairy_progress_hold(length(mixture_specs)))

          # The model H-reps do not include simplex constraints, so their raw
          # V-reps contain only rays/lines (no finite vertices).  To compute a
          # geometrically meaningful mixture we must add simplex constraints
          # (p_i >= 0, sum = 1) to each component before extracting vertices.
          h_base_mix <- isolate(h_reactive[[selected_mix[1]]])
          n_p_mix    <- ncol(h_base_mix) - 2L
          cube_h_mix <- d2q(rbind(
            makeH(a1 = -diag(n_p_mix), b1 = rep(0, n_p_mix)),
            makeH(a1 =  diag(n_p_mix), b1 = rep(1, n_p_mix))
          ))

          h_mix_snapshot <- setNames(
            lapply(selected_mix, function(m) isolate(h_reactive[[m]])),
            selected_mix
          )

          V_sel <- par_lapply(selected_mix, function(m) {
            h_comp <- h_mix_snapshot[[m]]
            if (ncol(h_comp) != ncol(cube_h_mix)) return(NULL)
            h_full <- rbind(h_comp, cube_h_mix)
            attr(h_full, "representation") <- "H"
            v_out  <- tryCatch(scdd(h_full)$output, error = function(e) NULL)
            if (is.null(v_out)) return(NULL)
            v_out[v_out[, 2] == "1", , drop = FALSE]
          })
          V_sel <- Filter(function(v) !is.null(v) && nrow(v) > 0, V_sel)
          if (length(V_sel) == 0) {
            fairy_progress_close()
            mix_dialog_open <- FALSE
            next
          }

          h1_cols <- colnames(isolate(h_reactive[[selected_mix[1]]]))
          mix_ncols <- vapply(selected_mix, function(m) {
            h <- isolate(h_reactive[[m]]); if (is.null(h)) NA_integer_ else ncol(h)
          }, integer(1))
          if (length(unique(na.omit(mix_ncols))) > 1) {
            ref_ncols <- mix_ncols[1]
            bad_models <- selected_mix[!is.na(mix_ncols) & mix_ncols != ref_ncols]
            fairy_progress_close()
            mix_dialog_open <- FALSE
            shinyalert(
              title = "Incompatible parameter spaces",
              text  = paste0(
                "Mixture '", mix_name, "' was skipped because the following models have a different number of parameters than the others: ",
                paste(bad_models, collapse = ", "),
                ". Models can only be mixed if they share the same parameter space."
              ),
              type = "warning", closeOnClickOutside = TRUE
            )
            next
          }
          mix_result <- tryCatch({
            V_comb <- do.call(rbind, V_sel)
            # rbind() drops the representation attribute; restore it so scdd
            # treats V_comb as a V-rep (not an H-rep) and returns the H-rep
            # of the convex hull.
            attr(V_comb, "representation") <- "V"
            hh <- scdd(V_comb)$output
            if (nrow(hh) > 1) {
              hh <- redundant(hh)$output
              attr(hh, "representation") <- "H"
            }
            colnames(hh) <- h1_cols
            vv <- scdd(hh)
            v1 <- isolate(v_reactive[[selected_mix[1]]])
            if (!is.null(v1)) colnames(vv$output) <- colnames(v1$output)
            list(h = hh, v = vv)
          }, error = function(e) NULL)

          if (!is.null(mix_result)) {
            h_reactive[[mix_name]] <- mix_result$h
            v_reactive[[mix_name]] <- mix_result$v
            mixture_names <- c(mixture_names, mix_name)
          }
          mix_feedback <- c(mix_feedback, mix_name)
        }
        if (mix_dialog_open) fairy_progress_close()
      }

      mixtures_reactive$names <- mixture_names

      ####* Multiple-items models ####
      # Runs BEFORE replication so items models can serve as base for replication.

      items_names  <- character()
      items_specs  <- isolate(mytable_items_reactive$value)
      items_dialog_open <- FALSE
      # Parallel to items_specs — records the computed items_name actually
      # used for h_reactive under THIS spec's position, filled in below
      # regardless of whether it was freshly computed or reused from
      # cache. Used after this block to alias h_reactive back onto
      # whatever name(s) the owning row(s) currently DISPLAY (see the
      # alias pass' own comment for why that can differ from items_name).
      items_name_by_pos <- rep(NA_character_, length(items_specs))

      if (!is.null(items_specs) && length(items_specs) > 0) {
        for (spec_pos in seq_along(items_specs)) {
          spec <- items_specs[[spec_pos]]
          base_m  <- spec$base
          factors <- spec$factors
          pf      <- spec$param_factors

          if (is.null(base_m) || !base_m %in% names(h_reactive)) next
          if (length(factors) == 0) next
          h_base  <- isolate(h_reactive[[base_m]])

          # Which original parameter maps to which stimulus type — NOT
          # captured by items_name (base + factor names/counts + type), so
          # two specs can collide on the same items_name while assigning
          # parameters differently. Compare against the cached matrix's own
          # tag (see type_assignment attr below) to force a recompute when
          # only the assignment changed, instead of silently reusing stale
          # data for the new assignment.
          # pf can include 0 for parameters assigned to "Shared" (not tied
          # to any factor) — factors[[0]] is invalid in R, so those must be
          # excluded from the factor-name lookup rather than indexed.
          pf_assigned <- pf[pf > 0]
          cur_assignment <- paste(
            vapply(pf_assigned, function(fi) factors[[fi]]$name, character(1)),
            collapse = ","
          )

          # A type PER FACTOR (factor_types) and a tolerance PER
          # PROBABILITY (eps_per_param — see build_items_h_mixed's own
          # comment) — produces exactly ONE combined model per spec,
          # named from each factor's own type and its members' own
          # tolerances.
          factor_types  <- spec$factor_types
          eps_per_param <- spec$eps_per_param

          factor_tag_full <- paste(vapply(seq_along(factors), function(fi)
            items_factor_tag(factors[[fi]]$name, factors[[fi]]$n, factor_types[fi],
              member_eps_for_factor(pf, eps_per_param, fi)),
            character(1)), collapse = ", ")
          items_name <- paste0(base_m, " [", factor_tag_full, "]")
          items_name_by_pos[spec_pos] <- items_name

          if (!items_name %in% items_names) {
            cached_h <- isolate(h_reactive[[items_name]])
            if (!is.null(cached_h) && !base_m %in% recomputed_models &&
                identical(attr(cached_h, "type_assignment"), cur_assignment)) {
              items_names <- c(items_names, items_name)
            } else {
              items_prog_title <- paste0("Computing items model: ", items_name)
              items_prog_text <- fairy_progress_text(
                items = items_name, done = items_names)
              if (!items_dialog_open) {
                fairy_progress_open(items_prog_title, items_prog_text)
                items_dialog_open <- TRUE
              } else {
                fairy_progress_update(title = items_prog_title, text = items_prog_text)
              }
              Sys.sleep(0.7)

              h_it <- tryCatch({
                # build_items_h_mixed honors each factor's own type
                # (joint/substitutable/identical/average, real semantics
                # for each — see its own comment) independently, so it is
                # correct whether every factor shares one type or they
                # are genuinely mixed.
                m_mat <- build_items_h_mixed(h_base, factors, pf, factor_types, eps_per_param)
                saved_cn <- colnames(m_mat)
                m_mat <- d2q(m_mat); attr(m_mat, "representation") <- "H"
                if (nrow(m_mat) > 1) {
                  m_mat <- redundant(m_mat)$output; attr(m_mat, "representation") <- "H"
                }
                colnames(m_mat) <- saved_cn
                attr(m_mat, "type_assignment") <- cur_assignment
                m_mat
              }, error = function(e) { message("items error [", items_name, "]: ", conditionMessage(e)); NULL })

              if (!is.null(h_it)) {
                h_reactive[[items_name]] <- h_it
                items_names <- c(items_names, items_name)
                recomputed_models <- c(recomputed_models, items_name)
              } else {
                fairy_progress_close()
                items_dialog_open <- FALSE
                shinyalert(title = paste0("Failed: ", items_name),
                  text = "Check the R console for details.",
                  type = "error", closeOnClickOutside = TRUE)
              }
            }
          }
        }
        if (items_dialog_open) fairy_progress_close()
      }

      # An items row's REAL data always lives in h_reactive under its
      # spec-computed items_name (base model's name + factor tag) — but
      # the "loop models" pass just above processes EVERY row, including
      # this one, off its own (blank, since the row's spec box is
      # disabled) typed text, writing an empty/unconstrained
      # h_reactive[[<row's own current display name>]] for it — normally
      # harmless, since that display name starts out equal to items_name
      # and this items block's own write (above, same h_reactive[[
      # items_name]] key) simply overwrites it right back to the correct
      # value. But once a user renames the ROW itself (its Name box) to
      # something that no longer matches the auto-generated items_name —
      # a supported, ordinary rename, no different from renaming any
      # other model — the two keys diverge: the row's own display name
      # keeps pointing at the stale EMPTY entry the main loop wrote,
      # while the correct data sits under a name nothing on screen shows
      # anymore. Confirmed directly: renaming an items row produced a
      # SECOND, unconstrained card under the new name, alongside the
      # correct one still sitting under the old computed name. Move
      # (not just copy) h_reactive's entry onto whatever name the row
      # currently displays, and drop the old computed name out of
      # items_names too, so exactly one card shows — under the row's
      # own current name, same as every other model type already works.
      if (length(items_specs) > 0) {
        spec_idx_map <- isolate(item_model_spec_idx$value)
        for (row_key in names(spec_idx_map)) {
          si <- spec_idx_map[[row_key]]
          if (is.null(si) || si < 1 || si > length(items_name_by_pos) || is.na(items_name_by_pos[si])) next
          computed_name <- items_name_by_pos[si]
          row_idx <- suppressWarnings(as.integer(row_key))
          if (is.na(row_idx) || row_idx < 1 || row_idx > nrow(mytable_input)) next
          row_display_name <- mytable_input[row_idx, 1]
          if (!identical(row_display_name, computed_name) &&
              !is.null(isolate(h_reactive[[computed_name]]))) {
            h_reactive[[row_display_name]] <- isolate(h_reactive[[computed_name]])
            h_reactive[[computed_name]] <- NULL
            items_names[items_names == computed_name] <- row_display_name
            # recomputed_models also still says computed_name — same
            # rename applied here so the V-representation block's own
            # "was this model just recomputed" check (a few lines below,
            # keyed by the row's CURRENT display name) still finds it and
            # doesn't skip a stale pre-rename V-rep cache entry.
            recomputed_models[recomputed_models == computed_name] <- row_display_name
          }
        }
      }

      items_reactive$names <- items_names

      ####* V-representation ####
      # Runs after items so all H-reps are available.

      n_model_rows <- nrow(mytable_input)

      # Used to blindly overwrite mytable_v_reactive$value's names by
      # POSITION with mytable_input's current names — a leftover
      # assumption from when that table was always fully rebuilt (by the
      # old "V-representations" dialog's Submit) in exactly mytable_
      # input's order, so position-based realignment happened to be safe.
      # The per-model V toggle (fairy-vrep-toggle-btn) instead grows this
      # table incrementally via rbind() as models get toggled, in
      # whatever order that happened — no longer guaranteed to match
      # mytable_input's order or length, so renaming by position silently
      # reassigned some models' V-representation selection to a
      # DIFFERENT model. Now a no-op; a model renamed after being toggled
      # on will keep its selection under the old name instead (a milder,
      # pre-existing-style edge case, not active misattribution).
      sync_model_rows <- function(nm) nm

      if (!is.null(mytable_v_reactive$value)) {
        mytable_v_reactive$value$`Model name` <-
          sync_model_rows(mytable_v_reactive$value$`Model name`)
      }

      mytable_v <- mytable_v_reactive$value

      if (!exists("names_p_multi")) {
        probs_tmp <- isolate(probs_reactive$value)
        names_p_multi <- c("", "", probs_tmp[, 2])
      }

      if (!is.null(mytable_v) && sum(mytable_v[, 2]) > 0) {
        mytable_v_reactive$value$`Model name` <- sync_model_rows(mytable_v$`Model name`)
        mytable_v$`Model name` <- sync_model_rows(mytable_v$`Model name`)

        models_picked_for_v_representation <- mytable_v[which(mytable_v[, 2]), 1]
        models_picked_for_v_representation <-
          models_picked_for_v_representation[!is.na(models_picked_for_v_representation)]

        invalid_model <- character()

        models_to_compute_v <- models_picked_for_v_representation[vapply(
          models_picked_for_v_representation,
          function(m) is.null(isolate(v_reactive[[m]])) || m %in% recomputed_models,
          logical(1)
        )]

        if (length(models_to_compute_v) > 0 && isTRUE(isolate(input$use_parallel))) {
          # Each model's V-representation (vertex enumeration via scdd()) is
          # independent of the others, so this is the actual heavy lifting
          # worth parallelizing. All models run at once here, so there is no
          # meaningful "currently computing" single model to name — the
          # popup instead acknowledges parallel mode and the core count.
          n_v_jobs <- length(models_to_compute_v)
          fairy_progress_open("Computing V-representation(s)",
            fairy_progress_text(items = models_to_compute_v, parallel = TRUE, n_jobs = n_v_jobs))
          v_batch_start <- Sys.time()
          Sys.sleep(fairy_progress_hold(n_v_jobs))

          h_v_snapshot <- setNames(
            lapply(models_to_compute_v, function(m) isolate(h_reactive[[m]])),
            models_to_compute_v
          )
          v_results <- par_lapply(models_to_compute_v, function(m) {
            h_m <- h_v_snapshot[[m]]
            list(res = tryCatch(scdd(h_m), error = function(e) NULL), h_cn_m = colnames(h_m))
          })
          names(v_results) <- models_to_compute_v

          for (m in models_to_compute_v) {
            res <- v_results[[m]]$res
            h_cn_m <- v_results[[m]]$h_cn_m
            if (is.null(res) || nrow(data.frame(res)) == 0) {
              invalid_model <- c(invalid_model, m)
            } else {
              colnames(res$output) <- if (!is.null(h_cn_m) && length(h_cn_m) == ncol(res$output)) h_cn_m else names_p_multi
              v_reactive[[m]] <- res
            }
          }
          # Parallel mode computes every model in ONE batch (a single
          # dialog frame), so it can otherwise finish and vanish before
          # the dialog even registers, unlike the sequential H-rep loop
          # which naturally holds once per model. An earlier attempt at
          # this scaled the pad by job count and badly overshot for small
          # batches (multi-second freezes reported as "much slower" than
          # H-rep) — capped at a single fairy_progress_hold() worth here
          # instead, same as one extra "frame", regardless of batch size.
          v_elapsed <- as.numeric(difftime(Sys.time(), v_batch_start, units = "secs"))
          v_target_total <- fairy_progress_hold(n_v_jobs)
          if (v_elapsed < v_target_total) Sys.sleep(v_target_total - v_elapsed)
          fairy_progress_close()
        } else if (length(models_to_compute_v) > 0) {
          # Not in parallel mode: compute one model at a time, same
          # current-vs-done popup style as the H-representation loop above.
          v_done <- character()
          v_dialog_open <- FALSE
          for (m in models_to_compute_v) {
            v_prog_title <- paste0("Computing V-representation: ", m)
            v_prog_text <- fairy_progress_text(
              items = setdiff(models_to_compute_v, v_done),
              done = v_done, total = length(models_to_compute_v))
            if (!v_dialog_open) {
              fairy_progress_open(v_prog_title, v_prog_text)
              v_dialog_open <- TRUE
            } else {
              fairy_progress_update(title = v_prog_title, text = v_prog_text)
            }
            Sys.sleep(fairy_progress_hold(length(models_to_compute_v)))

            h_m <- isolate(h_reactive[[m]])
            res <- tryCatch(scdd(h_m), error = function(e) NULL)
            if (is.null(res) || nrow(data.frame(res)) == 0) {
              invalid_model <- c(invalid_model, m)
            } else {
              h_cn_m <- colnames(h_m)
              colnames(res$output) <- if (!is.null(h_cn_m) && length(h_cn_m) == ncol(res$output)) h_cn_m else names_p_multi
              v_reactive[[m]] <- res
            }
            v_done <- c(v_done, m)
          }
          if (v_dialog_open) fairy_progress_close()
        }

        v_add_text <- fairy_success_chips(models_picked_for_v_representation)
        if (length(empty_set) > 0) {
          v_add_text <- paste0(v_add_text,
            "<p style='margin-top:12px;color:#b05000;font-size:13px;'><b>Warning:</b> do not interpret the H ",
            "and V-descriptions of model(s) ", paste0(empty_set, collapse = ", "), " — they form an empty set. ",
            "For instance, the input 'p1 > .5; p1 < .1' cannot be simultaneously satisfied.</p>")
        }
        shinyalert(title = "V-representation created", text = v_add_text, html = TRUE,
          showConfirmButton = FALSE, type = "success", animation = FALSE)
        Sys.sleep(0.8)
        shinyalert::closeAlert()
      } else {
        hideTab(inputId = "tabs", target = "V-representation")
        hideTab(inputId = "tabs", target = "Plot Polytope(s)")
        hideTab(inputId = "tabs", target = "Plot Edge Cases")
      }

      ####* Display Formula H-Representation ####

      equation_all_total <- numeric()
      # Parallel plain-LaTeX collection (see equation_all_total_latex_
      # reactive's own comment) — one align* block appended per model,
      # alongside (not instead of) the HTML card text above.
      equation_all_total_latex <- character()

      # Global parameter display order (p1, p2, ... in their current,
      # possibly-renamed form), fixed once per Go run so param_color_map
      # can assign the SAME color to the same probability across every
      # model card below, instead of a fresh per-table assignment that
      # drifted whenever a model's own column order differed.
      global_param_names <- isolate(probs_reactive$value)[, 2]

      # unique(): mixture_names now overlaps names_models_reactive$value —
      # a "Mixture model" row is a real, named row in the model list
      # itself (see derived_model_sources), not a separate synthetic name
      # the way the old dialog-driven mixtures were.
      all_display_names <- unique(c(names_models_reactive$value, intersection_names, mixture_names, items_names))
      # Reorder to match the Input tab's own row order (top to bottom)
      # instead of this category-grouped order (plain models, then
      # intersections, then mixtures, then items) — an intersection or
      # mixture row can sit anywhere among the plain ones on Input (it's
      # a real row there too, not appended separately), so the H-rep/
      # V-rep cards should read top-to-bottom the same way. row_order
      # below is exactly that visual order; intersect() keeps only names
      # actually in scope here while adopting row_order's ordering, and
      # setdiff() tacks on anything (rare) not yet reflected in a row —
      # same fallback the old unqualified order effectively was.
      row_order <- if (counter_input$n > 0) vapply(seq_len(counter_input$n), function(i)
        input[[paste0("textin_relations_name", i)]] %||% paste0("m", i), character(1)) else character(0)
      all_display_names <- c(intersect(row_order, all_display_names), setdiff(all_display_names, row_order))
      for (loop_latex in seq_along(all_display_names)) {
        current_name <- all_display_names[loop_latex]

        h_representation_latex <- q2d(h_reactive[[current_name]])
        h_representation_latex <- fractions(h_representation_latex)

        p_names_lt <- colnames(h_representation_latex)[-(1:2)]
        n_p_lt    <- length(p_names_lt)

        # Remove adjacent duplicate parameter columns (can arise in some model constructions)
        if (n_p_lt > 1) {
          keep <- c(TRUE, p_names_lt[-1] != p_names_lt[-n_p_lt])
          if (!all(keep)) {
            keep_cols <- c(1L, 2L, which(keep) + 2L)
            h_representation_latex <- h_representation_latex[, keep_cols, drop = FALSE]
            p_names_lt <- p_names_lt[keep]
            n_p_lt <- length(p_names_lt)
          }
        }

        n_equal   <- sum(h_representation_latex[, 1] == 1)
        n_inequal <- sum(h_representation_latex[, 1] == 0)

        # Non-trivial inequality count, for the corner badge below — the
        # same "+p_i <= 1" / "-p_i <= 0" bound test used for the
        # fairy-trivial-row class further down, computed once up front
        # here so both the badge and the per-row class share one
        # definition instead of drifting apart.
        trivial_vec <- vapply(seq_len(nrow(h_representation_latex)), function(ri) {
          row <- h_representation_latex[ri, ]
          if (row[1] == 1) return(FALSE)
          rhs <- row[2]
          cf  <- -row[-(1:2)]
          nz  <- which(cf != 0)
          length(nz) == 1 && ((cf[nz] == 1 && rhs == 1) || (cf[nz] == -1 && rhs == 0))
        }, logical(1))
        n_inequal_real <- n_inequal - sum(trivial_vec)

        # One color per column, keyed by the parameter's position in the
        # app's global probability list, so the same probability keeps
        # the same color across every model card (see global_param_names
        # above and param_color_map's own comment).
        col_color <- param_color_map(p_names_lt, global_param_names)

        if (TRUE) {

          row_data <- lapply(seq_len(nrow(h_representation_latex)), function(ri) {
            row    <- h_representation_latex[ri, ]
            is_eq  <- row[1] == 1
            rhs    <- row[2]
            # rcdd's H-representation stores coefficients as the negation of
            # the constraint's true left-hand side (a = -v internally), so
            # the row must be negated back before display or the printed
            # inequality/equation reads in the wrong direction.
            coeffs <- -row[-(1:2)]
            # Inequalities always stay "<=" — never flipped to ">=" (an
            # earlier version flipped the whole row, negating every
            # coefficient and the rhs, whenever that meant fewer minus
            # signs; dropped because novices reading ">=" sporadically
            # mixed in with "<=" rows was its own source of confusion).
            # Minus signs are still cleared, just via the trick below
            # instead — moving negative terms across to the rhs, which
            # doesn't touch the operator at all.
            nz_all <- which(coeffs != 0)
            neg_all <- nz_all[coeffs[nz_all] < 0]
            # Move every negative term across to the rhs — "+p1 -p2 <= 0"
            # becomes "p1 <= p2", "-p1 -p2 +p3 <= 5" becomes
            # "p3 <= 5 +p1 +p2" — algebraically identical (add the term
            # to both sides), the general "move it to the other side"
            # from school algebra. Applies to equalities too (an "="
            # doesn't care which side a term is on either) — only the
            # earlier whole-row FLIP was inequality-specific (it swapped
            # the operator, which "=" has no direction to swap). If
            # EVERY coefficient is negative (e.g. the trivial "-p_i <= 0"
            # bound), moving all of them leaves the lhs with nothing —
            # handled below by showing an explicit "0" there
            # ("0 <= p_i") rather than a blank-looking gap in front of
            # the operator.
            move_idx <- neg_all
            # A negative CONSTANT (rhs < 0, e.g. "p3 <= p2 - 1/3") is the
            # same "novices reading a minus sign" problem the term-moving
            # above already solves for variable coefficients — move it to
            # the lhs too, same "add it to both sides" algebra, so it
            # shows up as a positive number there instead
            # ("1/3 + p3 <= p2"). Rendered in its own leading cell rather
            # than folded into one of the per-parameter <td>s, since it
            # isn't tied to any single parameter's column.
            lhs_const <- if (rhs < 0) abs(rhs) else 0
            rhs_const <- if (rhs > 0) rhs else 0
            list(is_eq = is_eq, coeffs = coeffs, nz_all = nz_all, move_idx = move_idx,
                 lhs_const = lhs_const, rhs_const = rhs_const)
          })
          # co/aco carry the "fractions" class from the matrix-wide
          # fractions() call above; as.character()/paste0() on a
          # fractions value invoke its print formatter, which has the
          # same silent-rounds-tiny-numbers-to-0 issue as num2str_lin
          # was fixed for — go through num2str_lin on the plain numeric
          # instead of trusting the fractions formatter directly.
          math_of <- function(co, j) {
            aco <- abs(co)
            coef_str <- if (aco == 1) "" else num2str_lin(as.numeric(aco))
            paste0(if (co > 0) "+" else "-", coef_str, tex_name(p_names_lt[j]))
          }
          # Plain (Aligned layout): column position ALREADY is the
          # same-parameter cue there, so an added color would be
          # redundant noise, not a second signal.
          term_html <- function(co, j) paste0('\\(', math_of(co, j), '\\)')
          # Colored (Compact layout only): color by column (see
          # col_color/param_color_map), set before the \\(...\\) so
          # MathJax's rendered output inherits it (renders in
          # currentColor by default) — the substitute cue once Compact
          # has dropped column alignment.
          term_html_colored <- function(co, j)
            paste0('<span style="color:', col_color[j], ';">\\(', math_of(co, j), '\\)</span>')

          # ---- Aligned layout: one <td> per parameter column, same
          # column top-to-bottom for the same parameter (see
          # h_layout_toggle_group's own comment). Empty cells get almost no
          # padding since they carry no text — real savings only when a
          # column is genuinely unused by every row, but harmless
          # otherwise. Built twice, once plain and once with
          # term_html_colored — color used to be Compact-only (column
          # position already being Aligned's own same-parameter cue was
          # reason enough not to bother), but color is now its own
          # independent toggle (see h_layout_toggle_group / h_color_
          # toggle_group's own comments) that has to work under EITHER
          # layout, not just Compact. ----
          build_aligned_rows <- function(term_fn) {
            vapply(seq_along(row_data), function(ri) {
              d <- row_data[[ri]]
              lhs_empty <- d$lhs_const == 0 && length(d$move_idx) > 0 && length(d$move_idx) == length(d$nz_all)
              bg <- if (ri %% 2 == 0) 'background:var(--ct-default);' else 'background:var(--fairy-panel);'
              lhs_const_cell <- paste0('<td style="padding:3px ', if (d$lhs_const > 0) '4px' else '1px', ';text-align:right;">',
                if (d$lhs_const > 0) paste0('\\(+', num2str_lin(as.numeric(d$lhs_const)), '\\)') else '',
                '</td>')
              td_cells <- paste(vapply(seq_len(n_p_lt), function(j) {
                co <- d$coeffs[j]
                if (co == 0 || j %in% d$move_idx)
                  return('<td style="padding:3px 1px;"></td>')
                paste0('<td style="padding:3px 4px;text-align:right;">', term_fn(co, j), '</td>')
              }, character(1)), collapse = "")
              td_cells <- paste0(lhs_const_cell, td_cells)
              op_math  <- if (d$is_eq) "=" else "\\leq"
              if (lhs_empty) op_math <- paste0("0 ", op_math)
              # Right side laid out the same way as the left: its own
              # constant cell, then one cell per parameter, each moved term
              # in the SAME column index j its parameter already occupies
              # on the lhs — column order lines up top-to-bottom with the
              # lhs's own p1, p2, p3... layout instead of a free-form blob
              # that could list moved terms in a different order row to
              # row (items-expanded models lay columns out level-major,
              # not always p1-before-p2 — see the reordering this avoids).
              rhs_const_cell <- paste0('<td style="padding:3px ', if (d$rhs_const > 0) '4px' else '1px', ';text-align:right;">',
                if (d$rhs_const > 0) paste0('\\(', num2str_lin(as.numeric(d$rhs_const)), '\\)') else '',
                '</td>')
              rhs_td_cells <- paste(vapply(seq_len(n_p_lt), function(j) {
                if (!(j %in% d$move_idx))
                  return('<td style="padding:3px 1px;"></td>')
                paste0('<td style="padding:3px 4px;text-align:right;">', term_fn(abs(d$coeffs[j]), j), '</td>')
              }, character(1)), collapse = "")
              rhs_empty <- d$rhs_const == 0 && length(d$move_idx) == 0
              rhs_cells <- paste0(rhs_const_cell, rhs_td_cells)
              if (rhs_empty) {
                # Neither side has anything left after the constant/term
                # moves above (e.g. a bound that fully collapsed) — same
                # "show an explicit 0 instead of a blank-looking gap"
                # handling the lhs_empty case already gets, just mirrored
                # onto the (per-column) rhs: put it in the rhs's own
                # constant cell rather than leaving every cell empty.
                rhs_cells <- paste0(
                  '<td style="padding:3px 4px;text-align:right;">\\(0\\)</td>',
                  paste(rep('<td style="padding:3px 1px;"></td>', n_p_lt), collapse = "")
                )
              }
              # Trivial row: the automatic "keep this probability inside
              # [0,1]" bound rather than something the user actually typed
              # — see trivial_vec above (single source of truth, also used
              # for the card's non-trivial-count badge below). Flagged with
              # a class so the client-side show/gray/hide toggle (see the
              # tags$script below) can act on it without a server round-trip.
              tr_class <- if (trivial_vec[ri]) ' class="fairy-trivial-row"' else ""
              paste0('<tr', tr_class, ' style="', bg, '">',
                     td_cells,
                     '<td style="padding:3px 6px;text-align:center;">\\(', op_math, '\\)</td>',
                     rhs_cells,
                     '</tr>')
            }, character(1))
          }
          row_htmls_aligned_plain   <- build_aligned_rows(term_html)
          row_htmls_aligned_colored <- build_aligned_rows(term_html_colored)

          # ---- Compact layout: every row packs only its OWN terms into
          # one cell per side, so a row is exactly as wide as its own
          # content regardless of how many other parameters exist
          # elsewhere in the model — no reserved-but-empty columns. Built
          # twice, once plain and once with term_html_colored (col_color
          # is the substitute same-parameter cue once packing drops
          # column position) — color is its own independent toggle (see
          # h_color_toggle_group's own comment), not tied to this layout
          # specifically; Aligned gets the same plain/colored pair above
          # for the same reason. ----
          build_packed_rows <- function(term_fn) {
            vapply(seq_along(row_data), function(ri) {
              d <- row_data[[ri]]
              bg <- if (ri %% 2 == 0) 'background:var(--ct-default);' else 'background:var(--fairy-panel);'
              lhs_idx <- setdiff(d$nz_all, d$move_idx)
              lhs_parts <- c(
                if (d$lhs_const > 0) paste0('\\(+', num2str_lin(as.numeric(d$lhs_const)), '\\)') else NULL,
                vapply(lhs_idx, function(j) term_fn(d$coeffs[j], j), character(1))
              )
              if (length(lhs_parts) == 0) lhs_parts <- '\\(0\\)'
              rhs_parts <- c(
                if (d$rhs_const > 0) paste0('\\(', num2str_lin(as.numeric(d$rhs_const)), '\\)') else NULL,
                vapply(d$move_idx, function(j) term_fn(abs(d$coeffs[j]), j), character(1))
              )
              if (length(rhs_parts) == 0) rhs_parts <- '\\(0\\)'
              op_math <- if (d$is_eq) "=" else "\\leq"
              tr_class <- if (trivial_vec[ri]) ' class="fairy-trivial-row"' else ""
              paste0('<tr', tr_class, ' style="', bg, '">',
                     '<td style="padding:3px 8px;text-align:right;">', paste(lhs_parts, collapse = " "), '</td>',
                     '<td style="padding:3px 6px;text-align:center;">\\(', op_math, '\\)</td>',
                     '<td style="padding:3px 8px;text-align:left;">', paste(rhs_parts, collapse = " "), '</td>',
                     '</tr>')
            }, character(1))
          }
          row_htmls_packed_plain   <- build_packed_rows(term_html)
          row_htmls_packed_colored <- build_packed_rows(term_html_colored)

          tbl_html <- paste0(
            '<div class="fairy-h-table-scroll">',
            # display:inline-table (not the default 'table', which
            # stretches to fill its containing block's full width
            # regardless of how narrow the actual content is, spreading
            # the leftover space evenly across columns — exactly the
            # huge blank gaps reported) makes the table size to its own
            # content instead, like an inline-block. Also what makes
            # window.fitHTablesToWidth's scrollWidth measurement
            # meaningful at all — with the old stretch-to-fill behavior
            # scrollWidth was always ~= the wrapper's own width by
            # construction, so it never found anything to shrink.
            '<table class="fairy-h-aligned-plain-table" style="display:inline-table;border-collapse:collapse;font-size:0.85em;margin-top:6px;white-space:nowrap;">',
            paste(row_htmls_aligned_plain, collapse = ""),
            '</table>',
            '<table class="fairy-h-aligned-color-table" style="display:none;border-collapse:collapse;font-size:0.85em;margin-top:6px;white-space:nowrap;">',
            paste(row_htmls_aligned_colored, collapse = ""),
            '</table>',
            '<table class="fairy-h-packed-plain-table" style="display:none;border-collapse:collapse;font-size:0.85em;margin-top:6px;white-space:nowrap;">',
            paste(row_htmls_packed_plain, collapse = ""),
            '</table>',
            '<table class="fairy-h-packed-color-table" style="display:none;border-collapse:collapse;font-size:0.85em;margin-top:6px;white-space:nowrap;">',
            paste(row_htmls_packed_colored, collapse = ""),
            '</table></div>')
          count_badge <- paste0(
            '<div class="fairy-h-count-badge fairy-tooltip" data-tooltip="Excludes the automatic 0-1 bounds">',
            n_equal, ' eq · ', n_inequal_real, ' ineq</div>'
          )
          equation_all <- paste0(
            count_badge,
            '<div class="model-card-title">', current_name, '</div>', tbl_html)

          # Plain-LaTeX align* block for this model, reusing the same
          # math_of()/row_data/trivial_vec already computed above for the
          # HTML cards — real \leq/\text{...} macros (valid LaTeX either
          # way), just without the <table>/<span> markup that's only
          # meaningful to a browser/MathJax, which is what made the old
          # "LaTeX file" download fail to compile (see d_latex's own
          # comment). Trivial 0-1 bound rows are skipped here too, same
          # as the count badge already excludes them.
          latex_rows <- vapply(seq_along(row_data), function(ri) {
            if (trivial_vec[ri]) return(NA_character_)
            d <- row_data[[ri]]
            lhs_terms <- c(
              if (d$lhs_const > 0) paste0('+', num2str_lin(as.numeric(d$lhs_const))) else NULL,
              vapply(setdiff(d$nz_all, d$move_idx), function(j) math_of(d$coeffs[j], j), character(1))
            )
            if (length(lhs_terms) == 0) lhs_terms <- "0"
            rhs_terms <- c(
              if (d$rhs_const > 0) num2str_lin(as.numeric(d$rhs_const)) else NULL,
              vapply(d$move_idx, function(j) math_of(abs(d$coeffs[j]), j), character(1))
            )
            if (length(rhs_terms) == 0) rhs_terms <- "0"
            op_latex <- if (d$is_eq) "&=" else "&\\leq"
            paste0(paste(lhs_terms, collapse = ""), " ", op_latex, " ", paste(rhs_terms, collapse = ""), " \\\\")
          }, character(1))
          latex_rows <- latex_rows[!is.na(latex_rows)]

          equation_all_total_latex <- c(equation_all_total_latex, paste0(
            "\\subsection*{", gsub("_", "\\\\_", current_name), "}\n",
            if (length(latex_rows) > 0) {
              paste0("\\begin{align*}\n", paste(latex_rows, collapse = "\n"), "\n\\end{align*}\n")
            } else {
              "(no non-trivial constraints)\n"
            }
          ))
        } else {
          count_badge <- paste0(
            '<div class="fairy-h-count-badge fairy-tooltip" data-tooltip="Excludes the automatic 0-1 bounds">',
            n_equal, ' eq · ', n_inequal_real, ' ineq</div>'
          )
          equation_all <- paste0(
            count_badge,
            '<div class="model-card-title">', current_name, '</div>',
            '<p style="color:#888;font-size:0.9em;">H-representation: ',
            n_equal, ' equalities, ', n_inequal,
            ' inequalities (too many to display).</p>'
          )
        }

        equation_all_total <- c(equation_all_total, equation_all)
      }

      output$h <- renderUI({
        # Cards flow left-to-right and wrap, centered as a group — plain
        # stacked block divs just left-aligned under the narrow icon
        # column, leaving the rest of the wide tab mostly empty.
        div(
          class = "fairy-h-cards-wrap",
          withMathJax(
            lapply(seq_along(equation_all_total), function(i) {
              div(class = "model-card", helpText(HTML(equation_all_total[i])))
            })
          ),
          if (length(disjoint_pairs) > 0) {
            div(
              class = "model-card",
              div(class = "model-card-title", "Empty intersections"),
              helpText(HTML(paste0(
                "The following model pairs do not overlap (their intersection is empty): ",
                paste0("<b>", disjoint_pairs, "</b>", collapse = ", "), "."
              )))
            )
          }
        )
      })

      equation_all_total_reactive$value <- equation_all_total
      equation_all_total_latex_reactive$value <- equation_all_total_latex

      ####* Tables V-representation ####

      show_table <- names(v_reactive)

      all_displayable <- unique(c(names_models_reactive$value, intersection_names, mixture_names, items_names))
      # Same Input-tab row-order fix as all_display_names above (see its
      # own comment) — row_order was already computed there, in this
      # same server execution, so it's reused here rather than
      # recomputed.
      all_displayable <- c(intersect(row_order, all_displayable), setdiff(all_displayable, row_order))
      show_table <- show_table[show_table %in% all_displayable]
      show_table <- all_displayable[all_displayable %in% show_table]

      updatePickerInput(session,
        inputId = "go_example",
        choices = show_table,
        # Otherwise this starts with nothing selected and the Edge-Cases
        # plot is just blank until the user manually picks a model —
        # default to the first one so something renders immediately.
        selected = if (length(show_table) > 0) show_table[1] else character(0)
      )

      if (length(show_table) > 0) {
        showTab(inputId = "tabs", target = "V-representation")

        counter_table <- 1
        v_table_list <- list()

        for (loop_table in show_table) {

          v_table <- data.frame(v_reactive[[loop_table]])

          if (length(v_table) > 0) {
            v_table <- v_table[, 3:ncol(v_table)]
            v_table <- cbind(loop_table, v_table)

            rownames(v_table) <-
              paste("V_", 1:nrow(v_table), sep = "")

            h_cn_lt <- colnames(isolate(h_reactive[[loop_table]]))
            v_p_names_lt <- if (!is.null(h_cn_lt) && length(h_cn_lt) > 2) h_cn_lt[-(1:2)] else names_p_multi[-(1:2)]
            colnames(v_table) <- c("model_name", v_p_names_lt)

            v_table_list[[counter_table]] <- v_table
          }
          counter_table <- counter_table + 1
        }

        output$v_representation_table <- renderUI({
          # Same flow-left-to-right-and-wrap layout as the H-representation
          # cards (see fairy-h-cards-wrap), rather than one full-width
          # stack — most V-rep tables are narrow enough that stacking them
          # left a lot of empty space to their right.
          div(
            class = "fairy-h-cards-wrap",
            lapply(as.list(seq_len(length(v_table_list))), function(i) {
              id <- paste0("v_representation_table", i)
              model_nm <- tryCatch(as.character(v_table_list[[i]][1, 1]), error = function(e) "")
              # A NULL/empty entry in v_table_list (e.g. a slot never
              # populated for a skipped model) makes [1, 1] come back as
              # character(0) without throwing, which crashed the if() below
              # (a zero-length condition is a runtime error, not FALSE).
              if (length(model_nm) == 0) model_nm <- ""
              div(
                class = "model-card",
                if (nzchar(model_nm)) div(class = "model-card-title", model_nm),
                DT::dataTableOutput(id)
              )
            })
          )
        })

        for (loop_table in seq_len(length(v_table_list))) {
          if (is.null(v_table_list[[loop_table]]) == F) {
            local({
              id <- paste0("v_representation_table", loop_table)
              pl_t <- v_table_list[[loop_table]]
              colnames_pl_t <- colnames(pl_t)
              pl_t <- cbind(pl_t[, 1], matrix(as.character(fractions(q2d(unlist(pl_t[, 2:ncol(pl_t)])))), ncol = ncol(pl_t) - 1))
              colnames(pl_t) <- colnames_pl_t
              colnames(pl_t)[1] <- "Model Name"
              # DT column headers are raw text, not run through MathJax like
              # the H-representation table's HTML output is — wrap each
              # parameter name in \(...\) and rely on the drawCallback below
              # to retypeset after DT (re)draws, or these show as literal
              # LaTeX source ("{p_{1}}^{(1)}") instead of rendering.
              colnames(pl_t)[-1] <- paste0("\\(", tex_name(colnames_pl_t[-1]), "\\)")
              output[[id]] <- DT::renderDataTable({
                # Reads input$v_rep_words live, inside the render expression
                # (not baked in once at Go-time) — same trap as the earlier
                # "Model insights" checkbox: a value computed once outside a
                # render*()/reactive() block doesn't retrigger when the
                # checkbox changes afterward, only when Go runs again.
                display_t <- pl_t
                if (isTRUE(input$v_rep_words)) {
                  # Cast 0/1 vertex coordinates as words ("none"/"all"),
                  # same convention as Tables 1-2 in the paper: "a randomly
                  # drawn person cringes with probability one precisely
                  # when everybody in that (sub)population cringes," and
                  # probability zero precisely when nobody does. Fractional
                  # coordinates (e.g. 2/3) are left as fractions, matching
                  # Table 2, which uses the same mixed convention.
                  display_t[, -1][display_t[, -1] == "0"] <- "none"
                  display_t[, -1][display_t[, -1] == "1"] <- "all"
                }
                DT::datatable(display_t, escape = FALSE, options = list(
                  scrollX = TRUE,
                  drawCallback = DT::JS(
                    "function() { if (window.MathJax && MathJax.Hub) {",
                    "setTimeout(function() { MathJax.Hub.Queue(['Typeset', MathJax.Hub]); }, 30); } }"
                  )
                ))
              })
            })
          }
        }

        function_plot(v_table_list, length(v_table_list))
      }
    } else {
      shinyalert("Error", "Please type in all model specifications.",
        type = "error", closeOnClickOutside = T
      )
    }
  })

  #### Parsimony ####

  observeEvent(input$go, {
    last_parsimony_snapshot(make_parsimony_snapshot())
    input_volume <- isolate(input_volume_reactive)

    {
      setClass(
        "model_s4",
        representation(
          A = "matrix", b = "numeric",
          type = "character"
        )
      )

      # Volumes are computed ONLY for what the user actually specified on
      # the Input page: base models plus their explicit intersections and
      # mixtures (and replication/multi-item models) — never any
      # auto-detected pairwise intersection. An earlier version of this
      # tab used to also pre-compute every pairwise overlap between base
      # models for a comparison table; that table is gone, and silently
      # adding e.g. "[m1 ∩ m2]" rows the user never asked to see was
      # confusing, not useful — see comp_int_reactive's own comment.
      names_h_rep <- c(
        names_models_reactive$value,
        isolate(intersections_reactive$names),
        isolate(mixtures_reactive$names),
        isolate(items_reactive$names)
      )
      names_h_rep <- names_h_rep[!is.na(names_h_rep) & nchar(names_h_rep) > 0]

      n_rep <- 1
      parsim_rep <- numeric()


      for (loop_repeat_parsimony in 1:n_rep) {
        parsim <- numeric()
        act_dim <- numeric()

        for (loop_parsimony in seq_along(names_h_rep)) {
          act_pars <- (isolate(h_reactive[[names_h_rep[loop_parsimony]]]))
          act_pars <- matrix(as.character(act_pars), ncol = ncol(act_pars))


          not_full_dim <- ifelse(sum(q2d(act_pars)[, 1]) > 0, 1, 0)


          if (not_full_dim == 0) {
            left_pars <- q2d((act_pars[, 3:ncol(act_pars)]))
            right_pars <- q2d((act_pars[, 2]))

            model_s4 <- new("model_s4",
              A = -left_pars,
              b = right_pars,
              type = "Hpolytope"
            )


            ####* CB ####

            if (isolate(input$CB) == TRUE) {
              #####* settings strings ####


              settings_string_CB <- rep(NA, 6)

              settings_string_CB[1] <- ifelse(input_volume$value[1] == "default", "",
                paste("'error' =", input_volume$value[1], sep = "")
              )


              rand_w <- ifelse(input_volume$value[2] == "default", "default", input_volume_reactive$value[2])
              rand_w <- ifelse(rand_w == "Coordinate Directions Hit-and-Run", "CDHR", rand_w)
              rand_w <- ifelse(rand_w == "Random Directions Hit-and-Run", "RDHR", rand_w)
              rand_w <- ifelse(rand_w == "Ball Walk", "BaW", rand_w)
              rand_w <- ifelse(rand_w == "Billiard Walk", "BiW", rand_w)


              settings_string_CB[2] <- ifelse(rand_w == "default", "",
                paste("'random_walk' = '", rand_w, "'", sep = "")
              )

              settings_string_CB[3] <- ifelse(input_volume$value[3] == "default", "",
                paste("'walk_length' =", input_volume$value[3], sep = "")
              )

              settings_string_CB[4] <- ifelse(input_volume$value[4] == "default", "",
                paste("'win_len' =", input_volume$value[4], sep = "")
              )


              settings_string_CB[5] <- ifelse(input_volume$value[5] == "default", "",
                paste("'hpoly' =", input_volume$value[5], sep = "")
              )

              settings_string_CB[6] <- ifelse(input_volume$value[6] == "none", "",
                paste("'seed' =", input_volume$value[6], sep = "")
              )

              settings_string_CB <- settings_string_CB[settings_string_CB != ""]

              settings_string_CB <- paste(settings_string_CB, collapse = ",")

              if (settings_string_CB[1] != "") {
                settings_string_final_CB <- paste("list('algorithm' = 'CB',", settings_string_CB, ")")
              } else {
                settings_string_final_CB <- "list('algorithm' = 'CB')"
              }

              #####* calc volume ####

              act_vol_CB <- tryCatch(volume(model_s4,
                settings =
                  eval(parse(
                    text = (settings_string_final_CB)
                  ))
              ), error = function(e) NA)
            } else {
              act_vol_CB <- NA
            }


            ####* SoB ####

            if (isolate(input$SoB) == TRUE) {
              #####* settings strings ####


              settings_string_SoB <- rep(NA, 4)

              settings_string_SoB[1] <- ifelse(input_volume$value[7] == "default", "",
                paste("'error' =", input_volume$value[7], sep = "")
              )


              rand_w <- ifelse(input_volume$value[8] == "default", "default", input_volume_reactive$value[8])
              rand_w <- ifelse(rand_w == "Coordinate Directions Hit-and-Run", "CDHR", rand_w)
              rand_w <- ifelse(rand_w == "Random Directions Hit-and-Run", "RDHR", rand_w)
              rand_w <- ifelse(rand_w == "Ball Walk", "BaW", rand_w)
              rand_w <- ifelse(rand_w == "Billiard Walk", "BiW", rand_w)


              settings_string_SoB[2] <- ifelse(rand_w == "default", "",
                paste("'random_walk' = '", rand_w, "'", sep = "")
              )

              settings_string_SoB[3] <- ifelse(input_volume$value[9] == "default", "",
                paste("'walk_length' =", input_volume$value[9], sep = "")
              )


              settings_string_SoB[4] <- ifelse(input_volume$value[10] == "none", "",
                paste("'seed' =", input_volume$value[10], sep = "")
              )

              settings_string_SoB <- settings_string_SoB[settings_string_SoB != ""]

              settings_string_SoB <- paste(settings_string_SoB, collapse = ",")

              if (settings_string_SoB[1] != "") {
                settings_string_final_SoB <- paste("list('algorithm' = 'SOB',", settings_string_SoB, ")")
              } else {
                settings_string_final_SoB <- "list('algorithm' = 'SOB')"
              }


              #####* calc volume ####

              act_vol_SoB <- tryCatch(volume(model_s4,
                settings = eval(parse(
                  text = (settings_string_final_SoB)
                ))
              ), error = function(e) NA)
            } else {
              act_vol_SoB <- NA
            }

            ####* CG ####

            if (isolate(input$CG) == TRUE) {
              #####* settings strings ####


              settings_string_CG <- rep(NA, 5)

              settings_string_CG[1] <- ifelse(input_volume$value[11] == "default", "",
                paste("'error' =", input_volume$value[11], sep = "")
              )


              rand_w <- ifelse(input_volume$value[12] == "default", "default", input_volume_reactive$value[12])
              rand_w <- ifelse(rand_w == "Coordinate Directions Hit-and-Run", "CDHR", rand_w)
              rand_w <- ifelse(rand_w == "Random Directions Hit-and-Run", "RDHR", rand_w)
              rand_w <- ifelse(rand_w == "Ball Walk", "BaW", rand_w)
              rand_w <- ifelse(rand_w == "Billiard Walk", "BiW", rand_w)


              settings_string_CG[2] <- ifelse(rand_w == "default", "",
                paste("'random_walk' = '", rand_w, "'", sep = "")
              )

              settings_string_CG[3] <- ifelse(input_volume$value[13] == "default", "",
                paste("'walk_length' =", input_volume$value[13], sep = "")
              )

              settings_string_CG[4] <- ifelse(input_volume$value[14] == "default", "",
                paste("'win_len' =", input_volume$value[14], sep = "")
              )


              settings_string_CG[5] <- ifelse(input_volume$value[15] == "none", "",
                paste("'seed' =", input_volume$value[15], sep = "")
              )

              settings_string_CG <- settings_string_CG[settings_string_CG != ""]

              settings_string_CG <- paste(settings_string_CG, collapse = ",")

              if (settings_string_CG[1] != "") {
                settings_string_final_CG <- paste("list('algorithm' = 'CG',", settings_string_CG, ")")
              } else {
                settings_string_final_CG <- "list('algorithm' = 'CG')"
              }

              #####* calc volume ####

              act_vol_CG <- tryCatch(volume(model_s4,
                settings = eval(parse(
                  text = (settings_string_final_CG)
                ))
              ), error = function(e) NA)
            } else {
              act_vol_CG <- NA
            }


            n_dims <- ncol(act_pars) - 2
            act_dim <- c(act_dim, paste0("Full-dimensional (", n_dims, "D)"))
          } else {
            if (isolate(input$CB) == TRUE) {
              act_vol_CB <- 0
            } else {
              act_vol_CB <- NA
            }


            if (isolate(input$SoB) == TRUE) {
              act_vol_SoB <- 0
            } else {
              act_vol_SoB <- NA
            }


            if (isolate(input$CG) == TRUE) {
              act_vol_CG <- 0
            } else {
              act_vol_CG <- NA
            }

            # n_dims here is the same "how many parameter columns does
            # this model have" count as the Full-dimensional branch
            # above — it's the AMBIENT space the model is embedded in,
            # not the polytope's own actual dimension (which is exactly
            # what "not full-dimensional" means: a genuine equality
            # constraint — e.g. a repeated-items "Identical" type —
            # collapses the polytope onto a lower-dimensional subspace
            # within it, giving it zero volume there). "(6D)" alone reads
            # as directly contradicting "Not full-dimensional" right
            # next to it (reported directly: "not full-dimensional but
            # 6D?") — "in 6D space" instead makes clear that's the
            # space the (lower-dimensional) model lives IN, not its own
            # dimension.
            n_dims <- ncol(act_pars) - 2
            act_dim <- c(act_dim, paste0("Not full-dimensional (in ", n_dims, "D space)"))
          }

          parsim <- c(parsim, c(act_vol_CB, act_vol_SoB, act_vol_CG))
        }


        # Explicit levels = names_h_rep's own order (the order the user
        # actually entered/created these models in, on the Input page) —
        # as.factor() on its own sorts levels alphabetically, which is
        # what silently reordered every downstream table/plot away from
        # input order.
        parsim <- data.frame(
          "Model" = rep(factor(unlist(names_h_rep), levels = unique(unlist(names_h_rep))), each = 3),
          "Algorithm" = as.factor(rep(
            c("Cooling Bodies", "Sequence of Balls", "Cooling Gaussian"), length(names_h_rep)
          )),
          "Volume" = parsim,
          "Dimensionality" = rep(act_dim, each = 3)
        )


        selected_algos <- c(
          if (isolate(input$CB)) "Cooling Bodies",
          if (isolate(input$SoB)) "Sequence of Balls",
          if (isolate(input$CG)) "Cooling Gaussian"
        )
        parsim <- parsim[parsim$Algorithm %in% selected_algos, ]

        parsim_rep <- rbind(parsim_rep, parsim)
      }


      parsim_raw <- data.frame(parsim_rep[order(parsim_rep$Model, parsim_rep$Algorithm), ])

      # Cache mean volume across all selected algorithms per model for Model Properties table
      for (nm in unique(as.character(parsim_raw$Model))) {
        vol_rows <- parsim_raw[parsim_raw$Model == nm, "Volume"]
        vol_vals <- vol_rows[!is.na(vol_rows)]
        if (length(vol_vals) > 0)
          h_pars_reactive[[nm]] <- mean(vol_vals)
      }
      if (length(names_models_reactive$value) >= 2)
        showTab(inputId = "tabs", target = "Model Properties")

      parsim <- parsim_rep %>%
        group_by(Model, Algorithm, Dimensionality) %>%
        summarize(
          SD = sd(Volume), Volume = mean(Volume),
          Max_BF = 1 / Volume
        )


      parsim_wide <- parsim %>%
        pivot_wider(
          names_from = c("Algorithm"),
          values_from = c(Volume, SD, Max_BF)
        )

      parsim_wide_table <- (parsim_wide %>% select(-contains("SD")))
      parsim_wide_table[3:ncol(parsim_wide_table)] <- (parsim_wide_table[3:ncol(parsim_wide_table)])

      parsim_wide_table_reactive$value <- parsim_wide_table

      # Build display table: one row per model × algorithm
      parsim_display <- as.data.frame(parsim[order(parsim$Model, parsim$Algorithm), ])
      # Plain decimal rounds tiny volumes to "0.0000", and as.character()'s
      # default scientific notation ("5e-04") is inconsistent with the rest
      # of the app's number formatting — show a fixed "< m.mm x 10^-n" form
      # instead so small volumes stay legible without a raw R exponent.
      vol_str <- function(x) {
        rounded <- round(x, 4)
        # as.character() on a small-but-nonzero rounded value (e.g. 0.0005)
        # STILL falls back to R's own scientific notation ("5e-04") — must
        # force fixed notation explicitly, not just branch on rounded==0.
        if (rounded != 0) return(format(rounded, scientific = FALSE, trim = TRUE))
        if (x <= 0) return("0")
        exp  <- floor(log10(x))
        mant <- x / 10^exp
        sprintf("< %.2f x 10^%d", mant, exp)
      }
      # Same idea as vol_str above, mirrored for Max BF (= 1/volume, so a
      # tiny volume means a huge BF here) — a plain round() left values
      # like 8336282.75 as a long, hard-to-scan decimal; same "m.mm x
      # 10^n" scientific form once it's large enough to matter.
      # No "<"/">" here: vol_str's own "< X" doesn't mean "the true
      # volume is below X" (a real bound) — X there is computed AS
      # x/10^exp, i.e. it already IS the actual volume estimate, just
      # written in scientific notation because plain 4-decimal rounding
      # would otherwise show a misleading "0.0000". Since Max BF is
      # simply 1 divided by that same already-precise number, it's a
      # plain value here too, not a bound in either direction — an
      # earlier version of this added a ">" prefix on that (incorrect)
      # bound assumption; reverted after being questioned on it.
      bf_str <- function(x) {
        if (is.na(x)) return("—")
        # Max_BF = 1/Volume, so an exactly-zero (not merely tiny) Volume
        # — e.g. a full-dimensional model whose Monte Carlo estimate
        # landed on 0 without tripping the separate "Not full-dimensional"
        # branch above — gives Inf here. floor(log10(Inf)) is itself Inf,
        # and sprintf("%d", Inf) errors ("invalid format '%d' ... for
        # numeric objects") since Inf has no integer representation —
        # exactly the crash this guards against.
        if (!is.finite(x)) return("∞")
        if (abs(x) < 1000) return(as.character(round(x, 2)))
        exp  <- floor(log10(abs(x)))
        mant <- x / 10^exp
        sprintf("%.2f x 10^%d", mant, exp)
      }
      simple_tbl <- data.frame(
        Model          = gsub("^\\[(.*)\\]$", "\\1", as.character(parsim_display$Model)),
        Algorithm      = as.character(parsim_display$Algorithm),
        Dimensionality = parsim_display$Dimensionality,
        Volume         = ifelse(startsWith(parsim_display$Dimensionality, "Not full-dimensional"), "0",
                          ifelse(is.na(parsim_display$Volume), "—", vapply(parsim_display$Volume, function(v) if (is.na(v)) "—" else vol_str(v), character(1)))),
        `Max BF`       = ifelse(startsWith(parsim_display$Dimensionality, "Not full-dimensional"), "∞",
                          vapply(parsim_display$Max_BF, bf_str, character(1))),
        check.names    = FALSE
      )
      # Sort: base models first, intersections (containing ∩) below
      is_intersection <- grepl("∩", simple_tbl$Model)
      simple_tbl <- rbind(simple_tbl[!is_intersection, ], simple_tbl[is_intersection, ])

      # Whether an ACTUAL comparison table is being shown at all — mirrors
      # the exact "need >= 2 models" gate output$comparison_table_ui uses.
      # A model merely existing/being computed is NOT enough — with only
      # one model defined, that view shows a placeholder instead of a
      # table, so the parsimony detail table is the only place this
      # information exists and must default open. (Since the comparison
      # table is now a single unified table across base/replication/item
      # models — see all_comparison_model_names() — this is just its
      # length, not three separate per-type thresholds.)
      any_comparison_shown <- length(all_comparison_model_names()) >= 2
      parsim_needs_open$value <- !any_comparison_shown

      # Assign a color per unique model
      model_order  <- unique(simple_tbl$Model)
      palette_pool <- c("#e8edf5", "#e8f5eb", "#f5ebe8", "#f5f5e8", "#ede8f5", "#f5e8f0", "#e8f5f5", "#f5f0e8", "#eef5e8", "#f0e8f5")
      model_colors <- setNames(
        rep_len(palette_pool, length(model_order)),
        model_order
      )
      simple_tbl$.grp <- simple_tbl$Model

      output$parsim_table <- DT::renderDataTable({
        DT::datatable(simple_tbl,
          rownames = FALSE,
          options = list(
            scrollX   = FALSE,
            # autoWidth measures the table's CONTAINER at the exact
            # moment DataTables initializes — on the very first
            # Compute, that can happen before this panel has settled
            # into its final layout width, locking columns to a too-
            # narrow snapshot that a later re-init (Recalculate)
            # "fixes" only because the container is already correctly
            # sized by then. Leaving it off lets the table's own
            # width:100% (below) size it via normal CSS instead of
            # DataTables' own pixel-snapshotted column widths, so
            # there's nothing to get stuck on a stale measurement.
            autoWidth = FALSE,
            pageLength = 12,
            lengthMenu = c(6, 12, 18, 24, 48, 96),
            dom = "ltp",
            columnDefs = list(
              list(className = "dt-left", targets = 0),
              list(visible = FALSE, targets = ncol(simple_tbl) - 1)
            )
          ),
          width = "100%"
        ) %>%
          DT::formatStyle(
            columns = ".grp",
            target  = "row",
            backgroundColor = DT::styleEqual(names(model_colors), unname(model_colors))
          )
      })

      # One (grouped) bar per model x algorithm — same numbers as the
      # "Volume" column of the table below, just visual, so both a big
      # volume gap between models AND a disagreement between algorithms
      # on the same model are legible at a glance instead of read off a
      # column of numbers. "Not full-dimensional" reads as a genuine 0
      # volume here, same convention as the table's Volume column.
      bar_df <- data.frame(
        Model     = gsub("^\\[(.*)\\]$", "\\1", as.character(parsim_display$Model)),
        Algorithm = as.character(parsim_display$Algorithm),
        Volume    = ifelse(startsWith(parsim_display$Dimensionality, "Not full-dimensional"), 0, parsim_display$Volume)
      )
      bar_df <- bar_df[!is.na(bar_df$Volume), ]
      n_algos_used <- length(unique(bar_df$Algorithm))

      # Shared between the compact overview chart and its "expand" modal
      # view — same data/colors/ordering, only the font/margin sizing
      # scales up for the larger one so it doesn't look like the same
      # cramped chart just stretched.
      build_parsimony_plot <- function(large = FALSE) {
        if (nrow(bar_df) == 0) return(NULL)
        # Keep models in the same top-to-bottom order as the table above
        # rather than plotly's default (alphabetical/appearance) order.
        bar_df$Model <- factor(bar_df$Model, levels = rev(unique(bar_df$Model)))
        algo_colors <- c("#3a6df0", "#f0ad4e", "#5cb85c")[seq_len(n_algos_used)]
        fsize <- if (large) 15 else 11
        p <- plot_ly(
          bar_df,
          x = ~Volume, y = ~Model, color = ~Algorithm, type = "bar", orientation = "h",
          colors = algo_colors,
          marker = list(line = list(width = 0)),
          hovertemplate = "%{y} (%{fullData.name}): %{x:.4f}<extra></extra>"
        ) %>%
          layout(
            barmode = "group",
            bargap = 0.35, bargroupgap = 0.15,
            font = list(size = fsize, color = "#555"),
            paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
            xaxis = list(
              title = list(text = "Volume", standoff = if (large) 14 else 8), range = c(0, 1),
              gridcolor = "rgba(120,120,120,0.15)", zeroline = FALSE,
              showline = TRUE, linecolor = "rgba(120,120,120,0.35)"
            ),
            yaxis = list(title = "", showgrid = FALSE, ticks = "", automargin = TRUE),
            # Placed ABOVE the plot area (y > 1, paper coordinates) rather
            # than below it — a bottom-anchored legend kept landing on top
            # of the x-axis title regardless of how far down it was pushed,
            # since plotly draws the axis title relative to the actual
            # tick labels, not a fixed offset a legend's y can reliably
            # clear. Above the bars there's nothing else to collide with.
            legend = if (n_algos_used > 1) list(orientation = "h", x = 0, y = if (large) 1.1 else 1.2, font = list(size = fsize - 1)) else list(visible = FALSE, showlegend = FALSE),
            margin = if (large) list(l = 10, r = 30, t = if (n_algos_used > 1) 50 else 20, b = 50, pad = 6)
                     else list(l = 10, r = 20, t = if (n_algos_used > 1) 34 else 10, b = 34, pad = 4)
          ) %>%
          config(displayModeBar = large)
        if (n_algos_used <= 1) p <- p %>% layout(showlegend = FALSE)
        p
      }

      output$parsimony_bar_plot <- renderPlotly(build_parsimony_plot(large = FALSE))
      output$parsimony_bar_plot_large <- renderPlotly(build_parsimony_plot(large = TRUE))
      # Its home container renders with style="display:none" (see
      # output$parsimony_plot_ui) until expanded — Shiny suspends an
      # output's computation by default while its container isn't
      # visible, which would leave this stale/blank the first time it's
      # actually shown. Always compute it instead.
      outputOptions(output, "parsimony_bar_plot_large", suspendWhenHidden = FALSE)

      parsim_has_results(TRUE)

    }

    #### Parsimony output ####

    output$parsimony_results_header_ui <- renderUI({
      div(style = "margin-bottom:10px;",
        h5("Parsimony results", style = "margin:0 0 6px 0;"),
        HTML("<p style='font-size:13px; color:#555; margin:0;'>
          <b>Volume</b> — proportion of the parameter space consistent with the model; smaller = more precise.
          <b>Max BF</b> — the largest Bayes factor this model could achieve relative to the unconstrained model.
        </p>")
      )
    })

    output$parsimony_plot_ui <- renderUI({
      # A compact overview, not a full-size chart: fixed small per-row
      # height (a few more px per model when several algorithms are
      # grouped side by side within each row) instead of stretching to
      # fill the page, capped so a long model list still scrolls its
      # table rather than the plot growing without bound.
      n_models <- length(model_order)
      n_algos  <- max(1, n_algos_used)
      # Extra fixed room at the top for the legend row (see the matching
      # top margin in output$parsimony_bar_plot) when more than one
      # algorithm is shown.
      legend_h <- if (n_algos > 1) 50 else 0
      plot_h <- min(400, max(140, 55 + legend_h + n_models * (20 + n_algos * 14)))
      tagList(
        div(
          class = "model-card",
          style = "margin-bottom:20px; max-width:560px; padding:14px 18px;",
          div(style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:8px;",
            h5("Volume by model", style = "margin:0; font-size:13px;"),
            # Plain button, not a Shiny actionButton — expanding this is
            # purely a client-side DOM move now (see openParsimonyPlot-
            # Full()/closeParsimonyPlotFull() in the tags$script below),
            # reusing the same #fairy-card-overlay every other card's
            # click-to-expand already uses, instead of a bounded
            # Bootstrap modalDialog with its own margins/max-width that
            # never actually reached full-screen.
            tags$button(
              id = "expand_parsimony_plot_btn", type = "button",
              class = "fairy-plot-expand-btn fairy-tooltip",
              `data-tooltip` = "Expand",
              icon("expand")
            )
          ),
          plotlyOutput("parsimony_bar_plot", height = paste0(plot_h, "px"))
        ),
        # The full-screen version — same data, bigger fonts/margins (see
        # build_parsimony_plot(large=TRUE)) — lives here always, just
        # hidden, so openParsimonyPlotFull() can move the REAL node (not
        # a clone) into the overlay: a cloned live Plotly widget doesn't
        # actually resize/respond, since Plotly's own internal state is
        # tied to the specific DOM node it was drawn into.
        div(
          id = "parsimony_plot_large_home", style = "display:none;",
          div(class = "model-card-title", style = "margin-bottom:12px;", "Volume by model"),
          plotlyOutput("parsimony_bar_plot_large", height = "calc(100vh - 160px)")
        )
      )
    })

    output$parsimony_spinner_table <- renderUI({
      # This tab's whole purpose is this per-algorithm results table, so it
      # is always shown directly — no collapsed <details> to click through.
      div(style = "width:100%;",
        h5("Detailed per-algorithm results", style = "margin:0 0 8px 0;"),
        DT::dataTableOutput("parsim_table", width = "100%")
      )
    })

  })

  #####* Download H-representation QTEST ####

  # QTest's H-representation file format is a pure inequality system
  # (A*p <= b) -- it has no notion of "=" at all. A model with
  # equalities used to just be refused a QTest file outright ("Use
  # V-representation instead"). This eliminates each equality by
  # Gaussian substitution -- solving it for one parameter in terms of
  # the others, substituting that into every remaining row (other
  # equalities and all inequalities), and dropping that parameter's
  # column -- so what's left is a smaller pure-inequality system QTest
  # can actually read. One parameter is eliminated per INDEPENDENT
  # equality; a redundant one (already implied, reduces to 0=0 once
  # prior eliminations are applied) is silently dropped, and a genuinely
  # contradictory one (0=nonzero) marks the model infeasible instead of
  # emitting a bogus file.
  #
  # Returns a list:
  #   ineq          - the reduced inequality-only matrix (numeric,
  #                    [type, b, coefs...] rcdd-style rows, type always 0)
  #   remaining_idx - original column index (1-based, into the model's
  #                    own p1..pn) each column of `ineq` corresponds to
  #   eliminated    - named list (key = original index, as a string) of
  #                    each eliminated parameter's formula, fully
  #                    resolved in terms of the FINAL surviving
  #                    parameters only (list(const=, idx=, coefs=))
  #   infeasible    - TRUE if the equalities are contradictory
  eliminate_h_equalities <- function(h_mat) {
    h_num <- q2d(h_mat)
    n_p   <- ncol(h_num) - 2L
    is_eq <- h_num[, 1] == 1

    eq_pool   <- h_num[is_eq, , drop = FALSE]
    ineq_rows <- h_num[!is_eq, , drop = FALSE]

    remaining_idx <- seq_len(n_p)
    elim_raw <- list()
    elim_order <- character()
    infeasible <- FALSE

    # Drops column `pivot_pos` (a position WITHIN the current, possibly
    # already-shrunk, column set) from every row of M, after first
    # subtracting whatever multiple of `eq_row` zeroes that column out.
    # Column 1 (the row's own type flag, 0=inequality/1=equality) is
    # deliberately left OUT of that subtraction and copied straight from
    # M itself — it isn't part of the linear system at all, just a tag,
    # and subtracting eq_row's own type value (always 1) through it
    # corrupted every substituted inequality row into looking like an
    # equality (or worse) instead of staying 0. Confirmed directly: a
    # hand-built 1-equality/3-inequality test case came back with type
    # values of -1 and 1 on rows that were, and needed to stay, plain
    # inequalities.
    reduce_matrix <- function(M, pivot_pos, a_piv, eq_row) {
      if (nrow(M) == 0) return(M[, -(2L + pivot_pos), drop = FALSE])
      out <- matrix(0, nrow = nrow(M), ncol = ncol(M) - 1L)
      for (r in seq_len(nrow(M))) {
        k <- M[r, 2L + pivot_pos]
        new_row <- if (abs(k) < 1e-12) M[r, ] else M[r, ] - (k / a_piv) * eq_row
        new_row[1] <- M[r, 1]
        out[r, ] <- new_row[-(2L + pivot_pos)]
      }
      out
    }

    while (nrow(eq_pool) > 0 && length(remaining_idx) > 0) {
      row <- eq_pool[1, ]
      eq_pool <- eq_pool[-1, , drop = FALSE]

      coefs <- row[3L:(2L + length(remaining_idx))]
      pivot_pos <- which(abs(coefs) > 1e-9)[1]
      if (is.na(pivot_pos)) {
        # Nothing left standing that this equality still constrains --
        # either it's implied by eliminations already applied (0 = 0,
        # drop it) or it contradicts them (0 = nonzero, infeasible).
        if (abs(row[2]) > 1e-9) { infeasible <- TRUE; break }
        next
      }

      pivot_orig <- remaining_idx[pivot_pos]
      a_piv <- coefs[pivot_pos]
      other_pos <- setdiff(seq_along(remaining_idx), pivot_pos)

      elim_raw[[as.character(pivot_orig)]] <- list(
        const = -row[2] / a_piv,
        idx   = remaining_idx[other_pos],
        coefs = if (length(other_pos) > 0) -coefs[other_pos] / a_piv else numeric(0)
      )
      elim_order <- c(elim_order, as.character(pivot_orig))

      eq_pool   <- reduce_matrix(eq_pool,   pivot_pos, a_piv, row)
      ineq_rows <- reduce_matrix(ineq_rows, pivot_pos, a_piv, row)
      remaining_idx <- remaining_idx[-pivot_pos]
    }

    # Each formula above is only in terms of whatever was still standing
    # AT THE TIME its own equality was processed -- which can include a
    # parameter eliminated LATER (never one eliminated earlier, since
    # that one had already been dropped from the pool). Resolving fully
    # down to just the FINAL survivors is therefore one clean backward
    # pass over elim_order, last-processed first: the last one is
    # already survivor-only, and substituting each already-resolved
    # formula into the ones before it (in reverse) can never re-
    # introduce a dependency this pass hasn't reached yet.
    resolved <- list()
    for (ki in rev(seq_along(elim_order))) {
      key <- elim_order[ki]
      e <- elim_raw[[key]]
      const <- e$const
      new_idx <- numeric(0); new_coefs <- numeric(0)
      for (ii in seq_along(e$idx)) {
        dep_key <- as.character(e$idx[ii])
        if (dep_key %in% names(resolved)) {
          dep <- resolved[[dep_key]]
          const <- const + e$coefs[ii] * dep$const
          new_idx   <- c(new_idx,   dep$idx)
          new_coefs <- c(new_coefs, e$coefs[ii] * dep$coefs)
        } else {
          new_idx   <- c(new_idx,   e$idx[ii])
          new_coefs <- c(new_coefs, e$coefs[ii])
        }
      }
      if (length(new_idx) > 0) {
        u <- unique(new_idx)
        agg <- vapply(u, function(ui) sum(new_coefs[new_idx == ui]), numeric(1))
        new_idx <- u; new_coefs <- agg
      }
      resolved[[key]] <- list(const = const, idx = new_idx, coefs = new_coefs)
    }

    list(
      ineq = ineq_rows, remaining_idx = remaining_idx,
      eliminated = resolved, infeasible = infeasible
    )
  }

  # Shared by both the plain (no-equalities) and equality-eliminated
  # paths below -- same fraction/common-denominator formatting either
  # way, just fed a different (already numeric, already reduced-to-the-
  # right-columns) inequality matrix, so there's exactly one place that
  # knows the actual QTest text layout.
  format_qtest_ineq_file <- function(ineq_mat, n_p) {
    act_v_left  <- -ineq_mat[, 3L:(2L + n_p), drop = FALSE]
    act_v_right <- ineq_mat[, 2]

    act_v_left  <- fractions(act_v_left)
    act_v_right <- fractions(act_v_right)

    nom_left    <- str_extract_part(act_v_left,  "/", before = TRUE)
    denom_left  <- str_extract_part(act_v_left,  "/", before = FALSE)
    nom_right   <- str_extract_part(act_v_right, "/", before = TRUE)
    denom_right <- str_extract_part(act_v_right, "/", before = FALSE)

    pos_ratios_left  <- str_detect(act_v_left,  "/")
    pos_ratios_right <- str_detect(act_v_right, "/")

    nom_left[!pos_ratios_left]   <- c(act_v_left)[!pos_ratios_left]
    denom_left  <- ifelse(is.na(denom_left),  "1", denom_left)
    nom_right[!pos_ratios_right] <- c(act_v_right)[!pos_ratios_right]
    denom_right <- ifelse(is.na(denom_right), "1", denom_right)

    nom_left    <- as.numeric(nom_left);   denom_left  <- as.numeric(denom_left)
    nom_right   <- as.numeric(nom_right);  denom_right <- as.numeric(denom_right)

    prod_denom_all <- prod(unique(c(denom_left[denom_left != 0], denom_right[denom_right != 0])))

    act_v_left  <- format(act_v_left  * prod_denom_all, scientific = FALSE)
    act_v_right <- format(act_v_right * prod_denom_all, scientific = FALSE)

    header_v <- c(nrow(ineq_mat), n_p)
    paste(
      paste(as.character(header_v), collapse = " "), "\n\n",
      paste(apply(act_v_left, 1, paste, collapse = " "), collapse = "\n"), "\n\n",
      paste(act_v_right, collapse = "\n"),
      sep = ""
    )
  }

  # Human-readable companion note for a QTest file that went through
  # eliminate_h_equalities -- QTest's own format has no room for
  # comments, so this ships as a SEPARATE file in the same tar rather
  # than inline, explaining which reduced column is which original
  # parameter and how each eliminated one is recovered from the
  # survivors (e.g. to reconstruct a full result vector afterward).
  format_qtest_substitution_note <- function(elim, orig_names) {
    lines <- c(
      "This model included equalities. QTest's H-representation format",
      "has no way to express \"=\", so one parameter was eliminated per",
      "independent equality (solved for and substituted out) instead --",
      "the accompanying .txt file is the resulting pure-inequality",
      "system in the parameters listed below as \"kept\".",
      ""
    )
    kept_names <- orig_names[elim$remaining_idx]
    lines <- c(lines, "Kept parameters (in the order they appear in the QTest file):")
    lines <- c(lines, paste0("  ", seq_along(kept_names), ": ", kept_names))
    lines <- c(lines, "")
    if (length(elim$eliminated) > 0) {
      lines <- c(lines, "Eliminated parameters (recover after solving the reduced system):")
      for (key in names(elim$eliminated)) {
        e <- elim$eliminated[[key]]
        orig_idx <- as.integer(key)
        terms <- character(0)
        ordered <- order(match(e$idx, elim$remaining_idx))
        for (ii in ordered) {
          c_i <- e$coefs[ii]
          if (abs(c_i) < 1e-9) next
          nm <- orig_names[e$idx[ii]]
          sign <- if (c_i >= 0) "+" else "-"
          coef_txt <- if (abs(abs(c_i) - 1) < 1e-9) "" else paste0(format(round(abs(c_i), 6)), "*")
          terms <- c(terms, paste(sign, paste0(coef_txt, nm)))
        }
        const_txt <- format(round(e$const, 6))
        formula <- paste(c(const_txt, terms), collapse = " ")
        lines <- c(lines, paste0("  ", orig_names[orig_idx], " = ", formula))
      }
    }
    paste(lines, collapse = "\n")
  }

  output$d_h <- downloadHandler(
    filename = function() {
      paste("h_representation_",
        str_replace_all(Sys.Date(), "-", "_"),
        ".tar",
        sep = ""
      )
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL

      export_names <- c(
        isolate(names_models_reactive$value),
        isolate(intersections_reactive$names),
        isolate(mixtures_reactive$names),
        isolate(items_reactive$names)
      )

      for (loop_v in export_names) {
        h_mat <- isolate(h_reactive[[loop_v]])
        if (is.null(h_mat)) next

        h_num      <- q2d(h_mat)
        act_v_ineq <- h_mat[h_num[, 1] == 0, , drop = FALSE]
        act_v_eq   <- h_mat[h_num[, 1] == 1, , drop = FALSE]
        orig_names <- colnames(h_mat)[3:ncol(h_mat)]

        safe_name          <- str_replace_all(loop_v, "[^A-Za-z0-9_.-]", "_")
        file_name_addendum <- ""
        note_file          <- NULL

        if (nrow(act_v_eq) > 0) {
          elim <- eliminate_h_equalities(h_mat)
          if (elim$infeasible) {
            total_file         <- "This model's equalities are contradictory (no assignment of probabilities satisfies all of them) -- no QTest file is possible."
            file_name_addendum <- "_no_QTest_file_available"
          } else if (nrow(elim$ineq) == 0) {
            next
          } else {
            total_file <- format_qtest_ineq_file(elim$ineq, length(elim$remaining_idx))
            note_file  <- format_qtest_substitution_note(elim, orig_names)
          }
        } else if (nrow(act_v_ineq) == 0) {
          next
        } else {
          n_p        <- ncol(h_mat) - 2L
          total_file <- format_qtest_ineq_file(q2d(act_v_ineq), n_p)
        }

        fileName <- paste0(safe_name, file_name_addendum, ".txt")
        write.table(total_file, fileName, quote = FALSE, col.names = FALSE, row.names = FALSE)
        files <- c(fileName, files)

        if (!is.null(note_file)) {
          noteFileName <- paste0(safe_name, "_substitutions.txt")
          write.table(note_file, noteFileName, quote = FALSE, col.names = FALSE, row.names = FALSE)
          files <- c(noteFileName, files)
        }
      }

      tar(file, files)
    }
  )


  #####* Download H-representation multinomineq ####

  output$d_h_multinomineq <- downloadHandler(
    filename = function() {
      paste("h_representation_",
        str_replace_all(Sys.Date(), "-", "_"),
        ".tar",
        sep = ""
      )
    },
    content = function(file) {
      
      shinyalert(
        text = "<a href='https://www.modeling-for-everyone.space/analysis_multinomineq.R' target='_blank'>Download</a> R-file for reading the H- or V-representation using <a href='https://www.dwheck.de/software/multinomineq/' target='_blank'>multinomineq</a>.",
        html = TRUE
      )
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL

      all_h_rep_in_matrix <- numeric()

      # models, intersections, and mixtures
      export_names <- c(
        isolate(names_models_reactive$value),
        isolate(intersections_reactive$names),
        isolate(mixtures_reactive$names),
        isolate(items_reactive$names)
      )
      # "m1 AND m2" is not filename-safe -> "m1_AND_m2"
      export_files <- str_replace_all(export_names, fixed(" AND "), "_AND_")
      export_files <- str_replace_all(export_files, "[^A-Za-z0-9_.-]", "_")
      numb_models <- length(export_names)

      for (loop_qtest_h in seq_len(numb_models)) {
        A <- isolate(h_reactive[[export_names[loop_qtest_h]]])
        # A's own type column (col 1: 0=inequality, 1=equality) is
        # NEVER included in the exported A/b below -- so an equality
        # row (b + a.p = 0), exported exactly like every inequality
        # row, only ever encoded ONE of the two directions it actually
        # means (a.p >= -b), silently dropping the other (a.p <= -b).
        # The exported polytope came out too large -- not an error, no
        # warning, just quietly under-constrained. Append a second,
        # sign-flipped copy of every equality row BEFORE the existing
        # pipeline below runs, so both directions survive as two
        # ordinary inequality rows -- no other change needed here,
        # since from this point on every row is already treated as a
        # plain "A.p <= b" row regardless of where it came from.
        h_num_mn <- q2d(A)
        is_eq_mn <- h_num_mn[, 1] == 1
        if (any(is_eq_mn)) {
          eq_rows_mn <- A[is_eq_mn, , drop = FALSE]
          flipped_mn <- eq_rows_mn
          flipped_mn[, 2:ncol(flipped_mn)] <-
            d2q(-1 * q2d(eq_rows_mn[, 2:ncol(eq_rows_mn), drop = FALSE]))
          A <- rbind(A, flipped_mn)
        }
        b <- A[, 2]
        A <- A[, 3:ncol(A)]
        A <- d2q(-1 * q2d(A))
        names_A <- colnames(A)
        colnames(A) <- names_A

        fileName <-
          paste("A_", export_files[loop_qtest_h], ".csv", sep = "")

        write.table(
          A,
          fileName,
          quote = FALSE,
          col.names = TRUE,
          row.names = FALSE,
          sep = ","
        )

        files <- c(fileName, files)

        fileName <-
          paste("b_", export_files[loop_qtest_h], ".csv", sep = "")

        write.table(
          b,
          fileName,
          quote = FALSE,
          col.names = FALSE,
          row.names = FALSE,
          sep = ","
        )


        files <- c(fileName, files)
      }

      # zip::zip(file, files)
      tar(file, files)
    }
  )

  #####* V-representation QTEST  ####

  output$d_v <- downloadHandler(
    filename = function() {
      paste("v_representation_",
        str_replace_all(Sys.Date(), "-", "_"),
        ".tar",
        sep = ""
      )
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL

      download_v_names <- names(v_reactive)

      download_v_names <- download_v_names[download_v_names %in% names_models_reactive$value]
      download_v_names <- names_models_reactive$value[names_models_reactive$value %in% download_v_names]

      for (loop_v_qtest in seq_len(length(download_v_names))) {
        vrep_entry <- v_reactive[[download_v_names[loop_v_qtest]]]
        csv_file <- if (is.list(vrep_entry) && !is.null(vrep_entry$output))
          data.frame(vrep_entry$output) else data.frame(vrep_entry)

        if (is.null(csv_file) == F) {
          csv_file_dec <- csv_file
          csv_file_dec <- unlist(csv_file_dec)

          for (loop_eval in seq_len(length(csv_file_dec))) {
            right_dec <-
              eval(parse(text = csv_file_dec[loop_eval]))

            numb_dec <- str_count(gsub(
              ".*[.]", "",
              format(right_dec, scientific = F)
            ), "0")

            csv_file_dec[loop_eval] <- format(round(right_dec, digits = 6), scientific = F)
          }

          csv_file <- matrix(csv_file_dec, ncol = ncol(csv_file))

          ####

          csv_file <- csv_file[, 3:ncol(csv_file)]
          csv_file <- t(csv_file)

          header_v <- paste("V", 1:ncol(csv_file), sep = "")
          line_2 <- rep(1, ncol(csv_file))

          total_file <- paste(
            paste((header_v), collapse = ","),
            "\n",
            paste((line_2), collapse = ","),
            "\n",
            "\n",
            paste(apply((csv_file), 1, paste, collapse = ","), collapse = "\n"),
            sep = ""
          )

          fileName <-
            paste0(str_replace_all(download_v_names[loop_v_qtest], "[^A-Za-z0-9_.-]", "_"), ".csv")

          write.table(
            total_file,
            fileName,
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE
          )

          files <- c(fileName, files)
        }
      }

      tar(file, files)
    }
  )


  ####* V-representation multinomineq

  output$d_v_multinomineq <- downloadHandler(
    filename = function() {
      paste("v_representation_",
        str_replace_all(Sys.Date(), "-", "_"),
        ".tar",
        sep = ""
      )
    },
    content = function(file) {
      
      shinyalert(
        text = "<a href='https://www.modeling-for-everyone.space/analysis_multinomineq.R' target='_blank'>Download</a> R-file for reading the H- or V-representation using <a href='https://www.dwheck.de/software/multinomineq/' target='_blank'>multinomineq</a>.",
        html = TRUE
      )
  
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL

      download_v_names <- names(v_reactive)

      download_v_names <- download_v_names[download_v_names %in% names_models_reactive$value]
      download_v_names <- names_models_reactive$value[names_models_reactive$value %in% download_v_names]

      for (loop_v_qtest in seq_len(length(download_v_names))) {
        vrep_entry    <- v_reactive[[download_v_names[loop_v_qtest]]]
        current_model <- if (is.list(vrep_entry) && !is.null(vrep_entry$output))
          vrep_entry$output else vrep_entry
        current_model <- current_model[, 3:ncol(current_model)]

        # Per-model parameter names, not the shared probs_reactive$value —
        # that reactive holds whichever single model was last processed
        # elsewhere in the app, not the model currently being exported here,
        # so every model after the first got the wrong (or mismatched-length)
        # column headers. A V-representation's columns 3+ correspond 1:1 with
        # its own H-representation's columns 3+, so pull the names from there.
        h_this <- isolate(h_reactive[[download_v_names[loop_v_qtest]]])
        colnames(current_model) <- if (!is.null(h_this))
          colnames(h_this)[-(1:2)] else paste0("V", seq_len(ncol(current_model)))

        fileName <-
          paste0("V_", str_replace_all(download_v_names[loop_v_qtest], "[^A-Za-z0-9_.-]", "_"), ".csv")

        write.table(
          current_model,
          fileName,
          sep = ",",
          quote = FALSE,
          col.names = TRUE,
          row.names = FALSE
        )

        files <- c(fileName, files)
      }

      tar(file, files)
    }
  )

  ####* LaTeX-file of V-representation (words or fractions, matching the
  # "Show vertices in words" toggle) ####
  # One APA-style table per currently-displayed model (base models,
  # intersections, mixtures, replications, items), vertices as rows,
  # parameters as columns. Mirrors whatever the on-screen V-representation
  # table is showing AT DOWNLOAD TIME (input$v_rep_words, the same
  # checkbox the table itself reads live — see its own renderDataTable
  # comment) rather than always forcing words: 0 -> "none", 1 -> "all",
  # anything else left as a fraction (frac_str, the same validated
  # formatter the H-representation table already uses) when the checkbox
  # is on; plain numeric fractions throughout when it's off.

  output$d_v_latex <- downloadHandler(
    filename = function() {
      words_on <- isTRUE(isolate(input$v_rep_words))
      paste0("v_representation_", if (words_on) "words_" else "numbers_",
        str_replace_all(Sys.Date(), "-", "_"), ".tex")
    },
    content = function(file) {
      words_on <- isTRUE(isolate(input$v_rep_words))
      export_names <- c(
        isolate(names_models_reactive$value),
        isolate(intersections_reactive$names),
        isolate(mixtures_reactive$names),
        isolate(items_reactive$names)
      )

      header_latex <- "\\documentclass{article}
\\usepackage{amsmath}
\\usepackage{booktabs}
\\usepackage{caption}
\\captionsetup{labelfont=bf, labelsep=newline, justification=raggedright, singlelinecheck=false}
\\begin{document}
"
      footer_latex <- "\\end{document}
"

      table_blocks <- character()
      table_no <- 0L

      for (nm in export_names) {
        vrep_entry <- isolate(v_reactive[[nm]])
        if (is.null(vrep_entry)) next
        v_mat <- if (is.list(vrep_entry) && !is.null(vrep_entry$output)) vrep_entry$output else vrep_entry
        if (is.null(v_mat) || nrow(v_mat) == 0) next

        v_num <- q2d(v_mat[, 3:ncol(v_mat), drop = FALSE])

        h_this  <- isolate(h_reactive[[nm]])
        p_names <- if (!is.null(h_this) && ncol(h_this) > 2)
          colnames(h_this)[-(1:2)] else paste0("p_{", seq_len(ncol(v_num)), "}")

        cell_str <- matrix(vapply(v_num, function(x) {
          s <- frac_str(x)
          if (words_on) {
            if (s == "0") "none" else if (s == "1") "all" else s
          } else {
            s
          }
        }, character(1)), nrow = nrow(v_num), ncol = ncol(v_num))

        table_no <- table_no + 1L
        col_spec <- paste0("l", strrep("c", ncol(cell_str)))
        header_row <- paste(c("", paste0("$", tex_name(p_names), "$")), collapse = " & ")
        body_rows <- vapply(seq_len(nrow(cell_str)), function(r) {
          paste(c(paste0("$V_{", r, "}$"), cell_str[r, ]), collapse = " & ")
        }, character(1))

        table_blocks <- c(table_blocks, paste0(
          "\\begin{table}[h]\n\\centering\n",
          "\\caption{Table ", table_no, "}\n",
          "\\textit{V-representation of ", gsub("_", "\\\\_", nm), "}\\\\[4pt]\n",
          "\\begin{tabular}{", col_spec, "}\n\\toprule\n",
          header_row, " \\\\\n\\midrule\n",
          paste(body_rows, collapse = " \\\\\n"), " \\\\\n",
          "\\bottomrule\n\\end{tabular}\n\n",
          if (words_on) {
            paste0("\\vspace{2pt}\n{\\small \\textit{Note.} A vertex coordinate of ``all'' denotes probability 1 ",
              "(every member of the relevant subpopulation), ``none'' denotes probability 0; fractional ",
              "coordinates are left as fractions.}\n")
          } else {
            "\\vspace{2pt}\n{\\small \\textit{Note.} Vertex coordinates are left as plain fractions.}\n"
          },
          "\\end{table}\n\n"
        ))
      }

      body_latex <- if (length(table_blocks) == 0)
        "No V-representations have been computed yet.\n" else paste(table_blocks, collapse = "")

      write.table(
        paste0(header_latex, body_latex, footer_latex),
        file,
        quote = FALSE,
        col.names = FALSE,
        row.names = FALSE
      )
    }
  )

  ####* LaTeX-file of H-representation ####

  output$d_latex <- downloadHandler(
    filename = function() {
      paste("models_",
        str_replace_all(Sys.Date(), "-", "_"),
        ".tex",
        sep = ""
      )
    },
    content = function(file) {
      header_latex <- (
        "\\documentclass{article}
\\usepackage{amsmath}
\\begin{document}
\\section*{Models}
"
      )

      footer_latex <- ("
\\end{document}
")

      # equation_all_total_reactive$value holds the on-screen cards' HTML/
      # MathJax markup (<table>, <span style=\"color:...\">, \\(...\\)) —
      # meaningful in a browser, not valid LaTeX on its own, which is why
      # this download used to produce a .tex file that wouldn't compile.
      # equation_all_total_latex_reactive$value holds a parallel, plain-
      # LaTeX align* block per model built alongside it for exactly this
      # export (see its own comment) — use that instead.
      equation_all_total_latex <- unlist(equation_all_total_latex_reactive$value)
      body_latex <- if (length(equation_all_total_latex) == 0)
        "No models have been computed yet.\n" else paste(equation_all_total_latex, collapse = "\n")

      formula_h_all_download <- paste0(header_latex, body_latex, footer_latex)

      write.table(
        formula_h_all_download,
        file,
        quote = FALSE,
        col.names = FALSE,
        row.names = FALSE
      )
    }
  )

  ##### Settings for Approximate Equalities ####

  # The "≈" button next to a model's Model Specification box (only shown
  # once that model's spec contains an "=" — see the conditionalPanel in
  # textboxes_relations_complete()) opens a dialog on click, listing every
  # equality clause currently in that model's spec with its own
  # approximate/exact choice and tolerance — not one shared value for the
  # whole model, and not interrupting typing with an automatic popup.
  # Re-wired whenever the model count changes, same pattern as the
  # join_specs observer earlier; each per-model observer then keeps
  # listening on its own eq_tol_btn_i going forward regardless of what
  # triggered the re-wiring. Only NEW indices are wired each time (see
  # wired_eq_tol_ids's own comment) — re-registering one for a model that
  # already has it would just leave two identical observers responding to
  # the same click, harmless here since the handler just re-derives and
  # re-shows the same modal either way, but wasteful and not a pattern to
  # copy for anything that isn't idempotent (see the V-representation
  # toggle below, which is not).
  observeEvent(counter_input$n, {
    n <- counter_input$n
    already <- isolate(wired_eq_tol_ids())
    new_ids <- setdiff(seq_len(n), already)
    for (loop_n in new_ids) {
      local({
        ii <- loop_n
        key <- as.character(ii)

        observeEvent(input[[paste0("eq_tol_btn_", ii)]], {
          spec <- isolate(input[[paste0("textin_relations_complete_", ii)]]) %||% ""
          raw_clauses <- trimws(unlist(strsplit(spec, ";")))
          raw_clauses <- raw_clauses[nzchar(raw_clauses)]
          # Chain-expand (see expand_clause_to_rows's own comment) so
          # this dialog lists — and so tol_current below is keyed by —
          # one row per actual equality the Go pipeline ends up with, not
          # one per raw semicolon-clause. A chained "p1=p4=0" is a single
          # typed clause but two real "=" rows; without this, every
          # equality typed after a chain silently got the WRONG saved
          # tolerance (or none at all) applied to it.
          clauses <- unlist(lapply(raw_clauses, expand_clause_to_rows))
          clauses <- clauses[grepl("=", clauses)]
          if (length(clauses) == 0) {
            return()
          }

          equality_pending_reactive$value[[key]] <- clauses

          tol_current <- isolate(equality_tolerances_reactive$value[[key]]) %||% numeric(0)
          model_name <- isolate(AllInputs())[[paste0("textin_relations_name", ii)]] %||% paste0("m", ii)

          eq_rows <- lapply(seq_along(clauses), function(k) {
            clause <- clauses[k]
            existing_tol <- if (k <= length(tol_current) && !is.na(tol_current[k])) tol_current[k] else 0
            tags$div(
              class = "fairy-eq-tol-row",
              tags$code(clause, class = "fairy-eq-tol-clause"),
              tags$div(
                class = "fairy-eq-tol-switch-line",
                materialSwitch(paste0("eq_tol_switch_", ii, "_", k), label = "Approximate", status = "primary", value = existing_tol != 0, right = TRUE)
              ),
              numericInput(paste0("eq_tol_value_", ii, "_", k), label = "Maximum permissible deviation", value = if (existing_tol != 0) existing_tol else 0.05, min = 0, max = 1, step = 0.01, width = "140px")
            )
          })

          showModal(modalDialog(
            title = NULL,
            size = "s",
            class = "fairy-eq-tol-modal",
            tags$div(
              class = "fairy-eq-tol-header",
              tags$strong(paste0("Approximate equalities — ", model_name))
            ),
            tags$p(
              class = "fairy-eq-tol-help",
              "Turn an equality on to treat it as approximate and set its tolerance — e.g. .05 turns p1 = p2 into p1 - p2 ≤ .05 and -p1 + p2 ≤ .05. Leave off to keep it exact."
            ),
            tagList(eq_rows),
            footer = tagList(
              modalButton("Cancel"),
              actionBttn(paste0("submit_eq_tol_", ii), "Save", style = "material-flat", color = "primary", size = "sm")
            ),
            easyClose = TRUE,
            fade = TRUE
          ))
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0("submit_eq_tol_", ii)]], {
          clauses <- isolate(equality_pending_reactive$value[[key]])
          if (is.null(clauses)) {
            return()
          }
          new_tol <- numeric(length(clauses))
          for (k in seq_along(clauses)) {
            on <- isTRUE(input[[paste0("eq_tol_switch_", ii, "_", k)]])
            val <- input[[paste0("eq_tol_value_", ii, "_", k)]]
            new_tol[k] <- if (on && !is.null(val) && !is.na(val)) val else 0
          }
          equality_tolerances_reactive$value[[key]] <- new_tol
          removeModal()
        })

        # V-representation on/off toggle for this model — flips this
        # model's own row in mytable_v_reactive$value, the exact same
        # data.frame the "V-representations" dialog (below) builds from
        # scratch on Submit, so a toggle here and a selection made there
        # stay consistent with each other either way.
        observeEvent(input[[paste0("vrep_toggle_btn_", ii)]], {
          model_name <- isolate(input[[paste0("textin_relations_name", ii)]]) %||% paste0("m", ii)
          vtab <- isolate(mytable_v_reactive$value)
          if (is.null(vtab) || ncol(vtab) < 2) {
            vtab <- data.frame(
              `Model name` = character(0), `Include V-representation` = logical(0),
              check.names = FALSE, stringsAsFactors = FALSE
            )
          }
          row_idx <- which(vtab[[1]] == model_name)
          now_included <- TRUE
          if (length(row_idx) == 0) {
            vtab <- rbind(vtab, setNames(
              data.frame(model_name, TRUE, stringsAsFactors = FALSE), colnames(vtab)
            ))
          } else {
            now_included <- !isTRUE(vtab[row_idx[1], 2])
            vtab[row_idx[1], 2] <- now_included
          }
          mytable_v_reactive$value <- vtab

          n_total_v <- sum(vtab[[2]] == TRUE)
          updateActionButton(session, "show_v_rep",
            label = if (n_total_v > 0) paste0("V-representations (n=", n_total_v, ")") else "V-representations"
          )

          # The "V-representation" tab's table is built once per Go/
          # Parsimony run from a plain snapshot list, not a live reactive
          # read of mytable_v_reactive$value — so toggling this here
          # wouldn't otherwise make its (already-computed) card vanish/
          # reappear until the next full recompute. Hiding or re-showing
          # the matching card client-side instead is a cheap, safe way to
          # reflect the change immediately without touching that pipeline.
          session$sendCustomMessage("fairy_hide_vrep_card",
            list(model = model_name, included = now_included))
        }, ignoreInit = TRUE)
      })
    }
    wired_eq_tol_ids(union(already, new_ids))
  }, ignoreNULL = FALSE)

  # Master V-representation on/off toggle (see its uiOutput placement in
  # the "Model(s)" header toolbar) — mirrors the per-model
  # vrep_toggle_btn_i toggle above, just applied to every current model
  # row at once instead of one. Flips to "exclude all" once every row is
  # already included, and to "include all" otherwise (so a mixed state —
  # some on, some off — always reads as "click to finish including
  # everything" rather than immediately clearing what's already set).
  #
  # This button is a renderUI, re-run (and its DOM node recreated) on
  # every model-count/inclusion change — a CSS "plays automatically on
  # mount" intro animation would therefore replay on EVERY such change,
  # not just when the button actually (re)appears. Track whether it was
  # visible (n > 1) on the PREVIOUS render instead of a one-shot "seen"
  # flag, so the intro plays again each time it goes from hidden back to
  # visible (e.g. deleting down to 1 model, then adding a 2nd again) —
  # not just once ever per session.
  vrep_toggle_all_was_visible <- reactiveVal(FALSE)
  # Whether THIS render pass is the moment the cube button just
  # (re)appeared — computed in the always-running observer below, not
  # inline in the renderUI itself. The renderUI's own output is
  # suspended while the Input tab isn't the active one (standard Shiny
  # behavior for a hidden tab-pane), so switching away and back used to
  # resume it as if from a blank slate, replaying the pulse even though
  # nothing had actually changed while it was hidden — confirmed
  # directly: switch to H-representation and back to Input re-glowed
  # both this button and the trash-all one below with nothing else
  # touched. An observe() block is never suspended by tab visibility, so
  # deciding the transition there and just having the renderUI read the
  # already-settled result fixes it regardless of when the render
  # actually resumes.
  # A plain boolean flag here turned out to have its OWN bug: the
  # observer only fires (and sets this) on a genuine transition, but the
  # renderUI can be READ many times afterward with no new transition at
  # all (every resume from tab-hidden suspension re-reads it) — and
  # nothing was ever putting it back to FALSE between "the observer set
  # it TRUE" and "the very next real transition", so every one of those
  # extra reads saw the same stale TRUE and pulsed again (confirmed
  # directly: switching tabs away and back left the button glowing
  # every single time, not just once). An ever-increasing id instead:
  # the observer stamps each genuine transition with a new id, and the
  # renderUI tracks (via vrep_toggle_all_last_shown_id below) which id
  # IT has already shown a pulse for, so re-reading the same id twice
  # (e.g. across a suspend/resume with nothing new happening) no longer
  # replays anything.
  vrep_toggle_all_appear_id <- reactiveVal(0)
  vrep_toggle_all_last_shown_id <- reactiveVal(0)
  # Same "was it visible on the PREVIOUS render" tracking, for the
  # "Delete all models" trash icon right next to this button — it needs
  # its own tracker rather than sharing this one since it isn't built
  # inside a renderUI of its own (its markup sits directly in the model
  # rows' own render pass — see .fairy-del-all-models-btn's own R
  # comment), so it can't just re-derive "just appeared" locally the
  # way this output does.
  del_all_models_was_visible <- reactiveVal(FALSE)
  # Same suspension problem and same fix as vrep_toggle_all_just_appeared
  # above — the model rows' own render pass is likewise suspended while
  # the Input tab is hidden.
  # Same id-based fix as vrep_toggle_all_appear_id above, same reason.
  del_all_models_appear_id <- reactiveVal(0)
  del_all_models_last_shown_id <- reactiveVal(0)
  # Single always-running source of truth for BOTH buttons' "did I just
  # (re)appear" flags — observe() blocks are never suspended by tab
  # visibility, unlike the renderUI outputs that only ever READ these
  # flags now. Also still resets del_all_models_was_visible back to
  # FALSE the moment n drops to 1, so going back up to 2 later replays
  # the pulse instead of reading as "already seen" (the model rows'
  # render pass, gated on n>1, would otherwise never itself run the code
  # that could reset it).
  # priority: Shiny doesn't otherwise guarantee this observer runs
  # before the outputs/reactives that READ these two flags within the
  # same reactive flush — confirmed directly: without it, the cube
  # button's own output happened to see the freshly-set flag but the
  # trash button's (built inside the model rows' own render pass) read
  # it BEFORE this observer had run, missing its own first-appearance
  # pulse entirely. A higher priority than the default (0) guarantees
  # this settles first.
  observe({
    n <- counter_input$n
    now_visible <- n > 1
    if (now_visible && !isolate(vrep_toggle_all_was_visible())) {
      vrep_toggle_all_appear_id(isolate(vrep_toggle_all_appear_id()) + 1)
    }
    vrep_toggle_all_was_visible(now_visible)
    if (now_visible && !isolate(del_all_models_was_visible())) {
      del_all_models_appear_id(isolate(del_all_models_appear_id()) + 1)
    }
    del_all_models_was_visible(now_visible)
  }, priority = 10)
  output$vrep_toggle_all_ui <- renderUI({
    n <- counter_input$n
    vtab <- mytable_v_reactive$value
    model_names <- if (n > 0) vapply(seq_len(n), function(i)
      input[[paste0("textin_relations_name", i)]] %||% paste0("m", i), character(1)) else character(0)
    all_included <- length(model_names) > 0 && !is.null(vtab) && ncol(vtab) >= 2 &&
      all(model_names %in% vtab[[1]][vtab[[2]] == TRUE])
    # Reads the id the always-running observer above already stamped —
    # see vrep_toggle_all_appear_id's own comment for why this output no
    # longer decides the transition itself, and why a one-shot boolean
    # wasn't enough on its own (this compares against the id THIS
    # output last actually showed a pulse for, consuming it so a later
    # re-read of the same id — e.g. resuming from tab-hidden suspension
    # with nothing new — doesn't replay it).
    cur_id <- vrep_toggle_all_appear_id()
    just_appeared <- cur_id > isolate(vrep_toggle_all_last_shown_id())
    if (just_appeared) vrep_toggle_all_last_shown_id(cur_id)
    btn <- actionButton(
      "vrep_toggle_all_btn", label = NULL, icon = icon("cube"),
      class = paste("btn action-button fairy-vrep-toggle-btn fairy-vrep-toggle-all-btn",
        if (all_included) "active" else ""),
      style = "padding:5px 12px; font-size:14px; border-radius:8px; min-width:0;"
    )
    # See .fairy-intro-pulse-wrap's own comment for why this is a
    # wrapping span rather than a class on the button itself.
    if (just_appeared) tags$span(class = "fairy-intro-pulse-wrap", btn) else btn
  })

  # Small muted status line filling the space next to the master cube/
  # trash buttons — gives context for what those two act on ("N models,
  # M selected for V-representation") instead of leaving it empty. Spelled
  # out in full rather than "V-rep" (reported as cryptic shorthand), and
  # "selected for" rather than "included in" — a model isn't inside the
  # V-representation, it contributes to computing one.
  # Same n/vtab computation as
  # vrep_toggle_all_ui just above, kept as its own output so it updates
  # independently (e.g. a V-rep toggle alone shouldn't have to re-render
  # the whole row markup, just this line).
  output$model_count_summary_ui <- renderUI({
    n <- counter_input$n
    if (n <= 1) return(NULL)
    vtab <- mytable_v_reactive$value
    model_names <- vapply(seq_len(n), function(i)
      input[[paste0("textin_relations_name", i)]] %||% paste0("m", i), character(1))
    # A mixture row's own cube button (see its "Always included for
    # mixtures" branch just above) is forced on regardless of vtab — it
    # never actually needs to appear there, since there's nothing to
    # toggle. Mirror that same forced-on rule here per row, rather than
    # relying on vtab alone, so this count doesn't undercount mixtures
    # vtab hasn't caught up with yet (or never will).
    row_included <- vapply(seq_len(n), function(i) {
      is_forced_mixture <- !is.null(mixture_model_flags$value[[as.character(i)]]) &&
        is.null(item_model_spec_idx$value[[as.character(i)]])
      if (is_forced_mixture) return(TRUE)
      !is.null(vtab) && ncol(vtab) >= 2 &&
        model_names[i] %in% vtab[[1]][vtab[[2]] == TRUE]
    }, logical(1))
    included_count <- sum(row_included)
    tags$span(
      style = "font-size:12px; color:var(--fairy-text-muted); white-space:nowrap;",
      paste0(n, " model", if (n != 1) "s" else "", " · ",
        included_count, " selected for V-representation")
    )
  })

  observeEvent(input$vrep_toggle_all_btn, {
    n <- isolate(counter_input$n)
    if (n < 1) return()
    model_names <- vapply(seq_len(n), function(i)
      isolate(input[[paste0("textin_relations_name", i)]]) %||% paste0("m", i), character(1))

    vtab <- isolate(mytable_v_reactive$value)
    if (is.null(vtab) || ncol(vtab) < 2) {
      vtab <- data.frame(
        `Model name` = character(0), `Include V-representation` = logical(0),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
    all_included <- all(model_names %in% vtab[[1]][vtab[[2]] == TRUE])
    target <- !all_included   # currently-all-in -> turn everything off; anything else -> turn everything on

    for (model_name in model_names) {
      row_idx <- which(vtab[[1]] == model_name)
      if (length(row_idx) == 0) {
        vtab <- rbind(vtab, setNames(
          data.frame(model_name, target, stringsAsFactors = FALSE), colnames(vtab)
        ))
      } else {
        vtab[row_idx[1], 2] <- target
      }
    }
    mytable_v_reactive$value <- vtab

    n_total_v <- sum(vtab[[2]] == TRUE)
    updateActionButton(session, "show_v_rep",
      label = if (n_total_v > 0) paste0("V-representations (n=", n_total_v, ")") else "V-representations"
    )

    # Same client-side card show/hide as the per-model toggle — with no
    # `model` field the JS handler applies `included` to every card at
    # once instead of matching one by name (see its own comment).
    session$sendCustomMessage("fairy_hide_vrep_card", list(included = target))
  }, ignoreInit = TRUE)

  # Keeps an intersection model's Unique Model Specification (and its
  # per-equality approximate tolerances) live-synced to its source
  # models — recomputed whenever ANY source's own spec or tolerances
  # change, not just captured once at creation. Only wires NEW indices
  # each time (same reasoning as wired_eq_tol_ids above) since this is an
  # `observe()`, not an `observeEvent()` tied to one specific action —
  # duplicates here would be harmless (each just re-pushes the same
  # freshly-recomputed value) but wasteful, and this avoids that instead
  # of relying on it being harmless.
  observeEvent(counter_input$n, {
    n <- counter_input$n
    already <- isolate(wired_derived_ids())
    new_ids <- setdiff(seq_len(n), already)
    for (loop_n in new_ids) {
      local({
        ii <- loop_n
        observe({
          info <- isolate(derived_model_sources$value[[as.character(ii)]])
          if (is.null(info)) {
            return()
          }
          src_idx <- info$source_idx

          if (info$type == "mixture") {
            # A mixture has no live spec to recompute (see
            # mixture_model_flags's own comment) — just keeps its
            # "Mixture of ..." description in sync with its sources'
            # CURRENT names, so renaming a source is reflected here too.
            # The placeholder text itself is only ever baked in at render
            # time (textboxes_relations() is isolate()d against everything
            # except counter_input$n), so updating mixture_model_flags
            # alone wouldn't reach the already-rendered box — a direct
            # client message (handled near the top of the UI) sets the
            # live DOM placeholder attribute instead.
            src_names <- vapply(src_idx, function(s) input[[paste0("textin_relations_name", s)]] %||% paste0("m", s), character(1))
            new_label <- paste0("Mixture of ", paste(src_names, collapse = ", "))
            isolate({
              # Same self-invalidation risk/guard as the tolerance write
              # below — mixture_model_flags$value is one shared field.
              if (!identical(mixture_model_flags$value[[as.character(ii)]], new_label)) {
                mixture_model_flags$value[[as.character(ii)]] <- new_label
                session$sendCustomMessage("fairy_set_placeholder", list(
                  id = paste0("textin_relations_", ii), placeholder = new_label
                ))
              }
            })
            return()
          }
          if (info$type != "intersection") {
            return()
          }
          # A "text-only" intersection (one of its sources is a mixture,
          # or is itself a text-only intersection — see the
          # involves_mixture check in observeEvent(input$
          # submit_intersection_model, ...)) has nothing real to
          # recompute: its Unique Model Specification box is a
          # placeholder, not live constraint text, and must stay that way
          # rather than getting overwritten with a partial/misleading
          # concatenation of just its non-mixture sources.
          is_text_only <- isolate(!is.null(mixture_model_flags$value[[as.character(ii)]]))
          if (is_text_only) {
            return()
          }
          specs <- vapply(src_idx, function(s) input[[paste0("textin_relations_", s)]] %||% "", character(1))
          # Also depend on equality_tolerances_reactive$value (the whole
          # field, not a specific key — reactiveValues invalidates per
          # field, not per list entry) so a source's tolerance edit
          # re-triggers this too, not just a spec-text edit.
          equality_tolerances_reactive$value

          combined <- paste(specs[nzchar(trimws(specs))], collapse = "; ")
          updateTextAreaInput(session, inputId = paste0("textin_relations_", ii), value = combined)

          combined_tol <- unlist(lapply(src_idx, unique_model_tol_slice))
          isolate({
            # Guarded: writing to equality_tolerances_reactive$value
            # invalidates EVERY reader of that field (reactiveValues has
            # no per-list-key granularity), including this very observer
            # (it reads the same field above to know when a source's
            # tolerance changed) — an unconditional write would
            # invalidate itself every flush forever, recomputing and
            # rewriting the same value in an infinite loop. Only actually
            # writing when the value truly changed breaks that cycle.
            old_tol <- equality_tolerances_reactive$value[[as.character(ii)]]
            if (!identical(old_tol, combined_tol)) {
              equality_tolerances_reactive$value[[as.character(ii)]] <- combined_tol
            }
          })
        })
      })
    }
    wired_derived_ids(union(already, new_ids))
  }, ignoreNULL = FALSE)

  ##### Settings for V-representation ####

  # The "V-representations" sidebar button/dialog (bulk picker covering
  # base models + intersections + replications + multi-item models) was
  # removed in favor of the per-model toggle next to each model's name
  # (see fairy-vrep-toggle-btn) — that only covers base models, so
  # intersections/replications/multi-item models can no longer have their
  # V-representation toggled independently; accepted tradeoff, not an
  # oversight.

  # The "Model intersections"/"Model mixtures" sidebar buttons and their
  # dialogs (bulk pickers building mytable_int_reactive/mytable_mix_
  # reactive) were removed the same way — replaced by the "Intersection
  # model"/"Mixture model" buttons on the Input tab, which add a real row
  # to the model list directly (see derived_model_sources). The
  # computation code that reads mytable_int_reactive further up in the Go
  # pipeline is untouched and still runs — it just never gets populated
  # by anything anymore, so it silently no-ops; mytable_mix_reactive's
  # equivalent was replaced outright (see the "Model mixtures" section
  # earlier, now driven by derived_model_sources). Both reactiveValues
  # are kept (not deleted) since old save-file uploads still restore into
  # them (see observeEvent(input$upload, ...)).


  ##### Settings for Volume #####

  observeEvent(input$show, {
    input_volume <- isolate(input_volume_reactive)

    # These models are always H-polytopes (built from the typed
    # inequalities), never V-polytopes/zonotopes — so volesti's own
    # per-algorithm defaults (see ?volesti::volume) resolve to one fixed
    # concrete value/method per field here, not "it depends". Showing
    # that resolved value instead of the word "default" up front means
    # the field is only ever still literally showing "default" if it's
    # a field volesti computes case-by-case (win_len, walk_length for
    # SoB) — those are filled in using the CURRENT probability count
    # (d), same formula volesti itself uses, so they stay accurate as
    # the model changes. A field the user has already edited away from
    # "default" keeps their value, not the recomputed one.
    d <- isolate(counter$n)
    resolved_defaults <- c(
      error_cb = "0.1", random_walk_cb = "Coordinate Directions Hit-and-Run",
      walk_length_cb = "1", win_len_cb = as.character(400 + 3 * d^2),
      error_sob = "1", random_walk_sob = "Coordinate Directions Hit-and-Run",
      walk_length_sob = as.character(floor(10 + d / 10)),
      error_cg = "0.1", random_walk_cg = "Coordinate Directions Hit-and-Run",
      walk_length_cg = "1", win_len_cg = as.character(500 + 4 * d^2)
    )
    resolved <- function(idx, key) if (identical(input_volume$value[idx], "default")) resolved_defaults[[key]] else input_volume$value[idx]

    algo_field <- function(label, input_tag) {
      div(style = "margin-bottom:12px;",
        tags$label(label, style = "font-size:12px; color:#555; margin-bottom:3px; display:block;"),
        input_tag
      )
    }
    showModal(modalDialog(
      title = "Algorithm settings",
      tabsetPanel(
        tabPanel("Cooling Bodies",
          div(style = "padding:16px 4px 0 4px;",
            algo_field("Approximation error (upper bound)",
              textInput("error_cb", NULL, value = resolved(1, "error_cb"), width = "100%")),
            algo_field("Random walk method",
              pickerInput("random_walk_cb", NULL, width = "100%",
                choices = c("default", "Coordinate Directions Hit-and-Run", "Random Directions Hit-and-Run", "Ball Walk", "Billiard Walk"),
                selected = resolved(2, "random_walk_cb"))),
            algo_field("Walk length (steps per iteration)",
              textInput("walk_length_cb", NULL, value = resolved(3, "walk_length_cb"), width = "100%")),
            algo_field("Sliding window length",
              textInput("win_len_cb", NULL, value = resolved(4, "win_len_cb"), width = "100%")),
            algo_field("Use H-polytopes in MMC (for zonotopes)",
              pickerInput("hpoly_cb", NULL, width = "100%",
                choices = c("default", "TRUE", "FALSE"),
                selected = input_volume$value[5])),
            algo_field("Random seed",
              textInput("seed_cb", NULL, value = input_volume$value[6], width = "100%"))
          )
        ),
        tabPanel("Cooling Gaussian",
          div(style = "padding:16px 4px 0 4px;",
            algo_field("Approximation error (upper bound)",
              textInput("error_cg", NULL, value = resolved(11, "error_cg"), width = "100%")),
            algo_field("Random walk method",
              pickerInput("random_walk_cg", NULL, width = "100%",
                choices = c("default", "Coordinate Directions Hit-and-Run", "Random Directions Hit-and-Run", "Ball Walk"),
                selected = resolved(12, "random_walk_cg"))),
            algo_field("Walk length (steps per iteration)",
              textInput("walk_length_cg", NULL, value = resolved(13, "walk_length_cg"), width = "100%")),
            algo_field("Sliding window length",
              textInput("win_len_cg", NULL, value = resolved(14, "win_len_cg"), width = "100%")),
            algo_field("Random seed",
              textInput("seed_cg", NULL, value = input_volume$value[15], width = "100%"))
          )
        ),
        tabPanel("Sequence of Balls",
          div(style = "padding:16px 4px 0 4px;",
            algo_field("Approximation error (upper bound)",
              textInput("error_sob", NULL, value = resolved(7, "error_sob"), width = "100%")),
            algo_field("Random walk method",
              pickerInput("random_walk_sob", NULL, width = "100%",
                choices = c("default", "Coordinate Directions Hit-and-Run", "Random Directions Hit-and-Run", "Ball Walk", "Billiard Walk"),
                selected = resolved(8, "random_walk_sob"))),
            algo_field("Walk length (steps per iteration)",
              textInput("walk_length_sob", NULL, value = resolved(9, "walk_length_sob"), width = "100%")),
            algo_field("Random seed",
              textInput("seed_sob", NULL, value = input_volume$value[10], width = "100%"))
          )
        )
      ),
      footer = tagList(
        p(style = "font-size:11px; color:#888; display:inline; margin-right:12px;",
          "See the ",
          tags$a(href = "https://cran.r-project.org/web/packages/volesti/volesti.pdf",
            "volesti manual", target = "_blank"),
          " for details. Fields are pre-filled with each algorithm's own default."
        ),
        actionBttn("submit", "Save", style = "material-flat", color = "primary", size = "xs")
      ),
      easyClose = TRUE,
      fade = TRUE,
      size = "m"
    ))
  })

  observeEvent(input$submit, {
    removeModal()

    input_volume_reactive$value[1] <- isolate(input$error_cb)
    input_volume_reactive$value[2] <- isolate(input$random_walk_cb)
    input_volume_reactive$value[3] <- isolate(input$walk_length_cb)
    input_volume_reactive$value[4] <- isolate(input$win_len_cb)
    input_volume_reactive$value[5] <- isolate(input$hpoly_cb)
    input_volume_reactive$value[6] <- isolate(input$seed_cb)

    input_volume_reactive$value[7] <- isolate(input$error_sob)
    input_volume_reactive$value[8] <- isolate(input$random_walk_sob)
    input_volume_reactive$value[9] <- isolate(input$walk_length_sob)
    input_volume_reactive$value[10] <- isolate(input$seed_sob)

    input_volume_reactive$value[11] <- isolate(input$error_cg)
    input_volume_reactive$value[12] <- isolate(input$random_walk_cg)
    input_volume_reactive$value[13] <- isolate(input$walk_length_cg)
    input_volume_reactive$value[14] <- isolate(input$win_len_cg)
    input_volume_reactive$value[15] <- isolate(input$seed_cg)
  })

  #### Plot examples ####

  observeEvent(input$go_example, {
    output$plot_example <- renderPlotly({
      select_v <- input$go_example

      # input$go_example can be NULL, character(0), or (before the
      # picker's very first updatePickerInput() calls choices=NA — see
      # its own choices=NA default) even a bare logical NA rather than a
      # real string. validate()/need() kept crashing with an internal
      # "is.character(txt) is not TRUE" on some combination of these
      # (never fully pinned down which — need()'s message-typing
      # machinery turned out to be genuinely fragile here across several
      # attempts to harden the checks feeding it). req() sidesteps that
      # whole category of failure: it doesn't go through a character
      # message at all, just silently stops rendering until its
      # condition is truthy — exactly "wait for a valid selection",
      # which is all this needed in the first place.
      req(select_v, cancelOutput = TRUE)
      select_v <- as.character(select_v)[1]
      if (identical(select_v, "NA")) select_v <- names(v_reactive)[1]
      req(select_v, !is.na(select_v), cancelOutput = TRUE)

      # select_v can point at a model that no longer has a V-representation
      # (e.g. the picker's choices haven't refreshed yet after the model
      # list changed) — building the plot data.frame from an empty/NULL
      # matrix below throws a confusing "differing number of rows" error
      # instead of just showing nothing until a valid model is selected.
      req(v_reactive[[select_v]]$output, cancelOutput = TRUE)

      v_pl <- q2d(v_reactive[[select_v]]$output)

      # plain_axis_name, not the raw column name: plotly's axis/tick text
      # is plain text with no MathJax pass, so the LaTeX source these
      # columns carry (see tex_name/cn_factors) shows up completely
      # unrendered — literal "p_{1}^{(\text{p_{1}},1)}" instead of a
      # readable label (confirmed directly from a screenshot of this
      # exact axis).
      parameter_names <- plain_axis_name(colnames(v_pl)[3:ncol(v_pl)])

      v_pl <- data.frame(
        rep(parameter_names, nrow(v_pl)),
        as.numeric(t(v_pl[, 3:ncol(v_pl)])),
        rep(1:nrow(v_pl),
          each =
            length(parameter_names)
        )
      )

      colnames(v_pl) <- c("parameter_names", "parameters", "vert_no")

      # Build the animated scatter with plot_ly directly. The previous
      # ggplotly() approach broke on current ggplot2/plotly versions: the
      # `frame` aesthetic is silently dropped ("Ignoring unknown aesthetics:
      # frame"), so no animation frames are created and animation_opts() then
      # fails with "subscript out of bounds". A single vertex also cannot be
      # animated, so guard on the number of vertices.
      n_vert <- length(unique(v_pl$vert_no))

      if (n_vert > 1) {
        p <- plot_ly(v_pl,
          x = ~parameter_names, y = ~parameters, frame = ~vert_no,
          type = "scatter", mode = "markers", marker = list(size = 10)
        ) %>%
          animation_opts(transition = 0) %>%
          animation_slider(currentvalue = list(prefix = "Vertex "))
      } else {
        p <- plot_ly(v_pl,
          x = ~parameter_names, y = ~parameters,
          type = "scatter", mode = "markers", marker = list(size = 10)
        )
      }

      p %>% layout(
        xaxis = list(title = "Parameters"),
        # Keep the 0-1 tick labels but pad the range slightly so markers
        # sitting exactly at 0 or 1 aren't clipped at the axis edge.
        yaxis = list(
          title = "Parameter values",
          range = c(-0.05, 1.05), tick0 = 0, dtick = 0.1
        )
      )
    })

  })
})

shinyApp(ui, server)

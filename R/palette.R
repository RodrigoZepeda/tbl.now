# =============================================================================
# The tbl.now colour palette.
#
# Every colour any plot in the package draws comes from here, and every one of
# them is named for the ROLE it plays rather than for the hue it happens to be.
# That distinction is the whole point of this file: the old names encoded the
# colour (`primary_green`, `accent_red`), so a user who handed in a blue-and-
# orange palette had to call their orange `primary_green` for the plots to find
# it. A role name survives a re-theme; a hue name does not.
#
# The red/green *grammar* (DEVELOPMENT_SKILL.md section 6) is unchanged, and is
# what the role names now say out loud: `reporting*` is the reporting process
# (when we found out), `epidemic*` is the epidemic process (what happened).
# =============================================================================

#' The `tbl.now` colour palette
#'
#' @description
#' Builds the named colour palette every `plot_*()` function, [autoplot()] and
#' [diagnostic_plot()] draw from. Each element is named for the **role** it
#' plays in a plot, never for its hue, so a palette in different colours is a
#' matter of overriding the roles you care about:
#'
#' ```r
#' plot_reporting_triangle(x, palette = tbl_now_palette(reporting = "#5B4B8A"))
#' ```
#'
#' Arguments you do not name keep the package default, so a partial palette is
#' always complete. Passing a bare named vector works too, as long as it carries
#' every role below -- the plots validate it and name what is missing.
#'
#' @details
#' # The grammar
#'
#' The package has one visual grammar and the role names state it:
#'
#' * **`reporting*`** -- the *reporting* process: report dates, delays, anything
#'   about **when we found out**. Red by default.
#' * **`epidemic*`** -- the *epidemic* process: event dates, case counts,
#'   anything about **what happened**. Green by default.
#'
#' A palette that swaps the two hues is fine; a *plot* that draws delays with an
#' `epidemic*` role is a bug, whatever colour it comes out.
#'
#' The remaining roles are furniture — text, gridlines, reference lines and the
#' three data states that are not a process (`zero`, `pending`, `observed`).
#'
#' @param reporting Strong colour of the reporting process (bars, medians,
#'   flagged points).
#' @param reporting_light Attenuated reporting colour (box fills, wide
#'   intervals).
#' @param epidemic Strong colour of the epidemic process (lines, bars).
#' @param epidemic_light Attenuated epidemic colour (area fills).
#' @param epidemic_mid Mid-tone epidemic colour (the middle stop of the count
#'   ramp).
#' @param epidemic_dark Darkest epidemic colour (dense overplotted curves).
#' @param ink Body text, axis text and titles.
#' @param ink_muted Secondary text: subtitles, captions, immature-region shading.
#' @param ink_inverse Text drawn *on top of* a filled label.
#' @param surface Fill of a label or a highlight drawn over the data.
#' @param surface_muted Palest surface: the low end of a sequential ramp.
#' @param surface_dark Deep surface for a region with no estimate (the
#'   scalogram's cone of influence).
#' @param grid_major Major gridlines the package draws itself.
#' @param grid_minor Minor gridlines the package draws itself.
#' @param guide Weak reference lines (a zero line, the low end of a count ramp).
#' @param guide_strong Stronger reference lines (the reporting triangle's
#'   iso-report diagonals).
#' @param annotation Text of an annotation label drawn over the data.
#' @param neutral De-emphasised marks: the points a test did *not* flag.
#' @param zero A cell that is observable but reported nothing -- a genuine zero,
#'   as opposed to a cell that is blank because it is not yet reportable.
#' @param pending A case that is reported and not yet resolved.
#' @param observed Counts as they stand now, drawn underneath an estimate of
#'   them. Deliberately neutral: colouring these with a process role would put
#'   the data in the same visual family as the model fitted to it.
#'
#' @returns A named character vector of colours, one per role, with class
#'   `tbl_now_palette`.
#'
#' @seealso [autoplot()][autoplot.tbl_now], [diagnostic_plot()] and
#'   [plot_reporting_hexamap()], all of which take a `palette` argument.
#'
#' @examples
#' tbl_now_palette()
#'
#' # Override one role; the rest keep the package defaults.
#' tbl_now_palette(reporting = "#5B4B8A")
#'
#' data(denguedat)
#' dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_epidemic_process(dn, palette = tbl_now_palette(epidemic = "#2F6DB4"))
#'
#' @export
#' @md
tbl_now_palette <- function(
  reporting       = "#B85348",
  reporting_light = "#e78b7f",
  epidemic        = "#5F7E62",
  epidemic_light  = "#A8BFA9",
  epidemic_mid    = "#7A9E7E",
  epidemic_dark   = "#334335",
  ink             = "#262626",
  ink_muted       = "#607060",
  ink_inverse     = "#FFFFFF",
  surface         = "#FFFFFF",
  surface_muted   = "#F5F5F5",
  surface_dark    = "#1A1A1A",
  grid_major      = "#999999",
  grid_minor      = "#E0E0E0",
  guide           = "#D9D9D9",
  guide_strong    = "#737373",
  annotation      = "#333333",
  neutral         = "#B3B3B3",
  zero            = "#C4D5DE",
  pending         = "#C9CEC9",
  observed        = "#DFE1DF"
) {
  out <- c(
    reporting       = reporting,
    reporting_light = reporting_light,
    epidemic        = epidemic,
    epidemic_light  = epidemic_light,
    epidemic_mid    = epidemic_mid,
    epidemic_dark   = epidemic_dark,
    ink             = ink,
    ink_muted       = ink_muted,
    ink_inverse     = ink_inverse,
    surface         = surface,
    surface_muted   = surface_muted,
    surface_dark    = surface_dark,
    grid_major      = grid_major,
    grid_minor      = grid_minor,
    guide           = guide,
    guide_strong    = guide_strong,
    annotation      = annotation,
    neutral         = neutral,
    zero            = zero,
    pending         = pending,
    observed        = observed
  )
  if (!is.character(out) || anyNA(out)) {
    cli::cli_abort("Every colour in a {.cls tbl_now_palette} must be a string.")
  }
  structure(out, class = c("tbl_now_palette", "character"))
}

#' The roles a palette must carry
#'
#' Taken from the constructor's own formals so the two cannot drift apart.
#'
#' @return A character vector of role names.
#'
#' @keywords internal
#' @noRd
.tbl_now_palette_roles <- function() {
  names(formals(tbl_now_palette))
}

#' Default palette
#'
#' Kept as the internal name the plotting code has always used; it is now a
#' one-line wrapper so there is a single source of the defaults.
#'
#' @return A `tbl_now_palette`.
#'
#' @keywords internal
#' @noRd
.tbl_now_palette <- function() {
  tbl_now_palette()
}

#' Check that a user-supplied palette carries every role
#'
#' A missing role is an error naming the roles, not a silent fall back to the
#' default: falling back would draw a plot in a colour the caller did not choose
#' and could not see they had not chosen.
#'
#' @param palette The `palette` argument as the user supplied it.
#' @param fn Calling function, for the message.
#'
#' @return `palette`, invisibly, as a plain named character vector.
#'
#' @keywords internal
#' @noRd
.tbl_now_check_palette <- function(palette, fn = "a plot function") {
  if (!is.character(palette) || is.null(names(palette))) {
    cli::cli_abort(c(
      "{.arg palette} must be a named character vector of colours.",
      "i" = "Build one with {.fn tbl_now_palette}."
    ))
  }
  missing_roles <- setdiff(.tbl_now_palette_roles(), names(palette))
  if (length(missing_roles) > 0) {
    cli::cli_abort(c(
      "The {.arg palette} given to {.fn {fn}} is missing \\
       {length(missing_roles)} role{?s}: {.val {missing_roles}}.",
      "i" = "{.fn tbl_now_palette} fills every role, so \\
             {.code tbl_now_palette(...)} only needs the ones you change."
    ))
  }
  invisible(palette)
}

#' Print a palette, one role per line
#'
#' Registered on `base::print` rather than with a plain `@export`: the package
#' namespace defines an S7 `print` generic, which shadows `base::print` for an
#' attached session, so a plainly-exported `print.*` method never dispatches on
#' auto-print. (`print.diagnose_batches` and `print.transport_discriminant`
#' still have that bug.)
#'
#' @param x A `tbl_now_palette`.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @keywords internal
#' @exportS3Method base::print
print.tbl_now_palette <- function(x, ...) {
  cli::cat_rule("tbl.now palette")
  roles <- names(x)
  cli::cat_line(format(roles, width = max(nchar(roles))), "  ", unclass(x))
  invisible(x)
}

#' Colours and subtitle for a panel family
#'
#' Every panel belongs to one of two processes, and the whole package uses the
#' same visual grammar for them: the **reporting** roles for anything
#' reporting-related, the **epidemic** roles for the event-date process. The
#' subtitle says which one the panel describes, so a panel lifted out of the
#' `autoplot()` grid still reads on its own.
#'
#' @param process `"reporting"` or `"epidemic"`.
#' @param palette A named colour palette (see [tbl_now_palette()]).
#'
#' @return A list with `fill`, `line` and `subtitle`.
#'
#' @keywords internal
#' @noRd
.tbl_now_process_style <- function(process, palette) {
  if (identical(process, "reporting")) {
    list(
      fill     = palette[["reporting_light"]],
      line     = palette[["reporting"]],
      subtitle = "Reporting delay process"
    )
  } else {
    list(
      fill     = palette[["epidemic_light"]],
      line     = palette[["epidemic"]],
      subtitle = "Epidemic (event-date) process"
    )
  }
}

#' Shared ggplot2 theme for the diagnostic panels
#'
#' @param palette A named colour palette (see [tbl_now_palette()]).
#'
#' @return A ggplot2 theme object.
#'
#' @keywords internal
#' @noRd
.tbl_now_theme <- function(palette) {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", colour = palette[["ink"]]),
      plot.subtitle    = ggplot2::element_text(colour = palette[["ink_muted"]]),
      plot.caption     = ggplot2::element_text(colour = palette[["ink_muted"]], hjust = 0,
                                               size = 8, lineheight = 1.05),
      plot.caption.position = "plot",
      axis.title       = ggplot2::element_text(colour = palette[["ink"]]),
      axis.text        = ggplot2::element_text(colour = palette[["ink"]]),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# =============================================================================
# Size arguments.
#
# Every plotting function takes at least `size` and/or `linewidth`. Outside
# `plot_reporting_hexamap()` -- whose points are drawn in millimetres against a
# lattice measured in data units, so an absolute size is the only useful knob --
# they are MULTIPLIERS on the values the panel already uses, defaulting to 1.
#
# The reason is that most panels draw several layers whose relative sizes carry
# meaning: the transport plane's flagged points are 2.6 against the unflagged
# 1.1, and a single absolute `size` would erase that distinction at exactly the
# moment the user was trying to make the flagged ones easier to see. A
# multiplier scales the hierarchy instead of flattening it, and leaves every
# existing figure byte-identical at the default.
# =============================================================================

#' Validate a size / linewidth argument
#'
#' @param value The value supplied.
#' @param arg The argument's name, for the message.
#'
#' @return `value`, invisibly.
#'
#' @keywords internal
#' @noRd
.tbl_now_check_size <- function(value, arg) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < 0) {
    cli::cli_abort("{.arg {arg}} must be a single non-negative number.")
  }
  invisible(value)
}

#' A length in centimetres, or a `unit` passed straight through
#'
#' Lets `legend_width = 7` and `legend_width = grid::unit(2, "in")` both work.
#'
#' @param value A number (centimetres) or a \link[grid]{unit}.
#'
#' @return A \link[grid]{unit}.
#'
#' @keywords internal
#' @noRd
.tbl_now_cm <- function(value) {
  if (grid::is.unit(value)) {
    return(value)
  }
  .tbl_now_check_size(value, "legend size")
  grid::unit(value, "cm")
}

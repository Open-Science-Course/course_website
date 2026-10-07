# Interactive summary of course participants for the courses page.
# Reads data/student_stats.xlsx. The newest year is treated as the upcoming course.

participant_stats <- function(path) {
  dat <- read_participants(path)
  upcoming <- max(dat$year)
  world <- participant_world(dat)

  payload <- list(
    people = lapply(seq_len(nrow(dat)), function(i) {
      list(
        year = dat$year[[i]],
        iso = dat$iso[[i]],
        country = dat$country[[i]],
        degree = dat$degree[[i]]
      )
    }),
    fields = lapply(seq_len(nrow(dat)), function(i) {
      list(
        year = dat$year[[i]],
        habitat = as.list(dat$habitat[[i]]),
        organism = as.list(dat$organism[[i]]),
        field = as.list(dat$field[[i]])
      )
    }),
    fieldGroups = research_field_groups(),
    shapes = as.character(world$adm0_a3),
    placeNames = as.list(stats::setNames(as.character(world$name), world$adm0_a3)),
    upcoming = upcoming
  )
  map <- build_participant_map(world, dat, payload)

  htmlwidgets::prependContent(
    map,
    participant_css(),
    htmltools::tags$div(
      class = "os-stats",
      participant_intro(dat, upcoming),
      participant_filters(sort(unique(dat$year)), upcoming),
      participant_cards(dat)
    )
  ) |>
    htmlwidgets::appendContent(
      htmltools::tags$div(
        class = "os-stats",
        htmltools::tags$p(id = "os-stats-detail", class = "os-detail", hidden = NA),
        htmltools::tags$div(
          class = "os-split",
          htmltools::tags$div(
            class = "os-panel",
            htmltools::tags$p(class = "os-kicker", "Degree"),
            htmltools::tags$div(id = "os-stats-degrees")
          ),
          htmltools::tags$div(
            class = "os-panel",
            htmltools::tags$p(class = "os-kicker", "Countries"),
            htmltools::tags$div(id = "os-stats-countries")
          )
        ),
        htmltools::tags$div(
          class = "os-panel os-years-panel",
          htmltools::tags$p(class = "os-kicker", "Participants by year"),
          htmltools::tags$div(id = "os-stats-years"),
          htmltools::tags$p(
            class = "os-caption",
            sprintf("The pink bar is %s, registrations for the upcoming course.", upcoming)
          )
        ),
        htmltools::tags$div(
          class = "os-panel os-fields-panel",
          htmltools::tags$p(class = "os-kicker", "Research topics"),
          htmltools::tags$div(id = "os-stats-fields")
        )
      )
    )
}

research_field_groups <- function() {
  list(
    list(
      key = "habitat",
      label = "Habitat",
      levels = c("Terrestrial", "Aquatic", "NA")
    ),
    list(
      key = "organism",
      label = "Organism",
      levels = c("Animals", "Plants", "Fungi", "Microbes", "NA")
    ),
    list(
      key = "field",
      label = "Field",
      levels = c(
        "Conservation ecology",
        "Community ecology",
        "Population ecology",
        "Ecosystem ecology",
        "Behavioural ecology",
        "Genetics",
        "Biodiversity informatics",
        "NA"
      )
    )
  )
}

parse_field_values <- function(x, allowed, column) {
  lapply(x, function(value) {
    if (length(value) != 1 || is.na(value)) {
      return("NA")
    }
    text <- trimws(as.character(value))
    if (!nzchar(text) || toupper(text) == "NA") {
      return("NA")
    }
    vals <- trimws(strsplit(text, ";", fixed = TRUE)[[1]])
    vals <- vals[nzchar(vals)]
    vals[toupper(vals) == "NA"] <- "NA"
    unknown <- setdiff(vals, allowed)
    if (length(unknown)) {
      stop(
        "Unknown ", column, " values: ", paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }
    vals
  })
}

read_participants <- function(path) {
  raw <- readxl::read_excel(path)
  raw <- raw[, !grepl("^\\.\\.\\.", names(raw), perl = TRUE), drop = FALSE]
  needed <- c("Course", "Country", "Degree")
  missing <- setdiff(needed, names(raw))
  if (length(missing)) {
    stop("Participant file is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if ("Name" %in% names(raw) || "name" %in% names(raw)) {
    stop("Participant file still contains names. Remove the Name column before rendering.", call. = FALSE)
  }

  degree <- trimws(as.character(raw$Degree))
  degree[is.na(degree) | !nzchar(degree) | toupper(degree) %in% c("NA", "N/A")] <- "Not recorded"
  degree[toupper(degree) == "PHD"] <- "PhD"
  degree <- dplyr::case_when(
    degree %in% c("Professor", "Associate professor", "Assistant professor and PhD") ~ "Professor",
    degree %in% c("Researcher", "Lecturer") ~ "Researcher and lecturer",
    degree %in% c("Technician", "Research Assistant") ~ "Technician and research assistant",
    TRUE ~ degree
  )

  country_raw <- trimws(as.character(raw$Country))
  country <- dplyr::case_when(
    country_raw %in% c("Finnland", "Finland") ~ "Finland",
    country_raw %in% c("Island", "Iceland") ~ "Iceland",
    country_raw %in% c("UK", "United Kingdom", "Northern Ireland") ~ "United Kingdom",
    country_raw == "Denmark/Greenland" ~ "Greenland",
    TRUE ~ country_raw
  )

  iso <- c(
    Norway = "NOR", India = "IND", Spain = "ESP", Sweden = "SWE", Denmark = "DNK",
    Armenia = "ARM", Belarus = "BLR", Tajikistan = "TJK", Ukraine = "UKR", Georgia = "GEO",
    Italy = "ITA", Thailand = "THA", Switzerland = "CHE", Belgium = "BEL", Finland = "FIN",
    "United Kingdom" = "GBR", Greenland = "GRL", Iceland = "ISL", Ghana = "GHA",
    Austria = "AUT", Germany = "DEU"
  )

  groups <- research_field_groups()
  allowed <- stats::setNames(
    lapply(groups, `[[`, "levels"),
    vapply(groups, `[[`, character(1), "key")
  )
  habitat_raw <- if ("Habitat" %in% names(raw)) raw$Habitat else rep("NA", nrow(raw))
  organism_raw <- if ("Organism" %in% names(raw)) raw$Organism else rep("NA", nrow(raw))
  field_raw <- if ("Field" %in% names(raw)) raw$Field else rep("NA", nrow(raw))

  dat <- dplyr::tibble(
    year = as.integer(raw$Course),
    country = country,
    iso = unname(iso[country]),
    degree = degree,
    habitat = parse_field_values(habitat_raw, allowed$habitat, "habitat"),
    organism = parse_field_values(organism_raw, allowed$organism, "organism"),
    field = parse_field_values(field_raw, allowed$field, "field")
  )

  if (any(is.na(dat$year))) {
    stop("Every row needs a course year.", call. = FALSE)
  }
  unknown <- unique(dat$country[is.na(dat$iso)])
  if (length(unknown)) {
    stop("No map code for: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  dat
}

participant_world <- function(dat) {
  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)

  world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
  world <- world[!world$adm0_a3 %in% c("ATA"), c("adm0_a3", "name")]
  world <- sf::st_make_valid(world)
  world <- sf::st_simplify(world, dTolerance = 0.25, preserveTopology = TRUE)
  world <- sf::st_make_valid(world)
  world <- world[!sf::st_is_empty(world), ]
  world$adm0_a3 <- as.character(world$adm0_a3)

  missing <- setdiff(unique(dat$iso), world$adm0_a3)
  if (length(missing)) {
    stop("Missing from the world map: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  world
}

participant_intro <- function(dat, upcoming) {
  past <- dat[dat$year < upcoming, , drop = FALSE]
  registered <- sum(dat$year == upcoming)
  htmltools::tags$p(
    class = "os-lead",
    htmltools::HTML(sprintf(
      "Since <strong>%s</strong>, <strong>%s</strong> people have taken part, from <strong>%s</strong> countries. Another <strong>%s</strong> are registered for %s. Most people on the list are PhD students. Choose a year to see where participants came from and which degree they were working towards.",
      min(past$year),
      nrow(past),
      dplyr::n_distinct(past$country),
      registered,
      upcoming
    ))
  )
}

participant_filters <- function(years, upcoming) {
  button <- function(year, label, pressed = FALSE, title = NULL) {
    htmltools::tags$button(
      type = "button",
      class = paste("os-year-btn", if (pressed) "is-active"),
      `data-year` = year,
      `aria-pressed` = if (pressed) "true" else "false",
      title = title,
      label
    )
  }
  htmltools::tags$div(
    class = "os-filters",
    role = "group",
    `aria-label` = "Choose a course year",
    button("all", "All years", pressed = TRUE),
    lapply(years, function(year) {
      button(
        year,
        as.character(year),
        title = if (year == upcoming) "Registrations for the upcoming course"
      )
    })
  )
}

participant_cards <- function(dat) {
  phd <- sum(dat$degree == "PhD")
  card <- function(id, value, label) {
    htmltools::tags$div(
      class = "os-card",
      htmltools::tags$div(class = "os-num", id = id, value),
      htmltools::tags$div(class = "os-label", label)
    )
  }
  htmltools::tags$div(
    class = "os-cards",
    card("os-stat-people", nrow(dat), "Participants"),
    card("os-stat-countries", dplyr::n_distinct(dat$country), "Countries"),
    card("os-stat-phd", phd, "PhD students"),
    card("os-stat-other", nrow(dat) - phd, "Other degrees")
  )
}

fill_for <- function(n, max_n) {
  t <- (n / max_n)^0.45
  t[n <= 0] <- NA_real_
  ramp <- grDevices::colorRamp(c("#F8E3E9", "#344966"))(ifelse(is.na(t), 0, t))
  hex <- grDevices::rgb(ramp[, 1], ramp[, 2], ramp[, 3], maxColorValue = 255)
  hex[is.na(t)] <- "#e7eef5"
  hex
}

build_participant_map <- function(world, dat, payload) {
  counts <- dat |>
    dplyr::count(iso, name = "n")
  world <- dplyr::left_join(world, counts, by = c("adm0_a3" = "iso"))
  world$n[is.na(world$n)] <- 0L
  max_n <- max(counts$n, 1L)
  world$fill <- fill_for(world$n, max_n)

  labels <- lapply(seq_len(nrow(world)), function(i) {
    if (world$n[[i]] > 0) {
      htmltools::HTML(sprintf(
        "<strong>%s</strong><br/>%d %s",
        world$name[[i]],
        world$n[[i]],
        if (world$n[[i]] == 1) "participant" else "participants"
      ))
    } else {
      NULL
    }
  })

  leaflet::leaflet(
    world,
    width = "100%",
    height = 460,
    elementId = "os-participant-map",
    options = leaflet::leafletOptions(
      zoomControl = TRUE,
      attributionControl = FALSE,
      scrollWheelZoom = FALSE,
      minZoom = 1,
      maxZoom = 7,
      worldCopyJump = FALSE
    )
  ) |>
    leaflet::addPolygons(
      layerId = ~adm0_a3,
      fillColor = ~fill,
      fillOpacity = 1,
      color = ~ifelse(n > 0, "#ffffff", "transparent"),
      weight = ~ifelse(n > 0, 0.7, 0),
      opacity = 1,
      label = labels,
      labelOptions = leaflet::labelOptions(
        className = "os-tip",
        direction = "auto",
        opacity = 1,
        textsize = "13px"
      )
    ) |>
    leaflet::addControl(
      html = htmltools::HTML(
        '<div class="os-legend"><span>None</span><span class="os-legend-bar" aria-hidden="true"></span><span>More</span></div>'
      ),
      position = "topright",
      className = "os-legend-control"
    ) |>
    leaflet::setView(lng = 18, lat = 24, zoom = 2) |>
    htmlwidgets::onRender(participant_js(), data = payload)
}

participant_css <- function() {
  htmltools::tags$style(htmltools::HTML("
.os-stats { color: #243140; }
.os-lead { margin: 0 0 0.9rem; }
.os-filters { display: flex; flex-wrap: wrap; gap: 0.45rem; margin: 0 0 0.85rem; }
.os-year-btn, .os-clear {
  border: 1px solid #344966;
  background: transparent;
  color: #344966;
  border-radius: 999px;
  padding: 0.28rem 0.8rem;
  font: inherit;
  cursor: pointer;
  line-height: 1.3;
}
.os-year-btn.is-active { background: #344966; color: #fff; }
.os-clear { margin-left: 0.55rem; padding: 0.12rem 0.6rem; font-size: 0.82rem; }
.os-stats button:focus-visible { outline: 2px solid #c00ca9; outline-offset: 2px; }
.os-cards { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 0.7rem; margin: 0 0 0.85rem; }
.os-card, .os-panel {
  background: rgba(255, 255, 255, 0.84);
  border-radius: 16px;
  box-shadow: 0 8px 24px rgba(52, 73, 102, 0.08);
}
.os-card { padding: 0.85rem 0.95rem 0.75rem; }
.os-num {
  font-size: 1.85rem;
  font-weight: 700;
  letter-spacing: -0.03em;
  line-height: 1;
  color: #344966;
  font-variant-numeric: tabular-nums;
}
.os-label { margin-top: 0.28rem; color: #4c5d70; font-size: 0.88rem; }
.os-split { display: grid; grid-template-columns: 1fr 1fr; gap: 0.75rem; margin-top: 0.75rem; }
.os-panel { padding: 0.95rem 1rem 0.8rem; }
.os-years-panel, .os-fields-panel { margin-top: 0.75rem; }
.os-kicker { margin: 0 0 0.65rem; font-weight: 700; color: #344966; }
.os-field-groups {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 0.9rem 1rem;
}
.os-field-group-label {
  margin: 0 0 0.45rem;
  font-size: 0.82rem;
  font-weight: 700;
  color: #4c5d70;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}
.os-more {
  margin: 0.15rem 0 0;
  padding: 0.15rem 0;
  border: 0;
  background: transparent;
  color: #344966;
  font: inherit;
  font-size: 0.82rem;
  font-weight: 600;
  cursor: pointer;
  text-decoration: underline;
  text-underline-offset: 2px;
}
.os-more:hover { color: #c00ca9; }
.os-pie-wrap {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.55rem;
}
.os-pie {
  width: 148px;
  height: 148px;
  display: block;
}
.os-pie-legend {
  width: 100%;
  display: flex;
  flex-direction: column;
  gap: 0.28rem;
}
.os-pie-item {
  display: grid;
  grid-template-columns: 0.7rem minmax(0, 1fr) auto;
  gap: 0.4rem;
  align-items: center;
  font-size: 0.82rem;
  color: #243140;
}
.os-pie-swatch {
  width: 0.7rem;
  height: 0.7rem;
  border-radius: 2px;
}
.os-pie-label { line-height: 1.25; }
.os-pie-n {
  font-weight: 600;
  font-variant-numeric: tabular-nums;
  color: #344966;
}
@media (max-width: 900px) {
  .os-field-groups { grid-template-columns: 1fr; }
}
.os-bar {
  display: grid;
  grid-template-columns: minmax(6.2rem, 46%) minmax(0, 1fr) 1.7rem;
  gap: 0.45rem;
  align-items: center;
  width: 100%;
  margin: 0 0 0.38rem;
  padding: 0;
  border: 0;
  background: transparent;
  color: inherit;
  font: inherit;
  text-align: left;
}
button.os-bar { cursor: pointer; }
.os-bar-name { font-size: 0.86rem; line-height: 1.25; }
.os-bar-track { background: #e7eef5; border-radius: 999px; height: 0.7rem; overflow: hidden; }
.os-bar-fill { display: block; height: 100%; background: #344966; border-radius: 999px; }
.os-bar.is-selected .os-bar-fill { background: #DA7B93; }
.os-bar-n { font-size: 0.86rem; font-weight: 600; color: #344966; text-align: right; font-variant-numeric: tabular-nums; }
.os-years { display: flex; align-items: stretch; gap: 0.45rem; }
.os-year {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.28rem;
  border: 0;
  background: transparent;
  color: inherit;
  font: inherit;
  cursor: pointer;
  padding: 0;
}
.os-year-plot { height: 120px; width: 100%; display: flex; align-items: flex-end; justify-content: center; }
.os-year-fill {
  width: min(56px, 78%);
  min-height: 4px;
  border-radius: 8px 8px 4px 4px;
  background: #344966;
}
.os-year.is-upcoming .os-year-fill { background: #DA7B93; }
.os-years.is-filtered .os-year:not(.is-selected) .os-year-fill { opacity: 0.35; }
.os-year.is-selected .os-year-fill { outline: 2px solid #243140; outline-offset: 2px; }
.os-year-count, .os-year-name { font-size: 0.82rem; color: #344966; font-variant-numeric: tabular-nums; }
.os-year-name { font-weight: 700; }
.os-caption, .os-note { margin: 0.7rem 0 0; font-size: 0.86rem; color: #243140; }
.os-detail {
  position: absolute;
  z-index: 450;
  left: 12px;
  right: 12px;
  bottom: 12px;
  margin: 0;
  padding: 0.55rem 0.75rem;
  background: rgba(255, 255, 255, 0.96);
  border-radius: 12px;
  box-shadow: 0 6px 18px rgba(52, 73, 102, 0.14);
  color: #243140;
}
#os-participant-map {
  border-radius: 16px;
  box-shadow: 0 8px 24px rgba(52, 73, 102, 0.08);
  background: #f3f7fb;
  font-family: inherit;
}
#os-participant-map .leaflet-bar { border: none; box-shadow: 0 4px 14px rgba(52, 73, 102, 0.16); }
#os-participant-map .leaflet-bar a { color: #344966; }
#os-participant-map .os-legend-control { background: transparent; border: 0; box-shadow: none; margin-top: 0; }
.os-legend {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  background: rgba(255, 255, 255, 0.94);
  border-radius: 999px;
  padding: 0.28rem 0.65rem;
  color: #344966;
  font-size: 0.75rem;
  box-shadow: 0 4px 14px rgba(52, 73, 102, 0.12);
}
.os-legend-bar { width: 84px; height: 8px; border-radius: 999px; background: linear-gradient(90deg, #F8E3E9, #344966); }
#os-participant-map .os-tip {
  background: #344966;
  color: #fff;
  border: none;
  border-radius: 8px;
  box-shadow: 0 4px 16px rgba(36, 49, 64, 0.18);
  padding: 0.35rem 0.55rem;
}
#os-participant-map .os-tip:before { display: none; }
@media (max-width: 760px) {
  .os-cards, .os-split { grid-template-columns: 1fr 1fr; }
  .os-split { grid-template-columns: 1fr; }
  #os-participant-map { height: 360px !important; }
}
"))
}

participant_js <- function() {
  "function(el, x, data) {
  if (el.dataset.osReady === '1') return;
  el.dataset.osReady = '1';
  var map = (this && this.getMap) ? this.getMap() : this;
  if (!map || !map.layerManager) return;

  var people = data.people || [];
  var fields = data.fields || [];
  var fieldGroups = data.fieldGroups || [];
  var upcoming = Number(data.upcoming);
  var year = 'all';
  var selected = null;
  var hovered = null;
  var swallow = false;
  var degreesOpen = false;
  var countriesOpen = false;
  var shapes = {};
  var ever = {};
  var pieColors = ['#344966', '#DA7B93', '#5B8FA8', '#C4A35A', '#6B8F71', '#8B6B9E', '#D08B5B', '#9aa8b8'];

  people.forEach(function(p) { ever[p.iso] = true; });
  (data.shapes || []).forEach(function(iso) {
    var layer = map.layerManager.getLayer('shape', iso);
    if (!layer) return;
    shapes[iso] = layer;
    layer.on('mouseover', function() { hovered = iso; restyle(); });
    layer.on('mouseout', function() { if (hovered === iso) hovered = null; restyle(); });
    layer.on('click', function() {
      swallow = true;
      if (!ever[iso]) return;
      selected = selected === iso ? null : iso;
      paint();
    });
  });

  map.on('click', function() {
    if (swallow) { swallow = false; return; }
    if (!selected) return;
    selected = null;
    paint();
  });

  var detail = document.getElementById('os-stats-detail');
  if (detail) el.appendChild(detail);

  document.querySelectorAll('.os-year-btn').forEach(function(btn) {
    btn.addEventListener('click', function() {
      var v = btn.getAttribute('data-year');
      var next = v === 'all' ? 'all' : Number(v);
      year = (next !== 'all' && year === next) ? 'all' : next;
      selected = null;
      paint();
    });
  });

  function rowsFor(iso) {
    return people.filter(function(p) {
      if (iso && p.iso !== iso) return false;
      return year === 'all' || Number(p.year) === year;
    });
  }

  function countsOf(key, subset) {
    var out = {};
    subset.forEach(function(p) { out[p[key]] = (out[p[key]] || 0) + 1; });
    return out;
  }

  function sortedKeys(obj) {
    return Object.keys(obj).sort(function(a, b) {
      if (a === 'Not recorded') return 1;
      if (b === 'Not recorded') return -1;
      if (obj[b] !== obj[a]) return obj[b] - obj[a];
      return a < b ? -1 : 1;
    });
  }

  function placeName(iso) {
    if (data.placeNames && data.placeNames[iso]) return data.placeNames[iso];
    var hit = people.filter(function(p) { return p.iso === iso; })[0];
    return hit ? hit.country : iso;
  }

  function fillFor(n, maxN) {
    if (!n || !maxN) return '#e7eef5';
    var t = Math.pow(n / maxN, 0.45);
    return mix('#F8E3E9', '#344966', t);
  }

  function mix(a, b, t) {
    function part(i) {
      var av = parseInt(a.substr(1 + i * 2, 2), 16);
      var bv = parseInt(b.substr(1 + i * 2, 2), 16);
      return Math.round(av + (bv - av) * t);
    }
    function hex(n) { return ('0' + n.toString(16)).slice(-2); }
    return '#' + hex(part(0)) + hex(part(1)) + hex(part(2));
  }

  function restyle() {
    var subset = rowsFor(null);
    var byIso = countsOf('iso', subset);
    var maxN = 0;
    Object.keys(byIso).forEach(function(k) { if (byIso[k] > maxN) maxN = byIso[k]; });
    Object.keys(shapes).forEach(function(iso) {
      var n = byIso[iso] || 0;
      var chosen = n > 0 && iso === selected;
      var hot = n > 0 && iso === hovered;
      shapes[iso].setStyle({
        fillColor: fillFor(n, maxN),
        fillOpacity: 1,
        color: chosen ? '#243140' : (hot ? '#8b9aab' : (n > 0 ? '#ffffff' : 'transparent')),
        weight: n > 0 ? (chosen ? 2.4 : (hot ? 1.8 : 0.7)) : 0
      });
      if (chosen || hot) shapes[iso].bringToFront();
      bindTip(shapes[iso], n ? '<strong>' + escapeHtml(placeName(iso)) + '</strong><br/>' + peopleWord(n) : null);
    });
  }

  function bindTip(layer, html) {
    layer.unbindTooltip();
    if (!html) return;
    layer.bindTooltip(html, {className: 'os-tip', direction: 'auto', opacity: 1, sticky: true});
  }

  function peopleWord(n) {
    return n + (n === 1 ? ' participant' : ' participants');
  }

  function escapeHtml(s) {
    return String(s).replace(/[&<>]/g, function(c) {
      return c === '&' ? '&amp;' : (c === '<' ? '&lt;' : '&gt;');
    });
  }

  function setCards(subset) {
    var countries = {};
    var phd = 0;
    subset.forEach(function(p) {
      countries[p.country] = true;
      if (p.degree === 'PhD') phd += 1;
    });
    document.getElementById('os-stat-people').textContent = subset.length;
    document.getElementById('os-stat-countries').textContent = Object.keys(countries).length;
    document.getElementById('os-stat-phd').textContent = phd;
    document.getElementById('os-stat-other').textContent = subset.length - phd;
  }

  function appendBarRow(node, key, n, maxN, opts) {
    opts = opts || {};
    var btn = document.createElement(opts.button ? 'button' : 'div');
    if (opts.button) btn.type = 'button';
    btn.className = 'os-bar' + (opts.selected ? ' is-selected' : '');
    var name = document.createElement('span');
    name.className = 'os-bar-name';
    name.textContent = key;
    var track = document.createElement('span');
    track.className = 'os-bar-track';
    var fill = document.createElement('span');
    fill.className = 'os-bar-fill';
    fill.style.width = (maxN ? (100 * n / maxN) : 0) + '%';
    if (n > 0) fill.style.minWidth = '8px';
    track.appendChild(fill);
    var num = document.createElement('span');
    num.className = 'os-bar-n';
    num.textContent = n;
    btn.appendChild(name);
    btn.appendChild(track);
    btn.appendChild(num);
    if (opts.onClick) btn.addEventListener('click', opts.onClick);
    if (opts.onEnter) btn.addEventListener('mouseenter', opts.onEnter);
    if (opts.onLeave) btn.addEventListener('mouseleave', opts.onLeave);
    node.appendChild(btn);
  }

  function appendMoreButton(node, hidden, open, onToggle) {
    if (hidden <= 0) return;
    var btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'os-more';
    btn.textContent = open ? 'Show less' : ('Show ' + hidden + ' more');
    btn.addEventListener('click', onToggle);
    node.appendChild(btn);
  }

  function renderBars(target, obj, isoKey) {
    var node = document.getElementById(target);
    node.replaceChildren();
    var keys = sortedKeys(obj);
    var maxN = 0;
    keys.forEach(function(k) { if (obj[k] > maxN) maxN = obj[k]; });
    var limit = 5;
    var open = target === 'os-stats-degrees' ? degreesOpen : false;
    var shown = open ? keys : keys.slice(0, limit);
    shown.forEach(function(key) {
      appendBarRow(node, key, obj[key], maxN, {});
    });
    appendMoreButton(node, keys.length - limit, open, function() {
      degreesOpen = !degreesOpen;
      paint();
    });
  }

  function renderYears() {
    var byYear = countsOf('year', people);
    var years = Object.keys(byYear).map(Number).sort(function(a, b) { return a - b; });
    var maxN = 0;
    years.forEach(function(y) { if (byYear[y] > maxN) maxN = byYear[y]; });
    var node = document.getElementById('os-stats-years');
    node.className = 'os-years' + (year === 'all' ? '' : ' is-filtered');
    node.replaceChildren();
    years.forEach(function(y) {
      var n = byYear[y];
      var btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'os-year' +
        (y === upcoming ? ' is-upcoming' : '') +
        (year === y ? ' is-selected' : '');
      btn.setAttribute('aria-pressed', year === y ? 'true' : 'false');
      var count = document.createElement('span');
      count.className = 'os-year-count';
      count.textContent = n;
      var plot = document.createElement('span');
      plot.className = 'os-year-plot';
      var fill = document.createElement('span');
      fill.className = 'os-year-fill';
      fill.style.height = Math.max(6, Math.round(120 * n / maxN)) + 'px';
      plot.appendChild(fill);
      var label = document.createElement('span');
      label.className = 'os-year-name';
      label.textContent = y;
      btn.appendChild(count);
      btn.appendChild(plot);
      btn.appendChild(label);
      btn.addEventListener('click', function() {
        year = year === y ? 'all' : y;
        selected = null;
        paint();
      });
      node.appendChild(btn);
    });
  }

  function renderDetail() {
    if (!detail) return;
    if (!selected) {
      detail.hidden = true;
      detail.replaceChildren();
      return;
    }
    var subset = rowsFor(selected);
    var name = placeName(selected);
    detail.hidden = false;
    detail.replaceChildren();
    var strong = document.createElement('strong');
    strong.textContent = name;
    detail.appendChild(strong);
    var when = year === 'all' ? ', all courses' : ' in ' + year;
    if (!subset.length) {
      detail.appendChild(document.createTextNode(when + ': no participants.'));
    } else {
      var degrees = countsOf('degree', subset);
      var parts = sortedKeys(degrees).map(function(d) { return d + ' ' + degrees[d]; });
      detail.appendChild(document.createTextNode(
        when + ' · ' + peopleWord(subset.length) + ': ' + parts.join(', ') + '.'
      ));
    }
    var clear = document.createElement('button');
    clear.type = 'button';
    clear.className = 'os-clear';
    clear.textContent = 'Clear';
    clear.addEventListener('click', function() {
      selected = null;
      paint();
    });
    detail.appendChild(clear);
  }

  function fieldRows() {
    return fields.filter(function(f) {
      return year === 'all' || Number(f.year) === year;
    });
  }

  function asList(value) {
    if (value == null || value === '') return ['NA'];
    return Array.isArray(value) ? value : [value];
  }

  function groupCounts(subset, key, levels) {
    var out = {};
    levels.forEach(function(level) { out[level] = 0; });
    subset.forEach(function(f) {
      asList(f[key]).forEach(function(level) {
        if (out[level] == null) out[level] = 0;
        out[level] += 1;
      });
    });
    return out;
  }

  function polar(cx, cy, r, angle) {
    return [cx + r * Math.cos(angle), cy + r * Math.sin(angle)];
  }

  function pieSlice(cx, cy, r, start, end) {
    var a0 = start - Math.PI / 2;
    var a1 = end - Math.PI / 2;
    var p0 = polar(cx, cy, r, a0);
    var p1 = polar(cx, cy, r, a1);
    var large = (end - start) > Math.PI ? 1 : 0;
    return 'M ' + cx + ' ' + cy +
      ' L ' + p0[0] + ' ' + p0[1] +
      ' A ' + r + ' ' + r + ' 0 ' + large + ' 1 ' + p1[0] + ' ' + p1[1] +
      ' Z';
  }

  function colorForLevel(level, index) {
    if (level === 'NA') return '#9aa8b8';
    return pieColors[index % pieColors.length];
  }

  function renderPie(target, levels, counts) {
    var entries = levels
      .map(function(level) { return { level: level, n: counts[level] || 0 }; })
      .filter(function(e) { return e.n > 0; });
    var total = 0;
    entries.forEach(function(e) { total += e.n; });

    var wrap = document.createElement('div');
    wrap.className = 'os-pie-wrap';

    var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.setAttribute('viewBox', '0 0 120 120');
    svg.setAttribute('class', 'os-pie');
    svg.setAttribute('role', 'img');

    if (!total) {
      var empty = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
      empty.setAttribute('cx', '60');
      empty.setAttribute('cy', '60');
      empty.setAttribute('r', '52');
      empty.setAttribute('fill', '#e7eef5');
      svg.appendChild(empty);
    } else if (entries.length === 1) {
      var full = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
      full.setAttribute('cx', '60');
      full.setAttribute('cy', '60');
      full.setAttribute('r', '52');
      full.setAttribute('fill', colorForLevel(entries[0].level, 0));
      svg.appendChild(full);
    } else {
      var angle = 0;
      entries.forEach(function(entry, i) {
        var slice = (entry.n / total) * Math.PI * 2;
        var path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        path.setAttribute('d', pieSlice(60, 60, 52, angle, angle + slice));
        path.setAttribute('fill', colorForLevel(entry.level, i));
        path.setAttribute('stroke', '#ffffff');
        path.setAttribute('stroke-width', '1.2');
        svg.appendChild(path);
        angle += slice;
      });
    }
    wrap.appendChild(svg);

    var legend = document.createElement('div');
    legend.className = 'os-pie-legend';
    entries.forEach(function(entry, i) {
      var row = document.createElement('div');
      row.className = 'os-pie-item';
      var swatch = document.createElement('span');
      swatch.className = 'os-pie-swatch';
      swatch.style.background = colorForLevel(entry.level, i);
      var label = document.createElement('span');
      label.className = 'os-pie-label';
      label.textContent = entry.level;
      var num = document.createElement('span');
      num.className = 'os-pie-n';
      num.textContent = entry.n;
      row.appendChild(swatch);
      row.appendChild(label);
      row.appendChild(num);
      legend.appendChild(row);
    });
    wrap.appendChild(legend);
    target.appendChild(wrap);
  }

  function renderFields() {
    var node = document.getElementById('os-stats-fields');
    if (!node) return;
    node.replaceChildren();
    var subset = fieldRows();
    if (!subset.length) {
      var empty = document.createElement('p');
      empty.className = 'os-caption';
      empty.style.margin = '0';
      empty.textContent = year === 'all'
        ? 'No research-topic summaries yet.'
        : 'No research-topic summaries for ' + year + '.';
      node.appendChild(empty);
      return;
    }
    var wrap = document.createElement('div');
    wrap.className = 'os-field-groups';
    fieldGroups.forEach(function(group) {
      var col = document.createElement('div');
      col.className = 'os-field-group';
      var label = document.createElement('p');
      label.className = 'os-field-group-label';
      label.textContent = group.label;
      col.appendChild(label);
      renderPie(col, group.levels || [], groupCounts(subset, group.key, group.levels || []));
      wrap.appendChild(col);
    });
    node.appendChild(wrap);
  }

  function paint() {
    var subset = rowsFor(null);
    setCards(subset);
    var degreeCounts = countsOf('degree', subset);
    var countryCounts = {};
    subset.forEach(function(p) {
      countryCounts[p.iso] = (countryCounts[p.iso] || 0) + 1;
    });
    var countryByName = {};
    Object.keys(countryCounts).forEach(function(iso) {
      countryByName[placeName(iso)] = {n: countryCounts[iso], iso: iso};
    });
    renderNamedBars('os-stats-degrees', degreeCounts, false);
    renderCountryBars(countryByName);
    renderYears();
    renderFields();
    renderDetail();
    restyle();
    document.querySelectorAll('.os-year-btn').forEach(function(btn) {
      var v = btn.getAttribute('data-year');
      var on = (v === 'all' && year === 'all') || Number(v) === year;
      btn.classList.toggle('is-active', on);
      btn.setAttribute('aria-pressed', on ? 'true' : 'false');
    });
  }

  function renderNamedBars(target, obj) {
    renderBars(target, obj, false);
  }

  function renderCountryBars(countryByName) {
    var node = document.getElementById('os-stats-countries');
    node.replaceChildren();
    var names = Object.keys(countryByName).sort(function(a, b) {
      if (countryByName[b].n !== countryByName[a].n) return countryByName[b].n - countryByName[a].n;
      return a < b ? -1 : 1;
    });
    var maxN = 0;
    names.forEach(function(name) { if (countryByName[name].n > maxN) maxN = countryByName[name].n; });
    var limit = 5;
    var shown = countriesOpen ? names : names.slice(0, limit);
    shown.forEach(function(name) {
      var item = countryByName[name];
      appendBarRow(node, name, item.n, maxN, {
        button: true,
        selected: item.iso === selected,
        onClick: function() {
          selected = selected === item.iso ? null : item.iso;
          paint();
        },
        onEnter: function() { hovered = item.iso; restyle(); },
        onLeave: function() { if (hovered === item.iso) hovered = null; restyle(); }
      });
    });
    appendMoreButton(node, names.length - limit, countriesOpen, function() {
      countriesOpen = !countriesOpen;
      paint();
    });
  }

  paint();
}"
}

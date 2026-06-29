library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(tidyr)
library(readxl)
library(stringr)
library(stringi)
library(lubridate)
library(httr2)
library(jsonlite)
library(rvest)
library(xml2)

# ════════════════════════════════════════════════════════════════
# SCRAPING DE RESULTADOS — Wikipedia
# Devuelve un data.frame con las MISMAS columnas que el Excel:
# partido_id, fecha, grupo, local, visitante, goles_l, goles_v
# ════════════════════════════════════════════════════════════════
scrape_nombre_equipo <- function(td_node) {
  if (is.na(td_node)) return(NA_character_)
  nodo <- read_html(as.character(td_node))
  for (s in nodo |> html_elements("span[style*='display:none'], span[style*='display: none']")) {
    xml_remove(s)
  }
  links <- nodo |> html_elements("a") |> html_text2() |> str_squish()
  links <- links[links != "" & !str_detect(links, "^UTC")]
  if (length(links) > 0) return(str_to_upper(tail(links, 1)))
  str_to_upper(str_squish(html_text2(nodo)))
}

scrape_resultados_wiki <- function() {
  url  <- "https://es.wikipedia.org/wiki/Anexo:Calendario_de_la_Copa_Mundial_de_F%C3%BAtbol_de_2026"
  page <- read_html(url)

  tabla_node <- page |> html_element("table#mwAjk")
  if (is.na(tabla_node)) {
    tabla_node <- page |>
      html_element("section[aria-labelledby='Partidos']") |>
      html_element("table.wikitable")
  }
  if (is.na(tabla_node)) stop("No se encontró la tabla de Wikipedia.")

  filas <- tabla_node |> html_elements("tr[align='center']")

  partidos <- lapply(filas, function(fila){
    celdas <- fila |> html_elements("td")
    if (length(celdas) < 3) return(NULL)
    textos <- celdas |> html_text2() |> str_squish()
    num_raw <- textos[1]
    if (!str_detect(num_raw,"^\\d+$")) return(NULL)

    # Fecha: primera válida
    f <- str_extract(textos,"\\d{1,2}\\s+de\\s+[[:alpha:]]+")
    f <- f[!is.na(f)]
    fecha_txt <- if (length(f)>0) paste(f[1],"de 2026") else NA_character_

    # Resultado: quitar penales entre paréntesis ANTES de extraer el marcador
    texto_sin_pen <- str_replace_all(textos[3],"\\([^)]*\\)","")
    goles <- str_match(texto_sin_pen,"(\\d+)\\s*[-–]\\s*(\\d+)")
    goles_l <- if(!is.na(goles[1,1])) as.integer(goles[1,2]) else NA_integer_
    goles_v <- if(!is.na(goles[1,1])) as.integer(goles[1,3]) else NA_integer_

    # Equipos
    local <- scrape_nombre_equipo(fila |> html_element("td[align='right']"))
    visitante <- scrape_nombre_equipo(fila |> html_element("td[align='left']"))

    # Grupo
    grupo <- str_extract(textos,"Grupo [A-L]")
    grupo <- grupo[!is.na(grupo)][1]

    data.frame(num=as.integer(num_raw), fecha=fecha_txt, grupo=grupo,
               local=local, visitante=visitante,
               goles_l=goles_l, goles_v=goles_v, stringsAsFactors=FALSE)
  })

  partidos_df <- bind_rows(partidos) |> arrange(num) |>
    fill(fecha, .direction="down") |>
    mutate(
      fase=case_when(
        num<=72 ~ "Fase de grupos", num<=88 ~ "Dieciseisavos",
        num<=96 ~ "Octavos", num<=100 ~ "Cuartos",
        num<=102 ~ "Semifinal", num==103 ~ "Tercer puesto",
        num==104 ~ "Final", TRUE ~ NA_character_)
    ) |>
    # GRUPO por mapeo fijo equipo→grupo (no se scrapea, evita contaminación).
    # Solo fase de grupos (num<=72); eliminatoria queda sin grupo.
    rowwise() |>
    mutate(
      grupo = if (num <= 72) {
        gl <- EQUIPO_GRUPO[[normalizar(local)]]
        gv <- EQUIPO_GRUPO[[normalizar(visitante)]]
        # Si ambos equipos pertenecen al mismo grupo (deben), usar ese
        if (!is.null(gl) && !is.null(gv) && gl == gv) paste0("Grupo ", gl)
        else if (!is.null(gl)) paste0("Grupo ", gl)
        else if (!is.null(gv)) paste0("Grupo ", gv)
        else NA_character_
      } else NA_character_
    ) |>
    ungroup() |>
    # partido_id basado en el NÚMERO OFICIAL de Wikipedia, no en row_number()
    # (evita desalineación si falta algún partido al scrapear)
    mutate(partido_id = sprintf("WC2022_%03d", num)) |>
    select(partido_id, fecha, grupo, local, visitante, goles_l, goles_v)

  partidos_df
}

# ════════════════════════════════════════════════════════════════
# CONSTANTES
# ════════════════════════════════════════════════════════════════
SUPABASE_URL <- "https://hnuqbspysocaklfyvmaa.supabase.co"
SUPABASE_KEY <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImhudXFic3B5c29jYWtsZnl2bWFhIiwicm9sZSI6ImFub24iLCJpYXQiOjE3ODE1MzE0MzYsImV4cCI6MjA5NzEwNzQzNn0.5A9g0YkjdzlZmy3MrciBTqDozprMCT2AxAFd7-14FPI"
LOGO_URL     <- "https://phantom-marca-mx.unidadeditorial.es/f0b61cfe64c66c21d9beb3167957e41c/resize/828/f/jpg/mx/assets/multimedia/imagenes/2023/05/18/16843852531648.jpg"

JUGADORES_LISTA <- sort(c(
  "ADRI MUSIC","ALBERTI","BARBA","CARA DE FLAN","COLMENA","CONRAD",
  "Comandante Pina","Cosmopolita","Diego LUNFARDO","Don Alejo",
  "El destructor de mundos","El tano Luigi MACHINEGUN","El tano Mateo",
  "El tano Pastore","Enzo Casella","Facumbo","Fefo Fútbol","Fonso el Andaluz",
  "GOLABOLSO","GOLITA JR","Giancarlo","Guzi Gang","IMANOL","Juan Derecho",
  "LECHU","LUCA","Lil Rosen","Marcelinho Cisplatino","Memo","Miguelástico",
  "Mito Giles","NOVATO","Nacho Amaral","Nacho Vegas","Nene","Neo Chucho",
  "Neo Nicola","Neo Rafa","Nicolay Sur","Nicotera Jr.","Pinche Sarah",
  "RODRI JV","Renato del Sur","SENSEI","TANITANITANI","TRAEME LA COPA MESSI",
  "Tim Peine","Tomás el cuñado de Alejo","UncleKoss","Yuri"
))

# Cruces de 8avos → qué partidos de 16avos alimentan cada llave
# Según reglamento FIFA Art 12.7
CRUCES_8AVOS <- list(
  "89"  = c(74, 77),   "90"  = c(73, 75),
  "91"  = c(76, 78),   "92"  = c(79, 80),
  "93"  = c(83, 84),   "94"  = c(81, 82),
  "95"  = c(86, 88),   "96"  = c(85, 87)
)
CRUCES_CUARTOS <- list(
  "97"=c(89,90), "98"=c(93,94), "99"=c(91,92), "100"=c(95,96)
)
CRUCES_SEMIS <- list("101"=c(97,98), "102"=c(99,100))
CRUCES_FINAL <- list("104"=c(101,102))

# Partidos de 16avos con descripción (equipos aún no definidos para mejores terceros)
DESC_16AVOS <- c(
  "73"="2°A vs 2°B",   "74"="1°E vs Mejor 3°",
  "75"="1°F vs 2°C",   "76"="1°C vs 2°F",
  "77"="1°I vs Mejor 3°","78"="2°E vs 2°I",
  "79"="1°A vs Mejor 3°","80"="1°L vs Mejor 3°",
  "81"="1°D vs Mejor 3°","82"="1°G vs Mejor 3°",
  "83"="2°K vs 2°L",   "84"="1°H vs 2°J",
  "85"="1°B vs Mejor 3°","86"="1°J vs 2°H",
  "87"="1°K vs Mejor 3°","88"="2°D vs 2°G"
)

# Horarios FE en hora Uruguay (UTC-3)
HORARIOS_FE <- tribble(
  ~num, ~fecha,        ~hora,
  73,  "2026-06-28", "16:00",  74,  "2026-06-28", "16:00",
  75,  "2026-06-28", "17:30",  76,  "2026-06-28", "22:00",
  77,  "2026-06-29", "16:00",  78,  "2026-06-29", "16:00",
  79,  "2026-06-29", "22:00",  80,  "2026-06-30", "15:00",
  81,  "2026-06-30", "17:00",  82,  "2026-06-30", "21:00",
  83,  "2026-07-01", "20:00",  84,  "2026-07-01", "16:00",
  85,  "2026-07-02", "16:00",  86,  "2026-07-02", "22:00",
  87,  "2026-07-03", "16:00",  88,  "2026-07-03", "22:00",
  89,  "2026-07-04", "20:00",  90,  "2026-07-04", "16:00",
  91,  "2026-07-05", "16:00",  92,  "2026-07-05", "22:00",
  93,  "2026-07-06", "16:00",  94,  "2026-07-06", "22:00",
  95,  "2026-07-07", "16:00",  96,  "2026-07-07", "22:00",
  97,  "2026-07-09", "21:00",  98,  "2026-07-09", "17:00",
  99,  "2026-07-10", "17:00", 100,  "2026-07-11", "21:00",
  101, "2026-07-14", "21:00", 102,  "2026-07-15", "21:00",
  103, "2026-07-18", "16:00", 104,  "2026-07-19", "16:00"
) |> mutate(
  inicio    = as.POSIXct(paste(fecha, hora), tz="America/Montevideo"),
  partido_id = paste0("FE_", num)
)

# ════════════════════════════════════════════════════════════════
# SUPABASE HELPERS
# ════════════════════════════════════════════════════════════════
sb_hdrs <- function() list(
  "apikey"="SUPABASE_KEY_PLACEHOLDER",
  "Authorization"=paste("Bearer","SUPABASE_KEY_PLACEHOLDER"),
  "Content-Type"="application/json","Prefer"="return=representation"
)

make_hdrs <- function(prefer="return=representation") {
  list("apikey"=SUPABASE_KEY,
       "Authorization"=paste("Bearer",SUPABASE_KEY),
       "Content-Type"="application/json",
       "Prefer"=prefer)
}

sb_get <- function(tabla, params="") {
  # PostgREST devuelve como máximo 1000 filas por request. Paginamos con el
  # header Range para traer TODAS las filas (si no, los jugadores cargados
  # después de la fila 1000 quedan invisibles).
  tryCatch({
    page_size <- 1000
    offset <- 0
    acumulado <- list()
    repeat {
      desde <- offset
      hasta <- offset + page_size - 1
      r <- request(paste0(SUPABASE_URL,"/rest/v1/",tabla,params)) |>
        req_headers(!!!make_hdrs(),
                    "Range-Unit"="items",
                    "Range"=paste0(desde,"-",hasta)) |>
        req_perform()
      bloque <- resp_body_json(r, simplifyVector=TRUE)
      if (is.null(bloque) || length(bloque)==0) break
      # Normalizar a data.frame para poder apilar
      bdf <- tryCatch(as.data.frame(bloque), error=function(e) NULL)
      if (is.null(bdf) || nrow(bdf)==0) break
      acumulado[[length(acumulado)+1]] <- bdf
      if (nrow(bdf) < page_size) break  # última página
      offset <- offset + page_size
      if (offset > 100000) break        # tope de seguridad
    }
    if (length(acumulado)==0) return(NULL)
    do.call(rbind, acumulado)
  }, error=function(e) NULL)
}

# Convierte respuesta de sb_get a data.frame con esquema garantizado
sb_to_df <- function(r, empty_df) {
  if (is.null(r) || length(r)==0) return(empty_df)
  df <- tryCatch(as.data.frame(r), error=function(e) empty_df)
  if (nrow(df)==0 || ncol(df)==0) return(empty_df)
  # Aplanar columnas que son listas (pasa cuando Supabase devuelve arrays)
  for (col in names(df)) {
    if (is.list(df[[col]])) {
      df[[col]] <- tryCatch(unlist(df[[col]]), error=function(e) as.character(df[[col]]))
    }
  }
  # Asegurar que las columnas del esquema existen
  for (col in names(empty_df)) {
    if (!(col %in% names(df))) df[[col]] <- NA
  }
  df
}

# ── Cache global compartido entre TODAS las sesiones ──────────────────────────
# La mayoría de los usuarios solo MIRAN datos (ranking, cuadros). Sin cache, cada
# usuario consulta Supabase por su cuenta. Con esto, las lecturas pesadas se hacen
# UNA vez cada TTL segundos y se comparten entre todos, descargando Supabase.
.cache_env <- new.env()
sb_get_cached <- function(tabla, params="", ttl=120) {
  key <- paste0(tabla, "|", params)
  ahora <- as.numeric(Sys.time())
  ent <- get0(key, envir=.cache_env, ifnotfound=NULL)
  if (!is.null(ent) && (ahora - ent$ts) < ttl) return(ent$val)
  val <- sb_get(tabla, params)  # consulta real (paginada)
  assign(key, list(ts=ahora, val=val), envir=.cache_env)
  val
}
# Invalida el cache de una tabla (tras escribir, para ver el cambio al instante)
cache_invalidar <- function(tabla=NULL) {
  if (is.null(tabla)) { rm(list=ls(.cache_env), envir=.cache_env); return(invisible()) }
  for (k in ls(.cache_env)) if (startsWith(k, paste0(tabla,"|"))) rm(list=k, envir=.cache_env)
}

sb_upsert <- function(tabla, body, on_conflict="jugador_id,partido_id") {
  tryCatch({
    request(paste0(SUPABASE_URL,"/rest/v1/",tabla)) |>
      req_headers(!!!make_hdrs(paste0("resolution=merge-duplicates,return=minimal"))) |>
      req_body_raw(toJSON(body, auto_unbox=TRUE)) |>
      req_perform()
  }, error=function(e) NULL)
}

sb_patch <- function(tabla, params, body) {
  tryCatch({
    request(paste0(SUPABASE_URL,"/rest/v1/",tabla,params)) |>
      req_headers(!!!make_hdrs()) |>
      req_method("PATCH") |>
      req_body_raw(toJSON(body, auto_unbox=TRUE)) |>
      req_perform()
  }, error=function(e) NULL)
}

sb_post_single <- function(tabla, body) {
  tryCatch({
    request(paste0(SUPABASE_URL,"/rest/v1/",tabla)) |>
      req_headers(!!!make_hdrs()) |>
      req_body_raw(toJSON(body, auto_unbox=TRUE)) |>
      req_perform()
  }, error=function(e) NULL)
}

# Insert en bloque (todo o nada). Devuelve list(ok=TRUE/FALSE, status, msg)
sb_post_bulk <- function(tabla, rows_list) {
  tryCatch({
    resp <- request(paste0(SUPABASE_URL,"/rest/v1/",tabla)) |>
      req_headers(!!!make_hdrs("return=minimal")) |>
      req_body_raw(toJSON(rows_list, auto_unbox=TRUE, na="null")) |>
      req_error(is_error=function(resp) FALSE) |>
      req_perform()
    status <- resp$status_code
    if (status >= 200 && status < 300) {
      list(ok=TRUE, status=status, msg="")
    } else {
      msg <- tryCatch(rawToChar(resp$body), error=function(e) "")
      list(ok=FALSE, status=status, msg=substr(msg,1,300))
    }
  }, error=function(e) list(ok=FALSE, status=0, msg=conditionMessage(e)))
}

# Determina la ronda de un partido FE según su número
ronda_de_partido <- function(n) {
  n <- as.integer(n)
  if (n>=73 && n<=88) "16avos"
  else if (n>=89 && n<=96) "8avos"
  else if (n>=97 && n<=100) "Cuartos"
  else if (n>=101 && n<=102) "Semis"
  else if (n==103) "3er puesto"
  else if (n==104) "Final"
  else "?"
}

# ════════════════════════════════════════════════════════════════
# UTILIDADES
# ════════════════════════════════════════════════════════════════
normalizar <- function(x) x |> str_to_upper() |> str_squish() |>
  stringi::stri_trans_general("Latin-ASCII")

# Comparación robusta de IDs: jugador_id (texto en Supabase) vs id (entero en
# usuarios). Normaliza ambos lados a texto sin espacios para evitar fallos de
# tipo/formato (p. ej. el jugador 1, donde "1" vs 1 podía no matchear).
id_chr <- function(x) trimws(as.character(x))
mismo_id <- function(jugador_id, id_ref) id_chr(jugador_id) == id_chr(id_ref)

# ─── Mapeo fijo equipo→grupo (sorteo Mundial 2026, no cambia) ───────────────
# Usado por el scraper para asignar el grupo sin depender del HTML de Wikipedia.
EQUIPO_GRUPO <- c(
  "COREA DEL SUR" = "A", "MEXICO" = "A", "REPUBLICA CHECA" = "A", "SUDAFRICA" = "A",
  "BOSNIA Y HERZEGOVINA" = "B", "CANADA" = "B", "CATAR" = "B", "SUIZA" = "B",
  "BRASIL" = "C", "ESCOCIA" = "C", "HAITI" = "C", "MARRUECOS" = "C",
  "AUSTRALIA" = "D", "ESTADOS UNIDOS" = "D", "PARAGUAY" = "D", "TURQUIA" = "D",
  "ALEMANIA" = "E", "COSTA DE MARFIL" = "E", "CURAZAO" = "E", "ECUADOR" = "E",
  "JAPON" = "F", "PAISES BAJOS" = "F", "SUECIA" = "F", "TUNEZ" = "F",
  "BELGICA" = "G", "EGIPTO" = "G", "IRAN" = "G", "NUEVA ZELANDA" = "G",
  "ARABIA SAUDITA" = "H", "CABO VERDE" = "H", "ESPANA" = "H", "URUGUAY" = "H",
  "FRANCIA" = "I", "IRAK" = "I", "NORUEGA" = "I", "SENEGAL" = "I",
  "ARGELIA" = "J", "ARGENTINA" = "J", "AUSTRIA" = "J", "JORDANIA" = "J",
  "COLOMBIA" = "K", "PORTUGAL" = "K", "REPUBLICA DEMOCRATICA DEL CONGO" = "K", "UZBEKISTAN" = "K",
  "CROACIA" = "L", "GHANA" = "L", "INGLATERRA" = "L", "PANAMA" = "L"
)

estandarizar <- function(x) {
  x <- normalizar(x)
  str_replace_all(x, c(
    "BOSNIA-HERZEGOVINA"="BOSNIA Y HERZEGOVINA",
    "CATAR"="QATAR",
    "RD CONGO"="REPUBLICA DEMOCRATICA DEL CONGO"
  ))
}

FIFA_CODES <- c(
  "ALG"="ARGELIA","ARG"="ARGENTINA","AUS"="AUSTRALIA","AUT"="AUSTRIA",
  "BEL"="BELGICA","BIH"="BOSNIA Y HERZEGOVINA","BRA"="BRASIL","CAN"="CANADA",
  "CIV"="COSTA DE MARFIL","COD"="REPUBLICA DEMOCRATICA DEL CONGO",
  "COL"="COLOMBIA","CPV"="CABO VERDE","CRO"="CROACIA","CUW"="CURAZAO",
  "CZE"="REPUBLICA CHECA","ECU"="ECUADOR","EGY"="EGIPTO","ENG"="INGLATERRA",
  "ESP"="ESPANA","FRA"="FRANCIA","GER"="ALEMANIA","GHA"="GHANA","HAI"="HAITI",
  "IRN"="IRAN","IRQ"="IRAK","JOR"="JORDANIA","JPN"="JAPON",
  "KOR"="COREA DEL SUR","KSA"="ARABIA SAUDITA","MAR"="MARRUECOS",
  "MEX"="MEXICO","NED"="PAISES BAJOS","NOR"="NORUEGA","NZL"="NUEVA ZELANDA",
  "PAN"="PANAMA","PAR"="PARAGUAY","POR"="PORTUGAL","QAT"="QATAR",
  "RSA"="SUDAFRICA","SCO"="ESCOCIA","SEN"="SENEGAL","SUI"="SUIZA",
  "SWE"="SUECIA","TUN"="TUNEZ","TUR"="TURQUIA","URU"="URUGUAY",
  "USA"="ESTADOS UNIDOS","UZB"="UZBEKISTAN"
)

codigo_a_nombre <- function(x) ifelse(x %in% names(FIFA_CODES), FIFA_CODES[x], normalizar(x))

calcular_puntos_partido <- function(gl_p, gv_p, gl_r, gv_r) {
  if (any(is.na(c(gl_p,gv_p,gl_r,gv_r)))) return(NA_integer_)
  dp <- gl_p-gv_p; dr <- gl_r-gv_r
  dplyr::case_when(
    gl_p==gl_r & gv_p==gv_r ~ 8L,
    dp==dr & dr!=0           ~ 5L,
    sign(dp)==sign(dr)       ~ 3L,
    TRUE                     ~ 0L
  )
}

parse_fecha_es <- function(x) suppressWarnings(dmy(str_replace_all(str_to_lower(x),
  c("enero"="01","febrero"="02","marzo"="03","abril"="04","mayo"="05",
    "junio"="06","julio"="07","agosto"="08","septiembre"="09",
    "octubre"="10","noviembre"="11","diciembre"="12"))))

partido_bloqueado <- function(pid) {
  p <- HORARIOS_FE |> filter(partido_id==pid)
  if (nrow(p)==0) return(TRUE)
  Sys.time() >= p$inicio[1]
}

# ════════════════════════════════════════════════════════════════
# LÓGICA DE STANDINGS
# ════════════════════════════════════════════════════════════════
calcular_standings_reales <- function(res_raw) {
  fg <- res_raw |>
    mutate(.num_partido = suppressWarnings(as.integer(str_extract(partido_id,"(?<=_)\\d+")))) |>
    filter(!is.na(goles_l),!is.na(goles_v),!is.na(grupo),
           !is.na(.num_partido), .num_partido <= 72) |>  # SOLO fase de grupos
    mutate(pts_l=case_when(goles_l>goles_v~3L,goles_l==goles_v~1L,TRUE~0L),
           pts_v=case_when(goles_v>goles_l~3L,goles_l==goles_v~1L,TRUE~0L),
           eq_l=normalizar(local), eq_v=normalizar(visitante))

  # Totales por equipo (criterios generales)
  totales <- bind_rows(
    fg |> transmute(grupo,equipo=eq_l,pts=pts_l,gf=goles_l,ga=goles_v,dif=goles_l-goles_v),
    fg |> transmute(grupo,equipo=eq_v,pts=pts_v,gf=goles_v,ga=goles_l,dif=goles_v-goles_l)
  ) |>
    group_by(grupo,equipo) |>
    summarise(pts=sum(pts),gf=sum(gf),ga=sum(ga),dif=sum(dif),pj=n(),.groups="drop")

  # Stats head-to-head: para un conjunto de equipos empatados, sus pts/dif/gf
  # SOLO en los partidos jugados ENTRE ELLOS. Devuelve named vectors.
  h2h_stats <- function(equipos_tied, grupo_g) {
    sub <- fg |> filter(grupo==grupo_g, eq_l %in% equipos_tied, eq_v %in% equipos_tied)
    hp <- setNames(rep(0L,length(equipos_tied)), equipos_tied)
    hd <- hp; hg <- hp
    if (nrow(sub)>0) {
      for (i in seq_len(nrow(sub))) {
        l<-sub$eq_l[i]; v<-sub$eq_v[i]; gl<-sub$goles_l[i]; gv<-sub$goles_v[i]
        hp[l]<-hp[l]+sub$pts_l[i]; hp[v]<-hp[v]+sub$pts_v[i]
        hd[l]<-hd[l]+(gl-gv);      hd[v]<-hd[v]+(gv-gl)
        hg[l]<-hg[l]+gl;           hg[v]<-hg[v]+gv
      }
    }
    list(h_pts=hp, h_dif=hd, h_gf=hg)
  }

  # Ordenar cada grupo: 1) puntos; para empatados en puntos, un único orden
  # jerárquico por (h2h pts, h2h dif, h2h gf, dif general, gf general).
  # Cuando el h2h no separa (idénticos), pasa naturalmente a dif/gf general.
  resultado <- lapply(split(totales, totales$grupo), function(g) {
    grupo_g <- g$grupo[1]
    orden_final <- character(0)
    for (p in sort(unique(g$pts), decreasing=TRUE)) {
      tied <- g |> filter(pts==p)
      if (nrow(tied)==1) {
        orden_final <- c(orden_final, tied$equipo)
      } else {
        hh <- h2h_stats(tied$equipo, grupo_g)
        tied <- tied |>
          mutate(h_pts=hh$h_pts[equipo], h_dif=hh$h_dif[equipo], h_gf=hh$h_gf[equipo]) |>
          arrange(desc(h_pts), desc(h_dif), desc(h_gf), desc(dif), desc(gf))
        orden_final <- c(orden_final, tied$equipo)
      }
    }
    g$pos_grupo <- match(g$equipo, orden_final)
    g |> arrange(pos_grupo)
  })

  bind_rows(resultado)
}

mejores_terceros_fn <- function(st) {
  st |> filter(pos_grupo==3) |>
    arrange(desc(pts),desc(dif),desc(gf)) |> slice_head(n=8)
}

clasificados_reales_fn <- function(st) {
  primeros_segundos <- st |> filter(pos_grupo<=2) |> mutate(pos_final=as.character(pos_grupo))
  terceros <- mejores_terceros_fn(st) |> mutate(pos_final="3T")
  bind_rows(primeros_segundos, terceros) |> select(equipo, pos_final, grupo)
}

# Obtener equipo en posición de un grupo
get_eq <- function(st, grupo_letra, pos) {
  e <- st |> filter(grupo==paste0("Grupo ",grupo_letra), pos_grupo==pos) |> pull(equipo)
  if (length(e)>0) e[1] else paste0(pos,"° Gr.",grupo_letra)
}

# Obtener los dos equipos de un partido de 16avos
equipos_16avos <- function(num, st) {
  terceros_cl <- mejores_terceros_fn(st) |>
    mutate(letra=str_extract(grupo,"[A-L]$")) |> pull(letra) |> sort()

  switch(as.character(num),
    "73"=c(get_eq(st,"A",2), get_eq(st,"B",2)),
    "74"=c(get_eq(st,"E",1), if(length(terceros_cl)>=1) get_eq(st,terceros_cl[which(ANEXO_C_SIMPLE(terceros_cl,"1E")==terceros_cl)],3) else "Mejor 3°"),
    "75"=c(get_eq(st,"F",1), get_eq(st,"C",2)),
    "76"=c(get_eq(st,"C",1), get_eq(st,"F",2)),
    "77"=c(get_eq(st,"I",1), paste0("Mejor 3° CDFGH")),
    "78"=c(get_eq(st,"E",2), get_eq(st,"I",2)),
    "79"=c(get_eq(st,"A",1), paste0("Mejor 3° CEFHI")),
    "80"=c(get_eq(st,"L",1), paste0("Mejor 3° EHIJK")),
    "81"=c(get_eq(st,"D",1), paste0("Mejor 3° BEFIJ")),
    "82"=c(get_eq(st,"G",1), paste0("Mejor 3° AEHIJ")),
    "83"=c(get_eq(st,"K",2), get_eq(st,"L",2)),
    "84"=c(get_eq(st,"H",1), get_eq(st,"J",2)),
    "85"=c(get_eq(st,"B",1), paste0("Mejor 3° EFGIJ")),
    "86"=c(get_eq(st,"J",1), get_eq(st,"H",2)),
    "87"=c(get_eq(st,"K",1), paste0("Mejor 3° DEIJL")),
    "88"=c(get_eq(st,"D",2), get_eq(st,"G",2)),
    c("Por definir","Por definir")
  )
}

ANEXO_C_SIMPLE <- function(terceros, posicion) {
  # Simplificado para los casos más comunes
  terceros[1]
}

# ════════════════════════════════════════════════════════════════
# CALCULOS DE PUNTOS
# ════════════════════════════════════════════════════════════════
calcular_pts_standings_jugador <- function(pred_st, st) {
  clas <- clasificados_reales_fn(st)

  pred_long <- pred_st |>
    mutate(equipo_norm=codigo_a_nombre(Equipo), pts_num=as.integer(PTS)) |>
    group_by(participante_id, participante_nombre, Grupo) |>
    arrange(desc(pts_num),.by_group=TRUE) |>
    mutate(pos_pred=row_number()) |> ungroup()

  pred_long |>
    group_by(participante_id, participante_nombre) |>
    group_modify(function(df,keys) {
      clasif <- df |> filter(pos_pred<=2) |> mutate(pos_pred_label=as.character(pos_pred))
      terc   <- df |> filter(pos_pred==3) |> arrange(desc(pts_num)) |>
        slice_head(n=8) |> mutate(pos_pred_label="3T")
      bind_rows(clasif,terc)
    }) |> ungroup() |>
    left_join(clas |> rename(pos_real=pos_final), by=c("equipo_norm"="equipo")) |>
    mutate(
      clasifico    = !is.na(pos_real),
      pts_standing = case_when(!clasifico~0L, pos_pred_label==pos_real~10L, clasifico~5L, TRUE~0L)
    )
}

calcular_pts_fg <- function(pred_p, res_raw) {
  pred_std <- pred_p |>
    mutate(partido=paste(Grupo,estandarizar(Local),estandarizar(Visitante),sep="_"),
           jugador=participante_nombre, jugador_id=participante_id,
           gl_pred=Goles_L, gv_pred=Goles_V) |>
    select(partido,jugador,jugador_id,gl_pred,gv_pred)
  res_std <- res_raw |> filter(!is.na(goles_l),!is.na(goles_v)) |>
    mutate(partido=paste(grupo,estandarizar(local),estandarizar(visitante),sep="_"),
           gl_real=goles_l,gv_real=goles_v) |>
    select(partido,gl_real,gv_real,fecha_date,fecha_label)
  pred_std |> inner_join(res_std,by="partido") |>
    rowwise() |>
    mutate(puntos=calcular_puntos_partido(gl_pred,gv_pred,gl_real,gv_real),
           tipo=dplyr::case_when(puntos==8L~"exacto",puntos==5L~"diferencia",
                                 puntos==3L~"resultado",TRUE~"fallo")) |>
    ungroup()
}

# Puntos de picks eliminatorios (8avos en adelante, 20/9/0)
# ════════════════════════════════════════════════════════════════
# PUNTOS DEL CUADRO FE (basado en cruces, no en ganadores)
# ════════════════════════════════════════════════════════════════
# Cruces fuente de cada partido FE
CRUCES_FE <- list(
  "89"=c(74,77),"90"=c(73,75),"91"=c(76,78),"92"=c(79,80),
  "93"=c(83,84),"94"=c(81,82),"95"=c(86,88),"96"=c(85,87),
  "97"=c(89,90),"98"=c(93,94),"99"=c(91,92),"100"=c(95,96),
  "101"=c(97,98),"102"=c(99,100),"103"=c(101,102),"104"=c(101,102)
)

# Pick de un jugador para un partido (el equipo que apostó como ganador)
pick_de <- function(picks_jugador, n) {
  if (is.null(picks_jugador)||nrow(picks_jugador)==0) return(NA_character_)
  r <- picks_jugador |> filter(as.integer(partido_num)==as.integer(n)) |> pull(equipo_apostado)
  if (length(r)>0 && !is.na(r[1]) && r[1]!="") r[1] else NA_character_
}

# Cruce PREDICHO por el jugador para un partido n (los 2 equipos que predijo enfrentándose)
# Se reconstruye con los ganadores predichos de los 2 partidos fuente.
# Caso especial M103 (3er puesto): los PERDEDORES predichos de las semis.
cruce_predicho <- function(picks_jugador, n) {
  n <- as.integer(n)
  if (n==103) {
    # Perdedor predicho de cada semi: el equipo del cruce predicho de la semi
    # que el jugador NO eligió como ganador
    perdedor_semi <- function(psemi) {
      cr <- cruce_predicho(picks_jugador, psemi)
      gan <- pick_de(picks_jugador, psemi)
      if (any(is.na(cr)) || is.na(gan)) return(NA_character_)
      perd <- cr[cr != gan]
      if (length(perd)>0) perd[1] else NA_character_
    }
    return(c(perdedor_semi(101), perdedor_semi(102)))
  }
  fuentes <- CRUCES_FE[[as.character(n)]]
  if (is.null(fuentes)) return(c(NA_character_, NA_character_))
  c(pick_de(picks_jugador, fuentes[1]), pick_de(picks_jugador, fuentes[2]))
}

# Cruce REAL de un partido n: los 2 equipos (local/visitante del Excel) si están definidos
cruce_real <- function(res_excel, n) {
  if (is.null(res_excel)||nrow(res_excel)==0) return(c(NA_character_, NA_character_))
  row <- res_excel |> filter(as.integer(num)==as.integer(n))
  if (nrow(row)==0) return(c(NA_character_, NA_character_))
  a <- row$local[1]; b <- row$visitante[1]
  es_def <- function(x) !is.na(x) && trimws(as.character(x))!="" &&
    !grepl("(?i)por definir|mejor 3|ganador|2°|1°|G\\.M|^W ", as.character(x), perl=TRUE)
  if (es_def(a) && es_def(b)) c(a, b) else c(NA_character_, NA_character_)
}

# Puntos de un partido del cuadro: cuántos equipos del cruce predicho están en el real
# 2 coinciden = 20, 1 = 9, 0 = 0. NA si el cruce real aún no está definido.
puntos_partido_cuadro <- function(predicho, real) {
  if (any(is.na(real))) return(NA_integer_)        # partido aún sin cruce definido
  if (all(is.na(predicho))) return(0L)             # jugador no apostó nada útil
  pred <- predicho[!is.na(predicho)]
  aciertos <- sum(pred %in% real)
  if (aciertos>=2) 20L else if (aciertos==1) 9L else 0L
}

# Fecha en que un partido del cuadro queda "definido" = la del más tardío de sus partidos madre
# (el cruce recién se conoce cuando se jugaron ambos partidos fuente)
fecha_cuadro_partido <- function(res_excel, n) {
  n <- as.integer(n)
  fuentes <- CRUCES_FE[[as.character(n)]]
  if (is.null(fuentes)) return(as.Date(NA_character_))
  fechas <- vapply(fuentes, function(src) {
    row <- res_excel |> filter(as.integer(num)==as.integer(src))
    if (nrow(row)>0 && !is.na(row$fecha_date[1])) as.character(row$fecha_date[1]) else NA_character_
  }, character(1))
  fechas <- as.Date(fechas[!is.na(fechas)])
  if (length(fechas)==0) return(as.Date(NA_character_))
  as.Date(max(fechas, na.rm=TRUE))  # el más tardío de los partidos madre
}

# Detalle del cuadro de un jugador: por partido, cruce predicho, real y puntos
detalle_cuadro_jugador <- function(picks_jugador, res_excel) {
  partidos <- c(89:104)
  filas <- lapply(partidos, function(n) {
    pred <- cruce_predicho(picks_jugador, n)
    real <- cruce_real(res_excel, n)
    pts  <- puntos_partido_cuadro(pred, real)
    fp   <- fecha_cuadro_partido(res_excel, n)
    data.frame(
      partido_num = n,
      ronda       = ronda_de_partido(n),
      fecha_date  = as.Date(fp),
      pred_a = pred[1], pred_b = pred[2],
      real_a = real[1], real_b = real[2],
      puntos = pts,
      stringsAsFactors = FALSE
    )
  })
  bind_rows(filas)
}

# Total de puntos del cuadro de un jugador
calcular_pts_cuadro <- function(picks_jugador, res_excel) {
  if (is.null(picks_jugador)||nrow(picks_jugador)==0) return(0L)
  if (!("partido_num" %in% names(picks_jugador))) return(0L)
  picks_jugador$partido_num <- suppressWarnings(as.integer(picks_jugador$partido_num))
  det <- detalle_cuadro_jugador(picks_jugador, res_excel)
  sum(det$puntos, na.rm=TRUE)
}

# ════════════════════════════════════════════════════════════════
# MEDALLAS
# ════════════════════════════════════════════════════════════════
calcular_medallas <- function(puntos_fg_df, fecha_sel=NULL) {
  df <- puntos_fg_df
  if (!is.null(fecha_sel)) df <- df |> filter(fecha_date==as.Date(fecha_sel))
  if (nrow(df)==0) return(list())

  medallas <- list()

  # Profeta: más exactos
  profeta <- df |> filter(tipo=="exacto") |> count(jugador) |>
    slice_max(n,n=1,with_ties=FALSE)
  if (nrow(profeta)>0) medallas[["🔮 Profeta"]] <- paste0(profeta$jugador[1]," (",profeta$n[1]," exactos)")

  # Kamikaze: más goles apostados en promedio
  kamikaze <- df |> group_by(jugador) |>
    summarise(media_goles=mean(gl_pred+gv_pred,na.rm=TRUE),.groups="drop") |>
    slice_max(media_goles,n=1,with_ties=FALSE)
  if (nrow(kamikaze)>0) medallas[["🎰 Kamikaze"]] <- paste0(kamikaze$jugador[1],
    " (",round(kamikaze$media_goles[1],1)," goles/partido)")

  # Conservador: menos goles apostados
  conserv <- df |> group_by(jugador) |>
    summarise(media_goles=mean(gl_pred+gv_pred,na.rm=TRUE),.groups="drop") |>
    slice_min(media_goles,n=1,with_ties=FALSE)
  if (nrow(conserv)>0) medallas[["🛡️ Conservador"]] <- paste0(conserv$jugador[1],
    " (",round(conserv$media_goles[1],1)," goles/partido)")

  # Goleador Fantasma: eliminado (se superpone con Kamikaze)
  medallas
}

# Predicción más osada acertada
pred_osada <- function(puntos_fg_df, fecha_sel=NULL) {
  df <- puntos_fg_df
  if (!is.null(fecha_sel)) df <- df |> filter(fecha_date==as.Date(fecha_sel))
  df |> filter(tipo %in% c("exacto","diferencia")) |>
    mutate(osadia=gl_pred+gv_pred+(abs(gl_pred-gv_pred)*2)) |>
    slice_max(osadia,n=1,with_ties=FALSE)
}

# ════════════════════════════════════════════════════════════════
# CSS
# ════════════════════════════════════════════════════════════════
css_app <- "
body{background:#f8f9fa;font-family:'Segoe UI',sans-serif;}
.ph{background:linear-gradient(135deg,#1a1a2e,#0f3460);color:white;
    padding:20px 24px;display:flex;align-items:center;gap:20px;}
.ph img{height:80px;border-radius:8px;}
.ph h1{margin:0;font-size:2em;font-weight:900;color:#f5c518;text-transform:uppercase;}
.ph .sub{font-size:.85em;color:#aaa;}.ph .ts{font-size:.75em;color:#888;margin-top:4px;}
.nav-tabs{border-bottom:3px solid #0f3460;background:white;}
.nav-tabs>li>a{color:#333;font-weight:600;border-radius:0!important;padding:12px 16px;}
.nav-tabs>li.active>a{color:#f5c518!important;border-bottom:3px solid #f5c518!important;background:#1a1a2e!important;}
.tab-content{background:white;padding:20px;border:1px solid #dee2e6;}
.login-box{max-width:400px;margin:60px auto;background:white;border-radius:16px;
           padding:32px;box-shadow:0 4px 24px rgba(0,0,0,.12);}
.grupo-card{background:white;border:1px solid #dee2e6;border-radius:10px;
            padding:12px 14px;margin-bottom:10px;}
.grupo-title{font-weight:800;color:#0f3460;font-size:.85em;text-transform:uppercase;
             letter-spacing:1px;margin-bottom:8px;border-bottom:2px solid #f5c518;padding-bottom:4px;}
.eq-row{display:flex;align-items:center;padding:4px 0;font-size:.88em;}
.eq-clasificado{background:#e8f5e9;border-radius:4px;font-weight:600;}
.pts-10{color:#1b7837;font-weight:800;}
.pts-5{color:#5aae61;font-weight:700;}
.pts-0{color:#c0392b;font-weight:600;}
.bracket-container{overflow-x:auto;-webkit-overflow-scrolling:touch;padding:12px 0;}
.bteam{padding:4px 7px;font-size:.8em;color:#333;}
.bteam-won{font-weight:700;color:#1b7837;}
.bpick-cell{padding:6px 7px;display:flex;flex-direction:column;justify-content:center;}
.bpick-final{background:#fffbea!important;}
.bpick-num{font-size:.65em;color:#aaa;margin-bottom:3px;}
.bpick-chosen{font-weight:700;font-size:.88em;color:#0f3460;}
.bpick-win{color:#1b7837!important;}
.bpick-empty{color:#aaa;font-size:.82em;}
.b16-wrap{font-size:.8em;}
.llave-pts{font-size:.75em;margin-top:4px;}
.pts-20{color:#1b7837;font-weight:800;} .pts-9{color:#f5a623;font-weight:700;} .pts-0{color:#c0392b;font-weight:600;}
.pts-20{color:#1b7837;font-weight:800;} .pts-9{color:#5aae61;font-weight:700;}
.stat-box{display:inline-flex;flex-direction:column;align-items:center;
          border-radius:10px;padding:12px 16px;color:white;min-width:120px;margin:6px;}
.medalla-chip{display:inline-block;background:#f5c518;color:#1a1a2e;
              border-radius:20px;padding:4px 12px;margin:3px;font-size:.82em;font-weight:700;}
.evolucion-hint{background:#fff8e1;border-left:4px solid #f5c518;
                padding:10px 16px;margin-bottom:16px;border-radius:0 6px 6px 0;font-size:.88em;color:#555;}
.partido-card{background:white;border:1px solid #dee2e6;border-radius:10px;
              padding:12px 14px;margin-bottom:8px;display:flex;align-items:center;
              flex-wrap:wrap;gap:8px;}
.partido-locked{background:#f5f5f5;opacity:.75;}
.ronda-hdr{font-size:.8em;font-weight:700;color:#0f3460;text-transform:uppercase;
           letter-spacing:1px;margin:20px 0 8px;border-bottom:2px solid #f5c518;padding-bottom:4px;}
.accordion-btn{background:#f0f4ff;border:1px solid #dee2e6;border-radius:6px;
               width:100%;text-align:left;padding:10px 14px;cursor:pointer;
               font-weight:600;color:#0f3460;margin-bottom:8px;}
"

# ════════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(
    tags$title("PENCÓN-CACAF FE"),
    tags$style(HTML(css_app)),
    tags$script(HTML("
      $(document).on('click','.accordion-btn',function(){
        var target=$(this).data('target');
        $('#'+target).slideToggle(200);
        var icon=$(this).find('.acc-icon');
        icon.text(icon.text()=='+' ? '−' : '+');
      });

    "))
  ),
  tags$div(class="ph",
    tags$img(src=LOGO_URL),
    tags$div(
      tags$h1("El PENCÓN-CACAF"),
      tags$div(class="sub","Copa Mundial FIFA 2026 — Fase Eliminatoria"),
      uiOutput("header_info"),
      uiOutput("scrape_info")
    )
  ),
  uiOutput("main_ui")
)

# ════════════════════════════════════════════════════════════════
# SERVER
# ════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  usuario   <- reactiveVal(NULL)
  err_login <- reactiveVal("")

  # ── Datos base cacheados (se recargan cada 10 minutos) ───────
  datos_base <- reactivePoll(
    intervalMillis = 15 * 60 * 1000, session = session,  # scraping cada 15 min
    checkFunc = function() Sys.time(),
    valueFunc = function() {
      url_p  <- "https://raw.githubusercontent.com/agonzalezorge/PENCON-CACAF/main/predicciones_partidos.rds"
      url_st <- "https://raw.githubusercontent.com/agonzalezorge/PENCON-CACAF/main/predicciones_standings.rds"
      url_pr <- "https://raw.githubusercontent.com/agonzalezorge/PENCON-CACAF/main/predicciones_premios.rds"
      pred_p  <- readRDS(url(url_p,  method="libcurl"))
      pred_st <- readRDS(url(url_st, method="libcurl"))
      pred_pr <- tryCatch(readRDS(url(url_pr, method="libcurl")), error=function(e) NULL)
      # Resultados: SCRAPING de Wikipedia (en vez del Excel)
      fuente <- "Wikipedia"
      res <- tryCatch(
        scrape_resultados_wiki() |>
          mutate(fecha_date=parse_fecha_es(fecha), fecha_label=format(fecha_date,"%d/%m")),
        error=function(e) {
          # Fallback: si el scraping falla, intentar el Excel de GitHub
          fuente <<- "Excel (respaldo)"
          url_r <- "https://raw.githubusercontent.com/agonzalezorge/PENCON-CACAF/main/partidos_wc2026.xlsx"
          tmp <- tempfile(fileext=".xlsx"); download.file(url_r,tmp,mode="wb",quiet=TRUE)
          read_excel(tmp) |>
            mutate(fecha_date=parse_fecha_es(fecha), fecha_label=format(fecha_date,"%d/%m"))
        })
      list(pred_p=pred_p, pred_st=pred_st, pred_pr=pred_pr, res=res,
           scrape_ts=Sys.time(), scrape_fuente=fuente)
    }
  )

  # ── Datos Supabase (poll cada 5 minutos) ──────────────────────
  cuadro_refresh <- reactiveVal(0)

  picks_cuadro_db <- reactive({
    cuadro_refresh()             # se recarga al enviar el cuadro
    invalidateLater(15*60*1000)  # y cada 15 minutos (reduce carga a Supabase)
    empty <- data.frame(jugador_id=character(), partido_num=integer(),
                        equipo_apostado=character(), created_at=character(),
                        stringsAsFactors=FALSE)
    r <- sb_get_cached("picks_eliminatorios","?select=jugador_id,partido_num,equipo_apostado,created_at", ttl=120)
    df <- sb_to_df(r, empty)
    if (nrow(df)>0) df$jugador_id <- id_chr(df$jugador_id)  # id siempre texto normalizado
    df$partido_num <- suppressWarnings(as.integer(df$partido_num))
    df
  })

  resultados_refresh <- reactiveVal(0)

  resultados_fe_db <- reactive({
    resultados_refresh()           # se recarga cuando se ingresa un resultado
    invalidateLater(15*60*1000)    # y cada 15 minutos (reduce carga a Supabase)
    empty <- data.frame(jugador_id=character(), partido_id=character(),
                        partido_num=integer(), goles_local=integer(),
                        goles_visitante=integer(), equipo_ganador=character(),
                        created_at=character(), stringsAsFactors=FALSE)
    r <- sb_get_cached("apuestas_fe","?select=jugador_id,partido_id,goles_local,goles_visitante,equipo_ganador,created_at&order=created_at.desc", ttl=120)
    df <- sb_to_df(r, empty)
    if (nrow(df)>0) df$jugador_id <- id_chr(df$jugador_id)  # id siempre texto normalizado
    df$partido_num <- suppressWarnings(as.integer(str_extract(as.character(df$partido_id),"(?<=_)\\d+")))
    df
  })

  st_reales    <- reactive({ calcular_standings_reales(datos_base()$res) })
  pts_fg_rv    <- reactive({ calcular_pts_fg(datos_base()$pred_p, datos_base()$res) })
  pts_st_rv    <- reactive({ calcular_pts_standings_jugador(datos_base()$pred_st, st_reales()) })

  # Puntos FE por jugador y fecha (resultados FE + cuadro FE). Reutilizable.
  pts_fe_por_fecha_rv <- reactive({
    res_excel <- partidos_fe_excel()
    pfe_con_res <- res_excel |> filter(!is.na(goles_l))
    rfe_all <- resultados_fe_db()
    usuarios_x <- tryCatch(as.data.frame(sb_get_cached("usuarios","?select=id,nombre", ttl=600)), error=function(e) data.frame())
    vacio <- data.frame(jugador=character(), fecha_date=as.Date(character()), pts_dia=numeric())

    # Resultados FE por fecha
    df_res <- if (nrow(rfe_all)>0 && nrow(pfe_con_res)>0 && nrow(usuarios_x)>0) {
      rfe_all |>
        group_by(jugador_id, partido_id) |>
        slice_max(created_at, n=1, with_ties=FALSE) |> ungroup() |>
        mutate(num=as.integer(str_extract(partido_id,"(?<=_)\\d+"))) |>
        inner_join(pfe_con_res |> select(num, goles_l, goles_v, fecha_date), by="num") |>
        rowwise() |>
        mutate(pts=calcular_puntos_partido(goles_local,goles_visitante,goles_l,goles_v)) |>
        ungroup() |>
        left_join(usuarios_x |> mutate(jugador_id=id_chr(id)) |> rename(jugador=nombre) |> select(jugador_id,jugador), by="jugador_id") |>
        filter(!is.na(jugador)) |>
        group_by(jugador, fecha_date) |>
        summarise(pts_dia=sum(pts,na.rm=TRUE), .groups="drop")
    } else vacio

    # Cuadro FE por fecha
    picks_all <- picks_cuadro_db()
    df_cua <- if (nrow(picks_all)>0 && "partido_num" %in% names(picks_all) && nrow(usuarios_x)>0) {
      partes <- lapply(seq_len(nrow(usuarios_x)), function(i) {
        jid <- usuarios_x$id[i]; jn <- usuarios_x$nombre[i]
        mp <- picks_all |> filter(mismo_id(jugador_id, jid)) |>
          mutate(partido_num=suppressWarnings(as.integer(partido_num)))
        if (nrow(mp)==0) return(NULL)
        det <- detalle_cuadro_jugador(mp, res_excel) |>
          filter(!is.na(puntos), !is.na(fecha_date))
        if (nrow(det)==0) return(NULL)
        det |> group_by(fecha_date) |>
          summarise(pts_dia=sum(puntos,na.rm=TRUE), .groups="drop") |>
          mutate(jugador=jn)
      })
      partes <- bind_rows(partes[!sapply(partes, is.null)])
      if (nrow(partes)==0 || !("fecha_date" %in% names(partes))) vacio else partes
    } else vacio

    df_res$fecha_date <- as.Date(df_res$fecha_date)
    df_cua$fecha_date <- as.Date(df_cua$fecha_date)
    bind_rows(df_res, df_cua) |>
      filter(!is.na(fecha_date)) |>
      group_by(jugador, fecha_date) |>
      summarise(pts_dia=sum(pts_dia,na.rm=TRUE), .groups="drop")
  })

  # Acumulado TOTAL por jugador y fecha (misma lógica que el gráfico "Total").
  # Devuelve jugador, fecha_date, pts_acum. Fuente única para gráfico y evolución del ranking.
  acumulado_total_rv <- reactive({
    fecha_standings <- as.Date("2026-06-27")
    fg_dia <- pts_fg_rv() |> filter(!is.na(fecha_date)) |>
      group_by(jugador, fecha_date) |>
      summarise(pts_d=sum(puntos,na.rm=TRUE), .groups="drop")
    st_dia <- pts_st_rv() |>
      group_by(participante_nombre) |>
      summarise(pts_d=sum(pts_standing,na.rm=TRUE), .groups="drop") |>
      rename(jugador=participante_nombre) |>
      mutate(fecha_date=fecha_standings)
    fe_dia <- pts_fe_por_fecha_rv() |> rename(pts_d=pts_dia)
    bind_rows(fg_dia, st_dia, fe_dia) |>
      filter(!is.na(fecha_date)) |>
      arrange(jugador, fecha_date) |>
      group_by(jugador) |>
      mutate(pts_acum=cumsum(pts_d)) |> ungroup() |>
      # un solo punto por fecha: el acumulado final del día
      group_by(jugador, fecha_date) |>
      summarise(pts_acum=max(pts_acum), .groups="drop")
  })

  # Partidos FE desde Excel (con local/visitante ya cargados por el admin)
  partidos_fe_excel <- reactive({
    df <- datos_base()$res |>
      mutate(num=as.integer(str_extract(partido_id,"(?<=_)\\d+"))) |>
      filter(num > 72) |>
      select(num, partido_id, fecha_date, fecha_label, local, visitante, goles_l, goles_v) |>
      arrange(num)
    df
  })

  # ¿Un equipo está realmente definido? (no vacío, NA, ni placeholder)
  equipo_definido <- function(x) {
    if (length(x)==0) return(FALSE)
    x <- x[1]
    !is.na(x) && trimws(as.character(x))!="" &&
      !grepl("(?i)por definir|mejor 3|ganador|2°|1°|G\\.M|^W ", as.character(x), perl=TRUE)
  }

  # Partidos disponibles para ingresar resultado: ambos rivales definidos en el Excel
  partidos_disponibles <- reactive({
    pfe <- partidos_fe_excel()
    if (nrow(pfe)==0) return(integer(0))
    disp <- sapply(seq_len(nrow(pfe)), function(i)
      equipo_definido(pfe$local[i]) && equipo_definido(pfe$visitante[i]))
    pfe$num[disp]
  })

  # ── Fases FE: definición, rangos y estado ──────────────────────
  FASES_FE <- list(
    list(nombre="16avos",     min=73,  max=88),
    list(nombre="8avos",      min=89,  max=96),
    list(nombre="Cuartos",    min=97,  max=100),
    list(nombre="Semis",      min=101, max=102),
    list(nombre="3er puesto", min=103, max=103),
    list(nombre="Final",      min=104, max=104)
  )

  # Números de partido de una fase
  nums_de_fase <- function(fase) seq(fase$min, fase$max)

  # ¿Están TODOS los partidos de la fase con ambos rivales definidos?
  fase_completa <- function(fase, pfe) {
    nums <- nums_de_fase(fase)
    all(sapply(nums, function(n){
      row <- pfe |> filter(num==n)
      nrow(row)>0 && equipo_definido(row$local[1]) && equipo_definido(row$visitante[1])
    }))
  }

  # Fecha de inicio de una fase = fecha del primer partido (mínima del rango), del Excel
  fecha_inicio_fase <- function(fase, pfe) {
    nums <- nums_de_fase(fase)
    fechas <- pfe |> filter(num %in% nums, !is.na(fecha_date)) |> pull(fecha_date)
    if (length(fechas)==0) return(as.Date(NA))
    min(as.Date(fechas), na.rm=TRUE)
  }

  # Fase siguiente (para mostrar su fecha de inicio en el cartel)
  fase_siguiente <- function(idx) if (idx < length(FASES_FE)) FASES_FE[[idx+1]] else NULL

  # ¿El jugador habilitó acceso completo?
  # Condición: enviaron cuadro FE Y han ingresado resultados para todos los partidos FE disponibles
  jugador_habilitado <- reactive({
    u <- usuario()
    if (is.null(u)) return(FALSE)
    # 1. Cuadro enviado
    picks_db <- picks_cuadro_db()
    cuadro_ok <- nrow(picks_db)>0 && "partido_num" %in% names(picks_db) &&
      102L %in% suppressWarnings(as.integer(picks_db$partido_num[mismo_id(picks_db$jugador_id, u$id)]))
    if (!cuadro_ok) return(FALSE)
    # 2. Resultados FE: por cada fase COMPLETA, todos sus partidos deben estar ingresados
    pfe <- partidos_fe_excel()
    rfe <- resultados_fe_db()
    mis_rfe <- if(nrow(rfe)>0 && "partido_num" %in% names(rfe))
      rfe$partido_num[mismo_id(rfe$jugador_id, u$id)] else integer(0)
    for (fase in FASES_FE) {
      if (fase_completa(fase, pfe)) {
        nums <- nums_de_fase(fase)
        if (!all(nums %in% mis_rfe)) return(FALSE)  # fase lista pero no ingresada
      }
    }
    TRUE
  })

  # ── Helpers globales: ganador real de un partido FE ────────────
  # Lee del Excel (goles + local/visitante) — fuente de verdad
  get_gan_real_global <- function(n) {
    pfe <- partidos_fe_excel()
    row <- pfe |> filter(num==as.integer(n))
    if (nrow(row)==0) return(NA)
    gl <- row$goles_l[1]; gv <- row$goles_v[1]
    if (is.na(gl) || is.na(gv)) return(NA)
    if (gl > gv) return(row$local[1])
    if (gv > gl) return(row$visitante[1])
    NA  # empate
  }

  # Pick actual del jugador para un partido n
  # Prioridad: input (en proceso) > Supabase (guardado)
  get_mi_pick_global <- function(n) {
    u <- usuario()
    if (is.null(u)) return("")
    inp <- input[[paste0("pick_",n)]]
    if (!is.null(inp) && inp!="") return(inp)
    # Lectura de la base AISLADA: que el poll de Supabase no re-dispare la cascada
    picks_db <- isolate(picks_cuadro_db())
    if (nrow(picks_db)==0 || !("partido_num" %in% names(picks_db))) return("")
    r <- picks_db |> filter(mismo_id(jugador_id, u$id),
                            as.integer(partido_num)==as.integer(n)) |>
         pull(equipo_apostado)
    if (length(r)>0 && !is.na(r[1])) r[1] else ""
  }

  # Equipos del partido n (desde Excel o fallback a standings)
  equipos_de_partido_global <- function(n) {
    pfe <- isolate(partidos_fe_excel())
    row <- pfe |> filter(num==as.integer(n))
    eq_a <- if(nrow(row)>0 && !is.na(row$local[1]) && row$local[1]!="") row$local[1] else ""
    eq_b <- if(nrow(row)>0 && !is.na(row$visitante[1]) && row$visitante[1]!="") row$visitante[1] else ""
    if(eq_a==""||eq_b=="") {
      fb <- tryCatch(equipos_16avos(as.integer(n), isolate(st_reales())), error=function(e) c("",""))
      if(eq_a=="") eq_a <- fb[1]
      if(eq_b=="") eq_b <- if(length(fb)>1) fb[2] else ""
    }
    c(eq_a, eq_b)
  }

  # Para un partido fuente n, devuelve los 2 equipos posibles según el estado actual
  equipos_posibles_global <- function(n) {
    # El cuadro es PURAMENTE PREDICTIVO: no depende de resultados reales.
    # (Si dependiera, cada actualización del scraping movería las opciones solo.)
    if (n>=73 && n<=88) return(equipos_de_partido_global(n))
    # Para 8avos+: los 2 picks que el jugador hizo en sus partidos fuente
    ant <- if(n>=89 && n<=96) CRUCES_8AVOS[[as.character(n)]]
           else if(n>=97 && n<=100) CRUCES_CUARTOS[[as.character(n)]]
           else if(n>=101 && n<=102) CRUCES_SEMIS[[as.character(n)]]
           else return(c("",""))
    get_one <- function(src) {
      p <- get_mi_pick_global(src)
      if (p!="") return(p)
      ""
    }
    c(get_one(ant[1]), get_one(ant[2]))
  }

  # ── Ranking ──────────────────────────────────────────────────
  ranking_rv <- reactive({
    pts_p <- pts_fg_rv() |> group_by(jugador,jugador_id) |>
      summarise(pts_fg=sum(puntos,na.rm=TRUE),.groups="drop")
    pts_s <- pts_st_rv() |> group_by(participante_nombre,participante_id) |>
      summarise(pts_st=sum(pts_standing,na.rm=TRUE),.groups="drop") |>
      rename(jugador=participante_nombre,jugador_id=participante_id)

    # Puntos cuadro eliminatorio por jugador
    picks_db <- picks_cuadro_db()
    res_excel <- partidos_fe_excel()
    usuarios_all <- tryCatch(as.data.frame(sb_get_cached("usuarios","?select=id,nombre", ttl=600)), error=function(e) data.frame())
    pts_cuadro_list <- lapply(JUGADORES_LISTA, function(jnombre) {
      jid <- if(nrow(usuarios_all)>0) usuarios_all$id[usuarios_all$nombre==jnombre] else character(0)
      if (length(jid)==0) return(data.frame(jugador=jnombre,pts_cuadro=0L))
      jid <- jid[1]
      mis_picks <- if (nrow(picks_db)>0 && "partido_num" %in% names(picks_db)) {
        picks_db |> filter(mismo_id(jugador_id, jid)) |>
          mutate(partido_num=suppressWarnings(as.integer(partido_num)))
      } else data.frame(jugador_id=character(), partido_num=integer(), equipo_apostado=character(), created_at=character())
      pts <- calcular_pts_cuadro(mis_picks, res_excel)
      data.frame(jugador=jnombre, pts_cuadro=pts, stringsAsFactors=FALSE)
    })
    pts_cuadro <- bind_rows(pts_cuadro_list)

    # Puntos de RESULTADOS FE por jugador (apuestas vs goles reales del Excel)
    rfe_all <- resultados_fe_db()
    pfe_con_res <- res_excel |> filter(!is.na(goles_l))
    pts_res_fe <- if (nrow(rfe_all)>0 && nrow(pfe_con_res)>0 && nrow(usuarios_all)>0) {
      rfe_all |>
        group_by(jugador_id, partido_id) |>
        slice_max(created_at, n=1, with_ties=FALSE) |> ungroup() |>
        mutate(num=as.integer(str_extract(partido_id,"(?<=_)\\d+"))) |>
        inner_join(pfe_con_res |> select(num, goles_l, goles_v), by="num") |>
        rowwise() |>
        mutate(p=calcular_puntos_partido(goles_local,goles_visitante,goles_l,goles_v)) |>
        ungroup() |>
        mutate(jugador_id=id_chr(jugador_id)) |>
        left_join(usuarios_all |> mutate(jugador_id=id_chr(id), jugador=nombre) |>
                    select(jugador_id, jugador), by="jugador_id") |>
        filter(!is.na(jugador)) |>
        group_by(jugador) |> summarise(pts_res_fe=sum(p,na.rm=TRUE), .groups="drop")
    } else data.frame(jugador=character(), pts_res_fe=integer())

    pts_p |> left_join(pts_s,by=c("jugador","jugador_id")) |>
      left_join(pts_cuadro,by="jugador") |>
      left_join(pts_res_fe,by="jugador") |>
      mutate(across(c(pts_st,pts_cuadro,pts_res_fe),~replace_na(.,0L)),
             pts_fase_grupos = pts_fg + pts_st,
             total=pts_fg+pts_st+pts_cuadro+pts_res_fe) |>
      arrange(desc(total)) |> mutate(puesto=row_number())
  })

  # ── Login ────────────────────────────────────────────────────
  observeEvent(input$btn_login, {
    req(input$sel_jug, input$txt_pw)
    udb <- sb_get("usuarios","?select=id,nombre,password,picks_enviados")
    if (is.null(udb)) { err_login("Error de conexión."); return() }
    fila <- as.data.frame(udb) |> filter(nombre==input$sel_jug)
    if (nrow(fila)==0||fila$password[1]!=trimws(input$txt_pw)) {
      err_login("Contraseña incorrecta.")
    } else {
      err_login("")
      usuario(list(id=fila$id[1],nombre=fila$nombre[1],
                   picks_enviados=isTRUE(fila$picks_enviados[1])))
    }
  })
  observeEvent(input$btn_logout, {
    usuario(NULL)
  })

  output$header_info <- renderUI({
    u <- usuario()
    if (is.null(u)) tags$div(class="ts","No has iniciado sesión")
    else tags$div(class="ts",paste0("Sesión: ",u$nombre,"  "),
                  actionLink("btn_logout","Cerrar sesión",style="color:#f5c518;"))
  })

  # Hora del último scrape de Wikipedia, en huso horario de Uruguay (UTC-3)
  output$scrape_info <- renderUI({
    db <- tryCatch(datos_base(), error=function(e) NULL)
    if (is.null(db) || is.null(db$scrape_ts)) return(NULL)
    # Convertir a hora de Uruguay (UTC-3) sin depender de tz del servidor
    hora_uy <- format(as.POSIXct(db$scrape_ts, tz="UTC") - 3*3600, "%d/%m %H:%M")
    fuente <- if (!is.null(db$scrape_fuente)) db$scrape_fuente else "Wikipedia"
    tags$div(class="ts", style="font-size:.72em;opacity:.85;",
      paste0("Resultados actualizados: ", hora_uy, " hs (UY) · ", fuente))
  })

  # ── Main UI ──────────────────────────────────────────────────
  # Modo CONSULTA (sin login): todas las pestañas de lectura están disponibles
  # para cualquiera. Solo "Mi cuadro FE" y "Resultados FE" piden login, porque
  # son de CARGA. Así la mayoría (que solo mira) no pasa por el login pesado.
  output$main_ui <- renderUI({
    u <- usuario()
    login_box <- tags$div(class="login-box",
      tags$img(src=LOGO_URL,style="height:60px;border-radius:8px;display:block;margin:0 auto 16px;"),
      tags$h3(style="color:#0f3460;text-align:center;font-weight:800;","Ingresar al PENCÓN"),
      tags$p(style="text-align:center;color:#666;font-size:.9em;",
        "Iniciá sesión para cargar tus apuestas y para ver las apuestas de los demás jugadores. Para mirar el ranking, la evolución y los cuadros no hace falta."),
      selectInput("sel_jug","Tu nombre",choices=c("",JUGADORES_LISTA)),
      passwordInput("txt_pw","Contraseña"),
      actionButton("btn_login","Entrar",
        style="background:#0f3460;color:white;font-weight:700;width:100%;margin-top:8px;"),
      uiOutput("err_ui")
    )
    # Contenido de las pestañas de carga: si no hay login, mostrar el login box
    cuadro_tab    <- if (is.null(u)) login_box else uiOutput("cuadro_ui")
    resultados_tab<- if (is.null(u)) login_box else uiOutput("resultados_fe_ui")

    # Pestañas SENSIBLES (revelan predicciones): requieren login para no romper
    # el anti-trampa. Sin login muestran el cuadro de ingreso.
    gated <- function(out_id) if (is.null(u)) login_box else uiOutput(out_id)

    tabsetPanel(
        tabPanel("📊 Ranking",         br(), DTOutput("tbl_ranking")),
        tabPanel("📈 Evolución",        br(), uiOutput("evolucion_ui")),
        tabPanel("⚽ Por partido",      br(), gated("por_partido_ui")),
        tabPanel("🧪 Simulador",        br(), gated("simulador_ui")),
        tabPanel("🏆 Crack de jornada", br(), uiOutput("crack_ui")),
        tabPanel("🗓️ Mi cuadro FE",    br(), cuadro_tab),
        tabPanel("⚽ Resultados FE",   br(), resultados_tab),
        tabPanel("👤 Por jugador",      br(), gated("por_jugador_ui")),
        tabPanel("📋 Reglas",
          br(),
          tags$div(style="max-width:800px;margin:0 auto;",
            tags$div(
              style="background:linear-gradient(135deg,#1a1a2e,#0f3460);border-radius:14px;padding:24px 28px;margin-bottom:24px;color:white;text-align:center;",
              tags$img(src=LOGO_URL,style="height:60px;border-radius:8px;margin-bottom:12px;display:block;margin-left:auto;margin-right:auto;"),
              tags$h2(style="color:#f5c518;font-weight:900;letter-spacing:2px;margin:0 0 8px;","PENCÓN CACAF 2026"),
              tags$p(style="color:#ccc;font-size:0.95em;margin:0;",
                "¡Bienvenidos a una nueva edición del Mundial y con ella, su respectivo PENCÓN CACAF! ",
                "La intención de esta penca es incorporar todo lo que un amante de la verosimilitud desearía en una justa deportiva.")
            ),
            tags$h3(style="color:#0f3460;font-weight:800;border-bottom:3px solid #f5c518;padding-bottom:6px;margin-bottom:20px;","¿Cómo se suman puntos?"),
            tags$div(style="background:#f0f4ff;border-radius:10px;padding:14px 18px;margin-bottom:16px;font-size:0.88em;color:#333;",
              tags$strong("44%")," derivan de acertar o no los resultados exactos de los 104 partidos. ",
              tags$strong("56%")," derivan de tu clarividencia para acertar lo importante."),
            fluidRow(
              column(6,
                tags$div(style="background:white;border:1px solid #dee2e6;border-radius:10px;padding:16px;margin-bottom:16px;",
                  tags$h5(style="color:#0f3460;font-weight:700;margin-bottom:10px;","⚽ Por resultado del partido"),
                  tags$p(style="font-size:0.8em;color:#888;","Scores a 120 min. Penales no inciden en la apuesta."),
                  tags$table(style="width:100%;font-size:0.88em;",
                    tags$tr(tags$td(tags$span(style="background:#1b7837;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","8P")),tags$td(style="padding:4px 8px;color:#444;","Score exacto")),
                    tags$tr(style="background:#f8f9fa;",tags$td(tags$span(style="background:#5aae61;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","5P")),tags$td(style="padding:4px 8px;color:#444;","Acertás ganador/empate y diferencia de goles")),
                    tags$tr(tags$td(tags$span(style="background:#a6dba0;color:#333;border-radius:4px;padding:2px 7px;font-weight:700;","3P")),tags$td(style="padding:4px 8px;color:#444;","Acertás ganador/empate pero no la diferencia")),
                    tags$tr(style="background:#f8f9fa;",tags$td(tags$span(style="background:#f4a582;color:#333;border-radius:4px;padding:2px 7px;font-weight:700;","0P")),tags$td(style="padding:4px 8px;color:#444;","Fallo"))
                  )
                )
              ),
              column(6,
                tags$div(style="background:white;border:1px solid #dee2e6;border-radius:10px;padding:16px;margin-bottom:16px;",
                  tags$h5(style="color:#0f3460;font-weight:700;margin-bottom:10px;","🏅 Apuesta inicial del podio"),
                  tags$table(style="width:100%;font-size:0.88em;",
                    tags$tr(tags$td(tags$span(style="background:#c0a000;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","100P")),tags$td(style="padding:4px 8px;color:#444;","Campeón")),
                    tags$tr(style="background:#f8f9fa;",tags$td(tags$span(style="background:#9e9e9e;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","80P")),tags$td(style="padding:4px 8px;color:#444;","Subcampeón")),
                    tags$tr(tags$td(tags$span(style="background:#cd7f32;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","60P")),tags$td(style="padding:4px 8px;color:#444;","3er puesto")),
                    tags$tr(style="background:#f8f9fa;",tags$td(tags$span(style="background:#37474f;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","40P")),tags$td(style="padding:4px 8px;color:#444;","4to puesto"))
                  )
                ),
                tags$div(style="background:white;border:1px solid #dee2e6;border-radius:10px;padding:16px;margin-bottom:16px;",
                  tags$h5(style="color:#0f3460;font-weight:700;margin-bottom:10px;","🏆 Clasificación a 16avos (fase de grupos)"),
                  tags$table(style="width:100%;font-size:0.88em;",
                    tags$tr(tags$td(tags$span(style="background:#0f3460;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","10P")),tags$td(style="padding:4px 8px;color:#444;","Equipo clasificado en posición indicada (1°/2°/3°)")),
                    tags$tr(style="background:#f8f9fa;",tags$td(tags$span(style="background:#1a6eb5;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","5P")),tags$td(style="padding:4px 8px;color:#444;","Equipo clasificado en posición distinta"))
                  )
                )
              )
            ),
            fluidRow(
              column(6,
                tags$div(style="background:white;border:1px solid #dee2e6;border-radius:10px;padding:16px;margin-bottom:16px;",
                  tags$h5(style="color:#0f3460;font-weight:700;margin-bottom:10px;","🔗 Llave correcta en fase eliminatoria"),
                  tags$p(style="font-size:0.85em;color:#555;margin-bottom:8px;","De octavos de final en adelante:"),
                  tags$table(style="width:100%;font-size:0.88em;",
                    tags$tr(tags$td(tags$span(style="background:#8e44ad;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","20P")),tags$td(style="padding:4px 8px;color:#444;","Acertás ambos equipos de una llave")),
                    tags$tr(style="background:#f8f9fa;",tags$td(tags$span(style="background:#b07fd4;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","9P")),tags$td(style="padding:4px 8px;color:#444;","Acertás solo uno de los equipos"))
                  )
                )
              ),
              column(6,
                tags$div(style="background:white;border:1px solid #dee2e6;border-radius:10px;padding:16px;margin-bottom:16px;",
                  tags$h5(style="color:#0f3460;font-weight:700;margin-bottom:10px;","🌟 Premios individuales"),
                  tags$table(style="width:100%;font-size:0.88em;",
                    tags$tr(tags$td(tags$span(style="background:#c0392b;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","70P")),tags$td(style="padding:4px 8px;color:#444;","Bota de Oro")),
                    tags$tr(style="background:#f8f9fa;",tags$td(tags$span(style="background:#8e44ad;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","55P")),tags$td(style="padding:4px 8px;color:#444;","Balón de Oro")),
                    tags$tr(tags$td(tags$span(style="background:#0f3460;color:white;border-radius:4px;padding:2px 7px;font-weight:700;","20P")),tags$td(style="padding:4px 8px;color:#444;","Guante de Oro"))
                  )
                )
              )
            ),
            tags$h3(style="color:#0f3460;font-weight:800;border-bottom:3px solid #f5c518;padding-bottom:6px;margin-bottom:20px;","¿Y la biyuya?"),
            tags$div(style="background:linear-gradient(135deg,#1a1a2e,#0f3460);border-radius:14px;padding:20px 24px;color:white;margin-bottom:24px;",
              tags$div(style="text-align:center;margin-bottom:16px;",
                tags$span(style="background:#f5c518;color:#1a1a2e;font-size:1.1em;font-weight:800;padding:6px 18px;border-radius:20px;","Cuota: $200")),
              tags$div(style="display:flex;flex-wrap:wrap;gap:12px;justify-content:center;",
                tags$div(style="background:rgba(245,197,24,0.15);border:1px solid #f5c518;border-radius:10px;padding:14px 18px;min-width:180px;flex:1;",
                  tags$div(style="font-size:1.3em;margin-bottom:4px;","🥇"),
                  tags$div(style="font-size:0.75em;color:#f5c518;font-weight:700;letter-spacing:1px;margin-bottom:6px;","1ER PREMIO"),
                  tags$div(style="font-size:0.88em;color:#eee;","Pilcha de club/selección uruguaya de fútbol + la de la gloriosa ",tags$strong(style="color:white;","SELECCIÓN URUGUAYA DE DODGEBALL"))),
                tags$div(style="background:rgba(158,158,158,0.15);border:1px solid #9e9e9e;border-radius:10px;padding:14px 18px;min-width:180px;flex:1;",
                  tags$div(style="font-size:1.3em;margin-bottom:4px;","🥈"),
                  tags$div(style="font-size:0.75em;color:#ccc;font-weight:700;letter-spacing:1px;margin-bottom:6px;","2DO PREMIO"),
                  tags$div(style="font-size:0.88em;color:#eee;","Prenda de la tienda de tu club con valor estimado de ",tags$strong(style="color:white;","$2000"))),
                tags$div(style="background:rgba(205,127,50,0.15);border:1px solid #cd7f32;border-radius:10px;padding:14px 18px;min-width:180px;flex:1;",
                  tags$div(style="font-size:1.3em;margin-bottom:4px;","🥉"),
                  tags$div(style="font-size:0.75em;color:#cd7f32;font-weight:700;letter-spacing:1px;margin-bottom:6px;","3ER PREMIO"),
                  tags$div(style="font-size:0.88em;color:#eee;","Merienda para dos en el restaurante del Parque o del Centenario"))
              ),
              tags$p(style="font-size:0.78em;color:#aaa;text-align:center;margin-top:12px;margin-bottom:0;","* O sus equivalentes en efectivo. Se reparte todo, no sobra un peso.")
            ),
            tags$div(style="background:#fff8e1;border-left:4px solid #f5c518;padding:12px 16px;border-radius:0 8px 8px 0;font-size:0.88em;color:#555;",
              tags$strong("¿Cómo me entero de todo? "),
              "Al final de cada jornada te pasamos los puntajes actualizados con comentario incluido de los eventos de la jornada, por el grupo de WhatsApp del PENCÓN CACAF."),
            br()
          )
        )
      )
  })

  output$err_ui <- renderUI({
    if (err_login()!="") tags$p(style="color:red;font-size:.9em;margin-top:8px;",err_login())
  })

  # ══════════════════════════════════════════════════════════════
  # PESTAÑA: RANKING
  # ══════════════════════════════════════════════════════════════
  output$tbl_ranking <- renderDT({
    rk <- tryCatch(ranking_rv(), error=function(e) NULL)
    if (is.null(rk)) return(datatable(data.frame(Mensaje="Cargando...")))

    # Evolución: usa el acumulado total por día (misma fuente que el gráfico Total)
    acum <- acumulado_total_rv()
    fechas_orden <- sort(unique(acum$fecha_date))
    if (length(fechas_orden) >= 2) {
      f_hoy  <- fechas_orden[length(fechas_orden)]
      f_ayer <- fechas_orden[length(fechas_orden)-1]
      # Acumulado de cada jugador hasta una fecha de corte (su último valor <= corte)
      ranking_a_fecha <- function(corte) {
        acum |> filter(fecha_date <= corte) |>
          group_by(jugador) |>
          slice_max(fecha_date, n=1, with_ties=FALSE) |> ungroup() |>
          arrange(desc(pts_acum)) |> mutate(pos=row_number())
      }
      rk_hoy  <- ranking_a_fecha(f_hoy)  |> select(jugador, pos_hoy=pos)
      rk_ayer <- ranking_a_fecha(f_ayer) |> select(jugador, pos_ayer=pos)
      rk2 <- rk |>
        left_join(rk_hoy,  by="jugador") |>
        left_join(rk_ayer, by="jugador") |>
        mutate(
          pos_ref = ifelse(is.na(pos_hoy), puesto, pos_hoy),
          evol=case_when(
            is.na(pos_ayer)~"—",
            pos_ayer-pos_ref>0~paste0("▲ ",pos_ayer-pos_ref),
            pos_ayer-pos_ref<0~paste0("▼ ",pos_ref-pos_ayer),
            TRUE~"="
          ))
    } else {
      rk2 <- rk |> mutate(evol="—")
    }
    datatable(
      rk2 |> mutate(pts_fe_total = pts_cuadro + pts_res_fe) |>
        select(Puesto=puesto, Jugador=jugador,
                    `Pts. Fase de grupos`=pts_fase_grupos,
                    `Pts. Fase eliminatoria`=pts_fe_total,
                    Total=total, `Evolución`=evol),
      options=list(pageLength=55,
        columnDefs=list(list(className="dt-center",targets="_all"))),
      rownames=FALSE
    ) |> formatStyle("Evolución",
      color=styleEqual(
        c(grep("▲",unique(rk2$evol),value=TRUE),grep("▼",unique(rk2$evol),value=TRUE),"=","—"),
        c(rep("#1b7837",sum(grepl("▲",unique(rk2$evol)))),
          rep("#c0392b",sum(grepl("▼",unique(rk2$evol)))),"#333","#888")
      ), fontWeight="bold")
  })

  # ══════════════════════════════════════════════════════════════
  # PESTAÑA: EVOLUCIÓN
  # ══════════════════════════════════════════════════════════════
  output$evolucion_ui <- renderUI({
    tagList(
      tags$div(class="evolucion-hint",
        tags$strong("Consejo: "),
        "Click en un jugador para ocultarlo/mostrarlo. Doble click para ver solo ese jugador."),
      tags$div(style="display:flex;gap:20px;flex-wrap:wrap;align-items:flex-start;",
        selectInput("sel_fase_evol","Mostrar puntos de:",
          choices=c(
            "Fase de grupos (Resultados)"="fg",
            "Fase eliminatoria"="fe",
            "Total"="total"
          ), selected="total", width="280px"),
        radioButtons("sel_grupo_evol","Jugadores:",
          choices=c("Top 10"="top10","Topn't 10"="nottop10","Todos (individual)"="todos"),
          selected="top10", inline=TRUE)
      ),
      plotlyOutput("plot_evolucion", height="550px")
    )
  })

  output$plot_evolucion <- renderPlotly({
    tryCatch({
    fase <- if (is.null(input$sel_fase_evol)) "fg" else input$sel_fase_evol

    # --- Fase de grupos (Resultados) ---
    df_fg <- pts_fg_rv() |> filter(!is.na(fecha_date)) |> arrange(fecha_date) |>
      group_by(jugador, fecha_date) |>
      summarise(pts_dia=sum(puntos,na.rm=TRUE), .groups="drop") |>
      group_by(jugador) |> mutate(pts_acum=cumsum(pts_dia)) |> ungroup()

    if (fase == "fg") {
      df_plot <- df_fg
    } else {
      # Puntos de standings FG: se suman todos el 27/06 (último día FG)
      fecha_standings <- as.Date("2026-06-27")
      pts_st_j <- pts_st_rv() |>
        group_by(participante_nombre) |>
        summarise(pts_st=sum(pts_standing,na.rm=TRUE), .groups="drop") |>
        rename(jugador=participante_nombre)

      # Base FG al final de FG por jugador
      base_fg_final <- df_fg |> group_by(jugador) |>
        summarise(pts_fg_total=max(pts_acum, na.rm=TRUE), .groups="drop")

      # Puntos FE resultados por fecha
      rfe_all <- resultados_fe_db()
      pfe_con_res <- partidos_fe_excel() |> filter(!is.na(goles_l))

      df_fe_res_pts <- if (nrow(rfe_all)>0 && nrow(pfe_con_res)>0) {
        rfe_all |>
          group_by(jugador_id, partido_id) |>
          slice_max(created_at, n=1, with_ties=FALSE) |> ungroup() |>
          mutate(num=as.integer(str_extract(partido_id,"(?<=_)\\d+"))) |>
          inner_join(pfe_con_res |> select(num, goles_l, goles_v, fecha_date), by="num") |>
          rowwise() |>
          mutate(pts_fe=calcular_puntos_partido(goles_local,goles_visitante,goles_l,goles_v)) |>
          ungroup() |>
          left_join(
            sb_get_cached("usuarios","?select=id,nombre", ttl=600) |> as.data.frame() |>
              rename(jugador_id=id, jugador=nombre),
            by="jugador_id"
          ) |>
          filter(!is.na(jugador)) |>
          group_by(jugador, fecha_date) |>
          summarise(pts_dia_fe=sum(pts_fe,na.rm=TRUE), .groups="drop")
      } else data.frame(jugador=character(),fecha_date=as.Date(character()),pts_dia_fe=numeric())

      # Puntos CUADRO FE por fecha (fecha = la del último partido madre)
      picks_all <- picks_cuadro_db()
      res_excel_evol <- partidos_fe_excel()
      usuarios_evol <- tryCatch(as.data.frame(sb_get_cached("usuarios","?select=id,nombre", ttl=600)), error=function(e) data.frame())
      df_cuadro_pts <- if (nrow(picks_all)>0 && "partido_num" %in% names(picks_all) && nrow(usuarios_evol)>0) {
        partes <- lapply(seq_len(nrow(usuarios_evol)), function(i) {
          jid <- usuarios_evol$id[i]; jn <- usuarios_evol$nombre[i]
          mp <- picks_all |> filter(mismo_id(jugador_id, jid)) |>
            mutate(partido_num=suppressWarnings(as.integer(partido_num)))
          if (nrow(mp)==0) return(NULL)
          det <- detalle_cuadro_jugador(mp, res_excel_evol) |>
            filter(!is.na(puntos), !is.na(fecha_date))
          if (nrow(det)==0) return(NULL)
          det |> group_by(fecha_date) |>
            summarise(pts_dia_cuadro=sum(puntos,na.rm=TRUE), .groups="drop") |>
            mutate(jugador=jn)
        })
        partes <- partes[!sapply(partes, is.null)]
        if (length(partes)==0) data.frame(jugador=character(),fecha_date=as.Date(character()),pts_dia_cuadro=numeric())
        else bind_rows(partes)
      } else data.frame(jugador=character(),fecha_date=as.Date(character()),pts_dia_cuadro=numeric())

      # Asegurar columnas y tipos antes del join (evita fallo si algún df quedó vacío)
      if (!("fecha_date" %in% names(df_fe_res_pts))) df_fe_res_pts$fecha_date <- as.Date(character())
      if (!("pts_dia_fe" %in% names(df_fe_res_pts))) df_fe_res_pts$pts_dia_fe <- numeric()
      if (!("jugador" %in% names(df_fe_res_pts))) df_fe_res_pts$jugador <- character()
      if (!("fecha_date" %in% names(df_cuadro_pts))) df_cuadro_pts$fecha_date <- as.Date(character())
      if (!("pts_dia_cuadro" %in% names(df_cuadro_pts))) df_cuadro_pts$pts_dia_cuadro <- numeric()
      if (!("jugador" %in% names(df_cuadro_pts))) df_cuadro_pts$jugador <- character()

      # Combinar resultados FE + cuadro FE por fecha (forzar tipos Date consistentes)
      df_fe_res_pts$fecha_date <- as.Date(df_fe_res_pts$fecha_date)
      df_cuadro_pts$fecha_date <- as.Date(df_cuadro_pts$fecha_date)
      df_fe_pts <- full_join(df_fe_res_pts, df_cuadro_pts, by=c("jugador","fecha_date")) |>
        mutate(pts_dia_fe = replace_na(pts_dia_fe,0) + replace_na(pts_dia_cuadro,0)) |>
        filter(!is.na(fecha_date)) |>
        select(jugador, fecha_date, pts_dia_fe)

      if (fase == "fe") {
        # Empieza desde puntos FG total + standings FG el 27/06, luego suma FE por fecha
        todos_jugs <- unique(df_fg$jugador)
        base_df <- base_fg_final |>
          left_join(pts_st_j, by="jugador") |>
          mutate(pts_st=replace_na(pts_st,0), base=pts_fg_total+pts_st)

        if (nrow(df_fe_pts)>0) {
          df_plot <- df_fe_pts |>
            left_join(base_df |> select(jugador,base), by="jugador") |>
            mutate(base=replace_na(base,0)) |>
            arrange(jugador, fecha_date) |>
            group_by(jugador) |>
            mutate(pts_acum=base+cumsum(pts_dia_fe)) |> ungroup() |>
            select(jugador, fecha_date, pts_acum)
          # Añadir punto de arranque (27/06 con solo base)
          arranque <- base_df |>
            mutate(fecha_date=fecha_standings, pts_acum=base) |>
            select(jugador, fecha_date, pts_acum) |>
            filter(!jugador %in% df_plot$jugador | fecha_date < min(df_plot$fecha_date))
          df_plot <- bind_rows(arranque, df_plot) |> arrange(jugador, fecha_date)
        } else {
          df_plot <- base_df |>
            mutate(fecha_date=fecha_standings, pts_acum=base) |>
            select(jugador, fecha_date, pts_acum)
        }

      } else {
        # Total: FG diario + standings el 27/06 + FE por fecha
        df_fg2 <- df_fg |> rename(pts_acum_fg=pts_acum)
        df_all <- df_fg2

        if (nrow(df_fe_pts)>0) {
          df_all2 <- bind_rows(
            df_fg2 |> mutate(tipo="fg", pts_d=pts_dia),
            pts_st_j |> mutate(fecha_date=fecha_standings, pts_d=pts_st, tipo="st") |>
              select(jugador, fecha_date, pts_d, tipo),
            df_fe_pts |> rename(pts_d=pts_dia_fe) |> mutate(tipo="fe")
          ) |>
            arrange(jugador, fecha_date) |>
            group_by(jugador) |>
            mutate(pts_acum=cumsum(pts_d)) |> ungroup() |>
            # Un solo punto por fecha: el acumulado final del día (evita doble registro 27/06)
            group_by(jugador, fecha_date) |>
            summarise(pts_acum=max(pts_acum), .groups="drop") |>
            select(jugador, fecha_date, pts_acum)
          df_plot <- df_all2
        } else {
          # Solo FG + standings — colapsar 27/06 en un punto
          df_plot <- bind_rows(
            df_fg2 |> select(jugador, fecha_date, pts_acum=pts_acum_fg),
            pts_st_j |> mutate(fecha_date=fecha_standings) |>
              left_join(df_fg2 |> filter(fecha_date==max(fecha_date)) |>
                          select(jugador,pts_acum_fg), by="jugador") |>
              mutate(pts_acum=replace_na(pts_acum_fg,0)+replace_na(pts_st,0)) |>
              select(jugador, fecha_date, pts_acum)
          ) |>
            group_by(jugador, fecha_date) |>
            summarise(pts_acum=max(pts_acum), .groups="drop") |>
            arrange(jugador, fecha_date)
        }
      }
    }

    # Filtro de jugadores según grupo seleccionado (Top 10 / Topn't 10 / Todos)
    grupo_sel <- if (is.null(input$sel_grupo_evol)) "top10" else input$sel_grupo_evol
    rk <- tryCatch(ranking_rv(), error=function(e) NULL)
    if (!is.null(rk) && nrow(rk)>0 && grupo_sel != "todos") {
      if (grupo_sel == "top10") {
        jugs <- rk |> arrange(puesto) |> head(10) |> pull(jugador)
        df_plot <- df_plot |> filter(jugador %in% jugs)
      } else if (grupo_sel == "nottop10") {
        jugs <- rk |> arrange(puesto) |> tail(10) |> pull(jugador)
        df_plot <- df_plot |> filter(jugador %in% jugs)
      }
    }

    p <- ggplot(df_plot, aes(x=fecha_date, y=pts_acum, color=jugador, group=jugador,
      text=paste0("<b>",jugador,"</b><br>",format(fecha_date,"%d/%m"),": ",pts_acum," pts"))) +
      geom_line(linewidth=1) + geom_point(size=2) +
      scale_x_date(date_labels="%d/%m", date_breaks="2 days") +
      labs(x=NULL, y="Puntos acumulados") +
      theme_minimal(base_size=12) +
      theme(axis.text.x=element_text(angle=45,hjust=1), legend.title=element_blank(),
            panel.grid.minor=element_blank())
    ggplotly(p, tooltip="text") |>
      layout(legend=list(orientation="h",x=0,y=-0.25,font=list(size=10),itemwidth=30),
             margin=list(b=120,r=20)) |>
      config(displayModeBar=FALSE)
    }, error=function(e) {
      plotly::plot_ly() |>
        plotly::add_annotations(text=paste0("Error en gráfico: ",conditionMessage(e)),
          x=0.5, y=0.5, xref="paper", yref="paper", showarrow=FALSE,
          font=list(size=14, color="red"))
    })
  })

  # ══════════════════════════════════════════════════════════════
  # PESTAÑA: POR PARTIDO (FG + FE)
  # ══════════════════════════════════════════════════════════════
  output$por_partido_ui <- renderUI({
    if (!jugador_habilitado()) return(tags$div(
      style="background:#fff3cd;border-left:4px solid #e07b28;border-radius:6px;padding:20px 24px;margin:20px 0;",
      tags$h5(style="color:#7a4000;margin-bottom:8px;","🔒 Acceso bloqueado"),
      tags$p(style="color:#7a4000;margin:0;", "Para ver esta pestaña primero tenés que:"),
      tags$ol(style="color:#7a4000;",
        tags$li("Completar y enviar tu cuadro eliminatorio en \"Mi cuadro FE\""),
        tags$li("Ingresar tus resultados para todos los partidos disponibles en \"Resultados FE\"")
      )
    ))
    res <- datos_base()$res

    # Partidos FG ordenados
    partidos_fg_ord <- res |>
      filter(!is.na(grupo)) |>
      mutate(partido=paste(grupo,estandarizar(local),estandarizar(visitante),sep="_"),
             num=as.integer(str_extract(partido_id,"(?<=_)\\d+"))) |>
      select(num, partido, goles_l) |> distinct() |> arrange(num)

    # Label legible por ronda
    ronda_fe <- function(n) {
      dplyr::case_when(
        n>=73 & n<=88 ~ paste0("16avos #",n-72),
        n>=89 & n<=96 ~ paste0("8avos #",n-88),
        n>=97 & n<=100 ~ paste0("Cuartos #",n-96),
        n==101 | n==102 ~ paste0("Semis #",n-100),
        n==103 ~ "3er puesto",
        n==104 ~ "Final",
        TRUE ~ paste0("M",n)
      )
    }

    pfe <- partidos_fe_excel() |>
      filter(!is.na(local), local!="", !is.na(visitante), visitante!="") |>
      mutate(label=paste0(ronda_fe(num),": ",local," vs ",visitante))

    # FG
    partidos_jugados_fg    <- partidos_fg_ord |> filter(!is.na(goles_l)) |> pull(partido)
    partidos_no_jugados_fg <- partidos_fg_ord |> filter(is.na(goles_l))  |> pull(partido)

    # FE: separar sin jugar (próximos / apostables) de jugados
    pfe_sin_res <- pfe |> filter(is.na(goles_l))
    pfe_jugados <- pfe |> filter(!is.na(goles_l))
    choices_fe_sin <- if (nrow(pfe_sin_res)>0) setNames(pfe_sin_res$partido_id, pfe_sin_res$label) else c()
    choices_fe_jug <- if (nrow(pfe_jugados)>0) setNames(pfe_jugados$partido_id, pfe_jugados$label) else c()

    # Orden de la lista: lo que viene primero (FE sin jugar), luego FG sin jugar,
    # luego los ya jugados (FE y FG) al final
    choices <- c(
      if (length(choices_fe_sin)>0) c("── Fase eliminatoria (próximos) ──"=NA, choices_fe_sin),
      if (length(partidos_no_jugados_fg)>0) c("── Fase de grupos (pendientes) ──"=NA, partidos_no_jugados_fg),
      if (length(choices_fe_jug)>0) c("── Fase eliminatoria (jugados) ──"=NA, choices_fe_jug),
      if (length(partidos_jugados_fg)>0) c("── Fase de grupos (jugados) ──"=NA, partidos_jugados_fg)
    )

    # Default = primer próximo: FE sin jugar, sino FG sin jugar, sino el primero que haya
    selected_default <- if (nrow(pfe_sin_res)>0) {
      pfe_sin_res$partido_id[1]
    } else if (length(partidos_no_jugados_fg)>0) {
      partidos_no_jugados_fg[1]
    } else if (nrow(pfe_jugados)>0) {
      pfe_jugados$partido_id[nrow(pfe_jugados)]
    } else {
      partidos_fg_ord$partido[1]
    }

    tagList(
      selectInput("sel_partido","Partido",
        choices=choices,
        selected=selected_default),
      plotOutput("plot_matriz_partido",height="280px"),
      uiOutput("stats_partido_ui"),
      tags$hr(),
      DTOutput("tbl_por_partido")
    )
  })

  output$plot_matriz_partido <- renderPlot({
    req(input$sel_partido)
    sel <- input$sel_partido

    # Para FE: apuestas de cada jugador en Supabase
    is_fe <- grepl("WC2022_0[789]|WC2022_1", sel)
    if (is_fe) {
      rfe <- resultados_fe_db()
      if (nrow(rfe)==0 || !("partido_id" %in% names(rfe))) return(NULL)
      pnum <- as.integer(str_extract(sel,"(?<=_)\\d+"))
      preds <- rfe |> filter(partido_id==sel | as.integer(str_extract(as.character(partido_id),"(?<=_)\\d+"))==pnum) |>
        group_by(jugador_id) |> slice_max(created_at,n=1,with_ties=FALSE) |> ungroup() |>
        rename(gl_pred=goles_local, gv_pred=goles_visitante)
    } else {
      preds <- pts_fg_rv() |> filter(partido==sel)
    }
    if (nrow(preds)==0 || is.null(preds$gl_pred)) return(NULL)

    cap <- function(g) ifelse(g>=5,"5+",as.character(g))
    niveles <- c("0","1","2","3","4","5+")
    mat <- preds |>
      mutate(gl_c=cap(gl_pred),gv_c=cap(gv_pred)) |>
      count(gl_c,gv_c) |>
      complete(gl_c=niveles,gv_c=niveles,fill=list(n=0)) |>
      mutate(gl_c=factor(gl_c,niveles),gv_c=factor(gv_c,niveles),
             pct=round(100*n/sum(n),1))
    ggplot(mat,aes(x=gv_c,y=gl_c,fill=n))+
      geom_tile(color="white",linewidth=.8)+
      geom_text(aes(label=ifelse(n>0,as.character(n),"")),size=4.5,fontface="bold",color="white")+
      scale_fill_gradient(low="#dce8f5",high="#0f3460",name="Apuestas")+
      scale_y_discrete(limits=niveles)+
      labs(x="Goles Visitante",y="Goles Local")+
      theme_minimal(base_size=13)+theme(panel.grid=element_blank())
  })

  output$stats_partido_ui <- renderUI({
    req(input$sel_partido)
    sel <- input$sel_partido
    is_fe <- grepl("WC2022_0[789]|WC2022_1", sel)
    if (is_fe) {
      rfe <- resultados_fe_db()
      if (nrow(rfe)==0 || !("partido_id" %in% names(rfe))) return(NULL)
      pnum <- as.integer(str_extract(sel,"(?<=_)\\d+"))
      preds <- rfe |> filter(as.integer(str_extract(as.character(partido_id),"(?<=_)\\d+"))==pnum) |>
        group_by(jugador_id) |> slice_max(created_at,n=1,with_ties=FALSE) |> ungroup() |>
        rename(gl_pred=goles_local, gv_pred=goles_visitante)
    } else {
      preds <- pts_fg_rv() |> filter(partido==sel)
    }
    if (nrow(preds)==0) return(NULL)
    n <- nrow(preds)
    pct_l <- round(100*mean(preds$gl_pred>preds$gv_pred,na.rm=TRUE),1)
    pct_e <- round(100*mean(preds$gl_pred==preds$gv_pred,na.rm=TRUE),1)
    pct_v <- round(100*mean(preds$gl_pred<preds$gv_pred,na.rm=TRUE),1)
    chip <- function(lbl,val,col) tags$div(style=paste0(
      "display:inline-flex;align-items:center;gap:8px;background:",col,
      ";color:white;border-radius:8px;padding:10px 18px;margin:4px 6px;"),
      tags$span(style="font-size:1.4em;font-weight:900;",paste0(val,"%")),
      tags$span(lbl))
    tags$div(style="margin:12px 0;text-align:center;",
      tags$p(style="color:#888;font-size:.85em;margin-bottom:8px;",
             paste0("Distribución de apuestas (",n," jugadores)")),
      chip("Victoria local",pct_l,"#0f3460"),
      chip("Empate",pct_e,"#5aae61"),
      chip("Victoria visitante",pct_v,"#c0392b")
    )
  })

  output$tbl_por_partido <- renderDT({
    req(input$sel_partido)
    sel <- input$sel_partido
    is_fe <- grepl("WC2022_0[789]|WC2022_1", sel)

    if (is_fe) {
      rfe <- resultados_fe_db()
      if (nrow(rfe)==0 || !("partido_id" %in% names(rfe)))
        return(datatable(data.frame(Mensaje="Sin apuestas registradas para este partido.")))
      pnum <- as.integer(str_extract(sel,"(?<=_)\\d+"))
      # Resultado real: del Excel (goles_l / goles_v)
      pfe_row <- partidos_fe_excel() |> filter(num==pnum)
      gl_r <- if(nrow(pfe_row)>0 && !is.na(pfe_row$goles_l[1])) pfe_row$goles_l[1] else NA
      gv_r <- if(nrow(pfe_row)>0 && !is.na(pfe_row$goles_v[1])) pfe_row$goles_v[1] else NA

      # Traer nombres de jugadores
      usu <- tryCatch(as.data.frame(sb_get_cached("usuarios","?select=id,nombre", ttl=600)), error=function(e) data.frame())

      preds_fe <- rfe |>
        filter(as.integer(str_extract(as.character(partido_id),"(?<=_)\\d+"))==pnum) |>
        group_by(jugador_id) |> slice_max(created_at,n=1,with_ties=FALSE) |> ungroup()

      if (nrow(usu)>0) preds_fe <- preds_fe |>
        left_join(usu |> mutate(jugador_id=id_chr(id)) |> rename(Jugador=nombre) |> select(jugador_id,Jugador), by="jugador_id")
      else preds_fe <- preds_fe |> mutate(Jugador=jugador_id)

      df <- preds_fe |>
        mutate(
          Real=if(!is.na(gl_r)) paste(gl_r,gv_r,sep="-") else "Pendiente",
          Apuesta=paste(goles_local,goles_visitante,sep="-"),
          Puntos=if(!is.na(gl_r)) mapply(calcular_puntos_partido,goles_local,goles_visitante,gl_r,gv_r) else NA,
          Acierto=dplyr::case_when(
            is.na(Puntos)~"pendiente",
            Puntos==8L~"exacto",Puntos==5L~"diferencia",
            Puntos==3L~"resultado",TRUE~"fallo")
        ) |>
        select(Jugador, Real, Apuesta, Puntos, Acierto)

      datatable(df, options=list(pageLength=55,columnDefs=list(list(className="dt-center",targets="_all"))),rownames=FALSE) |>
        formatStyle("Acierto",backgroundColor=styleEqual(
          c("exacto","diferencia","resultado","fallo","pendiente"),
          c("#1b7837","#5aae61","#a6dba0","#f4a582","#eeeeee")),
          color=styleEqual(c("exacto","diferencia","resultado","fallo","pendiente"),
            c("white","white","white","white","#666")),fontWeight="bold")
    } else {
      pts_fg_rv() |> filter(partido==sel) |>
        mutate(Real=paste(gl_real,gv_real,sep="-"),Apuesta=paste(gl_pred,gv_pred,sep="-")) |>
        select(Jugador=jugador,Real,Apuesta,Puntos=puntos,Acierto=tipo) |>
        datatable(options=list(pageLength=55,columnDefs=list(list(className="dt-center",targets="_all"))),rownames=FALSE) |>
        formatStyle("Acierto",backgroundColor=styleEqual(c("exacto","diferencia","resultado","fallo"),
          c("#1b7837","#5aae61","#a6dba0","#f4a582")),color="white",fontWeight="bold")
    }
  })

  # ══════════════════════════════════════════════════════════════
  # PESTAÑA: SIMULADOR
  # ══════════════════════════════════════════════════════════════
  output$simulador_ui <- renderUI({
    if (!jugador_habilitado()) return(tags$div(
      style="background:#fff3cd;border-left:4px solid #e07b28;border-radius:6px;padding:20px 24px;margin:20px 0;",
      tags$h5(style="color:#7a4000;margin-bottom:8px;","🔒 Acceso bloqueado"),
      tags$p(style="color:#7a4000;margin:0;", "Para ver esta pestaña primero tenés que:"),
      tags$ol(style="color:#7a4000;",
        tags$li("Completar y enviar tu cuadro eliminatorio en \"Mi cuadro FE\""),
        tags$li("Ingresar tus resultados para todos los partidos disponibles en \"Resultados FE\"")
      )
    ))
    res <- datos_base()$res

    ronda_fe <- function(n) {
      dplyr::case_when(
        n>=73 & n<=88 ~ paste0("16avos #",n-72),
        n>=89 & n<=96 ~ paste0("8avos #",n-88),
        n>=97 & n<=100 ~ paste0("Cuartos #",n-96),
        n==101 | n==102 ~ paste0("Semis #",n-100),
        n==103 ~ "3er puesto",
        n==104 ~ "Final",
        TRUE ~ paste0("M",n)
      )
    }

    # FG sin resultado
    partidos_fg_sin_res <- res |>
      filter(!is.na(grupo)) |>
      mutate(partido=paste(grupo,estandarizar(local),estandarizar(visitante),sep="_"),
             num=as.integer(str_extract(partido_id,"(?<=_)\\d+"))) |>
      filter(is.na(goles_l)) |>
      select(num,partido) |> distinct() |> arrange(num) |> pull(partido)

    # FE sin resultado - value = partido_id, label = ronda + equipos
    pfe_sin_res_df <- partidos_fe_excel() |>
      filter(!is.na(local), local!="", is.na(goles_l)) |>
      mutate(label=paste0(ronda_fe(num),": ",local," vs ",visitante))
    pfe_sin_res <- if (nrow(pfe_sin_res_df)>0) setNames(pfe_sin_res_df$partido_id, pfe_sin_res_df$label) else c()

    # Default y orden: lo próximo (FE sin jugar) primero, luego FG pendientes
    todos_sin_res <- c(pfe_sin_res, partidos_fg_sin_res)
    default_sim <- if (length(todos_sin_res)>0) todos_sin_res[1] else NULL

    tagList(
      selectInput("sim_partido","Partido",
        choices=c(
          if(length(pfe_sin_res)>0) c("── Fase eliminatoria (próximos) ──"=NA, pfe_sin_res),
          if(length(partidos_fg_sin_res)>0) c("── Fase de grupos (pendientes) ──"=NA, partidos_fg_sin_res)
        ),
        selected=default_sim),
      fluidRow(
        column(6,numericInput("sim_gl","Goles Local",0,min=0)),
        column(6,numericInput("sim_gv","Goles Visitante",0,min=0))
      ),
      DTOutput("tbl_simulador")
    )
  })

  output$tbl_simulador <- renderDT({
    req(input$sim_partido)
    sel <- input$sim_partido
    is_fe <- grepl("WC2022_0[789]|WC2022_1", sel)

    if (is_fe) {
      rfe <- resultados_fe_db()
      if (nrow(rfe)==0 || !("partido_id" %in% names(rfe)))
        return(datatable(data.frame(Mensaje="Sin apuestas registradas para este partido.")))
      pnum <- as.integer(str_extract(sel,"(?<=_)\\d+"))
      usu <- tryCatch(as.data.frame(sb_get_cached("usuarios","?select=id,nombre", ttl=600)), error=function(e) data.frame())
      preds_fe <- rfe |>
        filter(as.integer(str_extract(as.character(partido_id),"(?<=_)\\d+"))==pnum) |>
        group_by(jugador_id) |> slice_max(created_at,n=1,with_ties=FALSE) |> ungroup()
      if (nrow(usu)>0) preds_fe <- preds_fe |>
        left_join(usu |> mutate(jugador_id=id_chr(id)) |> rename(Jugador=nombre) |> select(jugador_id,Jugador), by="jugador_id")
      else preds_fe <- preds_fe |> mutate(Jugador=jugador_id)
      df <- preds_fe |> rowwise() |>
        mutate(
          puntos=calcular_puntos_partido(goles_local,goles_visitante,input$sim_gl,input$sim_gv),
          tipo=dplyr::case_when(puntos==8L~"exacto",puntos==5L~"diferencia",
                                puntos==3L~"resultado",TRUE~"fallo"),
          Apuesta=paste(goles_local,goles_visitante,sep="-")) |> ungroup() |>
        select(Jugador, Apuesta, Puntos=puntos, Acierto=tipo)
      datatable(df,options=list(pageLength=55,columnDefs=list(list(className="dt-center",targets="_all"))),rownames=FALSE) |>
        formatStyle("Acierto",backgroundColor=styleEqual(c("exacto","diferencia","resultado","fallo"),
          c("#1b7837","#5aae61","#a6dba0","#f4a582")),color="white",fontWeight="bold")
    } else {
      pts_fg_rv() |> filter(partido==sel) |>
        rowwise() |>
        mutate(puntos=calcular_puntos_partido(gl_pred,gv_pred,input$sim_gl,input$sim_gv),
               tipo=dplyr::case_when(puntos==8L~"exacto",puntos==5L~"diferencia",
                 puntos==3L~"resultado",TRUE~"fallo"),
               Apuesta=paste(gl_pred,gv_pred,sep="-")) |>
        ungroup() |>
        select(Jugador=jugador,Apuesta,Puntos=puntos,Acierto=tipo) |>
        datatable(options=list(pageLength=55,columnDefs=list(list(className="dt-center",targets="_all"))),rownames=FALSE) |>
        formatStyle("Acierto",backgroundColor=styleEqual(c("exacto","diferencia","resultado","fallo"),
          c("#1b7837","#5aae61","#a6dba0","#f4a582")),color="white",fontWeight="bold")
    }
  })

  # ══════════════════════════════════════════════════════════════
  # PESTAÑA: CRACK DE JORNADA
  # ══════════════════════════════════════════════════════════════
  output$crack_ui <- renderUI({
    fechas_fg <- pts_fg_rv() |> filter(!is.na(fecha_date)) |> pull(fecha_date)
    fechas_fe <- pts_fe_por_fecha_rv() |> filter(!is.na(fecha_date)) |> pull(fecha_date)
    fechas <- sort(unique(c(as.Date(fechas_fg), as.Date(fechas_fe))), decreasing=TRUE)
    fechas_ch <- setNames(as.character(fechas), format(fechas,"%d/%m"))
    tagList(
      selectInput("sel_fecha_crack","Jornada",choices=fechas_ch),
      uiOutput("crack_contenido")
    )
  })

  output$crack_contenido <- renderUI({
    req(input$sel_fecha_crack)
    fecha_sel <- as.Date(input$sel_fecha_crack)
    df_j <- pts_fg_rv() |> filter(fecha_date==fecha_sel)

    # Si no hay datos FG para esta fecha, es una jornada de FE
    if (nrow(df_j)==0) {
      # Detalle de resultados FE de esa jornada: apuestas vs real, partido por partido
      pfe_jor <- partidos_fe_excel() |>
        filter(fecha_date==fecha_sel, !is.na(goles_l), !is.na(goles_v))
      if (nrow(pfe_jor)==0) return(tags$div(style="padding:16px;color:#888;",
        "Sin partidos jugados de fase eliminatoria en esta jornada."))

      rfe_all <- resultados_fe_db()
      usuarios_x <- tryCatch(as.data.frame(sb_get_cached("usuarios","?select=id,nombre", ttl=600)), error=function(e) data.frame())
      if (nrow(rfe_all)==0 || nrow(usuarios_x)==0)
        return(tags$div(style="padding:16px;color:#888;","Sin apuestas para esta jornada."))

      det_fe <- rfe_all |>
        group_by(jugador_id, partido_id) |>
        slice_max(created_at, n=1, with_ties=FALSE) |> ungroup() |>
        mutate(num=as.integer(str_extract(partido_id,"(?<=_)\\d+"))) |>
        inner_join(pfe_jor |> select(num, local, visitante, goles_l, goles_v), by="num") |>
        rowwise() |>
        mutate(pts=calcular_puntos_partido(goles_local,goles_visitante,goles_l,goles_v),
               tipo=dplyr::case_when(pts==8~"exacto",pts==5~"diferencia",pts==3~"resultado",TRUE~"fallo")) |>
        ungroup() |>
        left_join(usuarios_x |> mutate(jugador_id=id_chr(id)) |> rename(jugador=nombre) |> select(jugador_id,jugador), by="jugador_id") |>
        filter(!is.na(jugador)) |>
        mutate(Partido=paste(local,"vs",visitante))

      if (nrow(det_fe)==0) return(tags$div(style="padding:16px;color:#888;","Sin apuestas para esta jornada."))

      res_j <- det_fe |> group_by(jugador) |>
        summarise(pts_j=sum(pts,na.rm=TRUE),
                  dist=sum(abs(goles_local-goles_l)+abs(goles_visitante-goles_v),na.rm=TRUE),
                  .groups="drop")
      max_pts <- max(res_j$pts_j); min_pts <- min(res_j$pts_j)
      cracks  <- res_j |> filter(pts_j==max_pts) |> arrange(jugador)
      # Pifiadores: TODOS los empatados en el mínimo de puntos (sin desempatar por
      # distancia), igual que los cracks comparten el máximo. Puede haber varios.
      peores  <- res_j |> filter(pts_j==min_pts) |> arrange(jugador)

      tabla_det <- function(jugs) {
        det_fe |> filter(jugador %in% jugs) |>
          transmute(Jugador=jugador, Partido,
                    Real=paste(goles_l,goles_v,sep="-"),
                    Apuesta=paste(goles_local,goles_visitante,sep="-"),
                    Puntos=pts, Acierto=tipo) |>
          arrange(Jugador,Partido)
      }
      det_crack <- tabla_det(cracks$jugador)
      det_peor  <- tabla_det(peores$jugador)

      # Datos con las columnas que esperan calcular_medallas y pred_osada
      det_fe_med <- det_fe |>
        transmute(jugador, partido=Partido,
                  gl_pred=goles_local, gv_pred=goles_visitante,
                  gl_real=goles_l, gv_real=goles_v, tipo)
      meds_fe   <- calcular_medallas(det_fe_med)
      osada_fe  <- pred_osada(det_fe_med)

      return(tagList(
        # Banner crack
        tags$div(style="background:linear-gradient(135deg,#1a1a2e,#0f3460);border-radius:16px;
                        padding:28px 32px;margin-bottom:20px;color:white;display:flex;
                        align-items:center;gap:20px;",
          tags$img(src=LOGO_URL,style="height:60px;border-radius:8px;"),
          tags$div(
            tags$div(style="font-size:.9em;opacity:.7;letter-spacing:2px;",
                     if(nrow(cracks)>1)"CRACKS DE LA JORNADA" else "CRACK DE LA JORNADA"),
            tags$div(style="font-size:2.4em;font-weight:900;color:#f5c518;",
                     paste(cracks$jugador,collapse=" y ")),
            tags$div(style="font-size:1.2em;margin-top:6px;",paste0(max_pts," puntos"))
          )
        ),
        tags$h5("Sus pronósticos:"),
        renderDT(datatable(det_crack,options=list(dom="t",pageLength=40,
          columnDefs=list(list(className="dt-center",targets="_all"))),rownames=FALSE) |>
          formatStyle("Acierto",backgroundColor=styleEqual(c("exacto","diferencia","resultado","fallo"),
            c("#1b7837","#5aae61","#a6dba0","#f4a582")),color="white",fontWeight="bold")),

        # Medallas
        if (length(meds_fe)>0) {
          tagList(
            tags$hr(style="margin:20px 0;"),
            tags$h5("🏅 Medallas de la jornada"),
            tags$div(style="font-size:.8em;color:#888;margin-bottom:8px;",
              "🔮 Profeta: más resultados exactos | ",
              "🎰 Kamikaze: promedio más alto de goles apostados | ",
              "🛡️ Conservador: promedio más bajo de goles apostados"
            ),
            tags$div(
              lapply(names(meds_fe), function(nm) {
                tags$div(class="medalla-chip", paste0(nm,": ",meds_fe[[nm]]))
              })
            )
          )
        },

        # Predicción más osada
        if (nrow(osada_fe)>0) {
          tagList(
            tags$hr(style="margin:20px 0;"),
            tags$div(style="background:#fff8e1;border-left:4px solid #f5c518;border-radius:0 8px 8px 0;padding:12px 16px;",
              tags$strong("🎯 Predicción más osada acertada"),
              tags$span(style="font-size:.8em;color:#888;display:block;margin-bottom:6px;",
                "(la apuesta con mayor resultado global que fue al menos parcialmente correcta, pesando por cantidad de goles y diferencia)"),
              paste0(osada_fe$jugador[1]," apostó ",osada_fe$gl_pred[1],"-",osada_fe$gv_pred[1],
                     " en el partido ",osada_fe$partido[1]," (",osada_fe$tipo[1],")")
            )
          )
        },

        # Banner peor
        tags$hr(style="margin:24px 0;"),
        tags$div(style="background:#fff3e0;border-left:5px solid #e07b28;border-radius:4px;
                        padding:16px 24px;margin-bottom:12px;",
          tags$div(style="font-size:.85em;color:#7a4000;font-weight:600;",
                   if(nrow(peores)>1)"Los que la pifiaron más" else "El que la pifió más"),
          tags$div(style="font-size:1.6em;font-weight:800;color:#7a4000;",
                   paste(peores$jugador,collapse=" y ")),
          tags$div(style="color:#7a4000;font-size:.9em;",paste0(min_pts," puntos"))
        ),
        tags$h6("Sus pronósticos:"),
        renderDT(datatable(det_peor,options=list(dom="t",pageLength=40,
          columnDefs=list(list(className="dt-center",targets="_all"))),rownames=FALSE) |>
          formatStyle("Acierto",backgroundColor=styleEqual(c("exacto","diferencia","resultado","fallo"),
            c("#1b7837","#5aae61","#a6dba0","#f4a582")),color="white",fontWeight="bold"))
      ))
    }

    if (nrow(df_j)==0) return(tags$p("Sin datos para esta jornada."))

    res_j <- df_j |> group_by(jugador) |>
      summarise(pts_j=sum(puntos,na.rm=TRUE),dist=sum(abs(gl_pred-gl_real)+abs(gv_pred-gv_real),na.rm=TRUE),.groups="drop")

    max_pts  <- max(res_j$pts_j)
    min_pts  <- min(res_j$pts_j)
    cracks   <- res_j |> filter(pts_j==max_pts) |> arrange(jugador)
    peores   <- res_j |> filter(pts_j==min_pts) |> arrange(desc(dist))
    max_dist <- max(peores$dist)
    peores   <- peores |> filter(dist==max_dist) |> arrange(jugador)

    det_crack <- df_j |> filter(jugador %in% cracks$jugador) |>
      transmute(Jugador=jugador,Partido=partido,Real=paste(gl_real,gv_real,sep="-"),
                Apuesta=paste(gl_pred,gv_pred,sep="-"),Puntos=puntos,Acierto=tipo) |>
      arrange(Jugador,Partido)
    det_peor  <- df_j |> filter(jugador %in% peores$jugador) |>
      transmute(Jugador=jugador,Partido=partido,Real=paste(gl_real,gv_real,sep="-"),
                Apuesta=paste(gl_pred,gv_pred,sep="-"),Puntos=puntos,Acierto=tipo) |>
      arrange(Jugador,Partido)

    # Medallas
    meds <- calcular_medallas(pts_fg_rv(), fecha_sel)
    osada_df <- pred_osada(pts_fg_rv(), fecha_sel)

    tagList(
      # Banner crack
      tags$div(style="background:linear-gradient(135deg,#1a1a2e,#0f3460);border-radius:16px;
                      padding:28px 32px;margin-bottom:20px;color:white;display:flex;
                      align-items:center;gap:20px;",
        tags$img(src=LOGO_URL,style="height:60px;border-radius:8px;"),
        tags$div(
          tags$div(style="font-size:.9em;opacity:.7;letter-spacing:2px;",
                   if(nrow(cracks)>1)"CRACKS DE LA JORNADA" else "CRACK DE LA JORNADA"),
          tags$div(style="font-size:2.4em;font-weight:900;color:#f5c518;",
                   paste(cracks$jugador,collapse=" y ")),
          tags$div(style="font-size:1.2em;margin-top:6px;",paste0(max_pts," puntos"))
        )
      ),
      tags$h5("Sus pronósticos:"),
      renderDT(datatable(det_crack,options=list(dom="t",pageLength=40,
        columnDefs=list(list(className="dt-center",targets="_all"))),rownames=FALSE) |>
        formatStyle("Acierto",backgroundColor=styleEqual(c("exacto","diferencia","resultado","fallo"),
          c("#1b7837","#5aae61","#a6dba0","#f4a582")),color="white",fontWeight="bold")),

      # Medallas
      if (length(meds)>0) {
        tagList(
          tags$hr(style="margin:20px 0;"),
          tags$h5("🏅 Medallas de la jornada"),
          tags$div(style="font-size:.8em;color:#888;margin-bottom:8px;",
            "🔮 Profeta: más resultados exactos | ",
            "🎰 Kamikaze: promedio más alto de goles apostados | ",
            "🛡️ Conservador: promedio más bajo de goles apostados"
          ),
          tags$div(
            lapply(names(meds), function(nm) {
              tags$div(class="medalla-chip", paste0(nm,": ",meds[[nm]]))
            })
          )
        )
      },

      # Predicción más osada
      if (nrow(osada_df)>0) {
        tagList(
          tags$hr(style="margin:20px 0;"),
          tags$div(style="background:#fff8e1;border-left:4px solid #f5c518;border-radius:0 8px 8px 0;padding:12px 16px;",
            tags$strong("🎯 Predicción más osada acertada"),
            tags$span(style="font-size:.8em;color:#888;display:block;margin-bottom:6px;",
              "(la apuesta con mayor resultado global que fue al menos parcialmente correcta, pesando por cantidad de goles y diferencia)"),
            paste0(osada_df$jugador[1]," apostó ",osada_df$gl_pred[1],"-",osada_df$gv_pred[1],
                   " en el partido ",osada_df$partido[1]," (",osada_df$tipo[1],")")
          )
        )
      },

      # Banner peor
      tags$hr(style="margin:24px 0;"),
      tags$div(style="background:#fff3e0;border-left:5px solid #e07b28;border-radius:4px;
                      padding:16px 24px;margin-bottom:12px;",
        tags$div(style="font-size:.85em;color:#7a4000;font-weight:600;",
                 if(nrow(peores)>1)"Los que la pifiaron más" else "El que la pifió más"),
        tags$div(style="font-size:1.6em;font-weight:800;color:#7a4000;",
                 paste(peores$jugador,collapse=" y ")),
        tags$div(style="color:#7a4000;font-size:.9em;",paste0(min_pts," puntos"))
      ),
      tags$h6("Sus pronósticos:"),
      renderDT(datatable(det_peor,options=list(dom="t",pageLength=40,
        columnDefs=list(list(className="dt-center",targets="_all"))),rownames=FALSE) |>
        formatStyle("Acierto",backgroundColor=styleEqual(c("exacto","diferencia","resultado","fallo"),
          c("#1b7837","#5aae61","#a6dba0","#f4a582")),color="white",fontWeight="bold"))
    )
  })

  # ══════════════════════════════════════════════════════════════
  # PESTAÑA: MI CUADRO FE (bracket visual)
  # ══════════════════════════════════════════════════════════════
  output$cuadro_ui <- renderUI({
    u <- usuario(); req(u)
    cuadro_refresh()  # dependencia reactiva: re-render al enviar
    tryCatch({
    # Aislamos los datos del poll (cada 15 min) para que NO re-renderice el
    # cuadro mientras el jugador lo está completando (evita pantalla en blanco
    # y el loop de los dropdowns en cascada). La pestaña solo se reconstruye al
    # enviar (cuadro_refresh) o al cambiar de usuario.
    picks_db <- isolate(picks_cuadro_db())
    mis_picks <- if (nrow(picks_db)>0 && "partido_num" %in% names(picks_db)) {
      picks_db |> filter(mismo_id(jugador_id, u$id)) |>
        mutate(partido_num=suppressWarnings(as.integer(partido_num)))
    } else data.frame(jugador_id=character(), partido_num=integer(),
                      equipo_apostado=character(), created_at=character())
    # ya_enviado: o bien está en Supabase (M102) o el flag de sesión
    ya_enviado <- (nrow(mis_picks)>0 && 102L %in% mis_picks$partido_num) ||
                  isTRUE(u$picks_enviados)

    st <- isolate(st_reales())
    rfe <- isolate(resultados_fe_db())
    pfe_snapshot <- isolate(partidos_fe_excel())

    get_gan_real <- function(n) {
      # La pestaña Mi cuadro FE es SOLO predictiva: no usa resultados reales.
      # Lo único que viene del Excel son los equipos de 16avos (vía eq16).
      NA
    }
    get_mi_pick <- function(n) {
      # Primero ver si hay un pick en proceso (input del Shiny)
      inp <- input[[paste0("pick_",n)]]
      if (!is.null(inp) && inp!="") return(inp)
      # Si no, ver si está guardado en Supabase
      if (nrow(mis_picks)==0 || !("partido_num" %in% names(mis_picks))) return("")
      r <- mis_picks |> filter(as.integer(partido_num)==as.integer(n)) |> pull(equipo_apostado)
      if (length(r)>0 && !is.na(r[1])) r[1] else ""
    }

    # Helper: equipos del partido n (desde Excel)
    equipos_de_partido <- function(n) {
      pfe_row <- pfe_snapshot |> filter(num==n)
      eq_a <- if(nrow(pfe_row)>0 && !is.na(pfe_row$local[1]) && pfe_row$local[1]!="") pfe_row$local[1] else ""
      eq_b <- if(nrow(pfe_row)>0 && !is.na(pfe_row$visitante[1]) && pfe_row$visitante[1]!="") pfe_row$visitante[1] else ""
      if(eq_a==""||eq_b=="") {
        fb <- tryCatch(equipos_16avos(n,st), error=function(e) c("",""))
        if(eq_a=="") eq_a <- fb[1]
        if(eq_b=="") eq_b <- if(length(fb)>1) fb[2] else ""
      }
      c(eq_a, eq_b)
    }

    # Para un partido fuente n, devuelve los 2 equipos posibles que pueden ganarlo
    # según el ESTADO ACTUAL de picks del jugador
    # - 16avos: los dos equipos del Excel
    # - 8avos+: los dos equipos que el jugador apostó como ganadores en sus partidos fuente
    equipos_posibles <- function(n) {
      if (n>=73 && n<=88) {
        # 16avos: del Excel (los 32 clasificados)
        return(equipos_de_partido(n))
      }
      # Para 8avos+: los dos picks que el jugador hizo en sus partidos fuente
      ant <- if(n>=89 && n<=96) CRUCES_8AVOS[[as.character(n)]]
             else if(n>=97 && n<=100) CRUCES_CUARTOS[[as.character(n)]]
             else if(n>=101 && n<=102) CRUCES_SEMIS[[as.character(n)]]
             else return(c("",""))
      # Para cada partido fuente, el pick del jugador (si lo hizo)
      get_pick_o_placeholder <- function(src) {
        pick <- get_mi_pick(src)
        if (pick!="") return(pick)
        paste0("(falta pick M",src,")")
      }
      c(get_pick_o_placeholder(ant[1]), get_pick_o_placeholder(ant[2]))
    }

    # Opciones para cada llave: dos sub-dropdowns, uno por partido fuente
    # Cada sub-dropdown tiene los 2 equipos del partido fuente
    construir_opciones <- function(pnum) {
      cruces_map <- c(CRUCES_8AVOS, CRUCES_CUARTOS, CRUCES_SEMIS)
      cruces <- cruces_map[[as.character(pnum)]]
      if (is.null(cruces)) return(NULL)
      list(
        list(m_src=cruces[1], equipos=equipos_posibles(cruces[1])),
        list(m_src=cruces[2], equipos=equipos_posibles(cruces[2]))
      )
    }

    opciones_8avos   <- setNames(lapply(names(CRUCES_8AVOS),   construir_opciones), names(CRUCES_8AVOS))
    opciones_cuartos <- setNames(lapply(names(CRUCES_CUARTOS), construir_opciones), names(CRUCES_CUARTOS))
    opciones_semis   <- setNames(lapply(names(CRUCES_SEMIS),   construir_opciones), names(CRUCES_SEMIS))

    # Calcular puntos por llave (20/9/0)
    pts_llave <- function(pnum) {
      gan_real <- get_gan_real(as.integer(pnum))
      if (is.na(gan_real)) return(NULL)
      mi_pick  <- get_mi_pick(as.integer(pnum))
      if (mi_pick=="") return(NULL)
      cruces <- switch(pnum,
        "89"=c(74,77),"90"=c(73,75),"91"=c(76,78),"92"=c(79,80),
        "93"=c(83,84),"94"=c(81,82),"95"=c(86,88),"96"=c(85,87),
        "97"=c(89,90),"98"=c(93,94),"99"=c(91,92),"100"=c(95,96),
        "101"=c(97,98),"102"=c(99,100),"104"=c(101,102), NULL)
      if (is.null(cruces)) return(NULL)
      eq_a_real <- get_gan_real(cruces[1])
      eq_b_real <- get_gan_real(cruces[2])
      if (mi_pick==gan_real) return(list(pts=20,cls="pts-20"))
      if ((!is.na(eq_a_real)&&mi_pick==eq_a_real)||(!is.na(eq_b_real)&&mi_pick==eq_b_real))
        return(list(pts=9,cls="pts-9"))
      return(list(pts=0,cls="pts-0"))
    }

    # Render de una llave
    render_llave <- function(pnum, opciones, bloqueada=FALSE, is_16avos=FALSE) {
      pnum_chr <- as.character(pnum)
      mi_pick  <- get_mi_pick(as.integer(pnum))
      gan_real <- get_gan_real(as.integer(pnum))
      pts_info <- if (!is_16avos) pts_llave(pnum_chr) else NULL

      if (is_16avos) {
        # Solo informativo
        eq <- tryCatch(equipos_16avos(as.integer(pnum),st),error=function(e)c("?","?"))
        return(tags$div(class="llave-card",
          tags$div(style="font-size:.7em;color:#888;",paste0("M",pnum)),
          tags$div(style="font-weight:600;",eq[1]),
          tags$div(style="color:#888;font-size:.85em;","vs"),
          tags$div(style="font-weight:600;",eq[2]),
          if (!is.na(gan_real))
            tags$div(style="color:#5aae61;font-size:.8em;font-weight:700;margin-top:4px;",
                     paste0("✓ ",gan_real))
        ))
      }

      # Llave apostable
      tags$div(class="llave-card",
        tags$div(style="font-size:.7em;color:#888;margin-bottom:4px;",
                 paste0("M",pnum," — ",
                        if (!is.na(gan_real)) paste0("Real: ",gan_real) else "Sin resultado")),
        if (!bloqueada && !ya_enviado) {
          selectInput(paste0("pick_",pnum_chr),
            label=NULL,
            choices=c("Elegir..."="", opciones$a, opciones$b),
            selected=mi_pick, width="100%")
        } else {
          tags$div(style="font-weight:700;color:#0f3460;",
                   if (mi_pick!="") mi_pick else tags$em(style="color:#888;","Sin pick"))
        },
        if (!is.null(pts_info)) {
          tags$div(class=c("llave-pts",pts_info$cls),
                   paste0(pts_info$pts," pts"))
        }
      )
    }


    # ── Helpers de equipos ────────────────────────────────────────
    eq16 <- function(n) {
      pfe_row <- pfe_snapshot |> filter(num==n)
      eq_a <- if(nrow(pfe_row)>0 && !is.na(pfe_row$local[1]) && pfe_row$local[1]!="") pfe_row$local[1] else ""
      eq_b <- if(nrow(pfe_row)>0 && !is.na(pfe_row$visitante[1]) && pfe_row$visitante[1]!="") pfe_row$visitante[1] else ""
      if(eq_a==""||eq_b=="") {
        fb <- tryCatch(equipos_16avos(n,st), error=function(e) c(DESC_16AVOS[as.character(n)],""))
        if(eq_a=="") eq_a <- fb[1]
        if(eq_b=="") eq_b <- if(length(fb)>1) fb[2] else ""
      }
      c(eq_a, eq_b)
    }

    pts_color <- function(p) {
      if(is.null(p)) return("")
      if(p$pts==20) "color:#1b7837;font-weight:800;"
      else if(p$pts==9) "color:#f5a623;font-weight:700;"
      else "color:#c0392b;font-weight:600;"
    }

    # ── Celda de equipo en 16avos (solo info) ─────────────────────
    td16 <- function(n, rowspan=2) {
      eqs <- eq16(n)
      gan <- get_gan_real(n)
      won_a <- !is.na(gan) && gan==eqs[1]
      won_b <- !is.na(gan) && gan==eqs[2]
      tags$td(
        rowspan=rowspan,
        style="padding:0;vertical-align:middle;width:130px;",
        tags$div(class="b16-cell",
          tags$div(class=paste0("b16-team",if(won_a)" b-won" else ""), eqs[1]),
          tags$div(class=paste0("b16-team",if(won_b)" b-won" else ""), eqs[2])
        )
      )
    }

    # ── Celda apostable (8avos/cuartos/semis/final) ───────────────
    tdpick <- function(pnum, opciones, rowspan=2, is_final=FALSE) {
      pnum_chr <- as.character(pnum)
      mi_pick  <- get_mi_pick(pnum)
      gan_real <- get_gan_real(pnum)
      pts_info <- pts_llave(pnum_chr)

      inner <- if(ya_enviado || is.null(opciones)) {
        if(mi_pick!="") {
          tags$div(class="bpick-chosen",
            if(!is.na(gan_real) && mi_pick==gan_real) "✓ " else "",
            mi_pick,
            if(!is.null(pts_info))
              tags$span(style=paste0("font-size:.75em;margin-left:4px;",pts_color(pts_info)),
                paste0("(",pts_info$pts,"p)"))
          )
        } else tags$div(class="bpick-empty","—")
      } else {
        selectInput(paste0("pick_",pnum_chr), label=NULL,
          choices=c("Elegir..."="", opciones$a, opciones$b),
          selected=mi_pick, width="100%")
      }

      tags$td(
        rowspan=rowspan,
        style="padding:0;vertical-align:middle;",
        tags$div(
          class=paste0("bpick-cell", if(is_final)" bpick-final" else ""),
          tags$div(class="bpick-num", paste0("M",pnum,
            if(!is.na(gan_real)) paste0(" · ✓ ",gan_real) else "")),
          inner
        )
      )
    }

    # ── Celda vacía con línea de conexión ─────────────────────────
    tdspc <- function(rowspan=1, side="right") {
      tags$td(rowspan=rowspan, class=paste0("bspc bspc-",side), style="width:14px;padding:0;")
    }

    # Mapa: 16avos izquierda y sus 8avos (orden de arriba a abajo en el bracket)
    # Los 8 pares de 16avos que se enfrentan para dar lugar a cada 8avos
    # Layout del bracket (mitad izquierda, de arriba a abajo):
    # 16avos: M73,M75 → 8avos M90 → Cuartos M97 → Semi M101 → Final M104
    # 16avos: M79,M81 → 8avos M92
    # 16avos: M83,M85 → 8avos M94 → Cuartos M98 → Semi M101
    # 16avos: M87,? → 8avos M96 (pero M95,M96 son CRUCES_8AVOS$96=c(85,87))
    # Voy a usar el orden dado por CRUCES_8AVOS y CRUCES_CUARTOS

    # Orden visual del bracket (izquierda → derecha = 16avos → final):
    # Mitad izquierda (arriba): 73,75 → 90 → 97 → 101 → 104
    # Mitad izquierda (abajo):  79,81 → 92 → 97
    # Mitad izquierda (centro superior): 83,85 → 94 → 98 → 101
    # Mitad izquierda (centro inferior): 87,... → 96 → 98
    # Mitad derecha (arriba): 74,76 → 89 → 99 → 102 → 104
    # Mitad derecha (abajo):  80,82 → 91 → 99
    # etc.

    # Definición ordenada del bracket:
    # izquierda_pares: list of list(m16a, m16b, m8)
    iz <- list(
      list(m16=c(73,75), m8="90"),
      list(m16=c(79,81), m8="92"),
      list(m16=c(83,85), m8="94"),
      list(m16=c(87,88), m8="96")   # M88: 2°D vs 2°G
    )
    de <- list(
      list(m16=c(74,77), m8="89"),
      list(m16=c(76,78), m8="91"),
      list(m16=c(80,82), m8="93"),   # ajustar según reglamento
      list(m16=c(86,84), m8="95")
    )
    # Cuartos: iz[1,2]→97, iz[3,4]→98; de[1,2]→99, de[3,4]→100
    # Semis: 97,98→101; 99,100→102; Final: 101,102→104

    # ── Construir filas de la tabla ───────────────────────────────
    # Cada par de 16avos ocupa 4 filas (una fila por equipo × 2 partidos)
    # Cada 8avos ocupa 4 filas (rowspan=4)
    # Cada cuarto ocupa 8 filas (rowspan=8)
    # Semis ocupa 16 filas (rowspan=16)
    # Final ocupa 32 filas (rowspan=32) — toda la tabla

    # Altura de cada fila base: 28px → total 32×28 = 896px
    ROW_H <- 28

    make_team_row <- function(equipo, is_winner=FALSE) {
      tags$tr(style=paste0("height:",ROW_H,"px;"),
        tags$td(
          class=paste0("bteam",if(is_winner)" bteam-won" else ""),
          equipo
        )
      )
    }

    make_spacer_row <- function() {
      tags$tr(style=paste0("height:",ROW_H,"px;"),
        tags$td(class="bteam-spacer")
      )
    }

    # Construir celdas de 16avos como listas de tds para insertar en filas
    cell16 <- function(n, first_row_of_pair) {
      # Retorna el td con rowspan=2 solo en la primera fila del par
      if(!first_row_of_pair) return(NULL)
      eqs <- eq16(n)
      gan <- get_gan_real(n)
      tags$td(rowspan=2, style="padding:3px 4px;vertical-align:middle;width:125px;",
        tags$div(class="b16-wrap",
          tags$div(class=paste0("bteam",if(!is.na(gan)&&gan==eqs[1])" bteam-won" else ""),
            style="border-bottom:1px solid #eee;padding-bottom:2px;margin-bottom:2px;",
            paste0("M",n," ",eqs[1])
          ),
          tags$div(class=paste0("bteam",if(!is.na(gan)&&gan==eqs[2])" bteam-won" else ""),
            eqs[2]
          ),
          if(!is.na(gan))
            tags$div(style="font-size:.7em;color:#1b7837;font-weight:700;margin-top:2px;",
              paste0("✓ ",gan))
        )
      )
    }

    # ── Función principal: renderizar una mitad del bracket ────────
    # returns: lista de tds por posición
    # Mejor enfoque: renderizar el bracket completo como HTML string con JS para interactividad

    # Construir selectInput como HTML para usarlo en la tabla
    pick_content <- function(pnum_chr, opc) {
      mi_pick  <- get_mi_pick(as.integer(pnum_chr))
      gan_real <- get_gan_real(as.integer(pnum_chr))
      pts_info <- pts_llave(pnum_chr)

      if(ya_enviado) {
        tagList(
          tags$div(class=paste0("bpick-chosen",if(!is.na(gan_real)&&mi_pick==gan_real)" bpick-win" else ""),
            if(mi_pick!="") mi_pick else tags$em(style="color:#aaa;","—")
          ),
          if(!is.null(pts_info))
            tags$div(style=paste0("font-size:.72em;margin-top:2px;",pts_color(pts_info)),
              paste0(pts_info$pts," pts"))
        )
      } else if(!is.null(opc)) {
        tagList(
          selectInput(paste0("pick_",pnum_chr), label=NULL,
            choices=c("Elegir..."="", opc$a, opc$b),
            selected=mi_pick, width="130px"),
          if(!is.null(pts_info))
            tags$div(style=paste0("font-size:.72em;margin-top:2px;",pts_color(pts_info)),
              paste0(pts_info$pts," pts"))
        )
      } else {
        tags$div(class="bpick-empty","Sin opciones")
      }
    }

    # ── Renderizar el bracket como tabla HTML con rowspan ─────────
    # Estructura de 32 filas base, 8px de alto cada una
    # Col structure: 16L | conn | 8L | conn | 4L | conn | Semi | conn | Final | conn | Semi | conn | 4R | conn | 8R | conn | 16R

    H <- 36  # altura de cada fila en px

    # Helper: celda de partido de 16avos (2 filas de alto)
    c16 <- function(n) {
      eqs  <- eq16(n)
      gan  <- get_gan_real(n)
      mi_pick <- get_mi_pick(n)  # a quién marcó el jugador que pasa
      # Resaltar el pick del jugador (quién clasifica según él)
      sel_a <- mi_pick!="" && mi_pick==eqs[1]
      sel_b <- mi_pick!="" && mi_pick==eqs[2]
      tags$td(rowspan=4, style=paste0("padding:2px 4px;width:115px;vertical-align:middle;"),
        tags$div(style="border:1px solid #cdd3da;border-radius:5px;overflow:hidden;background:white;",
          tags$div(style="font-size:.62em;color:#aaa;padding:2px 7px 0;",paste0("M",n)),
          tags$div(style=paste0("padding:4px 7px;border-bottom:1px solid #eee;font-size:.8em;",
                                if(sel_a)"font-weight:700;background:#1b7837;color:white;" else "font-weight:500;color:#333;"),
            paste0(if(sel_a)"▶ " else "",eqs[1])),
          tags$div(style=paste0("padding:4px 7px;font-size:.8em;",
                                if(sel_b)"font-weight:700;background:#1b7837;color:white;" else "font-weight:500;color:#333;"),
            paste0(if(sel_b)"▶ " else "",eqs[2])),
          if(!is.na(gan))
            tags$div(style="padding:2px 7px;background:#e8f5e9;font-size:.7em;color:#1b7837;font-weight:700;",
              paste0("Real: ",gan))
        )
      )
    }

    cpick <- function(pnum, opc, rs) {
      pnum_chr <- as.character(pnum)
      gan_real <- get_gan_real(pnum)
      pts_info <- pts_llave(pnum_chr)

      min_h <- paste0(rs*H,"px")

      # opc es lista de 2 elementos, cada uno {m_src, equipos}
      # pick del partido ACTUAL (quién el jugador eligió que avanza de pnum)
      pick_avanza <- get_mi_pick(pnum)

      render_subdrop <- function(sub) {
        m_src <- sub$m_src
        eqs   <- sub$equipos
        real_src <- get_gan_real(m_src)
        mi_pick_src <- get_mi_pick(m_src)
        # Validar opciones: filtrar vacíos y placeholders
        eqs_valid <- eqs[eqs!="" & !grepl("^\\(falta", eqs) & !is.na(eqs)]
        # Si el partido fuente ya se jugó: mostrar el ganador real (no editable)
        if (!is.na(real_src)) {
          # ¿Este equipo es el que el jugador eligió que avanza de pnum?
          avanza <- pick_avanza!="" && real_src==pick_avanza
          return(tags$div(style=paste0("border-radius:4px;padding:5px 7px;font-size:.78em;font-weight:700;",
            if(avanza)"background:#1b7837;color:white;" else "background:#eee;color:#555;"),
            paste0(if(avanza)"▶ " else "",real_src," (M",m_src,")")))
        }
        # Si está enviado: mostrar el pick, resaltando en verde solo si avanza
        if (ya_enviado) {
          es_avanza <- mi_pick_src!="" && mi_pick_src==pick_avanza
          return(tags$div(style=paste0("padding:5px 7px;font-size:.78em;font-weight:700;border-radius:4px;",
            if(es_avanza)"background:#1b7837;color:white;" else "background:#f0f0f0;color:#666;"),
            if(mi_pick_src!="") paste0(if(es_avanza)"▶ " else "",mi_pick_src) else tags$em(style="color:#aaa;font-weight:400;","Sin pick")))
        }
        # SIEMPRE renderizar el dropdown — las opciones se actualizan vía updateSelectInput
        tagList(
          tags$div(style="font-size:.62em;color:#888;margin-bottom:1px;",paste0("Ganador M",m_src,":")),
          selectInput(paste0("pick_",m_src), label=NULL,
            choices=c("Elegir..."="", eqs_valid),
            selected=mi_pick_src, width="115px")
        )
      }

      tags$td(rowspan=rs, style="padding:2px 4px;width:135px;vertical-align:middle;",
        tags$div(style=paste0("border:1px solid #cdd3da;border-radius:6px;background:white;",
                              "min-height:",min_h,";padding:6px 7px;",
                              "display:flex;flex-direction:column;justify-content:center;gap:6px;"),
          tags$div(style="font-size:.65em;color:#aaa;",
            paste0("M",pnum, if(!is.na(gan_real)) paste0(" · ✓ ",gan_real) else "")),
          if (is.null(opc) || length(opc)<2) {
            tags$div(style="color:#aaa;font-size:.82em;","Sin cruces definidos")
          } else {
            tagList(
              render_subdrop(opc[[1]]),
              render_subdrop(opc[[2]])
            )
          },
          if(!is.null(pts_info))
            tags$div(style=paste0("font-size:.7em;text-align:center;",pts_color(pts_info)),
              paste0(pts_info$pts," pts"))
        )
      )
    }

    # Celda para Final / 3er puesto / Semis (solo lectura, reactiva a picks en curso)
    cfinal <- function(pnum, rs, titulo) {
      pnum_chr <- as.character(pnum)
      min_h <- paste0(rs*H,"px")
      brd <- if(pnum==104) "2px solid #f5c518" else if(pnum==103) "1px solid #cdd3da" else "2px solid #f5c518"
      bg  <- if(pnum==104) "#fffbea" else if(pnum==103) "#fafafa" else "#fffbea"
      tags$td(rowspan=rs, style="padding:2px 4px;width:135px;vertical-align:middle;",
        tags$div(style=paste0("border:",brd,";border-radius:6px;background:",bg,";",
                              "min-height:",min_h,";padding:8px;",
                              "display:flex;flex-direction:column;justify-content:center;gap:4px;text-align:center;"),
          tags$div(style="font-size:.7em;font-weight:800;color:#0f3460;letter-spacing:1px;",titulo),
          # Contenido reactivo
          uiOutput(paste0("cfinal_content_",pnum))
        )
      )
    }

    # Helper: celda conector (líneas de bracket)
    cc <- function(rs, type="right") {
      # type: right = línea derecha cerrando dos fichas hacia arriba
      #       left  = línea izquierda abriendo hacia dos fichas
      style <- switch(type,
        "right" = paste0("width:14px;padding:0;border-right:2px solid #bbb;",
                          "border-top:2px solid #bbb;border-bottom:2px solid #bbb;"),
        "left"  = paste0("width:14px;padding:0;border-left:2px solid #bbb;",
                          "border-top:2px solid #bbb;border-bottom:2px solid #bbb;"),
        "center-right" = "width:14px;padding:0;border-right:2px solid #bbb;",
        "center-left"  = "width:14px;padding:0;border-left:2px solid #bbb;"
      )
      tags$td(rowspan=rs, style=style)
    }

    # ── Construcción de la tabla con 32 filas ─────────────────────
    # Mitad izquierda (4 pares → 4 8avos → 2 cuartos → 1 semi)
    # Mitad derecha (4 pares → 4 8avos → 2 cuartos → 1 semi)
    # Centro: 2 semis + final

    # Construir filas 1-32:
    # Cada par de 16avos: 4 filas (2 equipos × 2 partidos = 4 filas)
    # Los 8 pares izquierda: filas 1-4, 5-8, 9-12, 13-16, 17-20, 21-24, 25-28, 29-32
    # Pero hay 8 partidos de 16avos por lado → necesito 16 filas por lado
    # Total con 8 pares por lado: 8×4 = 32 filas — correcto

    # Layout visual del bracket, alineado con los CRUCES FIFA reales
    # Izquierda alimenta M97 (=M89+M90) y M98 (=M93+M94)
    # Derecha alimenta M99 (=M91+M92) y M100 (=M95+M96)
    # M97 va arriba izquierda, M98 abajo izquierda
    # M99 va arriba derecha, M100 abajo derecha
    # Semi izq M101 = M97+M98, Semi der M102 = M99+M100
    iz_order <- list(
      list(m16=CRUCES_8AVOS[["89"]], m8="89"),  # filas 1-8 (alimenta M97)
      list(m16=CRUCES_8AVOS[["90"]], m8="90"),  # filas 9-16 (alimenta M97)
      list(m16=CRUCES_8AVOS[["93"]], m8="93"),  # filas 17-24 (alimenta M98)
      list(m16=CRUCES_8AVOS[["94"]], m8="94")   # filas 25-32 (alimenta M98)
    )
    de_order <- list(
      list(m16=CRUCES_8AVOS[["91"]], m8="91"),  # filas 1-8 (alimenta M99)
      list(m16=CRUCES_8AVOS[["92"]], m8="92"),  # filas 9-16 (alimenta M99)
      list(m16=CRUCES_8AVOS[["95"]], m8="95"),  # filas 17-24 (alimenta M100)
      list(m16=CRUCES_8AVOS[["96"]], m8="96")   # filas 25-32 (alimenta M100)
    )

    n_rows <- 32  # total de filas de la tabla

    rows <- lapply(seq_len(n_rows), function(r) {

      cells <- list()

      # ── Columnas IZQUIERDA ──────────────────────────────────────
      # Cada bloque de 16avos ocupa 8 filas (2 partidos × 4 filas cada uno)
      blk_iz <- ceiling(r / 8)   # 1-4
      r_in_blk <- ((r-1) %% 8) + 1  # 1-8 dentro del bloque

      iz_b <- iz_order[[blk_iz]]

      # Col 1: 16avos izquierda (cada partido = 4 filas, cada 2 equipos = 2 filas)
      # Partido A del bloque: filas 1-4; partido B: filas 5-8
      pair_idx <- ceiling(r_in_blk / 4)  # 1 o 2 (qué partido del par)
      r_in_pair <- ((r_in_blk-1) %% 4) + 1  # 1-4 dentro del partido

      if(r_in_pair == 1) {  # Primera fila del partido → insertar celda con rowspan=4
        m16_n <- iz_b$m16[pair_idx]
        cells <- c(cells, list(c16(m16_n)))
      }

      # Col 2: conector derecho 16avos→8avos
      if(r_in_blk==1) cells <- c(cells, list(cc(8,"right")))

      # Col 3: 8avos izquierda (cada 8avos ocupa 8 filas)
      if(r_in_blk==1) cells <- c(cells, list(cpick(as.integer(iz_b$m8), opciones_8avos[[iz_b$m8]], 8)))

      # Col 4: conector 8avos→cuartos (16 filas por cuarto)
      r_in_half <- ((r-1) %% 16) + 1
      if(r_in_half==1) cells <- c(cells, list(cc(16,"right")))

      # Col 5: cuartos izquierda — M97 (filas 1-16), M98 (filas 17-32)
      blk_q_iz <- ceiling(r / 16)  # 1 o 2
      if(r_in_half==1) {
        q_pnum <- if(blk_q_iz==1) "97" else "98"
        cells <- c(cells, list(cpick(as.integer(q_pnum), opciones_cuartos[[q_pnum]], 16)))
      }

      # Col 6: conector cuartos→semi (32 filas por semi)
      if(r==1) cells <- c(cells, list(cc(32,"right")))

      # Col 7: SEMIFINAL izquierda (32 filas) — dropdown M101, igual que cuartos
      if(r==1) cells <- c(cells, list(cpick(101, opciones_semis[["101"]], 32)))

      # Col 8: conector semi→final
      if(r==1) cells <- c(cells, list(cc(32,"right")))

      # Col 9: FINAL + 3er PUESTO
      # FINAL: 2 dropdowns para elegir quién pasa de cada semi (M101, M102)
      # 3er PUESTO: autocompletado con los perdedores
      if(r==1) {
        # Dropdown para elegir el ganador de una semi (quién pasa a la final)
        finalista_drop <- function(psemi) {
          # Los dos rivales de la semi = los dos picks del jugador (ganadores de los cuartos)
          eqs <- equipos_posibles(psemi)
          eqs_v <- eqs[eqs!="" & !grepl("^\\(falta",eqs) & !is.na(eqs)]
          real <- get_gan_real(psemi)
          if(!is.na(real))
            return(tags$div(style="background:#e8f5e9;border-radius:4px;padding:5px 7px;font-size:.78em;font-weight:700;color:#1b7837;",
                            paste0("✓ ",real," (M",psemi,")")))
          if(ya_enviado) {
            mp <- get_mi_pick(psemi)
            return(tags$div(style="font-size:.78em;color:#0f3460;font-weight:700;padding:3px 0;",
              if(mp!="") mp else tags$em(style="color:#aaa;font-weight:400;","Sin pick")))
          }
          tagList(
            tags$div(style="font-size:.62em;color:#888;",paste0("Pasa de M",psemi,":")),
            selectInput(paste0("pick_",psemi), label=NULL,
              choices=c("Elegir..."="", eqs_v),
              selected=get_mi_pick(psemi), width="120px")
          )
        }
        cells <- c(cells, list(tags$td(rowspan=32, style="padding:2px 4px;width:140px;vertical-align:middle;",
          tags$div(style="display:flex;flex-direction:column;gap:8px;height:100%;justify-content:center;",
            # Bloque de la FINAL — elegir los dos finalistas
            tags$div(style="border:2px solid #f5c518;border-radius:6px;background:#fffbea;padding:8px;",
              tags$div(style="font-size:.7em;font-weight:800;color:#0f3460;letter-spacing:1px;text-align:center;margin-bottom:5px;","🏆 FINAL"),
              finalista_drop(101),
              finalista_drop(102)
            ),
            # Bloque del 3er PUESTO (autocompletado: los perdedores de las semis)
            tags$div(style="border:1px solid #cdd3da;border-radius:6px;background:#fafafa;padding:8px;text-align:center;",
              tags$div(style="font-size:.7em;font-weight:800;color:#0f3460;letter-spacing:1px;","🥉 3er PUESTO"),
              uiOutput("cfinal_content_103")
            )
          )
        )))
      }

      # Col 10: conector final→semi derecha
      if(r==1) cells <- c(cells, list(cc(32,"left")))

      # Col 11: SEMIFINAL derecha (32 filas) — dropdown M102, igual que cuartos
      if(r==1) cells <- c(cells, list(cpick(102, opciones_semis[["102"]], 32)))

      # Col 12: conector semi→cuartos derecha
      if(r==1) cells <- c(cells, list(cc(32,"left")))

      # ── Columnas DERECHA (espejo de izquierda) ──────────────────
      blk_de <- ceiling(r / 8)
      de_b <- de_order[[blk_de]]

      # Col 13: cuartos derecha
      blk_q_de <- ceiling(r / 16)
      if(r_in_half==1) {
        q_pnum <- if(blk_q_de==1) "99" else "100"
        cells <- c(cells, list(cpick(as.integer(q_pnum), opciones_cuartos[[q_pnum]], 16)))
      }

      # Col 14: conector cuartos→8avos derecha
      if(r_in_half==1) cells <- c(cells, list(cc(16,"left")))

      # Col 15: 8avos derecha
      if(r_in_blk==1) cells <- c(cells, list(cpick(as.integer(de_b$m8), opciones_8avos[[de_b$m8]], 8)))

      # Col 16: conector 8avos→16avos
      if(r_in_blk==1) cells <- c(cells, list(cc(8,"left")))

      # Col 17: 16avos derecha
      if(r_in_pair==1) {
        m16_n <- de_b$m16[pair_idx]
        cells <- c(cells, list(c16(m16_n)))
      }

      do.call(tags$tr, c(list(style=paste0("height:",H,"px;")), cells))
    })

    # Cabecera de la tabla
    hdr <- tags$thead(
      tags$tr(style="background:#0f3460;",
        tags$th(colspan=1, style="color:#f5c518;text-align:center;font-size:.75em;padding:7px 4px;font-weight:800;letter-spacing:1px;","16avos"),
        tags$th(colspan=2, style="color:#f5c518;text-align:center;font-size:.75em;padding:7px 4px;font-weight:800;letter-spacing:1px;","8avos"),
        tags$th(colspan=2, style="color:#f5c518;text-align:center;font-size:.75em;padding:7px 4px;font-weight:800;letter-spacing:1px;","Cuartos"),
        tags$th(colspan=2, style="color:#f5c518;text-align:center;font-size:.75em;padding:7px 4px;font-weight:800;letter-spacing:1px;","Semis"),
        tags$th(colspan=1, style="color:#f5c518;text-align:center;font-size:.75em;padding:7px 4px;font-weight:800;letter-spacing:1px;background:#1a1a2e;","Final"),
        tags$th(colspan=2, style="color:#f5c518;text-align:center;font-size:.75em;padding:7px 4px;font-weight:800;letter-spacing:1px;","Semis"),
        tags$th(colspan=2, style="color:#f5c518;text-align:center;font-size:.75em;padding:7px 4px;font-weight:800;letter-spacing:1px;","Cuartos"),
        tags$th(colspan=2, style="color:#f5c518;text-align:center;font-size:.75em;padding:7px 4px;font-weight:800;letter-spacing:1px;","8avos"),
        tags$th(colspan=1, style="color:#f5c518;text-align:center;font-size:.75em;padding:7px 4px;font-weight:800;letter-spacing:1px;","16avos")
      )
    )

    tagList(
      if (ya_enviado) {
        tags$div(style="background:#e8f5e9;border-left:4px solid #5aae61;padding:12px 16px;border-radius:4px;margin-bottom:16px;",
          tags$strong(style="color:#2e7d32;","✓ Tu cuadro está enviado y bloqueado."))
      } else {
        tags$div(style="background:#fff8e1;border-left:4px solid #f5c518;padding:12px 16px;border-radius:0 6px 6px 0;margin-bottom:16px;",
          tags$strong("Completá el cuadro de 8avos hasta la final y enviá. "),
          tags$span(style="color:#666;","Las listas se actualizan según tus picks anteriores. Cuando termines, hacé click en Enviar."))
      },

      tags$div(style="overflow-x:auto;-webkit-overflow-scrolling:touch;",
        tags$table(
          style="border-collapse:collapse;table-layout:fixed;",
          hdr,
          do.call(tags$tbody, rows)
        )
      ),

      if(!ya_enviado) tagList(
        tags$div(style="margin-top:16px;text-align:center;",
          actionButton("btn_enviar_cuadro","📤 Enviar mi cuadro",
            style="background:#0f3460;color:white;font-weight:700;padding:12px 32px;font-size:1em;border-radius:8px;"),
          uiOutput("msg_envio_cuadro")
        )
      )
    )
    }, error=function(e) tags$div(style="color:red;padding:16px;",
      tags$strong("Error al cargar el cuadro: "), conditionMessage(e)))
  })

  output$msg_envio_cuadro <- renderUI(NULL)

  observeEvent(input$btn_enviar_cuadro, {
    u <- usuario(); req(u)
    # Los picks reales son para los partidos 73-102 (todos los partidos fuente)
    # 104 (final) y 103 (3er puesto) se ingresan al inicio en otra parte
    partidos_cuadro_nums <- c(73:102)
    picks_vals <- lapply(partidos_cuadro_nums, function(n) input[[paste0("pick_",n)]])
    # El cuadro es PURAMENTE PREDICTIVO: se requieren picks para TODOS los
    # partidos 73-102, sin importar si ya se jugaron en la realidad.
    falta <- c()
    for (i in seq_along(partidos_cuadro_nums)) {
      n <- partidos_cuadro_nums[i]
      v <- picks_vals[[i]]
      if (is.null(v) || v=="") falta <- c(falta, n)
    }
    if (length(falta)>0) {
      output$msg_envio_cuadro <- renderUI(
        tags$p(style="color:red;font-size:.88em;margin-top:6px;",
               paste0("Faltan picks: M", paste(falta, collapse=", M"))))
      return()
    }

    # Construir todas las filas a insertar
    rows <- list()
    for (i in seq_along(partidos_cuadro_nums)) {
      n <- partidos_cuadro_nums[i]
      val <- picks_vals[[i]]
      if (is.null(val) || val=="") next
      rows[[length(rows)+1]] <- list(
        jugador_id      = u$id,
        ronda           = ronda_de_partido(n),
        partido_num     = as.integer(n),
        equipo_apostado = val
      )
    }
    if (length(rows)==0) {
      output$msg_envio_cuadro <- renderUI(
        tags$p(style="color:red;font-size:.88em;margin-top:6px;","No hay picks para guardar."))
      return()
    }

    # Borrar picks previos de este jugador (por si reenvía) y luego insertar en bloque
    tryCatch({
      request(paste0(SUPABASE_URL,"/rest/v1/picks_eliminatorios")) |>
        req_url_query(jugador_id=paste0("eq.",u$id)) |>
        req_headers(!!!make_hdrs("return=minimal")) |>
        req_method("DELETE") |>
        req_error(is_error=function(resp) FALSE) |>
        req_perform()
    }, error=function(e) NULL)

    res <- sb_post_bulk("picks_eliminatorios", rows)

    if (!res$ok) {
      output$msg_envio_cuadro <- renderUI(
        tags$p(style="color:red;font-weight:700;margin-top:6px;",
               paste0("⛔ NO se guardó (",res$status,"). Reintentá. Detalle: ",res$msg)))
      return()
    }

    # Verificación: releer y contar
    check <- sb_get("picks_eliminatorios",
      paste0("?jugador_id=eq.",u$id,"&select=partido_num"))
    n_guardados <- if (is.null(check)) 0 else length(check$partido_num)
    if (n_guardados < length(rows)) {
      output$msg_envio_cuadro <- renderUI(
        tags$p(style="color:#c0392b;font-weight:700;margin-top:6px;",
               paste0("⚠ Se guardaron ",n_guardados," de ",length(rows),
                      " picks. Reintentá para completar.")))
      return()
    }

    output$msg_envio_cuadro <- renderUI(
      tags$p(style="color:#2e7d32;font-weight:700;margin-top:6px;",
             paste0("✓ Cuadro enviado y bloqueado (",n_guardados," picks guardados).")))
    usuario(modifyList(u, list(picks_enviados=TRUE)))
    cache_invalidar("picks_eliminatorios")  # ver el cambio recién guardado
    cuadro_refresh(cuadro_refresh()+1)  # forzar re-render del cuadro
  })

  # ── Cascada de dropdowns: UN solo observe con debounce, en orden topológico.
  # Reemplaza la cadena de observers que se disparaban entre sí (causa del
  # tintineo/intercambio de opciones). Recalcula todo de una pasada tras cada
  # cambio del usuario, con un pequeño retardo (debounce) para estabilizar.
  cascada_estado <- new.env()

  # Reactivo que junta TODOS los picks fuente posibles (16avos..semis)
  picks_cascada <- reactive({
    nums <- 73:102
    sapply(nums, function(n) {
      v <- input[[paste0("pick_",n)]]
      if (is.null(v)) "" else v
    })
  }) |> debounce(350)

  observe({
    req(usuario())
    picks_cascada()  # se dispara (con debounce) cuando el usuario cambia algún pick
    # Recalcular en ORDEN: primero 8avos, luego cuartos, luego semis.
    targets <- c(names(CRUCES_8AVOS), names(CRUCES_CUARTOS), names(CRUCES_SEMIS))
    for (key in targets) {
      p_target <- as.integer(key)
      eqs <- equipos_posibles_global(p_target)
      eqs_valid <- eqs[eqs != "" & !is.na(eqs)]
      sel_actual <- isolate(input[[paste0("pick_",p_target)]])
      sel_final <- if (!is.null(sel_actual) && sel_actual %in% eqs_valid) sel_actual else ""
      firma <- paste(c(eqs_valid, "|", sel_final), collapse="~")
      if (!identical(get0(key, envir=cascada_estado, ifnotfound=NULL), firma)) {
        assign(key, firma, envir=cascada_estado)
        updateSelectInput(session, paste0("pick_",p_target),
          choices  = c("Elegir..."="", eqs_valid),
          selected = sel_final)
      }
    }
  })

  # ── Contenido de la FINAL (M104): los dos finalistas ──────────
  output$cfinal_content_104 <- renderUI({
    f1 <- get_mi_pick_global(101)
    f2 <- get_mi_pick_global(102)
    gan_real <- get_gan_real_global(104)
    tagList(
      tags$div(style="font-size:.62em;color:#aaa;",
        paste0("M104", if(!is.na(gan_real)) paste0(" · ✓ ",gan_real) else "")),
      if(f1!="" || f2!="") {
        tags$div(style="font-size:.82em;color:#0f3460;font-weight:700;",
          paste0(if(f1!="")f1 else "?", " vs ", if(f2!="")f2 else "?"))
      } else tags$em(style="color:#aaa;font-size:.82em;","Faltan finalistas")
    )
  })

  # ── Contenido del 3er PUESTO (M103): autocompletado ────────────
  # Los dos perdedores de las semis (el equipo de cada semi que NO eligió como finalista)
  output$cfinal_content_103 <- renderUI({
    # Perdedor de M101: el equipo de M101 que NO es el pick del jugador
    perdedor_de <- function(psemi) {
      pick <- get_mi_pick_global(psemi)
      if (pick=="") return("")
      eqs <- equipos_posibles_global(psemi)  # los 2 finalistas posibles
      eqs <- eqs[eqs!="" & !is.na(eqs)]
      perdedor <- eqs[eqs != pick]
      if (length(perdedor)>0) perdedor[1] else ""
    }
    p1 <- perdedor_de(101)
    p2 <- perdedor_de(102)
    gan_real <- get_gan_real_global(103)
    tagList(
      tags$div(style="font-size:.62em;color:#aaa;",
        paste0("M103", if(!is.na(gan_real)) paste0(" · ✓ ",gan_real) else "")),
      if(p1!="" || p2!="") {
        tags$div(style="font-size:.82em;color:#0f3460;font-weight:700;",
          paste0(if(p1!="")p1 else "?", " vs ", if(p2!="")p2 else "?"))
      } else tags$em(style="color:#aaa;font-size:.82em;","Definí las semis")
    )
  })

  # ══════════════════════════════════════════════════════════════
  # PESTAÑA: RESULTADOS FE (ingreso de resultados partido a partido)
  # ══════════════════════════════════════════════════════════════
  output$resultados_fe_ui <- renderUI({
    u <- usuario(); req(u)
    resultados_refresh()  # dependencia reactiva: re-render al ingresar
    # IMPORTANTE: aislamos los datos del poll de 5 min para que el re-render
    # automático NO borre los marcadores que el usuario está cargando.
    # La UI solo se reconstruye al cambiar de usuario o al ingresar una fase.
    st  <- isolate(st_reales())
    rfe <- isolate(resultados_fe_db())
    ganadores <- if (nrow(rfe)>0 && "partido_num" %in% names(rfe)) {
      rfe |> group_by(partido_num) |>
        slice_max(created_at,n=1,with_ties=FALSE) |>
        select(partido_num,equipo_ganador,goles_local,goles_visitante) |> ungroup()
    } else data.frame(partido_num=integer(), equipo_ganador=character(),
                      goles_local=integer(), goles_visitante=integer())

    pfe_excel <- isolate(partidos_fe_excel())

    tagList(
      # Cartel general de instrucciones
      tags$div(style="background:#e3f2fd;border-left:4px solid #1976d2;border-radius:4px;padding:12px 16px;margin-bottom:14px;",
        tags$div(style="font-weight:700;color:#0d47a1;margin-bottom:4px;","📋 Cómo funciona esta sección"),
        tags$div(style="color:#0d47a1;font-size:.9em;line-height:1.5;",
          "Los resultados se ingresan ", tags$strong("por fase completa"), ", no partido por partido. ",
          "Una fase se habilita para el ingreso recién cuando ", tags$strong("todos sus partidos tienen los dos equipos definidos"), ". ",
          "Cuando eso pasa, cargás todos los marcadores de la fase y hacés click en el botón ",
          tags$strong("«Ingresar resultados de la fase»"), " que aparece al final. ",
          tags$strong("El ingreso es definitivo: no se puede modificar después."))
      ),
      tags$div(style="background:#fff3cd;border-left:4px solid #e07b28;border-radius:4px;padding:10px 16px;margin-bottom:14px;",
        tags$strong(style="color:#7a4000;","⚠ Recordá: "),
        tags$span(style="color:#7a4000;","después de cargar los marcadores de una fase, NO te olvides de hacer click en «Ingresar resultados de la fase». Si no, no se guardan."),
        tags$br(),
        tags$span(style="color:#7a4000;font-size:.88em;","Ingresá el resultado a los 120 minutos (incluye prórroga, excluye penales).")
      ),

      lapply(seq_along(FASES_FE), function(idx) {
        fase <- FASES_FE[[idx]]
        ronda <- fase$nombre
        ps <- HORARIOS_FE |> filter(num >= fase$min & num <= fase$max)
        if (nrow(ps)==0) return(NULL)

        completa <- fase_completa(fase, pfe_excel)
        nums <- nums_de_fase(fase)
        # ¿Ya ingresó el jugador todos los de esta fase?
        mis_rfe_fase <- if(nrow(rfe)>0 && "partido_num" %in% names(rfe))
          rfe$partido_num[mismo_id(rfe$jugador_id, u$id)] else integer(0)
        fase_ingresada <- all(nums %in% mis_rfe_fase)
        algun_ingresado <- any(nums %in% mis_rfe_fase)

        # Fecha de inicio de la fase siguiente (para el cartel)
        # Fecha de inicio de ESTA fase (deadline: apostar antes de que empiece)
        fecha_ini <- fecha_inicio_fase(fase, pfe_excel)
        fecha_ini_txt <- if(!is.na(fecha_ini)) format(fecha_ini,"%d/%m/%Y") else "—"

        tagList(
          tags$div(class="ronda-hdr", ronda),

          # Cartel de estado de la fase
          if (fase_ingresada && completa) {
            tags$div(style="background:#e8f5e9;border-left:4px solid #5aae61;border-radius:4px;padding:9px 14px;margin:6px 0 10px;",
              tags$span(style="color:#2e7d32;font-weight:600;",paste0("✓ Ya ingresaste todos los resultados de ",ronda,".")))
          } else if (!completa) {
            tags$div(style="background:#fff8e1;border-left:4px solid #f5c518;border-radius:4px;padding:9px 14px;margin:6px 0 10px;",
              tags$span(style="color:#7a5b00;",
                paste0("Vas a poder ingresar los resultados de ", ronda,
                       " cuando estén definidos todos sus partidos. ")),
              tags$strong(style="color:#7a5b00;",
                if(!is.na(fecha_ini)) paste0("Recordá hacerlo antes de que empiece ", ronda," (",fecha_ini_txt,").") else "")
            )
          } else {
            tags$div(style="background:#e3f2fd;border-left:4px solid #1976d2;border-radius:4px;padding:9px 14px;margin:6px 0 10px;",
              tags$span(style="color:#0d47a1;font-weight:600;",
                paste0("✅ ", ronda, " habilitada para ingreso. ")),
              tags$span(style="color:#0d47a1;",
                "Cargá todos los marcadores y hacé click en «Ingresar resultados de la fase» al final."))
          },

          # Lista de partidos de la fase
          lapply(seq_len(nrow(ps)), function(i) {
            p    <- ps[i,]
            pnum <- p$num
            pfe_row  <- pfe_excel |> filter(num==pnum)
            pid <- if(nrow(pfe_row)>0) pfe_row$partido_id[1] else p$partido_id

            eq_a_raw <- if(nrow(pfe_row)>0) pfe_row$local[1] else NA
            eq_b_raw <- if(nrow(pfe_row)>0) pfe_row$visitante[1] else NA
            fecha_part <- if(nrow(pfe_row)>0 && !is.na(pfe_row$fecha_date[1]))
              format(pfe_row$fecha_date[1],"%d/%m") else "Fecha por confirmar"
            eq_a <- if(equipo_definido(eq_a_raw)) eq_a_raw else "Por definir"
            eq_b <- if(equipo_definido(eq_b_raw)) eq_b_raw else "Por definir"

            ya_ingresado <- pnum %in% mis_rfe_fase
            mi_res <- if(nrow(rfe)>0) {
              rfe |> filter(mismo_id(jugador_id, u$id), partido_num==pnum) |>
                slice_max(created_at,n=1,with_ties=FALSE)
            } else data.frame()
            gl_v <- if(nrow(mi_res)>0&&!is.na(mi_res$goles_local[1])) mi_res$goles_local[1] else NA
            gv_v <- if(nrow(mi_res)>0&&!is.na(mi_res$goles_visitante[1])) mi_res$goles_visitante[1] else NA

            mi_pick_partido <- isolate(get_mi_pick_global(pnum))

            tags$div(class="partido-card",
              style=if(ya_ingresado) "opacity:.85;background:#f8f8f8;" else "",
              tags$div(style="min-width:36px;font-weight:700;color:#0f3460;",paste0("M",pnum)),
              tags$div(style="flex:1;min-width:160px;",
                tags$div(style="font-weight:600;",paste(eq_a,"vs",eq_b)),
                tags$div(style="font-size:.75em;color:#888;",fecha_part),
                if (mi_pick_partido!="" && pnum>=73 && pnum<=88 && equipo_definido(eq_a_raw) && equipo_definido(eq_b_raw))
                  tags$div(style="font-size:.75em;color:#0f3460;font-style:italic;margin-top:2px;",
                    paste0("Marcaste que pasaba ", mi_pick_partido))
              ),
              if (ya_ingresado) {
                tags$div(style="color:#5aae61;font-weight:700;font-size:1.1em;",
                  paste0(gl_v," – ",gv_v," ✓"))
              } else if (!completa) {
                tags$span(style="color:#aaa;font-size:.82em;","Esperando que se definan todos los partidos de la fase")
              } else {
                tagList(
                  numericInput(paste0("gl_",pnum), eq_a, value=NULL, min=0, max=30, width="65px"),
                  tags$span(style="font-size:1.1em;","–"),
                  numericInput(paste0("gv_",pnum), eq_b, value=NULL, min=0, max=30, width="65px")
                )
              }
            )
          }),

          # Botón ÚNICO al final de la fase (solo si está completa y no ingresada)
          if (completa && !fase_ingresada) {
            tags$div(style="text-align:center;margin:10px 0 20px;",
              tags$div(style="background:#fff3cd;border-radius:4px;padding:6px 12px;margin-bottom:8px;display:inline-block;",
                tags$span(style="color:#7a4000;font-size:.85em;",
                  paste0("☝ Cargá los ",nrow(ps)," marcadores de ",ronda," y después tocá el botón:"))),
              tags$br(),
              actionButton(paste0("save_fase_",idx), paste0("✅ Ingresar resultados de ",ronda),
                style="background:#0f3460;color:white;font-size:1em;padding:10px 24px;font-weight:700;"),
              uiOutput(paste0("msg_fase_",idx))
            )
          } else tags$div(style="margin-bottom:16px;")
        )
      })
    )
  })

  save_obs_inicializados <- reactiveVal(FALSE)
  observe({
    u <- usuario(); req(u)
    if (isolate(save_obs_inicializados())) return()
    save_obs_inicializados(TRUE)
    lapply(seq_along(FASES_FE), function(idx) {
      observeEvent(input[[paste0("save_fase_",idx)]], {
        u <- isolate(usuario()); req(u)
        fase <- FASES_FE[[idx]]
        nums <- nums_de_fase(fase)
        pfe  <- isolate(partidos_fe_excel())

        # Validar que la fase esté completa (todos los rivales definidos)
        if (!fase_completa(fase, pfe)) {
          output[[paste0("msg_fase_",idx)]] <- renderUI(
            tags$p(style="color:red;margin-top:8px;","⛔ La fase no está completa todavía."))
          return()
        }

        # Validar que TODOS los marcadores estén cargados
        faltan <- c()
        valores <- list()
        for (n in nums) {
          gl <- input[[paste0("gl_",n)]]; gv <- input[[paste0("gv_",n)]]
          if (is.null(gl)||is.null(gv)||is.na(gl)||is.na(gv)) { faltan <- c(faltan, n); next }
          valores[[as.character(n)]] <- c(as.integer(gl), as.integer(gv))
        }
        if (length(faltan)>0) {
          output[[paste0("msg_fase_",idx)]] <- renderUI(
            tags$p(style="color:red;margin-top:8px;",
              paste0("⛔ Faltan marcadores: M", paste(faltan, collapse=", M"),
                     ". Cargá todos antes de ingresar.")))
          return()
        }

        # Verificar que el jugador no ingresó ya esta fase
        rfe_check <- isolate(resultados_fe_db())
        ya <- if(nrow(rfe_check)>0 && "partido_num" %in% names(rfe_check))
          rfe_check$partido_num[mismo_id(rfe_check$jugador_id, u$id)] else integer(0)
        if (any(nums %in% ya)) {
          output[[paste0("msg_fase_",idx)]] <- renderUI(
            tags$p(style="color:red;margin-top:8px;","⛔ Ya ingresaste resultados de esta fase. No se puede modificar."))
          return()
        }

        # Construir filas para insertar en bloque
        rows <- lapply(nums, function(n) {
          v <- valores[[as.character(n)]]
          gl <- v[1]; gv <- v[2]
          pfe_row <- pfe |> filter(num==n)
          pid <- if(nrow(pfe_row)>0) pfe_row$partido_id[1] else paste0("WC2022_",sprintf("%03d",n))
          eq_local <- if(nrow(pfe_row)>0) pfe_row$local[1] else NA
          eq_visit <- if(nrow(pfe_row)>0) pfe_row$visitante[1] else NA
          eq_gan <- if (gl > gv) eq_local else if (gv > gl) eq_visit else NA
          # equipo_ganador SIEMPRE presente (null si empate) para que todas las filas
          # tengan idénticas claves — PostgREST lo exige en inserts en bloque
          list(jugador_id=u$id, partido_id=pid,
               goles_local=gl, goles_visitante=gv, enviado=TRUE,
               equipo_ganador=if(is.na(eq_gan)) NA else eq_gan)
        })

        # Insertar en bloque
        res <- tryCatch({
          resp <- request(paste0(SUPABASE_URL,"/rest/v1/apuestas_fe")) |>
            req_headers(!!!make_hdrs("return=minimal")) |>
            req_body_raw(toJSON(rows, auto_unbox=TRUE, na="null")) |>
            req_error(is_error=function(resp) FALSE) |>
            req_perform()
          list(status=resp$status_code,
               msg=tryCatch(rawToChar(resp$body), error=function(e) ""))
        }, error=function(e) list(status=0, msg=conditionMessage(e)))

        if (res$status >= 200 && res$status < 300) {
          showNotification(paste0("✅ Resultados de ",fase$nombre," guardados (",length(nums)," partidos). No se pueden modificar."),
            type="message", duration=5)
          output[[paste0("msg_fase_",idx)]] <- renderUI(
            tags$p(style="color:#2e7d32;font-weight:700;margin-top:8px;",
                   paste0("✓ ",fase$nombre," ingresada y bloqueada.")))
          cache_invalidar("apuestas_fe")  # ver el cambio recién guardado
          resultados_refresh(isolate(resultados_refresh())+1)
        } else {
          output[[paste0("msg_fase_",idx)]] <- renderUI(
            tags$p(style="color:red;font-weight:700;margin-top:8px;",
                   paste0("⛔ Error (",res$status,"): ",substr(res$msg,1,200),". Reintentá.")))
        }
      }, ignoreInit=TRUE)
    })
  })

  # ══════════════════════════════════════════════════════════════
  # PESTAÑA: POR JUGADOR
  # ══════════════════════════════════════════════════════════════
  output$por_jugador_ui <- renderUI({
    u <- usuario(); req(u)
    if (!jugador_habilitado()) return(tags$div(
      style="background:#fff3cd;border-left:4px solid #e07b28;border-radius:6px;padding:20px 24px;margin:20px 0;",
      tags$h5(style="color:#7a4000;margin-bottom:8px;","🔒 Acceso bloqueado"),
      tags$p(style="color:#7a4000;margin:0;", "Para ver esta pestaña primero tenés que:"),
      tags$ol(style="color:#7a4000;",
        tags$li("Completar y enviar tu cuadro eliminatorio en \"Mi cuadro FE\""),
        tags$li("Ingresar tus resultados para todos los partidos disponibles en \"Resultados FE\"")
      )
    ))
    tagList(
      selectInput("sel_jugador_perfil","Jugador",choices=JUGADORES_LISTA,selected=u$nombre),
      uiOutput("perfil_contenido")
    )
  })

  output$perfil_contenido <- renderUI({
    req(input$sel_jugador_perfil)
    jnombre <- input$sel_jugador_perfil
    tryCatch({

    udb <- sb_get("usuarios",paste0("?nombre=eq.",URLencode(jnombre),"&select=id"))
    if (is.null(udb)||length(udb)==0) return(tags$p("Jugador no encontrado."))
    udb_df <- tryCatch(as.data.frame(udb), error=function(e) data.frame())
    if (nrow(udb_df)==0||!("id" %in% names(udb_df))) return(tags$p("Jugador no encontrado."))
    jid <- udb_df$id[1]

    # Datos FG
    mis_p  <- pts_fg_rv()  |> filter(mismo_id(jugador_id, jid))
    mis_st_raw <- pts_st_rv() |> filter(as.character(participante_id)==as.character(jid))
    # Asegurar que equipo_norm existe en mis_st
    mis_st <- if (nrow(mis_st_raw)>0 && !("equipo_norm" %in% names(mis_st_raw))) {
      mis_st_raw |> mutate(equipo_norm=codigo_a_nombre(Equipo))
    } else mis_st_raw
    tot_p  <- sum(mis_p$puntos, na.rm=TRUE)
    tot_st <- sum(mis_st$pts_standing, na.rm=TRUE)

    # Viñetas de perfil
    todos <- pts_fg_rv()
    media_g   <- round(mean(mis_p$gl_pred+mis_p$gv_pred, na.rm=TRUE),1)
    pct_emp   <- round(100*mean(mis_p$gl_pred==mis_p$gv_pred, na.rm=TRUE),1)
    pct_gan   <- round(100*mean(mis_p$gl_pred!=mis_p$gv_pred, na.rm=TRUE),1)
    pct_gol   <- round(100*mean(abs(mis_p$gl_pred-mis_p$gv_pred)>=3, na.rm=TRUE),1)
    precision <- round(100*mean(mis_p$puntos>=3, na.rm=TRUE),1)
    g_media_g <- round(mean(todos$gl_pred+todos$gv_pred, na.rm=TRUE),1)
    g_pct_emp <- round(100*mean(todos$gl_pred==todos$gv_pred, na.rm=TRUE),1)
    g_pct_gan <- round(100*mean(todos$gl_pred!=todos$gv_pred, na.rm=TRUE),1)
    g_pct_gol <- round(100*mean(abs(todos$gl_pred-todos$gv_pred)>=3, na.rm=TRUE),1)
    g_prec    <- round(100*mean(todos$puntos>=3, na.rm=TRUE),1)

    sb <- function(val,lab,grp,col) tags$div(class="stat-box",
      style=paste0("background:",col,";"),
      tags$div(style="font-size:1.6em;font-weight:bold;",val),
      tags$div(style="font-size:.75em;margin-top:3px;",lab),
      tags$div(style="font-size:.68em;margin-top:5px;opacity:.8;border-top:1px solid rgba(255,255,255,.4);padding-top:3px;",
               paste0("grupo: ",grp))
    )

    # Puntos diarios combinados FG + FE para crack/pifiador
    fg_dia <- pts_fg_rv() |> filter(!is.na(fecha_date)) |>
      group_by(jugador, fecha_date) |>
      summarise(pts_dia=sum(puntos,na.rm=TRUE), .groups="drop")
    fe_dia <- pts_fe_por_fecha_rv()
    pts_dia_all <- bind_rows(fg_dia, fe_dia) |>
      group_by(jugador, fecha_date) |>
      summarise(pts_dia=sum(pts_dia,na.rm=TRUE), .groups="drop")

    todas_fechas <- pts_dia_all |> pull(fecha_date) |> unique()

    n_crack <- sum(sapply(todas_fechas, function(f) {
      res_j <- pts_dia_all |> filter(fecha_date==f)
      if (nrow(res_j)==0) return(FALSE)
      max_pts <- max(res_j$pts_dia)
      cracks <- res_j |> filter(pts_dia==max_pts) |> pull(jugador)
      jnombre %in% cracks
    }))

    n_pifi <- sum(sapply(todas_fechas, function(f) {
      res_j <- pts_dia_all |> filter(fecha_date==f)
      if (nrow(res_j)==0) return(FALSE)
      min_pts <- min(res_j$pts_dia)
      peores <- res_j |> filter(pts_dia==min_pts) |> pull(jugador)
      jnombre %in% peores
    }))

    # Puntos FE resultados
    rfe_j <- if(nrow(resultados_fe_db())>0)
      resultados_fe_db() |> filter(mismo_id(jugador_id, jid)) |>
        group_by(partido_id) |> slice_max(created_at,n=1,with_ties=FALSE) |> ungroup()
    else data.frame()

    # Partidos FE con resultado real (para calcular puntos)
    pfe_con_res <- partidos_fe_excel() |> filter(!is.na(goles_l))
    partidos_jugados_fe <- nrow(pfe_con_res)
    # Denominador = total posible: 32 partidos FE (M73-M104) × 8 pts
    max_pts_res_fe <- 32 * 8

    pts_res_fe <- if(nrow(rfe_j)>0 && nrow(pfe_con_res)>0) {
      rfe_j |>
        mutate(num=as.integer(str_extract(partido_id,"(?<=_)\\d+"))) |>
        inner_join(pfe_con_res |> select(num,goles_l,goles_v), by="num") |>
        rowwise() |>
        mutate(p=calcular_puntos_partido(goles_local,goles_visitante,goles_l,goles_v)) |>
        ungroup() |> pull(p) |> sum(na.rm=TRUE)
    } else 0

    # Puntos cuadro FE (basado en cruces reales del Excel)
    picks_db <- picks_cuadro_db()
    mis_picks_c <- if(nrow(picks_db)>0 && "partido_num" %in% names(picks_db)) {
      picks_db |> filter(mismo_id(jugador_id, jid)) |>
        mutate(partido_num=suppressWarnings(as.integer(partido_num)))
    } else data.frame(jugador_id=character(), partido_num=integer(), equipo_apostado=character(), created_at=character())
    res_excel_now <- partidos_fe_excel()
    det_cuadro <- detalle_cuadro_jugador(mis_picks_c, res_excel_now)
    pts_cuadro_j <- sum(det_cuadro$puntos, na.rm=TRUE)
    # Denominador = total posible: 16 partidos (M89-M104) × 20 pts
    max_pts_cuadro <- 16 * 20

    # Grupos visuales
    st <- st_reales()
    clas_r <- clasificados_reales_fn(st)

    grupos_ui <- lapply(sort(unique(st$grupo)), function(g) {
      equipos_g <- st |> filter(grupo==g) |> arrange(pos_grupo)
      tags$div(class="grupo-card", style="min-width:200px;",
        tags$div(class="grupo-title", g),
        lapply(seq_len(nrow(equipos_g)), function(i) {
          eq   <- equipos_g$equipo[i]
          pos  <- equipos_g$pos_grupo[i]
          clas <- eq %in% clas_r$equipo

          # Qué apostó el jugador para este equipo en este grupo
          mi_apuesta <- if (nrow(mis_st)>0 && "equipo_norm" %in% names(mis_st)) {
            mis_st |> filter(Grupo==g, equipo_norm==eq)
          } else data.frame()

          # Si el jugador no apostó nada para este equipo, buscar si apostó que clasificara
          pts_jugador <- if (nrow(mi_apuesta)>0) mi_apuesta$pts_standing[1] else NA
          pos_apostada<- if (nrow(mi_apuesta)>0 && "pos_pred_label" %in% names(mi_apuesta)) mi_apuesta$pos_pred_label[1] else NA

          # Si clasificó pero el jugador NO lo tenía en su lista de clasificados
          if (clas && is.na(pts_jugador)) {
            mi_pos_raw <- if (nrow(mis_st)>0 && "equipo_norm" %in% names(mis_st) && "pos_pred" %in% names(mis_st)) {
              mis_st |> filter(Grupo==g, equipo_norm==eq) |> pull(pos_pred)
            } else character(0)
            pos_apostada_raw <- if (length(mi_pos_raw)>0) paste0(mi_pos_raw[1],"°") else "no apostado"
            pts_jugador <- 0
            pos_apostada <- pos_apostada_raw
          }

          pts_col <- if (is.na(pts_jugador)) "#aaa"
                     else if (pts_jugador==10) "#1b7837"
                     else if (pts_jugador==5)  "#5aae61"
                     else "#c0392b"
          pts_lbl <- if (is.na(pts_jugador)) ""
                     else paste0("+",pts_jugador,
                                 if (!is.na(pos_apostada)) paste0(" (Apuesta: ",pos_apostada,")") else "")

          tags$div(
            class=if(clas)"eq-row eq-clasificado" else "eq-row",
            style="padding:4px 6px;display:flex;align-items:center;",
            tags$span(
              style=paste0("color:",if(clas)"#1b7837" else "#888",
                           ";font-weight:",if(clas)"600" else "400",";flex:1;font-size:.85em;"),
              paste0(pos,"° ",eq)
            ),
            if (pts_lbl!="")
              tags$span(
                style=paste0("color:",pts_col,";font-size:.78em;font-weight:700;",
                             "margin-left:6px;white-space:nowrap;"),
                pts_lbl
              )
          )
        })
      )
    })

    # mis_picks_cuadro para la tabla de cuadro
    mis_picks_cuadro <- mis_picks_c

    tagList(
      # Chips de resumen
      tags$h4(paste("Perfil apostador:", jnombre)),

      # Crack / Pifiador
      tags$div(style="text-align:center;margin-bottom:12px;",
        tags$div(style="display:inline-flex;gap:12px;flex-wrap:wrap;justify-content:center;",
          tags$div(style="background:#f5c518;color:#1a1a2e;border-radius:10px;padding:10px 18px;text-align:center;min-width:120px;",
            tags$div(style="font-size:1.6em;font-weight:900;",n_crack),
            tags$div(style="font-size:.75em;font-weight:700;","🔮 Crack de jornada")),
          tags$div(style="background:#e07b28;color:white;border-radius:10px;padding:10px 18px;text-align:center;min-width:120px;",
            tags$div(style="font-size:1.6em;font-weight:900;",n_pifi),
            tags$div(style="font-size:.75em;font-weight:700;","💩 Pifiador de jornada"))
        )
      ),

      # Puntos Fase de grupos
      tags$div(style="text-align:center;margin-bottom:12px;",
        tags$div(style="display:inline-flex;gap:10px;flex-wrap:wrap;justify-content:center;",
          tags$div(style="background:#0f3460;color:white;border-radius:10px;padding:12px 18px;min-width:150px;text-align:center;",
            tags$div(style="font-size:1.6em;font-weight:900;",paste0(tot_p," / ",nrow(mis_p)*8)),
            tags$div(style="font-size:.75em;margin-top:3px;","Pts resultados Fase de grupos")),
          tags$div(style="background:#5aae61;color:white;border-radius:10px;padding:12px 18px;min-width:150px;text-align:center;",
            tags$div(style="font-size:1.6em;font-weight:900;",paste0(tot_st," / 320")),
            tags$div(style="font-size:.75em;margin-top:3px;","Pts clasificados Fase de grupos"))
        )
      ),

      # Puntos Fase eliminatoria
      tags$div(style="text-align:center;margin-bottom:12px;",
        tags$div(style="display:inline-flex;gap:10px;flex-wrap:wrap;justify-content:center;",
          tags$div(style="background:#8e44ad;color:white;border-radius:10px;padding:12px 18px;min-width:150px;text-align:center;",
            tags$div(style="font-size:1.6em;font-weight:900;",paste0(pts_res_fe," / ",max_pts_res_fe)),
            tags$div(style="font-size:.75em;margin-top:3px;","Pts resultados Fase eliminatoria")),
          tags$div(style="background:#b07fd4;color:white;border-radius:10px;padding:12px 18px;min-width:150px;text-align:center;",
            tags$div(style="font-size:1.6em;font-weight:900;",paste0(pts_cuadro_j," / ",max_pts_cuadro)),
            tags$div(style="font-size:.75em;margin-top:3px;","Pts cuadro Fase eliminatoria")),
          tags$div(style="background:#c0392b;color:white;border-radius:10px;padding:12px 18px;min-width:150px;text-align:center;",
            tags$div(style="font-size:1.6em;font-weight:900;","? / 145"),
            tags$div(style="font-size:.75em;margin-top:3px;","Pts premios ind.")),
          tags$div(style="background:#37474f;color:white;border-radius:10px;padding:12px 18px;min-width:150px;text-align:center;",
            tags$div(style="font-size:1.6em;font-weight:900;","? / 280"),
            tags$div(style="font-size:.75em;margin-top:3px;","Pts top 4"))
        )
      ),

      # Apuestas globales: posiciones finales y premios individuales
      local({
        pr <- datos_base()$pred_pr
        if (is.null(pr) || nrow(pr)==0) return(NULL)
        # nombre de la columna que identifica al jugador
        col_nom <- if("nombre" %in% names(pr)) "nombre"
                   else if("participante_nombre" %in% names(pr)) "participante_nombre" else NULL
        if (is.null(col_nom)) return(NULL)
        fila <- pr[pr[[col_nom]]==jnombre, , drop=FALSE]
        if (nrow(fila)==0) return(NULL)
        val <- function(x) if(length(x)==0||is.na(x)||x=="") "—" else as.character(x)
        getc <- function(nm) if(nm %in% names(fila)) fila[[nm]][1] else NA

        pos_card <- function(emoji, titulo, valor, bg, txt="#fff") {
          tags$div(style=paste0("display:inline-flex;flex-direction:column;align-items:center;",
            "margin:6px 8px;padding:14px 12px;border-radius:12px;background:",bg,";color:",txt,
            ";min-width:100px;box-shadow:0 2px 8px rgba(0,0,0,0.15);"),
            tags$div(style="font-size:1.8em;line-height:1;", emoji),
            tags$div(style="font-size:.65em;opacity:.85;margin-top:4px;text-transform:uppercase;letter-spacing:1px;", titulo),
            tags$div(style="font-size:.95em;font-weight:800;margin-top:4px;text-align:center;", val(valor)))
        }
        premio_card <- function(emoji, titulo, valor, bg) {
          tags$div(style=paste0("display:inline-flex;align-items:center;gap:10px;",
            "margin:6px 8px;padding:12px 16px;border-radius:10px;background:",bg,
            ";color:white;min-width:200px;box-shadow:0 2px 8px rgba(0,0,0,0.15);"),
            tags$div(style="font-size:1.8em;", emoji),
            tags$div(
              tags$div(style="font-size:.7em;opacity:.85;text-transform:uppercase;letter-spacing:1px;", titulo),
              tags$div(style="font-size:1em;font-weight:800;", val(valor))))
        }
        tags$div(style="background:linear-gradient(135deg,#1a1a2e,#0f3460);border-radius:14px;padding:18px 20px;margin-bottom:16px;color:white;",
          tags$div(style="font-size:.8em;opacity:.7;letter-spacing:2px;margin-bottom:12px;","SUS APUESTAS GLOBALES"),
          tags$div(style="margin-bottom:14px;",
            tags$div(style="font-size:.75em;color:#aaa;margin-bottom:6px;","POSICIONES FINALES"),
            pos_card("🥇","Campeón",    getc("campeon"),    "#f5c518","#1a1a2e"),
            pos_card("🥈","Subcampeón", getc("subcampeon"), "#9e9e9e"),
            pos_card("🥉","3er puesto", getc("tercero"),    "#cd7f32"),
            pos_card("4","4to puesto",  getc("cuarto"),     "#37474f")
          ),
          tags$div(
            tags$div(style="font-size:.75em;color:#aaa;margin-bottom:6px;","PREMIOS INDIVIDUALES"),
            premio_card("👟","Botín de Oro",  getc("botin_oro"),  "#c0392b"),
            premio_card("🏅","Balón de Oro",  getc("balon_oro"),  "#8e44ad"),
            premio_card("🧤","Guante de Oro", getc("guante_oro"), "#0f3460")
          )
        )
      }),

      tags$hr(),

      # Viñetas perfil apostador
      tags$div(style="text-align:center;margin-bottom:16px;",
        sb(media_g,"Media goles/partido",g_media_g,"#0f3460"),
        sb(paste0(pct_emp,"%"),"% apuestas empate",paste0(g_pct_emp,"%"),"#5aae61"),
        sb(paste0(pct_gan,"%"),"% con ganador",paste0(g_pct_gan,"%"),"#e07b28"),
        sb(paste0(pct_gol,"%"),"% goleadas (dif≥3)",paste0(g_pct_gol,"%"),"#8e44ad"),
        sb(paste0(precision,"%"),"% precisión (pts≥3)",paste0(g_prec,"%"),"#c0392b")
      ),

      tags$hr(),

      # ── 1. RESULTADOS FE (abierto, formato igual a Resultados FG) ──
      tags$button(
        class="accordion-btn",
        onclick="var t=this.nextElementSibling;t.style.display=t.style.display==='none'?'block':'none';var i=this.querySelector('.acc-icon');i.textContent=i.textContent==='+'?'−':'+';",
        "Resultados fase eliminatoria  ", tags$span(class="acc-icon","−")
      ),
      tags$div(style="display:block;",
        local({
          if (nrow(rfe_j)==0) {
            tags$div(style="padding:12px;color:#888;","Aún no ingresaste apuestas de fase eliminatoria.")
          } else {
            pfe_todos <- partidos_fe_excel()  # todos los partidos FE (jugados o no)
            df_fe <- rfe_j |>
              mutate(num=as.integer(str_extract(partido_id,"(?<=_)\\d+"))) |>
              left_join(pfe_todos |> select(num, local, visitante, goles_l, goles_v, fecha_label), by="num") |>
              rowwise() |>
              mutate(
                jugado = !is.na(goles_l) && !is.na(goles_v),
                pts = if(jugado) calcular_puntos_partido(goles_local,goles_visitante,goles_l,goles_v) else NA_integer_,
                tipo = dplyr::case_when(
                  !jugado ~ "pendiente",
                  pts==8 ~ "exacto", pts==5 ~ "diferencia",
                  pts==3 ~ "resultado", TRUE ~ "fallo")
              ) |>
              ungroup() |> arrange(num)
            tot_fe <- sum(df_fe$pts, na.rm=TRUE)
            color_tipo <- function(t) switch(t,
              "exacto"="#1b7837","diferencia"="#5aae61","resultado"="#a6dba0","fallo"="#f4a582",
              "pendiente"="#ffffff","#fff")
            filas <- lapply(seq_len(nrow(df_fe)), function(i){
              d <- df_fe[i,]
              txt_col <- if(d$tipo %in% c("exacto","diferencia"))"white" else "#333"
              tags$div(style=paste0("display:flex;align-items:center;gap:10px;padding:6px 10px;border-bottom:1px solid #eee;min-width:440px;background:",
                                    color_tipo(d$tipo),";color:",txt_col,";"),
                tags$div(style="flex:1;font-size:.82em;font-weight:600;",paste(d$local,"vs",d$visitante)),
                tags$div(style="min-width:55px;font-size:.78em;",d$fecha_label),
                tags$div(style="min-width:90px;font-size:.8em;text-align:center;",
                  if(d$jugado) paste0("Real ",d$goles_l,"-",d$goles_v) else tags$em(style="color:#999;","sin jugar")),
                tags$div(style="min-width:90px;font-size:.8em;text-align:center;",
                  paste0("Tu ",d$goles_local,"-",d$goles_visitante)),
                tags$div(style="min-width:42px;font-weight:800;text-align:right;",
                  if(d$jugado) paste0("+",d$pts) else "—")
              )
            })
            tagList(
              tags$div(style="background:#f0f4ff;padding:7px 10px;font-size:.8em;color:#555;border-radius:6px 6px 0 0;",
                "Tus apuestas de fase eliminatoria. Los puntos se calculan cuando se juega cada partido."),
              tags$div(style="overflow-x:auto;-webkit-overflow-scrolling:touch;",
                tags$div(style="border:1px solid #dee2e6;border-top:none;",filas)),
              tags$div(style="text-align:right;padding:8px 10px;font-weight:800;color:#0f3460;",
                paste0("Total resultados FE: ",tot_fe," pts"))
            )
          }
        })
      ),

      tags$hr(),

      # ── 2. CUADRO ELIMINATORIO (abierto, con nombres de columna) ──
      tags$button(
        class="accordion-btn",
        onclick="var t=this.nextElementSibling;t.style.display=t.style.display==='none'?'block':'none';var i=this.querySelector('.acc-icon');i.textContent=i.textContent==='+'?'−':'+';",
        "Cuadro eliminatorio (resultados reales y tus aciertos)  ", tags$span(class="acc-icon","−")
      ),
      tags$div(style="display:block;",
        local({
          det <- det_cuadro
          # Encabezado de columnas
          header <- tags$div(style="display:flex;align-items:center;gap:10px;padding:7px 10px;background:#0f3460;color:white;font-size:.78em;font-weight:700;border-radius:6px 6px 0 0;min-width:520px;",
            tags$div(style="min-width:42px;","Partido"),
            tags$div(style="min-width:70px;","Ronda"),
            tags$div(style="flex:1;","Cruce real"),
            tags$div(style="min-width:200px;text-align:right;","Tu apuesta y puntos")
          )
          filas_ui <- lapply(seq_len(nrow(det)), function(i) {
            d <- det[i,]
            real_def <- !is.na(d$real_a) && !is.na(d$real_b)
            pred_txt <- if(!is.na(d$pred_a)||!is.na(d$pred_b))
              paste(ifelse(is.na(d$pred_a),"?",d$pred_a),"vs",ifelse(is.na(d$pred_b),"?",d$pred_b))
              else "Sin apuesta"
            real_txt <- if(real_def) paste(d$real_a,"vs",d$real_b) else "Por definirse"
            pts <- d$puntos
            # Fondo de la fila según nivel de acierto
            bg <- if(is.na(pts)) "#ffffff" else if(pts==20) "#1b7837" else if(pts==9) "#5aae61" else "#f4a582"
            txt_col <- if(!is.na(pts) && pts>=9) "white" else "#333"
            info <- if(is.na(pts)) {
                      pred_txt
                    } else {
                      paste0(pred_txt, "  (+", pts, ")")
                    }
            tags$div(style=paste0("display:flex;align-items:center;gap:10px;padding:7px 10px;border-bottom:1px solid #eee;min-width:520px;",
                                  "background:",bg,";color:",txt_col,";",
                                  if(!real_def)"opacity:.55;" else ""),
              tags$div(style=paste0("min-width:42px;font-weight:700;font-size:.85em;color:",if(!is.na(pts)&&pts>=9)"white" else "#0f3460",";"),paste0("M",d$partido_num)),
              tags$div(style=paste0("min-width:70px;font-size:.78em;color:",if(!is.na(pts)&&pts>=9)"#e8f5e9" else "#888",";"),d$ronda),
              tags$div(style="flex:1;font-weight:600;font-size:.9em;",real_txt),
              tags$div(style="font-weight:700;font-size:.88em;min-width:200px;text-align:right;",info)
            )
          })
          tagList(
            tags$div(style="background:#f0f4ff;padding:8px 10px;font-size:.82em;color:#555;",
              "Cruce real de cada partido y tu apuesta. Puntos: ",
              tags$span(style="color:#1b7837;font-weight:700;","+20 ambos"), " · ",
              tags$span(style="color:#5aae61;font-weight:700;","+9 uno"), " · ",
              tags$span(style="color:#e07b28;font-weight:700;","+0 ninguno"),
              ". Si el partido aún no se definió, se muestra solo tu apuesta."),
            tags$div(style="overflow-x:auto;-webkit-overflow-scrolling:touch;",
              header,
              tags$div(style="border:1px solid #dee2e6;border-top:none;border-radius:0 0 6px 6px;",filas_ui)
            ),
            tags$div(style="text-align:right;padding:8px 10px;font-weight:800;color:#0f3460;",
              paste0("Total cuadro: ",pts_cuadro_j," pts"))
          )
        })
      ),

      tags$hr(),

      # ── 3. STANDINGS FASE DE GRUPOS (abierto) ──
      tags$button(
        class="accordion-btn",
        onclick="var t=this.nextElementSibling;t.style.display=t.style.display==='none'?'block':'none';var i=this.querySelector('.acc-icon');i.textContent=i.textContent==='+'?'−':'+';",
        "Clasificados de fase de grupos — tus apuestas vs. lo real  ", tags$span(class="acc-icon","−")
      ),
      tags$div(style="display:block;",
        tags$div(style="display:flex;flex-wrap:wrap;gap:10px;padding-top:8px;",
                 grupos_ui)
      ),

      tags$hr(),

      # ── 4. RESULTADOS FG (cerrado) ──
      tags$button(
        class="accordion-btn",
        onclick="var t=this.nextElementSibling;t.style.display=t.style.display==='none'?'block':'none';var i=this.querySelector('.acc-icon');i.textContent=i.textContent==='+'?'−':'+';",
        "Resultados fase de grupos  ", tags$span(class="acc-icon","+")
      ),
      tags$div(style="display:none;",
        local({
          mis_p <- pts_fg_rv() |> filter(mismo_id(jugador_id, jid)) |> arrange(fecha_date)
          tot_p <- sum(mis_p$puntos, na.rm=TRUE)
          if (nrow(mis_p)==0) {
            tags$div(style="padding:12px;color:#888;","Sin datos de fase de grupos.")
          } else {
            color_tipo <- function(t) switch(t,
              "exacto"="#1b7837","diferencia"="#5aae61","resultado"="#a6dba0","fallo"="#f4a582","#fff")
            filas <- lapply(seq_len(nrow(mis_p)), function(i){
              d <- mis_p[i,]
              tags$div(style=paste0("display:flex;align-items:center;gap:10px;padding:6px 10px;border-bottom:1px solid #eee;min-width:440px;background:",
                                    color_tipo(d$tipo),";color:",if(d$tipo %in% c("exacto","diferencia"))"white" else "#333",";"),
                tags$div(style="flex:1;font-size:.82em;font-weight:600;",d$partido),
                tags$div(style="min-width:55px;font-size:.78em;",d$fecha_label),
                tags$div(style="min-width:80px;font-size:.8em;text-align:center;",
                  paste0("Real ",d$gl_real,"-",d$gv_real)),
                tags$div(style="min-width:90px;font-size:.8em;text-align:center;",
                  paste0("Tu ",d$gl_pred,"-",d$gv_pred)),
                tags$div(style="min-width:42px;font-weight:800;text-align:right;",paste0("+",d$puntos))
              )
            })
            tagList(
              tags$div(style="overflow-x:auto;-webkit-overflow-scrolling:touch;border:1px solid #dee2e6;border-radius:6px;",
                filas),
              tags$div(style="text-align:right;padding:8px 10px;font-weight:800;color:#0f3460;",
                paste0("Total FG: ",tot_p," pts"))
            )
          }
        })
      )
    )
    }, error=function(e) tags$div(style="color:red;padding:16px;",
      tags$strong("Error al cargar el perfil: "), conditionMessage(e)))
  })

}

shinyApp(ui, server)

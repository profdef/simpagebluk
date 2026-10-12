# Figure 1 for "The PAGEBLUK framework: an integrated epidemiological model of civilizational fractures"
# Panel A: structure (P-A-L core, U ring, civilizational shell with four fractures G, E, K, B).
# Panel B: mapping of fractures onto R_t parameters and an illustration of proposition 4.
# Run from the repository root:  Rscript R/figure1.R
suppressPackageStartupMessages({ library(ggplot2) })
dir.create("output", showWarnings = FALSE)

f <- if ("Helvetica Neue" %in% systemfonts::system_fonts()$family) "Helvetica Neue" else "sans"   # fallback outside macOS
tinta  <- "#1f2a30"; abu <- "#6b7579"; garis <- "#cfc8bd"
inti_w <- "#2f6f73"                       # segitiga epidemiologi
u_w    <- c("#e9dfcf", "#e2d5c0", "#dccdb5", "#e2d5c0", "#e9dfcf")
cang   <- "#d8d1c6"                       # cangkang peradaban
fr     <- c(G = "#3b6ea5", E = "#c7822b", K = "#a4492b", B = "#6a4c93")

busur <- function(r1, r2, a1, a2, cx = 0, cy = 0, n = 120) {
  t <- seq(a1, a2, length.out = n) * pi / 180
  data.frame(x = cx + c(r2 * cos(t), r1 * cos(rev(t))), y = cy + c(r2 * sin(t), r1 * sin(rev(t))))
}
lingkar <- function(r, cx = 0, cy = 0, n = 200) { t <- seq(0, 2 * pi, length.out = n); data.frame(x = cx + r * cos(t), y = cy + r * sin(t)) }
# Teks melengkung: tiap huruf diletakkan dan diputar mengikuti busur.
teks_busur <- function(lab, r, pusat, rentang, cx = 0, cy = 0, atas = TRUE) {
  ch <- strsplit(lab, "")[[1]]; n <- length(ch)
  a <- if (atas) seq(pusat + rentang / 2, pusat - rentang / 2, length.out = n) else seq(pusat - rentang / 2, pusat + rentang / 2, length.out = n)
  data.frame(x = cx + r * cos(a * pi / 180), y = cy + r * sin(a * pi / 180), ch = ch, rot = if (atas) a - 90 else a + 90)
}

## Panel A ---------------------------------------------------------------------
cx <- 0; cy <- 0
# cangkang peradaban dipecah empat rekahan pada 45, 135, 225, 315 derajat
sud_r <- c(G = 135, K = 45, E = 225, B = 315)
lebar <- 9
cangkang <- do.call(rbind, lapply(1:4, function(i) {
  a <- c(45, 135, 225, 315, 405)
  d <- busur(3.05, 3.6, a[i] + lebar, a[i + 1] - lebar); d$g <- i; d }))
retak <- do.call(rbind, lapply(names(sud_r), function(k) {
  a <- sud_r[[k]]; r <- seq(2.95, 3.7, length.out = 7)
  zz <- a + c(-1, 1, -1, 1, -1, 1, -1) * 2.2
  kiri <- data.frame(x = r * cos((zz - lebar + 3) * pi / 180), y = r * sin((zz - lebar + 3) * pi / 180))
  kanan <- data.frame(x = rev(r) * cos((rev(zz) + lebar - 3) * pi / 180), y = rev(r) * sin((rev(zz) + lebar - 3) * pi / 180))
  d <- rbind(kiri, kanan); d$k <- k; d }))
# cincin U: lima simpul ilmu
u_seg <- data.frame(a1 = c(90, 162, 234, 306, 18) + 2, a2 = c(162, 234, 306, 378, 90) - 2,
                    lab = c("NATURAL & ENVIRONMENTAL", "ENGINEERING", "LAW & POLICY", "MEDICINE & PUBLIC HEALTH", "BEHAVIOURAL SCIENCES"))
u_poly <- do.call(rbind, lapply(1:5, function(i) { d <- busur(1.95, 2.7, u_seg$a1[i], u_seg$a2[i]); d$g <- i; d }))
u_txt <- do.call(rbind, lapply(1:5, function(i) {
  m <- (u_seg$a1[i] + u_seg$a2[i]) / 2; m0 <- (m + 360) %% 360
  rent <- nchar(u_seg$lab[i]) * 2.5
  teks_busur(u_seg$lab[i], if (m0 > 180 && m0 < 360) 2.26 else 2.33, m, rent, atas = !(m0 > 180 && m0 < 360)) }))
# panah umpan balik di dalam cincin
umpan <- do.call(rbind, lapply(c(100, 172, 244, 316, 28), function(a) {
  t <- seq(a, a + 52, length.out = 30) * pi / 180; data.frame(x = 1.78 * cos(t), y = 1.78 * sin(t), g = a) }))
# inti
nod <- data.frame(h = c("A", "P", "L"), a = c(90, 330, 210), nama = c("AGENT", "HOST", "ENVIRONMENT"),
                  sub = c("transmissibility\nvirulence", "immunity, age\nnutrition", "physical, social,\ninformational"))
nod$x <- 0.92 * cos(nod$a * pi / 180); nod$y <- 0.92 * sin(nod$a * pi / 180)
tri <- data.frame(x = nod$x[c(1, 2, 3, 1)], y = nod$y[c(1, 2, 3, 1)])
nod_poly <- do.call(rbind, lapply(1:3, function(i) { d <- lingkar(0.53, nod$x[i], nod$y[i]); d$h <- nod$h[i]; d }))
# jalur patogen menembus rekahan
jalur <- do.call(rbind, lapply(names(sud_r), function(k) {
  a <- sud_r[[k]] * pi / 180; data.frame(k = k, x = 4.35 * cos(a), y = 4.35 * sin(a), xend = 2.78 * cos(a), yend = 2.78 * sin(a)) }))
# kotak label rekahan
lab_r <- data.frame(k = c("G", "K", "E", "B"),
                    x = c(-4.15, 4.15, -4.15, 4.15), y = c(4.2, 4.2, -4.2, -4.2),
                    judul = c("GERAK  |  mobility", "KESENJANGAN  |  inequity", "EKSPOSUR  |  exposure density", "BERITA & BIAS  |  information disorder"),
                    isi = c("travel, trade, pilgrimage", "who is susceptible, who is reached", "crowding, livestock, vectors", "misinformation, fallacies, trust"))
lab_r$hj <- ifelse(lab_r$x < 0, 0, 1)
lab_r$bx <- ifelse(lab_r$x < 0, lab_r$x - 0.42, lab_r$x + 0.42)

## Panel B ---------------------------------------------------------------------
ox <- 6.6                                   # tepi kiri panel B
eq <- data.frame(t = c("italic(R)[t]", "phantom() == phantom()", "beta", "phantom() %*% phantom()", "italic(c)", "phantom() %*% phantom()", "italic(D)", "phantom() %*% phantom()", "italic(S/N)", "phantom() + phantom()", "iota"),
                 x = ox + c(0.25, 0.95, 1.55, 2.05, 2.5, 2.95, 3.4, 3.85, 4.45, 5.05, 5.5),
                 w = c(tinta, tinta, inti_w, abu, fr["E"], abu, fr["K"], abu, fr["K"], abu, fr["G"]))
eq_y <- 3.55
eq_lab <- data.frame(x = eq$x[c(3, 5, 7, 9, 11)], y = eq_y - 0.62,
                     l = c("per-contact\ntransmission (A)", "contact\nrate", "infectious\nduration", "susceptible\nfraction", "importation"),
                     w = c(inti_w, fr["E"], fr["K"], fr["K"], fr["G"]))
brk_y <- eq_y + 0.55
baris <- data.frame(k = c("G", "E", "K", "B"), y = c(1.55, 0.75, -0.05, -0.85),
                    nm = c("Mobility", "Exposure density", "Inequity", "Information disorder"),
                    par = c("iota*', spatial coupling'", "italic(c)", "italic(S/N)*','~italic(D)*', CFR'", "'multiplier on '*italic(c)*','~italic(D)*','~italic(S/N)*','~iota"),
                    ind = c("passenger flows, mobility indices", "floor area per person, room volume", "coverage by quintile, diagnostic delay", "belief in misinformation, trust"))
# Proposisi 4
bar_y <- -2.75; bx0 <- ox + 0.2; bx1 <- ox + 5.8; sk <- function(p) bx0 + (bx1 - bx0) * p

p <- ggplot() +
  # judul panel
  annotate("text", x = -5.2, y = 5.35, label = "A", family = f, fontface = "bold", size = 6, hjust = 0, colour = tinta) +
  annotate("text", x = -4.75, y = 5.35, label = "Structure: triad, interdisciplinary ring, civilizational shell", family = f, size = 3.6, hjust = 0, colour = tinta) +
  annotate("text", x = ox - 0.3, y = 5.35, label = "B", family = f, fontface = "bold", size = 6, hjust = 0, colour = tinta) +
  annotate("text", x = ox + 0.15, y = 5.35, label = "From fractures to transmission", family = f, size = 3.6, hjust = 0, colour = tinta) +
  annotate("segment", x = ox - 0.75, xend = ox - 0.75, y = -4.9, yend = 5.1, colour = garis, linewidth = 0.4) +
  # cangkang
  geom_polygon(data = cangkang, aes(x, y, group = g), fill = cang, colour = NA) +
  geom_polygon(data = retak, aes(x, y, group = k, fill = k), colour = NA, alpha = 0.95) +
  geom_text(data = teks_busur("CIVILIZATIONAL SHELL: SETTLEMENT / TRADE / INSTITUTIONS", 3.32, 90, 72),
            aes(x, y, label = ch, angle = rot), family = f, size = 2.15, colour = "#7d7468", fontface = "bold") +
  # cincin U
  geom_polygon(data = u_poly, aes(x, y, group = g, fill = I(u_w[g])), colour = "white", linewidth = 0.6) +
  geom_text(data = u_txt, aes(x, y, label = ch, angle = rot), family = f, size = 1.95, colour = "#5b4a35", fontface = "bold") +
  annotate("label", x = 0, y = -1.5, label = "U", family = f, fontface = "bold", size = 4.2, colour = "white", fill = "#8a7354",
           label.r = unit(0.5, "lines")) +
  geom_path(data = umpan, aes(x, y, group = g), colour = "#a8957a", linewidth = 0.45,
            arrow = arrow(length = unit(1.4, "mm"), type = "closed")) +
  # inti
  geom_polygon(data = tri, aes(x, y), fill = "#e4eeee", colour = inti_w, linewidth = 0.5, linetype = "22") +
  geom_polygon(data = nod_poly, aes(x, y, group = h), fill = inti_w, colour = "white", linewidth = 1) +
  geom_text(data = nod, aes(x, y + 0.13, label = h), family = f, fontface = "bold", size = 6.6, colour = "white") +
  geom_text(data = nod, aes(x, y - 0.24, label = nama), family = f, fontface = "bold", size = 1.6, colour = "white") +
  # jalur patogen
  geom_segment(data = jalur, aes(x, y, xend = xend, yend = yend, colour = k), linewidth = 0.75, linetype = "41",
               arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
  # label rekahan
  geom_point(data = lab_r, aes(bx, y, colour = k), size = 10.5) +
  geom_text(data = lab_r, aes(bx, y, label = k), family = f, fontface = "bold", size = 5, colour = "white") +
  geom_text(data = lab_r, aes(ifelse(x < 0, x, x), y + 0.13, label = judul, hjust = hj, colour = k), family = f, fontface = "bold", size = 2.75,
            nudge_x = ifelse(lab_r$x < 0, 0.05, -0.05)) +
  geom_text(data = lab_r, aes(x, y - 0.2, label = isi, hjust = hj), family = f, size = 2.5, colour = abu,
            nudge_x = ifelse(lab_r$x < 0, 0.05, -0.05)) +
  annotate("text", x = 0, y = -4.75, label = "Dashed arrows: pathways through which pathogens reach the core when the shell fractures",
           family = f, size = 2.35, colour = abu, fontface = "italic") +
  # panel B: persamaan
  geom_text(data = eq, aes(x, eq_y, label = t, colour = I(w)), parse = TRUE, family = f, size = 6.2) +
  geom_text(data = eq_lab, aes(x, y, label = l, colour = I(w)), family = f, size = 2.15, lineheight = 0.9) +
  annotate("segment", x = eq$x[5] - 0.2, xend = eq$x[11] + 0.2, y = brk_y, yend = brk_y, colour = fr["B"], linewidth = 0.7) +
  annotate("segment", x = c(eq$x[5], eq$x[7], eq$x[9], eq$x[11]), xend = c(eq$x[5], eq$x[7], eq$x[9], eq$x[11]),
           y = brk_y, yend = brk_y - 0.18, colour = fr["B"], linewidth = 0.6) +
  annotate("text", x = (eq$x[5] + eq$x[11]) / 2 - 0.15, y = brk_y + 0.24, label = "B multiplies every modifiable term (adherence, uptake, testing)",
           family = f, size = 2.35, colour = fr["B"], fontface = "bold") +
  # panel B: tabel pemetaan
  annotate("text", x = ox + c(0.65, 2.55), y = 2.2, label = c("Fracture and parameter", "Example routine indicators"),
           hjust = 0, family = f, fontface = "bold", size = 2.55, colour = tinta) +
  annotate("segment", x = ox, xend = ox + 6, y = 2.0, yend = 2.0, colour = garis, linewidth = 0.4) +
  geom_point(data = baris, aes(ox + 0.25, y, colour = k), size = 7.5) +
  geom_text(data = baris, aes(ox + 0.25, y, label = k), family = f, fontface = "bold", size = 3.8, colour = "white") +
  geom_text(data = baris, aes(ox + 0.65, y + 0.14, label = nm, colour = k), hjust = 0, family = f, fontface = "bold", size = 2.65) +
  geom_text(data = baris, aes(ox + 0.65, y - 0.17, label = par), hjust = 0, family = f, size = 2.4, colour = tinta, parse = TRUE) +
  geom_text(data = baris, aes(ox + 2.55, y, label = ind), hjust = 0, family = f, size = 2.4, colour = abu) +
  annotate("segment", x = ox, xend = ox + 6, y = baris$y[-4] - 0.4, yend = baris$y[-4] - 0.4, colour = "#ece7df", linewidth = 0.3) +
  # panel B: Proposisi 4
  annotate("text", x = ox, y = -1.62, label = "Proposition 4: information disorder defeats an efficacious vaccine", hjust = 0,
           family = f, fontface = "bold", size = 2.75, colour = tinta) +
  annotate("text", x = ox, y = -1.94, label = "R0 = 3, vaccine efficacy 90%, critical coverage = (1 - 1/R0) / 0.9 = 74%", hjust = 0,
           family = f, size = 2.35, colour = abu) +
  annotate("rect", xmin = sk(0), xmax = sk(1), ymin = bar_y - 0.2, ymax = bar_y + 0.2, fill = "#f1ede7") +
  annotate("rect", xmin = sk(0), xmax = sk(0.60), ymin = bar_y - 0.2, ymax = bar_y + 0.2, fill = inti_w) +
  annotate("rect", xmin = sk(0.60), xmax = sk(0.741), ymin = bar_y - 0.2, ymax = bar_y + 0.2, fill = fr["B"], alpha = 0.35) +
  annotate("segment", x = sk(0.741), xend = sk(0.741), y = bar_y - 0.38, yend = bar_y + 0.38, colour = tinta, linewidth = 0.6) +
  annotate("text", x = sk(0.741) + 0.08, y = bar_y + 0.3, hjust = 0, label = "74% needed", family = f, size = 2.4, colour = tinta, fontface = "bold") +
  annotate("text", x = sk(0.30), y = bar_y, label = "60% reached", family = f, size = 2.4, colour = "white", fontface = "bold") +
  annotate("text", x = sk(0.67), y = bar_y - 0.48, label = "gap opened\nby fracture B", family = f, size = 2.15, colour = fr["B"],
           lineheight = 0.9, fontface = "bold") +
  annotate("text", x = sk(c(0, 0.5, 1)), y = bar_y - 0.36, label = c("0%", "50%", "100%"), family = f, size = 2.1, colour = abu) +
  annotate("text", x = ox, y = -3.75, hjust = 0, vjust = 1, family = f, size = 2.3, colour = abu, lineheight = 1.05,
           label = "Because the terms multiply, fractures compound: narrowing several\nfractures modestly can lower the reproduction number more than closing one completely.") +
  scale_fill_manual(values = fr, guide = "none") + scale_colour_manual(values = fr, guide = "none") +
  coord_equal(xlim = c(-5.3, 12.7), ylim = c(-4.95, 5.55), expand = FALSE, clip = "off") +
  theme_void(base_family = f) + theme(plot.margin = margin(10, 12, 8, 12), plot.background = element_rect(fill = "white", colour = NA))

ggsave(file.path("output", "figure1-pagebluk-framework.png"), p, width = 12, height = 7.2, dpi = 300, bg = "white", device = ragg::agg_png)
ggsave(file.path("output", "figure1-pagebluk-framework.pdf"), p, width = 12, height = 7.2, bg = "white", device = cairo_pdf)
cat("Figure 1 selesai.\n")

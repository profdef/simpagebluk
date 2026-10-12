# Online supplementary material, "The PAGEBLUK framework" (BMJ Global Health)
# Deterministic illustration of the fracture-to-parameter mapping:
#   R_t = beta x c x D x S/N   (+ iota, importation, handled as an additive inflow per generation)
# Run from the repository root:  Rscript R/simulasi.R
# All parameter values are illustrative and listed in table S1 of the supplement.
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
out <- "output"; dir.create(out, showWarnings = FALSE)

f <- if ("Helvetica Neue" %in% systemfonts::system_fonts()$family) "Helvetica Neue" else "sans"   # fallback outside macOS
tinta <- "#1f2a30"; abu <- "#6b7579"; inti <- "#2f6f73"
fr <- c(G = "#3b6ea5", E = "#c7822b", K = "#a4492b", B = "#6a4c93")
tema <- theme_minimal(base_size = 9.5, base_family = f) +
  theme(plot.title = element_text(face = "bold", size = 10, colour = tinta),
        plot.subtitle = element_text(size = 8.2, colour = abu, margin = margin(b = 6)),
        panel.grid.minor = element_blank(), axis.title = element_text(size = 8.5, colour = "#4a5559"),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 8),
        legend.key.width = unit(14, "pt"), plot.title.position = "plot")

## Baseline parameters ----------------------------------------------------------
beta <- 0.05; cc <- 12; D <- 5             # per-contact probability, contacts/day, infectious days
R0 <- beta * cc * D                        # = 3
eff <- 0.90                                # vaccine efficacy against infection
hasil <- list()

## S1. Proposition 2: fractures compound multiplicatively ------------------------
# Equal proportional reduction x applied to k multiplicative terms gives R0 (1 - x)^k.
# Reduction needed per term to reach R_t = 1:  x_k = 1 - R0^(-1/k).
xk <- function(R, k) 1 - R^(-1 / k)
d1 <- expand.grid(R = seq(1.2, 6, by = 0.05), k = 1:3)
d1$x <- xk(d1$R, d1$k)
d1$lab <- factor(c("one term (eg, contact rate only)", "two terms (eg, contact rate and duration)", "three terms (contact, duration, susceptible fraction)")[d1$k],
                 levels = c("one term (eg, contact rate only)", "two terms (eg, contact rate and duration)", "three terms (contact, duration, susceptible fraction)"))
tab1 <- expand.grid(R0 = c(2, 3, 4, 6), k = 1:3); tab1$x <- round(100 * xk(tab1$R0, tab1$k))
hasil$S1 <- tab1
pa <- ggplot(d1, aes(R, 100 * x, colour = lab)) +
  geom_line(linewidth = 0.9) +
  annotate("point", x = 3, y = 100 * xk(3, 1:3), colour = tinta, size = 1.6) +
  annotate("text", x = 3.1, y = 100 * xk(3, 1:3), label = paste0(round(100 * xk(3, 1:3)), "%"), hjust = 0, vjust = 1.3,
           size = 2.7, family = f, colour = tinta) +
  scale_colour_manual(values = c(fr[["E"]], fr[["K"]], inti)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(title = "a  Proposition 2: fractures compound",
       subtitle = "Reduction needed in each term to bring Rt to 1",
       x = expression("Basic reproduction number, " * italic(R)[0]), y = "Required reduction per term (%)") + tema

## S2. Proposition 4: information disorder as a multiplier -----------------------
# Planned response: vaccine coverage V* = 80% and a 30% reduction in contacts.
# A proportion m of the population holds misinformation; among them uptake and adherence fall by r.
# Achieved coverage V = V*(1 - r m); achieved contact reduction = 0.30 (1 - r m).
Vs <- 0.80; npi <- 0.30
Rb <- function(m, r) R0 * (1 - npi * (1 - r * m)) * (1 - eff * Vs * (1 - r * m))
Rv <- function(m, r) R0 * (1 - npi) * (1 - eff * Vs * (1 - r * m))       # same plan, misinformation hits vaccine uptake only
d2 <- do.call(rbind, lapply(c(0.5, 1), function(r) rbind(
  data.frame(m = seq(0, 0.6, 0.005), r = r, jalur = "vaccine and distancing affected", R = Rb(seq(0, 0.6, 0.005), r)),
  data.frame(m = seq(0, 0.6, 0.005), r = r, jalur = "vaccine uptake affected only", R = Rv(seq(0, 0.6, 0.005), r)))))
d2$grp <- paste0(d2$jalur, ", r = ", d2$r)
m_star <- function(fn, r) uniroot(function(m) fn(m, r) - 1, c(0, 1))$root
hasil$S2 <- data.frame(r = c(0.5, 1, 0.5, 1), jalur = rep(c("both", "vaccine only"), each = 2),
                       R_at_m0 = round(c(Rb(0, .5), Rb(0, 1), Rv(0, .5), Rv(0, 1)), 2),
                       m_threshold = round(100 * c(m_star(Rb, .5), m_star(Rb, 1), m_star(Rv, .5), m_star(Rv, 1))))
# Worked example in the main text: R0 = 3, efficacy 90%
Vc <- (1 - 1 / R0) / eff
hasil$Vc <- round(100 * Vc, 1); hasil$R_at_60 <- round(R0 * (1 - eff * 0.60), 2)
pb <- ggplot(d2, aes(100 * m, R, colour = factor(r), linetype = jalur)) +
  geom_hline(yintercept = 1, colour = tinta, linewidth = 0.35) +
  geom_line(linewidth = 0.85) +
  annotate("text", x = 1, y = 1.06, label = "Rt = 1", hjust = 0, vjust = 0, size = 2.6, family = f, colour = tinta) +
  scale_colour_manual(values = c("0.5" = "#a58fc4", "1" = fr[["B"]]), labels = c("0.5" = "r = 0.5", "1" = "r = 1")) +
  scale_linetype_manual(values = c("vaccine and distancing affected" = "solid", "vaccine uptake affected only" = "22")) +
  guides(colour = guide_legend(order = 1, ncol = 1), linetype = guide_legend(order = 2, ncol = 1)) +
  labs(title = "b  Proposition 4: B multiplies the other terms",
       subtitle = "Planned response: 80% vaccine coverage, 30% fewer contacts",
       x = "Population holding misinformation, m (%)", y = expression("Effective reproduction number, " * italic(R)[t])) + tema

## S3. Mobility: importation sets an endemic floor that rises steeply near Rt = 1 --
# Discrete generations: I_{g+1} = R_t I_g + iota.  For R_t < 1 the equilibrium is iota / (1 - R_t).
d3 <- expand.grid(R = seq(0.3, 0.97, 0.005), iota = c(1, 5, 20)); d3$I <- d3$iota / (1 - d3$R)
hasil$S3 <- data.frame(R = c(0.5, 0.8, 0.9, 0.95), I_per_iota = c(0.5, 0.8, 0.9, 0.95) |> (\(r) round(1 / (1 - r), 1))())
pc <- ggplot(d3, aes(R, I, colour = factor(iota))) +
  geom_line(linewidth = 0.9) +
  scale_y_log10(labels = scales::label_comma()) +
  scale_colour_manual(values = c("1" = "#a9c1dd", "5" = "#6f96c3", "20" = fr[["G"]]),
                      labels = c(expression(iota == 1 ~ "imported case per generation"), expression(iota == 5 ~ "imported cases per generation"), expression(iota == 20 ~ "imported cases per generation"))) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(title = "c  G and the other fractures interact",
       subtitle = expression("Equilibrium cases per generation" == iota / (1 - italic(R)[t])),
       x = expression("Effective reproduction number, " * italic(R)[t] * " (below 1)"), y = "Cases per generation (log scale)") + tema

## S4. Proposition 3: inequity shapes burden ------------------------------------
# Final size with a vaccinated fraction V (all-or-nothing protection, efficacy eff):
#   z = s0 (1 - exp(-R0 z)),  s0 = 1 - eff V   (homogeneous mixing within each group)
fs <- function(R, V) { s0 <- 1 - eff * V; if (R * s0 <= 1) return(0)
  uniroot(function(z) z - s0 * (1 - exp(-R * z)), c(1e-9, s0))$root }
d4 <- expand.grid(V = seq(0, 0.9, 0.005), ifr = c(0.005, 0.010))
d4$z <- sapply(d4$V, function(v) fs(R0, v)); d4$mati <- 1e5 * d4$z * d4$ifr
grp <- data.frame(g = c("better-off group", "disadvantaged group"), V = c(0.70, 0.40), ifr = c(0.005, 0.010))
grp$z <- sapply(grp$V, function(v) fs(R0, v)); grp$Rt <- R0 * (1 - eff * grp$V); grp$mati <- 1e5 * grp$z * grp$ifr
hasil$S4 <- transform(grp, z = round(100 * z, 1), Rt = round(Rt, 2), mati = round(mati))
pd <- ggplot(d4, aes(100 * V, mati, colour = factor(ifr))) +
  geom_line(linewidth = 0.9) +
  geom_point(data = grp, aes(100 * V, mati), inherit.aes = FALSE, colour = tinta, size = 1.8) +
  geom_text(data = grp, aes(100 * V, mati, label = paste0(g, "\n", round(mati), " deaths per 100 000")), inherit.aes = FALSE,
            hjust = c(1, 0), nudge_x = c(-2.5, 2.5), nudge_y = c(40, 0), size = 2.45, family = f, colour = tinta, lineheight = 0.9) +
  scale_colour_manual(values = c("0.005" = "#d9a08b", "0.01" = fr[["K"]]), labels = c("0.005" = "IFR 0.5% (timely care)", "0.01" = "IFR 1.0% (delayed care)")) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(title = "d  Proposition 3: K shapes who dies",
       subtitle = "Final epidemic size by vaccine coverage, R0 = 3",
       x = "Vaccine coverage in the group (%)", y = "Deaths per 100 000") + tema

## Gabung dan simpan -----------------------------------------------------------
g <- (pa | pb) / (pc | pd) + plot_annotation(theme = theme(plot.background = element_rect(fill = "white", colour = NA)))
ggsave(file.path(out, "figureS1-simulation.png"), g, width = 9.2, height = 8.6, dpi = 300, bg = "white", device = ragg::agg_png)
saveRDS(hasil, file.path(out, "hasil-simulasi.rds"))
write.csv(hasil$S1, file.path(out, "S1-reduction-per-term.csv"), row.names = FALSE)
write.csv(hasil$S2, file.path(out, "S2-misinformation-threshold.csv"), row.names = FALSE)
write.csv(hasil$S3, file.path(out, "S3-importation-multiplier.csv"), row.names = FALSE)
write.csv(hasil$S4, file.path(out, "S4-inequity-groups.csv"), row.names = FALSE)
print(hasil)
cat("Simulasi selesai.\n")

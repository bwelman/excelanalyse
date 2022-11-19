library(dplyr)
library(ggplot2)
library(patchwork)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# meetniveaus van variabelen ------------------------------
myd <- grViz("
  digraph stat1 {
    graph[rankdir = LR, bgcolor=transparent]
    node[shape = oval, fontname = Arial]
    V [label = 'Variabele']
    node[shape = box, width = 1.2, fontsize = 12]
    CA [label = 'Categorie']
    NU [label = 'Numeriek']
    node[shape = box, width = 0.8, fontsize = 10]
    NO [label = 'Nominaal']
    OR [label = 'Ordinaal']
    IN [label = 'Interval']
    RA [label = 'Ratio']

    {rank=same CA NU}
    V -> {CA,NU}
    CA -> {NO,OR}
    NU -> {IN,RA}
  }
")

myd %>%
	export_svg() %>%
	charToRaw() %>%
	rsvg_svg("images/data/meetniveaus.svg")

rsvg_png(svg="images/data/meetniveaus.svg", file="images/data/meetniveaus.png")

# diagramselectie -----------------------------------------
myd <- grViz("
    digraph desc {
      # graph opdracht
      graph [rankdir = LR, bgcolor=transparent]

      # node opdrachten
      node[fontname = Sans, shape = cds, style = filled, fillcolor = Khaki, width = 1.5]
      T [label = 'Type variabele']
      node[fontname = Sans, shape = ellipse, style = filled, fillcolor = Beige, width = 2.0]
      C1 [label = '1 categorie\n variabele']
      C2 [label = '2 categorie\nvariabelen']
      N1 [label = '1 numerieke\nvariabele']
      N2 [label = '2 numerieke\nvariabelen']
      CN [label = '1 cat & 1 num\nvariabele']
      node[fontname = Sans, shape = box, style = filled, fillcolor = Azure, width = 2.7]
      C1R [label = 'Frequentietabel\nCirkeldiagram\nKolomdiagram']
      C2R [label = 'Kruistabel\nKolomdiagram (gegroepeerd)']
      N1R [label = 'Statistieken\nHistogram\nBoxplot']
      N2R [label = 'Spreidingsdiagram\nLijndiagram']
      CNR [label = 'Boxplot (gegroepeerd)']
      # edge opdrachten
      T -> {C1, N1,C2,CN,N2}
      C1 -> C1R
      C2 -> C2R
      N1 -> N1R
      N2 -> N2R
      CN -> CNR
    }
")

myd %>%
	export_svg() %>%
	charToRaw() %>%
	rsvg_svg("images/eda/diagramselectie.svg")

rsvg_png(svg="images/eda/diagramselectie.svg", file="images/eda/diagramselectie.png")


# ts-componenten ------------------------------------------
myd <- grViz("
  digraph stat1 {
    graph[rankdir = TB, bgcolor=transparent]
    node[shape = box, fontname = Arial]
    TS [label = 'Tijdreeks']

    node[shape = box, width = 0.8, fontsize = 10]
    T [label = 'Trend (T)\ncomponent']
    S [label = 'Seizoen (S)\ncomponent']
    C [label = 'Cyclische (C)\ncomponent']
    R [label = 'Random (R)\ncomponent']

    TS -> {T, S, C, R}
  }
")

myd %>%
	export_svg() %>%
	charToRaw() %>%
	rsvg_svg("images/timeseries/ts-componenten.svg")

rsvg_png(svg="images/timeseries/ts-componenten.svg", file="images/timeseries/ts-componenten.png")


# sim1-regressie ------------------------------------------
sim1.data <- read.csv2("data/sim1.csv")

myplot <- sim1.data %>%
	mutate(prog = 4.71 + 2.05*x) %>%
	ggplot(aes(x, y)) +
	geom_abline(intercept = 4.71, slope = 2.05, colour = "red") +
	geom_point(colour = "black") +
	geom_linerange(aes(ymin = y, ymax = prog), colour = "blue")  +
	theme_bw()

ggsave(filename = "images/linregressie/sim1-regressie.png", plot = myplot, width = 5.50, height = 4.30, dpi = 300)


# airpassengers -------------------------------------------
myplot <- forecast::autoplot(AirPassengers) +
	labs(title = "Internationale vliegtuigpassagiers per maand (1949-1960)",
		 x = "Tijd",
		 y = "Aantal passagiers") +
	theme_bw()

ggsave(filename = "images/timeseries/airpassengers.png", plot = myplot, width = 5.50, height = 4.30, dpi = 300)


# ap-trend ------------------------------------------------
ap.components <- decompose(AirPassengers,type='multiplicative')

myplot <- autoplot(ap.components$trend) +
	labs(title = "",
		 x = "Tijd",
		 y = "") +
	theme_bw()

ggsave(filename = "images/timeseries/ap-trend.png", plot = myplot, width = 5.50, height = 4.30, dpi = 300)

# ap-season -----------------------------------------------
ap.components <- decompose(AirPassengers,type='multiplicative')

myplot <- autoplot(ap.components$seasonal) +
	labs(title = "",
		 x = "Tijd",
		 y = "") +
	theme_bw()

ggsave(filename = "images/timeseries/ap-season.png", plot = myplot, width = 5.50, height = 4.30, dpi = 300)

# ap-random -----------------------------------------------
ap.components <- decompose(AirPassengers,type='multiplicative')

myplot <- autoplot(ap.components$random) +
	labs(title = "",
		 x = "Tijd",
		 y = "") +
	theme_bw()

ggsave(filename = "images/timeseries/ap-random.png", plot = myplot, width = 5.50, height = 4.30, dpi = 300)

# scheefheid ----------------------------------------------
dfs <- data.frame(
	xl = rbeta(n = 10000, shape1 = 5, shape2 = 2),  # links scheef
	xr = rbeta(n = 10000, shape1 = 2, shape2 = 5))   # rechts scheef

l_scheef <- ggplot(data = dfs, aes(x = xl)) +
	geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
	geom_density(color="red") +
	labs(title = "Links scheef", x ="x", y="dichtheid") +
	theme_minimal()
r_scheef <- ggplot(data = dfs, aes(x = xr)) +
	geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
	geom_density(color="blue") +
	labs(title = "Rechts scheef", x ="x", y="dichtheid") +
	theme_minimal()

ggsave(filename = "images/beschrijvend/scheefheid.png", plot = r_scheef + l_scheef, width = 5.0, height = 3.5, dpi=300)


# kurtosis ------------------------------------------------
xn <- rep(seq(from=65, to=135, by=5),
		  times = c(3,4,4,4,5,5,5,5,5,5,5,4,4,4,3))
xp <- rep(seq(from=65, to=135, by=5),
		  times = c(2,2,3,3,3,4,27,32,27,4,3,3,3,2,2))

dfxn <- data.frame(xn)
n_kurtosis <- ggplot(data = dfxn, aes(xn)) +
	geom_density() +
	labs(title = "Negatieve kurtosis", x ="x", y="dichtheid") +
	theme_minimal()


dfxp <- data.frame(xp)
p_kurtosis <- ggplot(data = dfxp, aes(xp)) +
	geom_density() +
	labs(title = "Positieve kurtosis", x ="x", y="dichtheid") +
	theme_minimal()

ggsave(filename = "images/beschrijvend/kurtosis.png", plot = n_kurtosis + p_kurtosis, width = 5.0, height = 3.5, dpi=300)


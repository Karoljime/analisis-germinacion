
# ANALISIS UNIVARIADO Y MULTIVARIADO
# GERMINACION DE ARVEJA

# Cargar paquete para leer Excel
library(readxl)

# Importar base de datos
datos <- read_excel("GerminaR.xlsx",
                    sheet = "pisum")

# Visualizar los datos
View(datos)

# Mostrar nombres de columnas
names(datos)

# PASO 4: PREPARAR VARIABLES

# Convertir tratamientos en factores
datos$Inoculant <- as.factor(datos$Inoculant)
datos$peg <- as.factor(datos$peg)
datos$block <- as.factor(datos$block)

# Revisar estructura
str(datos)

# Revisar semillas y germinacion
summary(datos[, c("seeds", "ger_0", "ger_7")])



# PASO 7: ANOVA FACTORIAL

# Crear modelo estadistico
modelo <- aov(
  germinacion ~ block + Inoculant * peg,
  data = datos
)

# Mostrar tabla ANOVA
summary(modelo)

# PASO 8: VERIFICAR SUPUESTOS DEL ANOVA

# Prueba de normalidad de residuos
shapiro.test(residuals(modelo))

# Prueba de homogeneidad de varianzas
bartlett.test(
  germinacion ~ interaction(Inoculant, peg),
  data = datos
)

# Graficos de diagnostico
par(mfrow = c(2, 2))
plot(modelo)
par(mfrow = c(1, 1))




# PASO 11: MODELO BINOMIAL REDUCIDO

# Modelo sin interaccion
modelo_reducido <- glm(
  cbind(ger_7, no_germinadas) ~
    block + Inoculant + peg,
  family = binomial,
  data = datos
)

# Evaluar efectos principales
drop1(
  modelo_reducido,
  test = "Chisq"
)

# Revisar dispersion
dispersion_reducida <- sum(
  residuals(modelo_reducido, type = "pearson")^2
) / df.residual(modelo_reducido)

print(dispersion_reducida)

# PASO 12: GRAFICO UNIVARIADO

# Instalar solo si no tienes ggplot2
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

# Grafico de germinacion por tratamientos
grafico1 <- ggplot(
  datos,
  aes(x = peg, y = germinacion,
      fill = Inoculant)
) +
  stat_summary(
    fun = mean,
    geom = "col",
    position = position_dodge(width = 0.8)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    title = "Germinacion de arveja",
    x = "Nivel de estres hidrico (PEG)",
    y = "Germinacion (%)",
    fill = "Inoculante"
  ) +
  coord_cartesian(ylim = c(0, 105)) +
  theme_minimal()

print(grafico1)

# PASO 14: ANALISIS MULTIVARIADO
# MATRIZ DE CORRELACIONES

# Seleccionar variables numericas
multi <- datos[, c(
  "germinacion",
  "biomasa",
  "peso"
)]

# Revisar estructura de variables
str(multi)

# Comprobar datos faltantes
colSums(is.na(multi))

# Calcular matriz de correlaciones
matriz <- cor(
  multi,
  use = "complete.obs"
)

# Mostrar resultados
print(matriz)

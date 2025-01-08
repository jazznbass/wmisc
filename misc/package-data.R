
load(file.path("misc", "ex.rds"))

ex_itrf <- scaledic::ex_itrf
ex_itrf_scales <- scaledic::get_scales(
  ex_itrf,
  Int = scale == "ITRF" & subscale == "Int",
  Ext = scale == "ITRF" & subscale == "Ext"
)


model_lmer_1 <- wmisc:::model_lmer_1
model_lmer_2 <- wmisc:::model_lmer_2

model_glmer_1 <- wmisc:::model_glmer_1
model_glmer_2 <- wmisc:::model_glmer_2


mtcars_labeled <- add_label(mtcars, list(
  mpg = "Miles/(US) gallon",
  cyl = "Number of cylinders",
  disp =	"Displacement (cu.in.)",
  hp	= "Gross horsepower",
  drat = "Rear axle ratio",
  wt="Weight (1000 lbs)",
  qsec="1/4 mile time",
  vs="Engine (0 = V-shaped, 1 = straight)",
  am="Transmission (0 = automatic, 1 = manual)",
  gear="Number of forward gears",
  carb="Number of carburetors"
))

save(
  mtcars_labeled,
  model_lmer_1, 
  model_lmer_2,
  model_glmer_1, 
  model_glmer_2,
  ex_agreement,
  ex_itrf,
  ex_itrf_scales,
  file = file.path("R", "sysdata.rda"),
  compress = "xz"
)


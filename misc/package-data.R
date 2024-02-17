
load(file.path("misc", "ex.rds"))

ex_itrf <- scaledic::ex_itrf
ex_itrf_scales <- get_scales(ex_itrf,
           Int = scale == "ITRF" & subscale == "Int",
           Ext = scale == "ITRF" & subscale == "Ext"
)


model_lmer_1 <- wmisc:::model_lmer_1
model_lmer_2 <- wmisc:::model_lmer_2

model_glmer_1 <- wmisc:::model_glmer_1
model_glmer_2 <- wmisc:::model_glmer_2

save(
  model_lmer_1, 
  model_lmer_2,
  model_glmer_1, 
  model_glmer_2,
  ex_agreement,
  ex_itrf,
  ex_itrf_scales,
  file = file.path("R", "sysdata.rda")
)


alpha_table(
  data = wmisc:::ex_itrf, 
  scales = wmisc:::ex_itrf_scales, 
  difficulty = TRUE, 
  values = list(c(0, 3)), 
  RMSEA = TRUE
)

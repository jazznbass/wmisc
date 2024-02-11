
model_lmer_1 <- wmisc:::model1
model_lmer_2 <- wmisc:::model2
model_glmer_1 <- wmisc:::model3
model_glmer_2 <- wmisc:::model4


save(
  model_lmer_1, model_lmer_2,
  model_glmer_1, model_glmer_2,
  
  file = "sysdata.rda"
)

nice_regression_table(
  nlme::lme(mpg~disp, data = mtcars, random = ~1|am),
  nlme::lme(mpg~disp + hp, data = mtcars, random = ~1|am)
)
model1 <- nlme::lme(mpg~disp+hp, data = mtcars, random = ~1|am)

nlme::lme(mpg~disp+hp, data = mtcars, random = ~1|am)
model1b<-nlme::lme(mpg~disp+hp, data = mtcars, random = ~1+hp|am)

nlme::getVarCov(model1b)

model2 <- nlme::lme(mpg~disp+hp+cyl, data = mtcars, random = ~1|am)

nice_regression_table(lmer(mpg~disp+hp, data = mtcars, random = ~1|am))

sjPlot::tab_model(model1)

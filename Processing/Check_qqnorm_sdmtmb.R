
# delta_lognormal()
rq_res <- residuals(model_fit, type = "mle-mvn",model = 1)
rq_res <- rq_res[is.finite(rq_res)] # in case some are Inf
qqnorm(rq_res);abline(0, 1)

rq_res <- residuals(model_fit, type = "mle-mvn",model = 2)
rq_res <- rq_res[is.finite(rq_res)] # in case some are Inf
qqnorm(rq_res);abline(0, 1)

# lognormal() or tweedie
rq_res <- residuals(model_fit, type = "mle-mvn")
rq_res <- rq_res[is.finite(rq_res)] # in case some are Inf
qqnorm(rq_res);abline(0, 1)

model_fit_tweedie <- model_fit

AIC(model_fit_tweedie, model_fit)

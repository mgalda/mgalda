structure(
  list(
    nombre = c(
      "lineal",
      "quadratic",
      "exp1",
      "exp2",
      "exp3",
      "asymp",
      "power",
      "logarithmic",
      "michment",
      "yieldloss",
      "sygmoidallog",
      "gompertz",
      "loglogistic1",
      "loglogistic2",
      "weibull1",
      "weibull2",
      "bc_3",
      "bc_4",
      "bc_5",
      "beta",
      "bragg_3",
      "bragg_4",
      "cousens85",
      "e2",
      "e3",
      "e4",
      "expodecay",
      "expogrowth",
      "g2",
      "g3",
      "g4",
      "l2",
      "l3",
      "l4",
      "linear",
      "linearorigin",
      "ll_comb",
      "ll2",
      "ll3",
      "ll4",
      "logbragg_3",
      "lorentz_3",
      "lorentz_4",
      "negexp",
      "negexpdist",
      "triangle",
      "w1_2",
      "w1_3",
      "w1_4",
      "w2_2",
      "w2_3",
      "w2_4",
      "yl"
    ),
    curva = list(
      lineal = quote(a + b * x),
      quadratic = quote(a + b * x + c * (x^2)),
      exp1 = quote(a * exp(b * x)),
      exp2 = quote(exp(a + b * x)),
      exp3 = quote(a * exp(-x / 2.718)),
      asymp = quote(a - (a - b) * exp(-c * x)),
      power = quote(a * x^b),
      logarithmic = quote(a + b * log(x)),
      michment = quote((a * x) / (b + x)),
      yieldloss = quote(b * x / (1 + b / a * x)),
      sygmoidallog = quote(c + (a - c) / (1 + exp((
        x - 2.718
      ) * b))),
      gompertz = quote(c + exp(-exp((
        x - 2.718
      ) * b)) * (a - c)),
      loglogistic1 = quote(c + (a - c) / (exp((
        log(x) - log(exp(1))
      ) * b) + 1)),
      loglogistic2 = quote(c + (a - c) / (1 + (x / exp(
        1
      ))^b)),
      weibull1 = quote(c + (a - c) * (1 - exp(-exp(
        b * log(x) - log(exp(1))
      )))),
      weibull2 = quote(c + (a - c) * exp(-exp((
        log(x) - log(1)
      ) * b))),
      BC_3 = quote((c * x) / (1 + exp(-a * (
        log(x + 1E-6) - log(b)
      )))),
      BC_4 = quote((d + c * x) / (1 + exp(-b * (
        log(x + 1E-6) - log(a)
      )))),
      BC_5 = quote(c + (d - c + a * x) / (1 + exp(-b * (
        log(x + 1E-6) - log(e)
      )))),
      beta = quote(ifelse(x > a &
                            x < e, d * (((x - a) / (c - a) * ((e - x) / (e - c))
                            )^((e - c) / (c - a)
                            ))^b, 0)),
      bragg_3 = quote(c * exp(-b * (x - a)^2)),
      bragg_4 = quote(c + (d - c) * exp(-b * (x - a)^2)),
      cousens85 = quote(a * (1 - (b * x) / (100 * (
        1 + b * x / c
      )))),
      E2 = quote(1 - exp(-exp(b * (
        x - a
      )))),
      E3 = quote(c * (1 - exp(-exp(
        b * (x - a)
      )))),
      E4 = quote(c + (d - c) * (1 - exp(-exp(
        b * (x - a)
      )))),
      expoDecay = quote(a * exp(-b * x)),
      expoGrowth = quote(a * exp(b * x)),
      G2 = quote(exp(-exp(-b * (
        x - a
      )))),
      G3 = quote(c * (exp(-exp(
        -b * (x - a)
      )))),
      G4 = quote(c + (d - c) * (exp(-exp(
        -b * (x - a)
      )))),
      L2 = quote(1 / (1 + exp(-b * (
        x - a
      )))),
      L3 = quote(c / (1 + exp(-b * (
        x - a
      )))),
      L4 = quote(c + (d - c) / (1 + exp(-b * (
        x - a
      )))),
      linear = quote(a + b * x),
      linearOrigin = quote(a * x),
      LL_Comb = quote(1 / (1 + exp(-a * (
        log(x + 1E-6) - log(c)
      ))) + 1 / (1 + exp(-b * (
        log(x + 1E-6) - log(d)
      )))),
      LL2 = quote(1 / (1 + exp(-b * (
        log(x + 1E-6) - log(a)
      )))),
      LL3 = quote(c / (1 + exp(-b * (
        log(x + 1E-6) - log(a)
      )))),
      LL4 = quote(c + (d - c) / (1 + exp(-b * (
        log(x + 1E-6) - log(a)
      )))),
      logBragg_3 = quote(a * exp(-b * (log(
        x + 1E-6
      ) - c)^2)),
      lorentz_3 = quote(c / (1 + b * (x - a)^2)),
      lorentz_4 = quote(c + (d - c) / (1 + b * (x - a)^2)),
      negExp = quote(a * (1 - exp(-b * x))),
      negExpDist = quote(1 - exp(-a * x)),
      triangle = quote(case_when(
        x < b ~ 0, x < c ~ a * (x - b) / (c - b), x < d ~ a * (d - x) / (d - c), T ~ 0
      )),
      W1_2 = quote(exp(-exp(-b * (
        log(x + 1E-6) - log(a)
      )))),
      W1_3 = quote(b * exp(-exp(-c * (
        log(x + 1E-6) - log(a)
      )))),
      W1_4 = quote(c + (d - c) * exp(-exp(-b * (
        log(x + 1E-6) - log(a)
      )))),
      W2_2 = quote(1 - exp(-exp(b * (
        log(x + 1E-6) - log(a)
      )))),
      W2_3 = quote(c * (1 - exp(-exp(
        b * (log(x + 1E-6) - log(a))
      )))),
      W2_4 = quote(c + (d - c) * (1 - exp(-exp(
        b * (log(x + 1E-6) - log(a))
      )))),
      YL = quote(b * x / (1 + b / a * x))
    ),
    derivada = list(
      lineal = quote(b * 1),
      quadratic = quote(b + c * (2 * x)),
      exp1 = quote(a * (exp(b * x) * b)),
      exp2 = quote(exp(a + b * x) * b),
      exp3 = quote(-(a * (
        exp(-x / 2.718) * (1 / 2.718)
      ))),
      asymp = quote((a - b) * (exp(-c * x) * c)),
      power = quote(a * (x^(b - 1) * b)),
      logarithmic = quote(b * (1 / x)),
      michment = quote(a / (b + x) - (a * x) / (b + x)^2),
      yieldloss = quote(b / (1 + b / a * x) - b * x * (b / a) / (1 + b / a * x)^
                          2),
      sygmoidallog = quote(-((a - c) * (exp((x - 2.718) * b) * b) / (1 + exp((x - 2.718) * b))^2)),
      gompertz = quote(-(exp(-exp((x - 2.718) * b)) * (exp((x - 2.718) * b) * b) * (a - c))),
      loglogistic1 = quote(-((a - c) * (exp((log(x) - log(exp(1))) * b) * (
        1 / x * b
      )) / (exp((log(x) - log(exp(1))) * b) + 1)^2)),
      loglogistic2 =
        quote(-((a - c) * ((x / exp(1))^(b - 1) * (b * (1 / exp(1)))
        ) / (1 + (
          x / exp(1)
        )^b)^2)),
      weibull1 = quote((a - c) * (exp(-exp(
        b * log(x) - log(exp(1))
      )) * (
        exp(b * log(x) - log(exp(1))) * (b * (1 / x))
      ))),
      weibull2 = quote(-((a - c) * (
        exp(-exp((log(
          x
        ) - log(
          1
        )) * b)) * (exp((log(
          x
        ) - log(
          1
        )) * b) * (1 / x * b))
      ))),
      BC_3 = quote(c / (1 + exp(-a * (
        log(x + 1E-6) - log(b)
      ))) + (c * x) * (exp(-a * (
        log(x + 1E-6) - log(b)
      )) * (a * (
        1 / (x + 1E-6)
      ))) / (1 + exp(-a * (
        log(x + 1E-6) - log(b)
      )))^2),
      BC_4 = quote(c / (1 + exp(-b * (
        log(x + 1E-6) - log(a)
      ))) + (d + c * x) * (exp(-b * (
        log(x + 1E-6) - log(a)
      )) * (b * (
        1 / (x + 1E-6)
      ))) / (1 + exp(-b * (
        log(x + 1E-6) - log(a)
      )))^2),
      BC_5 = quote(a / (1 + exp(-b * (
        log(x + 1E-6) - log(e)
      ))) + (d - c + a * x) * (exp(-b * (
        log(x + 1E-6) - log(e)
      )) * (b * (
        1 / (x + 1E-6)
      ))) / (1 + exp(-b * (
        log(x + 1E-6) - log(e)
      )))^2),
      beta = quote(ifelse(x > a &
                            x < e, d * ((((x - a) / (c - a) * ((e - x) / (e - c)))^((e - c) / (c - a))
                            )^(
                              b - 1
                            ) * (
                              b * (((x - a) / (c - a) * ((
                                e - x
                              ) / (
                                e - c
                              )))^(((
                                e - c
                              ) / (
                                c - a
                              )) - 1) * (((
                                e - c
                              ) / (
                                c - a
                              )) * (
                                1 / (c - a) * ((e - x) / (e - c)) - (x - a) / (c - a) * (1 / (e - c))
                              )))
                            )), 0)),
      bragg_3 = quote(-(c * (exp(
        -b * (x - a)^2
      ) * (
        b * (2 * (x - a))
      )))),
      bragg_4 = quote(-((d - c) * (exp(
        -b * (x - a)^2
      ) * (
        b * (2 * (x - a))
      )))),
      cousens85 = quote(-(a * (
        b / (100 * (1 + b * x / c)) - (b * x) * (100 * (b / c)) / (100 * (1 + b * x / c))^
          2
      ))),
      E2 = quote(exp(-exp(b * (
        x - a
      ))) * (exp(b * (
        x - a
      )) * b)),
      E3 = quote(c * (exp(-exp(
        b * (x - a)
      )) * (exp(
        b * (x - a)
      ) * b))),
      E4 = quote((d - c) * (exp(-exp(
        b * (x - a)
      )) * (exp(
        b * (x - a)
      ) * b))),
      expoDecay = quote(-(a * (exp(
        -b * x
      ) * b))),
      expoGrowth = quote(a * (exp(b * x) * b)),
      G2 = quote(exp(-exp(-b * (
        x - a
      ))) * (exp(-b * (
        x - a
      )) * b)),
      G3 = quote(c * (exp(-exp(
        -b * (x - a)
      )) * (exp(
        -b * (x - a)
      ) * b))),
      G4 = quote((d - c) * (exp(-exp(
        -b * (x - a)
      )) * (exp(
        -b * (x - a)
      ) * b))),
      L2 = quote(exp(-b * (x - a)) * b / (1 + exp(-b * (
        x - a
      )))^2),
      L3 = quote(c * (exp(-b * (
        x - a
      )) * b) / (1 + exp(-b * (
        x - a
      )))^2),
      L4 = quote((d - c) * (exp(-b * (
        x - a
      )) * b) / (1 + exp(-b * (
        x - a
      )))^2),
      linear = quote(b),
      linearOrigin = quote(a),
      LL_Comb = quote(exp(-a * (log(
        x + 1E-6
      ) - log(
        c
      ))) * (a * (1 / (
        x + 1E-6
      ))) / (1 + exp(-a * (
        log(x + 1E-6) - log(c)
      )))^2 + exp(-b * (log(
        x + 1E-6
      ) - log(
        d
      ))) * (b * (1 / (
        x + 1E-6
      ))) / (1 + exp(-b * (
        log(x + 1E-6) - log(d)
      )))^2),
      LL2 = quote(exp(-b * (log(
        x + 1E-6
      ) - log(
        a
      ))) * (b * (1 / (
        x + 1E-6
      ))) / (1 + exp(-b * (
        log(x + 1E-6) - log(a)
      )))^2),
      LL3 = quote(c * (exp(-b * (
        log(x + 1E-6) - log(a)
      )) * (b * (
        1 / (x + 1E-6)
      ))) / (1 + exp(-b * (
        log(x + 1E-6) - log(a)
      )))^2),
      LL4 = quote((d - c) * (exp(-b * (
        log(x + 1E-6) - log(a)
      )) * (b * (
        1 / (x + 1E-6)
      ))) / (1 + exp(-b * (
        log(x + 1E-6) - log(a)
      )))^2),
      logBragg_3 = quote(-(a * (exp(
        -b * (log(x + 1E-6) - c)^2
      ) * (
        b * (2 * (1 / (x + 1E-6) * (log(
          x + 1E-6
        ) - c)))
      )))),
      lorentz_3 = quote(-(c * (b * (
        2 * (x - a)
      )) / (1 + b * (
        x - a
      )^2)^2)),
      lorentz_4 = quote(-((d - c) * (b * (
        2 * (x - a)
      )) / (1 + b * (
        x - a
      )^2)^2)),
      negExp = quote(a * (exp(-b * x) * b)),
      negExpDist = quote(exp(-a * x) * a),
      triangle = quote(case_when(x < b ~ 0, x < c ~ a / (c - b), x < d ~ -(a / (
        d - c
      )), T ~ 0)),
      W1_2 = quote(exp(-exp(-b * (
        log(x + 1E-6) - log(a)
      ))) * (exp(-b * (
        log(x + 1E-6) - log(a)
      )) * (b * (
        1 / (x + 1E-6)
      )))),
      W1_3 = quote(b * (exp(-exp(
        -c * (log(x + 1E-6) - log(a))
      )) * (exp(
        -c * (log(x + 1E-6) - log(a))
      ) * (
        c * (1 / (x + 1E-6))
      )))),
      W1_4 = quote((d - c) * (exp(-exp(
        -b * (log(x + 1E-6) - log(a))
      )) * (exp(
        -b * (log(x + 1E-6) - log(a))
      ) * (
        b * (1 / (x + 1E-6))
      )))),
      W2_2 = quote(exp(-exp(b * (
        log(x + 1E-6) - log(a)
      ))) * (exp(b * (
        log(x + 1E-6) - log(a)
      )) * (b * (
        1 / (x + 1E-6)
      )))),
      W2_3 = quote(c * (exp(-exp(
        b * (log(x + 1E-6) - log(a))
      )) * (exp(
        b * (log(x + 1E-6) - log(a))
      ) * (
        b * (1 / (x + 1E-6))
      )))),
      W2_4 = quote((d - c) * (exp(-exp(
        b * (log(x + 1E-6) - log(a))
      )) * (exp(
        b * (log(x + 1E-6) - log(a))
      ) * (
        b * (1 / (x + 1E-6))
      )))),
      YL = quote(b / (1 + b / a * x) - b * x * (b / a) / (1 + b / a * x)^
                   2)
    ),
    args = list(
      lineal = c("a", "b", "x"),
      quadratic = c("a", "b", "c", "x"),
      exp1 = c("a", "b", "x"),
      exp2 = c("a", "b", "x"),
      exp3 = c("a", "x"),
      asymp = c("a", "b", "c", "x"),
      power = c("a", "b", "x"),
      logarithmic = c("a", "b", "x"),
      michment = c("a", "b", "x"),
      yieldloss = c("a", "b", "x"),
      sygmoidallog = c("a", "b", "c", "x"),
      gompertz = c("a", "b", "c", "x"),
      loglogistic1 = c("a", "b", "c", "x"),
      loglogistic2 = c("a", "b", "c", "x"),
      weibull1 = c("a", "b", "c", "x"),
      weibull2 = c("a", "b", "c", "x"),
      BC_3 = c("a", "b", "c", "x"),
      BC_4 = c("a", "b", "c", "d", "x"),
      BC_5 = c("a", "b", "c", "d", "e", "x"),
      beta = c("a", "b", "c", "d", "e", "x"),
      bragg_3 = c("a", "b", "c", "x"),
      bragg_4 = c("a", "b", "c", "d", "x"),
      cousens85 = c("a", "b", "c", "x"),
      E2 = c("a", "b", "x"),
      E3 = c("a", "b", "c", "x"),
      E4 = c("a", "b", "c", "d", "x"),
      expoDecay = c("a", "b", "x"),
      expoGrowth = c("a", "b", "x"),
      G2 = c("a", "b", "x"),
      G3 = c("a", "b", "c", "x"),
      G4 = c("a", "b", "c", "d", "x"),
      L2 = c("a", "b", "x"),
      L3 = c("a", "b", "c", "x"),
      L4 = c("a", "b", "c", "d", "x"),
      linear = c("a", "b", "x"),
      linearOrigin = c("a", "x"),
      LL_Comb = c("a", "b", "c", "d", "x"),
      LL2 = c("a", "b", "x"),
      LL3 = c("a", "b", "c", "x"),
      LL4 = c("a", "b", "c", "d", "x"),
      logBragg_3 = c("a", "b", "c", "x"),
      lorentz_3 = c("a", "b", "c", "x"),
      lorentz_4 = c("a", "b", "c", "d", "x"),
      negExp = c("a", "b", "x"),
      negExpDist = c("a", "x"),
      triangle = c("a", "b", "c", "d", "x"),
      W1_2 = c("a", "b", "x"),
      W1_3 = c("a", "b", "c", "x"),
      W1_4 = c("a", "b", "c", "d", "x"),
      W2_2 = c("a", "b", "x"),
      W2_3 = c("a", "b", "c", "x"),
      W2_4 = c("a", "b", "c", "d", "x"),
      YL = c("a", "b", "x")
    ),
    fn_curva = list(
      lineal = function(a, b, x) {
        a + b * x
      },
      quadratic = function(a, b, c, x) {
        a + b * x + c * (x^2)
      },
      exp1 = function(a, b, x) {
        a * exp(b * x)
      },
      exp2 = function(a, b, x) {
        exp(a + b * x)
      },
      exp3 = function(a, x) {
        a * exp(-x / 2.718)
      },
      asymp = function(a, b, c, x) {
        a - (a - b) * exp(-c * x)
      },
      power = function(a, b, x) {
        a * x^b
      },
      logarithmic = function(a, b, x) {
        a + b * log(x)
      },
      michment = function(a, b, x) {
        (a * x) / (b + x)
      },
      yieldloss = function(a, b, x) {
        b * x / (1 + b / a * x)
      },
      sygmoidallog = function(a, b, c, x) {
        c + (a - c) / (1 + exp((x - 2.718) * b))
      },
      gompertz = function(a, b, c, x) {
        c + exp(-exp((x - 2.718) * b)) * (a - c)
      },
      loglogistic1 = function(a, b, c, x) {
        c + (a - c) / (exp((log(x) - log(exp(
          1
        ))) * b) + 1)
      },
      loglogistic2 = function(a, b, c, x) {
        c + (a - c) / (1 + (x / exp(1))^b)
      },
      weibull1 = function(a, b, c, x) {
        c + (a - c) * (1 - exp(-exp(b * log(x) - log(exp(
          1
        )))))
      },
      weibull2 = function(a, b, c, x) {
        c + (a - c) * exp(-exp((log(x) - log(1)) * b))
      },
      BC_3 = function(a, b, c, x) {
        (c * x) / (1 + exp(-a * (log(x + 1E-6) - log(b))))
      },
      BC_4 = function(a, b, c, d, x) {
        (d + c * x) / (1 + exp(-b * (log(x + 1E-6) - log(a))))
      },
      BC_5 = function(a, b, c, d, e, x) {
        c + (d - c + a * x) / (1 + exp(-b * (log(x + 1E-6) - log(e))))
      },
      beta = function(a, b, c, d, e, x) {
        ifelse(x > a & x < e, d * (((x - a) / (c - a) * ((
          e - x
        ) / (
          e -
            c
        )))^((e - c) / (c - a)))^b, 0)
      },
      bragg_3 = function(a, b, c, x) {
        c * exp(-b * (x - a)^2)
      },
      bragg_4 = function(a, b, c, d, x) {
        c + (d - c) * exp(-b * (x - a)^2)
      },
      cousens85 = function(a, b, c, x) {
        a * (1 - (b * x) / (100 * (1 + b * x / c)))
      },
      E2 = function(a, b, x) {
        1 - exp(-exp(b * (x - a)))
      },
      E3 = function(a, b, c, x) {
        c * (1 - exp(-exp(b * (x - a))))
      },
      E4 = function(a, b, c, d, x) {
        c + (d - c) * (1 - exp(-exp(b * (x - a))))
      },
      expoDecay = function(a, b, x) {
        a * exp(-b * x)
      },
      expoGrowth = function(a, b, x) {
        a * exp(b * x)
      },
      G2 = function(a, b, x) {
        exp(-exp(-b * (x - a)))
      },
      G3 = function(a, b, c, x) {
        c * (exp(-exp(-b * (x - a))))
      },
      G4 = function(a, b, c, d, x) {
        c + (d - c) * (exp(-exp(-b * (x - a))))
      },
      L2 = function(a, b, x) {
        1 / (1 + exp(-b * (x - a)))
      },
      L3 = function(a, b, c, x) {
        c / (1 + exp(-b * (x - a)))
      },
      L4 = function(a, b, c, d, x) {
        c + (d - c) / (1 + exp(-b * (x - a)))
      },
      linear = function(a, b, x) {
        a + b * x
      },
      linearOrigin = function(a, x) {
        a * x
      },
      LL_Comb = function(a, b, c, d, x) {
        1 / (1 + exp(-a * (log(x + 1E-6) - log(c)))) + 1 / (1 + exp(-b *
                                                                      (log(x + 1E-6) - log(d))))
      },
      LL2 = function(a, b, x) {
        1 / (1 + exp(-b * (log(x + 1E-6) - log(a))))
      },
      LL3 = function(a, b, c, x) {
        c / (1 + exp(-b * (log(x + 1E-6) - log(a))))
      },
      LL4 = function(a, b, c, d, x) {
        c + (d - c) / (1 + exp(-b * (log(x + 1E-6) - log(a))))
      },
      logBragg_3 = function(a, b, c, x) {
        a * exp(-b * (log(x + 1E-6) - c)^2)
      },
      lorentz_3 = function(a, b, c, x) {
        c / (1 + b * (x - a)^2)
      },
      lorentz_4 = function(a, b, c, d, x) {
        c + (d - c) / (1 + b * (x - a)^2)
      },
      negExp = function(a, b, x) {
        a * (1 - exp(-b * x))
      },
      negExpDist = function(a, x) {
        1 - exp(-a * x)
      },
      triangle = function(a, b, c, d, x) {
        case_when(x < b ~ 0, x < c ~ a * (x - b) / (c - b), x < d ~
                    a * (d - x) / (d - c), T ~ 0)
      },
      W1_2 = function(a, b, x) {
        exp(-exp(-b * (log(x + 1E-6) - log(a))))
      },
      W1_3 = function(a, b, c, x) {
        b * exp(-exp(-c * (log(x + 1E-6) - log(a))))
      },
      W1_4 = function(a, b, c, d, x) {
        c + (d - c) * exp(-exp(-b * (log(x + 1E-6) - log(a))))
      },
      W2_2 = function(a, b, x) {
        1 - exp(-exp(b * (log(x + 1E-6) - log(a))))
      },
      W2_3 = function(a, b, c, x) {
        c * (1 - exp(-exp(b * (
          log(x + 1E-6) - log(a)
        ))))
      },
      W2_4 = function(a, b, c, d, x) {
        c + (d - c) * (1 - exp(-exp(b * (
          log(x + 1E-6) - log(a)
        ))))
      },
      YL = function(a, b, x) {
        b * x / (1 + b / a * x)
      }
    ),
    fn_deriv =
      list(
        lineal = function(a, b, c, x) {
          b + c * (2 * x)
        },
        quadratic = function(a, b, c, x) {
          b + c * (2 * x)
        },
        exp1 = function(a, b, x) {
          a * (exp(b * x) * b)
        },
        exp2 = function(a, b, x) {
          exp(a + b * x) * b
        },
        exp3 = function(a, x) {
          -(a * (exp(-x / 2.718) * (1 / 2.718)))
        },
        asymp = function(a, b, c, x) {
          (a - b) * (exp(-c * x) * c)
        },
        power = function(a, b, x) {
          a * (x^(b - 1) * b)
        },
        logarithmic = function(a, b, x) {
          b * (1 / x)
        },
        michment = function(a, b, x) {
          a / (b + x) - (a * x) / (b + x)^2
        },
        yieldloss = function(a, b, x) {
          b / (1 + b / a * x) - b * x * (b / a) / (1 + b / a * x)^2
        },
        sygmoidallog = function(a, b, c, x) {
          -((a - c) * (exp((x - 2.718) * b) * b) / (1 + exp((x - 2.718) *
                                                              b))^2)
        },
        gompertz = function(a, b, c, x) {
          -(exp(-exp((x - 2.718) * b)) * (exp((x - 2.718) * b) * b) * (a -
                                                                         c))
        },
        loglogistic1 = function(a, b, c, x) {
          -((a - c) * (exp((
            log(x) - log(exp(1))
          ) * b) * (1 / x * b)) / (exp((
            log(x) -
              log(exp(1))
          ) * b) + 1)^2)
        },
        loglogistic2 = function(a, b, c, x) {
          -((a - c) * ((x / exp(1))^(b - 1) * (b * (1 / exp(
            1
          )))) / (1 + (x / exp(1))^b)^2)
        },
        weibull1 = function(a, b, c, x) {
          (a - c) * (exp(-exp(b * log(x) - log(exp(
            1
          )))) * (exp(b *
                        log(x) - log(exp(
                          1
                        ))) * (b * (1 / x))))
        },
        weibull2 = function(a, b, c, x) {
          -((a - c) * (exp(-exp((
            log(x) - log(1)
          ) * b)) * (exp((
            log(x) -
              log(1)
          ) * b) * (1 / x * b))))
        },
        BC_3 = function(a, b, c, x) {
          c / (1 + exp(-a * (log(x + 0.000001) - log(b)))) + (c * x) *
            (exp(-a * (log(x + 0.000001) - log(b))) * (a * (1 / (x +
                                                                   0.000001)))) / (1 + exp(-a * (log(x + 0.000001) - log(b))))^
            2
        },
        BC_4 = function(a, b, c, d, x) {
          c / (1 + exp(-b * (log(x + 0.000001) - log(a)))) + (d + c *
                                                                x) * (exp(-b * (log(x + 0.000001) - log(a))) * (b * (1 / (x +
                                                                                                                            0.000001)))) / (1 + exp(-b * (log(x + 0.000001) - log(a))))^
            2
        },
        BC_5 = function(a, b, c, d, e, x) {
          a / (1 + exp(-b * (log(x + 0.000001) - log(e)))) + (d - c +
                                                                a * x) * (exp(-b * (log(x + 0.000001) - log(e))) * (b *
                                                                                                                      (1 / (x + 0.000001)))) / (1 + exp(-b * (log(x + 0.000001) -
                                                                                                                                                                log(e))))^2
        },
        beta = function(a, b, c, d, e, x) {
          ifelse(x > a & x < e, d * ((((x - a) / (c - a) * ((e - x) / (e -
                                                                         c))
          )^((
            e - c
          ) / (
            c - a
          )))^(b - 1) * (b * (((x - a) / (c -
                                            a) * ((e - x) / (e - c)))^(((e - c) / (c - a)) - 1) * (((e -
                                                                                                       c) / (c - a)) * (1 / (c - a) * ((
                                                                                                         e - x
                                                                                                       ) / (
                                                                                                         e - c
                                                                                                       )) - (x - a) / (c -
                                                                                                                         a) * (1 / (
                                                                                                                           e - c
                                                                                                                         ))))
          ))), 0)
        },
        bragg_3 = function(a, b, c, x) {
          -(c * (exp(-b * (x - a)^2) * (b * (2 * (
            x - a
          )))))
        },
        bragg_4 = function(a, b, c, d, x) {
          -((d - c) * (exp(-b * (x - a)^2) * (b * (2 * (
            x - a
          )))))
        },
        cousens85 = function(a, b, c, x) {
          -(a * (b / (100 * (1 + b * x / c)) - (b * x) * (100 * (b / c)) / (100 *
                                                                              (1 + b * x / c))^2))
        },
        E2 = function(a, b, x) {
          exp(-exp(b * (x - a))) * (exp(b * (x - a)) * b)
        },
        E3 = function(a, b, c, x) {
          c * (exp(-exp(b * (x - a))) * (exp(b * (x - a)) * b))
        },
        E4 = function(a, b, c, d, x) {
          (d - c) * (exp(-exp(b * (x - a))) * (exp(b * (x - a)) * b))
        },
        expoDecay = function(a, b, x) {
          -(a * (exp(-b * x) * b))
        },
        expoGrowth = function(a, b, x) {
          a * (exp(b * x) * b)
        },
        G2 = function(a, b, x) {
          exp(-exp(-b * (x - a))) * (exp(-b * (x - a)) * b)
        },
        G3 = function(a, b, c, x) {
          c * (exp(-exp(-b * (x - a))) * (exp(-b * (x - a)) * b))
        },
        G4 = function(a, b, c, d, x) {
          (d - c) * (exp(-exp(-b * (x - a))) * (exp(-b * (x - a)) *
                                                  b))
        },
        L2 = function(a, b, x) {
          exp(-b * (x - a)) * b / (1 + exp(-b * (x - a)))^2
        },
        L3 = function(a, b, c, x) {
          c * (exp(-b * (x - a)) * b) / (1 + exp(-b * (x - a)))^2
        },
        L4 = function(a, b, c, d, x) {
          (d - c) * (exp(-b * (x - a)) * b) / (1 + exp(-b * (x - a)))^2
        },
        linear = function(a, b, x) {
          b
        },
        linearOrigin = function(a, x) {
          a
        },
        LL_Comb = function(a, b, c, d, x) {
          exp(-a * (log(x + 0.000001) - log(c))) * (a * (1 / (x + 0.000001))) / (1 +
                                                                                   exp(-a * (log(x + 0.000001) - log(c))))^2 + exp(-b *
                                                                                                                                     (log(x + 0.000001) - log(d))) * (b * (1 / (x + 0.000001))) / (1 +
                                                                                                                                                                                                     exp(-b * (log(x + 0.000001) - log(d))))^2
        },
        LL2 = function(a, b, x) {
          exp(-b * (log(x + 0.000001) - log(a))) * (b * (1 / (x + 0.000001))) / (1 +
                                                                                   exp(-b * (log(x + 0.000001) - log(a))))^2
        },
        LL3 = function(a, b, c, x) {
          c * (exp(-b * (log(x + 0.000001) - log(a))) * (b * (1 / (x +
                                                                     0.000001)))) / (1 + exp(-b * (log(x + 0.000001) - log(a))))^
            2
        },
        LL4 = function(a, b, c, d, x) {
          (d - c) * (exp(-b * (log(x + 0.000001) - log(a))) * (b *
                                                                 (1 / (x + 0.000001)))) / (1 + exp(-b * (log(x + 0.000001) -
                                                                                                           log(a))))^2
        },
        logBragg_3 = function(a, b, c, x) {
          -(a * (exp(-b * (
            log(x + 0.000001) - c
          )^2) * (b * (2 * (
            1 / (x +
                   0.000001) * (log(x + 0.000001) - c)
          )))))
        },
        lorentz_3 = function(a, b, c, x) {
          -(c * (b * (2 * (x - a))) / (1 + b * (x - a)^2)^2)
        },
        lorentz_4 = function(a, b, c, d, x) {
          -((d - c) * (b * (2 * (x - a))) / (1 + b * (x - a)^2)^2)
        },
        negExp = function(a, b, x) {
          a * (exp(-b * x) * b)
        },
        negExpDist = function(a, x) {
          exp(-a * x) * a
        },
        triangle = function(a, b, c, d, x) {
          case_when(x < b ~ 0, x < c ~ a / (c - b), x < d ~ -(a / (d -
                                                                     c)), T ~ 0)
        },
        W1_2 = function(a, b, x) {
          exp(-exp(-b * (log(x + 0.000001) - log(a)))) * (exp(-b *
                                                                (log(x + 0.000001) - log(a))) * (b * (1 / (x + 0.000001))))
        },
        W1_3 = function(a, b, c, x) {
          b * (exp(-exp(-c * (
            log(x + 0.000001) - log(a)
          ))) * (exp(-c *
                       (
                         log(x + 0.000001) - log(a)
                       )) * (c * (1 / (
                         x + 0.000001
                       )))))
        },
        W1_4 = function(a, b, c, d, x) {
          (d - c) * (exp(-exp(-b * (
            log(x + 0.000001) - log(a)
          ))) *
            (exp(-b * (
              log(x + 0.000001) - log(a)
            )) * (b * (1 / (
              x +
                0.000001
            )))))
        },
        W2_2 = function(a, b, x) {
          exp(-exp(b * (log(x + 0.000001) - log(a)))) * (exp(b * (log(x +
                                                                        0.000001) - log(a))) * (b * (1 / (x + 0.000001))))
        },
        W2_3 = function(a, b, c, x) {
          c * (exp(-exp(b * (
            log(x + 0.000001) - log(a)
          ))) * (exp(b *
                       (
                         log(x + 0.000001) - log(a)
                       )) * (b * (1 / (
                         x + 0.000001
                       )))))
        },
        W2_4 = function(a, b, c, d, x) {
          (d - c) * (exp(-exp(b * (
            log(x + 0.000001) - log(a)
          ))) *
            (exp(b * (
              log(x + 0.000001) - log(a)
            )) * (b * (1 / (
              x +
                0.000001
            )))))
        },
        YL = function(a, b, x) {
          b / (1 + b / a * x) - b * x * (b / a) / (1 + b / a * x)^2
        }
      ),
    omethod = c(
      lineal = "L-BFGS-B",
      quadratic = "L-BFGS-B",
      exp1 = "L-BFGS-B",
      exp2 = "L-BFGS-B",
      exp3 = "BFGS",
      asymp = "L-BFGS-B",
      power = "L-BFGS-B",
      logarithmic = "L-BFGS-B",
      michment = "L-BFGS-B",
      yieldloss = "L-BFGS-B",
      sygmoidallog = "L-BFGS-B",
      gompertz = "L-BFGS-B",
      loglogistic1 = "L-BFGS-B",
      loglogistic2 = "L-BFGS-B",
      weibull1 = "L-BFGS-B",
      weibull2 = "L-BFGS-B",
      BC_3 = "L-BFGS-B",
      BC_4 = "L-BFGS-B",
      BC_5 = "L-BFGS-B",
      beta = "L-BFGS-B",
      bragg_3 = "L-BFGS-B",
      bragg_4 = "L-BFGS-B",
      cousens85 = "L-BFGS-B",
      E2 = "L-BFGS-B",
      E3 = "L-BFGS-B",
      E4 = "L-BFGS-B",
      expoDecay = "L-BFGS-B",
      expoGrowth = "L-BFGS-B",
      G2 = "L-BFGS-B",
      G3 = "L-BFGS-B",
      G4 = "L-BFGS-B",
      L2 = "L-BFGS-B",
      L3 = "L-BFGS-B",
      L4 = "L-BFGS-B",
      linear = "L-BFGS-B",
      linearOrigin = "BFGS",
      LL_Comb = "L-BFGS-B",
      LL2 = "L-BFGS-B",
      LL3 = "L-BFGS-B",
      LL4 = "L-BFGS-B",
      logBragg_3 = "L-BFGS-B",
      lorentz_3 = "L-BFGS-B",
      lorentz_4 = "L-BFGS-B",
      negExp = "L-BFGS-B",
      negExpDist = "BFGS",
      triangle = "L-BFGS-B",
      W1_2 = "L-BFGS-B",
      W1_3 = "L-BFGS-B",
      W1_4 = "L-BFGS-B",
      W2_2 = "L-BFGS-B",
      W2_3 = "L-BFGS-B",
      W2_4 = "L-BFGS-B",
      YL = "L-BFGS-B"
    ),
    par = list(
      lineal = function() {
        runif(1, 0.9, 1.1)
      },
      quadratic = function() {
        runif(3, 0.9, 1.1)
      },
      exp1 = function() {
        runif(2, 0.9, 1.1)
      },
      exp2 = function() {
        runif(2, 0.9, 1.1)
      },
      exp3 = function() {
        runif(1, 0.9, 1.1)
      },
      asymp = function() {
        runif(3, 0.9, 1.1)
      },
      power = function() {
        runif(2, 0.9, 1.1)
      },
      logarithmic = function() {
        runif(2, 0.9, 1.1)
      },
      michment = function() {
        runif(2, 0.9, 1.1)
      },
      yieldloss = function() {
        runif(2, 0.9, 1.1)
      },
      sygmoidallog = function() {
        runif(3, 0.9, 1.1)
      },
      gompertz = function() {
        runif(3, 0.9, 1.1)
      },
      loglogistic1 = function() {
        runif(3, 0.9, 1.1)
      },
      loglogistic2 = function() {
        runif(3, 0.9, 1.1)
      },
      weibull1 = function() {
        runif(3, 0.9, 1.1)
      },
      weibull2 = function() {
        runif(3, 0.9, 1.1)
      },
      BC_3 = function() {
        runif(3, 0.9, 1.1)
      },
      BC_4 = function() {
        runif(4, 0.9, 1.1)
      },
      BC_5 = function() {
        runif(5, 0.9, 1.1)
      },
      beta = function() {
        runif(5, 0.9, 1.1)
      },
      bragg_3 = function() {
        runif(3, 0.9, 1.1)
      },
      bragg_4 = function() {
        runif(4, 0.9, 1.1)
      },
      cousens85 = function() {
        runif(3, 0.9, 1.1)
      },
      E2 = function() {
        runif(2, 0.9, 1.1)
      },
      E3 = function() {
        runif(3, 0.9, 1.1)
      },
      E4 = function() {
        runif(4, 0.9, 1.1)
      },
      expoDecay = function() {
        runif(2, 0.9, 1.1)
      },
      expoGrowth = function() {
        runif(2, 0.9, 1.1)
      },
      G2 = function() {
        runif(2, 0.9, 1.1)
      },
      G3 = function() {
        runif(3, 0.9, 1.1)
      },
      G4 = function() {
        runif(4, 0.9, 1.1)
      },
      L2 = function() {
        runif(2, 0.9, 1.1)
      },
      L3 = function() {
        runif(3, 0.9, 1.1)
      },
      L4 = function() {
        runif(4, 0.9, 1.1)
      },
      linear = function() {
        runif(2, 0.9, 1.1)
      },
      linearOrigin = function() {
        runif(1, 0.9, 1.1)
      },
      LL_Comb = function() {
        runif(4, 0.9, 1.1)
      },
      LL2 = function() {
        runif(2, 0.9, 1.1)
      },
      LL3 = function() {
        runif(3, 0.9, 1.1)
      },
      LL4 = function() {
        runif(4, 0.9, 1.1)
      },
      logBragg_3 = function() {
        runif(3, 0.9, 1.1)
      },
      lorentz_3 = function() {
        runif(3, 0.9, 1.1)
      },
      lorentz_4 = function() {
        runif(4, 0.9, 1.1)
      },
      negExp = function() {
        runif(2, 0.9, 1.1)
      },
      negExpDist = function() {
        runif(1, 0.9, 1.1)
      },
      triangle = function() {
        runif(4, 0.9, 1.1)
      },
      W1_2 = function() {
        runif(2, 0.9, 1.1)
      },
      W1_3 = function() {
        runif(3, 0.9, 1.1)
      },
      W1_4 = function() {
        runif(4, 0.9, 1.1)
      },
      W2_2 = function() {
        runif(2, 0.9, 1.1)
      },
      W2_3 = function() {
        runif(3, 0.9, 1.1)
      },
      W2_4 = function() {
        runif(4, 0.9, 1.1)
      },
      YL = function() {
        runif(2, 0.9, 1.1)
      }
    ),
    fn_obj =
      list(
        lineal = function(par, data) {
          fn_obj <- function(a, b, x) {
            a + b * x
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        quadratic = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            a + b * x + c * (x^2)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        exp1 = function(par, data) {
          fn_obj <- function(a, b, x) {
            a * exp(b * x)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        exp2 = function(par, data) {
          fn_obj <- function(a, b, x) {
            exp(a + b * x)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        exp3 = function(par, data) {
          fn_obj <- function(a, x) {
            a * exp(-x / 2.718)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], x = x
          ))^2))
        },
        asymp = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            a - (a - b) * exp(-c * x)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        power = function(par, data) {
          fn_obj <- function(a, b, x) {
            a * x^b
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        logarithmic = function(par, data) {
          fn_obj <- function(a, b, x) {
            a + b * log(x)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        michment = function(par, data) {
          fn_obj <- function(a, b, x) {
            (a * x) / (b + x)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        yieldloss = function(par, data) {
          fn_obj <- function(a, b, x) {
            b * x / (1 + b / a * x)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        sygmoidallog = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c + (a - c) / (1 + exp((x - 2.718) * b))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        gompertz = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c + exp(-exp((x - 2.718) * b)) * (a - c)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        loglogistic1 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c + (a - c) / (exp((log(x) - log(exp(
              1
            ))) * b) + 1)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        loglogistic2 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c + (a - c) / (1 + (x / exp(1))^b)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        weibull1 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c + (a - c) * (1 - exp(-exp(b * log(x) - log(exp(
              1
            )))))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        weibull2 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c + (a - c) * exp(-exp((log(x) - log(1)) * b))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        BC_3 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            (c * x) / (1 + exp(-a * (log(x + 0.000001) - log(b))))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        BC_4 = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            (d + c * x) / (1 + exp(-b * (log(x + 0.000001) - log(a))))
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        BC_5 = function(par, data) {
          fn_obj <- function(a, b, c, d, e, x) {
            c + (d - c + a * x) / (1 + exp(-b * (log(x + 0.000001) -
                                                   log(e))))
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              e = par[5],
              x = x
            )
          )^2))
        },
        beta = function(par, data) {
          fn_obj <- function(a, b, c, d, e, x) {
            ifelse(x > a & x < e, d * (((x - a) / (c - a) * ((
              e - x
            ) / (
              e -
                c
            )))^((e - c) / (c - a)))^b, 0)
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              e = par[5],
              x = x
            )
          )^2))
        },
        bragg_3 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c * exp(-b * (x - a)^2)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        bragg_4 = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            c + (d - c) * exp(-b * (x - a)^2)
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        cousens85 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            a * (1 - (b * x) / (100 * (1 + b * x / c)))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        E2 = function(par, data) {
          fn_obj <- function(a, b, x) {
            1 - exp(-exp(b * (x - a)))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        E3 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c * (1 - exp(-exp(b * (x - a))))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        E4 = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            c + (d - c) * (1 - exp(-exp(b * (x - a))))
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        expoDecay = function(par, data) {
          fn_obj <- function(a, b, x) {
            a * exp(-b * x)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        expoGrowth = function(par, data) {
          fn_obj <- function(a, b, x) {
            a * exp(b * x)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        G2 = function(par, data) {
          fn_obj <- function(a, b, x) {
            exp(-exp(-b * (x - a)))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        G3 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c * (exp(-exp(-b * (x - a))))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        G4 = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            c + (d - c) * (exp(-exp(-b * (x - a))))
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        L2 = function(par, data) {
          fn_obj <- function(a, b, x) {
            1 / (1 + exp(-b * (x - a)))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        L3 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c / (1 + exp(-b * (x - a)))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        L4 = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            c + (d - c) / (1 + exp(-b * (x - a)))
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        linear = function(par, data) {
          fn_obj <- function(a, b, x) {
            a + b * x
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        linearOrigin = function(par, data) {
          fn_obj <- function(a, x) {
            a * x
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], x = x
          ))^2))
        },
        LL_Comb = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            1 / (1 + exp(-a * (log(x + 0.000001) - log(c)))) + 1 / (1 +
                                                                      exp(-b * (log(x + 0.000001) - log(d))))
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        LL2 = function(par, data) {
          fn_obj <- function(a, b, x) {
            1 / (1 + exp(-b * (log(x + 0.000001) - log(a))))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        LL3 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c / (1 + exp(-b * (log(x + 0.000001) - log(a))))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        LL4 = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            c + (d - c) / (1 + exp(-b * (log(x + 0.000001) - log(a))))
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        logBragg_3 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            a * exp(-b * (log(x + 0.000001) - c)^2)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        lorentz_3 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c / (1 + b * (x - a)^2)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        lorentz_4 = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            c + (d - c) / (1 + b * (x - a)^2)
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        negExp = function(par, data) {
          fn_obj <- function(a, b, x) {
            a * (1 - exp(-b * x))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        negExpDist = function(par, data) {
          fn_obj <- function(a, x) {
            1 - exp(-a * x)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], x = x
          ))^2))
        },
        triangle = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            case_when(x < b ~ 0, x < c ~ a * (x - b) / (c - b), x <
                        d ~ a * (d - x) / (d - c), T ~ 0)
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        W1_2 = function(par, data) {
          fn_obj <- function(a, b, x) {
            exp(-exp(-b * (log(x + 0.000001) - log(a))))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        W1_3 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            b * exp(-exp(-c * (log(x + 0.000001) - log(a))))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        W1_4 = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            c + (d - c) * exp(-exp(-b * (log(x + 0.000001) - log(a))))
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        W2_2 = function(par, data) {
          fn_obj <- function(a, b, x) {
            1 - exp(-exp(b * (log(x + 0.000001) - log(a))))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        },
        W2_3 = function(par, data) {
          fn_obj <- function(a, b, c, x) {
            c * (1 - exp(-exp(b * (
              log(x + 0.000001) - log(a)
            ))))
          }
          with(data = data, sum((y - fn_obj(
            a = par[1],
            b = par[2],
            c = par[3],
            x = x
          ))^2))
        },
        W2_4 = function(par, data) {
          fn_obj <- function(a, b, c, d, x) {
            c + (d - c) * (1 - exp(-exp(b * (
              log(x + 0.000001) -
                log(a)
            ))))
          }
          with(data = data, sum((
            y - fn_obj(
              a = par[1],
              b = par[2],
              c = par[3],
              d = par[4],
              x = x
            )
          )^2))
        },
        YL = function(par, data) {
          fn_obj <- function(a, b, x) {
            b * x / (1 + b / a * x)
          }
          with(data = data, sum((y - fn_obj(
            a = par[1], b = par[2],
            x = x
          ))^2))
        }
      )
  ),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(
    NA,
    -53L
  )
)

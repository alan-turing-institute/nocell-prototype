functions {
  real total(real[] discounted_cashflows, real discounted_terminal_value)
  {
    real result = 0;
    for (n in 1:4) result += discounted_cashflows[n];
    return result + discounted_terminal_value;
  }
  
  real terminal_value(real[] cashflows, real terminal_growth,
                      real discount_rate)
  {
    return (cashflows[4] * (1 + terminal_growth))
      / (discount_rate - terminal_growth);
  }

  real discount(real year, real value, real discount_rate, real[] years)
  {
    return value / pow(1 + discount_rate, year - years[1]);
  }

  real[] discounted_cashflows(real[] years, real[] cashflows,
                              real discount_rate)
  {
    real result[4];
    for (n in 1:4)
      result[n] = discount(years[n], cashflows[n], discount_rate, years);
    return result;
  }
  
  real dcf_value(real[] years, real[] cashflows, real terminal_growth,
                 real discount_rate)
  {
    return total(discounted_cashflows(years, cashflows, discount_rate), discount(years[4], terminal_value(cashflows, terminal_growth, discount_rate), discount_rate, years));
  }
}

parameters {
  real<lower=0.01,upper=0.1> terminal_growth;
  real<lower=0.01,upper=0.2> discount_rate;
}

model {
  terminal_growth ~ uniform(0.0, 0.1);
  discount_rate ~ uniform(0.0, 0.2);
}

generated quantities {
  real years[4] = {2010.0, 2011.0, 2012.0, 2013.0};
  real cashflows[4] = {-100.0, -50.0, 150.0, 500.0};

  real value;
  value = dcf_value(years, cashflows, terminal_growth, discount_rate);
}

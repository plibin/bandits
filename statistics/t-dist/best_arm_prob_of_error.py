#!/usr/local/bin/python

import sys
import csv

import numpy
from scipy.stats import t
from scipy.integrate import quadrature

csv_fn=sys.argv[1]
best_arm=int(sys.argv[2])

#row represents 1 arm, cols represent different params (freedom, mu, sigma)
with open(csv_fn, 'rb') as posteriors_f:
    posteriors_r = csv.DictReader(posteriors_f)
    def t_pdf(freedom, mu, sigma):
        return lambda x: t.pdf(x, freedom, mu, sigma) 
    def t_cdf(freedom, mu, sigma):
        return lambda x: t.cdf(x, freedom, mu, sigma)
    components = []
    integral_from = -1
    integral_to = -1
    for i, row in enumerate(posteriors_r):
        f = int(row['freedom'])
        m = float(row['mu'])
        s = float(row['sigma'])
        if i == best_arm:
            components.append(t_pdf(f,m,s))
            integral_from = m - 10*s
            integral_to = m + 10*s
        else:
            components.append(t_cdf(f,m,s))
    def integrand(x):
        t = map(lambda f: f(x), components)
        return reduce(lambda x, y: x*y, t)

    ans, err = quadrature(integrand, integral_from, integral_to)
    print ans

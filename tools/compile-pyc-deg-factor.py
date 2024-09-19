import numpy as np

lits = []

def add_lit(source, factor, unit, doi):
    lits.append({'source': source, 'factor': factor, 'unit': unit, 'doi': doi})


add_lit('Kuzyakov', 0.06/8.5, 'frac/year', 'https://doi.org/10.1016/j.soilbio.2013.12.021')
add_lit('Singh', np.mean([226, 204, 187, 89, 881, 1120, 397]), 'half life', 'https://doi.org/10.1021/es302545b')
add_lit('Santos', np.mean([605, 389]), 'mrt', 'https://doi.org/10.1016/j.soilbio.2012.04.005')
add_lit('Zimmermann', 0.015, 'frac/year', 'https://doi.org/10.1111/j.1365-2486.2012.02796.x')


def mrt2hl(mrt):
    #from https://web.viu.ca/krogh/chem302/residence%20time.pdf
    return np.log(2) * mrt

def hl2fy(hl):
    #from https://en.wikipedia.org/wiki/Exponential_decay?useskin=vector
    return np.log(2) / hl

def mrt2fy(mrt):
    return 1/mrt

def convert2fy(factor, unit):
    if unit == 'mrt':
        return mrt2fy(factor)
    elif unit == 'half life':
        return hl2fy(factor)
    elif unit == 'frac/year':
        return factor
    
factor_sum = 0
for lit in lits:
    factor_sum += convert2fy(lit['factor'], lit['unit'])

rate = factor_sum / len(lits)
print(rate, 'fraction/year')
print(rate / 365, 'fraction/day')
print(np.log(2) / rate, 'half life in years')
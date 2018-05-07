import csv
import sys

f = open("F:\\Courses\\R301\\Instacart Market Basket Analysis\\order_products__prior.csv", 'rt')
try:
    rows = csv.reader(f)
    print(len(rows))
finally:
    f.close()

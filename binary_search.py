from math import floor
def binary_search(data, value):
  left = 0
  right = len(data) - 1
  size = right - left
  while size  >  0:
    midpoint = left + floor(size /2)
    print(left, right, midpoint)
    size = right - left
    if data[midpoint] == value:
      return midpoint
      
    if data[midpoint] > value:
      right = midpoint
    else:
      left = midpoint

print(binary_search([2, 7, 15, 16, 26, 107], 107))
print(binary_search([1, 2, 3], 1))
print(binary_search([1, 2, 3], 2))
print(binary_search([1, 2, 3], 3))
print


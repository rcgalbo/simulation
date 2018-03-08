# example 1
# throwing marbles at wall
import numpy as np
import matplotlib.pyplot as plt
wall_width = 20

wall_height = 10

window_lower_left = (5, 2.5)
window_upper_right = (15, 7.5)

number_of_marbles = 1000

sample_widths = np.random.random(number_of_marbles) * wall_width
sample_height = np.random.random(number_of_marbles) * wall_height

random_marbles_thrown = zip(sample_widths, sample_height)

marbles_that_hit_the_wall = filter(lambda x: (x[0] < window_lower_left[0] or x[0] > window_upper_right[0]) or 
                                           (x[1] < window_lower_left[1] or x[1] > window_upper_right[1]), 
                                            random_marbles_thrown)
# plot window
x = [i for i in map(lambda x: x[0], marbles_that_hit_the_wall)]
y = [i for i in map(lambda x: x[1], marbles_that_hit_the_wall)]
plt.scatter(x=x, y=y)

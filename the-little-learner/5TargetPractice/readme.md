

```python
import matplotlib.pyplot as plt

# Defining the data
quad_xs = [-1.0, 0.0, 1.0, 2.0, 3.0]
quad_ys = [2.55, 2.1, 4.35, 10.2, 18.25]

# Creating the graph
plt.figure(figsize=(8, 6))
plt.plot(quad_xs, quad_ys, marker='o', linestyle='-', color='b')
plt.title('Graph of quad-xs vs. quad-ys')
plt.xlabel('quad-xs')
plt.ylabel('quad-ys')
plt.grid(True)
plt.show()
```
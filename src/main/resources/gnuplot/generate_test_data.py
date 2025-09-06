import numpy as np

# Define the output file
output_file = "matrix_data.txt"

# Define the grid size
x_min, x_max, x_step = -10, 10, 0.5
y_min, y_max, y_step = -10, 10, 0.5

# Create the grid
x = np.arange(x_min, x_max + x_step, x_step)
y = np.arange(y_min, y_max + y_step, y_step)
X, Y = np.meshgrid(x, y)

# Define the mathematical function for Z values
Z = np.sin(np.sqrt(X**2 + Y**2))  # Example: Radial sine wave pattern

# Write the data to a file
with open(output_file, "w") as f:
    for i in range(X.shape[0]):
        for j in range(X.shape[1]):
            f.write(f"{X[i, j]} {Y[i, j]} {Z[i, j]}\n")

print(f"Test data saved to '{output_file}'.")

#Methods to resolution of Non-linear Equations System
from math import sqrt
from random import random
import csv

def dist(x1, x2):
	sum = 0
	for i in range(len(x1)):

		if(sum > 10^15 or abs(x1[i] - x2[i]) > 10^15): return 10^15

		sum += (x1[i] - x2[i])**2

	return sqrt(sum)

def norm(x):
	sum = 0
	for i in x:
		if(sum > 10^15 or abs(i) > 10^15): return 10^15
		sum += i ** 2
	return sqrt(sum)

def sum(x1, x2):
	y = len(x1) * [0]
	for i in range(len(x1)):
		y[i] = x1[i] + x2[i]

	return y

def neg(x):
	for i in range(len(x)):
		x[i] = -1 * x[i]

	return x

def nonzero_diagonal(matrix):

	for i in range(len(matrix)):
		if( matrix[i][i] == 0 ):
			for j in range(i, len(matrix)):
				if( matrix[j][i] != 0 ):
					aux = matrix[i].copy()
					matrix[i] = matrix[j].copy()
					matrix[j] = aux.copy()

				if( matrix[i][i] == 0):
					for j in range(i):
						if( matrix[j][i] != 0 and matrix[i][j] != 0):
							aux = matrix[i].copy()
							matrix[i] = matrix[j].copy()
							matrix[j] = aux.copy()

	return matrix

#Method to solve linear equation system
def gauss_seidel(matrix, vector, eps, kmax):

	matrix = nonzero_diagonal(matrix)

	x_1 = [0]*len(vector)
	x_0 = [1]*len(vector)
	k = 0

	while(dist(x_1, x_0) >= eps and k <= kmax):

		if(dist(x_1, x_0) >= 10^15):
			return x_0

		x_0 = x_1.copy()

		for i in range(len(x_1)):
			x_1[i] = vector[i]

			for j in range(i):
				x_1[i] -= matrix[i][j] * x_1[j]


			for j in range(i+1, len(x_1)):
				x_1[i] -= matrix[i][j] * x_0[j]


			x_1[i] /= matrix[i][i]

		k += 1

	return x_0

#def function(x):
#	y1 = x[0]**2 + x[1]
#	y2 = x[0] * x[1] + 1

#	return [ y1, y2 ]

#def jacobian(x):
#	j11 = 2*x[0]
#	j12 = 1
#	j21 = x[1]
#	j22 = x[0]

#	return [[j11, j12], [j21, j22]]

def newton_rapson(function, jacobian, x, eps, kmax):

	k = 0

	delta_x = len(x)*[1]

	while(norm(delta_x) >= eps and k < kmax):

		delta_x = gauss_seidel(jacobian(x), neg(function(x)), eps, kmax)
		x = sum(x, delta_x)
		k += 1
	return x

def modified_newton_rapson(function, jacobian, x, eps, kmax):

	k = 0

	delta_x = len(x)*[1]

	jacobian = jacobian(x)

	while(norm(delta_x) >= eps and k < kmax):

		delta_x = gauss_seidel(jacobian, neg(function(x)), eps, kmax)
		x = sum(x, delta_x)

		k += 1
	return x



vec_x = []
vec_y = []

with open("data_tri_mean_cn43.csv") as file:
	data = csv.reader(file)
	for row in data:
		vec_x.append(int(row[1]))
		vec_y.append(float(row[2]))


def function(coef):
	func = [0,0,0,0]
	for i in range(len(vec_x)):
		n = coef[2] - vec_y[i]
		d = coef[0] * vec_x[i] + coef[1]

		func[0] += coef[3] * n * vec_x[i] / d ** 2 - coef[3] ** 2 * vec_x[i] / d ** 3
		func[1] += coef[3] * n / d ** 2 - coef[3] ** 2 / d ** 3
		func[2] += -vec_y[i] - coef[3] / d
		func[3] += -n/d + coef[3] / d ** 2

	func[2] += + coef[2] * len(vec_x)

	return func



def jacobian(coef):
	matrix = [[0,0,0,0],[0,0,0,0],[0,0,0,0], [0,0,0,0]]

	for i in range(len(vec_x)):
		n = coef[2] - vec_y[i]
		d = coef[0] * vec_x[i] + coef[1]

		matrix[0][0] += -2*coef[3]*vec_x[i] ** 2 * n / d ** 3 + 3*coef[3]**2 *vec_x[i]**2 / d**4
		matrix[0][1] += -2*coef[3]*vec_x[i] * n / d ** 3 + 3*coef[3]**2 * vec_x[i] / d**4
		matrix[0][2] += coef[3] * vec_x[i] / d ** 2
		matrix[0][3] += vec_x[i] * n / d ** 2 - 2 * coef[3] * vec_x[i] / d ** 3

		matrix[1][1] += -2*coef[3] * n / d ** 3 + 3*coef[3]**2 / d**4
		matrix[1][2] += coef[3] / d ** 2
		matrix[1][3] += n / d ** 2 - 2 * coef[3] / d ** 3

		matrix[2][3] += -1/d

		matrix[3][3] += 1 / d**2

	matrix[1][0] = matrix[0][1]

	matrix[2][0] = matrix[0][2]
	matrix[2][1] = matrix[2][1]
	matrix[2][2] = len(vec_x)

	matrix[3][0] = matrix[0][3]
	matrix[3][1] = matrix[1][3]
	matrix[3][2] = matrix[2][3]

	return matrix

#Usage

#matrix = [[3, 1], [1, 2]]
#vector = [1, 0]

#print(gauss_seidel(matrix, vector, 0.0001, 1000))

#print(nonzero_diagonal([[0, 1, 2], [1, 0, 2], [0, 0, 1]]))

#print(newton_rapson(function, jacobian, [1, 0], 0.00001, 10000))

#print(modified_newton_rapson(function, jacobian, [1, 0], 0.00001, 10000))


def fun(coef, x):
	return (coef[2] - coef[3] / (coef[0] * x + coef[1]))

def mean_squared_error(coef, fun):
	sum = 0
	for i in range(len(vec_x)):
		sum += (fun(coef, vec_x[i]) - vec_y[i]) ** 2

	return (sum / len(vec_x))

# init = [1,2,3,4]
# solution = modified_newton_rapson(function, jacobian, init, 10**(-5), 1000000)
# print("Solution:")
# print(solution)
# print("Image:")
# print(function(solution))
# print("Error")
# print(quadratic_medium_error(solution, fun))

solution = []
mse = 10000000
for i in range(1000000):
	init = [10 * random(), 10 * random(), 10 * random(), 10 * random()]
	#print(init)
	result = newton_rapson(function, jacobian, init, 10**(-15), 1000)
	new_mse = mean_squared_error(result, fun)

	if(new_mse < mse):
		solution = result
		mse = new_mse
		print(solution)
		print(mse)

# result = [2.415, 67.565, 0.277, 4.741]
# print(mean_squared_error(result, fun))

print("Solution:")
print(solution)
print("Quadratic Medium Error:")
print(mse)

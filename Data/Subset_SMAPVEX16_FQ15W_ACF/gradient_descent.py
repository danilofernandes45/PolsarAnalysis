import random
import csv

value_x = []
value_y = []

with open("data_tri_mean.csv") as file:
	data = csv.reader(file)
	for row in data:
		value_x.append(int(row[1]))
		value_y.append(float(row[2]))

def manhattan_norm(vector):
    result = 0
    for i in vector:
        result += abs(i)
        if(result > 10**20):
            break

    return result

def is_aprox_zero(ncoords, vector, eps):
    res = True
    for i in range(ncoords):
        res = res and ( abs(vector[i]) < eps )

    return res

def update(ncoords, solution, grad, alpha):
    for i in range(ncoords):
        solution[i] -= alpha * grad[i]
    return solution

def gradient(vector):
    grad = [0, 0, 0, 0]

    for i in range(len(value_x)):
        diff = -vector[3] / (vector[0] * value_x[i] + vector[1]) + vector[2] - value_y[i]
        factor1 = -1 / (vector[0] * value_x[i] + vector[1])
        factor2 = vector[3] / (vector[0] * value_x[i] + vector[1])**2

        grad[0] += 2 * diff * factor2 * value_x[i]
        grad[1] += 2 * diff * factor2
        grad[2] += 2 * diff
        grad[3] += 2 * diff * factor1

    return grad

def gradient_descent(ncoords, start, alpha, eps):

    solution = start.copy()
    grad = gradient(start)
    count = 0
    imax = 100

    while(count < imax and not is_aprox_zero(ncoords, grad, eps)):
        solution = update(ncoords, solution, grad, alpha)
        try:
            grad = gradient(solution)
        except OverflowError:
            print("Overflow 1")
            break
        count += 1
    # print(count)
    # print(grad)
    return solution

def gradient_random_restart(ncoords, kmax, eps, alpha):

    vec = []
    for i in range(ncoords):
        vec.append(100)

    mnorm = 1000
    for i in range(kmax):

        start = []
        for i in range(ncoords):
            start.append( random.choice([-1, 1]) * random.random() * 10 )

        new_vec = gradient_descent(ncoords, start, alpha, eps)
        new_mnorm = mean_squared_error(new_vec, fun)

        if(new_mnorm < mnorm):
            mnorm = new_mnorm
            vec = new_vec
            print(vec)
            print(mnorm)

    return vec

def fun(coef, x):
	return (coef[2] - coef[3] / (coef[0] * x + coef[1]))

def mean_squared_error(coef, fun):
    sum = 0
    #print(coef)
    for i in range(len(value_x)):
        try:
            sum += (fun(coef, value_x[i]) - value_y[i]) ** 2
        except OverflowError:
            print("Overflow Error 2")
            return 100000

    return (sum / len(value_x))

print("Solution:")
solution = gradient_random_restart(4, 10000, 10**(-10), 0.05)
print(solution)
print("Squared Mean Error:")
print(mean_squared_error(solution, fun))

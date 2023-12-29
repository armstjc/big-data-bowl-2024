from matplotlib import rc
rc('animation', html='jshtml')

import math
from enum import Enum
#from numpy.lib.type_check import nan_to_num
from scipy.special import expit
from tqdm import tqdm
#import matplotlib.pyplot as plt
#from matplotlib import animation
#from matplotlib import ax
import numpy as np
import pandas as pd

import os
#os.chdir('C:/Users/Owner/Documents/BDB24')
#os.chdir('nfl-big-data-bowl-2024')
# reading the CSV file
ex1 = pd.read_csv('ex_play_tot.csv')
def_df = ex1.query('`side` == "DEF"')
off_df = ex1.query('`side` == "OFF"')

# ia = np.load('play_ex.npz')

def pos(x, y):
    return np.array([x, y])

def def_i(n, i, P):
    def_df1 = def_df[(def_df.frame == i)]
    # theta0 = abs(def_df1.iloc[0]["dir_std"] - 180)
    # theta = np.where(theta0<90, theta0+270, theta0-90)
    theta = def_df1.iloc[n]["dir_std"]
    speed = def_df1.iloc[n]["s"]
    dist = def_df1.iloc[n]["dist_to_carrier"]
    x = def_df1.iloc[n]["x_std"]
    y = def_df1.iloc[n]["y_std"]
    current_pos = pos(x, y)
    # Covariance Matrix
    COV = COV_i(theta, speed, dist)
    determinant = np.linalg.det(COV)
    inverse_COV = np.linalg.inv(COV)
    # velocity vector and mu array
    vel = speed * np.array([np.cos(np.deg2rad(theta)), np.sin(np.deg2rad(theta))])
    mu = np.add(np.array([x, y]), 0.5 * vel)
    #Transpose is not working for either diff.T or np.transpose
    diff = current_pos - mu
    # Exponent Term
    exp_term = -0.5 * np.einsum("ij,ij->i", np.dot(P - mu, inverse_COV), P - mu)
    exp2_term = -0.5 * np.dot(diff.T, np.dot(inverse_COV, diff))
    f_i = np.array((1 / ((2 * np.pi) ** 2 * np.sqrt(determinant))) * np.exp(exp_term))
    f_pi = np.array((1 / ((2 * np.pi) ** 2 * np.sqrt(determinant))) * np.exp(exp2_term))

    return np.divide(f_i, f_pi)

# Define the function f_i(p; t)
def off_i(n, i, P):
    off_df1 = off_df[(off_df.frame == i)]
    # theta0 = abs(off_df1.iloc[0]["dir_std"] - 180)
    # theta = np.where(theta0<90, theta0+270, theta0-90)
    theta = off_df1.iloc[n]["dir_std"]
    speed = off_df1.iloc[n]["s"]
    dist = off_df1.iloc[n]["dist_to_carrier"]
    x = off_df1.iloc[n]["x_std"]
    y = off_df1.iloc[n]["y_std"]
    current_pos = pos(x, y)
    # Covariance Matrix
    COV = COV_i(theta, speed, dist)
    determinant = np.linalg.det(COV)
    inverse_COV = np.linalg.inv(COV)
    # velocity vector and mu array
    vel = speed * np.array([np.cos(np.deg2rad(theta)), np.sin(np.deg2rad(theta))])
    mu = np.add(np.array([x, y]), 0.5 * vel)
    #Transpose is not working for either diff.T or np.transpose
    diff = current_pos - mu
    # Exponent Term
    exp_term = -0.5 * np.einsum("ij,ij->i", np.dot(P - mu, inverse_COV), P - mu)
    exp2_term = -0.5 * np.dot(diff.T, np.dot(inverse_COV, diff))
    f_i = np.array((1 / ((2 * np.pi) ** 2 * np.sqrt(determinant))) * np.exp(exp_term))
    f_pi = np.array((1 / ((2 * np.pi) ** 2 * np.sqrt(determinant))) * np.exp(exp2_term))

    return np.divide(f_i, f_pi)
# Define the rotation matrix R
def rotation_matrix(theta):
    return np.array([
        [np.cos(np.deg2rad(theta)), -np.sin(np.deg2rad(theta))],
        [np.sin(np.deg2rad(theta)), np.cos(np.deg2rad(theta))]
    ])

# Define the scaling matrix S
def scaling_matrix(s_x, s_y):
    return np.array([
        [s_x, 0],
        [0, s_y]
    ])

# Define the function S_rat_i based on formula 13
def S_rat_i(speed):
    return speed**2/11**2

def distance(dist):
    if dist > 19:
      d_t = 10
    else:
      #[ 4.023024091680029, 0.02485712,  -0.00197807, 0.0009415 ]
      d_t = 4.023024091680029 + 0.02485712*dist - 0.00197807*(dist)**2 + 0.0009415*(dist)**3
    return d_t

# Update the definition for S_i(t) based on formula 14
def S_i(speed, dist):
    dist_i_val = distance(dist)
    S_rat_i_val = S_rat_i(speed)

    s_x = (dist_i_val + dist_i_val*S_rat_i_val)/2
    s_y = (dist_i_val - dist_i_val*S_rat_i_val)/2
    s_xy = np.array([ [s_x, 0], [0, s_y] ])

    return s_xy

# Update the definition for COV_i(t) based on formula 15
def COV_i(theta, speed, dist):
    R_theta = rotation_matrix(theta)
    Si_val = S_i(speed, dist)
    R_theta_i_inv = np.linalg.inv(R_theta)
    cov = np.dot(R_theta, np.dot(Si_val, np.dot(Si_val, R_theta_i_inv)))

    return cov

def influence_areas(i):
    field_dimen = (120, 54)

    # Define the grid
    x = np.linspace(0, field_dimen[0], 120)
    y = np.linspace(0, field_dimen[1], 54)
    X, Y = np.meshgrid(x, y)

    P = np.stack([X.ravel(), Y.ravel()], axis=-1)
    Z1_total = np.zeros(P.shape[0])
    for n in range(11):
        Z1_total += def_i(n, i, P)

    Z2_total = np.zeros(P.shape[0])
    for n in range(10):
        Z2_total += off_i(n, i, P)


    WFinal = expit(np.subtract(Z1_total, Z2_total)).reshape(X.shape)
    return WFinal

#Locating Influence Areas
def find_indices_of_non_zero(input_array: np.array) -> list:
    non_matching_value_coordinates = []
    for i, col in enumerate(input_array.T):
        if 0 in col:
            locs = np.where(col != 0)[0]
            for row_index in locs:
                non_matching_value_coordinates.append([i, row_index])

    return non_matching_value_coordinates

def current_mat(mat0):
    mat = np.where((mat0>=0.7) & (mat0<=0.79), 1,  #when... then
            np.where((mat0>=0.8) & (mat0<=0.89), 2, #when... then
              np.where((mat0>=0.9) & (mat0<=1), 3, #when... then
                        0)))

    return mat

def current_zone(mat):
    zones = find_indices_of_non_zero(mat)

    return np.array(zones)


def dwa_control(x, config, goal, ob, ia):
    """
    Dynamic Window Approach control
    """
    dw = calc_dynamic_window(x, config)

    u, trajectory = calc_control_and_trajectory(x, dw, config, goal, ob, ia)

    return u, trajectory

class RobotType(Enum):
    circle = 0
    rectangle = 1

class Config:
    """
    simulation parameter class
    """

    def __init__(self):
        #1.0
        self.max_speed = 11.0  # [m/s]
        #-0.5
        self.min_speed = 0.0  # [m/s]
        #45.0 * math.pi / 180.0  # [rad/s]
        self.max_yaw_rate = 0.25 * math.pi  # [rad/s]
        #0.2
        self.max_accel = 11.0  # [m/ss]
        self.max_delta_yaw_rate = 0.25 * math.pi  # [rad/ss]
        self.v_resolution = 0.01  # [m/s]
        self.yaw_rate_resolution = 0.1 * math.pi / 180.0  # [rad/s]
        self.dt = 0.1  # [s] Time tick for motion prediction
        #3.0
        self.predict_time = 0.5 # [s]
        self.to_goal_cost_gain = 0.15
        self.speed_cost_gain = 1.0
        #1.0
        self.obstacle_cost_gain = 1.0
        self.zone_cost_gain = 1.0
        self.robot_stuck_flag_cons = 0.001  # constant to prevent robot stucked
        self.robot_type = RobotType.circle
        self.robot_radius = 1.0  # [m] for collision check

    @property
    def robot_type(self):
        return self._robot_type

    @robot_type.setter
    def robot_type(self, value):
        if not isinstance(value, RobotType):
            raise TypeError("robot_type must be an instance of RobotType")
        self._robot_type = value

config = Config()

def motion(x, u, dt):
    """
    motion model
    """

    x[2] += u[1] * dt
    x[0] += u[0] * math.cos(x[2]) * dt
    x[1] += u[0] * math.sin(x[2]) * dt
    x[3] = u[0]
    x[4] = u[1]

    return x

def calc_dynamic_window(x, config):
    """
    calculation dynamic window based on current state x
    """

    # Dynamic window from robot specification
    Vs = [config.min_speed, config.max_speed,
          -config.max_yaw_rate, config.max_yaw_rate]

    # Dynamic window from motion model
    Vd = [x[3] - config.max_accel * config.dt,
          x[3] + config.max_accel * config.dt,
          x[4] - config.max_delta_yaw_rate * config.dt,
          x[4] + config.max_delta_yaw_rate * config.dt]

    #  [v_min, v_max, yaw_rate_min, yaw_rate_max]
    dw = [max(Vs[0], Vd[0]), min(Vs[1], Vd[1]),
          max(Vs[2], Vd[2]), min(Vs[3], Vd[3])]

    return dw

def predict_trajectory(x_init, V, Y, config):
    """
    Predict trajectories for a vector of velocities (V) and yaw rates (Y).
    """
    num_combinations = len(V)
    trajectories = [None] * num_combinations

    for i in range(num_combinations):
        x = np.array(x_init)
        trajectory = np.array(x)
        time = 0
        while time <= config.predict_time:
            x = motion(x, [V[i], Y[i]], config.dt)
            trajectory = np.vstack((trajectory, x))
            time += config.dt
        trajectories[i] = trajectory

    return np.array(trajectories)


def calc_control_and_trajectory(x, dw, config, goal, ob, ia):
    """
    Calculation final input with dynamic window, vectorized version.
    """
    # Create a mesh of all (v, y) combinations
    v_values = np.arange(dw[0], dw[1], config.v_resolution)
    y_values = np.arange(dw[2], dw[3], config.yaw_rate_resolution)
    V, Y = np.meshgrid(v_values, y_values)
    V = V.flatten()
    Y = Y.flatten()

    # Vectorize trajectory prediction
    trajectories = predict_trajectory(x, V, Y, config)

    # Vectorize cost calculations
    to_goal_costs = config.to_goal_cost_gain * calc_to_goal_cost(trajectories, goal)
    speed_costs = config.speed_cost_gain * (config.max_speed - trajectories[:, -1, 3])
    ob_costs = config.obstacle_cost_gain * calc_obstacle_cost(trajectories, config, ob)
    ia_costs = config.zone_cost_gain * calc_influence_area_cost(trajectories, config, ia)

    final_costs = to_goal_costs + speed_costs + ob_costs + ia_costs

    # Find the minimum cost and corresponding best control (v, y)
    min_cost_index = np.argmin(final_costs)
    best_u = [V[min_cost_index], Y[min_cost_index]]
    best_trajectory = trajectories[min_cost_index]

    return best_u, best_trajectory

def calc_influence_area_cost(trajectories, config, ia):
    """
    Calculate obstacle cost with additional cost for intersections for multiple trajectories.
    """
    mat0 = ia
    mat = current_mat(mat0)
    zone = current_zone(mat)

    all_costs = []

    for trajectory in trajectories:
        zx, zy = zone[:, 0], zone[:, 1]
        tx, ty = trajectory[:, 0], trajectory[:, 1]

        # Vectorized distance calculation
        dx = tx - zx[:, None]
        dy = ty - zy[:, None]
        r = np.hypot(dx, dy)

        # Check for intersections and calculate additional costs
        intersection_mask = r <= config.robot_radius
        intersecting_zones = np.any(intersection_mask, axis=1)

        total_cost = 0
        for i in np.where(intersecting_zones)[0]:
            additional_cost = mat[zone[int(zx[i]), 1], zone[int(zy[i]), 0]]
            total_cost += additional_cost

        # Include some base cost calculation if required
        min_r = np.min(r)
        base_cost = 1.0 / min_r if min_r != 0 else 0

        all_costs.append(total_cost + base_cost)

    return np.array(all_costs)

def calc_obstacle_cost(trajectories, config, ob):
    """
    Calculate obstacle cost for a batch of trajectories.
    """

    costs = []
    for trajectory in trajectories:
        ox = ob[:, 0]
        oy = ob[:, 1]
        dx = trajectory[:, 0] - ox[:, None]
        dy = trajectory[:, 1] - oy[:, None]
        r = np.hypot(dx, dy)

        if np.array(r <= config.robot_radius).any():
            costs.append(float("Inf"))
        else:
            min_r = np.min(r)
            costs.append(1.0 / min_r)

    return np.array(costs)

def calc_to_goal_cost(trajectories, goal):
    """
    Calculate the cost to the goal for each trajectory.
    """
    costs = []
    for trajectory in trajectories:
        dx = goal[0] - trajectory[-1, 0]
        dy = goal[1] - trajectory[-1, 1]
        error_angle = math.atan2(dy, dx)
        cost_angle = error_angle - trajectory[-1, 2]
        cost = abs(math.atan2(math.sin(cost_angle), math.cos(cost_angle)))
        costs.append(cost)

    return np.array(costs)


# def plot_arrow(x, y, yaw, length=0.5, width=0.1):  # pragma: no cover
#     plt.arrow(x, y, length * math.cos(yaw), length * math.sin(yaw),
#               head_length=width, head_width=width)
#     plt.plot(x, y)


# def plot_robot(x, y, yaw, config):  # pragma: no cover

#     if config.robot_type == RobotType.circle:
#         circle = plt.Circle((x, y), config.robot_radius, color="b")
#         plt.gcf().gca().add_artist(circle)
#         out_x, out_y = (np.array([x, y]) +
#                         np.array([np.cos(yaw), np.sin(yaw)]) * config.robot_radius)
#         plt.plot([x, out_x], [y, out_y], "-k")


def dwa(i):
    def_df1 = def_df[(def_df.frame == i)]
    off_df1 = off_df[(off_df.frame == i)]
    # initial state [x(yds), y(yds), yaw(rad), v(yds/s), omega(rad/s)]
    #orientation used could apply carrier direction here; car_dir
    car_dir = abs(off_df1.iloc[0]["car_dir"] - 180)
    # car_o2 = np.where(car_o<90, car_o+270, car_o-90)
    # final_o = math.radians(car_o2)
    car_s = off_df1.iloc[0]["car_s"]
    x = np.array([off_df1.iloc[0]["car_x"], off_df1.iloc[0]["car_y"], car_dir, car_s, 0.125*math.pi])
    # goal position [x(yds), y(yds)]
    goal = np.array([110, off_df1.iloc[0]["car_y"]])

    xfield = 120
    yfield = 54

    # Create boundary obstacles using list comprehensions
    boundary_obs = [[k, 0] for k in range(xfield)] + \
                   [[k, yfield] for k in range(xfield)] + \
                   [[0, j] for j in range(yfield)] + \
                   [[xfield, j] for j in range(yfield)]

    # Create obstacles for offensive and defensive players
    off_obs = off_df1[['x_std', 'y_std']].values.tolist()
    def_obs = def_df1[['x_std', 'y_std']].values.tolist()

    # Combine all obstacles and convert to numpy array
    ob = np.array(boundary_obs + off_obs + def_obs)

    ia = influence_areas(i)

    # input [forward speed, yaw_rate]
    # Define the grid
    x0 = np.linspace(0, 120, 120)
    y0 = np.linspace(0, 54, 54)
    X, Y = np.meshgrid(x0, y0)

    config.robot_type = RobotType.circle
    trajectory = np.array(x)
    #mat0 = ia

    u, predicted_trajectory = dwa_control(x, config, goal, ob, ia)
    x1 = motion(x, u, config.dt)  # simulate robot
    trajectory = np.vstack((trajectory, x1))  # store state history

    projection = [off_df1.iloc[0]["gameId"], off_df1.iloc[0]["playId"], i, off_df1.iloc[0]["car_x"], off_df1.iloc[0]["car_y"], trajectory[1, 0], trajectory[1, 1]]
    print(projection)

    # #ax.clear()
    # plt.cla()
    # #predicted_trajectory or actual trajectory
    # plt.plot(predicted_trajectory[:, 0], predicted_trajectory[:, 1], "-g")
    # plt.plot(x1[0], x1[1], "xr")
    # plt.plot(goal[0], goal[1], "xb")
    # plt.plot(ob[:, 0], ob[:, 1], "ok")
    # plt.contour(X, Y, mat0, [0.7, 0.8, 0.9], cmap='viridis')
    # plot_robot(x1[0], x1[1], x1[2], config)
    # plot_arrow(x1[0], x1[1], x1[2])
    # plt.axis("equal")
    # plt.grid(True)
    # #plt.pause(0.0001)
    # plot = plt

    return projection #plot

frame_numbers = ex1['frame'].unique()

results = []

for frame in tqdm(frame_numbers):
    result = dwa(frame)
    results.append(result)

print(results)

import argparse
import logging
import math
from scipy.special import expit
import numpy as np
import pandas as pd
import gc
from tqdm import tqdm

import os
#os.chdir('C:/Users/Owner/Documents/BDB24/data')

## Set this to the week you want to parse through
week = 1
games0 = pd.DataFrame()
# games0 = pd.read_csv('influence_prep2.csv')
# games = games0.query(f'week == {week}')

del games0
gc.collect()

"""
Influence Areas
"""

def pos(x, y):
    """
    Array for Player Position
    """
    return np.array([x, y])


def def_i(def_df, n, P):
    """
    Influence Areas for the Defense; f_i(p; t)
    """
    #Player Parameters
    theta = def_df.iloc[n]["dir_std"]
    speed = def_df.iloc[n]["s"]
    dist = def_df.iloc[n]["dist_to_carrier"]
    x = def_df.iloc[n]["x_std"]
    y = def_df.iloc[n]["y_std"]
    current_pos = pos(x, y)
    # Covariance Matrix
    COV = COV_i(theta, speed, dist)
    determinant = np.linalg.det(COV)
    inverse_COV = np.linalg.inv(COV)
    # velocity vector and mu array
    vel = speed * np.array([np.cos(np.deg2rad(theta)), np.sin(np.deg2rad(theta))])
    mu = np.add(np.array([x, y]), 0.5 * vel)
    diff = current_pos - mu
    # Exponent Term
    exp_term = -0.5 * np.einsum("ij,ij->i", np.dot(P - mu, inverse_COV), P - mu)
    exp2_term = -0.5 * np.dot(diff.T, np.dot(inverse_COV, diff))
    f_i = np.array((1 / ((2 * np.pi) ** 2 * np.sqrt(determinant))) * np.exp(exp_term))
    f_pi = np.array((1 / ((2 * np.pi) ** 2 * np.sqrt(determinant))) * np.exp(exp2_term))

    return np.divide(f_i, f_pi)


def off_i(off_df, n, P):
    """
    Influence Areas for the Defense; f_i(p; t)
    """
    #Player Parameters
    theta = off_df.iloc[n]["dir_std"]
    speed = off_df.iloc[n]["s"]
    dist = off_df.iloc[n]["dist_to_carrier"]
    x = off_df.iloc[n]["x_std"]
    y = off_df.iloc[n]["y_std"]
    current_pos = pos(x, y)
    # Covariance Matrix
    COV = COV_i(theta, speed, dist)
    determinant = np.linalg.det(COV)
    inverse_COV = np.linalg.inv(COV)
    # velocity vector and mu array
    vel = speed * np.array([np.cos(np.deg2rad(theta)), np.sin(np.deg2rad(theta))])
    mu = np.add(np.array([x, y]), 0.5 * vel)
    diff = current_pos - mu
    # Exponent Term
    exp_term = -0.5 * np.einsum("ij,ij->i", np.dot(P - mu, inverse_COV), P - mu)
    exp2_term = -0.5 * np.dot(diff.T, np.dot(inverse_COV, diff))
    f_i = np.array((1 / ((2 * np.pi) ** 2 * np.sqrt(determinant))) * np.exp(exp_term))
    f_pi = np.array((1 / ((2 * np.pi) ** 2 * np.sqrt(determinant))) * np.exp(exp2_term))

    return np.divide(f_i, f_pi)


def rotation_matrix(theta):
    """
    Rotation Matrix
    """
    return np.array([
        [np.cos(np.deg2rad(theta)), -np.sin(np.deg2rad(theta))],
        [np.sin(np.deg2rad(theta)), np.cos(np.deg2rad(theta))]
    ])


def scaling_matrix(s_x, s_y):
    """
    Scaling Matrix
    """
    return np.array([
        [s_x, 0],
        [0, s_y]
    ])


def S_rat_i(speed):
    """
    Speed in Yards per Second; based on formula 13
    """
    return speed**2/11**2


def distance(dist):
    """
    Radius of Player based on Distance
    """
    if dist > 19:
      d_t = 10
    else:
      d_t = 4.023024091680029 + 0.02485712*dist - 0.00197807*(dist)**2 + 0.0009415*(dist)**3
    return d_t


def S_i(speed, dist):
    """
    Update the definition of S_i(t); based on formula 14
    """
    dist_i_val = distance(dist)
    S_rat_i_val = S_rat_i(speed)

    s_x = (dist_i_val + dist_i_val*S_rat_i_val)/2
    s_y = (dist_i_val - dist_i_val*S_rat_i_val)/2
    s_xy = np.array([ [s_x, 0], [0, s_y] ])

    return s_xy


def COV_i(theta, speed, dist):
    """
    Covariance Matrix; based on formula 15
    """
    R_theta = rotation_matrix(theta)
    Si_val = S_i(speed, dist)
    R_theta_i_inv = np.linalg.inv(R_theta)
    cov = np.dot(R_theta, np.dot(Si_val, np.dot(Si_val, R_theta_i_inv)))

    return cov


def influence_areas(def_df, off_df):
    """
    Influence Areas for the entire Field of Play
    """
    field_dimen = (120, 54)

    # Define the grid
    x = np.linspace(0, field_dimen[0], 120)
    y = np.linspace(0, field_dimen[1], 54)
    X, Y = np.meshgrid(x, y)

    P = np.stack([X.ravel(), Y.ravel()], axis=-1)
    Z1_total = np.zeros(P.shape[0])
    for n in range(11):
        Z1_total += def_i(def_df, n, P)

    Z2_total = np.zeros(P.shape[0])
    for n in range(10):
        Z2_total += off_i(off_df, n, P)


    WFinal = expit(np.subtract(Z1_total, Z2_total)).reshape(X.shape)
    return WFinal

"""
Format Influence Areas for DWA Algorithm
"""

def find_indices_of_non_zero(input_array: np.array) -> list:
    """
    Identify Coordinates from Influence Areas
    """
    non_matching_value_coordinates = []
    for i, col in enumerate(input_array.T):
        if 0 in col:
            locs = np.where(col != 0)[0]
            for row_index in locs:
                non_matching_value_coordinates.append([i, row_index])

    return non_matching_value_coordinates

def current_mat(mat0):
    """
    Travel Costs for Different Influence Area Values
    """
    mat = np.where((mat0>=0.7) & (mat0<=0.79), 1,  #when... then
            np.where((mat0>=0.8) & (mat0<=0.89), 2, #when... then
              np.where((mat0>=0.9) & (mat0<=1), 3, #when... then
                        0)))

    return mat

def current_zone(mat):
    zones = find_indices_of_non_zero(mat)

    return np.array(zones)

"""
Dynamic Window Approach
"""

def dwa_control(x, z, goal, ob, ia):
    """
    Dynamic Window Approach control
    """
    dw = calc_dynamic_window(x, z)

    u, trajectory = calc_control_and_trajectory(x, dw, z, goal, ob, ia)

    return u, trajectory

def motion(x, u):
    """
    Motion Model
    """

    x[2] += u[1] * 0.1
    x[0] += u[0] * math.cos(x[2]) * 0.1
    x[1] += u[0] * math.sin(x[2]) * 0.1
    x[3] = u[0]
    x[4] = u[1]

    return x

def calc_dynamic_window(x, z):
    """
    Calculate Dynamic Window based on current state x
    """

    # Dynamic window from robot specification
    Vs = [0, z[0], #Min and Max Speed
          -1*z[2], z[2]] #max_yaw_rate; 0.25*math.pi

    # Dynamic window from motion model
    Vd = [x[3] - z[1] * 0.1, #Max Acceleration
          x[3] + z[1] * 0.1, #Max Acceleration
          x[4] - z[2] * 0.1, #max_delta_yaw_rate; (0.25 * math.pi)
          x[4] + z[2] * 0.1] #max_delta_yaw_rate; (0.25 * math.pi)

    #  [v_min, v_max, yaw_rate_min, yaw_rate_max]
    dw = [max(Vs[0], Vd[0]), min(Vs[1], Vd[1]),
          max(Vs[2], Vd[2]), min(Vs[3], Vd[3])]

    return dw

def predict_trajectory(x_init, V, Y):
    """
    Predict trajectories for a vector of velocities (V) and yaw rates (Y).
    """
    num_combinations = len(V)
    trajectories = [None] * num_combinations

    for i in range(num_combinations):
        x = np.array(x_init)
        trajectory = np.array(x)
        time = 0
        while time <= 0.5:
            x = motion(x, [V[i], Y[i]])
            trajectory = np.vstack((trajectory, x))
            time += 0.1
        trajectories[i] = trajectory

    return np.array(trajectories)


def calc_control_and_trajectory(x, dw, z, goal, ob, ia):
    """
    Calculate final input with dynamic window [vectorized version]
    """
    # Create a mesh of all (v, y) combinations
    v_values = np.arange(dw[0], dw[1], 0.01) #v_resolution
    y_values = np.arange(dw[2], dw[3], 0.1 * math.pi / 180.0) #yaw_rate_resolution
    V, Y = np.meshgrid(v_values, y_values)
    V = V.flatten()
    Y = Y.flatten()

    # Vectorize trajectory prediction
    trajectories = predict_trajectory(x, V, Y)

    # Vectorize cost calculations
    to_goal_costs = 0.15 * calc_to_goal_cost(trajectories, goal)
    speed_costs = 1.0 * (z[0] - trajectories[:, -1, 3])
    ob_costs = 1.0 * calc_obstacle_cost(trajectories, ob)
    ia_costs = 1.0 * calc_influence_area_cost(trajectories, ia)

    final_costs = to_goal_costs + speed_costs + ob_costs + ia_costs

    # Find the minimum cost and corresponding best control (v, y)
    min_cost_index = np.argmin(final_costs)
    best_u = [V[min_cost_index], Y[min_cost_index]]
    best_trajectory = trajectories[min_cost_index]

    return best_u, best_trajectory

def calc_influence_area_cost(trajectories, ia):
    """
    Calculate obstacle cost with additional cost for intersections for multiple trajectories.
    """
    mat0 = ia
    mat = current_mat(mat0)
    zone = current_zone(mat)
    
    # Check if zone is a 1D array and return 0 if true
    if len(zone.shape) == 1:
        return 0

    for trajectory in trajectories:
        zx, zy = zone[:, 0], zone[:, 1]
        tx, ty = trajectory[:, 0], trajectory[:, 1]

        # Vectorized distance calculation
        dx = tx - zx[:, None]
        dy = ty - zy[:, None]
        r = np.hypot(dx, dy)

        # Check for intersections and calculate additional costs
        intersection_mask = r <= 0.5
        intersecting_zones = np.any(intersection_mask, axis=1)

        total_cost = 0
        for i in np.where(intersecting_zones)[0]:
            additional_cost = mat[zone[[i], 1], zone[[i], 0]]
            total_cost += additional_cost

    return np.array(total_cost)

def calc_obstacle_cost(trajectories, ob):
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

        if np.array(r <= 0.5).any():
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


def dwa(play, i):
    """
    Dynamic Window Approach for Each Frame within a Play
    """
    play_df = play[play.frame == i]
    def_df = play_df[(play_df.side == "DEF")]
    off_df = play_df[(play_df.side == "OFF")]
    #Ball Carrier Position, Direction and Speed
    car_x = play_df.iloc[0]["car_x"]
    car_y = play_df.iloc[0]["car_y"]
    car_dir = math.radians(play_df.iloc[0]["car_dir"])
    #delta_dir = math.radians(play_df.iloc[0]["change_dir"])
    delta_dir = 0.125*math.pi/180
    max_delta = math.radians(play_df.iloc[0]["max_change_dir"])/2
    car_s = play_df.iloc[0]["car_s"]
    max_s = play_df.iloc[0]["max_s"]
    max_a = play_df.iloc[0]["max_a"]
    # initial state [x(yd), y(yd), yaw(rad), v(yd/s), omega(rad/s)]
    x = np.array([car_x, car_y, car_dir, car_s, delta_dir])
    # max speed [max(s), max(a), max(yaw)]
    z = np.array([max_s, max_a, max_delta])
    # goal position [x(yd), y(yd)]
    goal = np.array([110, car_y])
    
    # Create boundary obstacles using list comprehensions
    xfield = 120
    yfield = 54
    
    boundary_obs = [[k, 0] for k in range(xfield)] + \
                   [[k, yfield] for k in range(xfield)] + \
                   [[0, j] for j in range(yfield)] + \
                   [[xfield, j] for j in range(yfield)]

    # Create obstacles for offensive and defensive players
    off_obs = off_df[['x_std', 'y_std']].values.tolist()
    def_obs = def_df[['x_std', 'y_std']].values.tolist()

    # Combine all obstacles and convert to numpy array
    ob = np.array(boundary_obs + off_obs + def_obs)
    
    # Influence Areas
    ia = influence_areas(def_df, off_df)

    # Define the grid
    x0 = np.linspace(0, 120, 120)
    y0 = np.linspace(0, 54, 54)
    X, Y = np.meshgrid(x0, y0)

    # config.robot_type = RobotType.circle
    trajectory = np.array(x)

    u, predicted_trajectory = dwa_control(x, z, goal, ob, ia)
    x1 = motion(x, u)  # simulate ball carrier
    trajectory = np.vstack((trajectory, x1))  # store state history

    projection = [play_df.iloc[0]["new_gameId"], play_df.iloc[0]["playId"], i, car_x, car_y, trajectory[1, 0], trajectory[1, 1]]
    #print(projection)

    return projection

# game_plays = games[games.new_gameId == "2022_01_BUF_LA"]
# unique_plays = game_plays['playId'].unique()
# results = []

# play_df = game_plays[game_plays.playId == 692]
# results += [dwa(play_df, frame) for frame in play_df['frame'].unique()]
        
        
def playLoop(gameId):
    """
    Loop through all of the plays in a game
    """
    game_plays = games[games.new_gameId == gameId]
    unique_plays = game_plays['playId'].unique()

    results = []
    for playId in tqdm(unique_plays):
        print(f"Game ID #{gameId}\tPlay ID #{playId}")
        play_df = game_plays[game_plays.playId == playId]
        results += [dwa(play_df, frame) for frame in play_df['frame'].unique()]

    return results


def create_new_directory(directory:str):
    try:
        os.mkdir(directory)
    except FileExistsError as e:
        logging.warning(f"{directory} already exists.")
    except Exception as e:
        logging.critical(f"Unhandeled exception: {e}")

if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument("--week", type=int, required=False)
    args = parser.parse_args()

    if args.week == None:
        pass
    else:
        week = args.week

    create_new_directory("dwa")
    create_new_directory(f"dwa/wk{week}")
    
    print("Loading in `influence_prep2.csv`")
    games0 = pd.read_csv('influence_prep2.csv')

    print(f"Parsing games for week {week}.")
    games = games0.query(f'week == {week}')    
    unique_games = games['new_gameId'].unique()

    for game in unique_games:
        needs_parsing = True
        fullpath = f"dwa/wk{week}/{game}.csv"
        try:
            df = pd.read_csv(fullpath)
            if len(df) >0:
                needs_parsing = False
            else:
                needs_parsing = True
        except:
            needs_parsing = True

        if needs_parsing == True:
            sample_game = playLoop(game)
            df = pd.DataFrame(sample_game, columns=['gameId', 'playId', 'frame', 'car_x', 'car_y', 'traj_x', 'traj_y'])
            
            df.to_csv(fullpath, index = False)
        else:
            print(f"Already parsed game ID {game} in week {week}.")

import pandas as pd
import numpy as np
from tqdm import tqdm
import multiprocessing
from math import degrees, sqrt, atan2
import logging

def get_angle_from_two_points(
        point_1_x:float,
        point_1_y:float,
        point_2_x:float,
        point_2_y:float):
    """
    """
    # v1_theta = atan2(point_1_y, point_1_x)
    # v2_theta = atan2(point_2_y, point_2_x)
    # r = (v2_theta - v1_theta) * (180.0 / np.pi)
    # if r < 0:
    #     r % 360

    # return r
    angle = degrees(atan2(point_1_y - point_2_y, point_1_x - point_2_x))
    if angle < 0:
        angle += 360

    return angle


def parse_height_from_string(ht_ft:str) -> float:
    """
    """
    ht_dict={
        "5-0":{"ft":5,"in":0},
        "5-1":{"ft":5,"in":1},
        "5-2":{"ft":5,"in":2},
        "5-3":{"ft":5,"in":3},
        "5-4":{"ft":5,"in":4},
        "5-5":{"ft":5,"in":5},
        "5-6":{"ft":5,"in":6},
        "5-7":{"ft":5,"in":7},
        "5-8":{"ft":5,"in":8},
        "5-9":{"ft":5,"in":9},
        "5-10":{"ft":5,"in":10},
        "5-11":{"ft":5,"in":11},
        "6-0":{"ft":6,"in":0},
        "6-1":{"ft":6,"in":1},
        "6-2":{"ft":6,"in":2},
        "6-3":{"ft":6,"in":3},
        "6-4":{"ft":6,"in":4},
        "6-5":{"ft":6,"in":5},
        "6-6":{"ft":6,"in":6},
        "6-7":{"ft":6,"in":7},
        "6-8":{"ft":6,"in":8},
        "6-9":{"ft":6,"in":9},
        "6-10":{"ft":6,"in":10},
        "6-11":{"ft":6,"in":11},
        "7-0":{"ft":7,"in":0},
        "7-1":{"ft":7,"in":1},
        "7-2":{"ft":7,"in":2},
        "7-3":{"ft":7,"in":3},
        "7-4":{"ft":7,"in":4},
        "7-5":{"ft":7,"in":5},
        "7-6":{"ft":7,"in":6},
        "7-7":{"ft":7,"in":7},
        "7-8":{"ft":7,"in":8},
        "7-9":{"ft":7,"in":9},
        "7-10":{"ft":7,"in":10},
        "7-11":{"ft":7,"in":11}
    }
    return (ht_dict[ht_ft]['ft']+(ht_dict[ht_ft]['in']/12))

def generate_relative_velo(week: int):
    """

    """
    parsed_tracking_df = pd.DataFrame()
    temp_trakcing_df = pd.DataFrame()
    player_df = pd.DataFrame()

    logging.info("Loading data files.")
    
    logging.debug(f"Loading in `./nfl-big-data-bowl-2024/tracking_week_{week}.csv`.")
    tracking_df = pd.read_csv(
        f"nfl-big-data-bowl-2024/tracking_week_{week}.csv")
    
    logging.debug(f"Loading in `./nfl-big-data-bowl-2024/plays.csv`.")
    plays_df = pd.read_csv("nfl-big-data-bowl-2024/plays.csv")

    logging.debug(f"Loading in `./nfl-big-data-bowl-2024/games.csv`.")
    games_df = pd.read_csv("nfl-big-data-bowl-2024/games.csv")

    logging.debug(f"Loading in `./nfl-big-data-bowl-2024/players.csv`.")
    players_df = pd.read_csv("nfl-big-data-bowl-2024/players.csv")
    players_df['ht_float'] = players_df.apply(
        lambda x: parse_height_from_string(x['height']),axis=1
    )

    logging.info("Creating object `pbp_df`, a combination of `games_df` and `plays_df`.")
    pbp_df = games_df.merge(
        plays_df,
        on=['gameId'],
        how="left"
    )
    del plays_df, games_df

    pbp_df = pbp_df[pbp_df['week'] == week]

    # Normalize play direction
    logging.info("Normalizing play direction so the play direction is always `right`.")
    tracking_df.loc[
        tracking_df['playDirection'] == "left", "x"] = 120 - tracking_df['x']
    tracking_df.loc[
        tracking_df['playDirection'] == "left", "y"] = (160/3) - tracking_df['y']

    tracking_df.loc[
        (tracking_df['playDirection'] == "left") &
        (tracking_df['o'] < 180), "o"] = tracking_df['o'] + 180
    tracking_df.loc[
        (tracking_df['playDirection'] == "left") &
        (tracking_df['o'] > 180), "o"] = tracking_df['o'] - 180

    tracking_df.loc[
        (tracking_df['playDirection'] == "left") &
        (tracking_df['dir'] < 180), "dir"] = tracking_df['dir'] + 180
    tracking_df.loc[
        (tracking_df['playDirection'] == "left") &
        (tracking_df['dir'] > 180), "dir"] = tracking_df['dir'] - 180
    
    tracking_df['playDirection'] = 'right'

    logging.debug("Creating numpy arays for the play data.")
    game_id_arr = pbp_df['gameId'].to_numpy()
    play_id_arr = pbp_df['playId'].to_numpy()
    ball_car_id_arr = pbp_df['ballCarrierId'].to_numpy()
    def_team_arr = pbp_df['defensiveTeam'].to_numpy()

    logging.debug("Parsing tracking data.")
    for i in tqdm(range(0, len(play_id_arr))):
        p_game_id = game_id_arr[i]
        p_play_id = play_id_arr[i]
        p_ball_car_id = ball_car_id_arr[i]
        p_def_team = def_team_arr[i]

        temp_trakcing_df = tracking_df[
            (tracking_df["gameId"] == p_game_id) &
            (tracking_df["playId"] == p_play_id)
        ]

        # Ball Carrier
        ball_car_df = temp_trakcing_df[
            temp_trakcing_df['nflId'] == p_ball_car_id]

        ball_car_df = ball_car_df.rename(
            columns={
                "nflId": "ball_car_nflId",
                "jerseyNumber": "ball_car_jerseyNumber",
                "displayName": "ball_car_displayName",
                "x": "ball_car_x",
                "y": "ball_car_y",
                "s": "ball_car_s",
                "a": "ball_car_a",
                "dis": "ball_car_dis",
                "o": "ball_car_o",
                "dir": "ball_car_dir",
                "club": "ball_car_club"
            }
        )

        # Remove other offensive players for this excersie
        temp_trakcing_df = temp_trakcing_df[temp_trakcing_df['club'] == p_def_team]

        defenders_arr = temp_trakcing_df['nflId'].to_numpy()
        defenders_arr = np.unique(defenders_arr)

        for player_id in defenders_arr:
            player_df = temp_trakcing_df[temp_trakcing_df['nflId'] == player_id]
            player_df = pd.merge(
                player_df,
                ball_car_df,
                on=[
                    "gameId",
                    "playId",
                    "frameId",
                    "time",
                    "event",
                    "playDirection"
                ],
                how="inner"
            )

            parsed_tracking_df = pd.concat(
                [parsed_tracking_df, player_df], ignore_index=True)
            del player_df

        del ball_car_df
        del temp_trakcing_df
        del p_game_id, p_play_id, p_ball_car_id, p_def_team

    logging.info("Applying relative velocity data to this dataset.")

    ## Relative velo calculation
    # Sources:
    # https://courses.lumenlearning.com/suny-osuniversityphysics/chapter/4-5-relative-motion-in-one-and-two-dimensions/
    # https://socratic.org/questions/objects-a-and-b-are-at-the-origin-if-object-a-moves-to-7-2-and-object-b-moves-to
    # https://www.schoolphysics.co.uk/age16-19/Mechanics/Kinematics/text/Relative_velocity/index.html
    # https://assets.openstax.org/oscms-prodcms/media/documents/College_Physics_2e-WEB_7Zesafu.pdf - 126-132

    # parsed_tracking_df['bc_to_player_angle'] = parsed_tracking_df.apply(
    #     lambda x: get_angle_from_two_points(x['ball_car_x'],x['ball_car_y'],x['x'],x['y']),axis=1)
    parsed_tracking_df['relative_velo'] = parsed_tracking_df.apply(
        lambda x: sqrt((x['s']**2)+(x['ball_car_s']**2)),axis=1
    )

    parsed_tracking_df['relative_velo_angle'] = parsed_tracking_df.apply(
        lambda x: get_angle_from_two_points(x['x'],x['y'],x['ball_car_x'],x['ball_car_y']),axis=1)

    # parsed_tracking_df['player_rel_velo'] = ""

    logging.info("Writing the dataframe to a file on the disk.")
    parsed_tracking_df.to_csv(
        f"relative_velo/rv_tracking_week_{week}.csv", index=False)
    return parsed_tracking_df


if __name__ == "__main__":
    for i in range(1,10):
        generate_relative_velo(i)

    # For testing
    #generate_relative_velo(9)

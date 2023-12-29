import pandas as pd
import numpy as np
from tqdm import tqdm
# import multiprocessing
# from math import sqrt, atan2
import logging


def parse_height_from_string(ht_ft: str) -> float:
    """ """
    ht_dict = {
        "5-0": {"ft": 5, "in": 0},
        "5-1": {"ft": 5, "in": 1},
        "5-2": {"ft": 5, "in": 2},
        "5-3": {"ft": 5, "in": 3},
        "5-4": {"ft": 5, "in": 4},
        "5-5": {"ft": 5, "in": 5},
        "5-6": {"ft": 5, "in": 6},
        "5-7": {"ft": 5, "in": 7},
        "5-8": {"ft": 5, "in": 8},
        "5-9": {"ft": 5, "in": 9},
        "5-10": {"ft": 5, "in": 10},
        "5-11": {"ft": 5, "in": 11},
        "6-0": {"ft": 6, "in": 0},
        "6-1": {"ft": 6, "in": 1},
        "6-2": {"ft": 6, "in": 2},
        "6-3": {"ft": 6, "in": 3},
        "6-4": {"ft": 6, "in": 4},
        "6-5": {"ft": 6, "in": 5},
        "6-6": {"ft": 6, "in": 6},
        "6-7": {"ft": 6, "in": 7},
        "6-8": {"ft": 6, "in": 8},
        "6-9": {"ft": 6, "in": 9},
        "6-10": {"ft": 6, "in": 10},
        "6-11": {"ft": 6, "in": 11},
        "7-0": {"ft": 7, "in": 0},
        "7-1": {"ft": 7, "in": 1},
        "7-2": {"ft": 7, "in": 2},
        "7-3": {"ft": 7, "in": 3},
        "7-4": {"ft": 7, "in": 4},
        "7-5": {"ft": 7, "in": 5},
        "7-6": {"ft": 7, "in": 6},
        "7-7": {"ft": 7, "in": 7},
        "7-8": {"ft": 7, "in": 8},
        "7-9": {"ft": 7, "in": 9},
        "7-10": {"ft": 7, "in": 10},
        "7-11": {"ft": 7, "in": 11},
    }
    return ht_dict[ht_ft]["ft"] + (ht_dict[ht_ft]["in"] / 12)


def generate_impact_velo(week: int):
    """ """
    parsed_tracking_df = pd.DataFrame()
    temp_trakcing_df = pd.DataFrame()
    player_df = pd.DataFrame()

    logging.info("Loading data files.")

    logging.debug(f"Loading in `./nfl-big-data-bowl-2024/tracking_week_{week}.csv`.")
    tracking_df = pd.read_csv(f"nfl-big-data-bowl-2024/tracking_week_{week}.csv")

    logging.debug(f"Loading in `./nfl-big-data-bowl-2024/plays.csv`.")
    plays_df = pd.read_csv("nfl-big-data-bowl-2024/plays.csv")

    logging.debug(f"Loading in `./nfl-big-data-bowl-2024/games.csv`.")
    games_df = pd.read_csv("nfl-big-data-bowl-2024/games.csv")

    logging.debug(f"Loading in `./nfl-big-data-bowl-2024/players.csv`.")
    players_df = pd.read_csv("nfl-big-data-bowl-2024/players.csv")
    players_df["ht_ft_float"] = players_df.apply(
        lambda x: parse_height_from_string(x["height"]), axis=1
    )
    players_df = players_df[["nflId", "height", "ht_ft_float", "weight", "position"]]

    logging.info(
        "Creating object `pbp_df`, a combination of `games_df` and `plays_df`."
    )
    pbp_df = games_df.merge(plays_df, on=["gameId"], how="left")
    del plays_df, games_df

    pbp_df = pbp_df[pbp_df["week"] == week]

    # Normalize play direction
    logging.info("Normalizing play direction so the play direction is always `right`.")
    tracking_df.loc[tracking_df["playDirection"] == "left", "x"] = (
        120 - tracking_df["x"]
    )
    tracking_df.loc[tracking_df["playDirection"] == "left", "y"] = (
        160 / 3
    ) - tracking_df["y"]

    tracking_df.loc[
        (tracking_df["playDirection"] == "left") & (tracking_df["o"] < 180), "o"
    ] = (tracking_df["o"] + 180)
    tracking_df.loc[
        (tracking_df["playDirection"] == "left") & (tracking_df["o"] > 180), "o"
    ] = (tracking_df["o"] - 180)

    tracking_df.loc[
        (tracking_df["playDirection"] == "left") & (tracking_df["dir"] < 180), "dir"
    ] = (tracking_df["dir"] + 180)
    tracking_df.loc[
        (tracking_df["playDirection"] == "left") & (tracking_df["dir"] > 180), "dir"
    ] = (tracking_df["dir"] - 180)

    tracking_df["playDirection"] = "right"

    logging.debug("Creating numpy arays for the play data.")
    game_id_arr = pbp_df["gameId"].to_numpy()
    play_id_arr = pbp_df["playId"].to_numpy()
    ball_car_id_arr = pbp_df["ballCarrierId"].to_numpy()
    def_team_arr = pbp_df["defensiveTeam"].to_numpy()

    logging.debug("Parsing tracking data.")
    for i in tqdm(range(0, len(play_id_arr))):
        p_game_id = game_id_arr[i]
        p_play_id = play_id_arr[i]
        p_ball_car_id = ball_car_id_arr[i]
        p_def_team = def_team_arr[i]

        temp_trakcing_df = tracking_df[
            (tracking_df["gameId"] == p_game_id) & (tracking_df["playId"] == p_play_id)
        ]

        # Ball Carrier
        ball_car_df = temp_trakcing_df[temp_trakcing_df["nflId"] == p_ball_car_id]

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
                "club": "ball_car_club",
            }
        )

        # Remove other offensive players for this excersie
        temp_trakcing_df = temp_trakcing_df[temp_trakcing_df["club"] == p_def_team]

        defenders_arr = temp_trakcing_df["nflId"].to_numpy()
        defenders_arr = np.unique(defenders_arr)

        for player_id in defenders_arr:
            player_df = temp_trakcing_df[temp_trakcing_df["nflId"] == player_id]
            player_df = pd.merge(
                player_df,
                ball_car_df,
                on=["gameId", "playId", "frameId", "time", "event", "playDirection"],
                how="inner",
            )

            parsed_tracking_df = pd.concat(
                [parsed_tracking_df, player_df], ignore_index=True
            )
            del player_df

        del ball_car_df
        del temp_trakcing_df
        del p_game_id, p_play_id, p_ball_car_id, p_def_team

    logging.info("Adding HT/WT data for the tacklers.")
    parsed_tracking_df = pd.merge(
        parsed_tracking_df, players_df, on=["nflId"], how="left"
    )

    logging.info("Adding HT/WT data for the ball carriers.")
    parsed_tracking_df = pd.merge(
        parsed_tracking_df,
        players_df,
        left_on=["ball_car_nflId"],
        right_on=["nflId"],
        how="left",
    )
    logging.info("Applying impact velocity data to this dataset.")

    parsed_tracking_df.rename(
        columns={
            # Tackler
            "height_x": "ht_ft_str",
            "ht_ft_float_x": "ht_ft_float",
            "weight_x": "wt_lbs",
            "position_x": "position",
            # Ball Carrier
            "height_y": "bc_ht_ft_str",
            "ht_ft_float_y": "bc_ht_ft_float",
            "weight_y": "bc_wt_lbs",
            "position_y": "bc_position",
        },
        inplace=True,
    )
    parsed_tracking_df = parsed_tracking_df.drop("nflId_y", axis=1)

    parsed_tracking_df["wt_kg"] = parsed_tracking_df["wt_lbs"] * 0.45359237
    parsed_tracking_df["bc_wt_kg"] = parsed_tracking_df["bc_wt_lbs"] * 0.45359237

    parsed_tracking_df["ht_m_float"] = 0.3048 * parsed_tracking_df["ht_ft_float"]
    parsed_tracking_df["bc_ht_m_float"] = 0.3048 * parsed_tracking_df["bc_ht_ft_float"]

    parsed_tracking_df["player_m/s"] = parsed_tracking_df["s"] * 0.9144
    parsed_tracking_df["bc_m/s"] = parsed_tracking_df["ball_car_s"] * 0.9144

    parsed_tracking_df["player_joules"] = (parsed_tracking_df["wt_kg"] / 2) * (
        parsed_tracking_df["player_m/s"] ** 2
    )
    parsed_tracking_df["bc_joules"] = (parsed_tracking_df["bc_wt_kg"] / 2) * (
        parsed_tracking_df["bc_m/s"] ** 2
    )

    parsed_tracking_df["player_bmi"] = parsed_tracking_df["wt_kg"] / (
        parsed_tracking_df["ht_m_float"] ** 2
    )
    parsed_tracking_df["bc_bmi"] = parsed_tracking_df["bc_wt_kg"] / (
        parsed_tracking_df["bc_ht_m_float"] ** 2
    )

    # Expected Impact Velocity
    ## Source:
    # - http://hyperphysics.phy-astr.gsu.edu/hbase/col1d.html
    # - http://hyperphysics.phy-astr.gsu.edu/hbase/inecol.html#c1
    # Assumes:
    ## A. Both players are going directly at each other.
    ## B. Both players are on a colision course.

    parsed_tracking_df["initial_momentum"] = (
        parsed_tracking_df["player_m/s"] * parsed_tracking_df["wt_kg"]
    ) + (parsed_tracking_df["bc_m/s"] * parsed_tracking_df["bc_wt_kg"])

    parsed_tracking_df["player_final_velo"] = (
        parsed_tracking_df["player_m/s"] - parsed_tracking_df["bc_m/s"]
    )
    
    parsed_tracking_df["bc_final_velo"] = (
        (parsed_tracking_df["player_m/s"] * parsed_tracking_df["wt_kg"])
        - (-1 * (parsed_tracking_df["bc_m/s"] * parsed_tracking_df["bc_wt_kg"]))
        - parsed_tracking_df["wt_kg"] * parsed_tracking_df["player_final_velo"]
    ) / parsed_tracking_df["bc_wt_kg"]

    parsed_tracking_df["final_joules"] = (
        (0.5 * parsed_tracking_df["wt_kg"])
        * (parsed_tracking_df["player_final_velo"] ** 2)
    ) + (
        (0.5 * parsed_tracking_df["bc_wt_kg"])
        * (parsed_tracking_df["bc_final_velo"] ** 2)
    )

    ## Final velo, assuming a perfectly elastic collision
    # (aka, defender bounces off the BC)
    parsed_tracking_df["player_elastic_final_velo"] = (
        (
            (parsed_tracking_df["wt_kg"] - parsed_tracking_df["bc_wt_kg"])
            * parsed_tracking_df["player_final_velo"]
        )
        / (parsed_tracking_df["wt_kg"] - (-1 * parsed_tracking_df["bc_wt_kg"]))
    ) - (-1 * parsed_tracking_df["bc_m/s"])

    parsed_tracking_df["bc_elastic_final_velo"] = (
        (2 * parsed_tracking_df["bc_m/s"] * parsed_tracking_df["player_final_velo"])
        / (parsed_tracking_df["bc_m/s"] - (parsed_tracking_df["player_joules"] * -1))
    ) - (-1 * parsed_tracking_df["bc_m/s"])

    ## Final velo, assuming a perfectly inelastic collision
    # (aka, successful tackle)
    
    parsed_tracking_df["inelastic_final_velo"] = (
        (parsed_tracking_df["wt_kg"] * parsed_tracking_df["player_m/s"])
        - (-1 * (parsed_tracking_df["bc_wt_kg"] * parsed_tracking_df["bc_m/s"]))
    ) / (parsed_tracking_df["wt_kg"] - (-1 * parsed_tracking_df["bc_wt_kg"]))
    # parsed_tracking_df["final_kinetic_energy"] = 0

    logging.info("Writing the dataframe to a file on the disk.")
    parsed_tracking_df.to_csv(f"impact_velo/iv_tracking_week_{week}.csv", index=False)

    return parsed_tracking_df


if __name__ == "__main__":
    for i in range(1, 10):
        generate_impact_velo(i)

    ## For testing
    # generate_impact_velo(9)

#import rfcde
import logging
from rfcde import RFCDE
import numpy as np
import pandas as pd
from datetime import datetime

import os
#os.chdir('C:/Users/Owner/Documents/BDB24/data')

if __name__=="__main__":
    model_data = pd.read_csv('rf_data.csv')
    n_trees = 100
    mtry = 1
    node_size = 1
    n_basis = 30
    basis_system = 'cosine'

    ## Uncomment to have all debug messages be printed.
    # logging.basicConfig(format='%(levelname)s:%(message)s', level=logging.DEBUG)

    unique_folds = model_data['game_fold'].unique()
    all_results = []

    for fold in unique_folds:
        logging.info(f"[{datetime.now()}] Training on fold {fold}...")
        print(f"[{datetime.now()}] Training on fold {fold}...")

        logging.info(f"[{datetime.now()}] Splitting the data on fold {fold}")
        # Splitting the data
        train = model_data[model_data['game_fold'] != fold]
        test = model_data[model_data['game_fold'] == fold]

        logging.info(f"[{datetime.now()}] Preparing the data on fold {fold}")
        # Preparing the data
        z_train = train['yds_remaining'].values.reshape(-1,1)
        x_train = train[['sum_prob','cum_dev']].values

        z_test = test['yds_remaining'].values.reshape(-1,1)
        x_test = test[['sum_prob','cum_dev']].values

        logging.info(f"[{datetime.now()}] Training the model on fold {fold}")
        # Training the model
        model = RFCDE(
            n_trees=n_trees, 
            mtry=mtry, 
            node_size=node_size, 
            n_basis=n_basis, 
            basis_system=basis_system
        )
        model.train(x_train, z_train)

        logging.info(f"[{datetime.now()}] Predicting conditional median and mode on fold {fold}")
        # Predicting conditional median
        cond_median = model.predict_quantile(x_test, 0.5)

        logging.info(f"[{datetime.now()}] Storing results in a DataFrame for fold {fold}")
        # Storing results in a DataFrame
        fold_results = pd.DataFrame({
            'cond_median': cond_median
        })

        fold_results.reset_index(drop=True, inplace=True)
        test.reset_index(drop=True, inplace=True)
        fold_results = pd.concat([test, fold_results], axis=1)
        all_results.append(fold_results)

        logging.info(f"[{datetime.now()}] Completed training on fold {fold}.")
        print(f"[{datetime.now()}] Completed training on fold {fold}.")

    logging.info(f"[{datetime.now()}] Combine results from all folds")
    # Combine results from all folds
    final_results = pd.concat(all_results, axis=0)
    final_results.to_csv('results.csv',index=False)
    print("Cross-validation results saved to 'results.csv'")
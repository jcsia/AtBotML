import pandas as pd
import numpy as np
import pickle
import time

import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="white")

from sklearn import metrics
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from scipy import stats
from imblearn.over_sampling import SMOTE

import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers


# Performance evaluation
def get_sk_metrics(y_test, y_pred, average='macro', verbose=0):
    accuracy = metrics.accuracy_score(y_test, y_pred)
    precision = metrics.precision_score(y_test, y_pred, average=average, zero_division=0)
    recall = metrics.recall_score(y_test, y_pred, average=average, zero_division=0)
    f1_score = metrics.f1_score(y_test, y_pred, average=average, zero_division=0)

    if verbose:
        print("Accuracy:\t %.2f%%" % (accuracy * 100.0))
        print("Precision:\t %.2f%%" % (precision * 100.0))
        print("Recall:\t\t %.2f%%" % (recall * 100.0))
        print("F1 score:\t %.2f%%" % (f1_score * 100.0))

    score = dict()
    score['Accuracy'] = accuracy
    score['Precision'] = precision
    score['Recall'] = recall
    score['F1 Score'] = f1_score

    return score


def get_prediction_df(nb_id, test_set=None, feature_size=None):
    ml_models = ['dnn', 'xgb', 'rf', 'svm', 'svm_lin']
    # cols = ['DNN', 'XGB', 'RF', 'SVM', 'SVM_LIN']

    predictions = np.zeros(0)

    if test_set is None:
        test_set = ''
    else:
        test_set = f'{test_set}_'

    for idx, model in enumerate(ml_models):
        if feature_size:
            filename = f"../results/03_At/prediction/{nb_id}_{model}_test_{test_set}predict_f{feature_size}.pkl"
        else:
            filename = f"../results/03_At/prediction/{nb_id}_{model}_test_{test_set}predict.pkl"
        prediction = pickle.load(open(filename, 'rb'))

        if idx:
            predictions = np.vstack((predictions, prediction))
        else:
            predictions = prediction
        # print(f"{idx}, {filename}: {prediction}")
    # print(f"{idx}, {filename}: {predictions}")

    df = pd.DataFrame(data=predictions.T, columns=ml_models)
    df = df.astype('int')

    return df


def load_prediction_results(nb_id):
    df = pd.DataFrame()

    return df


def load_feature_dict(filename, topN, verbose=False, show_plot=False, feature_title=""):
    node_ranking_dict = np.load(filename, allow_pickle=True)
    features = node_ranking_dict.item()

    df_node_rank = pd.DataFrame.from_dict(data=features, orient='index', columns=['importance'])
    df_node_rank_sorted = df_node_rank.sort_values('importance', ascending=False)

    if verbose:
        print(f'{feature_title} feature statistics')
        print(df_node_rank_sorted.describe())
        print()

    if show_plot:
        # fig = plt.figure(figsize=(6, 3))
        # plt.plot(list(features.values())[0:topN])
        # plt.yscale("log")
        # plt.title(f'{feature_title} top {topN} feature importance ')
        # plt.xlabel('top features')
        # plt.ylabel('log Importance')
        # plt.show()

        # Linear and log plots
        fig, axes = plt.subplots(1, 2, figsize=(12, 3), sharex=True, sharey=False)  #

        axes[0].plot(list(features.values()))  # [0:topN])
        axes[0].set_title(f'{feature_title} ranked feature importance')
        axes[0].set_xlabel('top features')
        axes[0].set_ylabel('Importance')

        axes[1].plot(list(features.values()))  # [0:topN])
        axes[1].set_yscale("log")
        axes[1].set_title(f'{feature_title} ranked feature importance')
        axes[1].set_xlabel('top features')
        axes[1].set_ylabel('log Importance')
        plt.show()

        # fig = plt.figure(figsize=(6, 3))
        # plt.plot(list(features.values())[0:topN])
        # plt.yscale("log")
        # plt.title(f'{feature_title} top {topN} feature importance ')
        # plt.xlabel('top features')
        # plt.ylabel('log Importance')
        # plt.show()

    return features


def plot_confusion_matrix(y_test, y_pred, font_scale=1.4, figsize=(10, 7), cmap="Blues"):
    matrix = metrics.confusion_matrix(y_test, y_pred, labels=list(range(10)))

    # df_cm = pd.DataFrame(matrix, columns=np.unique(y_test), index=np.unique(y_test))
    df_cm = pd.DataFrame(matrix, columns=list(range(10)), index=list(range(10)))

    df_cm.index.name = 'Actual'
    df_cm.columns.name = 'Predicted'
    plt.figure(figsize=figsize)
    sns.set(font_scale=font_scale)
    sns.heatmap(df_cm, cmap=cmap, annot=True, annot_kws={"size": 16})  # font size

    return matrix


def fetch_data_metrics(nb_id, feature_size=None):
    model = dict()
    score = dict()
    cm = dict()

    if feature_size is None:
        filename = './models/' + nb_id + '_dnn_model'
        model['dnn'] = keras.models.load_model(filename)

        filename = './models/' + nb_id + '_dnn_results.pkl'
        dnn_history, score['dnn'], cm['dnn'] = pickle.load(open(filename, 'rb'))

        filename = './models/' + nb_id + '_xgb_model_results.pkl'
        model['xgb'], score['xgb'], cm['xgb'] = pickle.load(open(filename, 'rb'))

        filename = './models/' + nb_id + '_rf_model_results.pkl'
        model['rf'], score['rf'], cm['rf'] = pickle.load(open(filename, 'rb'))

        filename = './models/' + nb_id + '_svm_model_results.pkl'
        model['svm'], score['svm'], cm['svm'] = pickle.load(open(filename, 'rb'))

        filename = './models/' + nb_id + '_svm_lin_model_results.pkl'
        model['svm_lin'], score['svm_lin'], cm['svm_lin'] = pickle.load(open(filename, 'rb'))
    else:
        filename = f'./models/{nb_id}_dnn_model_f{feature_size}'
        model['dnn'] = keras.models.load_model(filename)

        filename = f'./models/{nb_id}_dnn_results_f{feature_size}.pkl'
        dnn_history, score['dnn'], cm['dnn'] = pickle.load(open(filename, 'rb'))

        filename = f'./models/{nb_id}_xgb_model_results_f{feature_size}.pkl'
        model['xgb'], score['xgb'], cm['xgb'] = pickle.load(open(filename, 'rb'))

        filename = f'./models/{nb_id}_rf_model_results_f{feature_size}.pkl'
        model['rf'], score['rf'], cm['rf'] = pickle.load(open(filename, 'rb'))

        filename = f'./models/{nb_id}_svm_model_results_f{feature_size}.pkl'
        model['svm'], score['svm'], cm['svm'] = pickle.load(open(filename, 'rb'))

        filename =  f'./models/{nb_id}_svm_lin_model_results_f{feature_size}.pkl'
        model['svm_lin'], score['svm_lin'], cm['svm_lin'] = pickle.load(open(filename, 'rb'))

    return model, score, cm, dnn_history


def plot_performance_metrics(nb_id, max_ylim, file_out, fig_title, offset=0, feature_size=None):
    model, score, cm, dnn_history = fetch_data_metrics(nb_id, feature_size)

    if offset != 0:
        score['dnn'] = get_fuzzy_average_score(cm['dnn'], offset)
        score['xgb'] = get_fuzzy_average_score(cm['xgb'], offset)
        score['rf'] = get_fuzzy_average_score(cm['rf'], offset)
        score['svm'] = get_fuzzy_average_score(cm['svm'], offset)
        score['svm_lin'] = get_fuzzy_average_score(cm['svm_lin'], offset)

    # Create performance matrix dataframe
    col_metrics = ['Accuracy', 'Precision', 'Recall', 'F1 Score']
    df_metrics = pd.DataFrame(columns=col_metrics)

    df_metrics = df_metrics.append(pd.Series(data=score['dnn'], name='DNN'))
    df_metrics = df_metrics.append(pd.Series(data=score['xgb'], name='XGB'))
    df_metrics = df_metrics.append(pd.Series(data=score['rf'], name='RF'))
    df_metrics = df_metrics.append(pd.Series(data=score['svm'], name='SVM'))
    df_metrics = df_metrics.append(pd.Series(data=score['svm_lin'], name='linSVM'))
    df_metrics = df_metrics

    sns.set(font_scale=1.4)
    sns.set_style("ticks")  # whitegrid, white

    fig, axes = plt.subplots(2, 2, figsize=(12, 7), sharex=True, sharey=False)

    # axes y-limits
    for idx, ax in enumerate(axes.flatten()):
        col_metric = col_metrics[idx]
        ax.set(ylim=(0, max_ylim))
        g = sns.barplot(ax=ax, x=df_metrics.index, y=col_metric, data=df_metrics)

    # g.tick_params()
    # axes[2,2].get_xaxis().set_visible(False)

    print("Saving to", file_out)
    print()

    plt.tight_layout()
    fig.suptitle(fig_title)
    fig.subplots_adjust(top=0.91)
    plt.savefig(file_out)
    plt.show()

    return df_metrics


def plot_mse(nb_id, max_ylim, file_out, fig_title, feature_size=None):
    _, score, cm, _ = fetch_data_metrics(nb_id, feature_size)

    col_metrics = ['MSE']
    ml_types = ['dnn', 'xgb', 'rf', 'svm', 'svm_lin']
    ml_label = ['DNN', 'XGB', 'RF', 'SVM', 'linSVM']
    df_metrics = pd.DataFrame(columns=col_metrics)

    total_class_mse = dict()
    for idx, ml_type in enumerate(ml_types):
        total_class_mse[ml_type], _, _ = class_error(cm[ml_type])

        df_metrics = df_metrics.append(pd.Series(data={'MSE': total_class_mse[ml_type]}, name=ml_label[idx]))

    sns.set(font_scale=1.4)
    sns.set_style("ticks")  # whitegrid, white

    fig, axes = plt.subplots(1, 1, figsize=(12, 7), sharex=True, sharey=False)

    col_metric = col_metrics[0]
    axes.set(ylim=(0, max_ylim))
    g = sns.barplot(ax=axes, x=df_metrics.index, y=col_metric, data=df_metrics)

    # g.tick_params()
    # axes[2,2].get_xaxis().set_visible(False)

    print("Saving to", file_out)
    print()

    plt.tight_layout()
    plt.title(fig_title)
    # fig.subplots_adjust(top=0.91)
    plt.savefig(file_out)
    plt.show()

    return df_metrics


def get_fuzzy_average_score(cm, offset=1, average='macro'):
    num_classes = len(cm)
    score = get_fuzzy_score(cm, offset=offset)

    # macro average
    precisions = [x for x in score['fuzzy_precision'].values() if not np.isnan(x)]
    fuzzy_average_precision = sum(precisions) / num_classes

    recalls = [x for x in score['fuzzy_recall'].values() if not np.isnan(x)]
    fuzzy_average_recall = sum(recalls) / num_classes

    # fuzzy_average_recall = np.mean(list(score['fuzzy_recall'].values()))
    # fuzzy_average_precision = np.mean(list(score['fuzzy_precision'].values()))

    f1_scores = [x for x in score['fuzzy_f1_score'].values() if not np.isnan(x)]
    fuzzy_average_f1_score = sum(f1_scores) / num_classes

    average_score = dict()
    average_score['Accuracy'] = score['fuzzy_accuracy']
    average_score['Precision'] = fuzzy_average_precision
    average_score['Recall'] = fuzzy_average_recall
    average_score['F1 Score'] = fuzzy_average_f1_score

    return average_score


def get_fuzzy_score(cm, offset=1):
    num_classes = len(cm)
    fuzzy_true_count = 0
    fuzzy_recall = dict()
    fuzzy_precision = dict()
    fuzzy_f1_score = dict()

    offsets = range(-offset, offset + 1)
    for i in offsets:
        fuzzy_true_count += np.trace(cm, offset=i)

    fuzzy_accuracy = fuzzy_true_count / np.sum(cm)

    cmT = cm.T

    for i in range(num_classes):
        min_idx = max(0, i - offset)
        max_idx = min(num_classes - 1, i + offset)

        fuzzy_recall[i] = np.sum(cm[i, min_idx:max_idx + 1]) / np.sum(cm[i])
        fuzzy_precision[i] = np.sum(cmT[i, min_idx:max_idx + 1]) / np.sum(cmT[i])
        if fuzzy_recall[i] + fuzzy_precision[i] == 0:
            fuzzy_f1_score[i] = np.nan
        else:
            fuzzy_f1_score[i] = 2 * fuzzy_recall[i] * fuzzy_precision[i] / (fuzzy_recall[i] + fuzzy_precision[i])

    score = dict()
    score['fuzzy_accuracy'] = fuzzy_accuracy
    score['fuzzy_recall'] = fuzzy_recall
    score['fuzzy_precision'] = fuzzy_precision
    score['fuzzy_f1_score'] = fuzzy_f1_score

    return score


def class_error(cm):
    num_classes = len(cm)
    class_rss = dict()
    class_mse = dict()

    for actual in range(num_classes):
        class_rss[actual] = 0
        for predicted in range(num_classes):
            if actual != predicted:
                rss = ((actual - predicted) ** 2) * cm[actual, predicted]
                class_rss[actual] += rss

        class_mse[actual] = class_rss[actual] / sum(cm[actual])

    total_class_mse = sum(class_rss.values()) / np.sum(cm)

    return total_class_mse, class_mse, class_rss


# 02b functions
def preprocess_AtBotData(df, standardize_label_func, features_filter=None, num_classes=10,
                         verbose="Info", show_figure=False, nb_id='', test_size=0.3, random_state=7):
    # filter features
    if features_filter is None:
        gene_names = list(df)[6:29107]  # complete gene list
        X = df[gene_names].values
    else:
        X = df[features_filter].values

    # class labelling
    y_class, _, _ = standardize_label_func(df, num_classes, min_zero=True)

    # split data
    X_train, X_test, y_train_class, y_test_class = train_test_split(X, y_class,
                                                                    test_size=test_size,
                                                                    random_state=random_state)

    # pre-processing features (only scale on the training data)
    scaler = StandardScaler()
    scaler.fit(X_train)
    X_train_scaled = scaler.transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    # data balancing (only balance the training data)
    oversample = SMOTE(random_state=random_state)
    X_train_balanced, y_train_balanced = oversample.fit_resample(X_train_scaled, y_train_class, )

    if verbose == "Info":
        print("X:", X.shape)
        print("y:", stats.describe(y_class))
        print()
        print("x_train:", X_train.shape)
        print("x_test:", X_test.shape)
        print("y_train_class:", stats.describe(y_train_class))
        print("y_test_class:", stats.describe(y_test_class))
        print()
        print("Oversampled training data:", len(X_train_balanced))
        print()

    if show_figure:
        fig, axes = plt.subplots(1, 2, figsize=(10, 4), sharex=True, sharey=False)  #

        ax = axes[0]
        ax.hist(y_train_class, bins=num_classes)
        ax.hist(y_test_class, bins=num_classes)
        ax.set_title("Standardized lesion class")
        ax.set_xlabel("Lesion size class")
        ax.set_ylabel("Frequency")
        ax.legend(["train", "test"])

        ax = axes[1]
        ax.hist(y_train_balanced, bins=num_classes) # , density=True
        ax.set_title("Balanced training data")
        ax.set_ylabel("Probability")
        ax.set_xlabel("Lesion size class")

        plt.tight_layout()
        plt.savefig(f"./figures/{nb_id}_lesion_{str(num_classes)}class_dist.pdf")
        plt.show()

    return X_train_balanced, X_test_scaled, y_train_balanced, y_test_class, scaler


def plant_type_standardize_labelling(df, num_classes, min_zero=True):
    df_lesion = df.loc[:, ['HostGenoType', 'Lesion']]
    df_lesion_summary = df_lesion.groupby('HostGenoType').describe()

    host_genotype_min = df_lesion_summary['Lesion']['min'].to_dict()
    host_genotype_max = df_lesion_summary['Lesion']['max'].to_dict()

    y_class = df_lesion.apply(lambda row: standardize_minmax(row, df_lesion_summary,
                                                             host_genotype_min, host_genotype_max, num_classes,
                                                             min_zero), axis=1)
    df_lesion['Class'] = y_class

    return y_class, df_lesion, df_lesion_summary


def standardize_minmax(row, df_lesion_summary, host_genotype_min, host_genotype_max, num_classes, min_zero=True):
    host_genotypes = df_lesion_summary.index.to_list()
    minY = 0
    maxY = 0

    for host in host_genotypes:
        if row['HostGenoType'] == host:
            maxY = host_genotype_max[host] + 0.001
            if min_zero:
                minY = 0
            else:
                minY = host_genotype_min[host]

    y_scaled = (row['Lesion'] - minY) / (maxY - minY) * num_classes
    y_class = np.floor(y_scaled)

    return y_class


def direct_standardize_labelling(df, num_classes, min_zero=True):
    df_lesion = df.loc[:, 'Lesion']
    df_lesion_summary = df_lesion.describe()
    y = df_lesion.values

    # min-max scaling
    maxY = max(y) + 0.001
    if min_zero:
        minY = 0
    else:
        minY = min(y)

    y_scaled = (y - minY) / (maxY - minY) * num_classes
    y_class = np.floor(y_scaled)

    return y_class, df_lesion, df_lesion_summary


# ML models
def train_dnn_1layer(X_train, y_train, X_test, y_test, num_classes, epochs=50, verbose=0):
    output_neurons = num_classes

    model = keras.models.Sequential([
        layers.BatchNormalization(),
        layers.Flatten(input_shape=(X_train.shape[1],)),
        layers.Dense(512, activation='relu'),
        layers.Dropout(0.5),
        layers.Dense(output_neurons, activation='softmax')
    ])

    model.compile(
        optimizer=tf.keras.optimizers.Adam(0.001),
        loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
        metrics=[tf.keras.metrics.SparseCategoricalAccuracy()]
    )

    t0 = time.time()

    history = model.fit(
        x=X_train, y=np.array(y_train, dtype='float'),
        epochs=epochs,
        validation_data=(X_test, np.array(y_test, dtype='float')),
        verbose=verbose
    )

    if verbose:
        t = time.time() - t0
        model.summary()
        print()
        print("elapsed time: %.2f mins" % (t / 60))

    return model, history


# def evaluate_test3_prediction():
#
#     return None

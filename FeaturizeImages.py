import pandas as pd


def feat_data():
    train_data = []
    train_data.append(("img_normal", 0))
    train_data.append(("img_abnormal", 1))
    train_data = pd.DataFrame(train_data, columns=['image', 'label'],index=None)

    # Shuffle the data
    train_data = train_data.sample(frac=1.).reset_index(drop=True)
    return train_data

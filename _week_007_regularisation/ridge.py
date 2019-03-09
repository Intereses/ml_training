"""
Ridge Regression on TfIDF of text features and One-Hot-Encoded Categoricals
https://www.kaggle.com/apapiu/ridge-script
"""
import pandas as pd
import numpy as np
import scipy, os

from sklearn.linear_model import Ridge, LogisticRegression, Lasso
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.preprocessing import LabelBinarizer

import gc

pd.set_option('display.max_columns', 150)
pd.set_option('display.width', 1000)

NUM_BRANDS = 2500
NAME_MIN_DF = 10
MAX_FEAT_DESCP = 50000

print("Reading in Data")
path = '/Users/dhanley2/Documents/mercari/data'
df_train = pd.read_csv(os.path.join(path, 'train.tsv'), sep='\t', nrows = 1000000)
df_test  = pd.read_csv(os.path.join(path, 'test.tsv'), sep='\t')
print(df_train.iloc[1])
df_train.head()

df = pd.concat([df_train, df_test], 0)
nrow_train = df_train.shape[0]
y_train = np.log1p(df_train["price"])

del df_train
gc.collect()
df.iloc[0]
df.head()
print(df.memory_usage(deep = True))


df["category_name"] = df["category_name"].fillna("Other").astype("category")
df["brand_name"] = df["brand_name"].fillna("unknown")

pop_brands = df["brand_name"].value_counts().index[:NUM_BRANDS]
df.loc[~df["brand_name"].isin(pop_brands), "brand_name"] = "Other"

df["item_description"] = df["item_description"].fillna("None")
df["item_condition_id"] = df["item_condition_id"].astype("category")
df["brand_name"] = df["brand_name"].astype("category")

print(df.memory_usage(deep = True))



print("Encodings")
df['name']

count = CountVectorizer(min_df=NAME_MIN_DF)
X_name = count.fit_transform(df["name"])
print(X_name.shape)


df["category_name"].head()
print("Category Encoders")
unique_categories = pd.Series("/".join(df["category_name"].unique().astype("str")).split("/")).unique()
count_category = CountVectorizer()
X_category = count_category.fit_transform(df["category_name"])



print("Descp encoders")
count_descp = TfidfVectorizer(max_features = MAX_FEAT_DESCP, 
                              ngram_range = (1,2),
                              stop_words = "english")
df["item_description"].head()
X_descp = count_descp.fit_transform(df["item_description"])



print("Brand encoders")
vect_brand = LabelBinarizer(sparse_output=True)
X_brand = vect_brand.fit_transform(df["brand_name"])



print("Dummy Encoders")
X_dummies = scipy.sparse.csr_matrix(pd.get_dummies(df[[
    "item_condition_id", "shipping"]], sparse = True).values)



X = scipy.sparse.hstack((X_dummies, 
                         X_descp,
                         X_brand,
                         X_category,
                         X_name)).tocsr()



print([X_dummies.shape, X_category.shape, 
       X_name.shape, X_descp.shape, X_brand.shape])



X_train = X[:nrow_train]
model = Ridge(solver = "lsqr", fit_intercept=False)
model = Lasso( fit_intercept=False)
dir(model)
pd.Series(model.coef_).hist()

pd.Series(model.coef_>0).value_counts()

print("Fitting Model")
model.fit(X_train, y_train)

X_test = X[nrow_train:]
preds = model.predict(X_test)

df_test["price"] = np.expm1(preds)
df_test[["test_id", "price"]].to_csv("submission_ridge.csv", index = False)
df_test.iloc[0]
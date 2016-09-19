# Anne Chen
# run this file and get top five recommended museums

# import modules
import pandas as pd
import numpy as np
import sys
from sklearn.metrics.pairwise import cosine_similarity
import json

# import data
museum_df = pd.read_csv("./data/imputed_df_with_name.csv")
# drop unnamed column
museum_df = museum_df.drop(museum_df.columns[0], axis=1)
# drop column "MuseumName", "ReviewCount", "TotalThingsToDo"
imputed_df = museum_df.drop(museum_df.columns[[0,4,5]], axis=1)
# get number of rows of the dataframe
nrow = imputed_df.shape[0] 
 
def get_museum_lst(target_museum_input):
    '''get the museum lst from input'''
    return target_museum_input.split(';')[1:]

def get_master_srt_lst(museum_lst):
    '''concatenate all top five lists for museums in museum_lst'''
    master_srt_lst = []
    for m in museum_lst:
         master_srt_lst += get_top_five_for_one(m)
    return master_srt_lst

def sort_list(lst):
    '''sort the nested list based on the second item in list'''
    sorted_lst = sorted(lst, key=lambda x: x[1], reverse = True) 
    return sorted_lst

def get_top_five_for_one(target_museum):
    '''get top five museum and consine similarity for one musuem'''
    target_idx = museum_df[museum_df['MuseumName'] == target_museum].index.tolist()[0]
    input_vec = np.array(imputed_df.iloc[target_idx]).reshape(1, -1)
    cos_sim = []
    for i in range(nrow):
        # reshapre the row into a vector
        vec = np.array(imputed_df.iloc[i]).reshape(1, -1)
        # compute and store consine similarity along with musuem name
        cos_sim.append([museum_df['MuseumName'][i], cosine_similarity(input_vec, vec)[0][0]])
    top_five  = sort_list(cos_sim)
    return top_five[1:6] # ignore the top one since it's the target musuem itself

def lst_to_dic(lst):
    '''convert lst into dictionary'''
    top_five_dic = {}
    for i in lst:
        top_five_dic[i[0]] = i[1]
    return top_five_dic

def to_json(name, dic):
    '''write dictionary to json file'''
    filename = name + '.json'
    with open(filename, 'w') as f:
        json.dump(dic, f)

def exclude_selected(museum_lst, srt_lst):
    return [x for x in srt_lst if x[0] not in museum_lst]

def get_sorted_dic(lst):
    dic = {}
    for idx, item in enumerate(lst):
        dic[idx+1] = [item[0], item[1]]
    return dic

if __name__ == "__main__": 
    museum_lst = get_museum_lst(sys.argv[1])
    master_srt_lst = get_master_srt_lst(museum_lst)
    sorted_lst = sort_list(master_srt_lst)
    top_lst = exclude_selected(museum_lst, sorted_lst)
    sorted_dic = get_sorted_dic(top_lst)
    to_json('sorted_suggestion', sorted_dic)     
def gl_model(user_id, num_rec, business_types):
    import FitGraphLab as FG

    recommender = FG.GraphLabRecommmender()
    # recommender object

    recommender.LoadRestaurantTypeNMap(typeFile='diningOptions.csv',
                                       mapFile='restaurants_types.csv',
                                       typeDir='./scripts')

    recommender.LoadModel(train='full', type='ranking', modelName='ranking', dir='./scripts')
    # the model loaded from ./ranking/

    recommendation = recommender.Recommend(modelType='ranking',
                                           user_ids=user_id,
                                           k=num_rec,
                                           business_types=business_types)  # the list

    # user_id is the numpy array of user_id, k is the number of items to recommend

    return recommendation


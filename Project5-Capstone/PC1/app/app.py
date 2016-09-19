from flask import Flask, render_template, redirect, request, url_for
import pandas as pd
import json
from scripts.Supervisor import *
# from flask_googlemaps import GoogleMaps,Map

user_ids = pd.read_csv('user_id.csv')

app = Flask(__name__)

@app.route("/")
def login():
    if request.method == 'POST':
        user1 = request.form['user1']
        user2 = request.form['user2']
        return redirect(url_for('mapview'), user1=user1, user2=user2)
    else:
        return render_template('login.html')


@app.route('/mapview', methods=['POST'])
def mapview():
    user1 = request.form['user1']
    user2 = request.form['user2']
    return render_template('example.html', user1=user1, user2=user2)

@app.route('/model', methods=['GET','POST'])
def getResult():
    user1_row_num = int(request.form.get('user1'))
    user2_row_num = int(request.form.get('user2'))
    user1_id = user_ids['user_id'][user1_row_num]
    user2_id = user_ids['user_id'][user2_row_num]
    user_id = [user1_id, user2_id]
    print user_id

    sw_latitude = request.form.get('sw_latitude')
    sw_longitude = request.form.get('sw_longitude')
    ne_latitude = request.form.get('ne_latitude')
    ne_longitude = request.form.get('ne_longitude')
    bounding_box = {
         'sw_latitude': sw_latitude,
         'sw_longitude': sw_longitude,
         'ne_latitude': ne_latitude,
         'ne_longitude': ne_longitude
     }
    print bounding_box

    FoodType = str(request.form.get('FoodType'))
    print type(FoodType)
    print FoodType
    search_params = {
        'term': FoodType,
        'lang': 'en'
    }
    print search_params

    result = supervisor(user_id,20,bounding_box,search_params)
    print result
    with open('data.txt', 'w') as outfile:
        json.dump(result, outfile)
    return result


if __name__ == "__main__":
    app.run(
        debug=True
    )
from flask import Flask, request, redirect, g, render_template, jsonify
import json
import requests
import random
import base64
import urllib
import os
import pprint
from itertools import *
from sklearn.neighbors import DistanceMetric
#Itunes
from pyItunes import *
#Youtube
from apiclient.discovery import build
from apiclient.errors import HttpError
from oauth2client.tools import argparser


gData = {
    'authorization_header': ''
}
# Set home directory based on logged in user
homedir = os.environ['HOME']
user = homedir.rsplit('/', 1)[-1]

# Authentication Steps, paramaters, and responses are defined at https://developer.spotify.com/web-api/authorization-guide/
# Visit this url to see all the steps, parameters, and expected response.

app = Flask(__name__)

#  Client Keys
CLIENT_ID = "efe1eb24d4144c85820e486aac3dfe6d"
CLIENT_SECRET = "fc0d5b90374e49a4bb9de5f9a6e3abdc"

# Spotify URLS
SPOTIFY_AUTH_URL = "https://accounts.spotify.com/authorize"
SPOTIFY_TOKEN_URL = "https://accounts.spotify.com/api/token"
SPOTIFY_API_BASE_URL = "https://api.spotify.com"
API_VERSION = "v1"
SPOTIFY_API_URL = "{}/{}".format(SPOTIFY_API_BASE_URL, API_VERSION)


# Server-side Parameters
CLIENT_SIDE_URL = "http://127.0.0.1"
PORT = 8080
REDIRECT_URI = "{}:{}/callback/q".format(CLIENT_SIDE_URL, PORT)
SCOPE = "playlist-modify-public playlist-modify-private playlist-read-private"
STATE = ""
SHOW_DIALOG_bool = True
SHOW_DIALOG_str = str(SHOW_DIALOG_bool).lower()


auth_query_parameters = {
    "response_type": "code",
    "redirect_uri": REDIRECT_URI,
    "scope": SCOPE,
    "client_id": CLIENT_ID
}

@app.route("/")
def index():
    return render_template("landing.html")

# @app.route('/foo', methods=['GET', 'POST])
# def foo():
#     return 'HELLO'
@app.route("/authorize", methods=['GET', 'POST'])
def authorize():
    url_args = "&".join(["{}={}".format(key,urllib.quote(val)) for key,val in auth_query_parameters.iteritems()])
    auth_url = "{}/?{}".format(SPOTIFY_AUTH_URL, url_args)
    if os.path.exists("/Users/" + user + "/Music/iTunes/iTunes Music Library.xml"):
        gData['library'] = Library("/Users/" + user + "/Music/iTunes/iTunes Music Library.xml")
        gData['playlists']=gData['library'].getPlaylistNames()
        return redirect(auth_url)
    else:
        return redirect("/itunes")
    # gData['library'] = Library("/Users/" + user + "/Desktop/iTunes Music Library.xml")
    # gData['playlists']=gData['library'].getPlaylistNames()
@app.route("/itunes")
def add_library():
    return

@app.route("/callback/q")
def callback():
    # Auth Step 4: Requests refresh and access tokens
    auth_token = request.args['code']
    code_payload = {
        "grant_type": "authorization_code",
        "code": str(auth_token),
        "redirect_uri": REDIRECT_URI
    }
    base64encoded = base64.b64encode("{}:{}".format(CLIENT_ID, CLIENT_SECRET))
    headers = {"Authorization": "Basic {}".format(base64encoded)}
    post_request = requests.post(SPOTIFY_TOKEN_URL, data=code_payload, headers=headers)

    # Auth Step 5: Tokens are Returned to Application
    response_data = json.loads(post_request.text)
    access_token = response_data["access_token"]
    refresh_token = response_data["refresh_token"]
    token_type = response_data["token_type"]
    expires_in = response_data["expires_in"]

    # Auth Step 6: Use the access token to access Spotify API
    gData['authorization_header'] = {"Authorization":"Bearer {}".format(access_token)}

    # Get profile data
    user_profile_api_endpoint = "{}/me".format(SPOTIFY_API_URL)
    profile_response = requests.get(user_profile_api_endpoint, headers=gData['authorization_header'])
    profile_data = json.loads(profile_response.text)
    gData['avatar'] = profile_data['images'][0]['url']
    return redirect("/home")

@app.route("/home")
def home():
    return render_template("index.html",
                                avatar=gData['avatar'],
                                playlists=enumerate(gData['playlists']))
@app.route("/get_playlist")
def get_playlist():
    playlist = request.args.get('playlist', 0, type=int)
    # MUSE Algo
    seed_data = {
        'track_info': [],
        'track_titles': [],
        'track_artists': [],
        'artist_ids': [],
        'track_plays':[],
        'track_ids': [],
        'track_genres': [],
        'track_attributes': [],
        'track_images': [],
        'distances': []
    }
    # SPOTIFY PLAYLISTS
    # # Get user playlist data
    # playlist_api_endpoint = "{}/playlists".format(profile_data["href"]) + '/08hZj3VHb1C0uJE1gcZ4Cg'
    # playlists_response = requests.get(playlist_api_endpoint, headers=gData['authorization_header'])
    # playlist_data = json.loads(playlists_response.text)
    #
    # # Get song attributes of all songs in a user's playlist
    # ## work on getting better results sent back
    # playlist_tracks = playlist_data["tracks"]["items"]
    # track_ids = []
    # track_names = []
    # for track in playlist_tracks:
    #     artist = track['track']['artists'][0]['name'].lower() or ''
    #     song = track['track']['name'].lower() or ''

    # Extract playlist track data
    for track in gData['library'].getPlaylist(gData['playlists'][playlist]).tracks:
        artist = "%20artist:" + urllib.quote(track.artist).encode() if track.artist else ''
        song = urllib.quote(track.name).encode() or ''
        # API: Search for top ID hit based on artist and song
        track_api_endpoint = SPOTIFY_API_URL  + "/search?q=" + song + artist + "&type=track&limit=1"
        track_response = requests.get(track_api_endpoint, headers=gData['authorization_header'])
        tracks_data = json.loads(track_response.text)
        # Add track data if search for track yielded results
        if len(tracks_data['tracks']['items']) > 0:
            # Append to track_ids
            track_id = tracks_data['tracks']['items'][0]['id']
            seed_data['track_ids'].append(track_id)
            # Append to track_titles
            track_title = tracks_data['tracks']['items'][0]['name']
            seed_data['track_titles'].append(track_title)
            # Append to artist_ids
            track_artist = tracks_data['tracks']['items'][0]['artists'][0]['id']
            seed_data['artist_ids'].append(track_artist)
            # Append to artist_ids
            track_artist = tracks_data['tracks']['items'][0]['artists'][0]['name']
            seed_data['track_artists'].append(track_artist)
            # Append to track_info
            seed_data['track_info'].append(artist.replace('%20artist:', '')  + ' : ' + song + ' - ' + tracks_data['tracks']['items'][0]['artists'][0]['name'] +  ' : '
            + tracks_data['tracks']['items'][0]['name'] + ' - ' + str(track.play_count))
            # Append to play_counts
            seed_data['track_plays'].append(track.play_count)
            # Append images
            seed_data['track_images'].append(tracks_data['tracks']['items'][0]['album']['images'][0]['url'])


    # API: Get collected tracks attributes
    attributes_api_endpoint = SPOTIFY_API_URL  + "/audio-features?ids=" + ",".join(seed_data['track_ids'])
    attributes_response = requests.get(attributes_api_endpoint, headers=gData['authorization_header'])
    seed_data['track_attributes'] = json.loads(attributes_response.text)['audio_features']

    # Multiply songs node based on play_counts to weight songs played more
    library_tracks = []
    for i,attributes in enumerate(seed_data['track_attributes']):
        library_tracks.extend(repeat(attributes, seed_data['track_plays'][i]))

    ##PERFORM PCA TO REDUCE DIMENSIONALITY?

    # Create phantom_average_track
    phantom_average_track = {}
    target_attributes = ['energy','liveness','tempo','speechiness','acousticness','instrumentalness','danceability','loudness']
    for attribute in target_attributes:
        phantom_average_track[attribute] = sum(track[attribute] for track in library_tracks) / len(library_tracks)

    # Create tracks with just the float variables
    library_track_attributes = []
    for track in seed_data['track_attributes']:
        track_float_values = {}
        for attribute in target_attributes:
            track_float_values[attribute] = track[attribute]
        library_track_attributes.append(track_float_values.values())

    # Get seed_distances from phantom_average_track
    seed_distances = [phantom_average_track.values()] + library_track_attributes
    dist = DistanceMetric.get_metric('euclidean')
    distances = dist.pairwise(seed_distances)[0]
    seed_data['distances'] = distances[1:len(distances)]

    # Get attributes of the 5 closest tracks to the phantom_average_track
    seed_indexes = seed_data['distances'].argsort()[:5]
    seed_songs = [seed_data['track_ids'][i] for i in seed_indexes]
    seed_artists = [seed_data['artist_ids'][i] for i in seed_indexes]

    # Get target attributes from phantom_average_track (roudn to two decimals)
    target_energy = str(round(phantom_average_track['energy'],2))
    target_liveness = str(round(phantom_average_track['liveness'],2))
    target_tempo = str(round(phantom_average_track['tempo'],2))
    target_speechiness = str(round(phantom_average_track['speechiness'],2))
    target_acousticness = str(round(phantom_average_track['acousticness'],2))
    target_instrumentalness = str(round(phantom_average_track['instrumentalness'],2))
    target_danceability = str(round(phantom_average_track['danceability'],2))
    target_loudness = str(round(phantom_average_track['loudness'],2))

    # Sort recommendation_data object based on recommendation_data distances
    sorted_seed_indexes = seed_data['distances'].argsort()[:len(seed_data['distances'])]
    for key in seed_data.keys():
        if seed_data[key] != []:
            seed_data[key] = [seed_data[key][i] for i in sorted_seed_indexes]

    tracks = []
    for i, title in enumerate(seed_data['track_titles']):
        tracks.append([
            seed_data['track_artists'][i],
            title,
            round(seed_data['distances'][i], 2),
            seed_data['track_images'][i]
        ])
    #-----------------------------------------------------------
    # Pick best recommended song
    #------------------------------------------------------------

    recommendation_data = {
        'data': [],
        'titles': [],
        'artists': [],
        'images': [],
        'ids': [],
        'attributes': [],
        'distances': [],
        'videos': []
    }

    # API: Get recommended tracks data based on seed and target values
    recommendations_api_endpoint = SPOTIFY_API_URL  + "/recommendations?seed_artists=" + ",".join(seed_artists) + "&target_energy=" + target_energy + "&target_liveness=" + target_liveness + "&target_tempo=" + target_tempo + "&target_speechiness=" + target_speechiness + "&target_acousticness=" + target_acousticness + "&target_instrumentalness=" + target_instrumentalness + "&target_danceability=" + target_danceability + "&target_loudness=" + target_loudness + "&limit=20"
    recommendations_response = requests.get(recommendations_api_endpoint, headers=gData['authorization_header'])
    recommendation_data['data'] = json.loads(recommendations_response.text)['tracks']

    # Set recommended track title and ids
    for track in recommendation_data['data']:
        # Dont add duplicate recommendations or songs already in your playlist
        if track['id'] not in recommendation_data['ids'] and track['id'] not in seed_data['track_ids']:
            recommendation_data['titles'].append(track['name'])
            recommendation_data['artists'].append(track['artists'][0]['name'])
            recommendation_data['ids'].append(track['id'])
            recommendation_data['images'].append(track['album']['images'][0]['url'])

    # API: Get collected tracks attributes
    attributes_api_endpoint = SPOTIFY_API_URL  + "/audio-features?ids=" + ",".join(recommendation_data['ids'])
    attributes_response = requests.get(attributes_api_endpoint, headers=gData['authorization_header'])
    recommendation_data['attributes'] = json.loads(attributes_response.text)['audio_features']

    # Create tracks with just the float variables
    recommendation_track_attributes = []
    for track in recommendation_data['attributes']:
        track_float_values = {}
        for attribute in target_attributes:
            track_float_values[attribute] = track[attribute]
        recommendation_track_attributes.append(track_float_values.values())

    # Get recommendation_distances from phantom_average_track
    recommendation_distances = [phantom_average_track.values()] + recommendation_track_attributes
    dist = DistanceMetric.get_metric('euclidean')
    distances = dist.pairwise(recommendation_distances)[0]
    recommendation_data['distances'] = distances[1:len(distances)]

    # Sort recommendation_data object based on recommendation_data distances
    sorted_recommendation_indexes = recommendation_data['distances'].argsort()[:len(recommendation_data['distances'])]
    for key in recommendation_data.keys():
        if recommendation_data[key] != []:
            recommendation_data[key] = [recommendation_data[key][i] for i in sorted_recommendation_indexes]

    #---------------------------------------------------------------
    # Stats for my playist being better
    #---------------------------------------------------------------
    from scipy import stats
    # perform a two sample ttest of my library track distances from average and my recommend track distances
    seed_recommend_ttest = stats.ttest_ind(seed_data['distances'],recommendation_data['distances'][0:len(seed_data['distances'])])


    # Combine reommendation data into list
    recommendations = []
    for i, title in enumerate(recommendation_data['titles']):
        recommendations.append([
            recommendation_data['artists'][i],
            title,
            round(recommendation_data['distances'][i], 2),
            recommendation_data['images'][i]
        ])

    return jsonify(tracks=tracks,recommendations=recommendations)
    # return render_template("index.html",
    #                             avatar=avatar,
    #                             playlists=playlists,
    #                             recommendations=recommendations
    #                             # track_info=seed_data['track_info'],
    #                             # track_attributes=seed_data['track_attributes'],
    #                             # phantom_average_track=phantom_average_track,
    #                             # seed_distances=seed_data['distances'],
    #                             # seed_songs=seed_songs,
    #                             # seed_artists=seed_artists,
    #                             # results=results[0],
    #                             # video=random.choice(recommendation_data['videos']),
    #                             # seed_recommend_ttest=seed_recommend_ttest
    #                         )

@app.route("/get_video")
def get_video():
    #---------------------------------------------------------------
    # API: Youtube API
    #---------------------------------------------------------------
    artist = request.args.get('artist', 0, type=str)
    song = request.args.get('title', 0, type=str)

    # Set DEVELOPER_KEY to the API key value from the APIs & auth > Registered apps
    # tab of
    #   https://cloud.google.com/console
    # Please ensure that you have enabled the YouTube Data API for your project.
    YOUTUBE_API_SERVICE_NAME = "youtube"
    YOUTUBE_API_VERSION = "v3"

    def youtube_search(options):
        youtube = build(YOUTUBE_API_SERVICE_NAME, YOUTUBE_API_VERSION,
        developerKey='AIzaSyDZiOlhTzNkk1PyxtUygYfz8BRGrRPSrq4')

        # Call the search.list method to retrieve results matching the specified
        # query term.
        search_response = youtube.search().list(
            q=options['q'],
            type="video",
            part="id,snippet",
            maxResults=10
        ).execute()

        search_videos = []

      # Merge video ids
        for search_result in search_response.get("items", []):
            search_videos.append(search_result["id"]["videoId"])

        video_ids = search_videos

          # Call the videos.list method to retrieve location details for each video.
        video_response = youtube.videos().list(
            id=video_ids,
            part='snippet, recordingDetails'
        ).execute()

        videos = []

      # Add each result to the list, and then display the list of matching videos.
        for video_result in video_response.get("items", []):
            videos.append(video_result["snippet"]["title"])
            break;

        return video_ids[0]

    print "http://www.youtube.com/embed/" + str(youtube_search({'q': artist + "," + song})) + '?&iv_load_policy=3&showsearch=0'
    return "http://www.youtube.com/embed/" + str(youtube_search({'q': artist + "," + song})) + '?&iv_load_policy=3&showsearch=0'
    # autoplay=1
    # &showinfo=0
    # with open('./store/data.json', 'w') as outfile:
    #     json.dump(recommendation_data['videos'], outfile)

if __name__ == "__main__":
    app.run(debug=True,port=PORT)

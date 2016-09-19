from app import create_app
import time, sys, cherrypy, os
from paste.translogger import TransLogger
from pyspark import SparkContext, SparkConf

from cherrypy import wsgiserver

def init_spark_context():
    conf = SparkConf().setAppName("yeler-recommender")
    # IMPORTANT: pass aditional Python modules to each worker
    sc = SparkContext(conf=conf, pyFiles=['mf_based_recommender.py', 'app.py'])
    return sc

def run_server(app):
    # Enable WSGI access logging via Paste
    app_logged = TransLogger(app)

    # Mount the WSGI callable object (app) on the root directory
    cherrypy.tree.graft(app_logged, '/')

    # Set the configuration of the web server
    cherrypy.config.update({
        'engine.autoreload.on': True,
        'log.screen': True,
        'server.socket_port': 5000,
        'server.socket_host': '0.0.0.0'
    })
 
    # Start the CherryPy WSGI web server
    cherrypy.engine.start()
    cherrypy.engine.block()

if __name__ == "__main__":
    # Init spark context and load libraries
    sc = init_spark_context()
    app = create_app(sc)
    run_server(app)








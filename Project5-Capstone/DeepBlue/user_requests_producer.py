import time
import json
import random
import msgpack
from kafka import KafkaProducer
from kafka.errors import KafkaError

producer = KafkaProducer(value_serializer=lambda m: json.dumps(m).encode('ascii'),
                         retries=5)


# process all major cities
us_cities = [
                ("NC", "us_charlotte"), 
                ("NV", "us_lasvegas"), 
                ("WI", "us_madison"),
                ("AZ", "us_phoenix"), 
                ("PA", "us_pittsburgh"), 
                ("IL", "us_urbana_champaign")
            ]
canada_cities = [("QC", "canada_montreal")]
germany_cities = [("BW", "germany_karlsruhe")]
uk_cities = [("EDH", "uk_edinburgh")]

cities = us_cities + canada_cities + germany_cities + uk_cities
city_names = [p[1] for p in cities]

sample_keywords = ["restaurants", "bookstore", "cinerma", "eat", "shopping", "sight seeing", "hotel", "chinese food"
    "best ice cream", "shopping center", "exhibition", "library", "museum", "walmart", "macys", "club", "best indian food",
    "movie"]

# Asynchronous by default
for i in range(100):
    random_user_id = random.randint(1e6, 1e8)
    _idx = random.randint(0, len(city_names)-1)
    random_city_name = city_names[_idx]
    _idx_keywords = random.randint(0, len(sample_keywords)-1)
    random_sample_keywords = sample_keywords[_idx_keywords]

    payload = { 
                'user_id': random_user_id, 
                'city_name': random_city_name, 
                'keywords': random_sample_keywords
              }
    print "User {} wanted recommendation for {} in city {}".format(random_user_id, random_sample_keywords, random_city_name)
    future = producer.send('user-request-topic', payload)
    time.sleep(2*random.random())

# Block for 'synchronous' sends
try:
    record_metadata = future.get(timeout=10)
except KafkaError:
    # Decide what to do if produce request failed...
    log.exception()
    pass

# Successful result returns assigned partition and offset
print (record_metadata.topic)
print (record_metadata.partition)
print (record_metadata.offset)

producer.flush()

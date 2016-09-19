import time
import json
import msgpack
from kafka import KafkaProducer
from kafka.errors import KafkaError

producer = KafkaProducer(value_serializer=lambda m: json.dumps(m).encode('ascii'),
                         retries=5)

# Asynchronous by default
for i in range(50):
    #future = producer.send('json-topic', {'key': 'value'+str(i)})
    payload = {'user_id': 10336396, 'city_name': "nyc", 'keyword': "bookstore"}
    future = producer.send('user-request-topic', payload)
    time.sleep(1)

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

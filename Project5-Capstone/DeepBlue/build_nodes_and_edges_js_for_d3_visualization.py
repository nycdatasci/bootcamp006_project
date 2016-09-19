import os
import ast
import json
import random
import collections

# STEP 1: build bizid_to_index_map
with open('/Users/sundeepblue/Bootcamp/allweek/week9/capstone/data/yelp_data/bizid_to_index_map.json', 'r') as fp:
    bizid_to_index_map = json.load(fp)
    print "json file loaded!"
print len(bizid_to_index_map.keys())

# STEP 2: build businessid_to_raters_map
def get_businessid_to_raters_map_for_all_cities():
    folder = "/Users/sundeepblue/Bootcamp/allweek/week9/capstone/data/yelp_data/userid_bizid_star_tuple_for_graph.csv"
    files = os.listdir(folder)
    tuples = []
    for f in files:
        if f.startswith("part-"):
            full_path = os.path.join(folder, f)
            print full_path
            with open(full_path, "r") as g:
                lines = g.readlines()
            tuples += lines
    print len(tuples)
    #business_ids = [int(b.strip()) for b in business_ids]
    #print business_ids[:5]

    # generate businessid to userid map
    businessid_to_raters_map = collections.defaultdict(list)

    for t in tuples:
        sp = t.strip().split(",")
        user_id = int(sp[0].strip())
        
        business_id = int(sp[1].strip())
        businessid_to_raters_map[business_id].append(user_id)
    return businessid_to_raters_map

businessid_to_raters_map = get_businessid_to_raters_map_for_all_cities()


def get_businessid_to_raters_map_for_city(city_name, base_dir):
    file_name = "{}_business.csv".format(city_name)
    full_path = os.path.join(base_dir, city_name, file_name)
    with open(full_path, "r") as f:
        lines = f.readlines()

    lines = lines[1:] # discard header line
    # get all business ids in integer by looking up pre-saved map
    b2r_map_for_city = collections.defaultdict(list)

    for r in lines:
        sp = r.split(',')
        raw_business_id = sp[1].strip()
        b = bizid_to_index_map[raw_business_id]
        who_rated = businessid_to_raters_map[b]
        b2r_map_for_city[b] = who_rated 
    return b2r_map_for_city

# filter highly rated businesses
def __get_3_stuff(businessid_to_raters_map_for_city, num_rate_threshold):
    most_rated_businessids = []
    raters_of_most_rated_businessids = []
    filtered_map = collections.defaultdict(list)

    for b, who_rated in businessid_to_raters_map_for_city.iteritems():
        if len(who_rated) >= num_rate_threshold:
            most_rated_businessids.append(b)
            raters_of_most_rated_businessids += who_rated
            filtered_map[b] = who_rated
    print "number of most_rated_businessids: %d" % len(most_rated_businessids)
    print "number of total raters_of_most_rated_businessids: %d" % len(raters_of_most_rated_businessids)
    return (most_rated_businessids, raters_of_most_rated_businessids, filtered_map)


# combine most_rated_businessids and their raters together and save all of them to a big json
def __save_combined_ids_to_json_as_graph_nodes(most_rated_businessids, raters_of_most_rated_businessids, num_chosen_raters):
    
    if num_chosen_raters == -1:
        ids_together = most_rated_businessids + raters_of_most_rated_businessids
        print "number of ids_together: {} ({} users, {} biz)".format(len(ids_together), len(raters_of_most_rated_businessids), len(most_rated_businessids))
    else:
        r_indices = random.sample(range(len(raters_of_most_rated_businessids)), num_chosen_raters)
        random_chosen_raters = [raters_of_most_rated_businessids[i] for i in r_indices]
        ids_together = most_rated_businessids + random_chosen_raters
        print "number of ids_together: {} ({} users, {} biz)".format(len(ids_together), num_chosen_raters, len(most_rated_businessids))
    
    # dedup again just in case
    ids_together = list(set(ids_together))
    json_output = []
    for idn in ids_together:
        r = {}
        r['id'] = idn
        if idn >= 10000000: # user id
            r['color'] = 'green'
        else: # business id
            r['color'] = 'blue'
        json_output.append(r)
    
    nodes_full_saved_path = os.path.join(base_dir, city_name, "userid_businessid_together.json")
    with open(nodes_full_saved_path, "w") as f:
        json.dump(json_output, f)
    print "Graph nodes saved!"

def __save_from_to_as_graph_edges(filtered_map, num_edges_to_show):
    json_output = []
    for k in filtered_map.keys():
        business_id = k
        raters = filtered_map[k]
        
        if num_edges_to_show == -1:
            how_many = len(raters)
        else:
            how_many = num_edges_to_show
            
        for user_id in raters[:how_many]:
            r = {}
            r['from'] = user_id
            r['to'] = business_id
            json_output.append(r)
    print "total edges: {}".format(len(json_output))
    
    edges_full_saved_path = os.path.join(base_dir, city_name, "userid_bizid.json")
    with open(edges_full_saved_path, "w") as f:
        json.dump(json_output, f)
    print "Graph edges saved!"  

def build_graph_nodes_and_edges_json(base_dir, city_name, num_rate_threshold, num_chosen_raters, num_edges_to_show):
    businessid_to_raters_map_for_city = get_businessid_to_raters_map_for_city(city_name, base_dir)
    most_rated_businessids, raters_of_most_rated_businessids, filtered_map = __get_3_stuff(businessid_to_raters_map_for_city, num_rate_threshold)
    __save_combined_ids_to_json_as_graph_nodes(most_rated_businessids, raters_of_most_rated_businessids, num_chosen_raters)
    __save_from_to_as_graph_edges(filtered_map, num_edges_to_show)
    
    
def load_node_json(city_name, base_dir):
    #city_name = "us_pittsburgh"
    #base_dir = "/Users/sundeepblue/Bootcamp/allweek/week9/capstone/data/yelp_data/split_business_data_by_city/"
    file_name = "userid_businessid_together.json".format(city_name)
    full_path = os.path.join(base_dir, city_name, file_name)
    
    with open(full_path, 'r') as fn:
        nodes = json.load(fn)
    # http://stackoverflow.com/questions/8101649/python-dictionary-removing-u-chars
    # remove prefixing 'u' in the keys of json
    # eg: {u'name': u'A', u'primary_key': 1} ===> {'name': 'A', 'primary_key': 1}
    # otherwise, javascript won't recognize the generated js file
    better_nodes = ast.literal_eval(json.dumps(nodes))
    return better_nodes

def load_edge_json(city_name, base_dir):
    file_name = "userid_bizid.json".format(city_name)
    full_path = os.path.join(base_dir, city_name, file_name)
    
    with open(full_path, 'r') as fe:
        edges = json.load(fe)
    better_edges = ast.literal_eval(json.dumps(edges))
    return better_edges

def build_js_for_network_visualization(city_name, base_dir):
    nodes = load_node_json(city_name, base_dir)
    edges = load_edge_json(city_name, base_dir)
    print "nodes and edges were all loaded!"
    
    file_name = "generated_nodes_and_edges_from_json_{}.js".format(city_name)
    full_path = os.path.join(base_dir, city_name, file_name)
    with open(full_path, "w") as f:
        node_line = "var nodes = {}".format(str(nodes))
        edge_line = "var edges = {}".format(str(edges))
        together = node_line + "\n" + edge_line
        f.write(together)
        print "generate_js_for_network_visualization done!"  


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
base_dir = "/Users/sundeepblue/Bootcamp/allweek/week9/capstone/data/yelp_data/split_business_data_by_city/"

for city_name in city_names:
    print "-----------------------> Processing ", city_name
    num_rate_threshold = 200
    num_chosen_raters = -1
    num_edges_to_show = 20
    build_graph_nodes_and_edges_json(base_dir, city_name, num_rate_threshold, num_chosen_raters, num_edges_to_show)
    build_js_for_network_visualization(city_name, base_dir)


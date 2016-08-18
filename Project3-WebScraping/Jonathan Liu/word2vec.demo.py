print '#' * 47
print '#' * 5 + 'Word2Vec Skill Similarity Search Demo' + '#' * 5
print '#' * 47 + '\n'
print 'Loading...\n'

import gensim

def custom_skill_set( skill_set ):
	custom_skill_set = skill_set[:]
	while True:
		new_key = raw_input( 'Please add new keyword:\n' )
		if new_key:
			custom_skill_set.append( new_key )
		else:
			break
	print 'skill input:'
	print custom_skill_set

	return custom_skill_set

def skill_find( custom_skill_set ):
	print '{:^20} | {:^20}'.format('Word', 'Similarity')
	print '-' * 45
	for w, f in w2v_model.most_similar( custom_skill_set, topn=20 ):
		print '{:<20} | {:<20}'.format( w, f )

w2v_model = gensim.models.Word2Vec.load( 'pre_trained_w2v.model' )

skill_set = [ 'sql', 'python', 'r', 'shiny',
			  'pandas', 'numpy', 'matplotlib',
              'regression', 'lasso', 'ridge', 
              'mongodb', 'bash', 'aws', 'pca',
              'forest', 'knn', 'tableau', 'dataiku'
            ]

print 'Pre-set skills:\n%s\n' %skill_set

while True:
	try:
		if raw_input( 'Include pre-set skills?' ) == 'y':
			custom_skill = custom_skill_set( skill_set )
			skill_find( custom_skill )
		else:
			custom_skill = custom_skill_set( [] )
			skill_find( custom_skill )
		print '#' * 50 + '\n'	
	except KeyError:
		print 'New keyword not in training data'
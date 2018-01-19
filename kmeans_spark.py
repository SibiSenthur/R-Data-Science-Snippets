from pyspark.mllib.clustering import KMeans, KMeansModel
from numpy import array
from math import sqrt

dictionary_filepath = ''  # the path to dictionary.file-0 file
topN = 10
result_filepath = ''  # path to where you store result

with open(dictionary_filepath, 'r') as i:
	dictionary = [line.strip() for line in i]

data = sc.textFile("tfidf_text")
parsedData = data.map(lambda line: array([float(x) for x in line.split(',')]))
clusters = KMeans.train(parsedData, 4, maxIterations=10, runs=10, initializationMode="random")
centers = clusters.centers

with open(result_filepath, 'w') as o:
	for centerId, center in enumerate(centers):
		topNIndex = sorted(range(len(center)), key=lambda i: center[i], reverse=True)[:topN]
		o.write(str(centerId) + '\t' + ','.join(dictionary[index] for index in topNIndex) + '\n')

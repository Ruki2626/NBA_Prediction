# process_nba_data.py
import csv, sys


class CsvData:
	def __init__(self, header, data):
		self.header = header
		self.data = data
	def FieldIndex(self, field):
		return self.header.index(field)
	def Data(self):
		return self.data

def ReadCsv(filename):
	csv_data = []
	with open(filename, "r") as csv_file:
		csv_reader = csv.reader(csv_file, delimiter=",")
		for row in csv_reader:
			csv_data.append(row)
	return CsvData(csv_data[0], csv_data[1:])

		
file_path = "data/player_regular_season.csv"

if __name__ == "__main__":

	try:
		data = ReadCsv(file_path)
		print ("File successfully read into Python.")
	except:
		print ("Invalid log file name:", file_path)
		sys.exit(1)


	for row in data.Data():

		print (row[0])
		















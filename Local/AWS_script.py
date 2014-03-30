
import os
import subprocess
import datetime
import time
import json
import boto
from boto.s3.connection import S3Connection
from boto.s3.key import Key

code_dir = "Simulations_Code/"
simulations_R_script = code_dir + "Simulation.R"
results_dir = "Results/"
RConsoleOutputFile = results_dir + "Simulation.Rout";
config_file_name = 'Local/config.json'

sim_run_id = datetime.datetime.fromtimestamp(time.time()).strftime('%Y%m%d%H%M%S')

os.chdir('../')
subprocess.call(["R", "CMD", "BATCH", "--no-save", '--args ' + sim_run_id, simulations_R_script, RConsoleOutputFile])


with open(config_file_name, 'r') as config_file:
	config_json = str(config_file.read())
	config_values = json.JSONDecoder().decode(config_json)
# config_file = open(config_file_name, 'r')
# config_json = str(config_file.read())
# config_values = json.JSONDecoder().decode(config_json)
config_file.close()

AWS_access_key = str(config_values.get('AWS_access_key'))
AWS_secret_access_key = str(config_values.get('AWS_secret_access_key'))
bucket_name = str(config_values.get('bucket_name'))
results_directory = str(config_values.get('results_directory'))

conn = S3Connection(AWS_access_key, AWS_secret_access_key)
bucket = conn.get_bucket(bucket_name)
k = Key(bucket) 
k.key = results_dir + sim_run_id + '_Simulation'
k.set_contents_from_filename(results_dir + sim_run_id + '_Simulation')

os.system("shutdown now -h")


import sys
import os.path
if(len(sys.argv) != 2):
    print("usage: toJSON <name of tab-delimited file>")
    exit(1)
filename = sys.argv[1]
if(not(os.path.isfile(filename))):
    print("could not read file '%s'" % filename)
    exit(1)

from clustergrammer import Network
net = Network()
net.load_file(filename)
net.cluster()
outputFilename = "%s.json" % filename.split(".")[0]
print("writing json to %s" % outputFilename)
net.write_json_to_file('viz', outputFilename)

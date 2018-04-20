#!/usr/bin/env python
"""
Create a vrt with all the S2 bands.
DEPRECATED: use the R function s2_translate.R in package RSPrePro instead.

Author: Luigi Ranghetti

Year: 2017

License: GPS 3.0
"""

import os
import sys
from osgeo import gdal
from osgeo import osr
import glob
import re
import subprocess
import argparse
import warnings
import tempfile


# Check GDAL version >= 2.1.3
# (2.1.0 is sufficient for long naming convention, but 2.1.3 is required
# since 2016-12-06 to recognise compact names)
if gdal.__version__ < "2.1.2":
    print("This script requires GDAL 2.1.3 or newer")
    sys.exit(1)
        
def s2buildvrt(infile,outdir,infile_utm):
       
    # If a directory were passed instead of a xml, retrieve xml name automatically
    if os.path.isdir(infile):
        try:
            infile_all = glob.glob(os.path.join(os.path.abspath(infile), "*.xml"))
            infile = [f for f in infile_all if re.search('(^S2A_.*\_V[T0-9]+\_[T0-9]+\.xml$)|(^MTD\_MSIL[12][AC]\.xml$)', os.path.basename(f))][0]
        except IndexError:
            raise IOError("Could not find required XML file for S2 data")
    else:
        infile = os.path.abspath(infile)
    # TODO check existence
        
    # retrieve UTM zone
    if infile_utm=="":
        infile_dir = os.path.dirname(infile)
        infile_granules = [os.path.basename(p) for p in glob.glob(os.path.join(infile_dir,"GRANULE","*"))]
        infile_utm_auto = list(set([re.search('\_T([0-9]{2})[A-Z]{3}\_',g).group(1) for g in infile_granules]))
        if len(infile_utm_auto)>1:
            warnings.warn("More than one UTM zone was found in the product; using the first one.", UserWarning)
        infile_utm = infile_utm_auto[0]
    
    # create separate vrt for files
    infile_level = re.search('L([12][AC])', infile).group(1)
    infile_gdalname = ["SENTINEL2_L"+infile_level + ":" + infile + ":" + res + ":" + "EPSG_326"+infile_utm for res in ["10m","20m","60m","TCI"]]
    resvrt_name = [os.path.join(tempfile.gettempdir,"tmp01_"+res+".vrt") for res in ["10m","20m","60m","TCI"]]
    infile_gdal = [gdal.Open(i) for i in infile_gdalname]
    for sel_resvrt_name,sel_infile_gdal in zip(resvrt_name,infile_gdal):
        open(sel_resvrt_name, 'wb').write(sel_infile_gdal.GetMetadata('xml:VRT')[0].encode('utf-8'))




    #SENTINEL2_L1C:MTD_MSIL1C.xml:10m:EPSG_32632 10m.vrt


"""
python2 -c "import sys; from osgeo import gdal; ds = gdal.Open(sys.argv[1]); open(sys.argv[2], 'wb').write(ds.GetMetadata('xml:VRT')[0].encode('utf-8'))" SENTINEL2_L1C:MTD_MSIL1C.xml:10m:EPSG_32632 10m.vrt
python2 -c "import sys; from osgeo import gdal; ds = gdal.Open(sys.argv[1]); open(sys.argv[2], 'wb').write(ds.GetMetadata('xml:VRT')[0].encode('utf-8'))" SENTINEL2_L1C:MTD_MSIL1C.xml:20m:EPSG_32632 20m.vrt
python2 -c "import sys; from osgeo import gdal; ds = gdal.Open(sys.argv[1]); open(sys.argv[2], 'wb').write(ds.GetMetadata('xml:VRT')[0].encode('utf-8'))" SENTINEL2_L1C:MTD_MSIL1C.xml:60m:EPSG_32632 60m.vrt

gdalbuildvrt -b 1 b01.vrt 60m.vrt
gdalbuildvrt -b 1 b01.vrt 60m.vrt
gdalbuildvrt -b 1 b02.vrt 10m.vrt
gdalbuildvrt -b 2 b03.vrt 10m.vrt
gdalbuildvrt -b 3 b04.vrt 10m.vrt
gdalbuildvrt -b 1 b05.vrt 20m.vrt
gdalbuildvrt -b 2 b06.vrt 20m.vrt
gdalbuildvrt -b 3 b07.vrt 20m.vrt
gdalbuildvrt -b 4 b08.vrt 10m.vrt
gdalbuildvrt -b 4 b08a.vrt 20m.vrt
gdalbuildvrt -b 2 b09.vrt 60m.vrt
gdalbuildvrt -b 3 b10.vrt 60m.vrt
gdalbuildvrt -b 5 b11.vrt 20m.vrt
gdalbuildvrt -b 6 b12.vrt 20m.vrt

gdalbuildvrt -separate -resolution highest l1c.vrt b01.vrt b02.vrt b03.vrt 
"""




if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Create a vrt with all the S2 bands.")
    parser.add_argument("infile",nargs="+",
                        help="Input file")
    parser.add_argument("-o", "--outdir",
                        help="Output directory",
                        default="./")
    parser.add_argument("-t", "--utm",
                        help="UTM tile",
                        default="")
    args = parser.parse_args()
    
    s2buildvrt(args.infile[0], args.outdir, args.utm)

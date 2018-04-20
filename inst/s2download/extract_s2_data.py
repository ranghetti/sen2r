#!/usr/bin/env python
"""
Extracts images from Sentinel 2 data using GDAL.

Requires GDAL > 2.1

Saves a single file for each resolution and UTM zone.

Author: Dan Clewley

Creation Date: 08/08/2015

See https://spectraldifferences.wordpress.com/2016/08/16/convert-sentinel-2-data-using-gdal/

"""
from __future__ import print_function
import argparse
import os
import sys
import glob
import subprocess
from distutils.version import LooseVersion
from osgeo import gdal
from osgeo import osr

# Try to import NERC-ARF 'get_gdal_drivers' library
# Available from https://github.com/pmlrsg/arsf_dem_scripts/blob/master/arsf_dem/get_gdal_drivers.py
HAVE_GET_GDAL_DRIVERS = False
try:
    import get_gdal_drivers
    HAVE_GET_GDAL_DRIVERS = True
except ImportError:
    pass

# Check GDAL version is above 2.1 (required for Sentienl 2 driver)
if gdal.__version__ < LooseVersion("2.1.0"):
    print("This script requires GDAL 2.1 or later", file=sys.stderr)
    sys.exit(1)

def get_subdataset_names(infile, resolution=None):
    """
    Get subdataset names from a Sentinel 2 file

    Excludes preview images (last two subdatasets)

    If 'resolution' is passed in will only return subdatasets matching this

    Returns tuple from GDAL with:
        (Name, Description)

    """
    # Check if a directory has been passed in (rather than xml)
    if os.path.isdir(infile):
        try:
            infile = glob.glob(os.path.join(infile, "S2*xml"))[0]
        except IndexError:
            raise IOError("Could not find required XML file for S2 data")

    out_names = []

    dataset = gdal.Open(infile, gdal.GA_ReadOnly)
    subdatasets = dataset.GetSubDatasets()

    # Last two subdatasets are preview images so ignore these.
    for sub_data in subdatasets[:-2]:
        if resolution is not None:
            if resolution not in VALID_RESOLUTIONS:
                raise Exception("Resolution {} is not valid. Available options "
                                "are 10, 20 and 60".format(resolution))
            # Get resolution
            subdata_res = sub_data[0].split(":")[2].rstrip("m")
            if int(subdata_res) == int(resolution):
                out_names.append(sub_data)
        else:
            out_names.append(sub_data)

    return out_names

def get_out_name(subdataset_name_tuple, out_ext=""):
    """
    Get output file name for sub dataset

    Takes tuple with (subdataset name, description)
    """
    subdataset_name = subdataset_name_tuple[0]
    outname = os.path.split(subdataset_name)[-1]

    outname = outname.replace(".xml","")
    outname = outname.replace(":","_")

    # Get UTM string from description.
    utm_str = subdataset_name_tuple[1].split(",")[-1].replace(" ","")
    outname = "_".join(outname.split("_")[:-2])
    outname = "{}_{}".format(outname, utm_str)

    outname = outname + out_ext

    return outname

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Optimise rasters for display "
                                                 "by calculating statistics and adding "
                                                 "overviews.")
    parser.add_argument("infiles",nargs="+",
                        help="Input file(s)")
    parser.add_argument("-o", "--outdir",
                        help="Output directory",
                        required=True)
    parser.add_argument("--of", default="KEA",
                        required=False,
                        help="Output format (GDAL name)")
    args = parser.parse_args()

    # Get extension and creation options
    if HAVE_GET_GDAL_DRIVERS:
        out_ext = get_gdal_drivers.GDALDrivers().get_ext_from_driver(args.of)
        out_creation_options = get_gdal_drivers.GDALDrivers().get_creation_options_from_ext(out_ext)
    # If get_gdal_drives is not available a couple of options are hardcoded
    else:
        if args.of == "KEA":
            out_ext = ".kea"
            out_creation_options = []
        elif args.of == "GTiff":
            out_ext = ".tif"
            out_creation_options = ["COMPRESS=LZW"]
        else:
            print("Could not import 'get_gdal_drivers' to determine correct "
                    "extension for {} format. Try using 'GTiff' or 'KEA' "
                    "instead".format(args.of))

    for dataset in args.infiles:
        print("\n** {} **\n".format(os.path.basename(dataset)))

        subdataset_names = get_subdataset_names(dataset)

        for num, subdataset in enumerate(subdataset_names):
            out_name = get_out_name(subdataset, out_ext)
            out_file = os.path.join(args.outdir, out_name)
            print(" [{0}/{1}] {2}".format(num+1, len(subdataset_names),
                                          out_name))

            gdal_translate_cmd = ["gdal_translate",
                                  "-of", args.of]
            # If there are creation options add these
            for creation_option in out_creation_options:
                gdal_translate_cmd.extend(["-co", creation_option])

            # NetCDF doesn't support uint16 so use int
            if args.of == "netCDF":
                gdal_translate_cmd.extend(["-ot", "Int16"])

            gdal_translate_cmd.extend([subdataset, out_file])
            print(" ".join(gdal_translate_cmd))
            subprocess.check_call(gdal_translate_cmd)


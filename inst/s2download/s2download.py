#! /usr/bin/env python
# -*- coding: iso-8859-1 -*-
"""
 Man function for list, download and correct Sentinel 2 data.
 THIS IS A VERY ALPHA VERSION: the code is only an integration of
 fetchLandsatSentinelFromGoogleCloudscript
 (https://github.com/vascobnunes/fetchLandsatSentinelFromGoogleCloud)
 with commands to run alsot sen2cor corrections.
 The code needs Anaconda2 to run (since Sen2Cor only runs with this
 python distribution), with gdal=2.1.3 and geos=3.5.1 installed.

 Author: Luigi Ranghetti <ranghetti.l @ irea.cnr.it>
 License: GPL 2.0
"""

import os
import sys
import argparse
import re
import shutil
import tempfile
from datetime import datetime
import subprocess
import numpy as np
import multiprocessing
import functools

# Fix copytree problem with dofferent file systems
# https://stackoverflow.com/questions/1303413/python-shutil-copytree-ignore-permissions
def copytree2(src,dst):
    _orig_copystat = shutil.copystat
    shutil.copystat = lambda x, y: x
    shutil.copytree(src, dst)
    shutil.copystat = _orig_copystat
# https://stackoverflow.com/questions/21261132/shutil-rmtree-to-remove-readonly-files
def rmtree2(folder):
    for root, dirs, files in os.walk(folder):
        for f in files:
            os.rmdir(os.path.join(root, f))
        for d in dirs:
            os.remove(os.path.join(root, d))



# Clone dependencies
script_dir = os.path.dirname(os.path.abspath(__file__))
#current_dir = os.getcwd()
sys.path.append(script_dir )
from install_dependencies import download_repo
download_repo(["ranghetti","fetchLandsatSentinelFromGoogleCloud"])
download_repo(["ranghetti","Sentinel-download"])

# Import them
sys.path.insert(0, script_dir+"/fetchLandsatSentinelFromGoogleCloud")
sys.path.insert(0, script_dir+"/Sentinel-download")
from Sentinel_download import download_s2product, Sentinel_download
from fetchFromGoogleCloud import downloadMetadataFile, findS2InCollectionMetadata, downloadS2FromGoogleCloud

SENTINEL2_METADATA_URL = 'http://storage.googleapis.com/gcp-public-data-sentinel-2/index.csv.gz'

def sen2cor_singleproc(u,l1c_dir,l2a_dir,sen2cor_dir,current_dir):
    # print(u)
    l1c_prodname = u
    l2a_prodname = l1c_prodname.replace('MSIL1C','MSIL2A').replace('OPER','USER')

    # Copy files in docker dir
    copytree2(l1c_dir+"/"+l1c_prodname,sen2cor_dir+'/unzipped_scenes/'+l1c_prodname)

    # Run sen2cor
    os.chdir(os.path.dirname(os.path.abspath(__file__)) + "/sen2cor_docker")
    try:
        retcode10 = subprocess.call('./run '+l1c_prodname +' 10', shell=True)
        if retcode10 < 0:
            print("Error in running sen2cor: " + repr(-retcode10))
        else:
            print("Sen2Cor (res 10m and 20m) correctly runned on product " + l1c_prodname)
    except OSError as e:
        print("System error in running sen2cor res 10-20m:" + e)
    try:
        retcode60 = subprocess.call('./run '+l1c_prodname +' 60', shell=True)
        if retcode60 < 0:
            print("Error in running sen2cor: " + repr(-retcode60))
        else:
            print("Sen2Cor (res 60m) correctly runned on product " + l1c_prodname)
    except OSError as e:
        print("System error in running sen2cor res 60m:" + e)
    os.chdir(current_dir)

    if os.path.exists(sen2cor_dir+'/unzipped_scenes/'+l2a_prodname+"/"):
        # Move files in output dir
        copytree2(sen2cor_dir+'/unzipped_scenes/'+l2a_prodname+"/",l2a_dir+"/"+l2a_prodname+'/')
    else:
        print("Error: product "+ l2a_prodname + "was not created.") # TODO create a list of not created products

    # Delete files in docker dir
    os.chdir(os.path.dirname(os.path.abspath(__file__)) + "/sen2cor_docker")
    try:
        retcode = subprocess.call('./run_clean '+l1c_prodname, shell=True)
        if retcode < 0:
            print("Error in cleaning docker workspace: " + repr(-retcode))
        else:
            print("ok cleaning " + l1c_prodname)
    except OSError as e:
        print("System error in cleaning docker workspace:" + e)
    os.chdir(current_dir)
#
#    try:
#        rmtree2(sen2cor_dir+'/unzipped_scenes/'+l2a_prodname)
#    except OSError as e:
#        print("System error 1: " + e.strerror)
#    try:
#        rmtree2(sen2cor_dir+'/unzipped_scenes/'+l1c_prodname)
#    except OSError as e:
#        print("System error 2: " + e.strerror)
#    try:
#        os.remove(sen2cor_dir+'/archives/'+l1c_zipname)
#    except OSError as e:
#        print("System error 3: " + e.strerror)

#    return "b0, "+u



def call_sen2cor(l1c_dir,l2a_dir,l1c_list=None,sen2cor_dir=None,current_dir=os.getcwd(),n_procs=None,overwrite=False):

    if sen2cor_dir is None:
        script_dir = os.path.dirname(os.path.abspath(__file__))
        sen2cor_dir_file = open(script_dir+"/sen2cor_docker/default_data_dir.txt", "r")
        sen2cor_dir = sen2cor_dir_file.read().rstrip('\n').rstrip('/')
        sen2cor_dir_file.close()

    # Retreve file list

#    # Zip unzipped products (FIXME improve doing it only on tocorrect products)
#    l1c_zip_list = [f for f in os.listdir(l1c_dir) if re.search(r'(?:^S2A\_MSIL1C\_[0-9T]+\_[N0-9]+\_[R0-9]+\_[A-Z0-9T]+\_[0-9T]+|^S2A\_OPER\_PRD\_MSIL1C\_[A-Z]+\_[A-Z0-9T]+\_[R0-9]+\_[A-Z0-9VT]+\_[0-9T]+)\.SAFE\.zip$', f)] # FIXME improve reading tile and orbit
    if l1c_list is None:
        l1c_list = [f for f in os.listdir(l1c_dir) if re.search(r'(?:^S2[AB]\_MSIL1C\_[0-9T]+\_[N0-9]+\_[R0-9]+\_[A-Z0-9T]+\_[0-9T]+|^S2[AB]\_OPER\_PRD\_MSIL1C\_[A-Z]+\_[A-Z0-9T]+\_[R0-9]+\_[A-Z0-9VT]+\_[0-9T]+)\.SAFE$', f)] # FIXME improve reading tile and orbit
    if type(l1c_list) is str:
        l1c_list = [l1c_list]
#    l1c_nonexisting_zip_which = np.array([g not in l1c_zip_list for g in [n+'.zip' for n in l1c_dir_list]])
#    l1c_nonexisting_zip_list = np.array(l1c_dir_list)[l1c_nonexisting_zip_which].tolist()
#    for u in l1c_nonexisting_zip_list:
#        print("Zipping product "+os.path.basename(u)+"...")
#        shutil.make_archive(l2a_dir+'/'+os.path.basename(u), 'zip', l2a_dir+'/'+os.path.basename(u))
#    l1c_list = l1c_zip_list

    l1c_partnames = [re.compile('^(.+)\_MSIL1C\_(.+)$').match(f).group(2) for f in l1c_list]

    # exclude existing L2A (being archives or directory)
    l2a_existing_list = [f for f in os.listdir(l2a_dir) if re.search(r'(?:^S2[AB]\_MSIL2A\_[0-9T]+\_[N0-9]+\_[R0-9]+\_[A-Z0-9T]+\_[0-9T]+|^S2[AB]\_[A-Z]+\_PRD\_MSIL2A\_[A-Z]+\_[A-Z0-9T]+\_[R0-9]+\_[A-Z0-9VT]+\_[0-9T]+)\.SAFE$', f)]
    # l2a_dir_existing_list = [f for f in os.listdir(l2a_dir) if re.search(r'(?:^S2[AB]\_MSIL2A\_[0-9T]+\_[N0-9]+\_[R0-9]+\_[A-Z0-9T]+\_[0-9T]+|^S2[AB]\_[A-Z]+\_PRD\_MSIL2A\_[A-Z]+\_[A-Z0-9T]+\_[R0-9]+\_[A-Z0-9VT]+\_[0-9T]+)\.SAFE$', f)]
    # l2a_zip_existing_list = [f for f in os.listdir(l2a_dir) if re.search(r'(?:^S2[AB]\_MSIL2A\_[0-9T]+\_[N0-9]+\_[R0-9]+\_[A-Z0-9T]+\_[0-9T]+|^S2[AB]\_[A-Z]+\_PRD\_MSIL2A\_[A-Z]+\_[A-Z0-9T]+\_[R0-9]+\_[A-Z0-9VT]+\_[0-9T]+)\.SAFE\.zip$', f)]
    l2a_existing_partnames = [t for t in [re.compile('^(.+)\_MSIL2A\_(.+)$').match(f).group(2) for f in l2a_existing_list]]
    # l2a_dir_existing_partnames = [t + '.zip' for t in [re.compile('^(.+)\_MSIL2A\_(.+)$').match(f).group(2) for f in l2a_dir_existing_list]]
    # l2a_zip_existing_partnames = [re.compile('^(.+)\_MSIL2A\_(.+)\.zip$').match(f).group(2) for f in l2a_zip_existing_list]
    # l2a_existing_partnames= l2a_dir_existing_partnames + l2a_zip_existing_partnames
    l1c_nonexisting_which = np.array([g not in l2a_existing_partnames for g in l1c_partnames])
    l1c_nonexisting_list = np.array(l1c_list)[l1c_nonexisting_which].tolist()
    if overwrite:
        l1c_list_tocorrect = l1c_list
    else:
        l1c_list_tocorrect = l1c_nonexisting_list

    if n_procs == 1:
        # run singleprocess
        for u in l1c_list_tocorrect:
            sen2cor_singleproc(u,l1c_dir=l1c_dir,l2a_dir=l2a_dir, sen2cor_dir=sen2cor_dir, current_dir=current_dir)
    else:
        # run multiprocess on sen2cor
        partial_sen2cor_singleproc = functools.partial(sen2cor_singleproc, l2a_dir=l2a_dir, sen2cor_dir=sen2cor_dir, current_dir=current_dir)
        # multithread: http://cslocumwx.github.io/blog/2015/02/23/python-multiprocessing/
        if n_procs is None:
            n_procs = max(1,int(multiprocessing.cpu_count()/2)) # set number of processes = half of cpus
        sen2cor_pool = multiprocessing.Pool(n_procs)
        results = sen2cor_pool.map(partial_sen2cor_singleproc, l1c_list_tocorrect)
#        sys.exit("temp")
        sen2cor_pool.close()
        sen2cor_pool.join()
        print("List processing complete.")


def Sentinel_download_google(start_date, end_date, outputcatalogs, max_cloud, tile, overwrite, write_dir, offline):
    date_start = datetime.strptime(start_date,'%Y%m%d') if start_date is not None else None
    date_end = datetime.strptime(end_date,'%Y%m%d') if end_date is not None else None
    sentinel2_metadata_file = downloadMetadataFile(SENTINEL2_METADATA_URL, outputcatalogs, 'Sentinel', verbose=False)
    url = findS2InCollectionMetadata(sentinel2_metadata_file, cc_limit=max_cloud,
                                     date_start=date_start, date_end=date_end, tile=tile)
    if len(url) == 0:
        print("No image was found with the criteria you chose! Please review your parameters and try again.")
    else:
        print("Found {0} files.".format(len(url)))
        for i, u in enumerate(url):
#            print "prodotti2: "+u
#            print "write_dir: "+write_dir
#            print "basename: "+os.path.basename(u)
            if not offline:
                print("\nDownloading {0} of {1}...".format(i+1, len(url)))
                downloadS2FromGoogleCloud(u, l1c_dir, verbose=False, overwrite=overwrite, partial=False)
#                        if not options.notcorr:
#                            print("\nCorrecting {0} of {1} with Sen2Cor...".format(i+1, len(url)))
#                            img = u.split("/")[len(u.split("/")) - 1]
#                            inputDir = os.path.join(options.write_dir, img)
#                            callSen2Cor(inputDir, verbose=options.verbose)
                # zip product (for compatibility, FIXME check if this operation can be skipped)
                if not os.path.isfile(write_dir+'/'+os.path.basename(u)+'.zip'):
                    shutil.make_archive(write_dir+'/'+os.path.basename(u), 'zip', write_dir+'/'+os.path.basename(u))

            else:
                print(url[i])


def s2_download(downloader="wget", lat=None, lon=None, latmin=None, lonmin=None, latmax=None, lonmax=None,
                  start_date=None, end_date=None, start_ingest_date=None, end_ingest_date=None, orbit=None, tile=None, apihub=None,
                  max_cloud=100.0, l1c_dir=tempfile.gettempdir(), l2a_dir=tempfile.gettempdir(), outputcatalogs=None, max_records=1E3,
                  overwrite=False, offline=False, corr_type="auto", source="scihub", list_only=False,
                  n_procs=None, current_dir = os.getcwd(), downloader_path=''):

    # corr_type:
    # "no" simply downloads available L1C (source?)
    # "manual" downloads L1C (source?) and correct them with sen2cor (delete L1C?)
    # "scihub" downloads available L2A products from sciHub
    # "auto" [still to implement]: check if L2A is available on scihub: if so, downlaod; if not, download L1C and correct it


    # metadata are stored in otuput if outputcatalogs is not defined
    if not outputcatalogs:
        outputcatalogs = l1c_dir

    # if no apithub path was provided, use default
    if apihub is None:
        script_dir = os.path.dirname(os.path.abspath(__file__))
        apihub = script_dir+"/../extdata/apihub.txt"

    # Download L1C products (if requested)
    if corr_type in ["no","manual"]:

        if source == "google":
            # FIXME: aggiungi ricerca del tile da lat/lon
            # TODO aggiungi modalità list_only
            Sentinel_download_google(start_date=start_date, end_date=end_date, outputcatalogs=outputcatalogs,
                                     max_cloud=max_cloud, tile=tile, overwrite=overwrite, write_dir=l1c_dir, offline=offline)


            if corr_type in ['manual']:
                call_sen2cor(l1c_dir=l1c_dir,l2a_dir=l2a_dir,n_procs=n_procs,overwrite=overwrite)

        elif source == "scihub":

            # Download L1C
            if offline is False:
                (s2_urls,s2_filenames) = Sentinel_download(downloader=downloader, lat=lat, lon=lon, latmin=latmin, lonmin=lonmin, latmax=latmax, lonmax=lonmax,
                          start_date=start_date, end_date=end_date, start_ingest_date=start_ingest_date, end_ingest_date=end_ingest_date, no_download=False, level="L1C",
                          orbit=orbit, apihub=apihub, proxy=None, max_cloud=max_cloud, write_dir=l1c_dir,
                          sentinel='S2',tile=tile, dhus=False, list_only=list_only, MaxRecords=max_records, downloaderPath=downloader_path)

            if (corr_type in ['manual']) & (list_only == False):
                print("Call sen2cor on product ")
                call_sen2cor(l1c_dir=l1c_dir,l2a_dir=l2a_dir,n_procs=n_procs,overwrite=overwrite)

        else:
            print("Wrong source value (accepted are: 'google', 'scihub', 'auto' [to implement]")
            sys.exit(-2)



    elif corr_type in ['scihub']:
        # Download L2A
        if offline is False:
            (s2_urls,s2_filenames) = Sentinel_download(downloader=downloader, lat=lat, lon=lon, latmin=latmin, lonmin=lonmin, latmax=latmax, lonmax=lonmax,
                      start_date=start_date, end_date=end_date, start_ingest_date=start_ingest_date, end_ingest_date=end_ingest_date, no_download=False, level="L2A",
                      orbit=orbit, apihub=apihub, proxy=None, max_cloud=max_cloud, write_dir=l2a_dir,
                      sentinel='S2',tile=tile, dhus=False, list_only=list_only, MaxRecords=max_records,downloaderPath=downloader_path)

    elif corr_type in ['auto']:

        # check if esa scihub server is up

        # check available L1C (on the selected source, or on scihub if working, google otherwise) and L2A (scihub, if working)
#        # on google:
#        date_start = datetime.strptime(start_date,'%Y%m%d') if start_date is not None else None
#        date_end = datetime.strptime(end_date,'%Y%m%d') if end_date is not None else None
#        sentinel2_metadata_file = downloadMetadataFile(SENTINEL2_METADATA_URL, outputcatalogs, 'Sentinel', verbose=False)
#        l1c_google_rawlist = findS2InCollectionMetadata(sentinel2_metadata_file, cc_limit=max_cloud,
#                                     date_start=date_start, date_end=date_end, tile=tile)

        if offline is False:
            # scihub L1C:
            (l1c_scihub_urls,l1c_scihub_filenames) = Sentinel_download(downloader=downloader, lat=lat, lon=lon, latmin=latmin, lonmin=lonmin, latmax=latmax, lonmax=lonmax,
                      start_date=start_date, end_date=end_date, start_ingest_date=start_ingest_date, end_ingest_date=end_ingest_date, no_download=False, level="L1C",
                      orbit=orbit, apihub=apihub, proxy=None, max_cloud=max_cloud, write_dir=l1c_dir,
                      sentinel='S2',tile=tile, dhus=False, MaxRecords=max_records,list_only=True,downloaderPath=downloader_path)

            # scihub L2A:
            (l2a_scihub_urls,l2a_scihub_filenames) = Sentinel_download(downloader=downloader, lat=lat, lon=lon, latmin=latmin, lonmin=lonmin, latmax=latmax, lonmax=lonmax,
                      start_date=start_date, end_date=end_date, start_ingest_date=start_ingest_date, end_ingest_date=end_ingest_date, no_download=False, level="L2A",
                      orbit=orbit, apihub=apihub, proxy=None, max_cloud=max_cloud, write_dir=l2a_dir,
                      sentinel='S2',tile=tile, dhus=False, MaxRecords=max_records,list_only=True,downloaderPath=downloader_path)

            s2_compactname_regex = '^S2[AB]\_MSIL[12][AC]\_([0-9]{8}T[0-9]{6})\_N[0-9]{4}\_R([0-9]{3})\_T([A-Z0-9]{5})\_[0-9]{8}T[0-9]{6}\.SAFE$'
            s2_oldname_regex = '^S2[AB]\_[A-Z]{4}\_(PRD)\_MSIL[12][AC]\_.{4}\_[0-9]{8}T[0-9]{6}\_R([0-9]{3})\_V[0-9]{8}T[0-9]{6}\_([0-9]{8}T[0-9]{6})\.SAFE$'
            l2a_scihub_oldnames_match = [re.compile(s2_oldname_regex).match(f) for f in l2a_scihub_filenames]
            l2a_scihub_compactnames_match = [re.compile(s2_compactname_regex).match(f) for f in l2a_scihub_filenames]
            l2a_scihub_oldnames_tuples = [f.group(3,2,1) for f in l2a_scihub_oldnames_match if f is not None]
            l2a_scihub_compactnames_tuples = [f.group(1,2,3) for f in l2a_scihub_compactnames_match if f is not None]
            l2a_scihub_names_tuples = l2a_scihub_compactnames_tuples + l2a_scihub_oldnames_tuples
            l1c_scihub_oldnames_match = [re.compile(s2_oldname_regex).match(f) for f in l1c_scihub_filenames]
            l1c_scihub_compactnames_match = [re.compile(s2_compactname_regex).match(f) for f in l1c_scihub_filenames]
            l1c_scihub_oldnames_tuples = [f.group(3,2,1) for f in l1c_scihub_oldnames_match if f is not None]
            l1c_scihub_compactnames_tuples = [f.group(1,2,3) for f in l1c_scihub_compactnames_match if f is not None]
            l1c_scihub_names_tuples = l1c_scihub_compactnames_tuples + l1c_scihub_oldnames_tuples
            l1c_scihub_names = []
            l2a_scihub_names = []
            for f in l1c_scihub_names_tuples:
                l1c_scihub_names.append('_'.join(f))
            for f in l2a_scihub_names_tuples:
                l2a_scihub_names.append('_'.join(f))

    #        l1c_google_names = [re.compile('^S2A\_MSIL2A\_(.+)$').match(os.path.basename(f)).group(1) for f in l1c_google_rawlist]

    #        if source == 'google':
    #            l1c_google_needed = [f not in l2a_scihub_names for f in l1c_google_names]
    #            l1c_scihub_needed = [f not in list(set(l2a_scihub_names).union(l1c_google_names)) for f in l1c_google_names]
    #        elif source in ['scihub','auto']:
            if l1c_scihub_names != []:
                l1c_scihub_needed_which = np.array([g not in l2a_scihub_names for g in l1c_scihub_names])
                l1c_scihub_needed_filenames = np.array(l1c_scihub_filenames)[l1c_scihub_needed_which].tolist()
                l1c_scihub_needed_urls = np.array(l1c_scihub_urls)[l1c_scihub_needed_which].tolist()
        #        l1c_google_needed = [f not in list(set(l2a_scihub_names).union(l1c_scihub_names)) for f in l1c_scihub_names]
            else:
                l1c_scihub_needed_which = np.array([])
                l1c_scihub_needed_filenames = l1c_scihub_needed_urls = []

            if list_only == False:
                # download L2A from scihub
                Sentinel_download(downloader=downloader, lat=lat, lon=lon, latmin=latmin, lonmin=lonmin, latmax=latmax, lonmax=lonmax,
                        start_date=start_date, end_date=end_date, start_ingest_date=start_ingest_date, end_ingest_date=end_ingest_date, no_download=False, level="L2A",
                        orbit=orbit, apihub=apihub, proxy=None, max_cloud=max_cloud, write_dir=l2a_dir,
                        sentinel='S2',tile=tile, dhus=False, MaxRecords=max_records, file_list=l2a_scihub_filenames,downloaderPath=downloader_path)
                # download L1C from scihub when L2A are not available
                Sentinel_download(downloader=downloader, lat=lat, lon=lon, latmin=latmin, lonmin=lonmin, latmax=latmax, lonmax=lonmax,
                        start_date=start_date, end_date=end_date, start_ingest_date=start_ingest_date, end_ingest_date=end_ingest_date, no_download=False, level="L1C",
                        orbit=orbit, apihub=apihub, proxy=None, max_cloud=max_cloud, write_dir=l1c_dir,
                        sentinel='S2',tile=tile, dhus=False, MaxRecords=max_records, file_list=l1c_scihub_needed_filenames,downloaderPath=downloader_path)
            else:
                (s2_urls,s2_filenames) = (l1c_scihub_needed_urls+l2a_scihub_urls,l1c_scihub_needed_filenames+l2a_scihub_filenames)

        # and correct them
        if list_only == False:
            call_sen2cor(l1c_dir=l1c_dir,l2a_dir=l2a_dir,n_procs=n_procs,overwrite=overwrite)

    if list_only == True:
        return s2_urls, s2_filenames
    else:
        print("Execution finished!\n")



if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Find and download Landsat and Sentinel-2 data from the public Google Cloud")
    parser.add_argument("--downloader", help="downloader options are aria2 or wget (default is wget)",default="wget")
    parser.add_argument("--lat", type=float, help="latitude in decimal degrees",default=None)
    parser.add_argument("--lon", type=float, help="longitude in decimal degrees",default=None)
    parser.add_argument("--latmin", type=float, help="min latitude in decimal degrees",default=None)
    parser.add_argument("--latmax", type=float, help="max latitude in decimal degrees",default=None)
    parser.add_argument("--lonmin", type=float, help="min longitude in decimal degrees",default=None)
    parser.add_argument("--lonmax", type=float, help="max longitude in decimal degrees",default=None)
    parser.add_argument("-d", "--start_date", help="start date, fmt('20151222')",default=None)
    parser.add_argument("-f","--end_date", help="end date, fmt('20151223')",default=None)
    parser.add_argument("--start_ingest_date", help="start ingestion date, fmt('20151222')",default=None)
    parser.add_argument("--end_ingest_date", help="end ingestion date, fmt('20151223')",default=None)
    parser.add_argument("-o","--orbit", help="Orbit Number", default=None)
    parser.add_argument("-t","--tile", help="Sentinel-2 Tile number",default=None)
    parser.add_argument("-a","--apihub", help="ESA apihub account and password file")
    parser.add_argument("-m","--max_cloud", type=float, help="Do not download products with more cloud percentage ",default=100)
    parser.add_argument("-w","--l1c_dir", help="Path where L1C products should be downloaded",default='.')
    parser.add_argument("--l2a_dir", help="Path where L2A products should be downloaded",default='.')
    parser.add_argument("--outputcatalogs", help="Where to download metadata catalog files", default=None)
    parser.add_argument("-r", "--max_records", type=int, help="maximum number of records to download (default=100)",default=100)
    parser.add_argument("--overwrite", help="Overwrite files if existing locally", default=False, action="store_true")
    parser.add_argument("--offline", help="Work offline", default=False, action="store_true")
    parser.add_argument("--list_only", help="Return product list instead of download", default=False, action="store_true")
    parser.add_argument("--corr_type", default="auto", help="")
    parser.add_argument("--source", default="scihub", help="")
    parser.add_argument("--n_procs", default=None, type=int, help="Number of parallel processes for sen2cor (default: half of CPUs number)")
    parser.add_argument("--downloader_path", help="Path in which the wget executable binary is (empty if this path is part of PATH environmental variable)",default='')
    options = parser.parse_args()
    s2_download(downloader=options.downloader, lat=options.lat, lon=options.lon,
                latmin=options.latmin, lonmin=options.lonmin, latmax=options.latmax, lonmax=options.lonmax,
                start_date=options.start_date, end_date=options.end_date, 
                start_ingest_date=options.start_ingest_date, end_ingest_date=options.end_ingest_date, 
                orbit=options.orbit, tile=options.tile, apihub=options.apihub,
                max_cloud=options.max_cloud, l1c_dir=options.l1c_dir, l2a_dir=options.l2a_dir,
                outputcatalogs=options.outputcatalogs, max_records=options.max_records,
                overwrite=options.overwrite, offline=options.offline, list_only=options.list_only,
                corr_type=options.corr_type, source=options.source, n_procs=options.n_procs,
                current_dir=os.getcwd(), downloader_path=options.downloader_path)

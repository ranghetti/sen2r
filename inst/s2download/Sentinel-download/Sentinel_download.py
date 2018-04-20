#! /usr/bin/env python
# -*- coding: iso-8859-1 -*-
"""
Download Sentinel 2 L1C or L2A products
from http://github.com/olivierhagolle/Sentinel-download (2017-06-06)
modified to allow API
Created on Wed Jun  7 13:54:58 2017

@contributor: Luigi Ranghetti <ranghetti.l @ irea.cnr.it>
"""

import os,sys,math
import optparse
from xml.dom import minidom
from datetime import date
import time
import zipfile

###########################################################################
class OptionParser (optparse.OptionParser):

    def check_required (self, opt):
      option = self.get_option(opt)

      # Assumes the option's 'default' is set to None!
      if getattr(self.values, option.dest) is None:
          self.error("%s option not supplied" % option)

###########################################################################


#get URL, name and type within xml file from Scihub
def get_elements(xml_file):
    urls=[]
    contentType=[]
    name=[]
    length=[]
    with open(xml_file) as fic:
        line=fic.readlines()[0].split('<entry>')
        for fragment in line[1:]:
            urls.append(fragment.split('<id>')[1].split('</id>')[0])
            contentType.append(fragment.split('<d:ContentType>')[1].split('</d:ContentType>')[0])
            name.append(fragment.split('<title type="text">')[1].split('</title>')[0])
            length.append(int(fragment.split('<d:ContentLength>')[1].split('</d:ContentLength>')[0]))
            #print name
    os.remove(xml_file)
    return urls,contentType,name,length

# recursively download file tree of a Granule
def download_tree(rep,xml_file,wg,auth,wg_opt,value):
    urls,types,names,length=get_elements(xml_file)
    #print
    #print urls,types,names,length

    for i in range(len(urls)):
        if length[i]==0: # then it is a directory
            nom_rep="%s/%s"%(rep,names[i])
            #print nom_rep
            if not(os.path.exists(nom_rep)):
                os.mkdir(nom_rep)
            commande_wget='%s %s %s%s "%s"'%(wg,auth,wg_opt,'files.xml',urls[i]+"/Nodes")
            # print commande_wget
            os.system(commande_wget)
            try:
                while os.path.getsize("files.xml")==0 : #in case of "bad gateway error"
                    os.system(commande_wget)
                    time.sleep(10)
            except KeyboardInterrupt:
                raise
            download_tree(nom_rep,'files.xml',wg,auth,wg_opt,value)
        else: # a file
            commande_wget='%s %s %s%s "%s"'%(wg,auth,wg_opt,rep+'/'+names[i],urls[i]+'/'+value)
            os.system(commande_wget)
            #while os.path.getsize(rep+'/'+names[i])==0 : #retry download in case of a Bad Gateway error"
            #    os.system(commande_wget)

def get_dir(dir_name,dir_url,product_dir_name,wg,auth,wg_opt,value):
    dir=("%s/%s"%(product_dir_name,dir_name))
    if not(os.path.exists(dir)) :
        os.mkdir(dir)
    commande_wget='%s %s %s%s "%s"'%(wg,auth,wg_opt,'temp.xml',dir_url)
    # print commande_wget
    os.system(commande_wget)
    try:
        while os.path.getsize("temp.xml")==0 : #in case of "bad gateway error"
            os.system(commande_wget)
            time.sleep(10)
    except KeyboardInterrupt:
        raise
    download_tree(product_dir_name+'/'+dir_name,"temp.xml",wg,auth,wg_opt,value)

# ------------------------
# Function to preprare wget command
# (splitted from main function in order to be used both in main than in download_s2product)
def wg_cmd(downloader,apihub,downloader_path=''):
    #====================
    # read password file
    #====================
    try:
        f=file(apihub)
        (account,passwd)=f.readline().split(' ')
        if passwd.endswith('\n'):
            passwd=passwd[:-1]
        f.close()
    except :
        print "error with password file"
        sys.exit(-2)

    #==================================================
    #      prepare wget command line to search catalog
    #==================================================
    if os.path.exists('query_results.xml'):
        os.remove('query_results.xml')

    if downloader=="aria2":
        if sys.platform.startswith('win'):
            wg_bin=os.path.join(downloader_path,"aria2c.exe")
        else:
            wg_bin=os.path.join(downloader_path,"aria2c")
        wg=wg_bin+' --check-certificate=false'
        auth='--http-user="%s" --http-passwd="%s"'%(account,passwd)
        search_output=" --continue -o query_results.xml"
        wg_opt=" -o "
        if sys.platform.startswith('linux') or sys.platform.startswith('darwin'):
            value="\$value"
        else:
            value="$value"
    else :
        if sys.platform.startswith('win'):
            wg_bin=os.path.join(downloader_path,"wget.exe")
        else:
            wg_bin=os.path.join(downloader_path,"wget")
        wg=wg_bin+" --no-check-certificate -q " # TODO added -q not to mess in R: create argument for it
        auth='--user="%s" --password="%s"'%(account,passwd)
        search_output="--output-document=query_results.xml"
        wg_opt=" --continue --output-document="
        if sys.platform.startswith('linux') or sys.platform.startswith('darwin'):
            value="\\$value"
        else:
            value="$value"

    return wg,auth,search_output,wg_opt,value



# ---------------------------------------------------------------
# Function to download a product after having it in the file list
# (this function was splitted from main function on order to allow using it with a file list)
def download_s2product(filename,link,downloader,apihub,tile=None,no_download=False,write_dir='.',file_list=None,downloader_path=''):

    # Compute wg parameters
    (wg,auth,search_output,wg_opt,value) = wg_cmd(downloader,apihub,downloader_path=downloader_path)

    #==================================download  whole product
    if tile==None:
        commande_wget='%s %s %s%s/%s "%s"'%(wg,auth,wg_opt,write_dir,filename+".zip",link)
        #do not download the product if it was already downloaded and unzipped, or if no_download option was selected.
        unzipped_file_exists= os.path.exists(os.path.join(write_dir,filename))
        # print commande_wget
        if unzipped_file_exists==False and no_download==False and (file_list==None or filename in file_list):
            os.system(commande_wget)
            # unzip
            zipfile.ZipFile(os.path.join(write_dir, filename+'.zip')).extractall(write_dir)
            os.remove(os.path.join(write_dir, filename+'.zip'))
        # else:
        #     print unzipped_file_exists, no_download

    # download only one tile, file by file.
    elif tile!=None:
        #do not download the product if the tile is already downloaded.
        unzipped_tile_exists = False
        if os.path.exists(("%s/%s")%(write_dir,filename)):
            if os.path.exists(("%s/%s/%s")%(write_dir,filename,"GRANULE")):
                entries = os.listdir(("%s/%s/%s")%(write_dir,filename,"GRANULE"))
                for entry in entries:
                    entry_split = entry.split("_")
                    if len(entry_split) == 11:
                        tile_identifier = "T"+tile
                        if tile_identifier in entry_split:
                            unzipped_tile_exists= True

        if unzipped_tile_exists or no_download or (file_list!=None and filename not in file_list):
            print "tile already exists or option -n is set, skipping this download"
        else:
            #find URL of header file
            url_file_dir=link.replace(value,"Nodes('%s')/Nodes"%(filename))
            commande_wget='%s %s %s%s "%s"'%(wg,auth,wg_opt,'file_dir.xml',url_file_dir)
            os.system(commande_wget)
            try:
                while os.path.getsize('file_dir.xml')==0 : #in case of "bad gateway error"
                    os.system(commande_wget)
                    time.sleep(10)
            except KeyboardInterrupt:
                raise
            urls,types,names,length=get_elements('file_dir.xml')
            #search for the xml file
            for i in range(len(urls)):
                if names[i].find('SAFL1C')>0 or names[i].find('MSIL1C')>0 or names[i].find('SAFL2A')>0 or names[i].find('MSIL2A.xml')>0:
                    xml=names[i]
                    url_header=urls[i]


            #retrieve list of granules
            url_granule_dir=link.replace(value,"Nodes('%s')/Nodes('GRANULE')/Nodes"%(filename))
            # print url_granule_dir
            commande_wget='%s %s %s%s "%s"'%(wg,auth,wg_opt,'granule_dir.xml',url_granule_dir)
            os.system(commande_wget)
            try:
                while os.path.getsize('granule_dir.xml')==0 : #in case of "bad gateway error"
                    os.system(commande_wget)
                    time.sleep(10)
            except KeyboardInterrupt:
                raise
            urls,types,names,length=get_elements('granule_dir.xml')
            # print(urls)
            granule=None
            #search for the tile
            for i in range(len(urls)):
                if names[i].find(tile)>0:
                    granule=names[i]
            if granule==None:
                print "========================================================================"
                print "Tile %s is not available within product (check coordinates or tile name)"%tile
                print "========================================================================"
            else :
                #create product directory
                product_dir_name=("%s/%s"%(write_dir,filename))
                if not(os.path.exists(product_dir_name)) :
                    os.mkdir(product_dir_name)
                #create tile directory
                granule_dir_name=("%s/%s"%(product_dir_name,'GRANULE'))
                if not(os.path.exists(granule_dir_name)) :
                    os.mkdir(granule_dir_name)
                #create tile directory

                nom_rep_tuile=("%s/%s"%(granule_dir_name,granule))
                if not(os.path.exists(nom_rep_tuile)) :
                    os.mkdir(nom_rep_tuile)
                # download product header file
                print "############################################### header"
                commande_wget='%s %s %s%s "%s"'%(wg,auth,wg_opt,product_dir_name+'/'+xml,url_header+"/"+value)
                # print commande_wget
                os.system(commande_wget)
                try:
                    while os.path.getsize(product_dir_name+'/'+xml)==0 : #in case of "bad gateway error"
                        os.system(commande_wget)
                        time.sleep(10)
                except KeyboardInterrupt:
                    raise
                #download INSPIRE.xml
                url_inspire=link.replace(value,"Nodes('%s')/Nodes('INSPIRE.xml')/"%(filename))
                commande_wget='%s %s %s%s "%s"'%(wg,auth,wg_opt,product_dir_name+'/'+"INSPIRE.xml",url_inspire+"/"+value)

                # print commande_wget
                os.system(commande_wget)
                try:
                    while os.path.getsize(product_dir_name+'/'+"INSPIRE.xml")==0 : #in case of "bad gateway error"
                        os.system(commande_wget)
                        time.sleep(10)
                except KeyboardInterrupt:
                    raise

                #download manifest.safe
                url_manifest=link.replace(value,"Nodes('%s')/Nodes('manifest.safe')/"%(filename))
                commande_wget='%s %s %s%s "%s"'%(wg,auth,wg_opt,product_dir_name+'/'+"manifest.safe",url_manifest+"/"+value)
                # print commande_wget
                os.system(commande_wget)
                try:
                    while os.path.getsize(product_dir_name+'/'+"manifest.safe")==0 : #in case of "bad gateway error"
                        os.system(commande_wget)
                        time.sleep(10)
                except KeyboardInterrupt:
                    raise

                # rep_info
                url_rep_info_dir=link.replace(value,"Nodes('%s')/Nodes('rep_info')/Nodes"%(filename))
                get_dir('rep_info',url_rep_info_dir,product_dir_name,wg,auth,wg_opt,value)

                # HTML
                url_html_dir=link.replace(value,"Nodes('%s')/Nodes('HTML')/Nodes"%(filename))
                get_dir('HTML',url_html_dir,product_dir_name,wg,auth,wg_opt,value)

                # AUX_DATA
                url_auxdata_dir=link.replace(value,"Nodes('%s')/Nodes('AUX_DATA')/Nodes"%(filename))
                get_dir('AUX_DATA',url_auxdata_dir,product_dir_name,wg,auth,wg_opt,value)

                # DATASTRIP
                url_datastrip_dir=link.replace(value,"Nodes('%s')/Nodes('DATASTRIP')/Nodes"%(filename))
                get_dir('DATASTRIP',url_datastrip_dir,product_dir_name,wg,auth,wg_opt,value)


                # granule files
                url_granule="%s('%s')/Nodes"%(url_granule_dir,granule)
                commande_wget='%s %s %s%s "%s"'%(wg,auth,wg_opt,'granule.xml',url_granule)
                # print commande_wget
                os.system(commande_wget)
                try:
                    while os.path.getsize("granule.xml")==0 : #in case of "bad gateway error"
                        os.system(commande_wget)
                        time.sleep(10)
                except KeyboardInterrupt:
                    raise
                download_tree(nom_rep_tuile,"granule.xml",wg,auth,wg_opt,value)



##########################################################################


def Sentinel_download(downloader=None,lat=None,lon=None,latmin=None,latmax=None,lonmin=None,lonmax=None,
                      start_ingest_date=None,end_ingest_date=None,start_date=None,level="L1C",end_date=None,
                      orbit=None,apihub=None,proxy=None,no_download=False,max_cloud=110,write_dir='.',
                      sentinel='S2',tile=None,dhus=False,MaxRecords=100,list_only=False,downloaderPath='',file_list=None):
                        
    #print "downloader_path Sentinel_download: "+downloaderPath # FIXME remove

    url_search="https://scihub.copernicus.eu/apihub/search?q="

    if lat==None or lon==None:
        if latmin==None or lonmin==None or latmax==None or lonmax==None:
            print "provide at least a point or rectangle"
            sys.exit(-1)
        else:
            geom='rectangle'
    else:
        if latmin==None and lonmin==None and latmax==None and lonmax==None:
            geom='point'
        else:
            print "please choose between point and rectangle, but not both"
            sys.exit(-1)
    if tile!=None and sentinel!='S2':
        print "The tile option (-t) can only be used for Sentinel-2"
        sys.exit(-1)

    (wg,auth,search_output,wg_opt,value) = wg_cmd(downloader,apihub,downloader_path=downloaderPath)

    producttype=None
    if sentinel=="S2":
        if level=="L1C":
            producttype="S2MSI1C"
        elif  level=="L2A":
            producttype="S2MSI2Ap"
    if geom=='point':
        query_geom='footprint:\\"Intersects(%f,%f)\\"'%(lat,lon)

    elif geom=='rectangle':
        query_geom='footprint:\\"Intersects(POLYGON(({lonmin} {latmin}, {lonmax} {latmin}, {lonmax} {latmax}, {lonmin} {latmax},{lonmin} {latmin})))\\"'.format(latmin=latmin,latmax=latmax,lonmin=lonmin,lonmax=lonmax)


    if orbit==None:
        query='%s filename:%s*'%(query_geom,sentinel)
    else :
        query='%s filename:%s*R%03d*'%(query_geom,sentinel,orbit)


    # ingestion date
    if start_ingest_date!=None:
        start_ingest_date=start_ingest_date+"T00:00:00.000Z"
    else :
        start_ingest_date="2015-06-23T00:00:00.000Z"

    if end_ingest_date!=None:
        end_ingest_date=end_ingest_date+"T23:59:50.000Z"
    else:
        end_ingest_date="NOW"

    if start_ingest_date!=None or end_ingest_date!=None:
        query_date=" ingestiondate:[%s TO %s]"%(start_ingest_date,end_ingest_date)
        query=query+query_date


    if producttype !=None:
        query_producttype=" producttype:%s "%producttype
        query=query+query_producttype

    # acquisition date

    if start_date!=None:
        start_date=start_date
    else :
        start_date="20150613" #Sentinel-2 launch date

    if end_date!=None:
        end_date=end_date
    else:
        end_date=date.today().strftime(format='%Y%m%d')

    if MaxRecords > 100 or MaxRecords == 0:
        if MaxRecords == 0: # ranghetti edit
            max_records = 1E6 # generate a unusefully huge number of possible requests
        else:
            max_records = MaxRecords
        requests_needed = math.ceil(max_records / 100.0)
        request_list = []
        current_records = 0
        for i in range(int(requests_needed)):
            if (i+1)*100 > max_records:
                request_list.append('%s %s %s "%s%s&rows=%d&start=%d"' % (wg, auth, search_output, url_search, query, max_records % 100, i * 100))
            else:
                request_list.append('%s %s %s "%s%s&rows=%d&start=%d"'%(wg,auth,search_output,url_search,query,100,i*100))
    else:
        commande_wget='%s %s %s "%s%s&rows=%d"'%(wg,auth,search_output,url_search,query,MaxRecords)
        # print commande_wget
        request_list = [commande_wget]


    #=======================
    # parse catalog output»
    #=======================

    # ranghetti edit: if list_only, do not download but save element to download later
    list_prod = list()
    list_filename = list()

    for i in range(len(request_list)):
        os.system(request_list[i])
        try:
            # print("The Copernicus Open Access Hub is temporarily unavailable.")
            xml=minidom.parse("query_results.xml")
        except:
            break
        products=xml.getElementsByTagName("entry")
        if (len(products)==0 and MaxRecords==0): # length of the  query: with MaxRecords==0, stop cycle after a query of length 0
            break # ranghetti edit
        if len(request_list) > 1:
            print "Querying products " + str(100*i+1) + " to " + str(100*i+len(products)) + "..."

        for prod in products:
            #ident=prod.getElementsByTagName("id")[0].firstChild.data # lranghetti hidden manually
            link=prod.getElementsByTagName("link")[0].attributes.items()[0][1]
            #to avoid wget to remove $ special character
            link=link.replace('$value',value)


            for node in prod.getElementsByTagName("str"):
                (name,field)=node.attributes.items()[0]
                if field=="filename":
                    filename= str(node.toxml()).split('>')[1].split('<')[0]   #ugly, but minidom is not straightforward

            #test if product is within the requested time period
            if sentinel.startswith("S2"):
                if len(filename.split("_")) == 7:
                    date_prod=filename.split('_')[-1][:8]
                else:
                    date_prod = filename.split('_')[7][1:9]
            elif  sentinel.startswith("S1"):
                date_prod=filename.split('_')[5][0:8]
            else :
                print "Please choose either S1 or S2"
                sys.exit(-1)

            if date_prod>=start_date and date_prod<=end_date:

                # print what has been found
                print "\n==============================================="
                print date_prod,start_date,end_date
                print filename
                print link
                if dhus==True:
                    link=link.replace("apihub","dhus")


                if sentinel.find("S2") >=0 :
                    for node in prod.getElementsByTagName("double"):
                        (name,field)=node.attributes.items()[0]
                        if field=="cloudcoverpercentage":
                            cloud=float((node.toxml()).split('>')[1].split('<')[0])
                            print "cloud percentage = %5.2f %%"%cloud
                else:
                    cloud=0

                print "===============================================\n"

                # ranghetti edit: if lis_only, do not download but save element as to download later
                list_prod.append(link)
                list_filename.append(filename)
                # otherwise, continue with the original code
                if list_only is False:
                    download_s2product(filename=filename,link=link,downloader=downloader,apihub=apihub,tile=tile,no_download=no_download,write_dir=write_dir,file_list=file_list,downloader_path=downloaderPath)


    # ranghetti edit: if list_only, return products list
    return list_prod, list_filename


if __name__ == "__main__":
    #==================
    #parse command line
    #==================
    if len(sys.argv) == 1:
        prog = os.path.basename(sys.argv[0])
        print '      '+sys.argv[0]+' [options]'
        print "     Aide : ", prog, " --help"
        print "        ou : ", prog, " -h"
        print "example python  %s --lat 43.6 --lon 1.44 -a apihub.txt "%sys.argv[0]
        print "example python  %s --lat 43.6 --lon 1.44 -a apihub.txt -t 31TCJ "%sys.argv[0]
        sys.exit(-1)
    else:
        usage = "usage: %prog [options] "
        parser = OptionParser(usage=usage)
        parser.add_option("--downloader", dest="downloader", action="store", type="string", \
                help="downloader options are aria2 or wget (default is wget)",default=None)
        parser.add_option("--lat", dest="lat", action="store", type="float", \
                help="latitude in decimal degrees",default=None)
        parser.add_option("--lon", dest="lon", action="store", type="float", \
                help="longitude in decimal degrees",default=None)
        parser.add_option("--latmin", dest="latmin", action="store", type="float", \
                help="min latitude in decimal degrees",default=None)
        parser.add_option("--latmax", dest="latmax", action="store", type="float", \
                help="max latitude in decimal degrees",default=None)
        parser.add_option("--lonmin", dest="lonmin", action="store", type="float", \
                help="min longitude in decimal degrees",default=None)
        parser.add_option("--lonmax", dest="lonmax", action="store", type="float", \
                help="max longitude in decimal degrees",default=None)
        parser.add_option("--id", "--start_ingest_date", dest="start_ingest_date", action="store", type="string", \
                help="start ingestion date, fmt('2015-12-22')",default=None)
        parser.add_option("--if","--end_ingest_date", dest="end_ingest_date", action="store", type="string", \
                help="end ingestion date, fmt('2015-12-23')",default=None)
        parser.add_option("-d", "--start_date", dest="start_date", action="store", type="string", \
                help="start date, fmt('20151222')",default=None)
        parser.add_option("-l", "--level", dest="level", action="store", type="string", \
                help="L1C,L2A...",default="L1C")
        parser.add_option("-f","--end_date", dest="end_date", action="store", type="string", \
                help="end date, fmt('20151223')",default=None)
        parser.add_option("-o","--orbit", dest="orbit", action="store", type="int", \
                help="Orbit Number", default=None)
        parser.add_option("-a","--apihub_passwd", dest="apihub", action="store", type="string", \
                help="ESA apihub account and password file")
        parser.add_option("-p","--proxy_passwd", dest="proxy", action="store", type="string", \
                help="Proxy account and password file",default=None)
        parser.add_option("-n","--no_download", dest="no_download", action="store_true",  \
                help="Do not download products, just print wget command",default=False)
        parser.add_option("-m","--max_cloud", dest="max_cloud", action="store",type="float",  \
                help="Do not download products with more cloud percentage ",default=110)
        parser.add_option("-w","--write_dir", dest="write_dir", action="store",type="string",  \
                help="Path where the products should be downloaded",default='.')
        parser.add_option("-s","--sentinel", dest="sentinel", action="store",type="string",  \
                help="Sentinel mission considered",default='S2')
        parser.add_option("-t","--tile", dest="tile", action="store",type="string",  \
                help="Sentinel-2 Tile number",default=None)
        parser.add_option("--dhus",dest="dhus",action="store_true",  \
                help="Try dhus interface when apihub is not working",default=False)
        parser.add_option("-r",dest="MaxRecords",action="store",type="int",  \
                help="maximum number of records to download (default=100)",default=100)
        parser.add_option("--list_only",dest="list_only",action="store_true",  \
                help="Only list available products, without downloading",default=False)
        parser.add_option("--downloaderPath", dest="downloaderPath", action="store",type="string",  \
                help="Path in which the wget executable binary is (empty if this path is part of PATH environmental variable)",default='')


        (options, args) = parser.parse_args()

        parser.check_required("-a")

    Sentinel_download(options.downloader,options.lat,options.lon,options.latmin,options.latmax,options.lonmin,options.lonmax,
                      options.start_ingest_date,options.end_ingest_date,options.start_date,options.level,options.end_date,
                      options.orbit,options.apihub,options.proxy,options.no_download,options.max_cloud,options.write_dir,
                      options.sentinel,options.tile,options.dhus,options.MaxRecords,options.downloaderPath,options.list_only)

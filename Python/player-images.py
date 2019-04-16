## Ben Kite

## This script saves player headshots from baseball-reference.com

import pandas, numpy
import requests, bs4
import re, os
import urllib.request as urlr

def getImages(url, tableID):
    if "pitching" in tableID:
        pos = "pitchers"
    else:
        pos = "batters"
    res = requests.get(url)
    ## Work around comments
    comm = re.compile("<!--|-->")
    soup = bs4.BeautifulSoup(comm.sub("", res.text), 'lxml')
    tables = soup.findAll('table', id = tableID)
    data_rows = tables[0].findAll('tr')
    for i in range(1, len(data_rows)):
        if len(data_rows[i].findAll("td")) > 0:
            tmp = str(data_rows[i].findAll("td")[1])  
            link = tmp[tmp.find("href=\"") + 6:tmp.find("shtml") + 5]
            name = tmp[tmp.find("shtml\">") + 7: ]
            name = name[:name.find("<")]
            name = name.replace(" ", "_")
            if not os.path.exists("../data/" + pos + "/plots/" + name + "/headshot.png"):
                ppage = "http://www.baseball-reference.com" + link
                res = requests.get(ppage)
                ## Work around comments
                comm = re.compile("<!--|-->")
                soup = bs4.BeautifulSoup(comm.sub("", res.text), 'lxml')
                image = str(soup.findAll("img")[1])
                image = image[image.find("src=\"") + 5: image.find(".jpg") + 4]
                try:
                    urlr.urlretrieve(image, "../data/" + pos + "/plots/" + name + "/headshot.png")
                except ValueError:
                    print("No image for " + name)
                except FileNotFoundError:
                    print("Couldn't find a directory for " + name)
   

teams = ['ATL', 'ARI', 'BAL', 'BOS', 'CHC', 'CHW', 'CIN', 'CLE', 'COL', 'DET',
         'KCR', 'HOU', 'LAA', 'LAD', 'MIA', 'MIL', 'MIN', 'NYM', 'NYY', 'OAK',
         'PHI', 'PIT', 'SDP', 'SEA', 'SFG', 'STL', 'TBR', 'TEX', 'TOR', 'WSN'] 
         
for t in teams:
    url = "http://www.baseball-reference.com/teams/" + t + "/2019.shtml"
    getImages(url, "team_pitching")
    #getImages(url, "team_batting")
    

## Ben Kite

## 2017-05-28

import requests, bs4
import re
import pandas
import io

## For the pid value provided, a csv file is saved for the desired pitcher.
## A list of possible pid values for pitchers can be provided using the listIDs
## function which is provided below.
def pitcherData(pid, directory):
    url = "https://baseballsavant.mlb.com/statcast_search/csv?hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2017%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&player_lookup%5B%5D=PLAYERID&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&player_id=PLAYERID"
    url = url.replace("PLAYERID", pid)
    path = directory + pid + '.csv'
    res = requests.get(url)
    tdat = res.text
    test = io.StringIO(tdat)
    dat = pandas.read_csv(test)
    dat.to_csv(path, index = False)

## This obtains data for Danny Duffy    
pitcherData("518633", "../data/")

def batterData(pid, directory):
    url = "https://baseballsavant.mlb.com/statcast_search/csv?hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2017%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&player_id=PLAYERID"
    url = url.replace("PLAYERID", pid)
    path = directory + pid + '.csv'
    res = requests.get(url)
    tdat = res.text
    test = io.StringIO(tdat)
    dat = pandas.read_csv(test)
    dat.to_csv(path, index = False)


## Generate a list of pitcher id values that can be used.
## Has only been tested with pitchers
def listIDs(ptype):
    url = "https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2017%7C&hfSit=&player_type=PLAYERTYPE&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0"
    url = url.replace("PLAYERTYPE", ptype)
    res = requests.get(url)
    comm = re.compile("<!--|-->") 
    soup = bs4.BeautifulSoup(comm.sub("", res.text), 'lxml')
    tables = soup.findAll('table')
    data_rows = tables[0].findAll('tr')
    tmpdict = dict()    
    for r in range(1, len(data_rows)):    
        tmp = data_rows[r] 
        tmp = tmp.findAll("td", {"class" :"player_name"})
        if len(tmp) > 0:
            playerid = tmp[0].get("id")
            pid = playerid.split("_")[1]
            playername = tmp[0].contents  
            tmpdict[r] = pandas.DataFrame({"Name": playername, "id": pid})
    
    pdict = pandas.concat(tmpdict)
    return(pdict)
    
plist = listIDs("pitcher")

## Runs over a list of pitchers and their ids to save .csv files.
def pullPitchingList(plist, directory):
    for p in range(0, len(plist)):
        idval = str(plist.iloc[p, 1])
        pitcherData(idval, directory)

## Ensure that ../data/pitchers/ exists!
pullPitchingList(plist, "../data/pitchers/")

## Now do batters
blist = listIDs("batter")

def pullBattingList(blist, directory):
    for b in range(0, len(blist)):
        idval = str(blist.iloc[b, 1])
        batterData(idval, directory)
        
## Ensure that ../data/batters/ exists!     
pullBattingList(blist, "../data/batters/")
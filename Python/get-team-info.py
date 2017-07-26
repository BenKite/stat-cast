## Ben Kite

import pandas, numpy
import requests, bs4
import re, os

## Pulls a single table from a url provided by the user.
## The desired table should be specified by tableID.
## This function is used in all functions that do more complicated pulls.
def pullTable(url, tableID):
    res = requests.get(url)
    ## Work around comments
    comm = re.compile("<!--|-->")
    soup = bs4.BeautifulSoup(comm.sub("", res.text), 'lxml')
    tables = soup.findAll('table', id = tableID)
    data_rows = tables[0].findAll('tr')
    data_header = tables[0].findAll('thead')
    data_header = data_header[0].findAll("tr")
    data_header = data_header[0].findAll("th")
    game_data = [[td.getText() for td in data_rows[i].findAll(['th','td'])]
        for i in range(len(data_rows))
        ]
    data = pandas.DataFrame(game_data)
    header = []
    for i in range(len(data.columns)):
        header.append(data_header[i].getText())
    data.columns = header
    data = data.loc[data[header[0]] != header[0]]
    data = data.reset_index(drop = True)
    return(data)
## For example:
## url = "http://www.baseball-reference.com/teams/KCR/2016.shtml"
## pullTable(url, "team_batting")

## Pulls data summarizing the season performance of all players on the
## team provided for the given year.
## The table type argument must be one of five possibilities:
## "team_batting"
## "team_pitching"
## "standard_fielding"
## "players_value_batting"
## "players_value_pitching"
def pullPlayerData (team, year, tabletype):
    url = "http://www.baseball-reference.com/teams/" + team + "/" + str(year) + ".shtml"
    data = pullTable(url, tabletype)
    data = data[data.Name.notnull()]
    data = data.reset_index(drop = True)
    names = data.columns
    for c in range(0, len(names)):
        replacement = []
        if type (data.loc[0][c]) == str:
            k = names[c]
            for i in range(0, len(data[k])):
                p = data.loc[i][c]
                xx = re.sub("[#@*&^%$!]", "", p)
                xx = xx.replace("\xa0", "_")
                xx = xx.replace(" ", "_")
                replacement.append(xx)
            data[k] = replacement
    data["Team"] = team
    data["Year"] = year
    return(data)




teams = ['ATL', 'ARI', 'BAL', 'BOS', 'CHC', 'CHW', 'CIN', 'CLE', 'COL', 'DET',
         'KCR', 'HOU', 'LAA', 'LAD', 'MIA', 'MIL', 'MIN', 'NYM', 'NYY', 'OAK',
         'PHI', 'PIT', 'SDP', 'SEA', 'SFG', 'STL', 'TBR', 'TEX', 'TOR', 'WSN']

for t in teams:
    tmp = pullPlayerData(t, 2017, "team_pitching")
    tmpb = pullPlayerData(t, 2017, "team_batting")
    tmp.to_csv("../data/teaminfo/" + t + "_pitching.csv")
    tmpb.to_csv("../data/teaminfo/" + t + "_batting.csv")


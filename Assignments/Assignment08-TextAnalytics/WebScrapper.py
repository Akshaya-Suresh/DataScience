#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr 15 22:33:55 2018

@author: akshayasuresh
"""

from bs4 import BeautifulSoup
import urllib.request
import pandas as pd
#Scraping Google Chromecast reviews from Best Buy using Beautiful Soup
data = []
df = pd.DataFrame(columns=['Author','Ratings','Title','Description'])
file_name = "/Users/akshayasuresh/MS_NEU/Spring2018/ADS/ads_assignments/data.csv"
#writer = csv.writer(open('/Users/akshayasuresh/MS_NEU/Spring2018/ADS/ads_assignments/data.csv','wb'))
for i in range(1,251):
    url = ("https://www.bestbuy.com/site/reviews/google-chromecast-black/4397400?page="+ str(i))
    page = urllib.request.urlopen(url)
    soup = BeautifulSoup(page,"lxml")
    #print(soup.prettify())
    x = soup.find_all("li", {"class":"review-item"})
    for item in x:
        author_name =item.find_all("div",attrs = {"class":"author"})[0].text
        rating = item.find_all("span",attrs={"class":"c-review-average"})[0].text
        review_title = item.find_all("h4",attrs={"class":"col-md-9 col-sm-9 col-xs-12 title"})[0].text
        review_description = item.find_all("p",attrs={"class":"pre-white-space"})[0].text
        data.append((author_name,rating, review_title,review_description ))
        temp =[author_name,rating, review_title,review_description]
        df.loc[len(df)] = temp
        #df.append([author_name,rating,review_title,review_description])
        #data.append((a2,r2))
        #print(author_name,",",rating,",",review_title,",",review_description)
        #writer.writerow(author_name+","+rating+","+review_title+","+review_description)
df.to_csv(file_name, sep='\t', encoding='utf-8')
print("Complete")
#print(df['Author'])      
#for item in data:
    #print(item.index)
      
        

     
        
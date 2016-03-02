from datetime import datetime
import tweepy
import time
import csv

# Consumer keys and access tokens, used for OAuth
consumer_key = '8Z6WffhtUWPqhnfTaaKUcn9MO'
consumer_secret = 'wVtZbtbtSo4g7qDoIJ7ouxeIzvCIJhDpgAuoCkR3I28UiJhNbX'
access_token = '2900922484-iLrkJubjxtMYSnHuvxMMOBkVOYtl2aS8VFBzRV5'
access_token_secret = 'b4wbG1I7In6ynpmNuDFcmVXGzsjyS05FyYm9WgHROyblZ'

# OAuth process, using the keys and tokens
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

# Creation of the actual interface, using authentication
api = tweepy.API(auth)

# Open/Create a file to append data
csvFile1 = open('tweetsForFinalProject15B.csv', 'a')
csvFile2 = open('tweetsForFinalProjectQ1.csv', 'a')
csvFile3 = open('tweetsForFinalProjectQ2.csv', 'a')
csvFile4 = open('tweetsForFinalProjectQ3.csv', 'a')
csvFile5 = open('tweetsForFinalProjectQ4.csv', 'a')
csvFile6 = open('tweetsForFinalProject15A.csv', 'a')


#Use csv Writer
csvWriter1 = csv.writer(csvFile1)
csvWriter2 = csv.writer(csvFile2)
csvWriter3 = csv.writer(csvFile3)
csvWriter4 = csv.writer(csvFile4)
csvWriter5 = csv.writer(csvFile5)
csvWriter6 = csv.writer(csvFile6)

search_cursor = tweepy.Cursor(api.search,
                              q="Packers OR Patriots OR #gbvsne",
                              since="2014-11-30",
                              until="2014-12-02",
                              lang="en").items()

while True:
    try:
        tweet = search_cursor.next()
        time_stamp_hour = (int(datetime.strftime(tweet.created_at,'%I')) - 5)
        time_stamp_min_second = int(datetime.strftime(tweet.created_at,'%M%S'))
        combined_time_stamp = int(str(time_stamp_hour) + str(time_stamp_min_second))
        
        if(int(datetime.strftime(tweet.created_at,'%d')) == 30):
            if((combined_time_stamp >= 41000) and (combined_time_stamp <= 42500)):
               print(str(combined_time_stamp)+' -> '+tweet.text+'\n')
               csvWriter1.writerow([datetime.strftime(tweet.created_at,'%I%M%S'), tweet.text.encode('utf-8')])
    
            elif((combined_time_stamp > 42500) and (combined_time_stamp <= 50800)):
                print(str(combined_time_stamp)+' -> '+tweet.text+'\n')
                csvWriter2.writerow([datetime.strftime(tweet.created_at,'%I%M%S'), tweet.text.encode('utf-8')])
    
            elif((combined_time_stamp > 50800) and (combined_time_stamp <= 60000)):
                print(str(combined_time_stamp)+' -> '+tweet.text+'\n')
                csvWriter3.writerow([datetime.strftime(tweet.created_at,'%I%M%S'), tweet.text.encode('utf-8')])
    
            elif((combined_time_stamp > 60000) and (combined_time_stamp <= 64700)):
                print(str(combined_time_stamp)+' -> '+tweet.text+'\n')
                csvWriter4.writerow([datetime.strftime(tweet.created_at,'%I%M%S'), tweet.text.encode('utf-8')])
    
            elif((combined_time_stamp > 64700) and (combined_time_stamp <= 72700)):
                print(str(combined_time_stamp)+' -> '+tweet.text+'\n')
                csvWriter5.writerow([datetime.strftime(tweet.created_at,'%I%M%S'), tweet.text.encode('utf-8')])
    
            elif((combined_time_stamp > 72700) and (combined_time_stamp <= 74200)):
                print(str(combined_time_stamp)+' -> '+tweet.text+'\n')
                csvWriter6.writerow([datetime.strftime(tweet.created_at,'%I%M%S'), tweet.text.encode('utf-8')])
        else:
            print("NOT NOVEMBER 30th")
    
    except tweepy.TweepError:
        print('\n******** TIME TO SLEEP ********\n\n')
        time.sleep(60 * 15)
        continue
    except StopIteration:
        break

csvFile1.close()
csvFile2.close()
csvFile3.close()
csvFile4.close()
csvFile5.close()
csvFile6.close()
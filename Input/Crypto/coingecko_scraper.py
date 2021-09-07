# -*- coding: utf-8 -*-
"""
Created on Fri Jan  8 18:48:17 2021
@author: Vanessa Guarino
"""

from pycoingecko import CoinGeckoAPI
import pandas as pd
import time
from datetime import datetime
from functools import reduce

data_type = {'prices':'prices', 
             'market caps': 'market_caps', 
             'total volumes': 'total_volumes'}

# update year, month and date
from_time = datetime(2021, 5, 26, 0, 0).timestamp()
to_time = datetime(2021, 7, 12, 0, 0).timestamp()

class APIwrapper: 
    
    def __init__(self):
        self.socket = CoinGeckoAPI()
        
    def mktcap_ord_api(self, *args): 
        return self.socket.get_coins_markets(*args)
                
    def coin_api(self, tries, delay, backoff, *args): 
        for n in range(tries):
            try: 
                return self.socket.get_coin_market_chart_range_by_id(*args)
            
            except (KeyboardInterrupt, SystemExit):
                raise
                
            except Exception as e:
                time.sleep(delay)
                delay *= backoff
                print(f"{e} occurred. New attempt in {delay} seconds")
                continue 
        raise NameError(f"Failed within {tries} attempts")

def catch(iteration, coin_id, tries = 15, handle = lambda e: e,*args,**kwargs): 
    for n in range(tries):
        try: 
            return iteration
    
        except (KeyboardInterrupt, SystemExit):
            raise
    
        except Exception as e:
            print(f"{handle(e)}, occurred for {coin_id}. New attempt in course.")
            time.sleep(10)
            continue
    raise NameError(f"Failed within {tries} attempts")
    
def get_top_coins_id(api):  
    top_100_coins = api.mktcap_ord_api('usd')[:100] #top 100
    coin_ids = [dic['id'] for dic in top_100_coins]
    coin_symbols = [dic['symbol'] for dic in top_100_coins] 
    return coin_ids, coin_symbols

def create_dataframe(variables, symbol, timeseries, data_type): 
    dataframe = pd.DataFrame(variables)
    dataframe.columns = [symbol]
    dataframe["Datetime"] = timeseries
    dataframe["Datetime"] = pd.to_datetime(dataframe["Datetime"])
    dataframe = dataframe.set_index(["Datetime"])
    dataframe = dataframe.median(level=0)
    dataframe = dataframe.applymap('{:.6f}'.format) if data_type == "prices" else dataframe.applymap('{:.2f}'.format)
    return dataframe 

def convert_var_to_df(coin, coin_id, symbol, data_type):    
    n_var = [len(val) for key, val in coin.items()][0]  
    variables = [catch(coin[data_type][var][1], coin_id) for var in range(n_var)]
    timeseries = [catch(datetime.fromtimestamp(coin[data_type][var][0]/1000).strftime("%Y-%m-%d"), coin_id) for var in range(n_var)]    
    dataframe = create_dataframe(variables, symbol, timeseries, data_type)
    
    print(f"{coin_id} successfully downloaded.")
    time.sleep(3)
    
    return dataframe, timeseries

def get_coins_dataframe_timeseries(api, coin_ids, symbols, data_type, from_time, to_time):
    crypto_dic = [api.coin_api(10, 1, 2, coin,'usd', from_time, to_time) for _, coin in enumerate(coin_ids)]
    frames, ts = zip(*(convert_var_to_df(crypto_dic[idx], coin, symbols[idx], data_type) for idx, coin in enumerate(coin_ids)))
    minim_date = [min(t) for t in ts]
    maxim_date = [max(t) for t in ts]
    return frames, minim_date, maxim_date

def main():
    
    api = APIwrapper()  
    coin_ids, coin_symbols = get_top_coins_id(api) 
    frames, minim_date, maxim_date = get_coins_dataframe_timeseries(api, coin_ids, coin_symbols, data_type['prices'], 
                                                                      from_time = str(from_time), to_time = str(to_time))   
    index = [pd.date_range(start= min(minim_date), end= max(maxim_date)).tolist()]
    match_df = pd.DataFrame(index = index).rename_axis("Datetime")
    filter_type_frames = match_df.merge(reduce(lambda left,right: pd.merge(left, right, on = "Datetime", how="outer"), frames), how="outer", on ="Datetime")
    filter_type_frames.to_csv(datetime.fromtimestamp(from_time).strftime("%Y%m%d") + " " +
                             datetime.fromtimestamp(to_time).strftime("%Y%m%d") + " " + data_type['prices'] + " cryptos.csv")

                                            
if __name__== "__main__":
  main()


"""
Spyder Editor

This is a temporary script file.
"""


from bs4 import BeautifulSoup
import requests
import pandas as pd

sg_ott=pd.DataFrame(columns=['name','sg_ott','sg_ott_rank'])
sg_aptg=pd.DataFrame(columns=['name','sg_aptg','sg_aptg_rank'])
sg_artg=pd.DataFrame(columns=['name','sg_artg','sg_artg_rank'])
sg_putt=pd.DataFrame(columns=['name','sg_putt','sg_putt_rank'])
sg_tot=pd.DataFrame(columns=['name','sg_tot','sg_tot_rank'])
drd=pd.DataFrame(columns=['name','drd','drd_rank'])
dra=pd.DataFrame(columns=['name','dra','dra_rank'])
gir=pd.DataFrame(columns=['name','gir','gir_rank'])
gof=pd.DataFrame(columns=['name','gof','gof_rank'])
pth=pd.DataFrame(columns=['name','pth','pth_rank'])
ssv=pd.DataFrame(columns=['name','ssv','ssv_rank'])
scr=pd.DataFrame(columns=['name','scr','scr_rank'])
pthatg=pd.DataFrame(columns=['name','pthatg','pthatg_rank'])
opp=pd.DataFrame(columns=['name','opp','opp_rank'])
ppr=pd.DataFrame(columns=['name','ppr','ppr_rank'])
pmd=pd.DataFrame(columns=['name','pmd','pmd_rank'])
apmd=pd.DataFrame(columns=['name','apmd','apmd_rank'])
sc_scr=pd.DataFrame(columns=['name','sc_scr','sc_scr_rank'])
sc_brd=pd.DataFrame(columns=['name','sc_brd','sc_brd_rank'])
sc_btb=pd.DataFrame(columns=['name','sc_btb','sc_btb_rank'])
rk_money=pd.DataFrame(columns=['name','rk_money','rk_money_rank'])
rk_tt=pd.DataFrame(columns=['name','rk_tt','rk_tt_rank'])
rk_fedex=pd.DataFrame(columns=['name','rk_fedex','rk_fedex_rank'])

golf_stat=pd.read_csv('golf_stats.csv',sep=',',index_col=False)

for i in range(0,len(golf_stat)):
    
    url = golf_stat.loc[i,'url']

    golf_text = requests.get(url).text
    golf_soup = BeautifulSoup(golf_text)
    #print golf_soup

    table = golf_soup.find_all('tbody')
    
    if i==0:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            #print rank
            #print name
            #print stat
            new=pd.DataFrame([[name,stat,rank]],columns=['name','sg_ott','sg_ott_rank'])
            sg_ott = pd.concat([sg_ott,new])
        #sg_ott.to_csv('sg_ott.csv', sep=',', encoding='utf-8')
    
    elif i==1:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','sg_aptg','sg_aptg_rank'])
            sg_aptg = pd.concat([sg_aptg,new])
        #sg.ott.to_csv('sg.ott.csv', sep=',', encoding='utf-8')
        full=pd.merge(sg_ott,sg_aptg,on='name',how='outer')
        
    elif i==2:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','sg_artg','sg_artg_rank'])
            sg_artg = pd.concat([sg_artg,new])
        full=pd.merge(full,sg_artg,on='name',how='outer')
        
    elif i==3:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','sg_putt','sg_putt_rank'])
            sg_putt = pd.concat([sg_putt,new])
        full=pd.merge(full,sg_putt,on='name',how='outer')
            
    elif i==4:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','sg_tot','sg_tot_rank'])
            sg_tot = pd.concat([sg_tot,new])
        full=pd.merge(full,sg_tot,on='name',how='outer')
            
    elif i==5:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','drd','drd_rank'])
            drd = pd.concat([drd,new])
        full=pd.merge(full,drd,on='name',how='outer')
            
    elif i==6:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','dra','dra_rank'])
            dra = pd.concat([dra,new])
        full=pd.merge(full,dra,on='name',how='outer')
            
    elif i==7:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','gir','gir_rank'])
            gir = pd.concat([gir,new])
        full=pd.merge(full,gir,on='name',how='outer')
            
    elif i==8:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','gof','gof_rank'])
            gof = pd.concat([gof,new])
        full=pd.merge(full,gof,on='name',how='outer')
            
    elif i==9:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','pth','pth_rank'])
            pth = pd.concat([pth,new])
        full=pd.merge(full,pth,on='name',how='outer')
            
    elif i==10:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','ssv','ssv_rank'])
            ssv = pd.concat([ssv,new])
        full=pd.merge(full,ssv,on='name',how='outer')
            
    elif i==11:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','scr','scr_rank'])
            scr = pd.concat([scr,new])
        full=pd.merge(full,scr,on='name',how='outer')
            
    elif i==12:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','pthatg','pthatg_rank'])
            pthatg = pd.concat([pthatg,new])
        full=pd.merge(full,pthatg,on='name',how='outer')
            
    elif i==13:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','opp','opp_rank'])
            opp = pd.concat([opp,new])
        full=pd.merge(full,opp,on='name',how='outer')
            
    elif i==14:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','ppr','ppr_rank'])
            ppr = pd.concat([ppr,new])
        full=pd.merge(full,ppr,on='name',how='outer')
            
    elif i==15:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','pmd','pmd_rank'])
            pmd = pd.concat([pmd,new])
        full=pd.merge(full,pmd,on='name',how='outer')
            
    elif i==16:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','sc_scr','sc_scr_rank'])
            sc_scr = pd.concat([sc_scr,new])
        full=pd.merge(full,sc_scr,on='name',how='outer')
            
    elif i==17:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','sc_brd','sc_brd_rank'])
            sc_brd = pd.concat([sc_brd,new])
        full=pd.merge(full,sc_brd,on='name',how='outer')
            
    elif i==18:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','rk_money','rk_money_rank'])
            rk_money = pd.concat([rk_money,new])
        full=pd.merge(full,rk_money,on='name',how='outer')
            
    elif i==19:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','rk_tt','rk_tt_rank'])
            rk_tt = pd.concat([rk_tt,new])
        full=pd.merge(full,rk_tt,on='name',how='outer')
            
    elif i==20:
        for row in table[0].find_all('tr'):
            stats = row.find_all('td')
            rank = stats[0].string
            name = stats[2].a.string
            stat = stats[4].string
            new=pd.DataFrame([[name,stat,rank]],columns=['name','rk_fedex','rk_fedex_rank'])
            rk_fedex = pd.concat([rk_fedex,new])
        full=pd.merge(full,rk_fedex,on='name',how='outer')
            


full.to_csv('full_golf.csv', sep=',', encoding='utf-8')
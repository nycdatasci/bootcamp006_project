##################################
##       Amy Tzu-Yu Chen        ##
##     Web Scraping Project     ##
##################################
library(stringr)
library(car)

#data cleaning: user profiles
user <- read.delim("https://raw.githubusercontent.com/amy17519/FredorangeMuggers/master/user.csv",stringsAsFactors = FALSE)
user<- user[,c(8,2,3,4,5,6,7)]

# delete extraneous characters in PercentageOwned
user[,"PercentageOwned"]<-str_extract(user[,"PercentageOwned"], "[[:digit:]]+")

#remove duplicate user profiles
user<-user[!duplicated(user),]

#combine and rename levels
user[which(is.na(user$Country)),3]<-'Undefined Country'
user$Country<-tolower(user$Country)
user$City<-tolower(user$City)
user$Country<-recode(user$Country,
                    "c('81250 r.o.c taiwan','台灣','taiwan( r.o.c)','taichung','roc')='taiwan';
                     c('156013 russia ','russia ','россия','russian federation',
                    'russia / austria')='russia';
                    c('america','arizona','california','ap','美國','wa','washington','wisconsin',
                    'アメリカ合衆国','vereinigte staaten','usa / philippines','usa/germany','va',
                    'usa ','usa','us ','united states of america ','united states of america',
                    'texas','united states ','us','new mexico','oh.','nm',
                    'united stated','u.s.a.','u.s.a','u.s. ','u.s.','co','united states',
                    'u.s','nv','ny','north  carolina ','new jersey','los angeles',
                    'florida usa','florida','rhode island','hawaii','ga')='usa';
                    c('p.r.c','中国')='china';
                    c('norwar')='norway';
                    c('phils','philippines ','pilipinas','ph')='philippines';
                    c('uk and belgium','uk ','england uk','united kingdom','united kingdon',
                    'united kingdom ','england','england  ','england and qatar','kent',
                    'wales','england ','scotland, uk','scotland','northern ireland')='uk';
                    c('united arab emirates ','uae ','uae','ua')='united arab emirates';
                    c('kingdom of saudi arabia')='saudi arabia';
                    c('hong kong ')='hong kong';
                    c('france ')='france';
                    c('italia')='italy';
                    c('españa')='spain';
                    c('switzerland ')='switzerland'; 
                    c('shizuoka ','shizuoka')='japan';
                    c('thailand ')='thailand'; 
                    c('holland','the netherland','the netherlands','netherlands ',
                    'nederland ','nederland','the netherlands ','rotterdam')='netherlands';
                    c('belgië ','belguim')='belgium';
                    c('brasil')='brazil';
                    c('polska')='poland';
                    c('singapore ')='singapore';
                    c('south africa ')='south africa';
                    c('australia ')='australia';
                    c('turkiye')='turkey';
                    c('sverige')='sweden';
                    c('brunei darussalam')='brunei';
                    c('canada ','canada   ','ontario')='canada';
                    c('украина','uraine')='ukraine';
                    c('德国','wendlingen','germany ','gernamy','gr',
                    'deutschalnd','deutschland','de')='germany';
                    c('viet nam','việt nam')='vietnam';
                    c('south korea')='korea';
                    c('mexico ','méxico','san luis potosí ')='mexico';
                    c('malaysia`','malaysia ','malsysia','selangor ')='malaysia';
                    c('das')='undefined country'")

user1<-user[which(!is.na(user$City) & user$Country=="undefined country"),]
user2<-setdiff(user, user1)

user1$Country<-user1$City
user1$Country<-recode(user1$Country,
                     "c('washington, dc','washington dc','waconia, mn','usa, nj','usa ',
                        'us- northeast','us','unitedstates','united states of america',
                        'united states','u.s.a.','u.s.a','texas','sumner, wa','st louis',
                        'so california','seattle, wa','seattle','san mateo, ca','qdf',
                        'san francisco, ca','san francisco','san diego','raleigh','arlington',
                        'pittsburgh, pa','pembroke','oklahoma city','oakland, ca','kirkland',
                        'new york, ny','new jersey','mclean, va','los angeles','las vegas',
                        'hood river','houston','fort worth tx','florida','denver','dallas',
                        'co','chicago','ca, usa','boston','austin','augusta ga','atlanta')='usa';
                     c('wales','united kingdom','london','kent','england, uk','england')='uk';
                     c('melbourne ')='australia';
                     c('beijing')='china';
                     c('germany ','deutschland')='germany';
                     c('italia')='italy';
                     c('dubai, uae')='united arab emirates';
                     c('okinawa')='japan';
                     c('españa ')='spain';
                     c('south korea')='korea';
                     c('taipei')='taiwan';
                     c('philppines')='philippines';
                     c('polen')='poland';
                     c('rio de janeiro','brasil')='brazil';
                     c('indonesia, singapore')='indonesia';
                     c('vancouver','toronto','canada ','calgary')='canada';
                     c('salzburg')='austria';
                     c('istanbul')='turkey';
                     c('the netherlands','nederland','amsterdam')='netherlands'")

user<-rbind(user1,user2)
user<-user[-2,]

#data cleaning: mug profiles
mug <- read.csv("https://raw.githubusercontent.com/amy17519/FredorangeMuggers/master/mug.csv",stringsAsFactors = FALSE)

#extract mug titles (there were extraneous elements in each string)
temp<-strsplit(mug[,'Name'], "— «")
temp<-lapply( temp, '[[', 2 )
vec<-unlist(temp)
temp<-strsplit(vec, "»]")
temp<-lapply( temp, '[[', 1 )
mug[,'Name']<-unlist(temp)

mug<-mug[!duplicated(mug),]

library(dplyr)
mug <- mug %>% mutate(City = ifelse(is.na(City),"Various",City)) 
detach(package:dplyr, unload=TRUE)

mug$City<-recode(mug$City,
             "c('----','Varioius','Varioud','various','Vaious','N/A',
                'Indonesia','Japan','korea','Korea','Limited Online','n/a',
                'USA','South Korea','Spain','Portugal Country Mug','Portugal',
                'Philippines','Peru','Others','Country Mug','Country Tumbler',
                'China','Brazil','Netherlands','POLAND','Romania','Turkey',
                'Saudi Arabia','Scotland','Taiwan','Sweden','Tumbler')='Various';
              c('Pingdong')='Pingtung';
              c('Saint Petersburg')='St. Petersburg';
              c('Shang hai')='Shanghai';
              c('Seattle, WA')='Seattle';
              c('Chiangmai')='Chiang Mai';
              c('Montréal')='Montreal';
              c('queretaro')='Queretaro';
              c('Rio De Janeiro')='Rio de Janeiro';
              c('Osnabrück')='Osnabruck';
              c('State - Florida')='Florida';
              c('State Texas')='Texas';
              c('Washington D. C.','Washington DC')='Washington D.C.';
              c('Wrocław')='Wroclaw';
              c('Yamanashi, Sizuoka','YAMANASHI')='Yamanashi';
              c('Zama or Yokota')='Zama';
              c('taichung')='Taichung';
              c('Tagaytay City')='Tagaytay';
              c('Sharn El Sheikh','Sharm El Sheikh')='Sharm el-Sheikh';
              c('Futako-Tamagawa Tokyo','Ginza, Tokyo','Harajuku, Tokyo',
                'Haneda Airport, Tokyo','Harajuku 4 Stores','Harajuku','Roppongi, Tokyo',
                'Meguro, Tokyo','tokyo','Shibuya, Tokyo','Asakusa, Tokyo')='Tokyo';
              c('Shizuoka, Yamanashi')='Shizuoka';
              c('hawaii')='Hawaii';
              c('Québec')='Quebec';
              c('Ho Chi Minh City')='Ho Chi Minh';
              c('Kuala Tumbler')='Kuala Lumpur';
              c('Kuta, Bali')='Kuta';
              c('Hong kong','Hongkong')='Hong Kong';
              c('Anaheim, CA','Newport Beach, CA')='Los Angeles';
              c('Minneapolis / St. Paul','Minneapolis','St. Paul')='Twin Cities';
              c('Berkeley, CA')='Berkeley';
              c('St Louis')='St. Louis';
              c('New York City')='New York'")

mug$Edition<-recode(mug$Edition,
                    "c('0 users','1 users','10 users','12 users','2 users','3 users',
                    '6 users','7 users','8 users','9 users','Other...')='Undefined Series';
                     c('Designed exclusively for Starbucks without a year')='Exclusive Design';
                     c('Giant Mugs')='02 Skyline Series City Giant Mugs';
                     c('94 Collectors Series City Mugs')='94 CITY MUG Collector Seris'")

#Add new variables: Rariry, Willingness to trade, and popularity
mug$Popularity<-mug$Seeker/max(mug$Seeker)
mug$WillingnessToTrade<-(mug$Trader/(mug$Owner+1))/max((mug$Trader/(mug$Owner+1)))
mug$Rarity<-mug$Owner/max(mug$Owner)


write.csv(user,file = 'FredorangeUser.csv')
write.csv(mug, file = 'FredorangeMug.csv')
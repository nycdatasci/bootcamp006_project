library(httr)
library(rvest)
require(Roxford)

linksactresses = 'http://www.imdb.com/list/ls058011111/'
out = read_html(linksactresses)
images = html_nodes(out, '.zero-z-index')
imglinks = html_nodes(out, xpath = "//img[@class='zero-z-index']/@src") %>% html_text()
for (i in c(1:9))
{ 
  infix=paste('?start=',i,'01&view=detail&sort=listorian:asc',sep="")
  linksactresses=paste('http://www.imdb.com/list/ls058011111/',infix,sep="")
  out = read_html(linksactresses)
  images = html_nodes(out, '.zero-z-index')
  templink=html_nodes(out, xpath = "//img[@class='zero-z-index']/@src") %>% html_text()
  imglinks=c(imglinks,templink)
}


#After renaming, it would be easier for us to look up the image files in the following steps.
#Then, as what we did in the previous project, we would iterate over every image file inside the directory
#and send them to the Microsoft API for facial analysis 

#Now lets set up the dataset and to store the image and build up the connection to the Microsoft API
require(Roxford)

facekey = '8fd43d9ea23045ed86f365eed9500ffc'
emotionkey = '355b73f0ac794563adf7898511373448'



finalframe=data.frame()

for (i in imglinks)
{
  if (i %in% finalframe[['link']])
  {
    next
  }
  tempface=getFaceResponseURL(i, facekey)
  if (length(tempface)!=14)
  {
    next
  }
  tempemotion=getEmotionResponseURL(i, emotionkey)
  if (length(tempemotion)!=12)
  {
    next
  }
  tempemotion=data.frame(tempemotion)
  tempface=data.frame(tempface)
  tempface['link']=i
  tempemotion=tempemotion[grep('scores',names(tempemotion))]
  newframe=cbind(tempface,tempemotion)
  finalframe=rbind(finalframe,newframe)
}



saveRDS(finalframe, file="facefeature.rds")

library(dplyr)
facelist_male = "https://api.projectoxford.ai/face/v1.0/facelists/list_gender_male"
facelist_female= "https://api.projectoxford.ai/face/v1.0/facelists/list_gender_female"
facelist_hair_y="https://api.projectoxford.ai/face/v1.0/facelists/list_hair_y"
facelist_hair_n="https://api.projectoxford.ai/face/v1.0/facelists/list_hair_n"
facelist_age_2="https://api.projectoxford.ai/face/v1.0/facelists/list_age_2"
facelist_age_3="https://api.projectoxford.ai/face/v1.0/facelists/list_age_3"
facelist_age_4="https://api.projectoxford.ai/face/v1.0/facelists/list_age_4"
facelist_list=c(facelist_male,facelist_female,facelist_hair_y,facelist_hair_n,facelist_age_2,facelist_age_3,facelist_age_4)
for (i in facelist_list)
{
  mybody=list(name=i)
  faceLIST = PUT(
    url = i,
    content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
    body = mybody,
    encode = 'json'
  )
  print(content(faceLIST))
}

facelist_image_male=unique(select(filter(finalframe,faceAttributes.gender=='male'),link))[[1]]
facelist_image_female=unique(select(filter(finalframe,faceAttributes.gender=='female'),link))[[1]]
facelist_image_hair_y=unique(select(filter(finalframe,faceAttributes.facialHair.beard==0),link))[[1]]
facelist_image_hair_n=unique(select(filter(finalframe,faceAttributes.facialHair.beard!=0),link))[[1]]
#finalframe['faceAttributes.age']=finalframe[['faceAttributes.age']]
facelist_age_2=unique(filter(finalframe,(faceAttributes.age>=20) & (faceAttributes.age<30))[['link']])
facelist_age_3=unique(filter(finalframe,(faceAttributes.age>=30) & (faceAttributes.age<40))[['link']])
facelist_age_4=unique(filter(finalframe,(faceAttributes.age>=40))[['link']])

image_list=list(facelist_image_male,facelist_image_female,facelist_image_hair_y,facelist_image_hair_n,facelist_age_2,facelist_age_3,facelist_age_4)

dataframe_list=list()
for (i in c(1:length(facelist_list)))
{
  URL.face = facelist_list[i]
  userdata=0
  tempframe_list=list()
  for (j in image_list[[i]])
  {
    face.uri = paste(
      URL.face,'/persistedFaces?userData=',
      userdata,
      sep = ""
    )
    face.uri = URLencode(face.uri)
    mybody = list(url = j )
    
    faceLISTadd = POST(
      url = face.uri,
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
      body = mybody,
      encode = 'json'
    )
    userdata=userdata+1
    if (is.null(content(faceLISTadd)$persistedFaceId))
    {
      next
    }
    else
    {
      tempframe_list=rbind(tempframe_list,list(link=j,perid=content(faceLISTadd)$persistedFaceId))
    }
    
    tempframe_list=data.frame(tempframe_list)
    
  }
  
  dataframe_list=c(dataframe_list,list(tempframe_list))
}

saveRDS(dataframe_list, file="dataframe_list.rds")


finalframe['faceRectangle.width']<-as.numeric(as.vector(finalframe[['faceRectangle.width']]))
finalframe['faceRectangle.height']<-as.numeric(as.vector(finalframe[['faceRectangle.height']]))



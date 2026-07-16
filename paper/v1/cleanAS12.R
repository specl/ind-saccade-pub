cleanAS12=function(){
    link="https://raw.githubusercontent.com/amthapar/data-east/main/antiSaccade/as12/as12All.csv"
    dat=read.csv(url(link))
    dat$sub=as.integer(as.factor(dat$sub))
    goodParticipants= (dat$sub != 26)
    goodBlock=dat$blk>1
    clean=dat[goodParticipants &goodBlock,]
    return(clean)}



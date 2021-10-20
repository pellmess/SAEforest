if(mymeth=="unc"){
  for(i in 1:areas){
    #MQ
    ysd<-my.ys[myregioncode==ar[i]]
    x.sd<-my.x.s[myregioncode==ar[i],]
    Eds.mq<-x.sd%*%beta.mq[,i]
    res.d.mq<-ysd-Eds.mq
    res.mq<-c(res.mq,res.d.mq)
  }

  for(i in 1:areas){
    #MQ
    y.sd<-my.ys[myregioncode==ar[i]]
    x.sd<-my.x.s[myregioncode==ar[i],]
    Inc.mq<-x.sd%*%beta.mq[,i]
    A<-matrix(rep(res.mq,pop.size[i]),nrow=pop.size[i],ncol=sum(sample.size),byrow=TRUE)
    x.rd<-my.X.pop[myregioncodepop==ar[i],]
    Inc.pred<-x.rd%*%beta.mq[,i]
    A<-A+c(Inc.pred)
    AA<-A<myz[[i]]
    B<-A
    B[which(B<0)]<-0
    BB<-((myz[[i]]-B)/myz[[i]])*AA
    f.MQ.0[i]<-mean(apply(AA,1,mean))
    f.MQ.1[i]<-mean(apply(BB,1,mean))
  }#i ends here

  f.MQ.0[which(f.MQ.0>1)]<-1
  f.MQ.1[which(f.MQ.1>1)]<-1

install.packages("igraph")
library(igraph)
init.igraph<-function(data,dir=F,rem.multi=T)
{
  labels<-union(unique(data[,1]),unique(data[,2]))
  ids<-1:length(labels);names(ids)<-labels
  from<-as.character(data[,1]);to<-as.character(data[,2])
  edges<-matrix(c(ids[from],ids[to]),nc=2)
  g<-graph.empty(directed = dir)
  g<-add.vertices(g,length(labels))
  V(g)$label=labels
  g<-add.edges(g,t(edges))
  if (rem.multi)
  {
    E(g)$weight<-count.multiple(g)
    g<-simplify(g,remove.multiple = TRUE,
                remove.loops = TRUE,edge.attr.comb = "mean")
  }
  g
}
#init.igraph参数说明：
#data,是两列关系数据，只能两列，而且要同等长度；
#dir，逻辑值，T代表有向图，F无向图；
#rem.multi，逻辑，T删除重复变量并更新线权重weight，F不删除并且线权重为1。

dat <- data.frame(node1 = c("A","A","A","B","B","C","E"),node2 = c("B","C","E","D","F","E","F"))
dat

g <- init.igraph(dat,dir=F,rem.multi=T)
g

#看着跟原数据对不上，其实是函数根据数据出现顺序对其进行了重编码，A-1，B-2，C-3，E-4，D-5，F-6

#点度中心度——点出度、点入度、相对点中心度、点度频率
degree(g,mode="total")     #mode=in点入度；out=点出度；total点度中心度，三者统称绝对点中心度

#结果对应的是ABCEDF的度数
degree(g,normalized = T)   #相对点中心度=绝对点中心度/最大度数

#但根据洒家多次试验，实际除以的数是(结点总数-1)，在这里即5
degree.distribution(g)  #点度频率每种点度数的个数/所有点个数（比如（1,2,4,4）点度数分别为1,1,0,2，密度是1/4，1/4,0/4,2/4）

#分别对应度数为0,1,2,3的点个数/结点总数，在此为(0,1,2,3)/6

#接近中心度
#该点与网络中其他点距离之和的倒数，越大说明越在中心，越能够很快到达其他点。与点度中心度不同的是，点度更强调某点的相对价值；而接近中心度是某点在网络的价值。
closeness(g,mode="in")  #mode="out"是有向图，默认是无向图为in，设置normalized = T为相对接近中心度
#结果对应的是ABCEDF的接近中心度
closeness(g,vids=which(V(g)$label=="E")) #仅输出指定结点E的接近中心度

#中间中心度——点的中心度以及线的中心度
#点的中心度，代表最短距离是否都经过该点，如果都经过说明这个点很重要。也是强调点与网络的价值，而且更能说明转发、中介的情况
betweenness(g,normalized = T)  #点的中心度：normalized = T代表相对数，默认值为F为绝对值，mode有Out和in分别代表有向和无向

edge.betweenness(g)            #线的中间中心度

#点的特征向量中心度——evcent
#如果某人四周都是大牛人，那么这个人也很牛逼，这个方法是根据相邻点的重要性来衡量该点的价值。首先计算邻接矩阵，然后计算邻接矩阵的特征向量
evcent(g,scale = F)$vector     ##scale=F没有归一化，T代表输出数据进行标准化，mode有Out和in分别代表有向和无向

#page.rank特征向量中心度，这个是谷歌开发的，对于一些搜索的竞价排名做的指标。原理跟特征向量中心度差不多
page.rank(g)$vector

#简单网络图形展示
plot(g,layout=layout.grid)

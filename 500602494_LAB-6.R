library(igraph)


edges <- read.table("E:/SAMIUL RYERSON/Winter2017/Social Media Analytics/LAB-5/twt_data.csv",header=T,sep=",")

g <- graph.edgelist(as.matrix(edges[,c(2,3)]),directed=T)


E(g)$time <- edges[,4]


#remove self-loops

g <- simplify(g, remove.multiple = FALSE, remove.loops = TRUE)


step <- 3

E(g)$weight <- ifelse(E(g)$time < step,1,0)


layout.old <- layout.graphopt(g,niter=100,spring.length=E(g)$weight)


png(file="E:/SAMIUL RYERSON/Winter2017/Social Media Analytics/LAB-6/pic/net%03d.png", width=1600,height=900,bg = "#F1F1F5")


total_time <- max(E(g)$time)

delta <- 0.5

nsteps <- max(E(g)$time)

for(step  in seq(3,total_time,delta)){  
  
  E(g)$weight <- ifelse(E(g)$time < step,1,0)
  
  E(g)$color <- ifelse(E(g)$time < step,"gray",rgb(0,0,0,0))
  
  V(g)$color <- ifelse(graph.strength(g)==0,rgb(0,0,0,0),"#3476A8")
  
  
  layout.new <- layout.graphopt(g,niter=10,start=layout.old,spring.length=E(g)$weight,max.sa.movement=1)
  
  
  plot(g,layout=layout.new,
       
       vertex.frame.color=V(g)$color,
       
       edge.width=1.5,
       
       asp=9/16,
       
       vertex.size= 1 + 1.5*log(graph.strength(g)),
       
       vertex.label=ifelse(degree(g)>10,V(g)$name,NA),
       
       vertex.label.color= "black",
       
       vertex.label.font=1,
       
       vertex.label.cex=2,
       
       edge.arrow.size=0.5,
       
       main="Dynamic Network Visualization"
       
  )
  
  layout.old <- layout.new
  
}

dev.off()

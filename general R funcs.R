# --- write the perceptual mapping func ---
pmap <- function(inp.mat, # input matrix with R rows and C colms
                 k=1)    # scaling factor
  
{
  
  
  
  # inp.mat = perception matrix with row and column headers
  # brands or units of analysis in rows and attributes in columns
  
  par(pty="m")  
  
  fit = prcomp(inp.mat, scale.=TRUE) # extract prin compts
  str(fit)
  
  plot(fit$rotation[,1:2], # use only top 2 prinComps       
       type ="n", 
       xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), # plot parms     
       main ="Perceptual map ") # plot title
  
  abline(h=0); abline(v=0) # build horiz & vert axes  
  attribnames = colnames(inp.mat)  
  brdnames = rownames(inp.mat) 
  
  # <-- insert attrib vectors as arrows--  
  for (i1 in 1:nrow(fit$rotation)){
    
    arrows(0, 0, x1=fit$rotation[i1,1]*fit$sdev[1], y1=fit$rotation[i1,2]*fit$sdev[2], col = "blue", lwd = 1.5);    
    text(x = fit$rotation[i1,1]*fit$sdev[1], y = fit$rotation[i1,2]*fit$sdev[2], labels = attribnames[i1], col = "blue", cex = 1.1)
    
  }
  
  # <--- make co-ords within (-1,1) frame #  
  fit1 = fit  
  fit1$x[,1] = fit$x[,1]/apply(abs(fit$x),2,sum)[1]  
  fit1$x[,2] = fit$x[,2]/apply(abs(fit$x),2,sum)[2]
  
  points(x = fit1$x[,1]*k, y = fit1$x[,2]*k, pch = 19, col ="red")  
  text(x = fit1$x[,1]*k, y = fit1$x[,2]*k, labels = brdnames,col ="black", cex = 1.1)
  
}	# JSM func ends

## Test example
officestar.percep = read.table("https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/JSM%20example%20data/officestar%20perceptual.txt", header=TRUE)
dim(officestar.percep);  officestar.percep
pmap(t(officestar.percep), k=1.5)
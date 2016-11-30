classify <- function(coord1, coord2, coord3, bandList) {# i like for user interfaces to walk you through what you need so not in the parameters
  # Load necessary library
  library(jpeg)
  #library(shiny)
  
	# ------------------------------------Part 1: Opening the images----------------------------------
	# First we'll have to read in the pictures. The bandList should be a
	# list of paths to the pictures we're classifying.

	# Create variables for the coordinates and the pictures
	
	# YOUR CODE HERE
	# The code below must be filled in to create the pictureList. This is a list
	# of pictures. Each picture was first read using readJPEG() (look this up!) and 
  # then put in the list.
  
  
  #fuction to get file loc'
  #100% not needed 
  
  #/home/ian/R/R-code examples/R-code examples/CHSMartianProject-master/r1.JPG
  
  imageLocations <- function()
  { 
    n <- readline(prompt="Enter location of image: ")
    return(n)
  }

  file1 <- imageLocations()

  file2 <- imageLocations()

  file3 <- imageLocations()

  file4 <- imageLocations()

  file5 <- imageLocations()

  file6 <- imageLocations()

  
  #END YOUR CODE
	pictureList <- list(readJPEG(file1),readJPEG(file2),readJPEG(file3),readJPEG(file4),readJPEG(file5),readJPEG(file6));
	#class(readJPEG(file1))
	#head(pictureList)






	# Test cases:
	if (length(pictureList) < 6) {
	  print("Not enough pictures in the pictureList")
	  print('Imported jpegs: \u2717')
	  return()
	}
	if (dim(pictureList[[1]])[1] != 256 || dim(pictureList[[1]])[2] != 256) {
	  print("Imported image does not have the correct dimensions.")
	  print('Imported jpegs: \u2717')
	  return()
	}
	print('Imported jpegs: \u2713')




	# -------------------------------------Part 2: Create the Stack-----------------------------------
	# Now we'll have to create a three dimensional array. Every layer
	# will be an individual band. When we access the matrix, we'll be
	# able to get a vector with a data point from each band.

	# YOUR CODE HERE
	# You will now fill in the code below to create the picture stack from each image in the
	# picture list. The dimensions will be 256 x 256 x 6. The stack is of type 'array'.
	# This is a tricky one, so feel free to talk to each other and consult the internet.

	
	
	
	pictureStack <- array(data = c(readJPEG(file1),readJPEG(file2),readJPEG(file3),readJPEG(file4),readJPEG(file5),readJPEG(file6)), dim = c(256, 256, 6));
  #pictureStack[1,1,]



	# Test cases:
	
	if (dim(pictureStack)[1] != 256 || dim(pictureStack)[2] != 256 || dim(pictureStack)[3] != 6) {
	  print("pictureStack does not have correct number of dimensions (should be 256 256 6)")
	  print('Created picture stack: \u2717')
	  return()
	}
	print('Created picture stack: \u2713')




	# --------------------------------Part 3: Create output image templates---------------------------
	# We'll need to create matrices to store the classified values afterwards.

	# Create variable with dimension of one slice of the picture stack
	dims <- list(x = 256, y = 256, z = 3)# YOUR CODE HERE;

	# Create class matrices with size dims
	class1 <- matrix(data = 0, nrow=dims$x,ncol = dims$y)
	class2 <- matrix(data = 0, nrow=dims$x,ncol = dims$y)
	class3 <- matrix(data = 0, nrow=dims$x,ncol = dims$y)
	#class3
	#colors()
	
	# Create final image stack for output. Each class will be its own color.
	red   <- matrix(data = 0, nrow=dims$x,ncol = dims$y)#rgb(1.0,.0,.0) # YOUR CODE HERE;
	blue  <- matrix(data = 0, nrow=dims$x,ncol = dims$y)#rgb(.0,.0,1.0)# YOUR CODE HERE;
	green <- matrix(data = 0, nrow=dims$x,ncol = dims$y)#rgb(.0,1.0,.0)# YOUR CODE HERE;
	rgbList <- list(red, blue, green)# YOUR CODE HERE;
	classifiedImage <- array(data = c(red, blue, green), dim = c(dims$x, dims$y, dims$z))   # YOUR CODE HERE;
	#classifiedImage[1,1,]

	classifiedImage







	# Test cases:
	if (dims[1] != 256 || dims[2] != 256) {
	  print("Incorrect dimensions in variable dims")
	  print('Created all matrix templates: \u2717')
	  return()
	}
	for (mat in list(class1, class2, class3, red, blue, green)) {
	  if (dim(mat)[1] != dims[1] || dim(mat)[2] != dims[2]) {
	    print("Class matrices and color matrices do not have the correct dimensions")
	    print('Created all matrix templates: \u2717')
	    return()
	  }
	}
	if (dim(classifiedImage)[1] != 256 || dim(classifiedImage)[2] != 256 || 
	    dim(classifiedImage)[3] != 3) {
	  print("classifiedImage matrix has the incorrect dimensions")
	  print("Created final image template: \u2717")
	  return()
	}
	
	print('Created all matrix templates and final image template : \u2713')




	# -----------------------------------------Part 4: Classification---------------------------------
	# Now we will run our classification algorithm for each of our pixel choices.
	# Compute the dot products of the chosen pixel vector against all the other pixels
	# To get a vector from the stack, write pictureStack[x,y,] 
	# (remember that last comma!)


	# -----------------------------------------Part 4: Classification---------------------------------
	# Now we will run our classification algorithm for each of our pixel choices.
	# Compute the dot products of the chosen pixel vector against all the other pixels
	# To get a vector from the stack, write pictureStack[x,y,] 
	# (remember that last comma!)
	
	
	# YOUR CODE HERE
	# Class 1: compare coord1 to every vector in the picture stack.
	vecCoord1 <- c(pictureStack[82, 33,])
	vecCoord1
	aMag <- sqrt(vecCoord1[1]^2+vecCoord1[2]^2+vecCoord1[3]^2+vecCoord1[4]^2+vecCoord1[5]^2+vecCoord1[6]^2)  #||a||
	aMag 
	bMag <- 0 #||b||
	dotProduct <- 0
	cosTheta <- 0
	
	for (i in 1:256)
	{
	  for (j in 1:256)
	  {
	    for (k in 1:6)
	    {
	      dotProduct <- dotProduct + (vecCoord1[k] * pictureStack[i,j,k])
	                                   
	        bMag <- bMag + (pictureStack[i,j,k]^2)                           
	                                   
	                                   
	    }
	    bMag
	    bMag <- sqrt(bMag)
	    cosTheta <- dotProduct/(aMag * bMag)
	    class1[i,j] <- cosTheta
	    
	    bMag <- 0 #||b||
	    dotProduct <- 0
	    
	  }
	}
	
	class1
	
	
	
	
	# YOUR CODE HERE
	# Class 2: compare coord2 to every vector in the picture stack.
	vecCoord2 <- c(pictureStack[180, 163,])
	vecCoord2
	aMag <- sqrt(vecCoord2[1]^2+vecCoord2[2]^2+vecCoord2[3]^2+vecCoord2[4]^2+vecCoord2[5]^2+vecCoord2[6]^2)  #||a||
	aMag 
	bMag <- 0 #||b||
	dotProduct <- 0
	cosTheta <- 0
	
	for (i in 1:256)
	{
	  for (j in 1:256)
	  {
	    for (k in 1:6)
	    {
	      dotProduct <- dotProduct + (vecCoord2[k] * pictureStack[i,j,k])
	      
	      bMag <- bMag + (pictureStack[i,j,k]^2)                           
	      
	      
	    }
	    bMag
	    bMag <- sqrt(bMag)
	    cosTheta <- dotProduct/(aMag * bMag)
	    class2[i,j] <- cosTheta
	    
	    bMag <- 0 #||b||
	    dotProduct <- 0
	    
	  }
	}
	
	class2
	
	
	
	# YOUR CODE HERE
	# Class 3: compare coord3 to every vector in the picture stack.
	vecCoord3 <- c(pictureStack[240, 233,])
	vecCoord3
	aMag <- sqrt(vecCoord3[1]^2+vecCoord3[2]^2+vecCoord3[3]^2+vecCoord3[4]^2+vecCoord3[5]^2+vecCoord3[6]^2)  #||a||
	aMag 
	bMag <- 0 #||b||
	dotProduct <- 0
	cosTheta <- 0
	
	for (i in 1:256)
	{
	  for (j in 1:256)
	  {
	    for (k in 1:6)
	    {
	      dotProduct <- dotProduct + (vecCoord3[k] * pictureStack[i,j,k])
	      
	      bMag <- bMag + (pictureStack[i,j,k]^2)                           
	      
	      
	    }
	    bMag
	    bMag <- sqrt(bMag)
	    cosTheta <- dotProduct/(aMag * bMag)
	    class3[i,j] <- cosTheta
	    
	    bMag <- 0 #||b||
	    dotProduct <- 0
	    
	  }
	}
	
	class3
	
	
	
	
	
	class1[44, 37]
	class2[180, 90]
	class3[230, 200]
	
	
	# Test cases:
	if (round(class1[44, 37], digits=6) != 0.998206) {
	  print("Dot product was not applied correctly to class 1")
	  print('Dot product 1 calculated: \u2717')
	  return()
	}
	if (round(class2[180, 90], digits=7) != 0.9888548) {
	  print("Dot product was not applied correctly to class 2")
	  print('Dot product 2 calculated: \u2717')
	  return()
	}
	if (round(class3[230, 200], digits=7) != 0.9976423) {
	  print("Dot product was not applied correctly to class 3")
	  print('Dot product 3 calculated: \u2717')
	  return()
	}
	print('Dot products have been calculated: \u2713')
	
	
	
	
	
	
	
	# --------------------------------------Part 5: Create the final image----------------------------
	
	# Create the final matrix of rgb values for output
	r   <- c(256, 0, 0); #class 1
	g   <- c(0, 256, 0); #class 2
	b   <- c(0, 0, 256);#class 3
	
	print("Class colors have been set: \u2713")
	
	# YOUR CODE HERE
	# Iterate over every pixel, compare the maximum values for that pixel from the class
	# matrices form part 4 and then fill in that pixel with the respective color.
	color = "none"
	rock1 <- 0
	rock2 <- 0
	rock3 <- 0
	rockundefined <- 0
	for (i in 1:256)
	{
	  for (j in 1:256)
	  {
	    for (k in 1:1)
	    {
  	    if (class1[i,j] > class2[i,j] && class1[i,j] > class3[i,j])#IF one is biggest cosTheta(smallest angle)
  	    {
  	      classifiedImage[i,j,1] <- 1
  	      rock1 <- rock1 + 1
  	    }
  	    else if (class2[i,j] > class3[i,j] && class2[i,j] > class1[i,j])#if 2 is biggest
  	    {
  	      classifiedImage[i,j,2] <- 1
  	      rock2 <- rock2 + 1
  	    }
  	    else if (class3[i,j] > class1[i,j] && class3[i,j] > class1[i,j])#if 3 biggest
  	    {
  	      classifiedImage[i,j,3] <- 1
  	      rock3 <- rock3 + 1
  	    }
	      else if (TRUE)
	      {
	        classifiedImage[i,j,] <- c(1,0,1)
	        rockundefined < rockundefined + 1
	      }
	      
	    }
	  }
	}
	
	rock1
	rock2
	rock3
	rockundefined
	
	
	# Test cases:
	# Check if random points are classified correctly
	#if (classifiedImage[1, 1,] != red || classifiedImage[83, 33,] != red || 
	 #   classifiedImage[180, 163,] != green || classifiedImage[240, 233,] != blue ||
	#    classifiedImage[161, 77,] != blue) {
	#  print("Data was not classified correctly.") 
	#  print("Check red, blue, green coordinates and how you get your maximum value")
	#  print('Image classfied: \u2717')
	#  return()
	#}
	
  #print('Image has been classified: \u2713')
	
  # Now we have our classified image, coded by color
  #classifiedImage <- classify(classifiedImage[,,1],classifiedImage[,,2], classifiedImage[,,3])
  classifiedImage 
  writeJPEG(classifiedImage, "classifiedIMage.jpg")
}


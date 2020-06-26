prediction<-function(x){
  k=load_model_hdf5("my_model_final64.h5", custom_objects = NULL, compile = TRUE)  
  img=image_load(x,target_size = c(64,64))
  data=image_to_array(img)
  dim(data) <- c(1, 64, 64, 3)
  ans=predict(k,data)
    return (ans)
    }
    
    
 
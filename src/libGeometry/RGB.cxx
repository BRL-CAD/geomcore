#include "RGB.h"

RGB::RGB(){
    val[0]=0;
    val[1]=0;
    val[2]=0;
////  cout << "   RGB::RGB()" << endl;

}
RGB:: RGB(double r, double g, double b){

    val[0]=r;
    val[1]=g;
    val[2]=b;
////    cout << "   RGB::RGB(r,g,b)" << endl; 

}
double RGB::getBlue(){
  //returns blue value
  ////cout << "RGB::getBlue" << endl;
  return val[2];
}
double RGB::getGreen(){
 //returns green value
 //// cout << "RGB::getGreen" << endl;
  return val[1];
}
double RGB::getRed(){
 //returns red value
 //// cout << "RGB::getRed" << endl;
  return val[0];
}
void RGB::setColor(double r, double g, double b){
  //sets RGB colors
    val[0]=r;
    val[1]=g;
    val[2]=b;    
 //// cout << "RGB::setColor" << endl;
}
void RGB::add(RGB & p, RGB & q){

        val[0]= p.val[0] + q.val[0];
        val[1]= p.val[1] + q.val[1];
        val[2]= p.val[2] + q.val[2];
        
// adds two RGBs componentwise and stores in current object
}
void RGB::scale(double k, RGB & p  ){
    
    val[0] = k*p.val[0];
    val[1] = k*p.val[1];
    val[2] = k*p.val[2];

//scales an RGB using componentwise multiplication with a scalar.
}



std::ostream&
operator<<(std::ostream& s, const RGB& rgb)
{
    return s << rgb.val[0] << " " << rgb.val[1] << " " << rgb.val[2] << " ";
}

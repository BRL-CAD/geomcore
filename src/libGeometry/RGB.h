#ifndef RGB_H
#define RGB_H
#include <iostream>
//using std::cout;
//using std::endl;
/** Implements methods to manipulate colors */
class RGB {

public:
 friend std::ostream&
     operator<<(std::ostream&, const RGB&);


    RGB();
    RGB(double r, double g, double b);
    /** Sets red, green, and blue values */
    void setColor(double r, double g, double b);
    /** Returns red value */
    double getRed();
    /** Returns green value */
    double getGreen();
    /** Returns blue value */
    double getBlue();
    /** adds two RGBs componentwise */
    void add(RGB & p, RGB & q);
    /** scales an RGB using componentwise multiplication with a scalar */
    void scale(double k , RGB & p );
    
private:    
    /** Stores the red-green-blue values */
    double val[3];

};


#endif //RGB_H

#ifndef VEC_H
#define VEC_H

#include <iostream>
#include "Point.h"

using std::cout;
using std::endl;


/**
 * Stores 3D vector, makes components accessible, and has cross and dot methods.
 */
class Vec {
 friend std::ostream&
     operator<<(std::ostream&, const Vec&);


  public:
    Vec();
    Vec(double x, double y, double z);
    Vec(Point A, Point B);
/**
  *  dot product of two vectors = a1*b1+a2*b2+a3*b3=|a| |b| Cos(angle between a and b).
 */
    double dot(Vec v);
/**
  * cross product of two vectors =(a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1), (a pseudovector)
 */
    void cross(Vec v1, Vec v2);
/**
  *  The Unitizing of a vector
 */
    void unitize();
/**
  *  The adds a vector to this one
 */
    void add(Vec v1);
/**
  *  The subtraction of two vectors
 */
    void sub(Vec v1);
/**
  *  The multiplication of a vector times a scalar
 */
    void mult(double s);
/**
  * gets x component
 */
    inline double getX() { return X; }

/**
  * gets y component
*/    
    inline double getY() { return Y; }
/**
  * gets z component
 */
    inline double getZ() { return Z; }

/**
 *  stores designated x,y, and z values in Vec attributes
 */
    void setVec(double x, double y, double z);

/**
 *  array of 3 vector components
 */
//    double v[3];
	double X;
	double Y;
	double Z;
};
#else

class Vec;

#endif //VEC_H

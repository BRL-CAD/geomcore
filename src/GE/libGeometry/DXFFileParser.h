#ifndef __DXFFILEPARSER_H__
#define __DXFFILEPARSER_H__

// system headers
#include <iostream>
#include <vector>

// interface headers
#include "Geometry/Scene.h"


namespace Geometry {

  class DXFFileParser {

  private:

    typedef struct {
      double	x;
      double	y;
      double	z;
    } ADM_3;

    void nextLine(char *B, FILE *fh);

  protected:
    DXFFileParser(std::string);
    ~DXFFileParser();

    Scene* parse(std::string filename);

  };

}

#endif  /* __DXFFILEPARSER_H__ */

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
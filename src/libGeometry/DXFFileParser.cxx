#include "DXFFileParser.h"

using namespace Geometry;

#include <string.h>
#include <stdlib.h>
#include "Geometry/Triangle.h"


void DXFFileParser::nextLine(char *B, FILE *FH)
{
  int	i= 0;

  do {
    fread(&B[i],1,1,FH);
  } while (B[i++] != 10);
  B[i-1]= 0;
}


Scene* DXFFileParser::parse(std::string F) 
{
  FILE		*FH;
  char		B[32];
  ADM_3		P[4];
  int		i;
  Scene *scene = new Scene();
  std::vector<Triangle*> geometry;

  //  printf("loading dxf file: %s\n",F.c_str());
  FH= fopen(F.c_str(),"r");
  while (!feof(FH)) {
    do {
      nextLine(B,FH);
    } while (strcmp(B,"3DFACE") && strcmp(B,"ENDSEC"));
    if (!strcmp(B,"ENDSEC")) {
      fclose(FH);
      //      std::cout << "leaving early from DXF file parser" << std::endl;

      scene->add(geometry);
      return scene;
    }
    // Advance 2 Lines
    nextLine(B,FH);
    nextLine(B,FH);

    // Read in First Corner
    for (i= 0; i < 4; i++) {
      nextLine(B,FH);
      nextLine(B,FH);
      P[i].x= atof(B);
      nextLine(B,FH);
      nextLine(B,FH);
      P[i].y= atof(B);
      nextLine(B,FH);
      nextLine(B,FH);
      P[i].z= atof(B);
//      printf("[%.2f,%.2f,%.2f]\n",P[i].x,P[i].y,P[i].z);
    }

    Triangle *t1 = new Triangle();
    Triangle *t2 = new Triangle();
    t1->vertexA[0] = P[0].x;
    t1->vertexA[1] = P[0].y;
    t1->vertexA[2] = P[0].z;
    t1->vertexB[0] = P[1].x;
    t1->vertexB[1] = P[1].y;
    t1->vertexB[2] = P[1].z;
    t1->vertexC[0] = P[2].x;
    t1->vertexC[1] = P[2].y;
    t1->vertexC[2] = P[2].z;

    t2->vertexA[0] = P[2].x;
    t2->vertexA[1] = P[2].y;
    t2->vertexA[2] = P[2].z;
    t2->vertexB[0] = P[3].x;
    t2->vertexB[1] = P[3].y;
    t2->vertexB[2] = P[3].z;
    t2->vertexC[0] = P[0].x;
    t2->vertexC[1] = P[0].y;
    t2->vertexC[2] = P[0].z;

    geometry.push_back(t1);
    geometry.push_back(t2);

    /*
    ADRT_Geometry_Triangle_Create(P[0],P[1],P[2]);
    ADRT_Geometry_Triangle_Create(P[2],P[3],P[0]);
    */
  }

  //  std::cout << "adding geometry with size " << geometry.size();
  scene->add(geometry);

  return scene;
}


DXFFileParser::DXFFileParser(std::string filename)
{
  //  load(filename);
}


DXFFileParser::~DXFFileParser()
{
  //  _filename = "";
}


// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8

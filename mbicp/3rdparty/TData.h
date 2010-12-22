/***************************************************/
/* Last Revised: 
$Id: TData.h,v 1.1 2004/02/11 20:29:39 jminguez Exp jminguez $
*/
/***************************************************/

#ifndef TData
#define TData

/* 
   Este fichero contiene los tipos de datos utilizados por todos 
*/

#define RADIO 0.4F  /* Radio del robot */

typedef struct {
  float x;
  float y;
}Tpf;


typedef struct {
  float r;
  float t;
}Tpfp;

typedef struct {
  int x;
  int y;
}Tpi;

typedef struct {
  float x;
  float y;
  float tita;
}Tsc;

typedef struct {
  int numPuntos;
  Tpf laserC[721];
  Tpfp laserP[721];
}Tscan;

// Associations information
typedef struct{
  float rx,ry,nx,ny,dist;				// Point (nx,ny), static corr (rx,ry), dist 
  int numDyn;							// Number of dynamic associations
  float unknown;						// Unknown weight
  int index;							// Index within the original scan
  int L,R;
}TAsoc;


#endif

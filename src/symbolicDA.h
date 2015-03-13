#define typ_metryczny 1
#define typ_porzadkowy 2
#define typ_nominalny 3
#define typ_mieszany 4

#ifdef WIN32
#define GDM_API __declspec(dllexport)
extern "C" void GDM_API  calculateProb(double * p , int * rows, int * cols, int * type, double * weights, double * wynik );
#else
extern "C" void calculateProb(double * x , int * rows, int * cols, int * type, double * weights, double * wynik );
#endif

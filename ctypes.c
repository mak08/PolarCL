/*
 * Description  Extract info from SSL headers  
 * Author       Michael Kappert 2015
 * Last Modified <michael 2022-06-11 23:29:55>
 */


#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>


int main () {
  FILE *fp;

  ////////////////////////////////////////////////////////////////////////////////
  // Create definitions

  fp = fopen("ctypes.cl", "w");  

  ////////////////////////////////////////////////////////////////////////////////
  // Write header
  fprintf(fp, ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n");
  fprintf(fp, ";;; Generated from ctypes.c, do not edit\n");
  fprintf(fp, ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n");
  fprintf(fp, "\n");
  fprintf(fp, "(in-package polarcl)\n");
  fprintf(fp, "\n");
  fprintf(fp, "\n");
  
  ////////////////////////////////////////////////////////////////////////////////
  // size_t
  fprintf(fp, "\n");
  if (sizeof(size_t) == sizeof(unsigned int)) 
	{ 
	  fprintf(fp, "(defctype size_t :unsigned-int)\n");
	}
  else if (sizeof(size_t) == sizeof(unsigned long)) 
	{
	  fprintf(fp, "(defctype size_t :unsigned-long)\n");
	}
  else
	{
	  fprintf(fp, "(defctype size_t :|Fixme - could not determine size of size_t|)\n");
	}

  ////////////////////////////////////////////////////////////////////////////////
  // ERRNO
  fprintf(fp, "\n");
  fprintf(fp, "(defconstant EINTR %i)\n", EINTR);  
 
  ////////////////////////////////////////////////////////////////////////////////
  // Posix Compliance
  fprintf(fp, "\n");
  fprintf(fp, "(defconstant _POSIX_C_SOURCE %lu)\n", _POSIX_C_SOURCE);

  ////////////////////////////////////////////////////////////////////////////////
  // STAT constants

  fprintf(fp, "\n");
  fprintf(fp, "(defconstant S_IFMT %i)\n", S_IFMT);
  fprintf(fp, "(defconstant S_IFBLK %i)\n", S_IFBLK);
  fprintf(fp, "(defconstant S_IFCHR %i)\n", S_IFCHR);
  fprintf(fp, "(defconstant S_IFDIR %i)\n", S_IFDIR);
  fprintf(fp, "(defconstant S_IFIFO %i)\n", S_IFIFO);
  fprintf(fp, "(defconstant S_IFLNK %i)\n", S_IFLNK);
  fprintf(fp, "(defconstant S_IFREG %i)\n", S_IFREG);

  return 0;
}

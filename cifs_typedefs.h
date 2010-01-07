#ifndef CIFS_TYPEDEFS_H
#define CIFS_TYPEDEFS_H
/* ========================================================================== **
 *                              cifs_typedefs.h
 *
 * Copyright:
 *  Copyright (C) 2003, 2004 by Christopher R. Hertel
 *
 * Email: crh@ubiqx.mn.org
 *
 * $Id: cifs_typedefs.h,v 0.7 2004/06/02 22:09:22 crh Exp $
 *
 * -------------------------------------------------------------------------- **
 *
 * Description:
 *
 *  This header contains type definitions and constants used throughout the
 *  entirety of the Implementing CIFS toolkit.
 *
 * -------------------------------------------------------------------------- **
 *
 * License:
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public   
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of   
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *  
 *  You should have received a copy of the GNU Lesser General Public   
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * -------------------------------------------------------------------------- **
 *
 * Notes:
 *
 *  This header file contains constants and type declarations common to most
 *  of the modules and utilities.  You'll need these typedefs as well, in
 *  order to use the API.  That's why this file is included in cifs.h.
 *
 *  If the C99 header files <stdint.h> and <stdbool.h> are not present, you
 *  may get an error when trying to compile this library.  The error message
 *  should point you at this file.
 *
 *  To fix the problem, you may need to edit cifs_system.h and create or
 *  edit the platform-specific file platform/<your platform>/platform.h.
 *  That file should contain one or both of the following lines:
 *
 *  #define NO_STDINT_H
 *  #define NO_STDBOOL_H
 *
 *  The platform.h file should also typedef (or include a header file that
 *  typedefs) the following types:
 *
 *  int8_t    - signed  8-bit (1 byte) integer (eg. signed char).
 *  int16_t   - signed 16-bit (2 byte) integer (eg. signed short).
 *  int32_t   - signed 32-bit (4 byte) integer (eg. signed long).
 *  uint8_t   - unsigned 8-bit (1 byte) integer (eg. unsigned char).
 *  uint16_t  - unsigned 16-bit (2 byte) integer (eg. unsigned short).
 *  uint32_t  - unsigned 32-bit (4 byte) integer (eg. unsigned long).
 *
 *  bool      - Enumerated type, like so:
 *              typedef enum { false = 0, true  = 1 } bool;
 *
 *  uint      - unsigned integer (machine int size, eg. signed int).
 *              This isn't C99 standard, but it is common.  It doesn't
 *              violate the C99 standard because it doesn't end in "_t".
 *
 * ========================================================================== **
 */

#include "cifs_system.h"  /* Provides system-specific definitions & macros. */
                          /* See the notes above regarding C99 integer and  */
                          /* boolean types.                                 */

#ifndef NO_STDINT_H       /* If NO_STDINT_H is *not* defined then include   */
#include <stdint.h>       /* stdint.h.                                      */
#endif

#ifndef NO_STDBOOL_H      /* If NO_STDBOOL_H *isn't* defined then include   */
#include <stdbool.h>      /* stdbool.h.                                     */
#endif


/* -------------------------------------------------------------------------- **
 * Typedefs:
 *
 *  uchar - Unsigned character (aka. 'byte').
 *          Used when dealing with blocks of bytes (network packets,
 *          data blocks to be encrypted, etc.).  Note that some systems
 *          define a char as signed, while on others the char type is
 *          unsigned.  That's one reason an explicitly unsigned byte
 *          type is useful.
 */

typedef unsigned char uchar;


/* ========================================================================== */
#endif /* CIFS_TYPEDEFS_H */

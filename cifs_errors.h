#ifndef CIFS_ERRORS_H
#define CIFS_ERRORS_H
/* ========================================================================== **
 *                                cifs_errors.h
 *
 * Copyright:
 *  Copyright (C) 2000-2004 by Christopher R. Hertel
 *
 * Email: crh@ubiqx.mn.org
 *
 * $Id: cifs_errors.h,v 0.12 2004/05/30 03:11:34 crh Exp $
 *
 * -------------------------------------------------------------------------- **
 *
 * Description:
 *
 *  This file contains error code definitions.
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
 * ========================================================================== **
 */

/* Error Classes */
#define cifs_errERR  (-0x0000)
#define cifs_errWARN (-0x1000)
#define cifs_errINFO (-0x2000)
#define cifs_errMASK   0xF000

typedef enum
  {
  /* Success */
  cifs_errSuccess         = 0,

  /* Errors */
  cifs_errGeneric         = (cifs_errERR - 1),
  cifs_errNullInput       = (cifs_errERR - 2),
  cifs_errNameTooLong     = (cifs_errERR - 3),
  cifs_errLeadingDot      = (cifs_errERR - 4),
  cifs_errDoubleDot       = (cifs_errERR - 5),
  cifs_errEndDot          = (cifs_errERR - 6),
  cifs_errScopeTooLong    = (cifs_errERR - 7),
  cifs_errBadLblFlag      = (cifs_errERR - 8),
  cifs_errOutOfBounds     = (cifs_errERR - 9),
  cifs_errTruncatedBufr   = (cifs_errERR - 10),
  cifs_errBufrTooSmall    = (cifs_errERR - 11),
  cifs_errBadL1Value      = (cifs_errERR - 12),
  cifs_errSyntaxError     = (cifs_errERR - 13),
  cifs_errInvalidLblLen   = (cifs_errERR - 14),
  cifs_errIllegalSSType   = (cifs_errERR - 15),
  cifs_errInvalidSSLen    = (cifs_errERR - 16),
  cifs_errBadCalledName   = (cifs_errERR - 17),
  cifs_errBadCallingName  = (cifs_errERR - 18),
  cifs_errUnknownCommand  = (cifs_errERR - 19),
  cifs_errInvalidPacket   = (cifs_errERR - 20),

  /* Warnings */
  cifs_warnGeneric        = (cifs_errWARN - 1),
  cifs_warnContainsDot    = (cifs_errWARN - 2),
  cifs_warnNonPrint       = (cifs_errWARN - 3),
  cifs_warnNonAlpha       = (cifs_errWARN - 4),
  cifs_warnNulByte        = (cifs_errWARN - 5),
  cifs_warnInvalidChar    = (cifs_errWARN - 6),
  cifs_warnNonAlphaNum    = (cifs_errWARN - 7),
  cifs_warnEmptyStr       = (cifs_errWARN - 8),
  cifs_warnAsterisk       = (cifs_errWARN - 9),
  cifs_warnLenExceeded    = (cifs_errWARN - 10),
  cifs_warnUnknownKey     = (cifs_errWARN - 11),
  cifs_warnDuplicateKey   = (cifs_errWARN - 12),

  /* Info Codes */
  cifs_infoGeneric        = (cifs_errINFO - 1),

  /* The End */
  cifs_errTheEnd          = -0xFFFF
  } cifs_error; 

/* -------------------------------------------------------------------------- **
 * Macros:
 *
 *  cifs_errIsError( E )  - Returns TRUE if E is in the Error catagory.
 *  cifs_errIsWarn( W )   - Returns TRUE if W is in the Warning category.
 *  cifs_errIsInfo( I )   - Returns TRUE if I is in the Information category.
 *  cifs_errClass( E )    - Returns the error class of error code E.
 *  cifs_errCode( E )     - Returns the error number (positive int) of E.
 *
 *  The cifs_errClass() macro allows you to do things like this:
 *
 *    switch( cifs_errClass(err) )
 *      {
 *      case cifs_errERR:  return( "Error: " );
 *      case cifs_errWARN: return( "Warning: " );
 *      case cifs_errINFO: return( "Info: " );
 *      }
 *    return( "Unknown Error Class: " );
 *
 */

#define cifs_errIsError( E ) \
        ( -cifs_errERR  == (cifs_errMASK & -(E)) ? true : false )

#define cifs_errIsWarn( W )  \
        ( -cifs_errWARN == (cifs_errMASK & -(W)) ? true : false )

#define cifs_errIsInfo( I )  \
        ( -cifs_errINFO == (cifs_errMASK & -(I)) ? true : false )

#define cifs_errClass( E ) (-(cifs_errMASK & -(E)))

#define cifs_errCode( E ) (~cifs_errMASK & -(E))

/* ========================================================================== */
#endif /* CIFS_ERRORS_H */

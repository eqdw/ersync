#ifndef CIFS_H
#define CIFS_H
/* ========================================================================== **
 *                                   cifs.h
 *
 * Copyright:
 *  Copyright (C) 2000-2004 by Christopher R. Hertel
 *
 * Email: crh@ubiqx.mn.org
 *
 * $Id: cifs.h,v 0.8 2007/11/06 21:13:53 crh Exp $
 *
 * -------------------------------------------------------------------------- **
 *
 * Description:
 *
 *  This is the global header file for the "Implementing CIFS" library and
 *  toolkit.
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
 *  This header file includes all of the headers of all of the subsystems.
 *  There is no run-time penalty for doing so.  Include this library when
 *  writing code that uses the ubiqx "Implementing CIFS" code (the libcifs
 *  code).
 *
 *  The header files are split up because I like modularization.  Each .c
 *  file has a corresponding .h file, and the two are closely related.
 *  This makes it much easier for me to maintain the code and, fortunately,
 *  you don't have to worry about it.  Simply include this file and you have
 *  all you need.  On the other hand, if you want all the SMB code but you
 *  have no use for the NBT subsystem (perhaps you are writing a CIFS over
 *  native TCP on port 445 implementation), then you only need include
 *  "SMB/smb.h".
 *
 *  See?
 *
 *  Chris -)-----
 *
 * ========================================================================== **
 */

#include "cifs_typedefs.h"  /* Common definitions, typedefs, etc.       */
#include "NBT/nbt.h"        /* Global include for the NBT subsystem.    */
#include "SMB/smb.h"        /* Global include for the SMB subsystem.    */
#include "Auth/auth.h"      /* Global include for the Auth subsystem.   */
#include "util/util.h"      /* Utility module headers.                  */

/* ========================================================================== */
#endif /* CIFS_H */

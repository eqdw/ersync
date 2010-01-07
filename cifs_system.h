#ifndef CIFS_SYSTEM_H
#define CIFS_SYSTEM_H
/* ========================================================================== **
 *                                cifs_system.h
 *
 * Copyright:
 *  Copyright (C) 2001,2004 by Christopher R. Hertel
 *
 * Email: crh@ubiqx.mn.org
 *
 * $Id: cifs_system.h,v 0.5 2004/12/23 20:13:05 crh Exp $
 *
 * -------------------------------------------------------------------------- **
 *
 * Descripton:
 *
 *  This is the system-specific header file for the "Implementing CIFS"
 *  utility toolkit.
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
 *  When porting to a new platform, this is the file that you should edit
 *  first.  Keep as many system-specific changes in this header as possible.
 *
 *  My goal is to be as 'standard' and simple as I can with the rest of the
 *  toolkit in hopes that I can reduce the amount of fiddling that people
 *  need to do in order to port this stuff.  Where necessary, I will try to
 *  segregate chunks of code that need to be changed on a per-platform basis.
 *
 *  I'll *try*.
 *
 *  crh
 *
 * ========================================================================== **
 */

/* -------------------------------------------------------------------------- **
 * Generic includes that are almost always needed.
 * If any of these cause trouble (eg., don't actually include what is needed)
 * then add the missing stuff in the platform/<platform_name>/platform.h
 * file.
 */

#include <stddef.h>
#include <string.h>


/* -------------------------------------------------------------------------- **
 * System-specific includes.
 */

/* Amiga */
#if defined (_AMIGA)
#if defined (__SASC)
#include "platform/Amiga_SAS/platform.h"
#endif
#endif

/* SGI Irix */
#if defined (sgi)
#include "platform/Irix_SGI_C/platform.h"
#endif

/* Default */
#ifndef PLATFORM_H
#include "platform.h"
#endif


/* ========================================================================== */
#endif /* CIFS_SYSTEM_H */

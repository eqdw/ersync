#ifndef CIFS_BLOCK_H
#define CIFS_BLOCK_H
/* ========================================================================== **
 *
 *                                cifs_block.h
 *
 * Copyright:
 *  Copyright (C) 2002-2004 by Christopher R. Hertel
 *
 * Email: crh@ubiqx.mn.org
 *
 * $Id: cifs_block.h,v 0.10 2004/10/06 04:32:31 crh Exp $
 *
 * -------------------------------------------------------------------------- **
 *
 * Description:
 *  A memory block object.  Used to manage blobs of memory.
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
 *  It seemed odd to me to formalize this particular structure, until I did
 *  it.  Then it seemed the most amazingly obvious thing to do.
 *
 *  Memory block objects are used to manage chunks of memory that are, in
 *  turn, used for building and decomposing protocol messages.  The block
 *  header keeps track of used and unused portions of the block.  If used
 *  correctly, memory block objects can prevent buffer overrun errors.
 *
 *  For our purposes, the 'buffer' is the allocated chunk of memory, all of
 *  which can be used to handle the marshalling and unmarshalling of data.
 *  The 'block' is the whole object, including the fields that keep track of
 *  the used and unused portions of the buffer.
 *
 *  The size of a memory buffer that can be handled by a memory block
 *  object is limited to the maximum value of a signed long integer.
 *  That should be ((2^31)-1).  RFC1001/1002 limits NBT Session messages
 *  to ((2^17)-1) bytes in total size.  Over naked transport, the limit
 *  is probably ((2^24)-1) bytes (though it may be less).  In either case,
 *  the maximum size of an SMB message is easily within the size that can
 *  be accommodated by an 32-bit signed long int.
 *
 *  SMB, however, is used as a transport for higher-level protocols such as
 *  RAP and MS-RPC.  These higher-level protocols can use SMB transactions
 *  to transport very large blocks of data, potentially exceeding the 24-bit
 *  limit.  The work-around is to receive these super-blocks using multiple
 *  blocks (possibly held together using an array or linked list).
 *
 * ========================================================================== **
 */

#include "cifs_common.h"    /* CIFS library common include file. */


/* -------------------------------------------------------------------------- **
 * Typedefs:
 *
 *  cifs_Block - A buffer header structure.  Used to manage a blob of memory.
 *
 */

typedef struct
  {
  long   size;
  long   used;
  uchar *bufr;
  } cifs_Block;


/* -------------------------------------------------------------------------- **
 * Macros:
 *
 *  cifs_BlockAvail( b )
 *      - returns the amount of free space available in the buffer.
 *        Input:  b - A pointer to a cifs_Block structure, the header of
 *                    the block being queried.
 *        Output: The number of free bytes available in the buffer.
 *        Errors: A negative return value indicates an error.  It may be
 *                that the block was not properly initialized, or it may
 *                indicate that more bytes have been marked used than are
 *                available in the buffer.  Either one is bad news.
 *
 *  cifs_BlockAlloc( b, bytes )
 *      - Allocates <bytes> more bytes worth of block <b>.
 *        Input:  b     - A pointer to the block header from which the byte
 *                        range will be allocated.
 *                bytes - Number of bytes to allocate.
 *        Output: NULL if the requested number of bytes is not available
 *                (in which case no change will be made to the input block),
 *                else a pointer (of type uchar *) to the start of the
 *                newly allocated byte range.
 *        Notes:  This macro simply calls cifs_BlockReAlloc() with a zero
 *                deallocation.
 *
 *  cifs_BlockDealloc( cifs_Block *b, long bytes );
 *      - Deallocate <bytes> worth of block <b>.
 *        Input:  b     - A pointer to the block header from which the byte
 *                        range will be relesed.
 *                bytes - Number of bytes to release.
 *        Output: None.
 *        Notes:  This macro simply calls cifs_BlockReAlloc() with a zero
 *                allocation.
 *                If the value of <bytes> is more than the number of bytes
 *                used in the block, then the entire block is deallocated.
 */

#define cifs_BlockAvail( b ) ((b)->size - (b)->used))

#define cifs_BlockAlloc( b, bytes ) cifs_BlockReAlloc( (b), 0, (bytes) )

#define cifs_BlockDealloc( b, bytes ) (void)cifs_BlockReAlloc( (b), (bytes), 0 )


/* -------------------------------------------------------------------------- **
 * Functions:
 */

cifs_Block *cifs_BlockInit( cifs_Block *b, long size, uchar *bufr );
  /* ------------------------------------------------------------------------ **
   * Initialize a block header.
   *
   *  Input:  b     - A pointer to the block header structure (cifs_Block *)
   *                  that is to be initialized.
   *          size  - The size of the memory block indicated by <bufr>.
   *          bufr  - A pointer to a blob of memory.
   *
   *  Output: A pointer to the initialized block header (same as <b>).
   *
   *  Notes:  The block header, <b>,  should not be within <bufr>.  If the
   *          two are allocated together, remember to subtract
   *          sizeof( cifs_Block ) from the total allocated byte count, and
   *          pass the result in as <size>.  Likewise, <bufr> and <b> should
   *          not point to the same location.  One common way to handle this
   *          is:
   *
   *          {
   *          cifs_Block *block;
   *
   *          block = (cifs_Block *)malloc( sizeof( cifs_Block ) + BUFRSIZE );
   *          return( cifs_BlockInit( block, BUFRSIZE, (uchar *)&block[1] ) );
   *          }
   *
   *          The block size is passed as a signed long.  Passing a
   *          negative value, however, is most likely a very wrong thing
   *          to do.  No testing is done for this situation, however.
   *
   * ------------------------------------------------------------------------ **
   */


cifs_Block *cifs_BlockSubInit( cifs_Block *child, cifs_Block *parent );
  /* ------------------------------------------------------------------------ **
   * Create a sub-block from an existing memory block.
   *
   *  Input:  child   - A pointer to a block header structure that will be
   *                    initialized as a header for a sub-block of the buffer
   *                    managed by <parent>.
   *          parent  - A pointer to the header of an existing memory block.
   *                    The free space within the <parent> block will become
   *                    the buffer managed by the <child> block header.
   *
   *  Output: A pointer to the child block header (same as <child>).
   *
   *  Notes:  There is a serious alignment issue here.  It is likely that
   *          the buffer managed by the child header will not be aligned.
   *          You certainly cannot count on it (unless you manage the
   *          alignment yourself).
   *
   *          The purpose of these blocks, however, is to facilitate the
   *          marshalling and unmarshalling of network messages--where
   *          alignment isn't an issue.
   *
   *          This function marks the entire parent block as "used" when
   *          the child is created.  When you release the child, be sure
   *          to cifs_BlockRelease( parent, child->size ) first.
   *
   * ------------------------------------------------------------------------ **
   */


uchar *cifs_BlockReAlloc( cifs_Block *b, long rel, long use );
  /* ------------------------------------------------------------------------ **
   * Release <rel> bytes, then allocate <use> bytes.
   *
   *  Input:  b   - A pointer to the buffer header from which the byte
   *                  range will be reallocated.
   *          rel - Number of used bytes to release.
   *          use - Number of bytes to use.
   *
   *  Output: NULL if the requested number of bytes is not available, else
   *          a pointer (of type uchar *) to the start of the newly
   *          (re)allocated byte range.
   *
   *  Notes:  There is no attempt at any sort of alignment being made here.
   *          These functions simply carve up and manage a block of bytes.
   *
   *          If the function returns NULL (to indicate that there were not
   *          enough bytes available for allocation), then the buffer and
   *          buffer header will be unchanged by the function.
   *
   *          If <rel> is greater than the number of bytes in use, then
   *          all bytes are released before <use> bytes are allocated.  No
   *          error is returned if <rel> is too large.
   *
   * ------------------------------------------------------------------------ **
   */


/* ========================================================================== */
#endif /* CIFS_BLOCK_H */

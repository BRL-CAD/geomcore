/*                    B Y T E B U F F E R . H
 * BRL-CAD
 *
 * Copyright (c) 2011 United States Government as represented by
 * the U.S. Army Research Laboratory.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this file; see the file named COPYING for more
 * information.
 */
/** @file ByteBuffer.h
 * ByteBuffer.h
 *
 */

#ifndef __BYTEBUFFER_H__
#define __BYTEBUFFER_H__

#include "bu.h"
#include <string>

class ByteBuffer
{
public:
  /**
   * Allocates a new byte buffer.
   *
   * <p> The new buffer's position will be zero, its limit will be its
   * capacity, and its mark will be undefined.</p>
   *
   * @param  capacity
   *         The new buffer's capacity, in bytes
   *
   * @return  The new byte buffer
   */
  static ByteBuffer*
  allocate(size_t size);
  /**
   * Wraps a byte array into a buffer.
   *
   * <p> The new buffer will be backed by a bu_vlb that is, modifications
   * to the buffer will cause the vlb to be modified and vice versa.
   * The new buffer's capacity will be <tt>length</tt> + the VLB_BLOCK_SIZE
   * setting from vlb.c (which is currently 512), its position
   * will be zero its limit will be <tt>length</tt>,
   * and its mark will be undefined.</p>
   *
   * @param  data
   *         The array that will back the new buffer
   *
   * @param  length
   *         The length of the data to be used;
   *         must be non-negative.
   *
   * @return  The new byte buffer
   */
  static ByteBuffer*
  wrap(char* data, size_t size);

  virtual
  ~ByteBuffer();

  /**
   * Creates a new byte buffer that shares this buffer's content.
   *
   * <p> The contents of the new buffer will be a copy of this buffer.
   * The new buffer's capacity, limit, position, and mark values will be
   * identical to those of this buffer.</p>
   *
   * @return  The new byte buffer
   */
  ByteBuffer*
  duplicate();

  /**
   * <p>Relative <i>get</i> method.  Reads the byte at this buffer's
   * current position, and then increments the position.</p>
   *
   * @return  The byte at the buffer's current position
   *
   * @throws  BufferUnderflowException
   *          If the buffer's current position is not smaller than its limit
   */
  char
  get();

  /**
   * Relative bulk <i>get</i> method.
   *
   * <p> This method transfers bytes from this buffer into the given
   * destination array.  If there are fewer bytes remaining in the
   * buffer than are required to satisfy the request, that is, if
   * <tt>length</tt>&nbsp;<tt>&gt;</tt>&nbsp;<tt>remaining()</tt>, then no
   * bytes are transferred.</p>
   *
   * <p> Otherwise, this method copies <tt>length</tt> bytes from this
   * buffer into the given array, starting at the current position of this
   * buffer.  The position of this buffer is then incremented by
   * <tt>length</tt>.</p>
   *
   * @param  data
   *         The array into which bytes are to be written
   *
   * @param  length
   *         The maximum number of bytes to be written to the given
   *         array; must be non-negative.
   *
   */
  void
  get(char* data, size_t length);

  /**
   * <p> Relative <i>put</i> method.  Writes the given byte into this buffer at the current
   * position, and then increments the position.</p>
   *
   * @param  c
   *         The byte to be written
   *
   * @return  a boolean indicating whether the put operation was successful.
   */
  bool
  put(char c);

  /**
   * Relative bulk <i>put</i> method.
   *
   * <p> This method transfers the bytes remaining in the given source
   * buffer into this buffer.  If there are more bytes remaining in the
   * source buffer than in this buffer, that is, if
   * <tt>src.remaining()</tt>&nbsp;<tt>&gt;</tt>&nbsp;<tt>remaining()</tt>,
   * then no bytes are transferred.</p>
   *
   * <p> Otherwise, this method copies
   * <i>n</i>&nbsp;=&nbsp;<tt>src.remaining()</tt> bytes from the given
   * buffer into this buffer, starting at each buffer's current position.
   * The positions of both buffers are then incremented by <i>n</i>.</p>
   *
   * @param  src
   *         The source buffer from which bytes are to be read;
   *         must not be this buffer
   *
   * @return  a boolean indicating whether the put operation was successful.
   */
  bool
  put(ByteBuffer* src);

  /**
   * Relative bulk <i>put</i> method&nbsp;&nbsp;<i>(optional operation)</i>.
   *
   * <p> This method transfers bytes into this buffer from the given
   * source array.  If there are more bytes to be copied from the array
   * than remain in this buffer, that is, if
   * <tt>length</tt>&nbsp;<tt>&gt;</tt>&nbsp;<tt>remaining()</tt>, then no
   * bytes are transferred.</p>
   *
   * <p> Otherwise, this method copies <tt>length</tt> bytes from the
   * given array into this buffer, starting at the given offset in the array
   * and at the current position of this buffer.  The position of this buffer
   * is then incremented by <tt>length</tt>.</p>
   *
   * @param  src
   *         The array from which bytes are to be read
   *
   * @param  length
   *         The number of bytes to be read from the given array;
   *         must be non-negative and no larger than
   *         <tt>array.length - offset</tt>
   *
   * @return  a boolean indicating whether the put operation was successful.
   */
  bool
  put(char* src, size_t length);

  /**
   * Returns the byte array that backs this buffer.
   *
   * <p> Modifications to this buffer's content will cause the returned
   * array's content to be modified, and vice versa.</p>
   *
   * @return  The array that backs this buffer
   *
   */
  char*
  array();

  /**
   * Compacts this buffer.
   *
   * <p> The bytes between the buffer's current position and its limit,
   * if any, are copied to the beginning of the buffer.  That is, the
   * byte at index <i>p</i>&nbsp;=&nbsp;<tt>position()</tt> is copied
   * to index zero, the byte at index <i>p</i>&nbsp;+&nbsp;1 is copied
   * to index one, and so forth until the byte at index
   * <tt>limit()</tt>&nbsp;-&nbsp;1 is copied to index
   * <i>n</i>&nbsp;=&nbsp;<tt>limit()</tt>&nbsp;-&nbsp;<tt>1</tt>&nbsp;-&nbsp;<i>p</i>.
   * The buffer's position is then set to <i>n+1</i> and its limit is set to
   * its capacity.  The mark, if defined, is discarded.
   *
   * <p> The buffer's position is set to the number of bytes copied,
   * rather than to zero, so that an invocation of this method can be
   * followed immediately by an invocation of another relative <i>put</i>
   * method.</p>
   *
   */
  void
  compact();

  /**
   * <p>Returns a string summarizing the state of this buffer.</p>
   *
   * @return  A summary string
   */
  std::string
  toString();

  /**
   * Relative <i>get</i> method for reading a 16 bit value.
   *
   * <p> Reads the next two bytes at this buffer's current position,
   * composing them into a uint16_t value,
   * and then increments the position by two.</p>
   *
   * @return  The 16 bit value at the buffer's current position
   *
   */
  uint16_t
  get16bit();

  /**
   * Relative <i>put</i> method for writing a 16 bit value.
   *
   * <p> Writes two bytes containing the given 16 bit value into this
   * buffer at the current position, and then increments the position
   * by two.</p>
   *
   * @param  v
   *         The 16 bit value to be written
   */
  void
  put16bit(uint16_t v);

  /**
   * Relative <i>get</i> method for reading a 32 bit value.
   *
   * <p> Reads the next four bytes at this buffer's current position,
   * composing them into a uint32_t value,
   * and then increments the position by two.</p>
   *
   * @return  The 32 bit value at the buffer's current position
   *
   */
  uint32_t
  get32bit();

  /**
   * Relative <i>put</i> method for writing a 32 bit value.
   *
   * <p> Writes four bytes containing the given 32 bit value into this
   * buffer at the current position, and then increments the position
   * by two.</p>
   *
   * @param  v
   *         The 32 bit value to be written
   */
  void
  put32bit(uint32_t v);

  /**
   * Relative <i>get</i> method for reading a 64 bit value.
   *
   * <p> Reads the next eight bytes at this buffer's current position,
   * composing them into a uint64_t value,
   * and then increments the position by two.</p>
   *
   * @return  The 64 bit value at the buffer's current position
   *
   */
  uint64_t
  get64bit();

  /**
   * Relative <i>put</i> method for writing a 64 bit
   * value.
   *
   * <p> Writes eight bytes containing the given 64 bit value into this
   * buffer at the current position, and then increments the position
   * by two.</p>
   *
   * @param  v
   *         The 64 bit value to be written
   */
  void
  put64bit(uint64_t v);


  /**
   * Relative <i>get</i> method for reading a standard string value.
   *
   * <p> Reads the four bytes containing the given string's length followed
   * by the string data, and then increments the position by 4 + length of
   * the string.</p>
   *
   * @return  The standard string value at the buffer's current position
   *
   */
  std::string
  getString();

  /**
   * Relative <i>put</i> method for writing a standard string value.
   *
   * <p> Writes the four bytes containing the given string's length into this
   * buffer at the current position followed by the string data, and then
   * increments the position by 4 + length of the string.</p>
   *
   * @param  str
   *         The string to be written
   */
  void
  putString(std::string str);

  /**
   * <p>Returns the number of bytes between <i>position</i> and
   * <i>limit</i>.</p>
   *
   * @return   The number of bytes between <i>position</i> and
   * <i>limit</i>.</p>
   */
  size_t
  remaining();

  /**
   * <p>Returns this buffer's capacity.</p>
   *
   * @return  The capacity of this buffer
   */
  size_t
  capacity();

  /**
   * <p>Returns this buffer's position.</p>
   *
   * @return  The position of this buffer
   */
  size_t
  position();

  /**
   * <p>Returns this buffer's limit.</p>
   *
   * @return  The limit of this buffer
   */
  size_t
  limit();

  /**
   * <p>Sets this buffer's mark at its position.</p>
   *
   * @return  the position marked.
   */
  ssize_t
  mark();

  /**
    * <p>Returns the curent mark.</p>
    *
    * @return  the current mark.
    */
  ssize_t
  getMark();

  /**
   * <p>Discards the current mark, if set.</p>
   */
  void
  discardMark();

  /**
   * Resets this buffer's position to the previously-marked position.
   *
   * <p> Invoking this method neither changes nor discards the mark's
   * value.</p>
   *
   * @return  a boolean indicating whether the position was reset to the
   * mark or not.
   */
  bool
  reset();

  /**
   * Clears this buffer.  The position is set to zero, the limit is set to
   * the capacity, and the mark is discarded.
   *
   * <p> This method does not actually erase the data in the buffer, but it
   * is named as if it did because it will most often be used in situations
   * in which that might as well be the case.</p>
   */
  void
  clear();

  /**
   * <p>Flips this buffer.  The limit is set to the current position and then
   * the position is set to zero.  If the mark is defined then it is
   * discarded.</p>
   *
   * <p> This method is often used in conjunction with the compact() method
   * when transferring data from one place to another.</p>
   */
  void
  flip();

  /**
   * <p>Rewinds this buffer.  The position is set to zero and the mark is
   * discarded.</p>
   */
  void
  rewind();

  /**
   * <p>Sets this buffer's limit.  If the position is larger than the new limit
   * then it is set to the new limit.  If the mark is defined and larger than
   * the new limit then it is discarded.</p>
   *
   * @param  newLimit
   *         The new limit value; must be non-negative
   *         and no larger than this buffer's capacity
   *
   * @return  Success
   */
  bool
  setLimit(size_t limit);

  /**
   * <p>Sets this buffer's position.  If the mark is defined and larger than the
   * new position then it is discarded.</p>
   *
   * @param  newPosition
   *         The new position value; must be non-negative
   *         and no larger than the current limit
   *
   * @return  Success
   */
  bool
  setPosition(size_t newPosition);

protected:
  bool
  setMark(ssize_t m);


private:
  ByteBuffer(size_t size);

  /* Copied the layout of the VLB struct here for reference.
   * struct bu_vlb {
   unsigned long magic;
   unsigned char *buf; //Dynamic memory for the buffer
   size_t bufCapacity; //Current capacity of the buffer
   size_t nextByte;    //Number of bytes currently used in the buffer
   };*/

  /*
   * use vlb.nextByte for 'position'
   * Limit notates the position just after the last written byte.  Used
   * in conjunction with 'limit', a ByteBuffer can maintain a handle on
   * what position the good data extends to inside the vlb while still being
   * able to manipulate data anywhere in the vlb.
   */
  bu_vlb vlb;
  size_t lim;
  int32_t mar;
};

#endif /* __BYTEBUFFER_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */

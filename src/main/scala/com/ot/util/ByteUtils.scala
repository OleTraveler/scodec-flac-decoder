package com.ot.util

import java.math.BigInteger

/**
  * Created by tstevens on 4/11/17.
  */
object ByteUtils {

  /** Copied from https://gist.github.com/fbettag/3936752
    *
    * @throws IllegalArgumentException if offset + length > buf.length
    * */
  def byteToLong(buf: Array[Byte], offset: Int, length: Int): Either[IllegalArgumentException, Long] = {

    if (offset + length > buf.length) Left(new IllegalArgumentException(s"offset (${offset}) + length($length) > buf.length (${buf.length})"))
    else {
      var ret = 0L
      var i = offset
      while (i < offset + length) {
        ret = ((ret << 8) & 0xffffffff) + (buf(i) & 0xff)
        i += 1
      }
      Right(ret)
    }
  }

  def byteToInt(buf: Array[Byte], offset: Int, length: Int): Either[IllegalArgumentException, Int] = {
    if (offset + length > buf.length) Left(new IllegalArgumentException(s"offset (${offset}) + length($length) > buf.length (${buf.length})"))
    else {
      var ret = 0
      var i = offset
      while (i < offset + length) {
        ret = ((ret << 8) & 0xffffffff) + (buf(i) & 0xff)
        i += 1
      }
      Right(ret)
    }
  }


  def offsetToMask(offset: Int) : Byte = {
    offset match {
      case 0 => 0xFF.toByte
    }
  }

  private val BYTE_SIZE = 8

  /** Specify a bit offset (0 based) in an array of bytes and a length and the result will be
    * an integer representation of the bits popped off the array.
    *
    * For instance Array(0x77) is 0111 0111 and if we say offset 2, length 3 the result will be 101.
    * Due to oddity in the algorithm we can only read 24 chars consistently.  Use bitToLong if you need more.
    * @param buf The Byte array buffer.
    * @param offset The first bit to start with, 0 based index.
    * @param length The length
    * @return an Int
    * @throws IllegalArgumentException if offset + length > buf.length * 8 or if length > 32.
    */
  def bitToInt(buf: Array[Byte], offset: Int, length: Int): Either[IllegalArgumentException, Int] = {

    if (length > 24) Left(new IllegalArgumentException(s"length argument max is 24, but is ${length}"))
    else if (offset + length > buf.length * 8)
      Left(new IllegalArgumentException(s"arguments will result in reading past array end.  buf length: ${buf.length}, offset: ${offset}, length: ${length}"))
    else {

      //the first index to read of buf
      val firstIndex = offset / BYTE_SIZE

      //the first bit to read of buf(firstIndex)
      val firstBit = offset % BYTE_SIZE

      //value is negative if the last bit is in the same byte as the first bit
      val lengthAfterFirstIndex = length - (BYTE_SIZE - firstBit)
      val fullIndexes = lengthAfterFirstIndex / BYTE_SIZE

      //any remaining bits to read after the full indexes have been read.
      //note: could also be negative
      val lastBit = lengthAfterFirstIndex % BYTE_SIZE

      //mask, so only the bits we care about are 1's.
      //start with the byte part all 1's and shift right to create a mask of just the bits we care about.
      //so for instance if firstBits is 6, the byte part should be 00000011
      val firstMask = 0x000000ff >> firstBit

      var i = firstIndex
      val baseMask = 0x000000FF
      var ret = 0
      while (i <= fullIndexes + firstIndex) {

        val mask = if (i == firstIndex && lastBit >= 0) firstMask
          else if (i == firstIndex && lastBit < 0) {
            firstMask & (0xffffffff << Math.abs(lastBit))
          }
          else baseMask

        ret = ((ret << 8) & 0xffffffff) + (buf(i) & 0xff & mask)

        if (i == 0 && lastBit < 0) {
          ret = ret >> Math.abs(lastBit)
        }

        i += 1
      }

      if (lastBit > 0) {
        val mask = 0xffffff00 >> lastBit
        ret = ((ret << lastBit) & 0xffffffff) + ((buf(i) & mask & 0x000000ff) >> (BYTE_SIZE - lastBit))
        i = i + 1
      }

      Right(ret)
    }
  }


  /** Specify a bit offset (0 based) in an array of bytes and a length and the result will be
    * an integer representation of the bits popped off the array.
    *
    * For instance Array(0x77) is 0111 0111 and if we say offset 2, length 3 the result will be 101
    * @param buf The Byte array buffer.
    * @param offset The first bit to start with, 0 based index.
    * @param length The length
    * @return A signed int, so if the length = 32, it could be a negative number
    * @throws IllegalArgumentException if offset + length > buf.length * 8 or if length > 32.
    */
  def bitToLong(buf: Array[Byte], offset: Int, length: Int): Either[IllegalArgumentException, Long] = {

    if (length > 64) Left(new IllegalArgumentException(s"length argument max is 64, but is ${length}"))
    else if (offset + length > buf.length * 8)
      Left(new IllegalArgumentException(s"arguments will result in reading past array end.  buf length: ${buf.length}, offset: ${offset}, length: ${length}"))
    else {

      //the first index to read of buf
      val firstIndex = offset / BYTE_SIZE

      //the first bit to read of buf(firstIndex)
      val firstBit = offset % BYTE_SIZE

      //value is negative if the last bit is in the same byte as the first bit
      val lengthAfterFirstIndex = length - (BYTE_SIZE - firstBit)
      val fullIndexes = lengthAfterFirstIndex / BYTE_SIZE

      //any remaining bits to read after the full indexes have been read.
      //note: could also be negative
      val lastBit = lengthAfterFirstIndex % BYTE_SIZE

      //mask, so only the bits we care about are 1's.
      //start with the byte part all 1's and shift right to create a mask of just the bits we care about.
      //so for instance if firstBits is 6, the byte part should be 00000011
      val firstMask = 0x000000ff >> firstBit

      var i = firstIndex
      val baseMask = 0x0000FFFF
      var ret = 0l
      while (i <= fullIndexes + firstIndex) {

        val mask = if (i == firstIndex && lastBit >= 0) firstMask
        else if (i == 0 && lastBit < 0) {
          firstMask & (0xffffffff << Math.abs(lastBit))
        }
        else baseMask

        ret = ((ret << 8) & 0xffffffff) + (buf(i) & 0xff & mask)

        if (i == 0 && lastBit < 0) {
          ret = ret >> Math.abs(lastBit)
        }

        i += 1
      }

      if (lastBit > 0) {
        val mask = 0xffffff00 >> lastBit
        ret = ((ret << lastBit) & 0xffffffff) + ((buf(i) & mask & 0x000000ff) >> (BYTE_SIZE - lastBit))
        i = i + 1
      }

      Right(ret)
    }
  }


  /** Specify a bit offset (0 based) in an array of bytes and a length and the result will be
    * an integer representation of the bits popped off the array.
    *
    * For instance Array(0x77) is 0111 0111 and if we say offset 2, length 3 the result will be 101.
    * Due to oddity in the algorithm we can only read 24 chars consistently.  Use bitToLong if you need more.
    * @param buf The Byte array buffer.
    * @param offset The first bit to start with, 0 based index.
    * @param length The length
    * @return an Int
    * @throws IllegalArgumentException if offset + length > buf.length * 8 or if length > 32.
    */
  def bitToBigInteger(buf: Array[Byte], offset: Int, length: Int): Either[IllegalArgumentException, BigInteger] = {

    if (offset + length > buf.length * 8)
      Left(new IllegalArgumentException(s"arguments will result in reading past array end.  buf length: ${buf.length}, offset: ${offset}, length: ${length}"))
    else {

      //the first index to read of buf
      val firstIndex = offset / BYTE_SIZE

      //the first bit to read of buf(firstIndex)
      val firstBit = offset % BYTE_SIZE

      //value is negative if the last bit is in the same byte as the first bit
      val lengthAfterFirstIndex = length - (BYTE_SIZE - firstBit)
      val fullIndexes = lengthAfterFirstIndex / BYTE_SIZE

      //any remaining bits to read after the full indexes have been read.
      //note: could also be negative
      val lastBit = lengthAfterFirstIndex % BYTE_SIZE

      //mask, so only the bits we care about are 1's.
      //start with the byte part all 1's and shift right to create a mask of just the bits we care about.
      //so for instance if firstBits is 6, the byte part should be 00000011
      val firstMask = 0x000000ff >> firstBit

      var i = firstIndex
      val baseMask = 0x0000FFFF
      var ret = BigInteger.ZERO
      while (i <= fullIndexes + firstIndex) {

        val mask = if (i == 0 && lastBit >= 0) firstMask
        else if (i == 0 && lastBit < 0) {
          firstMask & (0xffffffff << Math.abs(lastBit))
        }
        else baseMask

        ret = ret.shiftLeft(8).add( new BigInteger(String.valueOf( buf(i) & 0xff & mask)) )

        if (i == 0 && lastBit < 0) {
          ret = ret.shiftRight(Math.abs(lastBit))
        }

        val ar = ret.toByteArray
        i += 1
      }

      if (lastBit > 0) {
        val mask = 0xffffff00 >> lastBit
        ret = ret.shiftLeft(lastBit).add(new BigInteger(String.valueOf( (buf(i) & mask & 0x000000ff) >> (BYTE_SIZE - lastBit)) ))
        i = i + 1
      }

      Right(ret)
    }
  }


}

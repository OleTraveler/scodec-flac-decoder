package com.ot.flac.decoder

import com.ot.flac.decoder.FrameHeader.BlockingStrategy
import scodec.bits._
import scodec.Codec
import scodec.codecs._

/**
  * Created by tstevens on 6/21/17.
  */
case class FrameHeader(blockingStrategy: BlockingStrategy)


object FrameHeader {

  sealed trait BlockingStrategy {
    val bitRep: Boolean
  }
  case object Fixed extends BlockingStrategy {
    override val bitRep = false
  }
  case object Variable extends BlockingStrategy {
    override val bitRep = true
  }

  def bvToBlockingStrategy(bitVector: BitVector) = if (bitVector(0)) Variable else Fixed

  val blockingCodec : Codec[BlockingStrategy] = bits(1).xmap[BlockingStrategy](bvToBlockingStrategy, bs => BitVector(bs.bitRep))

  val baseCodec = constant(bin"11111111111110") :: constant(bin"0") :: blockingCodec
}

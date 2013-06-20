package org.fmindex

import java.io.File
import org.apache.commons.io.FilenameUtils;

trait IBWTReader {
  def copyReverse(t:Array[Byte]):Int
  def isEmpty:Boolean
  var pos:Int = 0
  def close
  val filename:String = "IBWTReader"
}

class FileBWTReader(_filename:String) extends IBWTReader {
  override val filename = _filename
  val f = new File(filename)
  val in = new java.io.FileInputStream(f)
  val inb = new java.io.BufferedInputStream(in)
  var lastByte:Int = inb.read()
  def isEmpty = ( lastByte == -1 )

  def copyReverse(t:Array[Byte]):Int = {
    var i = t.length -1 
    var b = 0
    t(i) = lastByte.toByte
    i-=1
    while ( i >= 0 && b != -1 ) {
      b = inb.read()
      if ( b > 0) {
        t(i) = b.toByte
        pos+=1
        i-=1
      }
    }
    lastByte = if ( b != -1 ) inb.read() else -1 
    t.length - i - 1 
  }
  def close {
    inb.close()
    in.close()
  }
}


class StringBWTReader(_b:Array[Byte]) extends IBWTReader {
  val b:Array[Byte] = _b
  def copyReverse(t:Array[Byte]):Int = {
    var i = t.length -1 
    while ( i >= 0 && pos < b.length ) {
      t(i) = b(pos)
      pos+=1
      i-=1
    }
    t.length - i - 1 
  }
  def isEmpty:Boolean = pos == b.length
  def close {}
}

object BWTTempStorage {
  def genTmpFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    count+=1
    fileNameWithOutExt + "."  + count + ".tmp.bwt" 
  }

  def genFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    fileNameWithOutExt + ".bwt" 
  }
  def genAuxFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    fileNameWithOutExt + ".aux" 
  }
  var count = 0
}

class BWTTempStorage(basename:String,size:Int,eof:Int) {
  val filename = BWTTempStorage.genTmpFilename(basename)
  val f = new File(filename)
  val out = new java.io.FileOutputStream(f)
  val outb =  new java.io.BufferedOutputStream(out)
  val outd = new java.io.DataOutputStream(outb)
  var closed = false

  outd.writeLong(size.toLong)
  outd.writeLong(eof.toLong)

  def save(s:Array[Byte]) {
    outd.write(s,0,s.length)
  }
  def close {
    closed = true
    outd.close()
    out.close()
  }

  def convertToPermanent():File =  {
    if (!closed) close  

    val to = new File(BWTTempStorage.genFilename(basename))
    
    if (! f.renameTo(to)) {
      throw new Exception("cant rename")
    }
    to
  }
}

object BWTMerger2 {
  val ALPHA_SIZE = 256
}
class BWTMerger2(size:Int) {
  val t:Array[Byte] = new Array[Byte](size)
  val sa:Array[Int] = new Array[Int](size+1)
  val isa:Array[Int] = new Array[Int](size+1)

  def calcSA(t:Array[Byte],offset:Int) = {
    val sa = new SAISBuilder(new ByteArrayNulledOffsetWrapper(t,offset))
    sa.build()
    sa.SA.view.slice(1,sa.SA.length)
  }
  def calcOcc(t:Array[Byte],offset:Int) = {
    val l = t.length
    var i = 0 
    val occ = new Array[Long](BWTMerger2.ALPHA_SIZE)
    while (i < l) {
      occ(t(i))+=1
      i+=1
    }
    occ
  }
  def writeAuxFile(basename:String,occ:Array[Long]) = {
    val f = new File(BWTTempStorage.genAuxFilename(basename))
    val out = new java.io.FileOutputStream(f)
    val outb =  new java.io.BufferedOutputStream(out)
    val outd = new java.io.DataOutputStream(outb)
    var i = 0
    val l = occ.length
    while (i< l) {
      outd.writeLong(occ(i))
      i+=1
    }
    outd.close
    outb.close
    out.close
    f
  }
  def firstSegmentBWT(sa:IndexedSeq[Int],t:IndexedSeq[Byte]):Array[Byte] = {
    val n = sa.length
    val bwt = new Array[Byte](n+1)
    bwt(0) = t(n-1)                   // text[n-1] is the first bwt char
    var i = 0
    while ( i < n ) {
      var j = sa(i)
      if ( j == 0 ) {                  // rank of text[0..]
        bwt(i+1) = bwt(i)              // to help compression
      } else 
        bwt(i+1) = t(j-1)      
      i+=1
    }

    bwt
  }

  def merge(r:IBWTReader):Pair[File,File] = {
    val size = 1024
    var n = r.copyReverse(t)
    
    val sa = calcSA(t,t.length-n)
    val occGlobal = calcOcc(t,t.length-n)

    val newRank0 = sa.indexOf(0)
    val bwtTs = new BWTTempStorage(r.filename,n+1,newRank0+1)
    bwtTs.save(firstSegmentBWT(sa,t))

    bwtTs.close
    if ( ! r.isEmpty ) {

    }
    while ( ! r.isEmpty ) {

    }
    r.close
    val auf = writeAuxFile(r.filename,occGlobal)
    val bwtf = bwtTs.convertToPermanent
    (bwtf,auf)
  }
}

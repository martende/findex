package org.fmindex

import Util._
import java.io.File

import org.apache.commons.io.FilenameUtils
import scala.collection.mutable.BitSet
import java.io.RandomAccessFile

object BWTTempStorage {
  def genTmpFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    count+=1
    fileNameWithOutExt + "."  + count + ".tmp.bwt" 
  }

  def genBWTFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    fileNameWithOutExt + ".bwt" 
  }  
  def genAuxFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    fileNameWithOutExt + ".aux" 
  }
  def genWFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    fileNameWithOutExt + ".w" 
  }
  def genCacheFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    fileNameWithOutExt + ".cache" 
  }
  def genFMFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    fileNameWithOutExt + ".fm" 
  }
  def genLCPFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    fileNameWithOutExt + ".lcp" 
  }
  def genDataFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    fileNameWithOutExt + ".data" 
  }
  def genSAFilename(s:String) = {
    val fileNameWithOutExt = FilenameUtils.removeExtension(s)
    fileNameWithOutExt + ".sa" 
  }
  
  var count = 0
}

class BWTTempStorageIN(f:File) {
  
  val in  = new java.io.FileInputStream(f)
  val inb = new java.io.BufferedInputStream(in)
  val ind = new java.io.DataInputStream(inb)
  
  val size = ind.readLong()
  val eof =  ind.readLong()
  
  var closed = false
  
  def read() = ind.read()

  def close {
    closed = true
    ind.close()
    inb.close()
    in.close()
  }

}

class BWTTempStorage(_basename:String,_size:Int,_eof:Int) {
  val basename = _basename
  val filename = BWTTempStorage.genTmpFilename(basename)
  val f = new File(filename)
  val out = new java.io.FileOutputStream(f)
  val outb =  new java.io.BufferedOutputStream(out)
  val outd = new java.io.DataOutputStream(outb)
  val size = _size
  val eof = _eof
  var closed = false
  
  outd.writeLong(size.toLong)
  outd.writeLong(eof.toLong)

  def save(s:Array[Byte]) =  outd.write(s)
  def save(c:Byte) = outd.write(c)
  
  def inStorage = new BWTTempStorageIN(f)

  def close {
    closed = true
    outd.close()
    out.close()
  }

  def convertToPermanent():File =  {
    if (!closed) close  

    val to = new File(BWTTempStorage.genBWTFilename(basename))
    
    if (! f.renameTo(to)) {
      throw new Exception("cant rename")
    }
    to
  }

  def blockTransfer(r:BWTTempStorageIN,n:Int,c:Option[Byte]) {
    var i = 0
    while (i < n) {
      val b = r.read()
      assert(b != -1)
      outd.write(b)
      i+=1
    }
    c match {
      case Some(oc) => outd.write(oc)
      case None => 
    }
  }
  def remove() {
    if ( ! closed ) close
    f.delete()
  }
}

class AUXLoader(f:File,bigEndian:Boolean=true) {
    val in = new java.io.FileInputStream(f)
    val inb = new java.io.DataInputStream(in)
    val occ   = new Array[Long](BWTMerger2.ALPHA_SIZE)
    
    for (i<-0 until occ.length) {
      occ(i) = if ( ! bigEndian ) java.lang.Long.reverseBytes(inb.readLong()) else inb.readLong()
    }

    inb.close
    in.close
    def == (that: AUXLoader): Boolean = occ.sameElements(that.occ)
}

class BWTLoader(f:File,bigEndian:Boolean=true) {
  val in = new java.io.FileInputStream(f)
  val inb = new java.io.DataInputStream(in)
  val inr = new RandomAccessFile(f,"r")

  val headerOffset = 0x10  
  val size = if (bigEndian) inb.readLong() else java.lang.Long.reverseBytes(inb.readLong())
  val eof =  if (bigEndian) inb.readLong() else java.lang.Long.reverseBytes(inb.readLong())

  if ( size + headerOffset != f.length  ) throw new Exception("File %s bad size %d != %d + 16 ".format(f.toString,size,f.length))

  def read(i:Int) = {
    if ( i == eof ) {
      0
    } else {
      inr.seek(headerOffset+i)
      inr.read()
    }
  }

  def readAll() = {
    val t = new Array[Byte](size.toInt)
    inb.read(t)
    t
  }
  def close() {
    inr.close
    inb.close
    in.close    
  }
}

class LCPLoader(f:File) {
  val in = new java.io.FileInputStream(f)
  val inb = new java.io.DataInputStream(in)
  val inr = new RandomAccessFile(f,"r")
  val size = f.length.toInt
  val elSize = 4
  val headerOffset = 0x0  

  def read(i:Int) = {
    inr.seek(headerOffset+i*elSize)
    inr.readInt()
  }

  def readAll() = {
    val b = new Array[Byte](size )
    val outInts   = new Array[Int](size / elSize )
    val n = b.length
    var i = 0
    var j = 0 
    inb.read(b)
    while ( i < n) {
      outInts(j)= ( b(i+3) & 0xFF ) |( (b(i+2) & 0xFF) << 8 )|
            ( (b(i+1) & 0xFF) << 16 )|
            ( (b(i+0) & 0xFF) << 24 ) 
      j+=1
      i+=elSize
    }
    outInts
  }

  def close() {
    inr.close
    inb.close
    in.close    
  } 
}


class SALoader(f:File) {
  val in = new java.io.FileInputStream(f)
  val inb = new java.io.DataInputStream(in)
  val inr = new RandomAccessFile(f,"r")
  val size = f.length.toInt
  val elSize = 4
  val headerOffset = 0x0  

  def read(i:Int) = {
    inr.seek(headerOffset+i*elSize)
    inr.readInt()
  }

  def readAll() = {
    val b = new Array[Byte](size )
    val outInts   = new Array[Int](size / elSize )
    val n = b.length
    var i = 0
    var j = 0 
    inb.read(b)
    while ( i < n) {
      outInts(j)= ( b(i+3) & 0xFF ) |( (b(i+2) & 0xFF) << 8 )|
            ( (b(i+1) & 0xFF) << 16 )|
            ( (b(i+0) & 0xFF) << 24 ) 
      j+=1
      i+=elSize
    }
    outInts
  }

  def close() {
    inr.close
    inb.close
    in.close    
  } 
}


class FMLoader(f:File,bigEndian:Boolean=true) {
  

  val in = new java.io.FileInputStream(f)
  val inb = new java.io.DataInputStream(in)
  val inr = new RandomAccessFile(f,"r")
  val headerOffset = 0x09
  val elSize:Int = inb.read()
  val size =  if (bigEndian) inb.readLong() else java.lang.Long.reverseBytes(inb.readLong())
  if ( elSize!=0x04  )  throw new Exception("File %s bad elSize %d".format(elSize))
  if ( size*elSize + headerOffset != f.length  ) throw new Exception("File %s bad size %d + 0x9 != %d(filelen) ".format(f.toString,size,f.length))

  def read(i:Int) = {
    inr.seek(headerOffset+i*elSize)
    inr.readInt()
  }

  def readAll() = {
    val b = new Array[Byte](size.toInt * elSize)
    val outInts   = new Array[Int](size.toInt )
    val n = b.length
    var i = 0
    var j = 0 
    inb.read(b)
    while ( i < n) {
      outInts(j)= ( b(i+3) & 0xFF ) |( (b(i+2) & 0xFF) << 8 )|
            ( (b(i+1) & 0xFF) << 16 )|
            ( (b(i+0) & 0xFF) << 24 ) 
      j+=1
      i+=4
    }
    outInts
  }
  def close() {
    inb.close
    inr.close
    in.close    
  }
}

class StringPosReader(f:RandomAccessFile,_pos:Int,_bufCap:Int=4048) extends Iterator[Char] {
  val bufCap = _bufCap
  val buf:Array[Byte] = new Array[Byte](bufCap)
  var bufpos = 0
  var fpos   = _pos
  f.seek(fpos)
  var bufSize = f.read(buf)
    
  def hasNext:Boolean = if (bufSize <= 0) false 
    else if (bufpos == bufSize) {
      if (bufCap!=bufSize) false else {
        fpos+=bufSize
        f.seek(fpos)
        bufSize = f.read(buf)
        bufpos=0
        if ( bufSize <= 0) false else buf(bufpos)!=0
      }
    } else buf(bufpos)!=0
  
  def next():Char = {
    if ( hasNext ) {
      val c = buf(bufpos)
      bufpos+=1
      c.toChar
    } else {
      ???
    }
  }
}

class LCPSearcher(filename:String,bigEndian:Boolean=true) extends NaiveFMSearcher(filename,bigEndian) with LCPSuffixWalkingAlgo {
  val lcpl = new LCPLoader(new File(BWTTempStorage.genLCPFilename(filename)))
  val sal = new SALoader(new File(BWTTempStorage.genSAFilename(filename)))
  val dataFile = new File(BWTTempStorage.genDataFilename(filename))
  val fsize = dataFile.length.toInt
  val inr  = new RandomAccessFile(dataFile,"r")
  def getLCP(i: Int): Int =  lcpl.read(i)
  def getStringOn(i:Int):Iterator[Char] = {
    println("getStringOn(%d) sa=%d".format(i,sal.read(i)))
    new StringPosReader(inr,fsize-sal.read(i))
  }
}

class NaiveFMSearcher(filename:String,bigEndian:Boolean=true) extends SuffixWalkingAlgo {
  val aux = new AUXLoader(new File(BWTTempStorage.genAuxFilename(filename)),bigEndian=bigEndian)
  val fm = new FMLoader(new File(BWTTempStorage.genFMFilename(filename)),bigEndian=bigEndian)
  val bwt = new BWTLoader(new File(BWTTempStorage.genBWTFilename(filename)),bigEndian=bigEndian)
  val n = fm.size.toInt
  // buckets without 0 
  val bucketStarts0 = {
    val c = aux.occ.map{_.toInt}
    //c(0)=1
    bwtstring.c2bs(c)
  }
  val bucketStarts = {
    val c = aux.occ.map{_.toInt}
    c(0)=1
    bwtstring.c2bs(c)
  }

  def cf(c:Int):Int = bucketStarts(c)
  val K = bucketStarts.length
  def occ(c:Int,key:Int):Int = {
    val istart = bucketStarts(c) 
    var imin = istart
    var imax = if ( c==K-1 ) n-1 else bucketStarts(c+1)-1
    
    if (imin <= imax) {
      var found = false
      var imid:Int = 0
      var ival:Int = 0
      while (! found && imax >= imin) {
        imid = (imax+imin) / 2
        ival = fm.read(imid)
        if (ival < key)
          imin = imid + 1
        else if (ival > key)
          imax = imid - 1
        else
          found = true
      }
      if (ival <= key) (imid - istart+1) else (imid - istart ) 
    } else 0
  }
  def pos2char(key:Int) = {
    var i = K-1
    if ( bucketStarts0(i)>key)
      while (bucketStarts0(i)>key && i>0) i-=1
    else {
      while (bucketStarts0(i-1)==bucketStarts0(i) && i>1) i-=1
      i-=1
    }
    i
  }
  def getPrevI(i:Int) = {
    val c = bwt.read(i)
    cf(c) + occ(c,i - 1)
  }
  def getNextI(i:Int) = {
    fm.read(i)
  }

  def nextSubstr(sp:Int,len:Int):String = {
    var cp = getNextI(sp)
    var ret = new StringBuilder()
    var eof:Boolean = false
    for ( i <- 0 until len if (!eof )) {
      val b = bwt.read(cp)
      eof = (b == 0)
      ret.append(b.toChar)
      cp = getNextI(cp)
    }
    ret.reverse.toString
  }
  
  def prevSubstr(sp:Int,len:Int):String = {
    var cp = sp
    var ret = new StringBuilder()
    var eof:Boolean = false
    for ( i <- 0 until len if ( ! eof )) {
      val b = bwt.read(cp)
      ret.append(b.toChar)
      cp = getPrevI(cp)
    }
    ret.result
  }

}


class FMCreator(filename:String,bufferSize:Int=1024,debugLevel:Int=0,bigEndian:Boolean=true) {
  import java.io.RandomAccessFile
  import java.io.FileInputStream
  import java.io.BufferedInputStream

  val f = new File(filename)
  if (!f.exists()) throw new Exception("File %s does not exists".format(filename))
  
  val aux = new AUXLoader(new File(BWTTempStorage.genAuxFilename(filename)),bigEndian=bigEndian)
  val bwt = new BWTLoader(new File(BWTTempStorage.genBWTFilename(filename)),bigEndian=bigEndian)
  val inbb = new java.io.BufferedInputStream(bwt.inb)

  val occ = aux.occ

  val outf = new File(BWTTempStorage.genFMFilename(filename))
  
  val bucketStarts = {
    var i = 1
    val bs = new Array[Long](BWTMerger2.ALPHA_SIZE)    
    var tot:Long = 1
    while (i < BWTMerger2.ALPHA_SIZE) {
      bs(i)+=tot
      tot+=occ(i)
      i+=1
    }
    bs
  }
  
  def create(bigEndian:Boolean=this.bigEndian,progressBar:Boolean=false):File = {
    var tmpBuffer:Array[Byte]  = null

    val outb = new RandomAccessFile(outf,"rw")
    val bwtlen = bwt.size
    val eofNum = bwt.eof
    
    var readb = 0
    val bkt=bucketStarts.clone()
    
    var i = 0L
    val progress = ConsoleProgress("Progress",100)

    val elSize:Byte = if ( bwtlen < 0xffffffffL ) 4 else 8
    // How oft refreshen progressbar - should be abhangig von bwtlen
    val pCoeff = 10000

    if ( elSize == 8 ) ??? 

    tmpBuffer = new Array[Byte](bufferSize)

    val bucketBuferSize = bufferSize/BWTMerger2.ALPHA_SIZE
    val bucketOffsets   = new Array[Int](BWTMerger2.ALPHA_SIZE)
    def getStartForChar(b:Int) = bucketBuferSize * b 
    def setIntValOn(i:Int,v:Int) {
      tmpBuffer(i+0) = (v >> 24).toByte;
      tmpBuffer(i+1) = (v >> 16).toByte;
      tmpBuffer(i+2) = (v >> 8).toByte;
      tmpBuffer(i+3) = (v /*>> 0*/).toByte;
    }
    // Header
    outb.write(elSize) // Item Size 
    
    outb.writeLong(bwtlen) // Item Size 

    val headerOffset = outb.getChannel.position
    outb.setLength(headerOffset)
    
    var readn = 0
    while (readb != -1) {
      readb = inbb.read()
      if ( readn == eofNum ) readb = 0

      if (readb != -1  ) {
        setIntValOn(bucketOffsets(readb) + getStartForChar(readb) , i.toInt )
        bucketOffsets(readb)+=elSize
        if ( bucketOffsets(readb) == bucketBuferSize) {
          val j = bkt(readb)
          outb.seek(j*elSize+headerOffset)
          val bs = getStartForChar(readb)
          //printf("Write %d items for char '%c' at offset %d\n",bucketBuferSize/4,readb,j*elSize+headerOffset)

          outb.write(tmpBuffer,bs,bucketBuferSize)
          bkt(readb)+=bucketBuferSize/elSize
          bucketOffsets(readb)=0
        }
        i+=1
      }
      readn+=1      
      if (progressBar &&  i % pCoeff == 0)
        progress(i.toFloat/bwtlen)
      
    }
    //printf("Readed %d towrite %d\n",readn,(bucketOffsets.sum))
    for ( i<- 0 until BWTMerger2.ALPHA_SIZE) {
      if ( bucketOffsets(i) != 0 ) {
          val j = bkt(i)
          outb.seek(j*elSize+headerOffset)
          val bs = getStartForChar(i)
          outb.write(tmpBuffer,bs,bucketOffsets(i))
          //printf("Write %d items for char '%c' at offset %d occ(%c)=%d %s\n",bucketOffsets(i)/4,i,j*elSize+headerOffset,i,occ(i),occ(i)!=bucketOffsets(i)/4)
      }
    }
    

    //outb.setLength(outb.getChannel.position)

    inbb.close()
    outb.close()
    outf
  }
}

class SACreator(filename:String,debugLevel:Int=0,bigEndian:Boolean=true) {
  val bwt = new BWTLoader(new File(BWTTempStorage.genBWTFilename(filename)),bigEndian=bigEndian)
  val fml = new FMLoader(new File(BWTTempStorage.genFMFilename(filename)),bigEndian=bigEndian)

  val outf = new File(BWTTempStorage.genSAFilename(filename))
  
  def create(progressBar:Boolean=false):File = {
    val n = bwt.size
    var i = bwt.eof.toInt
    var j = 0
    val outb = new RandomAccessFile(outf,"rw")

    while ( j < n) {
      outb.seek(i*4)
      outb.writeInt(j)
      i = fml.read(i)
      j+=1
    }
    outb.close()
    outf    
  }
}

class LCPCreator(filename:String,debugLevel:Int=0,bigEndian:Boolean=true) {
  import java.io.RandomAccessFile
  import java.io.FileInputStream
  import java.io.BufferedInputStream

  val f = new File(filename)
  if (!f.exists()) throw new Exception("File %s does not exists".format(filename))
  
  val aux = new AUXLoader(new File(BWTTempStorage.genAuxFilename(filename)),bigEndian=bigEndian)
  val bwt = new BWTLoader(new File(BWTTempStorage.genBWTFilename(filename)),bigEndian=bigEndian)
  val fml = new FMLoader(new File(BWTTempStorage.genFMFilename(filename)),bigEndian=bigEndian)

  //val inbb = new java.io.BufferedInputStream(bwt.inb)

  val occ = aux.occ

  val outf = new File(BWTTempStorage.genLCPFilename(filename))
  
  val bs = {
    var i = 1
    val bs = new Array[Long](BWTMerger2.ALPHA_SIZE)    
    var tot:Long = 1
    while (i < BWTMerger2.ALPHA_SIZE) {
      bs(i)+=tot
      tot+=occ(i)
      i+=1
    }
    bs
  }
  
  def create(progressBar:Boolean=false):File = {
    val n = fml.size
    var i = 0
    var nextk = 0
    var k = bwt.eof.toInt
    var h = 0
    val outb = new RandomAccessFile(outf,"rw")

    def ibs2c(i:Int):Int = {
      assert(i < n)
      val j = bs.indexWhere(i < _,0)
      j-1
    }
    def iterChar(_j:Int,_h:Int,_temp:Int) = {
      var h = _h
      var j = _j
      var temp = _temp        
      if (h != 0 && temp == -1) {
        var t = 0
        while ( h > 0 ) {
          j = fml.read(j)
          h -= 1
        }
        temp = j
      } else {
        if ( temp != -1 ) {
          j = fml.read(temp)
          temp = j
        }
      }
      (temp,ibs2c(j))
    }

    while ( i < n) {
      if (k==0) {
        outb.seek(0)
        outb.writeInt(0)
      } else {
        var temp1 = -1
        var temp2 = -1
        var j = k-1
        var stop = false
        while(  i+h < n && ! stop) {
          val ic1 = iterChar(k,h,temp1)
          val ic2 = iterChar(j,h,temp2)
          if ( ic1._2 == ic2._2) {
            temp1 = ic1._1
            temp2 = ic2._1
            h += 1
          } else {
            stop=true
          }
        }
        outb.seek((k-1)*4)
        outb.writeInt(h)
      }
      if( h > 0 ) h -= 1
      k=fml.read(k)
      i+=1
    }

    outb.close()
    outf
  }
}

object BWTMerger2 {
  val ALPHA_SIZE = 256
}

class BWTMerger2(size:Int,debugLevel:Int=0) {
  val t1:Array[Byte] = new Array[Byte](size)
  val t2:Array[Byte] = new Array[Byte](size)
  val sa:Array[Int] = new Array[Int](size+1)
  val isa:Array[Int] = new Array[Int](size+1)

  def debug(l:Int,s: =>String  ) = if (l<=debugLevel) println(s)

  def calcSA(t:Array[Byte],offset:Int=0) = {
    val tm = System.nanoTime()
    val sa = new SAISBuilder(new ByteArrayNulledOffsetWrapper(t,offset))
    sa.build()

    saisLastTime = System.nanoTime() - tm
    saisTotalTime += saisLastTime
    if ( debugLevel >= 5)
      sa.printSA()
    sa.SA.view.slice(1,sa.SA.length)
  }


  def remapAlphabet(t:IndexedSeq[Byte],gtEof:BitSet) = {
    def create_occ() = {
      val n = t.length - 1
      val occ = new Array[Int](BWTMerger2.ALPHA_SIZE+2)
      var i = 0
      while ( i < n ) {
        val ti=t(i)&0xff
        val tn=t(n)&0xff
        if ( ti < tn || ( ti == tn && ! gtEof(i+1)) ) {
          occ(ti)+=1
        } else {
          occ(ti+2)+=1
        }
        i+=1
      }
      occ((t(n)&0xff)+1)+=1
      occ
    }

    def create_map(occ:Array[Int]) = {
      val map = new Array[Int](BWTMerger2.ALPHA_SIZE+2)
      val oclen = occ.length
      var asize = 1
      var i = 0
      while (i < oclen) {
        if ( occ(i)>0) {
          map(i)=asize
          asize+=1
        } else {
          map(i)=oclen
        }
        i+=1
      }
      (map,asize)
    }

    def create_mapped_string(map:Array[Int]) = {
      var i = 0
      val n = t.length
      val n_1 = n -1 
      val newt = new Array[Int](n+1)
      while ( i < n ) {
        val c = if ( i == n - 1 ) (t(i)&0xff)+1 
          else if ( ( t(i) & 0xff )< ( t(n_1) & 0xff) || ( t(i) == t(n_1) && ! gtEof(i+1) ) ) (t(i)&0xff)
          else (t(i)&0xff)+2
        newt(i) = map(c)
        i+=1
      }
      newt(n)=0
      newt
    }
    val occ = create_occ()
    val (map,asize) = create_map(occ)
    (create_mapped_string(map),asize)
  }

 def kmpPreifx(t:IndexedSeq[Byte]):Array[Int] = {
    var q = 1
    var n = t.length
    var k = 0
    val prefix = new Array[Int](n)
    while (q < n) {
      while (t(q) != t(k) && k!=0) k = prefix(k-1)
      if (t(q) == t(k)) k+=1
      prefix(q) = k
      q+=1
    }
    prefix
  }
  // Create new t1..t2
  def computeGtEof(t1:IndexedSeq[Byte],t2:IndexedSeq[Byte],gtTn:BitSet) = {
    val n = t1.length
    val gt_eof = BitSet()
    val kmp_shift = kmpPreifx(t2)
    var i = 0
    var startj = 0
    while ( i < n) {
      var j = startj 
      while ( i + j  != n  && t1(i+j) == t2(j) ) j+=1
      if ( i + j  == n ) {
        gt_eof(i) = ! gtTn(j)
      } else {
        gt_eof(i) = ( ( t1(i+j) & 0xff ) > ( t2(j) & 0xff ) )
      }
      if ( j==0 ) {
        startj = 0 
        i+=1
      } else {
        val k = kmp_shift(j-1)
        startj=k
        var h=1
        val m=j-k
        assert(k < j)
        while (h < m) {
          gt_eof(i+h) = gtTn(h)
          h+=1
        }
        i+=m
      }
    }
    gt_eof
  }

  def sa2BWT(sa:IndexedSeq[Int],t:IndexedSeq[Byte]):Array[Byte] = {
    val n = sa.length
    val bwt = new Array[Byte](n)
    var i = 0
    var rank0 = -1
    while ( i < n ) {
      var j = sa(i) - 1
      if ( j < 0) {
        rank0 = i
        j = n -1 
      }
      bwt(i)=t(j)
      i+=1
    }
    
    assert(rank0>=0,"rank0 not found")

    // take care of the position where the eof symbol should go
    // writing a nearby symbol to help (run-length) compression 

    if (rank0 >0)
      bwt(rank0) =  bwt(rank0-1)
    else if ( bwt.length != 1 ) {
      bwt(rank0) =  bwt(rank0+1)
    }
    
    bwt
  }

  def calcOcc(t:IndexedSeq[Byte]) = {
    val l = t.length
    var i = 0 
    val occ = new Array[Long](BWTMerger2.ALPHA_SIZE)
    while (i < l) {
      occ(t(i)&0xff)+=1
      i+=1
    }
    occ
  }

  def calcBs(occ:Array[Long]) = {
    var i = 0
    val bs = new Array[Long](BWTMerger2.ALPHA_SIZE)    
    var tot:Long = 0
    while (i < BWTMerger2.ALPHA_SIZE) {
      bs(i)+=tot
      tot+=occ(i)
      i+=1
    }
    bs
  }
  def updateOcc(to:Array[Long],from:Array[Long]) {
    var i = 0 
    while (i < BWTMerger2.ALPHA_SIZE) {
      to(i)+=from(i)
      i+=1
    }
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
  def calcGtTn(newRank0:Int,sa:IndexedSeq[Int]):BitSet = {
    val gtTn = BitSet()
    var i = newRank0 + 1
    val n = sa.length
    while ( i < n ) {
      gtTn.add(sa(i))
      i+=1
    }
    gtTn
  }
  def recalcGtTn(_bucketStarts:Array[Long],bwt:Array[Byte],rankFirst:Int,rankLast:Int) = {
    val gtTn:BitSet = new BitSet()
    val n = bwt.length
    
    val bucketStarts = _bucketStarts.clone()
    val rankprev = new Array[Int](n)
    var i = 0
    while (i < n) {
      if ( i != rankFirst) {
        val j = bwt(i) & 0xff
        rankprev(i) = bucketStarts(j).toInt
        bucketStarts(j)+=1
        if (rankprev(i) == rankLast ) {
          rankprev(i) = bucketStarts(j).toInt
          bucketStarts(j)+=1
        }
      } else {
        rankprev(i) = n
      }
      i+=1
    }
    i = rankLast
    var j = n -1 
    while ( j > 0 ) {
      gtTn(j)=(i > rankFirst)
      i = rankprev(i)
      j-=1
    }
    gtTn
  }
  
  def completeKmpFilling(kmp:KMPBuffer,t2:Array[Byte],gtTn:BitSet) {
    def DEBUGdumpi = {
      val out = new java.io.FileOutputStream(new java.io.File("/tmp/smerge.txt"))
      val outb =  new java.io.BufferedOutputStream(out)

      var i = t2.length - 1
      while ( i > 0 ) {
        outb.write("%d\n".format(if (gtTn(i)) 1 else 0 ).getBytes)
        i-=1
      }
      outb.close
    }
    
    var i = t2.length - 1
    while ( i > 0 ) {
      kmp.addChar(t2(i),gtTn(i))
      i-=1
    }
  }
  
  def calcSAStatistic(t:IndexedSeq[Byte],bucketStarts:Array[Long],gtEof:BitSet) = {
    val (remapped,asize) = remapAlphabet(t,gtEof)
    val tm = System.nanoTime()
    val sab = new SAISIntBuilder(new IntArrayWrapper(remapped),asize)
    sab.build()
    saisLastTime = System.nanoTime() - tm
    saisTotalTime += saisLastTime
    
    val sa = sab.SA.view.slice(1,sab.SA.length)
    
    val bwt = sa2BWT(sa,t)
    val rankFirst = sa.indexOf(0)
    val rankLast  = sa.indexOf(sa.length-1)
    val searcher = new NaiveBWTSearcher(bwt,bucketStarts,rankFirst)

    assert(bwt.length==t.length,"bwt.len(%d) != n(%d)".format(bwt.length,t.length))

    (bwt,searcher,rankFirst,rankLast)
  }

  def longSuffixCmp(idx:Int,localPfx:Array[Byte],tLast:Array[Byte]) = {
    var k = KMPBuffer.PFX_BUFFER_SIZE-1
    var ret:Int = 0
    var i = idx
    assert(idx>=0,"longSuffixCmp: idx = %d ".format(idx))
    while (ret == 0) {
      if ( (tLast(k) & 0xff ) > (localPfx(i % KMPBuffer.PFX_BUFFER_SIZE)  &0xff) )ret = -1
      else if ( (tLast(k) & 0xff )< (localPfx(i % KMPBuffer.PFX_BUFFER_SIZE) &0xff ) )ret = 1
      else {
        k-=1
        assert(k >= 0,"Illegal lcp in longSuffixCmp")
        i-=1
        if ( i < 0) ret = -1 
      }
    }
    ret
  }
  /*
  if ( todebug > 0) {
    import java.io.FileWriter
    val fw = new FileWriter("/tmp/t.txt", true) ; 
    fw.write("compute_prefix_rank - cur_rank - 1 = %d c = %c rank = %d\n".format(curRank-1,c,searcher.occ(c,curRank-1))) ; 
    fw.close()

    todebug = todebug - 1
  }
  */
  def calcGaps(r:IBWTReader,searcher:SuffixAlgo,kmpIn:KMPBuffer,kmpOut:KMPBuffer,bwt:Array[Byte],lastChar:Byte,numOldSuf:Long,bucketStarts:Array[Long],rk0:Int,rklst:Int) = {
    val n = bwt.length
    val gaps = new Array[Int](n+1)
    val pfxBuffer = new Array[Byte](KMPBuffer.PFX_BUFFER_SIZE)
    var c = r.getByte.toByte
    var curRank = bucketStarts(c&0xff).toInt
    pfxBuffer(0) = c
    gaps(0)+=1
    gaps(curRank)+=1
    if (kmpIn != null ) kmpIn.addChar(c,curRank>rk0)
    var i = 1
    while (i < numOldSuf) {
      val ogt = kmpOut.revisitChar(c)
      c = r.getByte.toByte
      val cFirst = bucketStarts(c&0xff).toInt
      val oldRank = curRank
      curRank = if ( curRank == 0 ) cFirst else {
        cFirst + searcher.occ(c,curRank-1)
      }

      if ( c == lastChar) {
        if ( curRank == rklst) {
          ogt match {
            case Some(gt) =>  if (gt) curRank+=1 
            case None     =>  
              if ( longSuffixCmp(i-1,pfxBuffer,kmpOut.string ) > 0 ) curRank+=1
          }
        } else if (curRank > rklst) {
          curRank+=1
        }
      }
      pfxBuffer(i%KMPBuffer.PFX_BUFFER_SIZE) = c
      gaps(curRank)+=1
      if (kmpIn != null) kmpIn.addChar(c,curRank>rk0)
      i+=1
    }
    r.close

    assert(kmpOut.chars_seen==0,"kmpOut.chars_seen = %d".format(kmpOut.chars_seen))
    assert(gaps.sum==numOldSuf+1,"GAPS checking OK")

    gaps
  }

  def mergeTemp(oldStorage:BWTTempStorage,gaps:Array[Int],bwt:Array[Byte],curRank0:Int,_lastChar:Byte):BWTTempStorage = {
    var lastChar = _lastChar
    val n = bwt.length
    val oldEof = oldStorage.eof
    var newOef = 0
    var i = 0

    while ( i < curRank0) {
      newOef += (gaps(i) + 1)
      i+=1      
    }
    newOef += gaps(i)
    val bwtTs = new BWTTempStorage(oldStorage.basename,oldStorage.size+n,newOef)
    val bwtIn = oldStorage.inStorage
    var tot = 0
    i = 0
    while ( i <=n ) {
      val gi = gaps(i)
      
      val nextBwtChar = if (i < n) {
        if (i == curRank0) assert(tot + i + gi == newOef)
        Some(bwt(i))
      } else None

      if ( tot > oldEof || (tot + gi <= oldEof )) {
        bwtTs.blockTransfer(bwtIn,gi,nextBwtChar)
      } else {
        bwtTs.blockTransfer(bwtIn,oldEof-tot,Some(lastChar))
        lastChar = bwtIn.read().toByte
        bwtTs.blockTransfer(bwtIn,gi-(oldEof-tot)-1,nextBwtChar)
      }

      tot += gi
      i+=1
    }
    bwtIn.close
    bwtTs.close
    bwtTs
  }

  def time[R](block: => R,evaled:Long => Unit): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    evaled(t1-t0)
    result
  }

  var step = 0
  var mergeStartTime:Long = _
  var mergeTotalTime:Long = _
  var mergeLastStepTime:Long = _
  var firstBlockTime:Long = _
  var saisLastTime:Long = _
  var saisTotalTime:Long = _
  var datainpLastTime:Long = _ 
  var datainpTotalTime:Long = _ 
  var calcGapsLastTime:Long = _ 
  var calcGapsTotalTime:Long = _ 

  def merge(r:IBWTReader):Pair[File,File] = {
    var i = 0 
    var gtTn:BitSet = null
    var kmpIn:KMPBuffer = null
    var kmpOut:KMPBuffer = null

    def speedF(x:Double):String = if (x > 1024 * 1024 / 2 ) 
          "%.1f M/sec".format(x/(1024*1024) )
        else if (x > 1024 / 2 )
          "%.1f k/sec".format(x/(1024) )
        else 
          "" + x.toInt + " b/sec"
    def bytesF(x:Int):String = if (x > 1024 * 1024 / 2 ) 
          "%.1f Mb".format(x.toFloat/(1024*1024) )
        else if (x > 1024 / 2 )
          "%.1f kb".format(x.toFloat/(1024) )
        else 
          "" + x.toInt + " bytes"
    debug(1,"BWTMerger2.merge: Start BufSize=%d debugLevel=%d".format(t1.length,debugLevel))
    mergeStartTime = System.nanoTime()
    
    var n = time({ 
      r.copyReverse(t1) 
    },{
      x:Long => datainpLastTime=x;datainpTotalTime+=x
    })

    var first:Long = 0
    var last:Long = n
    val sa = calcSA(t1,t1.length-n)
    val t1slice = t1.view.slice(size-n,t1.length)
    if ( debugLevel >=4 ) {
      println("------------------------------------")
      println(t1slice.reverse.map{_.toChar}.mkString(""))
      /*
      println("------------------------------------")
      println(t1slice.reverse.mkString(","))
      */
      println("------------------------------------")
    }

    val occGlobal = calcOcc(t1slice)
    val newRank0 = sa.indexOf(0)
    var bwtTs = new BWTTempStorage(r.filename,n+1,newRank0+1)
    bwtTs.save(firstSegmentBWT(sa,t1slice ))
    bwtTs.close

    if ( ! r.isEmpty ) {
      gtTn = calcGtTn(newRank0,sa)
      kmpIn = KMPBuffer.init(t1)
      Array.copy(t1,0,t2,0,t1.length)
    }
    firstBlockTime = System.nanoTime() - mergeStartTime

    while ( ! r.isEmpty ) {
      var mergeStepStartTime = System.nanoTime()

      step+=1

      debug(2,{
        val sp1 = ( if (step == 1 ) last else (last-first) )/( (if (step == 1 ) firstBlockTime else mergeLastStepTime)/1e9)
        val del = " "
        ( "BWTMerger2.merge: start step %d"+ del + 
            "lastStep=%d sec"+ del + 
            "readed: %s"+ del + 
            "speed=%s"+ del + 
            "Avgspeed=%s"+ del + 
            "saisLastIteration = %d"+ del + 
            "diskread = %d"+ del + 
            "diskreadTotal = %d"+ del + 
            "calcGaps = %d"+ del + 
            "calcGapsTotal =%d" 
          ).format(
          step,
          ((if (step == 1 ) firstBlockTime else mergeLastStepTime)/1e9).toInt,
          bytesF(last.toInt),
          speedF( sp1 ),
          if (step == 1 ) speedF( sp1 ) else speedF( 
              last /( (mergeStepStartTime - mergeStartTime ) /1e9 ) 
          ),
          (saisLastTime/1e9).toInt,
          (datainpLastTime/1e9).toInt,
          (datainpTotalTime/1e9).toInt,
          (calcGapsLastTime/1e9).toInt,
          (calcGapsTotalTime/1e9).toInt
        )
      })

      n = time({ 
        r.copyReverse(t1) 
      },{
        x:Long => datainpLastTime=x;datainpTotalTime+=x
      })

      first = last
      last+=n

      val t1v = t1.view.slice(size-n,t1.length)
      if ( debugLevel >=4 ) {
        println("------------------------------------")
        println(t1v.reverse.map{_.toChar}.mkString(""))
        /*println("------------------------------------")
        println(t1v.reverse.mkString(","))
        */
        println("------------------------------------")
      }
      val lastSymbol = t1(t1.length-1)
      val occ  = calcOcc(t1v)
      val bs = calcBs (occ)
      updateOcc(occGlobal,occ)

      completeKmpFilling(kmpIn,t2,gtTn)
      assert(kmpIn.chars_seen==first-1,"kmpOut.chars_seen = %d (chould be %d) ".format(kmpIn.chars_seen,first-1))
      kmpIn.rewind()
      kmpOut = kmpIn
      kmpIn  = if (r.isEmpty ) null else KMPBuffer.init(t1)


      val gtEof = computeGtEof(t1v,t2,gtTn)

      val (bwt,searcher,rankFirst,rankLast) = calcSAStatistic(t1v,bs,gtEof)

      val gaps = time({
        calcGaps(r.reset,searcher,kmpIn,kmpOut,bwt,lastSymbol,first,bs,rankFirst,rankLast)
      },{
        t:Long => calcGapsLastTime = t ; calcGapsTotalTime+=t
      } )

      val newBwtTs = mergeTemp(bwtTs,gaps,bwt,rankFirst,lastSymbol)
      
      if ( ! r.isEmpty ) {
        gtTn = recalcGtTn(bs,bwt,rankFirst,rankLast)
        Array.copy(t1,0,t2,0,t1.length)
      }

      bwtTs.remove()
      bwtTs = newBwtTs

      mergeLastStepTime = System.nanoTime() - mergeStepStartTime
    }
    r.close
    val auf = writeAuxFile(r.filename,occGlobal)
    val bwtf = bwtTs.convertToPermanent
    
    mergeTotalTime = System.nanoTime() - mergeStartTime

    debug(2,{
        val sp1 = ( if (step == 1 ) last else (last-first) )/( (if (step == 1 ) firstBlockTime else mergeLastStepTime)/1e9)
        val del = " "
        ( "BWTMerger2.merge: Finish "+ del + 
            "lastStep=%d sec"+ del + 
            "readed: %d bytes"+ del + 
            "speed=%s"+ del + 
            "Avgspeed=%s"+ del + 
            "saisLastIteration = %d"+ del + 
            "diskread = %d"+ del + 
            "diskreadTotal = %d"+ del + 
            "calcGaps = %d"+ del + 
            "calcGapsTotal =%d" 
          ).format(
          step,
          last,
          speedF( sp1 ),
          if (step == 1 ) speedF( sp1 ) else speedF( 
              last /(  mergeTotalTime  /1e9 ) 
          ),
          (saisLastTime/1e9).toInt,
          (datainpLastTime/1e9).toInt,
          (datainpTotalTime/1e9).toInt,
          (calcGapsLastTime/1e9).toInt,
          (calcGapsTotalTime/1e9).toInt
        )
      })

    (bwtf,auf)
  }
}

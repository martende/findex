package org.fmindex

import java.io.File
import org.apache.commons.io.FilenameUtils
import scala.collection.mutable.BitSet

trait IBWTReader {
  def copyReverse(t:Array[Byte]):Int
  def getByte:Byte
  def isEmpty:Boolean
  var pos:Int = 0
  def close
  def reset:IBWTReader 
  val filename:String = "IBWTReader"
}

class FileBWTReader(_filename:String) extends IBWTReader {
  override val filename = _filename
  val f = new File(filename)
  val in = new java.io.FileInputStream(f)
  val inb = new java.io.BufferedInputStream(in)
  var lastByte:Int = inb.read()
  def isEmpty = ( lastByte == -1 )

  def reset:IBWTReader = new FileBWTReader(_filename)
  def getByte = {
    val b = lastByte.toByte
    lastByte = if ( b != -1 ) {
      pos+=1
      inb.read() 
    } else -1 
    b
  }
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
  def getByte = {
    val cb = b(pos)
    pos+=1
    cb
  }
  def isEmpty:Boolean = pos == b.length
  def close {}
  def reset:IBWTReader = new StringBWTReader(_b)
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

  def save(s:Array[Byte]) {
    outd.write(s,0,s.length)
  }
  def save(c:Byte) {
    outd.write(c)
  }
  
  def inStorage = new BWTTempStorageIN(f)

  def close {
    closed = true
    outd.close()
    out.close()
  }

  def convertToPermanent():File =  {
    if (!closed) close  

    val to = new File(BWTTempStorage.genFilename(basename))
    println("convertToPermanent ",f,to)
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
  }
}

object BWTMerger2 {
  val ALPHA_SIZE = 256
}
class BWTMerger2(size:Int) {
  val t1:Array[Byte] = new Array[Byte](size)
  val t2:Array[Byte] = new Array[Byte](size)
  val sa:Array[Int] = new Array[Int](size+1)
  val isa:Array[Int] = new Array[Int](size+1)

  def calcSA(t:Array[Byte],offset:Int=0) = {
    val sa = new SAISBuilder(new ByteArrayNulledOffsetWrapper(t,offset))
    sa.build()
    sa.SA.view.slice(1,sa.SA.length)
  }


  def remapAlphabet(t:IndexedSeq[Byte],gtEof:BitSet) = {
    def create_occ() = {
      val n = t.length - 1
      val occ = new Array[Int](BWTMerger2.ALPHA_SIZE+2)
      var i = 0
      while ( i < n ) {
        if ( t(i) < t(n) || ( t(i) == t(n) && ! gtEof(i+1)) ) {
          occ(t(i))+=1
        } else {
          occ(t(i)+2)+=1
        }
        i+=1
      }
      occ(t(n)+1)+=1
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
        val c = if ( i == n - 1 ) t(i)+1 
          else if ( t(i) < t(n_1) || ( t(i) == t(n_1) && ! gtEof(i+1) ) ) t(i)
          else t(i)+2
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
        gt_eof(i) = ( t1(i+j) > t2(j)  )
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
    
    // Why its not just smthng else 
    // take care of the position where the eof symbol should go
    // writing a nearby symbol to help (run-length) compression 

    assert(rank0>0)
    bwt(rank0) = bwt(rank0-1)

    bwt
  }



  def calcOcc(t:Array[Byte],offset:Int=0) = {
    val l = t.length
    var i = 0 
    val occ = new Array[Long](BWTMerger2.ALPHA_SIZE)
    while (i < l) {
      occ(t(i))+=1
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
  
  def completeKmpFilling(kmp:KMPBuffer,t2:Array[Byte],gtTn:BitSet) {
    var i = t2.length - 1
    while ( i > 0 ) {
      kmp.addChar(t2(i),gtTn(i))
      i-=1
    }
  }
  
  def calcSAStatistic(t:IndexedSeq[Byte],bucketStarts:Array[Long],gtEof:BitSet) = {
    val (remapped,asize) = remapAlphabet(t,gtEof)
    
    val sab = new SAISIntBuilder(new IntArrayWrapper(remapped),asize)
    sab.build()

    val sa = sab.SA.view.slice(1,sab.SA.length)

    val bwt = sa2BWT(sa,t)
    val rankFirst = sa.indexOf(0)
    val rankLast  = sa.indexOf(sa.length-1)
    val searcher = new NaiveBWTSearcher(bwt,bucketStarts,rankFirst)

    (bwt,searcher,rankFirst,rankLast)
  }
  def longSuffixCmp(idx:Int,localPfx:Array[Byte],tLast:Array[Byte]) = {
    var k = KMPBuffer.PFX_BUFFER_SIZE-1
    var ret:Int = 0
    var i = idx
    while (ret == 0) {
      if ( tLast(k) > localPfx(i % KMPBuffer.PFX_BUFFER_SIZE) )  ret = -1
      else if ( tLast(k) < localPfx(i % KMPBuffer.PFX_BUFFER_SIZE) )ret = 1
      else {
        k-=1
        i-=1
        if ( i == 0) ret = -1
      }
    }
    ret
  }
  def calcGaps(r:IBWTReader,searcher:SuffixAlgo,kmpIn:KMPBuffer,kmpOut:KMPBuffer,bwt:Array[Byte],lastChar:Byte,numOldSuf:Long,bucketStarts:Array[Long],rk0:Int,rklst:Int) = {
    val n = bwt.length
    val gaps = new Array[Int](n+1)
    val pfxBuffer = new Array[Byte](KMPBuffer.PFX_BUFFER_SIZE)
    var c = r.getByte
    var curRank = bucketStarts(c).toInt
    
    pfxBuffer(0) = c
    gaps(0)+=1
    gaps(curRank)+=1

    if (kmpIn != null ) kmpIn.addChar(c,curRank>rk0)
    var i = 1
    while (i < numOldSuf) {
      val ogt = kmpOut.revisitChar(c)
      c = r.getByte
      val cFirst = bucketStarts(c).toInt
      val oldRank = curRank
      curRank = if ( curRank == 0 ) cFirst else cFirst + searcher.occ(c,curRank-1)
      if ( c == lastChar) {
        if ( curRank == rklst) {
          ogt match {
            case Some(gt) =>  if (gt) curRank+=1 
            case None     =>  if ( longSuffixCmp(i-1,pfxBuffer,kmpOut.string ) > 0 ) curRank+=1
          }
        } else if (curRank > rklst) {
          curRank+=1
        }/* else {

        }*/
      }
      pfxBuffer(i%KMPBuffer.PFX_BUFFER_SIZE) = c
      gaps(curRank)+=1
      //if (i < 400)
      //  printf("%d. c=%c rank=%d occ(%c,%d)=%d oldRank=%d\n",i,c,curRank,c,oldRank-1,searcher.occ(c,oldRank-1),oldRank)
      i+=1
    }
    r.close
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
    while ( tot <=n ) {
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
  def merge(r:IBWTReader):Pair[File,File] = {
    val size = 1024
    var i = 0 
    var gtTn:BitSet = null
    var kmpIn:KMPBuffer = null
    var kmpOut:KMPBuffer = null

    var n = r.copyReverse(t1)
    var first:Long = 0
    var last:Long = n
    val sa = calcSA(t1,t1.length-n)
    val occGlobal = calcOcc(t1,t1.length-n)

    val newRank0 = sa.indexOf(0)
    var bwtTs = new BWTTempStorage(r.filename,n+1,newRank0+1)
    bwtTs.save(firstSegmentBWT(sa,t1))

    bwtTs.close
    if ( ! r.isEmpty ) {
      gtTn = calcGtTn(newRank0,sa)
      kmpIn = KMPBuffer.init(t1)
      Array.copy(t1,0,t2,0,t1.length)
    }

    while ( ! r.isEmpty ) {
      n = r.copyReverse(t1)
      val t1v = t1.view.slice(size-n,t1.length)
      first = last
      last+=n
      val lastSymbol = t1(t1.length-1)
      val occ  = calcOcc(t1,t1.length-n)
      val bs = calcBs (occ)
      updateOcc(occGlobal,occ)

      completeKmpFilling(kmpIn,t2,gtTn)
      kmpIn.rewind()
      kmpOut = kmpIn
      kmpIn  = if (r.isEmpty ) null else KMPBuffer.init(t1)
      
      val gtEof = computeGtEof(t1v,t2,gtTn)
      val (bwt,searcher,rankFirst,rankLast) = calcSAStatistic(t1v,bs,gtEof)
      
      assert(bwt.length==n)
      val gaps = calcGaps(r.reset,searcher,kmpIn,kmpOut,bwt,lastSymbol,first,bs,rankFirst,rankLast)
      
      assert({var i = 0; var j=0;while(j<=n){i+=gaps(j);j+=1};j==first+1},"GAPS checking OK")

      bwtTs = mergeTemp(bwtTs,gaps,bwt,rankFirst,lastSymbol)
    }
    r.close
    val auf = writeAuxFile(r.filename,occGlobal)
    val bwtf = bwtTs.convertToPermanent
    (bwtf,auf)
  }
}

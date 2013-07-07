package org.fmindex

import Util._
import java.io.File
import org.apache.commons.io.FilenameUtils

trait IBWTReader {
  def copyReverse(t:Array[Byte]):Int
  def getByte:Int
  def isEmpty:Boolean
  var pos:Int = 0
  def close
  def reset:IBWTReader 
  val filename:String = "IBWTReader"
}

class DirBWTReader(_dir:String,_filename:String="DirBWTReader",debugLevel:Int=0,caching:Boolean=false,maxSize:Int = 0,filterBinary:Boolean=true ) extends IBWTReader {
  def reset:IBWTReader = if ( caching ) {
    cacheoutb.flush()
    new FileBWTReader(cachefile)
  }
    else new DirBWTReader(_dir,_filename)
  override val filename:String = _filename
  val mainDir = new File(_dir)
  if ( ! mainDir.isDirectory ) throw new Exception("mainDir %s is not directory".format(mainDir))

  val files = recursiveListFiles(mainDir)
  var filesStream = files
  var bytesCache:List[Int] = List()
  var readCount:Int = 0
  //var in:java.io.FileInputStream = _
  var inb:java.io.BufferedInputStream = _ // new java.io.BufferedInputStream(in)

  val cachefile = BWTTempStorage.genCacheFilename(_filename)
  var cacheoutb:java.io.BufferedOutputStream = if (caching) {
      val cacheout = new java.io.FileOutputStream(cachefile)
      new java.io.BufferedOutputStream(cacheout)
  } else null
  
  var lastByte:Int = _readByte(filesStream,null)
  def isEmpty = ( lastByte == -1 )
  def debug(l:Int,s: =>String  ) = if (l<=debugLevel) println(s)
  def recursiveListFiles(f: File): Stream[File] = {
    def isBinary(f:File) = Util.isBinary(f) match  {
      case Some(true) => 
        debug(2,"DirBWTReader: read file '%s' File Is Binary".format(f))
        true
      case Some(false) => false
      case None => true
    }

    if (! f.isDirectory ) {
        Stream(f)
    } else {
        val these = f.listFiles
        if ( these == null )
            Stream.empty
        else
            these.filter{ x:File => ! x.isDirectory && ! (isBinary(x) && filterBinary )}.toStream append these.filter{x:File => x.isDirectory }.flatMap(recursiveListFiles)
    }
  }
  def getByte = {
    val b = lastByte
    lastByte = if ( b != -1 ) {
      pos+=1
      _readByte(filesStream,inb) 
    } else -1 
    b
  }
  def copyReverse(t:Array[Byte]):Int = {
    var i = t.length -1 
    var b = 0
    t(i) = lastByte.toByte
    i-=1
    while ( i >= 0 && b != -1 ) {
      b = _readByte(filesStream,inb)
      if ( b > 0) {
        t(i) = b.toByte
        pos+=1
        i-=1
      }
    }
    lastByte = if ( b != -1 ) _readByte(filesStream,inb) else -1 
    t.length - i - 1 
  }

  def _readByte(fStream:Stream[File],_inb:java.io.BufferedInputStream):Int = {
    val outb = if (maxSize > 0 && readCount>=maxSize) {
      -1
    } else if ( ! bytesCache.isEmpty) {
      val b = bytesCache.head
      bytesCache=bytesCache.tail
      b
    } else {
      __readByte(fStream:Stream[File],_inb:java.io.BufferedInputStream)
    }

    if ( outb != -1 ) {
      if ( caching) cacheoutb.write(outb)
      readCount+=1
    }
    outb

  }
  def __readByte(fStream:Stream[File],_inb:java.io.BufferedInputStream):Int = {
      if ( _inb == null ) {
        if ( fStream.isEmpty ) {
          if ( inb != null ) inb.close()
          inb = null
          filesStream = fStream
          -1 
        }
        else {
          debug(3,"DirBWTReader: read file '%s' total %d bytes".format(fStream.head,readCount))
          val opNewIs = try {
            val in = new java.io.FileInputStream(fStream.head)
            Some(new java.io.BufferedInputStream(in))
          } catch {
            case _:java.io.FileNotFoundException  => {
              debug(2,"DirBWTReader: read file '%s' File Not Found".format(fStream.head))
              None
            }
          } 
          opNewIs match {
            case Some(newIs) => __readByte(fStream.tail,newIs)
            case None        => __readByte(fStream.tail,null)
          }
          
        }
      } else {
        val b = _inb.read()
        if ( b == -1 ) {
          val nb = __readByte(fStream,null)
          if (nb != -1) bytesCache::=nb
          1 // File Splitter 
        } else {
          if (inb != _inb ) {
            if ( inb != null) inb.close()
            inb = _inb
            filesStream = fStream
          }
          
          b match {
            case 0 => 
              bytesCache::='0'
              '\\'
            case 1 => 
              bytesCache::='1'
              '\\'
            case 255 => 
              bytesCache::='f'
              '\\'
            case _ => b
          }
          
        }
      }  
    
  }

  def close {
    if ( inb != null) {
      inb.close
      inb=null
    }
    if ( cacheoutb != null ) {
      cacheoutb.close
      cacheoutb = null
    }
    filesStream = Stream.Empty
  }
}

class FileBWTReader(_filename:String,maxSize:Int=0) extends IBWTReader {
  override val filename = _filename
  val f = new File(filename)
  val in = new java.io.FileInputStream(f)
  val inb = new java.io.BufferedInputStream(in)
  var lastByte:Int = inb.read()
  def isEmpty = ( lastByte == -1 )

  def reset:IBWTReader = new FileBWTReader(_filename,maxSize)
  def getByte = {
    if (maxSize > 0 && pos>=maxSize) {
      -1
    } else {
      val b = lastByte
      lastByte = if ( b != -1 ) {
        pos+=1
        inb.read() 
      } else -1 
      b
    }
  }
  def copyReverse(t:Array[Byte]):Int = {
    var i = t.length -1 
    var b = 0
    t(i) = lastByte.toByte
    i-=1
    while ( i >= 0 && b != -1 ) {
      b = if (maxSize > 0 && pos>=maxSize) -1 else inb.read()
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


class StringBWTReader(_b:String,_filename:String="StringBWTReader",direct:Boolean=false) extends IBWTReader {
  override val filename = _filename
  val b:Array[Byte] = if (direct) _b.reverse.getBytes() else _b.getBytes()
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

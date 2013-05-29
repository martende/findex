package org.findex


class SuffixArray(T:Array[Byte]) {
  val n = T.size
  val SA: Array[Int] = new Array[Int](n)
  val k = 256

  lazy val C = {
    val C = new Array[Int](n)
    T foreach { idx:Byte => C(idx & 0xff)+=1 }
  }

  def getBuckets() = {
    val B = new Array[Int](k)
    var sum = 0
    for (i <- 0 until k) {
      sum+=C(i)
      B(i) = sum
    }
  }

  def build = {
    val bkt = getBuckets()

    // getCounts

    // getBuckets - end of symbol buckets



    // b = -1; i = n - 1; j = n; m = 0; c0 = T.get(n - 1);
    // do { c1 = c0; } while((0 <= --i) && ((c0 = T.get(i)) >= c1));

  }
  def stringLike(i:Int):String = {
    val idx = SA(i)
    val t = T.slice(idx,n) ++  T.slice(0,idx)
    new String(t)
  }

  private def sortFirst() = {
    val cnt = new Array[Int](n)



    /*
    for (i <- 0 to d.size-1) {
      val j = d(i) & 0xff
      sa(cnt(j))=i.asInstanceOf[Byte]
      cnt(j)+=1
    }
    */
  }

  override def toString: String = {
    for ( i <- 0 until n ) {
      println(i + ". " + stringLike(i))
    }
    "suffixArray dump = " + new String(T)
  }
}

object IndexMaker {
  object totoshka {
    val k = 1
  }
    import java.io.File
    import org.apache.commons.io.IOUtils
    import scalax.io._

    val EOF = 0xff
    val POWER = 256

    def recursiveListFiles(f: File): Stream[File] = {
        if (! f.isDirectory ) {
            Stream(f)
        } else {
            val these = f.listFiles
            if ( these == null )
                Stream.empty
            else
                these.filter(! _.isDirectory).toStream append these.filter(_.isDirectory).flatMap(recursiveListFiles)
        }
    }
    def packTo(buf:java.io.ByteArrayOutputStream,f:File) = {
      try {
        val fs = new java.io.FileInputStream(f)
        IOUtils.copy(fs,buf)
        buf.write(EOF)
      } catch  {
        case e:java.io.FileNotFoundException =>
      }
    }

    def buildSA(data: Array[Byte]) {
      val xs = new Array[Byte](data.size)

    }

  /*
template<typename string_type, typename sarray_type, typename index_type>
int
saisxx(string_type T, sarray_type SA, index_type n, index_type k = 256) {
int err;
if((n < 0) || (k <= 0)) { return -1; }
if(n <= 1) { if(n == 1) { SA[0] = 0; } return 0; }
try { err = saisxx_private::suffixsort(T, SA, 0, n, k, false); }
catch(...) { err = -2; }
return err;
}

if (saisxx(s.begin(), sa.begin(), (int)s.size(), 0x100) == -1) {
  */
    def make(dir:String,recursive:Boolean=true) = {
        val files = recursiveListFiles(new File(dir))
        val buf = new java.io.ByteArrayOutputStream()
        
        println(files)

        files foreach { f:File  => packTo(buf,f) } 
        
        // TODO: nado poluchit v pamiati vse faili kakto naibolee deshevim metodom
        val data = buf.toByteArray()

        println("Readed " + data.size  + " bytes")

        buildSA(data)

        println(data(0))
        /*
        for(i <- 0 to buf.size()) {
            println("i  " + i + "  " + buf[i] );
        }
        */
        
        //for((x,i) <- xs.view.zipWithIndex) println("String #" + i + " is " + x)
    }
}
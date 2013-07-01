package org.fmindex

object IndexerAppCache extends optional.Application {
    def main() {
        val r = new FileBWTReader("testdata/include.cache")
        val bm = new BWTMerger2(1024*1024*5,debugLevel=2)
        val (of,af) = bm.merge(r)
    }
}

object FMCreatorApp  {
    val usage = """
        Usage: FMCreatorApp --file filename
    """
    def main(args: Array[String]) {
        if (args.length == 0) println(usage)
    
        val arglist = args.toList
        type OptionMap = Map[Symbol, Any]

        def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
          def isSwitch(s : String) = (s(0) == '-')
          list match {
            case Nil => map
            case "--file" :: value :: tail =>
                                   nextOption(map ++ Map('file -> value), tail)
            /*case "--min-size" :: value :: tail =>
                                   nextOption(map ++ Map('minsize -> value.toInt), tail)
            case string :: opt2 :: tail if isSwitch(opt2) => 
                                   nextOption(map ++ Map('infile -> string), list.tail)
            case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
            */
            case option :: tail => println("Unknown option "+option) 
                                   exit(1) 
          }
        }
        val options = nextOption(Map(),arglist)

        options('file) match {
            case file:String => val bm = new FMCreator(file,1024*1024*300)
                bm.create()
        }
        
    }
}

object IndexerApp {
  def main(args: Array[String]) {
    var selfTest:Boolean = true
    var createFM = true
    var maxSize  = 0
    var dir:String = "/usr/include/";
    var i:Int = 10
    var filterBinary = true
    var mergeDebugLevel:Int=1
    import java.io.File

    def process(args: List[String]) :Boolean = {
        args match {
            case Nil => true
            case "--dir"         :: x :: rest => dir = x ; process(rest)
            case "-i"            :: x :: rest => i = x.toInt ; process(rest)
            case "--max-size"    :: x :: rest => maxSize = 1024*x.toInt ; process(rest)
            case "--merge-debug-level"    :: x :: rest => mergeDebugLevel = x.toInt ; process(rest)
            case "--no-filter-binary" :: rest => filterBinary = false ; process(rest)
            case _ => false
        }

    }

    process(args.toList)

    val r = if ( (new File(dir)).isDirectory ) 
        new DirBWTReader(dir,"testdata/include",debugLevel=2,caching=true,maxSize=maxSize,filterBinary=filterBinary) else 
        new FileBWTReader(dir,maxSize=maxSize)
    
    val bm = new BWTMerger2(1024*1024*i,debugLevel=mergeDebugLevel)
    val (of,af) = bm.merge(r)

    if ( createFM ) {
        println("Create FM index")
        val fm = new FMCreator(of.getAbsolutePath,1024*1024*10)
        fm.create()        
    }

    if ( selfTest ) {
        println("Selfchecking tests")
        val bwtl = new BWTLoader(of)
        //val bwtl2= new BWTLoader(new File("testdata/include.cache.bwt"),bigEndian=false)
        printf("BWT Eof=%d\n",bwtl.eof.toInt)
        //printf("BWT Eof2=%d\n",bwtl2.eof.toInt)

        val sa = new NaiveFMSearcher(of.getAbsolutePath)
        println("------------------------")
        /*
        println("BWT(0)",bwtl.read(0).toChar)
        println("BWT(1)",bwtl.read(1).toChar)
        println("BWT(2)",bwtl.read(2).toChar)
        println("BWT(EOF)",bwtl.read(bwtl.eof.toInt))
        println("*BWT(EOF--)",sa.getPrevI(bwtl.eof.toInt))
        println("BWT(EOF--)",bwtl.read(sa.getPrevI(bwtl.eof.toInt)).toChar)
        println("*BWT(EOF++)",sa.getNextI(bwtl.eof.toInt))
        println("BWT(EOF++)",bwtl.read(sa.getNextI(bwtl.eof.toInt)).toChar)

        println("sa.getPrevI(1)",sa.getPrevI(1))
        println("sa.getPrevI(48)",sa.getPrevI(48))
        println("sa.nextSubstr(1,3)",sa.nextSubstr(1,3))
        println("BWT(1000)",bwtl.read(1000).toChar)
        println("EOF++",sa.getNextI(bwtl.eof.toInt),sa.nextSubstr(sa.getNextI(bwtl.eof.toInt),100))
        println("EOF--",sa.getPrevI(bwtl.eof.toInt),sa.nextSubstr(sa.getPrevI(sa.getPrevI(bwtl.eof.toInt)),100))
        println("EOF",bwtl.eof.toInt,sa.nextSubstr(bwtl.eof.toInt,100))
        */
        def safeStr(s:String) = s.map{c:Char => if ( c < 0x20 ) '_' else c }

        println("First String:")
        val s = sa.prevSubstr(bwtl.eof.toInt,100)
        println(s)
        println(s == "\0GFORTRAN module version '6' created from ../../../../fortran/src/H5Pff.f90 on Wed Dec  7 15:28:19 2")
        
    }
    println("Done")
  }
}

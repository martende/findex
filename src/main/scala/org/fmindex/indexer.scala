package org.fmindex

object IndexerAppCache extends optional.Application {
    def main() {
        val r = new FileBWTReader("testdata/include.cache")
        val bm = new BWTMerger2(1024*1024*10,debugLevel=2)
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

object IndexerApp extends optional.Application {
  def main(dir: String = "/usr/include/",i:Int=10) {
    val r = new DirBWTReader(dir,"testdata/include",debugLevel=2,caching=true)
    
    //val r = new FileBWTReader("testdata/include.cache")
    
    //val r = new DirBWTReader("/data_nb/aleh/MProg/findex/src","testdata/include",debugLevel=0,caching=true)
    val bm = new BWTMerger2(1024*1024*i,debugLevel=2)
    val (of,af) = bm.merge(r)
  }
}

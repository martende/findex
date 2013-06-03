package scala.org.fmindex

class WAT(alphabetPower:Int,length:Int) {
    type cType = Int
    
    def log2(x: Double): Double = scala.math.log(x) / lnOf2
    val alphabetBitNum:Int = log2(alphabetPower)
    
}
object WAT {
    val lnOf2 = scala.math.log(2) // natural log of 2

    def Init(in:Array[cType],alphabetPower:Int=-1) = {
        val k = if ( alphabetPower == -1 ) GetAlphabetNum(in) else alphabetPower

        new WAT(k,in.length)
    }
    def GetAlphabetNum(in:Array[cType]) = max(in)+1
}
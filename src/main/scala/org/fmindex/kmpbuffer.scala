package org.fmindex

object KMPBuffer {
    val PFX_BUFFER_SIZE = 1024
    val KMP_BIT_BUF_SIZE = 128*1024
    val LONG_RUN_LIMIT = 16
    def init(s:Array[Byte]) = {
        val b = new KMPBuffer(PFX_BUFFER_SIZE,KMP_BIT_BUF_SIZE)
        b.initData(s)
        b
    }
}
class KMPBuffer(_size:Int,_bbsize:Int) {
    val size = _size
    val bit_buffer_size = 64 * (_bbsize / 64)
    val string = new Array[Byte](size)
    val kmp_shift = new Array[Int](size+1)
    val bit_buffer = new Array[Long](_bbsize/64)
    
    var cur_bit:Int = 0
    var current:Int = 0
    var pending0:Long = 0
    var pending1:Long = 0
    var chars_seen:Long = 0
    var stored_bits:Long = 0
    override def toString = 
        "KMPBuffer***"+
        "\nkmp_shift = " + kmp_shift.view.slice(0,10).mkString(",")+
        "\nstring = " + string.view.slice(0,10).map{_.toChar}.mkString(".")+
        "\ncurrent = " + current +
        "\nstored_bits = " + stored_bits +
        "\nchars_seen = " + chars_seen
    def initData(s:Array[Byte]) {
        assert(s.length >= size)
        var i = 0
        
        while ( i < size) {
            string(i)=s(size-i-1)
            i+=1
        }
        
        fillKmpShift(kmp_shift,string,size)
        // TODO: Remove test
        assert(testKmpShift(kmp_shift,string,size))
    }
    def rewind() {
        printf("KMPBuffer.rewind %d %d %d\n",cur_bit,pending0,pending1)
        if ( pending0 > 0) {
            writeBit(false)
        } else if (  pending1 > 0) {
            writeBit(true)
        }
        pending0 = 0
        pending1 = 0
        stored_bits = cur_bit
        cur_bit = 0
        current = 0
        if ( stored_bits > 0)
            printf("!%d bit(s) stored in bit buffer!\n",stored_bits)
    }
    def revisitChar(c:Byte):Option[Boolean] = {
        var dd:Boolean = false
        chars_seen-=1
        if(c == string(current) ) {
            current+=1
            if ( current == size ) {
                current = kmp_shift(current)
                if ( pending0 > 0 ) {
                    pending0-=1
                    Some(false)
                } else if ( pending1 > 0 ) {
                    pending1-=1
                    Some(true)
                } else {
                    val (len,gt) = readBit()
                    if ( gt ) pending1 = len -1 
                    else      pending0 = len -1 
                    Some(gt)
                }
            } else None
        
        } else {
            var cont = true
            while (current > 0 && cont) {
                current = kmp_shift(current)
                if ( c == string(current)) {
                    current+=1
                    cont=false
                }
            }
            None
        }
    }
    def addChar(c:Byte,gt:Boolean) {
        chars_seen+=1
        if ( c == string(current)) {
            current+=1
            if ( current == size ) {
                current = kmp_shift(current)
                if ( gt ) {
                    if ( pending0 > 0 ) {
                        writeBit(false)
                        pending0 = 0
                        pending1 = 1
                    } else 
                        pending1 += 1
                } else {
                    if ( pending1 > 0 ) {
                        writeBit(true)
                        pending1 = 0
                        pending0 = 1
                    } else 
                        pending0 += 1
                }
            }
        } else {
            var cont = true
            while (current > 0 && cont) {
                current = kmp_shift(current)
                if ( c == string(current)) {
                    current+=1
                    cont=false
                }
            }
        }
    }

    //uint64 len=read_run(gt,&(b->cur_bit),b->stored_bits,b->bit_buffer)       
    def readBit()  = {
        assert(cur_bit < stored_bits,"readBit buffer full wtf cur_bit=%d stored_bits=%d".format(cur_bit,stored_bits)) 
        var gt:Int = 0
        var count:Long = 1
        def gb(i:Int):Int = (( bit_buffer(i/64) >>  (i % 64) ) & 1).toInt
        gt = gb(cur_bit)
        if (stored_bits == 1787) {
            println("readBit",cur_bit,gt)
            for (i <- 0 until 20 ) {
                printf("%d,",gb(i+cur_bit))
            }
            println()
        }
        cur_bit+=1
        count = 1 
        while ( count < KMPBuffer.LONG_RUN_LIMIT && 
            cur_bit < stored_bits && 
            gb(cur_bit) == gt ) {
            count+=1
            cur_bit+=1
        }
        if (stored_bits == 1787) {
            println("readBit count",cur_bit,gt,count)
        }
        if ( count == KMPBuffer.LONG_RUN_LIMIT ) {
            println("count == KMPBuffer.LONG_RUN_LIMIT",cur_bit,stored_bits,gt)
            var bit = 0
            var nbits = 0
            while (bit != 1) {
                assert(cur_bit>=stored_bits)
                bit = gb(cur_bit)
                cur_bit+=1
                nbits+=1
            }
            nbits-=1
            var gamma:Long = 1
            while ( nbits > 0) {
                assert(cur_bit>=stored_bits)
                bit = gb(cur_bit)
                cur_bit+=1
                gamma = 2 * gamma + bit
                nbits-=1
            }
            count += gamma-1
        }
        (count,gt==1)
    }
    def writeBit(bit:Boolean) {
        val n = if ( bit ) pending1 else pending0
        println("writeBit ",bit,n)
        for (i<- 0 until 20 ) {
            printf("%08x,",bit_buffer(i))
        }
        println()
        
        var run:Int =  (n min KMPBuffer.LONG_RUN_LIMIT).toInt
        assert (cur_bit+run < bit_buffer_size,"Bit buffer full, write error")
        if (! bit ) {
            cur_bit+=run
        } else {
            var cur_bit_to=cur_bit+run
            while (cur_bit<cur_bit_to) {
                bit_buffer(cur_bit/64) |= 1 << (cur_bit % 64)
                cur_bit+=1
            }
        }
        if ( n >= KMPBuffer.LONG_RUN_LIMIT) {
            var v = 1 + ( n - KMPBuffer.LONG_RUN_LIMIT)
            var len = gammacodeLen(v)
            assert (cur_bit+len < bit_buffer_size,"Bit buffer full, write error")
            var i = (len-1)/2
            cur_bit += i
            while (i>=0) {
                if ( ((v >> i) & 1) == 1 )
                    bit_buffer(cur_bit/64) |= 1 << (cur_bit % 64)
                cur_bit+=1
                i-=1
            }
        }
        for (i<- 0 until 20 ) {
            printf("%08x,",bit_buffer(i))
        }
        println()
    }
    
    def fillKmpShift(_kmp_shift:Array[Int],_string:Array[Byte],_size:Int) {
        _kmp_shift(0) = 0
        _kmp_shift(1) = 0
        var q = 1
        var k = 0
        while ( q < _size ) {
            if (_string(q) == _string(k)) {
                k+=1
                _kmp_shift(q+1) = k
            } else {
                var cont = true
                while ( k > 0 && cont ) {
                    k = _kmp_shift(k)
                    if ( _string(q) == _string(k) ) {
                        k+=1
                        _kmp_shift(q+1) = k
                        cont = false
                    }
                }
            }
            if ( k == 0) _kmp_shift(q+1) = 0
            q+=1
        }
    }
    def gammacodeLen(_n:Long)= {
        var b = 0
        var n = _n
        while(n!=0)  {
            b+=1
            n>>=1
        }
        2*b-1
    }
    def testKmpShift(_kmp_shift:Array[Int],_string:Array[Byte],_size:Int):Boolean = {
        for ( k <- 1 until size ; q <- 0 until kmp_shift(k) ) {
            assert(string(q) == string(q+k-kmp_shift(k)))
        }
        true
    }
}
package hod.euler

import java.math.BigInteger


val fCache = mutableMapOf<Int, BigInteger>()

fun fac(n: Int): BigInteger {
    if (n == 0) {
        return BigInteger.ONE
    } else {
        return fCache.computeIfAbsent(n) { value ->
            fac(value - 1) * BigInteger.valueOf(n.toLong())
        }
    }
}

fun s(n: Int): Int {
    val seq = generateSequence(1) { it + 1 }
    return seq.find { e ->
        val factorialed = fac(e)
        factorialed.mod(BigInteger.valueOf(n.toLong())) == BigInteger.ZERO
    }!!
}

fun S(n: Int): Long {
    val range = 2..n
    val results = range.map { e -> s(e).toLong().also { println("S($e) = $it") } }
    return results.sum()
}

fun main() {
    println(S(10000))
}
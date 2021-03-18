// Data is generated with corretto-15

import java.io.File

val startCp = IntRange(0, 0x10FFFF).toList().filter { Character.isJavaIdentifierStart(it) }
val partCp = IntRange(0, 0x10FFFF).toList().filter { Character.isJavaIdentifierPart(it) }

fun group(xs: List<Int>): List<List<Int>> {
    val result: MutableList<List<Int>> = mutableListOf()
    var cur: List<Int>? = null
    for (x in xs) {
        if (cur == null) {
            cur = mutableListOf(x)
        } else {
            if (cur.last() == x - 1) {
                cur += x
            } else {
                result += cur
                cur = mutableListOf(x)
            }
        }
    }

    if (cur != null) {
        result += cur
    }
    return result
}

fun rangeStr(xs: List<Int>): String = if (xs.size == 1) {
    "\\x%02X".format(xs.first())
} else {
    "\\x%02X-\\x%02X".format(xs.first(), xs.last())
}

fun main() {
    val startLit = group(startCp).joinToString("") { rangeStr(it) }
    println("\$JavaIdentifierStart = [$startLit]")
    val partSet = partCp.toSet()
    val startSet = startCp.toSet()
    assert(partSet.containsAll(startSet))
    val extras = partSet - startSet
    val partLit = group(extras.toList()).joinToString("") { rangeStr(it) }
    println("\$JavaIdentifierPart = [\$JavaIdentifierStart$partLit]")
    File("/tmp/start.txt").writeText(startCp.toString())
    File("/tmp/part.txt").writeText(partCp.toString())
}

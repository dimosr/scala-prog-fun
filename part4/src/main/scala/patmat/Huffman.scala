package patmat

import scala.annotation.tailrec

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  

  // Part 1: Basics
  def weight(tree: CodeTree): Int = {
    tree match {
      case Leaf(_, weight) => weight
      case Fork(left, right, _, forkWeight) => forkWeight
    }
  }
  
  def chars(tree: CodeTree): List[Char] = {
    tree match {
      case Leaf(char, _) => List(char)
      case Fork(left, right, forkChars, _) => forkChars
    }
  }
  
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    timesAcc(chars, List())
  }

  @tailrec
  def timesAcc(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
    chars match {
      case List() => {
        acc.groupBy(_._1)
            .map(entry => (entry._1, entry._2.map(_._2).sum))
            .toList
      }
      case nextChar::cs => timesAcc(cs, (nextChar, 1)::acc)
    }
  }
  
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
      freqs.map(entry => Leaf(entry._1, entry._2))
        .sortBy(leaf => (leaf.weight, leaf.char))
    }
  
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
    def singleton(trees: List[CodeTree]): Boolean = trees.size == 1
  
  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    trees match {
      case List() => List()
      case  List(tree) => List(tree)
      case first::second::ts => {
        val parent = makeCodeTree(first, second)
        insertInSorted(parent, ts, List())
      }
    }
  }

  private def insertInSorted(tree: CodeTree, list: List[CodeTree], smallerElems: List[CodeTree]): List[CodeTree] = {
    list match {
      case List() => smallerElems:::List(tree)
      case nextTree::ts => {
        if(weight(tree) <= weight(nextTree)) smallerElems.reverse:::tree::nextTree::ts
        else insertInSorted(tree, ts, nextTree::smallerElems)
      }
    }
  }
  
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  @tailrec
  def until(terminationFn: List[CodeTree] => Boolean, reduceFn: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): CodeTree = {
    if(terminationFn(trees)) trees.head
    else until(terminationFn, reduceFn)(reduceFn(trees))
  }
  
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val leafs = makeOrderedLeafList(times(chars))
    until(singleton, combine)(leafs)
  }
  

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    decodeAcc(tree, bits, List())
  }

  @tailrec
  private def decodeAcc(tree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = {
    if(bits.isEmpty) acc.reverse
    else {
      val (nextChar, remainingBits) = decodeChar(tree, bits)
      decodeAcc(tree, remainingBits, nextChar::acc)
    }
  }

  @tailrec
  private def decodeChar(navigatedTree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = {
    navigatedTree match {
      case Leaf(char, weight) => (char, bits)
      case Fork(left, right, _, _) => {
        if(bits.head == 0) decodeChar(left, bits.tail)
        else decodeChar(right, bits.tail)
      }
    }
  }
  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the 'frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)
  

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    encodeAcc(tree, text, List())
  }

  @tailrec
  private def encodeAcc(tree: CodeTree, text: List[Char], acc: List[Bit]): List[Bit] = {
    text match {
      case List() => acc
      case char::cs => {
        val nextBits = encodeChar(tree, char, List())
        encodeAcc(tree, cs, acc:::nextBits)
      }
    }
  }

  @tailrec
  private def encodeChar(tree: CodeTree, char: Char, acc: List[Bit]): List[Bit] = {
    tree match {
      case Leaf(_, _) => acc.reverse
      case Fork(left, right, _, _) => {
        if(chars(left).contains(char)) encodeChar(left, char, 0::acc)
        else encodeChar(right, char, 1::acc)
      }
    }
  }
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table.find(_._1 == char)
        .map(_._2)
        .getOrElse(throw new Error("non-existing character"))
  }
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   */
  def convert(tree: CodeTree): CodeTable = {
    tree match {
      case Leaf(char, weight) => List((char, List()))
      case Fork(left, right, chars, weight) => {
        mergeCodeTables(convert(left), convert(right))
      }
    }
  }
  
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(leftTable: CodeTable, rightTable: CodeTable): CodeTable = {
    val leftCodeTable = leftTable.map(entry => (entry._1, 0::entry._2))
    val rightCodeTable = rightTable.map(entry => (entry._1, 1::entry._2))
    leftCodeTable:::rightCodeTable
  }
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)
    quickEncodeAcc(codeTable, text, List())
  }

  @tailrec
  private def quickEncodeAcc(table: CodeTable, text: List[Char], acc: List[Bit]): List[Bit] = {
    text match {
      case List() => acc
      case char::cs => {
        val encodedChar = codeBits(table)(char)
        quickEncodeAcc(table, cs, acc:::encodedChar)
      }
    }
  }
}

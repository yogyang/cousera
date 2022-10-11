package objsets

import TweetReader.*

import scala.::
import scala.annotation.tailrec

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int):
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet extends TweetSetInterface:

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, Empty())

  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet


  def reduceBy(func: (Tweet, Tweet) => Tweet): Tweet = {
    def inner(cur: Tweet, next: TweetSet): Tweet = {
      println(cur)
      next.nextTweet match {
        case None => cur
        case Some(c) =>
          inner(inner(func(cur, c), next.left), next.right)
      }
    }

    if (this.nextTweet.isEmpty)
      throw new NoSuchElementException()

    inner(this.nextTweet.get, this)
  }

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet = reduceBy((a, b) => if (a.retweets >= b.retweets) a else b)

  def nextTweet: Option[Tweet]

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList

  def incl(tweet: Tweet, compareFun: (Tweet, Tweet) => Int): TweetSet
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet =  incl(tweet, (a, b) => if (a.text < b.text) -1 else if (a.text > b.text) 1 else 0)

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  val left: TweetSet
  val right: TweetSet



class Empty extends TweetSet:
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
  def union(that: TweetSet): TweetSet = that
  def nextTweet: Option[Tweet] = None
  def descendingByRetweet: TweetList = Nil

  override val left: TweetSet = this
  override val right: TweetSet = this


  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet, compareFunc: (Tweet, Tweet) => Int): TweetSet = NonEmpty(tweet, Empty(), Empty())

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()


class NonEmpty(elem: Tweet, override val left: TweetSet, override val right: TweetSet) extends TweetSet:

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if (p(elem)) left.filterAcc(p, right.filterAcc(p, acc.incl(elem)))
    else left.filterAcc(p, right.filterAcc(p, acc))
  }

  def union(that: TweetSet): TweetSet =
    val r1 = that.incl(elem)
    val r2 = left.union(r1)
    right.union(r2)

  def nextTweet: Option[Tweet] = Some(elem)

  def incl(x: Tweet, compareFunc: (Tweet, Tweet) => Int): TweetSet = {
    val compare = compareFunc(x, elem)
    if compare < 0 then
      NonEmpty(elem, left.incl(x, compareFunc), right)
    else if compare > 0 then
      NonEmpty(elem, left, right.incl(x, compareFunc))
    else
      this
  }



  def descendingByRetweet: TweetList = {
    // build new tree based on retweet
    def inner(cur: TweetSet, next: TweetSet): TweetSet = {
      next.nextTweet match {
        case None => cur
        case Some(c) =>
          inner(inner(cur.incl(c, (a, b) => {
            if (a.retweets == b.retweets) 1
            else a.retweets - b.retweets
          } ), next.left), next.right)
      }
    }

    val newTree = inner(Empty(), this)


    def traverseLeft(cur: TweetList, next: TweetSet): TweetList = {
      next.nextTweet match {
        case None => cur
        case Some(c) =>
          traverseLeft(traverseLeft(cur, next.left).add(c), next.right)
      }
    }
    traverseLeft(Nil, newTree)
  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if x.text < elem.text then
      left.contains(x)
    else if elem.text < x.text then
      right.contains(x)
    else true

  def remove(tw: Tweet): TweetSet =
    if tw.text < elem.text then
      NonEmpty(elem, left.remove(tw), right)
    else if elem.text < tw.text then
      NonEmpty(elem, left, right.remove(tw))
    else
      left.union(right)

  def foreach(f: Tweet => Unit): Unit =
    f(elem)
    left.foreach(f)
    right.foreach(f)


trait TweetList:
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if !isEmpty then
      f(head)
      tail.foreach(f)

  def add(tweet: Tweet): TweetList

object Nil extends TweetList:
  def head = throw java.util.NoSuchElementException("head of EmptyList")
  def tail = throw java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true

  override def add(tweet: Tweet): TweetList = Cons(tweet, this)

class Cons(val head: Tweet, val tail: TweetList) extends TweetList:
  def isEmpty = false

  override def add(tweet: Tweet): TweetList = Cons(tweet, this)


object GoogleVsApple:
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val allTweets = TweetReader.allTweets

  lazy val googleTweets: TweetSet = allTweets.filter(t => google.exists(t.text.contains(_)))
  lazy val appleTweets: TweetSet = allTweets.filter(t => apple.exists(t.text.contains(_)))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet

object Main extends App:
  // Print the trending tweets
  GoogleVsApple.trending foreach println

package dataset

import dataset.util.Commit.Commit

import java.text.SimpleDateFormat
import java.util.SimpleTimeZone
import scala.math.Ordering.Implicits._

/**
 * Use your knowledge of functional programming to complete the following functions.
 * You are recommended to use library functions when possible.
 *
 * The data is provided as a list of `Commit`s. This case class can be found in util/Commit.scala.
 * When asked for dates, use the `commit.commit.committer.date` field.
 *
 * This part is worth 40 points.
 */
object Dataset {


  /** Q23 (4p)
   * For the commits that are accompanied with stats data, compute the average of their additions.
   * You can assume a positive amount of usable commits is present in the data.
   *
   * @param input the list of commits to process.
   * @return the average amount of additions in the commits that have stats data.
   */
  def avgAdditions(input: List[Commit]): Int = {
    val additions = input.flatMap( c => c.stats).map(_.additions)
    additions.sum/additions.length
  }

  /** Q24 (4p)
   * Find the hour of day (in 24h notation, UTC time) during which the most javascript (.js) files are changed in commits.
   * The hour 00:00-00:59 is hour 0, 14:00-14:59 is hour 14, etc.
   * NB!filename of a file is always defined.
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   *
   * @param input list of commits to process.
   * @return the hour and the amount of files changed during this hour.
   */
  def jsTime(input: List[Commit]): (Int, Int) = {

    val fmt = new SimpleDateFormat("H")
    fmt.setTimeZone(new SimpleTimeZone(0, "UTC"))

    // this gives a list of tuples of (hour of the commit, number of js files in that commit)
    val pairs = input.map(c => (fmt.format(c.commit.committer.date).toInt, c.files.flatMap(f => f.filename).count(n => n.endsWith(".js"))))
    val totals = pairs.groupBy(_._1)  // Map[hour, List[(hour, count)]]
    val summed = totals.map(e => (e._1, e._2.map(_._2).sum)) // Map[hour, sum of counts]

    if(summed.isEmpty) (0,0) else summed.maxBy(_._2)

    // fmt.format(someDate).toInt
    // commit.commit.commiter.date - date path
    // commit.files.filename - filename path
  }


  /** Q25 (5p)
   * For a given repository, output the name and amount of commits for the person
   * with the most commits to this repository.
   * For the name, use `commit.commit.author.name`.
   *
   * @param input the list of commits to process.
   * @param repo  the repository name to consider.
   * @return the name and amount of commits for the top committer.
   */
  def topCommitter(input: List[Commit], repo: String): (String, Int) = {
    val names = input.filter(c => c.url.split("/repos/")(1).split("/commits/")(0).equals(repo)).map(c => c.commit.author.name)
    val counts = names.groupBy(identity).map(e => (e._1, e._2.length))
    if(counts.isEmpty) ("",0) else counts.maxBy(_._2)
  }

  /** Q26 (9p)
   * For each repository, output the name and the amount of commits that were made to this repository in 2019 only.
   * Leave out all repositories that had no activity this year.
   *
   * @param input the list of commits to process.
   * @return a map that maps the repo name to the amount of commits.
   *
   *         Example output:
   *         Map("KosDP1987/students" -> 1, "giahh263/HQWord" -> 2)
   */
  def commitsPerRepo(input: List[Commit]): Map[String, Int] = {
    val fmt = new SimpleDateFormat("yyyy")
    fmt.setTimeZone(new SimpleTimeZone(0, "UTC"))


    val filtered = input.filter(c => fmt.format(c.commit.committer.date).toInt == 2019)
    val names = filtered.map(c => c.url.split("/").slice(4,6).mkString("/"))
    val map = names.groupBy(identity).map(e => (e._1, e._2.length))

    map
  }



  /** Q27 (9p)
   * Derive the 5 file types that appear most frequent in the commit logs.
   * NB!filename of a file is always defined.
   * @param input the list of commits to process.
   * @return 5 tuples containing the file extension and frequency of the most frequently appeared file types, ordered descendingly.
   */
  def topFileFormats(input: List[Commit]): List[(String, Int)] = {
    val files = input.flatMap(c => c.files.flatMap(f => f.filename))
    val types = files.filter(f => f.contains(".")).map(f => f.substring(f.lastIndexOf('.') + 1))
    val groups = types.groupBy(identity).map(e => (e._1, e._2.length))

    groups.toList.sortBy(e => -e._2).take(5)
  }


  /** Q28 (9p)
   *
   * A day has different parts:
   * morning 5 am to 12 pm (noon)
   * afternoon 12 pm to 5 pm.
   * evening 5 pm to 9 pm.
   * night 9 pm to 4 am.
   *
   * Which part of the day was the most productive in terms of commits ?
   * Return a tuple with the part of the day and the number of commits
   *
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   */
  def mostProductivePart(input: List[Commit]): (String, Int) = {
    val fmt = new SimpleDateFormat("H")
    fmt.setTimeZone(new SimpleTimeZone(0,"UTC"))

    val hours = input.map(c => fmt.format(c.commit.committer.date).toInt)
    
    val morning = ("morning", hours.count(h => h >= 5 && h < 12))
    val afternoon = ("afternoon", hours.count(h => h >= 12 && h < 17))
    val evening = ("evening", hours.count(h => h >= 17 && h < 21))
    val night = ("night", hours.count(h => h >= 21 || h <= 4))

    val parts = List(morning, afternoon, evening, night)
    parts.maxBy(_._2)
  }
}

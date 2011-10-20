package cc.giraphe

import java.io.{File, FileInputStream, FileNotFoundException, InputStream, InputStreamReader, IOException, Serializable, Reader}
import java.util.{TreeMap, Properties}
import java.util.logging.Logger
import javax.servlet.ServletContext
import org.apache.commons.io.FileUtils
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.kernel.EmbeddedGraphDatabase
import org.neo4j.graphdb.index.ReadableIndex

/**
 * Wrapper around a singleton instance of Neo4j embedded server.
 */
class Neo4jServer(properties: Reader) {
  private val log = Logger.getLogger(this.getClass.getName)
  private var neo: GraphDatabaseService = null

  def gdb = neo

  //def find[T<:Any](operation: (GraphDatabaseService, ReadableIndex[T]) => T): T = {
  //  synchronized {
  //    if (neo == null) 
  //      startup
  //  }
  //}

  /**
   * Initialize Neo4j with configuration stored in a properties file specified via a
   * JVM system property, by using <code>-Dneo4j.config=/path/to/neo4j.properties</code>
   * on the command line.
   */
  def startup {
    startup(properties)
  }

  /**
   * Initialize Neo4j with configuration stored in a properties file
   * <code>/WEB-INF/neo4j.properties</code> relative to the root of the given
   * servlet context.
   */
  def startup(context: ServletContext) {
    val stream = new InputStreamReader(context.getResourceAsStream("/WEB-INF/neo4j.properties"))
    if (stream == null) {
      log.warning("Cannot read Neo4j configuration from /WEB-INF/neo4j.properties: resource not found")
    } else {
      startup(stream)
    }
  }

  /**
   * Initialize Neo4j with configuration loaded from an InputStream in Java properties
   * file format.
   */
  def startup(stream: Reader) {
    val neoConfig = new Properties
    try {
      neoConfig.load(stream)
    } catch {
      case e: IOException => log.warning("Cannot read Neo4j configuration: " + e)
      case e: Throwable => log.warning("Underlying problem: " + e.getCause().getMessage())
    }
    val environment = System.getProperty("neo4j.env", "development")
    startup(neoConfig, environment)
  }

  /**
   * Initialize Neo4j with configuration from a properties file and an environment string
   * (typically "development", "test" or "production").
   */
  def startup(config: Properties, environment: String) {
    def prop(key: String, default: String) =
      config.getProperty("neo4j.%s.%s".format(environment, key),
         config.getProperty("neo4j.%s".format(key), default))

    def isTrue(value: String) = (value != null) && (Array("true", "yes", "1") contains value.toLowerCase)

    synchronized {
      if (neo != null) return
      val neoPath = prop("path", "/tmp/neo4j")
      log.info("Initializing Neo4j server in %s environment with data files in %s".format(environment, neoPath))

      // Delete the database if configured (DANGEROUS - use for test environment only)
      if (isTrue(prop("destroy_on_startup", "false"))) {
        log.info("Destroying any old Neo4j data files in %s".format(neoPath))
        try { FileUtils.deleteDirectory(new File(neoPath)) } catch { case _: IOException => }
      }

      neo = new EmbeddedGraphDatabase(neoPath)

      // Setup shell if required
      if (isTrue(prop("shell.enabled", "false"))) {
        val shellProperties = new TreeMap[String, Serializable]
        try {
          shellProperties.put("port", Integer.parseInt(prop("shell.port", "1337")))
        } catch {
          case _: NumberFormatException => // also catches getProperty == null
        }
        val shellName = prop("shell.name", null)
        if (shellName != null) shellProperties.put("name", shellName)
        log.info("!!!!enableRemoteShell is disabled")
        // neo.enableRemoteShell(shellProperties)
      }

      if (! isTrue(prop("skip.shutdown.hook", "false"))) {
        // Register a shutdown hook to ensure Neo4j is cleanly shut down before the JVM exits
        Runtime.getRuntime.addShutdownHook(new Thread() {
          override def run() {
            shutdown
          }
        })
      }
    }
  }

  /**
   * Do a clean shutdown of Neo4j.
   */
  def shutdown {
    synchronized {
      log.info("Shutting down Neo4j server")
      if (neo != null) neo.shutdown
      neo = null
    }
  }

  /**
   * Execute instructions within a Neo4j transaction; rollback if exception is raised and
   * commit otherwise; and return the return value from the operation.
   */
  def exec[T<:Any](operation: GraphDatabaseService => T): T = {
    val tx = synchronized {
      if (neo == null) startup
      neo.beginTx
    }
    try {
      val ret = operation(neo)
      tx.success
      return ret
    } finally {
      tx.finish
    }
  }

}

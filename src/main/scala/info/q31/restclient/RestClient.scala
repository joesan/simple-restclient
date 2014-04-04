package info.q31.restclient

import dispatch._
import com.ning.http.client.RequestBuilder
import com.typesafe.config.ConfigFactory

/**
 * A minimalistic REST client that can call REST style API's
 */
trait RestClient {

  /**
   * Do a HTTP Post request
   * @param resource The request url
   * @param requestBody  The request body
   * @return
   */
  def post(resource: String, requestBody: String): (Int, String)

  /**
   * Do a HTTP Get request
   * @param resource The request url
   * @param requestParam The request parameters
   * @return
   */
  def get(resource: String, requestParam: Option[Seq[(String, String)]] = None): String
}

trait RestClientUtil {
  val config = ConfigFactory.load("parameters.conf")
  val user = config.getString("user")
  val pass = config.getString("pass")
  val isHttps = config.getBoolean("isHttps")
  val host = config.getString("host")
  val port = config.getPort("port")

  val REST_HTTP_ADDRESS = host + ":" + port
}

class RestClientImpl extends RestClient with RestClientUtil {

  def post(reqURL: String, requestBody: String): (Int, String) = {

    def getRequestURL: RequestBuilder = {
      if(isHttps) url(REST_HTTP_ADDRESS + "/" + resource).as_!(user, pass)
      else        url(REST_HTTP_ADDRESS + "/" + resource)
    }

    val request = getRequestURL(reqURL) << requestBody

    val http = Http(request OK as.String)

    def shutdown = if (http.isComplete) Http.shutdown

    http.onFailure {
      case e: Throwable => shutdown
        throw e
    }

    http.onSuccess {
      case response => response
        shutdown
    }
  }

  def get(resource: String, requestParam: Option[Seq[(String, String)]] = None): String = {

    def getRequestURL: RequestBuilder = {
      if(isHttps) url(REST_HTTP_ADDRESS + "/" + resource).as_!(user, pass)
      else        url(REST_HTTP_ADDRESS + "/" + resource)
    }

    val request = requestParam match {
      case Some(params) => getRequestURL(reqURL) <<? params
      case None =>         getRequestURL(reqURL)
    }

    val http = Http(request OK as.String)

    def shutdown = if (http.isComplete) Http.shutdown

    http.onFailure {
      case e: Throwable => shutdown
      throw e
    }

    http.onSuccess {
      case response => response
      shutdown
    }
  }
}
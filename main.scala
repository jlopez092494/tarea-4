import java.io.OutputStream
import java.net.InetSocketAddress
import java.net.URL
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.Base64
import javax.imageio.ImageIO
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.awt.image.BufferedImage
import java.nio.charset.Charset

import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpHandler
import com.sun.net.httpserver.HttpServer


object Tarea {
	def main(args: Array[String]): Unit = {
		val server = HttpServer.create(new InetSocketAddress(8080), 0);
		server.createContext("/exercise1", new exercise1());
    	server.createContext("/exercise2", new exercise2());
    	server.createContext("/exercise3", new exercise3());
    	server.createContext("/exercise4", new exercise4());
    	server.start();
    	println("Server up!");
	}
}

class exercise1() extends HttpHandler{
	override def handle(client: HttpExchange){
		if (client.getRequestMethod == "POST") {
			val os: OutputStream = client.getResponseBody()
            var cont = 0
            var output = new ByteArrayOutputStream()
            var input = client.getRequestBody()
            var response: Array[Byte] = Stream.continually(input.read).takeWhile(_ != -1).map(_.toByte).toArray
            val test = new String(response, Charset.forName("UTF-8"))
            val idk = test.split("\"")
            var origen = idk(3).replace(' ', '+')
            var destino = idk(7).replace(' ', '+')
            println(origen)
            println(destino)
            val request_url = "https://maps.googleapis.com/maps/api/directions/json?origin=" + origen + "&destination=" + destino + "&key=AIzaSyAzzrnc71pLvEvOdY322DQwwbUsFQZT7Vg"
            val url = new URL(request_url)
            val br = new BufferedReader(new InputStreamReader(url.openStream()))
            var maps: String = ""
            var temp: String = ""
            while(br.ready()){
        		temp = br.readLine()
        		maps = maps + temp
      		}
      		var splitted = maps.split("\"steps\" \\: \\[|\\],               \"traffic_speed_entry\"")
      		splitted = splitted(1).split("\"start_location\" \\: |\"end_location\" \\: |,                     \"html_instructions\"|,                     \"travel_mode\"")
      		val buf = scala.collection.mutable.ListBuffer.empty[String]
      		var c = 0

      		while(c < splitted.length){
        		if(c % 2 == 1){
          			buf += splitted(c)
        		}
        		c = c + 1
      		}

      		val steps = buf.toList
      		var json = ""
      		c=3
      		if(steps.length == 1)
        		json += "{\"ruta\":["  + steps(1) + "]}"
      		else if(steps.length == 2)
        		json = "{\"ruta\":["  + steps(1) + ", " + steps(0) + "]}"
      		else if(steps.length == 3)
        		json = "{\"ruta\":["  + steps(1) + ", " + steps(0) + ", " + steps(2) + "]}"
      		else{
        		json = "{\"ruta\":[" + steps(1) + ", " + steps(0) + ", " + steps(2) + ", "

        		while(c < steps.size ){
          			if(c % 2 == 0){
            			json = json + steps(c) + ", "
          			}
          			c = c + 1
        		}
      		}


      		json = json.dropRight(2)
      		json = json + "]}"

      		response = json.getBytes(Charset.forName("UTF-8"))
      		clientt.getResponseHeaders.add("content-type", "json")
      		clientt.sendResponseHeaders(200, response.length.toLong)
      		os.write(response)
      		os.close()

		}
	}
}

class exercise2() extends HttpHandler{
  override def handle(client: HttpExchange){
    try{
      if(client.getRequestMethod() == "POST"){
        val jsonReq = new JsonParser().parse(new String(scala.io.Source.fromInputStream(client.getRequestBody()).mkString)).getAsJsonObject();
        var coordinatesLink = "https://maps.googleapis.com/maps/api/geocode/json?address=ADDRESS&key=AIzaSyDxkk38M1uRTyD6vw7OyBUQ8x_2W2qOsEU";
        try{
          coordinatesLink = coordinatesLink.replace("ADDRESS", jsonReq.get("origen").getAsString().replace(" ", "+"));
        }catch{
          case _: Throwable =>
          val response = "{\"error\": \"No se especifico origen\"}";
          client.getResponseHeaders().add("content-type", "json");
          client.sendResponseHeaders(400, response.getBytes("UTF-8").size.asInstanceOf[Number].longValue);
          client.getResponseBody().write(response.getBytes("UTF-8"));
          client.getResponseBody().close();
        }
        var conn = (new URL(coordinatesLink).openConnection()).asInstanceOf[HttpURLConnection];
        conn.setRequestMethod("GET");
        println(coordinatesLink);
        var mapsResp = new String(scala.io.Source.fromInputStream(conn.getInputStream()).mkString);
        var nearMeLink = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=LAT,LNG&radius=3000&types=restaurant&name=cruise&key=AIzaSyCx14BVgeJ89yixorOA7gaab-uscUWlNFU";
        nearMeLink = nearMeLink.replace("LAT", new JsonParser().parse(mapsResp).getAsJsonObject().get("results").getAsJsonArray().get(0).getAsJsonObject().get("geometry").getAsJsonObject().get("location").getAsJsonObject().get("lat").getAsString());
        nearMeLink = nearMeLink.replace("LNG", new JsonParser().parse(mapsResp).getAsJsonObject().get("results").getAsJsonArray().get(0).getAsJsonObject().get("geometry").getAsJsonObject().get("location").getAsJsonObject().get("lng").getAsString());
        conn = (new URL(nearMeLink).openConnection()).asInstanceOf[HttpURLConnection];
        conn.setRequestMethod("GET");
        println(nearMeLink);
        mapsResp = new String(scala.io.Source.fromInputStream(conn.getInputStream()).mkString);
        var restaurantes = Map[String, ArrayBuffer[JsonObject]]();
        restaurantes("restaurantes") = new ArrayBuffer[JsonObject]();
        val locations = new JsonParser().parse(mapsResp).getAsJsonObject().get("results").getAsJsonArray();
        for(location <- locations){
          var temp = new JsonObject();
          temp.add("nombre", location.getAsJsonObject().get("name"));
          temp.add("lat", location.getAsJsonObject().get("geometry").getAsJsonObject().get("location").getAsJsonObject().get("lat"));
          temp.add("lon", location.getAsJsonObject().get("geometry").getAsJsonObject().get("location").getAsJsonObject().get("lng"));
          restaurantes("restaurantes") += temp;
        }
        var response = new GsonBuilder().create().toJson(restaurantes("restaurantes").asJava);
        response = "{\"restaurantes\":" + response + "}";
        client.getResponseHeaders().add("content-type", "json");
        client.sendResponseHeaders(500, response.getBytes("UTF-8").size.asInstanceOf[Number].longValue);
        client.getResponseBody().write(response.getBytes("UTF-8"));
        client.getResponseBody().close();
        println(jsonReq);
      }
    }catch{
      case _: Throwable =>
      val response = "{\"error\": \"Opps, ha ocurrido un error :/\"}";
      client.getResponseHeaders().add("content-type", "json");
      client.sendResponseHeaders(500, response.getBytes("UTF-8").size.asInstanceOf[Number].longValue);
      client.getResponseBody().write(response.getBytes("UTF-8"));
      client.getResponseBody().close();
    }
  }
}

class exercise3() extends HttpHandler{
  override def handle(client: HttpExchange){
    try{
      if(client.getRequestMethod() == "POST"){
        val jsonReq = new JsonParser().parse(new String(scala.io.Source.fromInputStream(client.getRequestBody()).mkString)).getAsJsonObject();
        try{
          jsonReq.get("nombre").getAsString();
          jsonReq.get("data").getAsString();
        }catch{
          case _: Throwable =>
          val response = "{\"error\": \"No se especifico origen\"}";
          client.getResponseHeaders().add("content-type", "json");
          client.sendResponseHeaders(400, response.getBytes("UTF-8").size.asInstanceOf[Number].longValue);
          client.getResponseBody().write(response.getBytes("UTF-8"));
          client.getResponseBody().close();
        }
        var decodedImg = Base64.getDecoder().decode(jsonReq.get("data").getAsString())
        val decodedImgWidth = (ByteBuffer.allocate(4).put(decodedImg.slice(0x12, 0x15).reverse).getInt(0))/256;
        val decodedImgHeight = (ByteBuffer.allocate(4).put(decodedImg.slice(0x16, 0x19).reverse).getInt(0))/256;
        val decodedImgPixelArray = (ByteBuffer.allocate(4).put(decodedImg.slice(0x0A, 0x0D).reverse).getInt(0))/256;

        for(i <- 0 until decodedImgHeight; j <- 0 until decodedImgWidth){
          // println(i, j)
          val pos = decodedImgPixelArray+(i*decodedImgWidth*4)+(j*4);
          val greyPixel = ((decodedImg(pos+3) + decodedImg(pos+2) + decodedImg(pos+1))/3).asInstanceOf[Byte];
          decodedImg(pos) = greyPixel;
          decodedImg(pos+1) = greyPixel;
          decodedImg(pos+2) = greyPixel;
        }

        val encodedImg = Base64.getEncoder().encodeToString(decodedImg);
        val response = "{\"nombre\":\"" + jsonReq.get("nombre").getAsString().replace(".", "(blanco y negro).") + ",\",\"data\":\"" + encodedImg + "\"}";
        client.getResponseHeaders().add("content-type", "json");
        client.sendResponseHeaders(200, response.getBytes("UTF-8").size.asInstanceOf[Number].longValue);
        client.getResponseBody().write(response.getBytes("UTF-8"));
        client.getResponseBody().close();
      }
    }catch{
      case _: Throwable =>
      val response = "{\"error\": \"Opps, ha ocurrido un error :/\"}";
      client.getResponseHeaders().add("content-type", "json");
      client.sendResponseHeaders(500, response.getBytes("UTF-8").size.asInstanceOf[Number].longValue);
      client.getResponseBody().write(response.getBytes("UTF-8"));
      client.getResponseBody().close();
    }
  }

  class ejercicio4() extends HttpHandler{
    override def handle(client: HttpExchange){
        if (client.getRequestMethod() == "POST") {
            val os: OutputStream = client.getResponseBody()
            var cont = 0
            var output = new ByteArrayOutputStream()
            var input = client.getRequestBody()
            var response: Array[Byte] = Stream.continually(input.read).takeWhile(_ != -1).map(_.toByte).toArray
            val test = new String(response, Charset.forName("UTF-8"))
            
            var idk = test.split("\"")
            val nombre = idk(3)
            var img_data = idk(7)
            var alto_temp = idk(12).split("\\: |\\,")
            var ancho_temp = idk(14).split("\\: |\n")
            var alto = alto_temp(1).toInt
            ancho_temp = ancho_temp(1).split(" ")
            var ancho= ancho_temp(0).subSequence(0, ancho_temp(0).length - 1).toString().toInt

            println(alto)
            println(ancho)

            var small_img = ""

            var img= Base64.getDecoder().decode(img_data)
            var bais: ByteArrayInputStream = new ByteArrayInputStream(img)
            var editable_img: BufferedImage = ImageIO.read(bais)
            var smaller_img: BufferedImage = new BufferedImage(ancho, alto, 1)
            
            var height = editable_img.getHeight()
            var width = editable_img.getWidth()
            
            var divX = width.toFloat/ancho.toFloat
            var divY = height.toFloat/alto.toFloat

            var resizedWidth = (width/divX).toInt
            var resizedHeight = (height/divY).toInt
            println("" + width + " / " + ancho + " = " + divX)
            println("" + height + " / " + alto + " = " + divY)

            for(x <- 0 to resizedWidth - 1){
                for(y <- 0 to resizedHeight - 1){
                    var pixel = editable_img.getRGB((x * divX).toInt, (y * divY).toInt)
                    smaller_img.setRGB(x, y, pixel)
                }
            }

            var baos: ByteArrayOutputStream = new ByteArrayOutputStream()
            ImageIO.write(smaller_img, "bmp", baos)
            var new_img = baos.toByteArray()
            small_img = Base64.getEncoder().encodeToString(new_img)

            var json= ""
            var name = nombre.split("\\.")
            json = "{\"nombre\":\"" + name(0) + "(reducido)." + name(1) + "\", \"data\": \"" + small_img + "\"}"
            println(json)
            response = json.getBytes(Charset.forName("UTF-8"))
            client.getResponseHeaders().add("content-type", "json")
            client.sendResponseHeaders(200, response.size.toLong)
            os.write(response)
            os.close()
        }
    }
}
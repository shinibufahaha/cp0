import scala.collection.mutable.{Map, ListBuffer}
import scala.util.control.Breaks.{break, breakable}
import java.io.{File, PrintWriter, FileWriter, BufferedWriter}
import scala.io.Source
import ujson._
import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Method, Call}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language._
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

object ComponentMethods {
  val ComponentEntry = Map(
    // "Activity" -> List("onCreate", "onNewIntent", "onStart"),
    // "Service" -> List("onStartCommand"),
    // "IntentService" -> List("onHandleIntent"),
    // "JobIntentService" -> List("onHandleWork"),
    "Provider" -> List("call", "query", "update", "insert", "delete", "openFile", "openAssetFile", "openFileHelper", "applyBatch", "openTypedAssetFile", "openPipeHelper"),
    // "Receiver" -> List("onReceive")
  )

  val combinedList: List[String] = ComponentEntry.values.flatten.toList

  def run(c: String): Unit = {
    val json_result = ujson.Obj()

    val source = Source.fromFile(c)
    val jsonString = try source.mkString finally source.close()
 // 解析JSON
    val jsonArray = ujson.read(jsonString).arr

 // 遍历数组
    for (jsonElement <- jsonArray) {
      val elementKey = jsonElement.str  
      json_result(elementKey) = Arr()
      
      // 假设cpg.typeDecl.fullNameExact返回一个包含方法的类
        // 尝试获取类型声明，如果存在则继续处理
        cpg.typeDecl.fullNameExact(elementKey).headOption match {
        case Some(componentClass) =>
            val methods = componentClass.method.l
            for (method <- methods) {
            val methodName = method.name
            if (ComponentMethods.combinedList.contains(methodName)) {
                json_result(elementKey).arr.append(method.code.toString)
            }
            }
        case None =>
            println(s"Warning: No type declaration found for key '$elementKey'")
        }
    }

    // 写入文件
    val writer = new PrintWriter(new File("/mnt/data/program/V4/output.json"))
    writer.write(json_result.render())
    writer.close()
  }

  def test(): Unit = {
    var c = "/mnt/data/program/V4/exported.json"
    val outputPath = "/mnt/data/program/V4/output.json"
    val outputContent = Source.fromFile(outputPath)
    val outputString = try outputContent.mkString finally outputContent.close()
    val outputJson = ujson.read(outputString).obj

    val json_result = ujson.Obj()

    val source = Source.fromFile(c)
    val jsonString = try source.mkString finally source.close()
 // 解析JSON
    val jsonArray = ujson.read(jsonString).arr

    val splits = project.inputPath.split("/")
    val cpgName = splits(splits.length - 2) + "_" + splits(splits.length - 1)
    
  
 // 遍历数组
    for (jsonElement <- jsonArray) {
      val elementKey = jsonElement.str  // 假设jsonElement是一个字符串类型的键
      json_result(elementKey) = ujson.Obj()
      
      // 假设cpg.typeDecl.fullNameExact返回一个包含方法的类
      val componentClass = cpg.typeDecl.fullNameExact(elementKey).head
      val methods = componentClass.method.l
      
      for (method <- methods) {
        val methodName = method.name
        if (ComponentMethods.combinedList.contains(methodName)) {
          json_result(elementKey)(methodName) = method.code
        }
      }
    }

    outputJson(cpgName) = json_result

    // 写入文件
    val writer = new PrintWriter(new File(outputPath))
    writer.write(outputJson.render())
    writer.close()
  }
}

@main def start() = {
    val splits = project.inputPath.split("/")
    val cpgName = splits(splits.length - 2) + "_" + splits(splits.length - 1)
    val outputPath = "/mnt/data/program/V4/output.json"
    val outputContent = Source.fromFile(outputPath)
    val outputString = try outputContent.mkString finally outputContent.close()
    val outputJson = ujson.read(outputString).obj

    val json_result = ujson.Obj()

    val source = Source.fromFile(splits.slice(0, splits.length - 1).mkString("/") + "/components.json")
    val jsonString = try source.mkString finally source.close()
 // 解析JSON
    val jsonArray = ujson.read(jsonString).arr


    
  
 // 遍历数组
    for (jsonElement <- jsonArray) {
      val elementKey = jsonElement.str  
      json_result(elementKey) = Arr()
      
      // 假设cpg.typeDecl.fullNameExact返回一个包含方法的类
        // 尝试获取类型声明，如果存在则继续处理
        cpg.typeDecl.fullNameExact(elementKey).headOption match {
        case Some(componentClass) =>
            val methods = componentClass.method.l
            for (method <- methods) {
            val methodName = method.name
            if (ComponentMethods.combinedList.contains(methodName)) {
                json_result(elementKey).arr.append(method.code.toString)
            }
            }
        case None =>
            println(s"Warning: No type declaration found for key '$elementKey'")
        }
    }

    outputJson(cpgName) = json_result

    // 写入文件
    val writer = new PrintWriter(new File(outputPath))
    writer.write(outputJson.render())
    writer.close()
  }

  start()

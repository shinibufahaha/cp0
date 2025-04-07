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

val componentInfo = Map(
  "Activity" -> List("android.content.Context.startActivity.*"),
  "Service" -> List("android.content.Context.startService.*", "android.content.Context.bindService.*"),
  "Broadcast" -> List("android.content.Context.sendBroadcast.*"),
  "Provider" -> List("android.content.ContentResolver.*")
  //"Receiver" -> List("android.content.Context.registerReceiver.*")
)

val ComponentEntry = Map(
  "Activity" -> List("onCreate", "onNewIntent", "onStart"),
  "Service" -> List("onStartCommand"),
  "IntentService" -> List("onHandleIntent"),
  "JobIntentService" -> List("onHandleWork"),
  "Provider" -> List("call", "query", "update", "insert", "delete", "openFile", "openAssetFile", "openFileHelper", "applyBatch", "openTypedAssetFile", "openPipeHelper"),
  "Receiver" -> List("onReceive")
)

val ComponentClass = List("android.content.BroadcastReceiver", "android.app.Activity", "android.content.ContentProvider", "android.app.Service", "android.app.IntentService")

// 启动 WebView Activity的函数
val startWebViewMethods = List(
  "android.content.Context.startActivity:void(android.content.Intent,android.os.Bundle)",
  "android.content.Context.startActivity:void(android.content.Intent)",
  "android.app.Activity.startActivity:void(android.content.Intent)",
 "com.alibaba.android.arouter.facade.Postcard.navigation:java.lang.Object()",
  "com.alibaba.android.arouter.facade.Postcard.navigation:java.lang.Object(android.content.Context)",
  "com.alibaba.android.arouter.facade.Postcard.navigation:java.lang.Object(android.content.Context,com.alibaba.android.arouter.facade.callback.NavigationCallback)",
  "com.alibaba.android.arouter.facade.Postcard.navigation:void(android.app.Activity,int)",
  "com.alibaba.android.arouter.facade.Postcard.navigation:void(android.app.Activity,int,com.alibaba.android.arouter.facade.callback.NavigationCallback)",
  
  )

val androidCheckMethods = List(  
  "android.support.v4.media.session.MediaSessionCompat.getCallingPackage:java.lang.String()",
  "android.support.v4.media.session.MediaSessionCompat$MediaSessionImpl.getCallingPackage:java.lang.String()",
  "android.support.v4.media.session.MediaSessionCompat$MediaSessionImplBase.getCallingPackage:java.lang.String()",
  "androidx.core.app.ShareCompat.getCallingPackage:java.lang.String(android.app.Activity)",
  "androidx.core.app.ShareCompat.getCallingPackage:java.lang.String(android.content.Intent)",
  "android.support.v4.media.session.MediaSessionCompat$MediaSessionImplApi21.getCallingPackage:java.lang.String()",
  "androidx.core.app.ShareCompat$IntentReader.getCallingPackage:java.lang.String()",
  "android.os.Binder.getCallingUid:int()",
  "android.app.Activity.getCallingPackage:java.lang.String()",
  "android.content.ContentProvider.getCallingPackage:java.lang.String()"
)

valr wrongCheckMethods = List("android.os.Binder.getCallingPid:int()")

object MapToFile {
  def writeJsonToFile(jsonObj: ujson.Obj, filePath: String): Unit = {
  // 检查ujson.Obj是否为空
  if (jsonObj.value.isEmpty) {
    return
  } else {
    println(filePath)
    // 将ujson.Obj转换为JSON字符串
    val json = ujson.write(jsonObj, indent = 2)

    // 创建文件对象
    val file = new File(filePath)

    // 检查并创建必要的目录
    val parentDir = file.getParentFile
    if (parentDir != null && !parentDir.exists()) {
      parentDir.mkdirs()
    }

    // 创建BufferedWriter对象
    val bw = new BufferedWriter(new FileWriter(file))

    try {
      // 将JSON字符串写入文件
      bw.write(json)
      bw.write("\n")
    } finally {
      // 关闭BufferedWriter
      bw.close()
    }
  }

    
  }

}

object MyUtil {



  def scanAST(methodName: String, callId: String, excludeCallName: Array[String]): String = {
    val method = cpg.method.fullNameExact(methodName).head
    val ast = method.ast
    val code = new StringBuilder()
  breakable {
    for (node <- ast) {
      // 如果节点的代码在排除列表中，并且节点的ID不等于callId，则跳过当前节点
      if (node.isMethod || (excludeCallName.contains(node.code) && node.id.toString != callId)) {
        ()
      } else {
        if (node.id.toString == callId) {
          // acode.append("step in: ")
          code.append(node.code + ";")
          break()
        }
        code.append(node.code + ";")
      }
    }
  }
                        

    code.toString() // 返回构建的代码字符串
  }



  def getPathInfo(entryMethodName: String, target_method_fullName: String, outFile: String, depth: Int): ujson.Value = {
    val json_result = ujson.Obj()
    val paths = PathFinder.dfs(cpg, entryMethodName, "-1", target_method_fullName, depth)

    for (i <- 0 until paths.size) {
      val path = paths(i)
      json_result(i.toString) = ujson.Arr()
      // 暂时不加入路径 TODO
      // json_result(i.toString).arr.append(paths(i))
      for (j <- 0 until path.size) {
        val p = path(j)
        val json_temp = ujson.Obj()
        //json_temp("name") = p._1
        // json_temp("code") = MyUtil.getJimpleCode(p._1).toString
        if (j < path.size - 1) {
          val p_next = path(j+1)
          json_temp("code") = MyUtil.scanAST(p._1, p_next._2, Array[String]())
        } else {
          json_temp("code") = MyUtil.getJimpleCode(p._1).toString
        }
        
        json_result(i.toString).arr.append(json_temp)
        // if (p.contains("com.alibaba.android.arouter.facade.Postcard.navigation") && MyUtil.getJimpleCode(p).toString.contains("withString")) {
        //   //去除json_result的记录
        //   json_result(i.toString).arr.clear()
        // }
        
      }
    }
    json_result
    //MapToFile.writeJsonToFile(json_result, outFile)
  }

  // 根据method的fullName获取jimple形式的代码
  def getJimpleCode(methodName: String): String = {
    var finalMethodName = methodName
    // 最后一个name：android.content.Context.startActivity:void(android.content.Intent) : 30066927896
        if (methodName.contains(" : ")) {
          finalMethodName = methodName.split(" : ")(0)
        }
    val method = cpg.method.fullNameExact(finalMethodName).head
    if (method != null) {
      method.code
    } else {
      ""
    }
    // if (method.isDefined) {
    //   method.code
    // } else {
    //   Iterator.empty
    // }
  }

    def listCpgFiles(dir: String): List[String] = {
      val path = Paths.get(dir)
      if (Files.exists(path) && Files.isDirectory(path)) {
        Files.walk(path)
          .iterator().asScala
          .filter(Files.isRegularFile(_))
          .map(_.toString)
          .filter(_.endsWith(".cpg"))
          .toList
      } else {
        List.empty[String]
      }
  }

  // 找到调用者的初始化方法，因为调用者可能会初始化一些成员变量，然后再将成员变量的值赋给intent
  def initialClassFields(className: String)(implicit cpg: Cpg): Unit = {
    val targetClass = cpg.typeDecl.fullName(className).headOption
    for (method <- targetClass.method.l) {
      if (method.name == "<init>") {

      }
    }
  }

    def parseArg(argName: String, method: Method): String = {
        // breakable {
        //     for (assignment <- method.assignment.l) {
        //         val args = assignment.argument.l
        //         if (args.size == 1) {
        //             break
        //         } else if (args(0).code == argName) { // assignment的argument(0)是等式的左边
        //             //返回等式的右边
        //             args(1).code
        //         }
        //     }
        // }
        // argName
        // 逐行解析method的code
        var jsonObj = Obj()
        val codes = method.code.split("\n")
        var assignmentCode = argName
        var result = argName
        for (code <- codes) {
          // 记录每个参数或者变量的值
          // 一般而言，对象本身是r0，入参和变量以$开头，然后r加一个数字
          if (code.contains("=") && code.contains(argName) && !code.contains("if")) {
              result = code.strip()
              result
            // if (code.strip().indexof(argName) == 0) {
            //   code.strip().
            // }
          
            // // 取等号两侧的信息
            // val parts = code.split("=")
            // if (parts.length == 2) {
            //   val symbol = parts(0).split(" ").last
            //   println(symbol)
            //   val result = parts(1)
            //   if (symbol == argName) {
                  
            //       println(result)
            //   }
              
            // }
          }
        }
        result
    }

  // 参数类型io.shiftleft.codepropertygraph.generated.nodes.Expression = Identifier
    def parseIndentifier(identifier: Expression, method: Method, callSiteId: Long): Map[String, ListBuffer[ListBuffer[String]]] = {
      val result = Map[String, ListBuffer[ListBuffer[String]]]()
      val calls = method.call.l
      for (call <- calls) {
          breakable {
          val args = call.argument.l
          val key = call.name

          // 检查 args 长度，避免索引越界
          if (args.nonEmpty && args(0).code == identifier.code) {
              var updatedList = result.getOrElseUpdate(key, ListBuffer())
              val argList = ListBuffer[String]()
              for (i <- 1 until args.size) {
                  // TODO 如果用于初始化intent的
                  if (args(i).isLiteral) {
          
                      argList += args(i).code
                  } else {
                    
                      argList += parseArg(args(i).code, method)
                  }
                  
              }
              updatedList = result.getOrElseUpdate(key, ListBuffer()) :+ argList
              result.update(key, updatedList)
          }
          }
      }
      result
    }

  def isStartComponent(methodName: String): Boolean = {
    componentInfo.exists { case (key, value) =>
      value.exists { name =>
        methodName.startsWith(name.slice(0, name.length - 2))
      }
    }
  }

  // 获得调用者和被调用者的信息
  def getCallInfo(calleeName: String): ujson.Value = {
    var json_result = ujson.Obj();
    // 调用点
    val callSites = cpg.call.methodFullName(calleeName).l;
    // 调用者
    for (callSite <- callSites) {
      val callerName = callSite.method.fullName
      if (json_result.obj.contains(callerName) == false) {
        json_result(callerName) = ujson.Arr();
      }
      json_result(callerName).arr.append(callSite.id);
    }
    json_result;

  }

}

object Component {
  // 寻找所有的组件类
  def getComponentClassName()(implicit cpg: Cpg): Map[String, List[String]] = {
    val result = Map[String, List[String]]()

   
    val receivers = cpg.typeDecl.fullNameExact("android.content.BroadcastReceiver").derivedTypeDeclTransitive.fullName.filter(!_.startsWith("androidx")).l
    val activitys = cpg.typeDecl.fullNameExact("android.app.Activity").derivedTypeDeclTransitive.fullName.filter(!_.startsWith("androidx")).l
    val providers = cpg.typeDecl.fullNameExact("android.content.ContentProvider").derivedTypeDeclTransitive.fullName.filter(!_.startsWith("androidx")).l
    val services = cpg.typeDecl.fullNameExact("android.app.Service").derivedTypeDeclTransitive.fullName.filter(!_.startsWith("androidx")).l
    val intentServices = cpg.typeDecl.fullNameExact("android.app.IntentService").derivedTypeDeclTransitive.fullName.filter(!_.startsWith("androidx")).l

    result += ("Receiver" -> receivers)
    result += ("Activity" -> activitys)
    result += ("Provider" -> providers)
    result += ("Service" -> services)
    result += ("IntentService" -> intentServices)

    // TEST
    // result += ("Receiver" -> List("com.yozo.DispatchActivity"))
    // result += ("Activity" -> List("com.yozo.DispatchActivity"))
    // result += ("Provider" -> List("com.yozo.DispatchActivity"))
    // result += ("Service" -> List("com.yozo.DispatchActivity"))
    // result += ("IntentService" -> List("com.yozo.DispatchActivity"))

    result
  }

  def run(cpg: Cpg, packageName: String, componentType: String, methods: List[String]): ujson.Value = {
    var jsonObj_1 = ujson.Obj()
    for (method <- methods) {
      // 获取所有startActivity的调用点
      val regCallSites_1 = cpg.call.methodFullName(method).l
      // 获取所有startActivityForResult的调用点
      // val regCallSites_2 = cpg.call.methodFullName("android.content.Context.startActivityForResult.*").l

      val regCallSites = regCallSites_1
      // val result = ListBuffer[Map[String, Any]]()

      // 解析startActivity的参数，我们只关心startActivity的第一个参数——intent
      for (regCallSite <- regCallSites) {
        breakable {
          var jsonObj = ujson.Obj()
          // 调用点的id，具有唯一性
          val id = regCallSite.id
          jsonObj_1(id.toString) = jsonObj
          // 调用者的信息
          try {
            val callerMethod = regCallSite.method
            val methodName = callerMethod.name
            val methodFullName = callerMethod.fullName
            //val typeDeclName = callerMethod.typeDecl.headOption.fullName.head
            // 如果调用者是androidx，则忽略
            if (methodFullName.startsWith("androidx")) {
              break
            }
            //println(s"$typeDeclName: $methodName") 
            jsonObj("callerInfo") = methodFullName
            jsonObj("methodName") = regCallSite.methodFullName
            val cared_argument = regCallSite.argument(1)

            val initInfo = MyUtil.parseIndentifier(cared_argument, callerMethod, id)
            jsonObj("initInfo") = initInfo
            // println(jsonObj)
            // result.append(jsonResult)
           
          } catch {
            case e: Exception =>
              // 记录异常信息或其他处理
              println(s"An error occurred: ${e.getMessage}")
          }

        }

      }
      
    }
    jsonObj_1
  }
}


object ProviderFinder {
  def find()(implicit cpg: Cpg): Unit = {
    val callSites = cpg.call.methodFullName("android.content.ContentResolver.*").l
    for (callSite <- callSites) {
      // 暂时只关心第一个参数uri是否被保护
    }
  }
}


object Parser {
  def run()(implicit cpg: Cpg): Unit = {
    // for ((key, value) <- componentInfo) {
    //   Component.run(key, value)
    // }
  }
}

object PathFinder {

  // def dfs_back(cpg: Cpg, currentMethodFullName: String, currentCallSiteId: Long, targetMethodFullName: String, maxDepth: Int, currentDepth: Int = 0, visited: Set[String] = Set()): List[List[String]] = {
    
  //   if (currentMethodFullName.contains("<operator>") || currentMethodFullName.contains("androidx") || currentMethodFullName.startsWith("java.lang")) {
  //     List()
  //   } else {
  //     //println(s"currentMethodFullName: $currentMethodFullName")
  //     if (currentMethodFullName == targetMethodFullName) {
        
  //       List(List(s"$targetMethodFullName : $currentCallSiteId"))
  //     } else if (visited.contains(currentMethodFullName) || currentDepth >= maxDepth) {
  //       List()
  //     } else {
        
  //       val newVisited = visited + currentMethodFullName
  //       val currentMethodl = cpg.method.fullNameExact(currentMethodFullName).l
  //       if (currentMethodl.nonEmpty) {
  //         val currentMethod = currentMethodl.head
  //         val callDetails = currentMethod.call.map(n => (n.id, n.methodFullName)).toList
          
  //           if (callDetails.isEmpty) {
  //             List()
  //           } else {
  //             callDetails.flatMap { case (id, methodFullName) =>
  //               dfs(cpg, methodFullName, id, targetMethodFullName, maxDepth, currentDepth + 1, newVisited).map(path => currentMethodFullName :: path)
  //             }
  //           }
  //       } else {
  //         List()
  //       }
        
  //     }
  //   }
  // }

  def dfs(cpg: Cpg, currentMethodFullName: String, currentCallSiteId: String, targetMethodFullName: String, maxDepth: Int, currentDepth: Int = 0, visited: Set[String] = Set()): List[List[(String, String)]] = {
    if (currentMethodFullName.contains("<operator>") || currentMethodFullName.contains("androidx") || currentMethodFullName.startsWith("java.lang")) {
      List()
    } else {
      if (currentMethodFullName == targetMethodFullName) {
        List(List((targetMethodFullName, currentCallSiteId)))
      } else if (visited.contains(currentCallSiteId) || currentDepth >= maxDepth) {
        List()
      } else {

        val newVisited = visited + currentCallSiteId
        val currentMethodl = cpg.method.fullNameExact(currentMethodFullName).l
        if (currentMethodl.nonEmpty) {
          val currentMethod = currentMethodl.head
          val callDetails = currentMethod.call.map(n => (n.id.toString, n.methodFullName)).toList
          
          if (callDetails.isEmpty) {
            List()
          } else {
            callDetails.flatMap { case (id, methodFullName) =>
              dfs(cpg, methodFullName, id.toString, targetMethodFullName, maxDepth, currentDepth + 1, newVisited).map(path => (currentMethodFullName, currentCallSiteId) :: path)
            }
          }
        } else {
          List()
        }
      }
    }
  }

  // def back_dfs(): List[List[(String, String)]] = {

  // }

}


object My_Main {


  def runScript(): Unit = {
    // 调用链的默认深度
    var depth = 15
    // 默认的输出目录
    var outDir = "/mnt/data/result"
    // if (args.length >= 1) {
    //   outDir = args(0)
    // }
    // if (args.length >= 2) {
    //   depth = args(1).toInt
    // }
    IntentParser.getExtraInfos(s"$outDir/Intent.json")
    //WebViewParser.getPathInfo("all", s"$outDir/WebView.json", depth)

  }

  def run(cpgDir: String, jsonPath: String): Unit = {

   // 读取JSON文件内容
    val jsonString = os.read(os.Path(jsonPath))

    // 解析JSON字符串
    val json = ujson.read(jsonString)

    // 创建一个可变的Map
    val resultMap = Map[String, List[String]]()

    // 假设JSON文件的结构是一个对象，键是字符串，值是字符串数组
    json.obj.foreach { case (key, value) =>
      val list = value.arr.map(_.str).toList
      resultMap += (key -> list)
    }

    // 获取所有的cpg路径
    val cpgPaths = MyUtil.listCpgFiles(cpgDir)
    print(cpgPaths)
    // 逐个解析cpg
    for (cpgPath <- cpgPaths) {
      println(cpgPath)
      val cpg = importCpg(cpgPath).head
      // 获取所有暴露组件启动其他组件的调用链
      parseCpg(cpg, cpgPath, resultMap, "/mnt/data/program/Rowling/joern", 15)
  
      // 释放cpg
      //cpg.foreach(_.close())
      cpg.close()
    }
    
  }

  def parseCpg(cpg: Cpg, cpgPath: String, entryComponentInfo: Map[String, List[String]], resultDir: String, depth: Int): Unit = {
    val jsonObj = ujson.Obj()
    val packageName = cpgPath.split("/").dropRight(1).last
    
    // TODO
    // val apkName = cpgPath.split("/")dropRight()
    try {
      // 启动组件的函数
      val targetMethods = ListBuffer[Method]()
      for ((componentType, names) <- componentInfo) {
        for (name <- names) {
          val methods = cpg.method.fullName(name).l
          if (methods.nonEmpty) {
            targetMethods.append(methods.head)
          } else {
            println(s"Warning: No methods found for name $name")
          }
        }
      }

      // 获取启动组件的信息
      var jsonObj_2 = ujson.Obj()
      for ((key, value) <- componentInfo) {
        jsonObj_2(key) = Component.run(cpg, packageName, key, value)
      }
      MapToFile.writeJsonToFile(jsonObj_2, s"/mnt/data/program/Rowling/joern/callSite/$packageName.json")
      // val javaFileMethods = cpg.method.filter(_.fullName.startsWith("java.io.File")).l
      // if (javaFileMethods.nonEmpty) {
      //   for (javaFileMethod <- javaFileMethods) {
      //     targetMethods.append(javaFileMethod)
      //   }
          
      // }
      

      for ((componentType, vulComponents) <- entryComponentInfo) {
        val entryMethodNames = ComponentEntry.getOrElse(componentType, List())
        
          for (vulComponentInfo <- vulComponents) {
            breakable {
            
            val packageName_2 = vulComponentInfo.split("\\|")(0)
            if (!(packageName == packageName_2)) {
              break
            }
            val vulComponent = vulComponentInfo.split("\\|")(1)
            
            // 获取暴露组件对应的类
            val componentClasses = cpg.typeDecl.fullNameExact(vulComponent).l
            if (componentClasses.nonEmpty) {
              val componentClass = componentClasses.head
              // 分析暴露组件的入口函数

              for (entryMethodName <- entryMethodNames) {
                val entryMethods = componentClass.method.name(entryMethodName).l
                if (entryMethods.nonEmpty) {
                  val entryMethod = entryMethods.head
                  // 寻找入口函数到启动组件函数的路径
                  breakable {
                    for (targetMethod <- targetMethods) {
                      
                      val paths = PathFinder.dfs(cpg, entryMethod.fullName, "-1", targetMethod.fullName, depth)
                      // 将paths记录到文件中
                      if (paths.nonEmpty) {
                        val targetMethodName = targetMethod.name
                        jsonObj(s"$vulComponent -> $targetMethodName") = paths.toSet
                      }

                    }
                  }

                } else {
                  // println(s"Warning: No entry methods found for name $entryMethodName in component $vulComponent")
                }
              }
            } else {
              println(s"Warning: No component classes found for $vulComponent")
            }
          }
          }

      }
      MapToFile.writeJsonToFile(jsonObj, s"$resultDir/$packageName/paths.json")
    } catch {
      case e: Exception =>
        // 记录异常信息或其他处理
        println(s"An error occurred: ${e.getMessage}")
        e.printStackTrace() // 打印堆栈跟踪信息
        MapToFile.writeJsonToFile(jsonObj, s"$resultDir/$packageName/paths.json")
    }
}


  
  // def run(cpgDir: String, jsonPath: String): Unit = {
  //   val cpgDir = "/mnt/data/Program/PAD/cpg"
    
  //   // 从json文件读取暴露组件的信息
  //   val fileSource = Source.fromFile(jsonPath)
  //   val jsonString = try fileSource.mkString finally fileSource.close()

  //   // 解析 JSON 字符串
  //   val jsonData = ujson.read(jsonString)
  //   val allComInfo = MyUtil.jsonToMap(jsonData)

  //   // 启动组件的函数
  //   val targetMethods = ListBuffer[Method]()
  //   for ((componentType, names) <- componentInfo) {
  //     for (name <- names) {
  //       targetMethods.append(cpg.method.fullName(name).head)
  //     }
  //   }
  //   // 遍历目录下的所有cpg文件

  //   for (cpgPath <- cpgDir) {

  //   }
  // }
}


object Test {

  def run(cpgPath: String, jsonPath: String): Unit = {

    // 读取JSON文件内容
      val jsonString = os.read(os.Path(jsonPath))

      // 解析JSON字符串
      val json = ujson.read(jsonString)

      // 创建一个可变的Map
      val resultMap = Map[String, List[String]]()

      // 假设JSON文件的结构是一个对象，键是字符串，值是字符串数组
      json.obj.foreach { case (key, value) =>
        val list = value.arr.map(_.str).toList
        resultMap += (key -> list)
      }

      // 获取所有的cpg路径

      // 逐个解析cpg

        // 获取所有暴露组件启动其他组件的调用链
      My_Main.parseCpg(cpg, cpgPath, resultMap, "/mnt/data/program/Rowling/joern", 15)
  
      
    }

}

// 解析navigation的调用者，寻找navigation的目标Activity和携带的参数
object NavigationParser {
  //参数是navigation的调用函数名称
def run(): ujson.Value = {
  val navigation_name = "com.alibaba.android.arouter.facade.Postcard.navigation.*"
  val callSiteInfo = MyUtil.getCallInfo(navigation_name)
  var json_result = ujson.Obj()
  for ((callerName, calleeIds) <- callSiteInfo.obj) {
    
    // 根据官网信息，method.ast.isControlStructure.code.l会列出ast中的控制节点，但是实际运行却得不到任何结果
    // 因此暂时通过遍历call来解析，比如
    // "$r4 = c()",                                                                                                 
    // "$r5 = $r4.a(\"/category/virtual_category_new\")",                                                           
    // "$r5 = $r5.withString(\"key_VirtualCategory_id\", $r3)",                                                     
    // "$r5 = $r5.withBoolean(\"isFromNegativeScreen\", $z0)",                                                      
    // "$r5.navigation()",                                                                                          
    // "7 != $i0",                                                                                                  
    // "$r4 = c()",                                                                                                 
    // "$r5 = $r4.a(\"/funding/funding_list\")",                                                                    
    // "$r5 = $r5.withBoolean(\"isFromNegativeScreen\", $z0)",                                                      
    // "$r5.navigation()"
    val callerMethod = cpg.method.fullNameExact(callerName)
    if (callerMethod.hasNext) {
      // println(callerName)
      // println(callerMethod.head.code)
      val callees = callerMethod.head.call.l
      var find_ins = false
      var i = 0
      while (i < callees.size) {
        // 根据typeFullName找到com.alibaba.android.arouter.facade.Postcard的实例化方法
        // argumentIndex = -1,           
        // argumentName = None,          
        // code = "$r5 = $r4.a(\"/category/virtual_category_new\")",                                                                             
        // columnNumber = None,          
        // dispatchType = "STATIC_DISPATCH",                                                              
        // dynamicTypeHintFullName = IndexedSeq(),                                                        
        // lineNumber = Some(value = 8),                                                                  
        // methodFullName = "<operator>.assignment",                                                      
        // name = "<operator>.assignment",                                                                
        // order = 30,                                               
        // possibleTypes = IndexedSeq(),                                                                  
        // signature = "",                                           
        // typeFullName = "com.alibaba.android.arouter.facade.Postcard"
        
        if (callees(i).typeFullName == "com.alibaba.android.arouter.facade.Postcard" && callees(i).name == "<operator>.assignment") {
          val call = callees(i).argument(2).asInstanceOf[Call]
          if (call != null && call.argument(1) != null) {
            val activity_key = call.argument(1).code
            if (!json_result.obj.contains(activity_key)) {
              json_result(activity_key) = ujson.Arr()
            }
            i += 1
            while (i < callees.size && callees(i).code.contains("navigation") == false) {
              if (callees(i).code.contains(".with") && callees(i).name == "<operator>.assignment") {
                val innerCall = callees(i).argument(2).asInstanceOf[Call]
                if (innerCall != null && innerCall.argument(1) != null && innerCall.argument(2) != null) {
                  val json_tmp = ujson.Obj()
                  val paraName = innerCall.argument(1).code
                  val paraValue = innerCall.argument(2).code
                  val name = innerCall.name
                  json_tmp(name) = List(paraName, paraValue)
                  json_result(activity_key).arr.append(json_tmp)
                }
                i += 1
              } else {
                i += 1
              }
            }
            if (i < callees.size) {
              val json_tmp = ujson.Obj()
              json_tmp("navigation") = ujson.Arr.from(callees(i).argument.map(n => n.code))
              json_result(activity_key).arr.append(json_tmp)
            }
          }
        }
        i += 1
      }
    }
  }
  json_result
}

// 获取路由目标Activity
def getTargetActivity(): ujson.Value = {
  var json_result = ujson.Obj()
  val build_callSites = cpg.call.methodFullName("com.alibaba.android.arouter.facade.model.RouteMeta.*").l
  for (build_callSite <- build_callSites) {
    println(build_callSite.code)
    var json_tmp = ujson.Obj()
    json_tmp("targetActivity") = build_callSite.argument(2).code
    json_tmp("activityKey") = build_callSite.argument(3).code
    json_result(build_callSite.id.toString) = json_tmp
  }
  json_result
}



}



// Main.run("/mnt/data/program/Rowling/cpgs", "/mnt/data/result/test.json")


object IntentParser {
  // 获取所有intent.get***Extra的第一个参数
  def getExtraInfos(outFile: String): Unit = {
    var jsonObj = ujson.Obj()
    // 获取所有的intent.get***Extra调用点
    val calls = cpg.call.methodFullName(".*Intent.get.*Extra.*").l

    val getParaCalls = cpg.call.methodFullName("android.net.Uri.getQueryParameter.*").l

    val mergeList = calls ++ getParaCalls
    // 筛选参数数量大于1的call
    val filteredCalls = mergeList.filter(_.argument.size > 1).l
    /*
     Call(
    argumentIndex = 2,
    argumentName = None,
    code = "$r2.getBooleanExtra(\"enable_magazinelock_feature\", 0)",
    columnNumber = None,
    dispatchType = "DYNAMIC_DISPATCH",
    dynamicTypeHintFullName = IndexedSeq(),
    lineNumber = None,
    methodFullName = "android.content.Intent.getBooleanExtra:boolean(java.lang.String,boolean)",
    name = "getBooleanExtra",
    order = 2,
    possibleTypes = IndexedSeq(),
    signature = "boolean(java.lang.String,boolean)",
    typeFullName = "android.content.Intent"
  )
    */
    

    for (filteredCall <- filteredCalls) {
      var jsonObj_1 = ujson.Obj()
      var defaultValue = "N/A"
      // 调用method
      val caller_method = filteredCall.method
      // 调用类名
      val caller_class = caller_method.astParentFullName
      // 如果jsonObj的key不包含caller_class，则添加caller_class,且value是一个空的列表
      if (!jsonObj.obj.contains(caller_class)) {
        jsonObj(caller_class) = ujson.Arr()
      }
      jsonObj_1("name") = filteredCall.name
      jsonObj_1("returnType") = filteredCall.signature
      jsonObj_1("key") = filteredCall.argument(1).code
      if (filteredCall.argument.size > 2) {
        defaultValue = filteredCall.argument(2).code
      }
      jsonObj_1("defaultValue") = defaultValue
      jsonObj_1("id") = filteredCall.id
      jsonObj(caller_class).arr.append(jsonObj_1)
      //jsonObj(filteredCall.id.toString) = jsonObj_1
    }

  MapToFile.writeJsonToFile(jsonObj, outFile)

  }
}

// 拉起
object WebViewParser {
  // 获取loadUrl的调用点信息
  def getloadUrlCallInfo(outFile: String): ujson.Value = {
    var json_result = ujson.Obj()
    val callSites = cpg.call.methodFullName("android.webkit.WebView.loadUrl.*").l
    for (callSite <- callSites) {
      val callerMethod = callSite.method
      val callerName = callerMethod.fullName
      val callerClass = callerMethod.astParentFullName
      val callerCode = callerMethod.code

      val callerId = callSite.id
      val callerArgument = callSite.argument(1).code
      val jsonObj = ujson.Obj()
      jsonObj("callerName") = callerName
      //jsonObj("callerClass") = callerClass
      jsonObj("callerCode") = callerCode

      jsonObj("callerId") = callerId
      jsonObj("callerArgument") = callerArgument
      //println(jsonObj)
      json_result(callerClass) = jsonObj
    }

    
    MapToFile.writeJsonToFile(json_result, outFile)
    json_result
  }

  def getPathInfo(entry_class_fullName: String, outFile: String, depth: Int): Unit = {
    var json_result = ujson.Obj()
    // 先获取所有的Activity
    val activitys = cpg.typeDecl.fullNameExact("android.app.Activity").derivedTypeDeclTransitive.fullName.filter(!_.startsWith("androidx")).l
    for (activity <- activitys) {
      if (entry_class_fullName == "all" || activity == entry_class_fullName) {
        json_result(activity) = ujson.Obj()
        val entry_class = cpg.typeDecl.fullNameExact(entry_class_fullName).head
        val entry_method = entry_class.method.filter(_.name.equals("onCreate")).head

        for (startWebViewMethod <- startWebViewMethods) {
          json_result(activity)(startWebViewMethod) = MyUtil.getPathInfo(entry_method.fullName, startWebViewMethod, outFile, depth)
        }
      }
    }

    MapToFile.writeJsonToFile(json_result, outFile)
  }

}


// 找到调用校验函数的函数
// object CheckMethodFinder {
//   def run(): ujson.Value = {
//     var json_result = ujson.Obj()
//     val checkMethods = androidCheckMethods ++ wrongCheckMethods
//     for (checkMethod <- checkMethods) {
//       val callSites = cpg.call.methodFullName(checkMethod).l
//       for (callSite <- callSites) {
//         val callerMethod = callSite.method
//         val callerName = callerMethod.fullName
//         if (!json_result.obj.contains(callerName)) {
//           json_result(callerName) = ujson.Arr()
//         }
//         json_result(callerName).arr.append(callSite.id)
//       }
//     }
//     json_result
//   }
// }

@main def start(depth: Int, outFile: String) = {

    // // 调用链的默认深度
    // var depth = 15
    // // 默认的输出目录
    // var outDir = "/mnt/data/result"
    // val cpgPath = project.inputPath
    // val finaloutFile = cpgPath.replace(".cpg", ".json")
     
     
     //IntentParser.getExtraInfos(outFile)
}


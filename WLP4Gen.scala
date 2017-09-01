import scala.io.Source

object WLP4Gen {
  private var symbolTable = scala.collection.mutable.Map[String,String]() // eg: a -> 0, b -> -4
  private var symbolTableIdType = scala.collection.mutable.Map[String,String]() // eg: a -> int, b -> int*
  private var terminals = scala.collection.mutable.Set[String](
    "AMP", "BECOMES", "BOF", "COMMA", "DELETE", "ELSE", "EOF", "EQ", "GE", "GT", "ID", "IF", "INT", "LBRACE", "LBRACK",
    "LE", "LPAREN", "LT", "MINUS", "NE", "NEW", "NULL", "NUM", "PCT", "PLUS", "PRINTLN", "RBRACE", "RBRACK", "RETURN",
    "RPAREN", "SEMI", "SLASH", "STAR", "WAIN", "WHILE")

  private val decrementSP = "sub $30, $30, $4"
  private val incrementSP = "add $30, $30, $4"
  private var counter = 0;
  private var didPrint = false
  // Starter code from A03 P1 to construct a parse tree
  class TreeNode(x: String, y: String = "default") {
    var name: String = x
    var value: String = y
    var nodeType: String = ""
    var children: Array[TreeNode] = new Array[TreeNode](0)
  }

  def setNodeType(root: TreeNode): TreeNode = {
    if(root == null) return root // nothing to do here ..

    // this is triggered through a non - delc statement
    if(root.name == "ID") {
      val typeOfID = symbolTableIdType(root.value)
      root.nodeType = typeOfID
      return root
    }

    root.name match {
      case "start" | "procedure" | "procedures" | "main" | "arglist" | "params" | "paramlist" | "type" => {
        for(i <- 0 to (root.children.length - 1)) {
          root.children(i) = setNodeType(root.children(i))
        }
      }

      case "statements" => {
        root.children.length match {
          case 0 => return root // do nothing
          case 2 => {
            root.children(0) = setNodeType(root.children(0))
            root.children(1) = setNodeType(root.children(1))
          }
        }
      }

      case "statement" => {
        root.children.length match {
          case 4 => {
            root.children(0) = setNodeType(root.children(0))
            root.children(2) = setNodeType(root.children(2))
            val lvalue = root.children(0)
            val expr = root.children(2)
            // root.nodeType = lvalue.nodeType // perhaps not needed
          }
          case 11 => {
            root.children(2) = setNodeType(root.children(2))
            root.children(5) = setNodeType(root.children(5))
            root.children(9) = setNodeType(root.children(9))
          }
          case 7 => {
            root.children(2) = setNodeType(root.children(2))
            root.children(5) = setNodeType(root.children(5))
          }
          case 5 if(root.children(0).name == "PRINTLN") => {
            root.children(2) = setNodeType(root.children(2))
          }
          case 5 if(root.children(0).name == "DELETE") => {
            root.children(3) = setNodeType(root.children(3))
          }
        }
      }

      case "test" => {
        // determine type of left and right expr recursively
        root.children(0) = setNodeType(root.children(0)) // sets type of expr1
        root.children(2) = setNodeType(root.children(2)) // sets type of expr2
      }

      case "expr" => {
        root.children.length match {
          case 1 => {
            // eg: expr -> term
            root.children(0) = setNodeType(root.children(0)) // run algo on term
            val term = root.children(0)
            root.nodeType = term.nodeType
          }
          case 3 => {
            // eg: expr -> expr PLUS term
            // eg: expr -> expr MINUS term
            root.children(0) = setNodeType(root.children(0))
            root.children(2) = setNodeType(root.children(2))
            val lhs = root.children(0)
            val op = root.children(1)
            val rhs = root.children(2)

            // now we need to set the type of root for future computations
            if(lhs.nodeType == "int" && rhs.nodeType == "int") root.nodeType = "int"
            else root.nodeType = "int*"
          }
        }
      }

      case "term" => {
        root.children.length match {
          case 1 => {
            // eg: term -> factor
            root.children(0) = setNodeType(root.children(0))
            val factor = root.children(0)
            root.nodeType = factor.nodeType
          }
          case 3 => {
            // eg: term -> term STAR(*) factor
            // eg: term -> term SLASH(/) factor
            // eg: term -> term PCT(%) factor
            root.children(0) = setNodeType(root.children(0))
            root.children(2) = setNodeType(root.children(2))
            val lhs = root.children(0)
            val rhs = root.children(2)
            root.nodeType = "int"
          }
        }
      }

      case "factor" => {
        root.children.length match {
          case 1 => {
            // eg: factor -> ID
            // eg: factor -> NUM
            // eg: factor -> NULL
            root.children(0) = setNodeType(root.children(0)) // sets type of ID
            root.nodeType = root.children(0).nodeType
          }
          case 3 => {
            // eg: factor -> LPAREN expr RPAREN
            root.children(1) = setNodeType(root.children(1))
            val expr = root.children(1)
            root.nodeType = expr.nodeType // () maintains type
          }
          case 2 => {
            // eg: factor -> AMP lvalue
            // eg: factor -> STAR lvalue/factor
            val unaryOp = root.children(0)
            root.children(1) = setNodeType(root.children(1))
            val lvalue = root.children(1)
            unaryOp.value match {
              case "&" if(lvalue.nodeType == "int") => {
                root.nodeType = "int*"
              }
              case "*" if(lvalue.nodeType == "int*") => {
                root.nodeType = "int"
              }
              case _ =>
            }
          }
          case 5 => {
            root.children(3) = setNodeType(root.children(3))
            val expr = root.children(3)
            root.nodeType = expr.nodeType
          }
        }
      }

      case "lvalue" => {
        root.children.length match {
          case 1 => {
            // eg: lvalue -> ID
            root.children(0) = setNodeType(root.children(0))
            root.nodeType = root.children(0).nodeType
          }
          case 2 => {
            // eg: lvalue -> STAR factor
            root.children(1) = setNodeType(root.children(1))
            val factor = root.children(1)
            root.nodeType = "int"
          }
          case 3 => {
            // eg: lvalue -> LPAREN lvalue RPAREN
            root.children(1) = setNodeType(root.children(1))
            val lvalue = root.children(1)
            root.nodeType = lvalue.nodeType
          }
        }
      }

      case "dcl" => {
        val typeNode = root.children(0)
        val idNode = root.children(1)
        val id = idNode.value

        typeNode.children.length match {
          case 1 => {
            symbolTableIdType(id) = typeNode.children(0).value
            // now, I know that the type of this node is int. must set it.
            root.children(1).nodeType = "int"
            root.nodeType = "int"
          } // eg: a = int

          case 2 => {
            symbolTableIdType(id) = typeNode.children(0).value + typeNode.children(1).value
            // now, I know that the type of this node is int*. must set it.
            root.children(1).nodeType = "int*"
            root.nodeType = "int*"
          } // eg: a = int*
        }
      }
      case "dcls" => {
        if(root.children.length == 0) return root // nothing to do here
        root.children(0) = setNodeType(root.children(0)) // dcls
        root.children(1) = setNodeType(root.children(1)) // dcl
      }
      case _ => return root
    }
    return root
  }


  // code is now semantically correct #################################################################################

  private def makeSymbolTable(root: TreeNode): Unit = {
    if(root.name == "ID" && (! symbolTable.contains(root.value))) {
      symbolTable(root.value) = (counter * -4).toString // this is relative to the frame pointer ($29)
      counter = counter + 1
      return
    }

    if(terminals.contains(root.name)) return
    for(child <- root.children) {
      makeSymbolTable(child)
    }
  }

  def populate(): TreeNode = {
    var line = scala.io.StdIn.readLine()
    var mylist = line.split(" ").toList
    // println(mylist)
    var node = new TreeNode(mylist.head)

    if(terminals contains mylist.head) {
      node.value = mylist.tail.head
      if(node.name == "NUM") node.nodeType = "int"
      else if(node.name == "NULL") node.nodeType = "int*"
      return node
    }

    var numberOfChildren = mylist.tail.length

    if(mylist.head == "type" && numberOfChildren == 2) didPrint = true

    else if(mylist.head == "type" && numberOfChildren == 1 && !didPrint) {
      didPrint = true
      println("add $2, $0, $0") // $2 = 0
    }

    for(i <- 1 to numberOfChildren) {
      node.children :+= populate()
    }
    return node
  }

  def push(register: String, offset: Int, stackPointer: String): Unit = {
    println("sw " + register +  ", " + offset.toString + "(" + stackPointer + ")")
    println(decrementSP)
  }

  def callProcedure(procedure: String): Unit = {

  }

  def pop(register: String, offset: Int, stackPointer: String): Unit = {
    println(incrementSP)
    println("lw " + register +  ", " + offset.toString + "(" + stackPointer + ")")
  }

  def getID(lvalue: TreeNode): String = {
    if(lvalue.name == "ID") return lvalue.value
    return getID(lvalue.children(1))
  }

  def generateCode(root: TreeNode): Unit = {
    if(root == null) return
    // if root belongs to terminals, then just return

    if(terminals.contains(root.name)) return
    var productionRuleList = List[String]()
    var productionRule = ""
    root.children.foreach((element) => {
      productionRuleList = element.name :: productionRuleList
    })
    productionRuleList = productionRuleList.reverse
    productionRule = productionRuleList.mkString(" ")
    // println(productionRule)
    root.name match {
      case "main" => {
        // look through each child
        var i = 0
        while(i <= root.children.length - 1) {
          if(root.children(i).name == "RETURN") {
            generateCode(root.children(i + 1)) // if I hit return, then I know the next child is an expr
            i = i + 1
          }
          else if(! terminals.contains(root.children(i).name)) {
            generateCode(root.children(i))
          }
          i = i + 1
        }
      }

      case "dcls" => {
        productionRule match {
          case "dcls dcl BECOMES NUM SEMI" => {
            // only one derivation for dcl (dcl -> type ID)
            generateCode(root.children(0)) // run it on dcls
            println("lis $3")
            println(s".word ${root.children(3).value}")
            push("$3", -4, "$30")
          }
          case "dcls dcl BECOMES NULL SEMI" => {
            generateCode(root.children(0))
            println("add $3, $0, $11") // $3 = 0x01
            push("$3", -4, "$30") // still a var so gotta push it onto stack
          }
          case _ => // dcl ->
        }
      }

      case "factor" => {
        productionRule match {
          case "ID" => {
            val id = root.children(0).value
            val offset = symbolTable(id)
            println("lw $3, " + offset.toString + "($29)")
          }
          case "NUM" => {
            val num = root.children(0).value
            println("lis $3")
            println(s".word $num")
          }
          case "STAR factor" => {
            generateCode(root.children(1)) // run code on factor
            println("lw $3, 0($3)")
          }
          case "NULL" => {
            println("add $3, $0, $11") // $3 = 0x01 for some f*cked up reason
          }
          case "AMP lvalue" => {
            val lvalue = root.children(1)
            lvalue.children.length match {
              case 1 => {
                // lvalue -> ID
                val idName = lvalue.children(0).value
                val offset = symbolTable(idName)
                println("lis $3")
                println(s".word $offset")
                println("add $3, $3, $29")
              }
              case 2 => {
                // so we have,
                // factor -> AMP lvalue
                // lvalue -> STAR factor
                // in this case, code (factorLHS) = code (factorRHS) as & and * cancel out each other
                // hence, no println stmts here. just generateCode
                generateCode((lvalue.children(1)))
              }
              case 3 => {
                // so we have,
                // factor -> AMP lvalue
                // lvalue1 -> LPAREN lvalue2 RPAREN

                // eg: &(y)
                generateCode(lvalue.children(1)) // on lvalue2
              }
            }
          }
          case "NEW INT LBRACK expr RBRACK" => {
            val expr = root.children(3)
            generateCode(expr) // result in $3
            push("$31", -4, "$30") // push return address on stack
            push("$1", -4, "$30") // push $1 because new uses $1
            println("add $1, $3, $0") // $1 = $3
            println("lis $7")
            println(".word new")
            println("jalr $7")
            pop("$1", -4, "$30") // restore $1
            pop("$31", -4, "$30") // restore $31
            println("bne $3, $0, " + "newSuccess" + counter.toString)
            println("add $3, $11, $0") // if new failed, then set $3 = 0x01
            println("newSuccess" + counter.toString + ":") // label if new succeeded
            counter = counter + 1
          }
          case _ => {
            for(child <- root.children) {
              generateCode(child)
              // root.code = root.code ::: child.code
            }
          }
        }
      }

      case "expr" if(root.children.length == 3) => {
        // expr -> expr PLUS/MINUS term
        val expr = root.children(0)
        val term = root.children(2)
        val op = root.children(1).value
        generateCode(expr)
        // push $3 onto stack
        push("$3", -4, "$30")
        generateCode(term)

        if(expr.nodeType == "int" && term.nodeType == "int") {
          pop("$5", -4, "$30")
          op match {
            case "+" => println("add $3, $5, $3")
            case "-" => println("sub $3, $5, $3")
          }
        }

        else if(expr.nodeType == "int*" && term.nodeType == "int" && op == "+") {
          println("mult $3, $4") // multiply term with 4
          println("mflo $3")
          pop("$5", -4, "$30")
          println("add $3, $5, $3")
        }

        else if(expr.nodeType == "int" && term.nodeType == "int*" && op == "+") {
          pop("$5", -4, "$30") // $5 contains first value -> int
          println("mult $5, $4")
          println("mflo $5")
          println("add $3, $5, $3")
        }

        else if(expr.nodeType == "int*" && term.nodeType == "int" && op == "-") {
          println("mult $3, $4")
          println("mflo $3")
          pop("$5", -4, "$30")
          println("sub $3, $5, $3")
        }

        else if(expr.nodeType == "int*" && term.nodeType == "int*" && op == "-") {
          pop("$5", -4, "$30")
          println("sub $3, $5, $3")
          println("div $3, $4")
          println("mflo $3")
        }
      }

      case "term" if(root.children.length == 3) => {
        generateCode(root.children(0))
        push("$3", -4, "$30")
        generateCode(root.children(2)) // $3 contains right hand side value
        pop("$5", -4, "$30") // $5 contains left hand side value
        val op = root.children(1).value
        op match {
          case "*" => {
            println("mult $5, $3")
            println("mflo $3")
          }
          case "/" => {
            println("div $5, $3")
            println("mflo $3")
          }
          case "%" => {
            println("div $5, $3")
            println("mfhi $3") // taking remainder
          }
        }
      }

      case "statement" => {
        productionRule match {
          case "PRINTLN LPAREN expr RPAREN SEMI" => {
            generateCode(root.children(2))
            // now, the value will be stored in $3
            push("$31", -4, "$30") // push return address on stack
            push("$1", -4, "$30") // push $1 because print uses $1
            println("lis $5")
            println(".word print")
            println("add $1, $0, $3") // $1 = $3
            println("jalr $5")
            pop("$1", -4, "$30") // restore $1
            pop("$31", -4, "$30") // restore $31
          }
          case "lvalue BECOMES expr SEMI" => {
            val lvalue = root.children(0)
            val expr = root.children(2)
            // first calculate expr, and then store that value in stack
            generateCode(expr)
            lvalue.children.length match {
              case 1 => {
                // lvalue -> ID
                val idName = lvalue.children(0).value
                val offset = symbolTable(idName).toInt
                push("$3", offset, "$29") // has to be relative to $29
              }
              case 2 => {
                // lvalue -> STAR factor
                // pointer assignment
                push("$3", -4, "$30")
                generateCode(lvalue) // this will be stored in $3. Its an address
                pop("$5", -4, "$30")
                println("sw $5, 0($3)")
              }

              case 3 => {
                // lvalue → LPAREN lvalue RPAREN
                val idName = getID(lvalue)
                val offset = symbolTable(idName).toInt
                push("$3", offset, "$29") // has to be relative to $29
              }
            }
          }
          case "WHILE LPAREN test RPAREN LBRACE statements RBRACE" => {
            val copy = counter
            counter = counter + 1
            println("loop" + copy.toString + ":")
            generateCode(root.children(2))
            println("beq $3, $0, done" + copy.toString)
            generateCode(root.children(5))
            println("beq $0, $0, loop" + copy.toString)
            println("done" + copy.toString + ":")
          }
          case "IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE" => {
            val copy = counter
            counter = counter + 1
            generateCode(root.children(2))
            println("beq $3, $0, else" + copy.toString)
            generateCode(root.children(5))
            println("beq $0, $0, endif" + copy.toString)
            println("else" + copy.toString + ":")
            generateCode(root.children(9))
            println("endif" + copy.toString + ":")
          }
          case "DELETE LBRACK RBRACK expr SEMI" => {
            val expr =  root.children(3)
            generateCode(expr) // result in $3
            // check if $3 == 0x01. cannot delete nullptr
            println("beq $3, $11, " + "skipDelete" + counter.toString)

            push("$31", -4, "$30") // push return address on stack
            push("$1", -4, "$30") // push $1 because delete uses $1
            println("lis $7")
            println(".word delete")
            println("add $1, $0, $3") // $1 = $3
            println("jalr $7")
            pop("$1", -4, "$30") // restore $1
            pop("$31", -4, "$30") // restore $31

            println("skipDelete" + counter.toString + ":")
            counter = counter + 1
          }
        }
      }
      case "test" => {
        productionRule match {
          case "expr LT expr" => { // is $5 < $3?
            val expr1 = root.children(0)
            val expr2 = root.children(2)
            generateCode(expr1)
            push("$3", -4, "$30")
            generateCode(expr2)
            pop("$5", -4, "$30")
            if(expr1.nodeType == "int") println("slt $3, $5, $3")
            else println("sltu $3, $5, $3")
          }

          case "expr GT expr" => { // is $5 > $3?
            val expr1 = root.children(0)
            val expr2 = root.children(2)
            generateCode(expr1)
            push("$3", -4, "$30")
            generateCode(expr2)
            pop("$5", -4, "$30")
            if(expr1.nodeType == "int") println("slt $3, $3, $5")
            else println("sltu $3, $3, $5")
          }

          case "expr NE expr" => { // is $5 != $3?
            val expr1 = root.children(0)
            val expr2 = root.children(2)
            generateCode(expr1)
            push("$3", -4, "$30")
            generateCode(expr2)
            pop("$5", -4, "$30")

            if(expr1.nodeType == "int") {
              println("slt $6, $3, $5")
              println("slt $7, $5, $3")
            }

            else { // int* comparisons
              println("sltu $6, $3, $5")
              println("sltu $7, $5, $3")
            }
            println("add $3, $6, $7")
          }

          case "expr EQ expr" => { // is $5 == $3?
            val expr1 = root.children(0)
            val expr2 = root.children(2)
            generateCode(expr1)
            push("$3", -4, "$30")
            generateCode(expr2)
            pop("$5", -4, "$30")

            if(expr1.nodeType == "int") {
              println("slt $6, $3, $5")
              println("slt $7, $5, $3")
            }

            else {
              println("sltu $6, $3, $5")
              println("sltu $7, $5, $3")
            }

            println("add $3, $6, $7")
            println("sub $3, $11, $3")
          }

          case "expr GE expr" => { // is $5 >= $3?
            val expr1 = root.children(0)
            val expr2 = root.children(2)
            generateCode(expr1)
            push("$3", -4, "$30")
            generateCode(expr2)
            pop("$5", -4, "$30")

            if(expr1.nodeType == "int") println("slt $3, $5, $3")
            else println("sltu $3, $5, $3")

            println("sub $3, $11, $3")
          }

          case "expr LE expr" => { // is $5 <= $3
            val expr1 = root.children(0)
            val expr2 = root.children(2)
            generateCode(expr1)
            push("$3", -4, "$30")
            generateCode(expr2)
            pop("$5", -4, "$30")
            if(expr1.nodeType == "int") println("slt $3, $3, $5")
            else println("sltu $3, $3, $5")
            println("sub $3, $11, $3")
          }
        }
      }

      case _ => {
        for(child <- root.children) {
          generateCode(child)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val imports = List(".import print", ".import init", ".import new", ".import delete")
    val prolog = List("lis $11", ".word 1", "lis $4", ".word 4", "sub $29, $30, $4", "sw $1, -4($30)", decrementSP,
      "sw  $2, -4($30)", decrementSP)
    val epilog = List("add $30, $29, $4", "jr $31")

    imports.foreach(println)
    prolog.foreach(println)

    push("$2", -4, "$30") // <--- this one statement cost me 3.5 hours...Damn, CS is hard.
    push("$31", -4, "$30") // push return address on stack

    println("lis $5")
    println(".word init")

    var root = populate() // build the parse tree
    setNodeType(root)

    println("jalr $5")
    pop("$31", -4, "$30") // restore $31
    pop("$2", -4, "$30") // restore $2

    makeSymbolTable(root)
    counter = 0
    // print assembly code
    generateCode(root)
    epilog.foreach(println)
  }
}
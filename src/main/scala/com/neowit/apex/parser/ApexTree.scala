package com.neowit.apex.parser



import scala.collection.mutable
import scala.collection.JavaConversions._

class ApexTree(val tree: mutable.LinkedHashMap[String, Member], val classByClassName: mutable.LinkedHashMap[String, ClassLikeMember]) {

    def this() {
        this(
            new mutable.LinkedHashMap[String, Member](),//identity -> member, e.g. TopLevelClass -> Member
            new mutable.LinkedHashMap[String, ClassLikeMember]() //class-name.toLowerCase => ClassLikeMember
        )

    }

    def getMember(identity: String): Option[Member] = {
        val lowerCaseIdentity = identity.toLowerCase
        if (lowerCaseIdentity.indexOf(".") > 0) {
            //TODO resolve hierarchically
            //resolve OuterClass.InnerClass in two steps
            val identities = lowerCaseIdentity.split("\\.")
            if (identities.length > 1) {
                tree.get(identities.head) match {
                    case Some(outerClassMember) =>
                        outerClassMember.getChild(identities.tail.head)
                    case None => None
                }
            }

            None
        } else {
            tree.get(lowerCaseIdentity)
        }
    }
    def addMember(member: Member): Unit = {
        tree += ((member.getIdentity.toLowerCase, member))
        if (member.isInstanceOf[ClassMember] || member.isInstanceOf[InterfaceMember]) {
            classByClassName += (member.getType.toLowerCase -> member.asInstanceOf[ClassLikeMember])
        }
    }

    def getClassMemberByType(classTypeName: String): Option[ClassLikeMember] = {
        val lowerCaseTypeName = classTypeName.toLowerCase
        val typeNames = lowerCaseTypeName.split("\\.")
        if (typeNames.size > 1) {
            //resolve OuterClass.InnerClass in two steps
            classByClassName.get(typeNames.head) match {
                case Some(outerClassMember) =>
                    outerClassMember.getInnerClassByType(typeNames.tail.head)  match {
                      case Some(x: InnerClassLikeMember) => Some(x)
                      case _ => None
                    }
                case None => None
            }
        } else {
            classByClassName.get(lowerCaseTypeName)
        }
    }
    def dump(): Unit = {
        for(key <- tree.keySet) {
            println(key + ": " + tree(key).toString)
        }
    }

    def extend(anotherTree: ApexTree, overwriteExisting: Boolean = true): Unit = {
        if (overwriteExisting) {
            // overwrite own keys
            tree ++= anotherTree.tree
            classByClassName ++= anotherTree.classByClassName
        } else {
            // keep own keys
            tree ++= anotherTree.tree.filterKeys(k => !tree.containsKey(k))
            classByClassName ++= anotherTree.classByClassName.filterKeys(k => !classByClassName.containsKey(k))
        }
    }

    override def clone: ApexTree = {
        val _tree = new mutable.LinkedHashMap[String, Member]()//identity -> member, e.g. TopLevelClass -> Member
        val _classByClassName = new mutable.LinkedHashMap[String, ClassLikeMember]() //class-name.toLowerCase => ClassMember
        _tree.putAll(this.tree)
        _classByClassName.putAll(this.classByClassName)
        new ApexTree(_tree, _classByClassName)
    }
}

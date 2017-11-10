package com.github.samtebbs33.baldr

class Command(val minArgCount: Int, val handler: Array[String] => Unit)

object Command {
  val void = (_: Array[String]) => ()
  val commandMap = Map(
    "repl" -> new Command(0, a => Baldr.repl(a)),
    "init" -> new Command(0, _ => ()),
    "save" -> new Command(1, a => Baldr.save(a(0))),
    "ls" -> new Command(0, void),
    "revert" -> new Command(1, a => Baldr.revert(a(0))),
    "add" -> new Command(1, a => Baldr.stage(a(0))),
    "rm" -> new Command(1, a => Baldr.unstage(a(0))),
    "ignore" -> new Command(1, a => Baldr.ignore.add(a(0))),
    "unignore" -> new Command(1, a => Baldr.ignore.remove(a(0))),
    "reset" -> new Command(1, a => {
      val hash = if(a.length > 1) a(1) else Branch.current.head
      Baldr.resetFile(a(0), hash)
    }),
    "switch" -> new Command(1, a => {
      val branchName = a(0)
      Branch.getBranch(branchName) match {
        case None => println(s"Branch '$branchName' doesn't exist")
        case Some(branch) => Branch.switchBranch(branch)
      }
    }),
    "branch" -> new Command(1, a => {
      val branchName = a(0)
      Branch.branchExists(branchName) match {
        case true => println(s"The branch '$branchName' already exists")
        case false => {
          val head = if (a.length > 1) a(1) else Branch.current.head
          Branch.setCurrentBranch(Branch.createBranch(branchName, head))
          // If the new branch head is not the same as the current head, then revert back to that head
          if(!head.equals(Branch.current.head)) Branch.switchBranch(Branch.current)
        }
      }
    })
  )

  def accept(cmd: String, args: Array[String]): Unit = commandMap.get(cmd) match {
    case Some(value) if args.length >= value.minArgCount => value.handler(args)
    case Some(value) => println(s"The command '$cmd' expects at least ${value.minArgCount} argument(s)")
    case None => println("No such command found")
  }

}

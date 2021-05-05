package filesystem.commands

import filesystem.State
import filesystem.files.Directory

class Mkdir(name: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name))
      state.setMessage(s"Entry $name already exists")
    else if (name.contains(Directory.SEPARATOR))
      state.setMessage(s"$name must not contain separators")
    else if (checkIllegal(name))
      state.setMessage(s"$name: illegal entry name")
    else
      doMakedir(state, name)
  }

  def checkIllegal(name: String): Boolean =
    name.contains(".")

  def doMakedir(state: State, name: String): State = ???
}

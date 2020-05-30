package example4

object Simulations extends App {

  case class Action(name: String, queueName: String) {

    override def toString: String = {
      "Action `" + name + "` execute on: " + queueName
    }

    def execute(): List[Action] = {
      List()
    }
  }

  type ActionQueue = List[Action]
  type ActionQueues = Map[String, ActionQueue]

  class Simulation(initialActions: List[Action]) {

    val queues: ActionQueues = Map() withDefaultValue List()

    def scheduleActions(toSchedule: List[Action]): ActionQueues = {
      ((toSchedule foldLeft queues)
        ((m: ActionQueues, a: Action) =>
          (m + (a.queueName -> (m(a.queueName).appended(a))))))
    }

    def runNextActions(queues: ActionQueues): (List[Action], ActionQueues) = {
      val (executed: List[Action], toSchedule: List[Action]) =
        ((queues.values foldLeft(List[Action](), List[Action]()))
          ((result: (List[Action], List[Action]), actions: List[Action]) => (result._1.appended(actions.head), (result._2 ++ actions.tail))))
      (executed, scheduleActions(toSchedule))
    }

    val scheduleActions: ActionQueues = scheduleActions(initialActions)

    def run(stepQueues: ActionQueues): List[Action] = {
      if( stepQueues.isEmpty ){
        Nil
      } else {
        val (executed, queues) = runNextActions(stepQueues)
        executed.appendedAll(run(queues))
      }
    }

    def run():  List[Action] = {
      run(scheduleActions(initialActions))
    }
  }

  val simulation = new Simulation(List(Action("Test", "TestQueue"), Action("Test2", "TestQueue"), Action("Test3", "TestQueue2")))
  val executed: List[Action] = simulation.run()
  println(executed)
}

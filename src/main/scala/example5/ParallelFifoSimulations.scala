package example5

object ParallelFifoSimulations extends App {

  abstract class Action() {
    val name: String
    val queueName: String

    def execute(): List[Action]
  }

  case class SingleAction(name: String, queueName: String) extends Action {

    override def toString: String = {
      "Single Action `" + name + "` execute on: " + queueName
    }

    def execute(): List[Action] = {
      List()
    }
  }

  case class GeneratorAction(name: String, generationQueueName: String) extends Action {

    val queueName: String = "Generators"

    override def toString: String = {
      "Generator Action `" + name + "` execute on: " + queueName
    }

    def execute(): List[Action] = {
      List(SingleAction(name, generationQueueName))
    }
  }

  type ActionQueue = List[Action]
  type ActionQueues = Map[String, ActionQueue]

  class Simulation(initialActions: List[Action]) {

    val queues: ActionQueues = Map() withDefaultValue List()

    def scheduleActions(toSchedule: List[Action]): ActionQueues = {
      ((toSchedule foldLeft queues)
        ((m: ActionQueues, a: Action) =>
          m + (a.queueName -> m(a.queueName).appended(a))))
    }

    def runNextActions(queues: ActionQueues): (List[Action], ActionQueues) = {
      val (executed: List[Action], toSchedule: List[Action]) =
        ((queues.values foldLeft(List[Action](), List[Action]()))
          ((result: (List[Action], List[Action]), actions: List[Action]) => (result._1.appended(actions.head), (result._2 ++ actions.tail) ++ actions.head.execute())))
      (executed, scheduleActions(toSchedule))
    }

    val scheduleActions: ActionQueues = scheduleActions(initialActions)

    def run(stepQueues: ActionQueues): List[Action] = {
      if (stepQueues.isEmpty) {
        Nil
      } else {
        val (executed, queues) = runNextActions(stepQueues)
        executed.appendedAll(run(queues))
      }
    }

    def run(): List[Action] = {
      run(scheduleActions(initialActions))
    }
  }

  val simulation = new Simulation(List(
    GeneratorAction("Test1", "TestQueue1"),
    GeneratorAction("Test2", "TestQueue2"),
    GeneratorAction("Test3", "TestQueue3"),
    GeneratorAction("Test4", "TestQueue1"),
    GeneratorAction("Test5", "TestQueue2"),
    GeneratorAction("Test6", "TestQueue3"),
    GeneratorAction("Test7", "TestQueue1")))
  val executed: List[Action] = simulation.run()
  executed.foreach(println)
}

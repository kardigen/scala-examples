package example6

object TimeScheduledSimulations extends App {

  abstract class Action() {
    val name: String
    val queueName: String
    val executionTime: Int
    val finishesAt: Option[Int]

    def execute(): List[Action]

    def schedule(startAt: Int): Action
  }

  case class SingleAction(name: String, queueName: String, executionTime: Int, finishesAt: Option[Int]) extends Action {

    def this(name: String, queueName: String, executionTime: Int) {
      this(name, queueName, executionTime, Option.empty)
    }

    override def toString: String = {
      if (finishesAt.isEmpty)
        s"Single Action `$name` with execution time: $executionTime scheduled in queue: $queueName"
      else
        s"Single Action `$name` executed on: $queueName with execution time: $executionTime finishes at: ${finishesAt.get}"
    }

    override def execute(): List[Action] = {
      List()
    }

    override def schedule(startAt: Int): Action = {
      SingleAction(name, queueName, executionTime, Option(startAt + executionTime))
    }
  }

  case class GeneratorAction(name: String, generationQueueName: String, generationTime: Int, executionTime: Int, finishesAt: Option[Int]) extends Action {
    val queueName = "Generators"

    def this(name: String, generationQueueName: String, generationTime: Int, executionTime: Int) {
      this(name, generationQueueName, generationTime, executionTime, Option.empty)
    }

    override def toString: String = {
      if (finishesAt.isEmpty)
        s"Generator Action `$name` with execution time: $executionTime scheduled in queue: $queueName"
      else
        s"Generator Action `$name` executed on: $queueName with execution time: $executionTime finishes at: ${finishesAt.get}"
    }

    override def execute(): List[Action] = {
      List(SingleAction(name, generationQueueName, generationTime, Option.empty))
    }

    override def schedule(startAt: Int): Action = {
      GeneratorAction(name, generationQueueName, generationTime, executionTime, Option(startAt + executionTime))
    }
  }

  type ActionQueue = List[Action]
  type ActionQueueSchedules = Map[String, Int]

  case class ActionScheduler(queue: ActionQueue, lastSchedules: ActionQueueSchedules) {
    def scheduledQueue: ActionQueue = queue.sortBy(_.finishesAt.getOrElse(Int.MaxValue))

    def scheduleActions(timeNow: Int, actions: ActionQueue): ActionScheduler = {
      val (toSchedule, scheduledActions) = actions.partition(_.finishesAt.isEmpty)

      def newActionsScheduler = (toSchedule.foldLeft(ActionScheduler(List(), lastSchedules withDefaultValue timeNow))
      ((scheduler: ActionScheduler, action: Action) => {
        def scheduledAction = action.schedule(scheduler.lastSchedules(action.queueName))

        ActionScheduler(scheduledAction :: scheduler.queue, scheduler.lastSchedules + (action.queueName -> scheduledAction.finishesAt.get))
      }))

      ActionScheduler(scheduledActions ++ newActionsScheduler.queue, newActionsScheduler.lastSchedules)
    }

  }

  class Simulation(initialActions: List[Action]) {

    def run(scheduler: ActionScheduler): List[Action] = scheduler.scheduledQueue match {
      case List() => List()
      case action :: scheduledQueue => action :: run(scheduler.scheduleActions(action.finishesAt.get, scheduledQueue ++ action.execute()))
    }


    def run(): List[Action] = {
      run(ActionScheduler(List(), Map()).scheduleActions(0, initialActions))
    }
  }

  val simulation = new Simulation(List(
    new GeneratorAction("Test1", "SlowQueue", 50, 15),
    new GeneratorAction("Test2", "FastQueue", 20, 15),
    new GeneratorAction("Test3", "SlowQueue", 50, 15),
    new GeneratorAction("Test4", "SlowQueue", 50, 15),
    new GeneratorAction("Test5", "FastQueue", 20, 15),
    new GeneratorAction("Test6", "FastQueue", 20, 15),

  ))
  val executed: List[Action] = simulation.run()
  executed.foreach(println)
}

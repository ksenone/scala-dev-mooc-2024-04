package ru.otus.module1.futures


import ru.otus.module1.futures.HomeworksUtils.TaskSyntax

import scala.util.{Failure, Success}
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Promise}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    case class FuturesResults(success: List[A], failures: List[Throwable])

    @tailrec
    def completeFuture(future: Future[A], acc: FuturesResults): FuturesResults = if (future.isCompleted) {
      future.value match {
        case Some(Success(value)) => FuturesResults(acc.success :+ value, acc.failures)
        case Some(Failure(exception)) => FuturesResults(acc.success, acc.failures :+ exception)
      }
    } else completeFuture(future, acc)

    @tailrec
    def go(
      futuresList: List[Future[A]],
      acc: FuturesResults
    ): Promise[(List[A], List[Throwable])] = futuresList match {
      case Nil =>
        val promise = Promise[(List[A], List[Throwable])]
        promise.success(acc.success, acc.failures)

      case head :: tail =>
        go(tail, completeFuture(head, acc))
    }

    go(futures, FuturesResults(Nil, Nil)).future
  }

}

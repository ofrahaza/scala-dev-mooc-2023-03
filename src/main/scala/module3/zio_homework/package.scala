package module3

import zio.{Has, Task, ULayer, ZIO, ZLayer}
import zio.clock.{Clock, instant, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._
import zio.{URIO, clock, ZLayer}
import module3.zioConcurrency.printEffectRunningTime

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  lazy val guessProgram: ZIO[Random with Console, Throwable, Unit] = for {
    secretNumber <- nextIntBetween(1, 4)
    _ <- putStrLn("Ввелите число от 1 до 3")
    input <- ZIO.environment[Console].map(_.get)
    inputInt <- input.getStrLn
    res <- input.putStrLn(if (inputInt.toInt == secretNumber) "Success" else "Failed").orDie
  } yield res


  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile: Task[Boolean] => Task[Unit] = (effect: Task[Boolean]) => for {
    e <- effect
    _ <- if (e) ZIO.succeed(()) else doWhile(effect)
  } yield ()

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: ZIO[Any, Throwable, config.AppConfig] = for {
    c <- config.load.foldM(
      _ => ZIO.succeed(config.AppConfig("mur", "meow")),
      success => ZIO.succeed(success)
    )
    _ <- ZIO.effect(println(c))
  } yield c

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = for {
    r <- ZIO.environment[Random].map(_.get)
    _ <- ZIO.sleep(1 seconds)
    n <- r.nextIntBetween(1, 11)
  } yield n

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = List.fill(10)(eff)

  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = printEffectRunningTime(for {
    res <- effects.reduceLeft((a, b) => a.zipWith(b)(_ + _))
    console <- ZIO.environment[Console].map(_.get)
    _ <- console.putStrLn(res.toString).orDie
  } yield res)


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp =
    printEffectRunningTime(for {
      p <- ZIO.foreachParN(10)(effects)(identity)
      s <- ZIO.effect(p.sum)
      _ <- ZIO.accessM[Console](c => c.get.putStrLn(s.toString)).orDie
    } yield s)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type ZIORunTimeService = Has[ZIORunTimeService.ZIOService]

  object ZIORunTimeService {
    trait ZIOService {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A]
    }

    val run = ZLayer.succeed(new ZIOService {
      override def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[R with Clock, E, A] = for {
        start <- getTime
        result <- effect
        end <- getTime
        _ <- ZIO.succeed(println(s"Runtime - ${end - start}"))
      } yield result
    })

    def getTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.SECONDS)

    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with ZIORunTimeService with Clock, E, A] = for {
      service <- ZIO.environment[ZIORunTimeService].map(_.get)
      it <- service.printEffectRunningTime(zio)
    } yield it

    /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     *
     */

    lazy val appWithTimeLogg: ZIO[ZIORunTimeService with Console with Clock with Random, Throwable, Unit] = for {
      runTime <- ZIO.environment[ZIORunTimeService].map(_.get)
      result <- runTime.printEffectRunningTime(app)
    } yield result

    /**
     *
     * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
     */

    lazy val runApp: ZIO[zio.ZEnv, Throwable, Unit] = appWithTimeLogg.provideCustomLayer(ZIORunTimeService.run)
  }
}
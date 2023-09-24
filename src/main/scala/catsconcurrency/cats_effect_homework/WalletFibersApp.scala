package catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._
import cats.Monad
import scala.concurrent.duration.DurationInt
import cats.effect.{IO, IOApp, Spawn}

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
      f1 <- Spawn[IO].start(Monad[IO].whileM_(IO(true))(add(wallet1, 100)))
      f2 <- Spawn[IO].start(Monad[IO].whileM_(IO(true))(add(wallet2, 500)))
      f3 <- Spawn[IO].start(Monad[IO].whileM_(IO(true))(add(wallet3, 2000)))
      print <- Spawn[IO].start(Monad[IO].whileM_(IO(true))(print(wallet1, wallet2, wallet3)))
      _ <- IO.readLine
      _ <- f1.cancel *> f2.cancel *> f3.cancel *> print.cancel
    } yield ()

  private def add(w: Wallet[IO], timeout: Int) = for {
    _ <- w.topup(100)
    res <-IO.sleep(timeout.milliseconds)
  } yield res

  private def print(w1: Wallet[IO], w2: Wallet[IO], w3: Wallet[IO]) = for {
    _ <- w1.balance.flatMap(w => IO.println(s"Wallet 1 balance = $w"))
    _ <- w2.balance.flatMap(w => IO.println(s"Wallet 2 balance = $w"))
    _ <- w3.balance.flatMap(w => IO.println(s"Wallet 3 balance = $w"))
    result <- IO.sleep(1.second)
  } yield result
}
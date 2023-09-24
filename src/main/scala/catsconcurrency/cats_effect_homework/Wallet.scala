package catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {

  private def read: F[BigDecimal] = Sync[F].delay(BigDecimal(Files.readAllLines(Paths.get(id), StandardCharsets.UTF_8).toArray.mkString))

  private def write(value: BigDecimal): F[Unit] = Sync[F].delay(Files.write(Paths.get(id), value.toString.getBytes(StandardCharsets.UTF_8), StandardOpenOption.TRUNCATE_EXISTING)) >> Sync[F].unit

  override def balance: F[BigDecimal] = read

  override def topup(amount: BigDecimal): F[Unit] = Sync[F].flatMap(read)(b => write(b + amount))

  override def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = Sync[F].flatMap(read) { b =>
    if (b >= amount) Sync[F].map(write(b - amount))(Right(_)) else Sync[F].pure(Left(BalanceTooLow))
  }
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов

  def fileWallet[F[_] : Sync](id: WalletId): F[Wallet[F]] = {
    val storagePath = Paths.get(id)

    Sync[F].blocking(Files.exists(storagePath)).flatMap { f =>
      if (f) Sync[F].unit else {
        Sync[F].delay(Files.createFile(storagePath)) *>
          Sync[F].delay(Files.write(storagePath, BigDecimal(0).toString().getBytes)) *>
          Sync[F].unit
      }
    } *> Sync[F].delay(new FileWallet[F](id))
  }

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
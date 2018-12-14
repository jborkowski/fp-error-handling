package com.jobo

import cats._
import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, IOApp, Sync, Timer}
import cats.effect.concurrent.Ref
import cats.syntax.all._

import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.blaze.BlazeBuilder

import java.util.UUID

case class User(name: String, age: Int)

case class UpdateAge(age: Int)

trait UserAlgebra[F[_]] {
  def find(username: String): F[Option[User]]

  def save(user: User): F[Unit]

  def updateAge(username: String, age: Int): F[Unit]
}

sealed trait UserError extends Exception

case class UserAlreadyExists(username: String) extends UserError

case class UserNotFound(username: String) extends UserError

case class InvalidUserAge(age: Int) extends UserError

// User Algebra interpreter

object UserInterpreter {

  def create[F[_]](implicit F: Sync[F]): F[UserAlgebra[F]] =
    Ref.of[F, Map[String, User]](Map.empty[String, User]).map { state =>
      def validateAge(age: Int): F[Unit] =
        if (age <= 0) F.raiseError(InvalidUserAge(age)) else F.unit

      new UserAlgebra[F] {
        override def find(username: String): F[Option[User]] =
          state.get.map(_.get(username))

        override def save(user: User): F[Unit] =
          validateAge(user.age) *>
            find(user.name).flatMap {
              case Some(_) =>
                F.raiseError(UserAlreadyExists(user.name))
              case None =>
                state.update(_.updated(user.name, user))
            }

        override def updateAge(username: String, age: Int): F[Unit] =
          validateAge(age) *>
            find(username) flatMap {
            case None =>
              F.raiseError(UserNotFound(username))
            case Some(user) =>
              state.update(_.updated(user.name, user.copy(age = age)))
          }
      }
    }
}

import doobie._
import doobie.implicits._
import doobie.postgres.implicits._

object UserInterpreterDoobie {

  def create[F[_]: MonadError[?[_], UserError]](transactor: Transactor[F]): UserAlgebra[F] = new UserAlgebra[F] {
    private def validateAge(age: Int): F[Unit] =
      if (age <= 0) MonadError[F, UserError].raiseError(InvalidUserAge(age)) else MonadError[F, UserError].unit

    override def find(username: String): F[Option[User]] =
      fr"""SELECT name, age FROM public.user WHERE name = $username""".query[User].option.transact(transactor).flatMap {
        case None => MonadError[F, UserError].raiseError(UserNotFound(username))
        case user => user.pure[F]
      }

    override def save(user: User): F[Unit] =
      fr"""INSERT INTO public.user ( name, age ) VALUES ( ${user.name}, ${user.age} )""".update.run
        .transact(transactor)
        .map(_ => ())

    override def updateAge(username: String, age: Int): F[Unit] =
      fr"""UPDATE user SET age=$age WHERE name=username""".update.run.transact(transactor).map(_ => ())
  }

}

class UserRoutes[F[_]: Sync](userAlgebra: UserAlgebra[F]) extends Http4sDsl[F] {

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "users" / username =>
      userAlgebra.find(username).flatMap {
        case Some(user) => Ok(user.asJson)
        case None       => NotFound(username.asJson)
      }

    case req @ POST -> Root / "users" =>
      req.as[User].flatMap { user =>
        userAlgebra.save(user) *> Created(s"User ${user.name} created".asJson)
      }

    case req @ PUT -> Root / "users" / username =>
      req.as[UpdateAge].flatMap { d =>
        userAlgebra.updateAge(username, d.age) *> Ok(username.asJson)
      }
  }
}

class UserRoutesAlt[F[_]: Sync](userAlgebra: UserAlgebra[F]) extends Http4sDsl[F] {

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "users" / username =>
      userAlgebra.find(username).flatMap {
        case Some(user) => Ok(user.asJson)
        case None       => NotFound(username.asJson)
      }

    case req @ POST -> Root / "users" =>
      req
        .as[User]
        .flatMap { user =>
          userAlgebra.save(user) *> Created(s"User ${user.name} created".asJson)
        }
        .handleErrorWith {
          case UserAlreadyExists(username) => Conflict(s"User name $username already exists".asJson)
        }

    case req @ PUT -> Root / "users" / username =>
      req
        .as[UpdateAge]
        .flatMap { d =>
          userAlgebra.updateAge(username, d.age) *> Ok(username.asJson)
        }
        .handleErrorWith {
          case InvalidUserAge(age) => BadRequest(s"Provided age is invalid $age".asJson)
        }
  }
}

trait HttpErrorHandler[F[_], E <: Throwable] {
  def handle(routes: HttpRoutes[F]): HttpRoutes[F]
}

object HttpErrorHandler {
  def apply[F[_], E <: Throwable](implicit ME: MonadError[F, E]) = ME
}

class UserRoutesMEOW[F[_]: Sync: Monad](userAlgebra: UserAlgebra[F])(implicit ev: HttpErrorHandler[F, UserError])
    extends Http4sDsl[F] {
  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "users" / username =>
      userAlgebra.find(username).flatMap {
        case Some(user) => Ok(user.asJson)
        case None       => NotFound(username.asJson)
      }

    case req @ POST -> Root / "users" =>
      req.as[User].flatMap { user =>
        userAlgebra.save(user) *> Created(s"User ${user.name} created".asJson)
      }

    case req @ PUT -> Root / "users" / username =>
      req.as[UpdateAge].flatMap { d =>
        userAlgebra.updateAge(username, d.age) *> Ok(username.asJson)
      }
  }

  val routes: HttpRoutes[F] = ev.handle(httpRoutes)
}

import cats.ApplicativeError
import cats.data.{Kleisli, OptionT}

object RoutesErrorHandler {

  def apply[F[_], E <: Throwable](routes: HttpRoutes[F])(handler: E => F[Response[F]])(
    implicit AE: ApplicativeError[F, E]): HttpRoutes[F] =
    Kleisli { req: Request[F] =>
      OptionT {
        routes.run(req).value.handleErrorWith { e =>
          handler(e).map(Option(_))
        }
      }
    }
}

class UserHttpErrorHandler[F[_]: MonadError[?[_], UserError]] extends HttpErrorHandler[F, UserError] with Http4sDsl[F] {
  private val handler: UserError => F[Response[F]] = {
    case UserAlreadyExists(username) => Conflict(s"Provided User ($username) already exists".asJson)
    case InvalidUserAge(age)         => BadRequest(s"Invalid age $age".asJson)
    case UserNotFound(username)      => NotFound(s"User not found: $username".asJson)
  }

  override def handle(routes: HttpRoutes[F]): HttpRoutes[F] =
    RoutesErrorHandler(routes)(handler)
}

// PART 2

trait ErrorChannel[F[_], E <: Throwable] {
  def raise[A](e: E): F[A]
}

import cats.ApplicativeError

object ErrorChannel {
  def apply[F[_], E <: Throwable](implicit ev: ErrorChannel[F, E]): ErrorChannel[F, E] = ev

  implicit def instance[F[_], E <: Throwable](implicit F: ApplicativeError[F, Throwable]): ErrorChannel[F, E] =
    new ErrorChannel[F, E] {
      override def raise[A](e: E): F[A] = F.raiseError(e)
    }

  object syntax {

    implicit class ErrorChannelOps[F[_]: ErrorChannel[?[_], E], E <: Throwable](e: E) {
      def raise[A]: F[A] = ErrorChannel[F, E].raise[A](e)
    }

  }

}

abstract class UserAlg[F[_]: ErrorChannel[?[_], E], E <: Throwable] {
  def find(username: String): F[Option[User]]

  def save(user: User): F[Unit]

  def updateAge(username: String, age: Int): F[Unit]
}

object UserInterpreterErrorChannel {

  def mkUserAlg[F[_]: Sync](implicit error: ErrorChannel[F, UserError]): F[UserAlg[F, UserError]] =
    Ref.of[F, Map[String, User]](Map.empty).map { state =>
      new UserAlg[F, UserError] {
        private def validateAge(age: Int): F[Unit] =
          if (age <= 0) error.raise(InvalidUserAge(age)) else ().pure[F]

        def find(username: String): F[Option[User]] =
          state.get.map(_.get(username))

        def save(user: User): F[Unit] =
          validateAge(user.age) *>
            find(user.name).flatMap {
              case Some(_) =>
                error.raise(UserAlreadyExists(user.name))
              case None =>
                state.update(_.updated(user.name, user))
            }

        def updateAge(username: String, age: Int): F[Unit] =
          validateAge(age) *>
            find(username).flatMap {
              case Some(user) =>
                state.update(_.updated(username, user.copy(age = age)))
              case None =>
                error.raise(UserNotFound(username))
            }
      }
    }
}

abstract class RoutesImproved[F[_], E <: Throwable](implicit H: HttpErrorHandler[F, E]) extends Http4sDsl[F] {
  protected def httpRoutes: HttpRoutes[F]

  val routes: HttpRoutes[F] = H.handle(httpRoutes)
}

abstract class UserRoutesImproved[F[_]: HttpErrorHandler[?[_], E], E <: Throwable](
  users: UserAlg[F, E]
) extends RoutesImproved[F, E]

class UserRoutesAltImproved[F[_]: HttpErrorHandler[?[_], UserError]: Sync](
  users: UserAlg[F, UserError]
) extends UserRoutesImproved(users) {
  protected val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "users" / username =>
      users.find(username).flatMap {
        case Some(user) => Ok(user.asJson)
        case None       => NotFound(username.asJson)
      }

    case req @ POST -> Root / "users" =>
      req
        .as[User]
        .flatMap { user =>
          users.save(user) *> Created(user.name.asJson)
        }

    case req @ PUT -> Root / "users" / username =>
      req
        .as[UpdateAge]
        .flatMap { userUpdate =>
          users.updateAge(username, userUpdate.age) *> Ok(username.asJson)
        }
  }
}

sealed trait CatalogError extends Exception

case class ItemAlreadyExists(item: String) extends CatalogError

case class CatalogNotFound(id: Long) extends CatalogError

case class Item(name: String) extends AnyVal

abstract class CatalogAlg[F[_]: ErrorChannel[?[_], E], E <: Throwable] {
  def find(id: Long): F[List[Item]]

  def save(id: Long, item: Item): F[Unit]
}

class UserRoutesMTL[F[_]: Sync](
  users: UserAlg[F, UserError],
  catalog: CatalogAlg[F, CatalogError]
) extends Http4sDsl[F] {
  private val httpRoutes: HttpRoutes[F] = ???

  def routes(
    implicit
    H1: HttpErrorHandler[F, UserError],
    H2: HttpErrorHandler[F, CatalogError]
  ): HttpRoutes[F] =
    H2.handle(H1.handle(httpRoutes)) // Compose Error handling

}

// To compose Error algebras using CoProduct
import shapeless._

trait CoHttpErrorHandler[F[_], Err <: Coproduct] {
  def handle(routes: HttpRoutes[F]): HttpRoutes[F]
}

object CoHttpErrorHandler {
  def apply[F[_], Err <: Coproduct](implicit ev: CoHttpErrorHandler[F, Err]) = ev

  implicit def cNilInstance[F[_]]: CoHttpErrorHandler[F, CNil] =
    (routes: HttpRoutes[F]) => routes

  implicit def consInstance[F[_], E <: Throwable, T <: Coproduct](
    implicit H: HttpErrorHandler[F, E],
    CH: CoHttpErrorHandler[F, T]
  ): CoHttpErrorHandler[F, E :+: T] =
    (routes: HttpRoutes[F]) => CH.handle(H.handle(routes))
}

class CoUserRoutesMLTA[F[_]: Sync](
  users: UserAlg[F, UserError],
  catalogs: CatalogAlg[F, CatalogError]
) extends Http4sDsl[F] {
  private val httpRoutes: HttpRoutes[F] = ???

  import shapeless._

  def routes(implicit CH: CoHttpErrorHandler[F, UserError :+: CatalogError :+: CNil]): HttpRoutes[F] =
    CH.handle(httpRoutes)
}

abstract class CoRoutes[F[_], E <: Coproduct](implicit CH: CoHttpErrorHandler[F, E]) extends Http4sDsl[F] {
  protected def httpRoutes: HttpRoutes[F]

  val routes: HttpRoutes[F] = CH.handle(httpRoutes)
}

abstract class CoUserRoutes[
  F[_]: CoHttpErrorHandler[?[_], E],
  A <: Throwable,
  B <: Throwable,
  E <: Coproduct: =:=[?, A :+: B :+: CNil]
](
  users: UserAlg[F, A],
  catalog: CatalogAlg[F, B]
) extends CoRoutes[F, E]

// type CustomerError = UserError :+: CatalogError :+: CNil

class CoUserRoutesMLT[F[_]: CoHttpErrorHandler[?[_], UserError :+: CatalogError :+: CNil]](
  users: UserAlg[F, UserError],
  catalogs: CatalogAlg[F, CatalogError]
) extends CoUserRoutes(users, catalogs) {
  protected val httpRoutes: HttpRoutes[F] = ???
}

import cats.MonadError
import cats.effect.IO
import com.olegpy.meow.hierarchy._

object Server extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    implicit val userHttpErrorHandlerHierarchy: HttpErrorHandler[IO, UserError] = new UserHttpErrorHandler[IO]
    //implicit val catsTimer:                     Timer[IO]                       = IO.timer(global)

    // TODO: ADD HERE PURECONFIG
    lazy val transactor = Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql://localhost:5432/jborkowski",
      "jborkowski",
      ""
    )

    for {
      userInterp <- UserInterpreter.create[IO]
      doobieInterpreter = UserInterpreterDoobie.create[IO](transactor)
      userRoutes        = new UserRoutesMEOW[IO](doobieInterpreter)
      blaze <- BlazeBuilder[IO]
        .bindHttp(8080, "0.0.0.0")
        .mountService(userRoutes.routes, "/")
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield blaze

  }
}

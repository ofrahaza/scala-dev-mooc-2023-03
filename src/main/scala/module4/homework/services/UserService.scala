package module4.homework.services

import zio.Has
import zio.Task
import module4.homework.dao.entity.{Role, RoleCode, User, UserId, UserToRole}
import module4.homework.dao.repository.UserRepository
import zio.ZIO
import zio.RIO
import zio.ZLayer
import zio.macros.accessible
import module4.phoneBook.db

@accessible
object UserService {
    type UserService = Has[Service]

    trait Service {
        def listUsers(): RIO[db.DataSource, List[User]]

        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]

        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service {
        val dc = db.Ctx

        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] =
            userRepo.list()

        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]] = {
            for {
                users <- listUsers()
                usersDto <- ZIO.foreach(users) { u =>
                    userRepo.userRoles(u.typedId).map(roles => UserDTO(u, roles.toSet))
                }
            } yield usersDto
        }

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = for {
            u <- userRepo.createUser(user)
            _ <- userRepo.insertRoleToUser(roleCode, user.typedId)
            roles <- userRepo.userRoles(UserId(u.id))
            result <- ZIO.succeed(UserDTO(u, roles.toSet))
        } yield result

        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = for {
            users <- userRepo.listUsersWithRole(roleCode)
            roles <- for {
                r <- ZIO.foreach(users)(user => userRepo.userRoles(user.typedId).map(_.toSet))
            } yield r
            usersRoles <- ZIO.succeed(users zip roles)
        } yield usersRoles.map(u => UserDTO(u._1, u._2))
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] = ???
}

case class UserDTO(user: User, roles: Set[Role])